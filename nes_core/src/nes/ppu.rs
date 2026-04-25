use crate::{nes::mapper::Cartridge, render::Render};
use std::fmt::Write;

mod palette;
mod pattern;
mod registers;
mod sprite;

use palette::{Palette, Pixel};
pub use pattern::{PatternBand, draw_pattern};
use registers::{PpuCtrl, PpuMask, PpuStatus, Registers};
use sprite::SpriteManager;

// Pixel, palette data and COLORS are defined in the submodule `palette`.

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PatternAccess {
    Cpu,
    Background,
    Sprite,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct BackgroundTileOverride {
    pub palette_idx: u8,
    pub color_idx: u8,
}

const TILES_PER_ROW: u8 = 32;

// PPU Timing Constants
const SCANLINES_PER_FRAME: u16 = 262;
const DOTS_PER_SCANLINE: u16 = 341;
const PPU_OPEN_BUS_DECAY_TICKS: u64 = 3_221_591;
pub const VBLANK_SET_SCANLINE: u16 = 241;
pub const VBLANK_SET_DOT: u16 = 1;
const VBLANK_CLEAR_SCANLINE: u16 = 261;
const VBLANK_CLEAR_DOT: u16 = 1;
fn normalize_oam_byte(addr: u8, value: u8) -> u8 {
    if addr & 0x03 == 0x02 {
        value & 0xE3
    } else {
        value
    }
}

#[derive(Copy, Clone)]
struct BackgroundActivation {
    screen_x: u8,
    vram_addr: u16,
    fine_x: u8,
    ctrl: PpuCtrl,
}

impl BackgroundActivation {
    fn snapshot(ppu: &Ppu<impl Render>, screen_x: u8) -> Self {
        Self {
            screen_x,
            vram_addr: ppu.registers.vram_addr,
            fine_x: ppu.registers.fine_x,
            ctrl: ppu.registers.ctrl,
        }
    }
}

pub struct Ppu<R: Render = ()> {
    registers: Registers,

    palette: Palette, // palette memory

    suppress_vblank_for_current_frame: bool,
    suppress_nmi_for_current_frame: bool,

    renderer: R,

    // PPU timing
    scanline: u16, // 0-261
    dot: u16,      // 0-340
    background_anchor: Option<BackgroundActivation>,
    pending_background_activation: Option<BackgroundActivation>,
    odd_frame: bool,
    sprite: SpriteManager,

    /// PPU mask register changes are not visible until the next PPU tick;
    /// this delayed copy models the 1-dot internal pipeline delay.
    effective_mask: PpuMask,
    rendering_enabled_at_scanline_start: bool,

    frame_no: usize,
    ppu_ticks: u64,
}

/// PPU registers are mirrored every 8 bytes in range $2000-$3FFF
/// this function normalized $2000, $2008, .. $3ff8 to $2000
fn normalize_ppu_addr(addr: u16) -> u16 {
    addr & 0x2007
}

impl<R: Render> Ppu<R> {
    pub fn new(renderer: R) -> Self {
        Ppu {
            registers: Registers::new(),
            palette: Palette::default(),
            renderer,
            scanline: 0,
            dot: 0,
            background_anchor: None,
            pending_background_activation: None,
            odd_frame: false,
            sprite: SpriteManager::new(),
            suppress_vblank_for_current_frame: false,
            suppress_nmi_for_current_frame: false,
            frame_no: 0,
            ppu_ticks: 0,
            effective_mask: PpuMask::new(),
            rendering_enabled_at_scanline_start: false,
        }
    }

    pub fn reset(&mut self) {
        // https://www.nesdev.org/wiki/PPU_power_up_state
        self.registers.reset();
        self.effective_mask = PpuMask::new();
        self.background_anchor = None;
        self.pending_background_activation = None;
        self.odd_frame = false;
        self.sprite.reset();
        self.ppu_ticks = 0;
        self.registers.write_scroll(0);
        self.rendering_enabled_at_scanline_start = false;
    }

    pub fn timing(&self) -> (u16, u16) {
        (self.scanline, self.dot)
    }

    pub fn frame_no(&self) -> usize {
        self.frame_no
    }

    pub fn renderer(&self) -> &R {
        &self.renderer
    }

    fn scroll_xy(&self) -> (u16, u16) {
        let scroll_addr = self.registers.temp_vram_addr;
        let scroll_x = ((scroll_addr & 0x001f) << 3) | self.registers.fine_x as u16;
        let scroll_y = (((scroll_addr >> 5) & 0x001f) << 3) | ((scroll_addr >> 12) & 0x0007);
        (scroll_x, scroll_y)
    }

    fn schedule_background_activation_if_visible(&mut self) {
        if self.rendering_enabled()
            && let Some(screen_x) = self.next_visible_x()
        {
            let screen_x = screen_x.saturating_add(16);
            self.pending_background_activation =
                Some(BackgroundActivation::snapshot(self, screen_x));
        }
    }

    fn apply_pending_background_activation(&mut self, screen_x: u8) {
        if let Some(pending) = self.pending_background_activation
            && screen_x >= pending.screen_x
        {
            self.background_anchor = Some(pending);
            self.pending_background_activation = None;
        }
    }

    fn next_visible_x(&self) -> Option<u8> {
        if self.scanline < 240 && (1..=256).contains(&self.dot) {
            Some((self.dot - 1) as u8)
        } else {
            None
        }
    }

    /// Increment fine Y in vram_addr. Called at dot 256 of each visible scanline
    /// when rendering is enabled. Wraps fine Y → coarse Y → nametable Y toggle.
    fn increment_vram_y(&mut self) {
        let fine_y = (self.registers.vram_addr >> 12) & 0x07;
        if fine_y < 7 {
            self.registers.vram_addr += 0x1000; // increment fine Y
        } else {
            self.registers.vram_addr &= !0x7000; // clear fine Y
            let mut coarse_y = (self.registers.vram_addr >> 5) & 0x1F;
            if coarse_y == 29 {
                coarse_y = 0;
                self.registers.vram_addr ^= 0x0800; // toggle nametable Y
            } else if coarse_y == 31 {
                coarse_y = 0; // wrap without toggling nametable
            } else {
                coarse_y += 1;
            }
            self.registers.vram_addr = (self.registers.vram_addr & !0x03E0) | (coarse_y << 5);
        }
    }

    /// Copy horizontal scroll bits (coarse X + nametable X) from temp to vram.
    /// Called at dot 257 of each visible/pre-render scanline when rendering is enabled.
    fn reload_horizontal_from_temp(&mut self) {
        // Horizontal bits: coarse X (bits 0-4) and nametable X (bit 10)
        self.registers.vram_addr =
            (self.registers.vram_addr & !0x041F) | (self.registers.temp_vram_addr & 0x041F);
    }

    /// Copy vertical scroll bits (coarse Y + nametable Y + fine Y) from temp to vram.
    /// Called at dots 280-304 of the pre-render scanline when rendering is enabled.
    fn reload_vertical_from_temp(&mut self) {
        // Vertical bits: coarse Y (bits 5-9), nametable Y (bit 11), fine Y (bits 12-14)
        self.registers.vram_addr =
            (self.registers.vram_addr & !0x7BE0) | (self.registers.temp_vram_addr & 0x7BE0);
    }

    pub fn dump_state(&self, _cartridge: &Cartridge) -> String {
        let mut out = String::new();

        let _ = writeln!(out, "# PPU State");
        let _ = writeln!(out, "- Frame: {}", self.frame_no);
        let _ = writeln!(
            out,
            "- Timing: scanline {}, dot {}",
            self.scanline, self.dot
        );
        let cur_name_table_addr = 0x2000 + (self.registers.ctrl.name_table_select() as u16 * 0x400);
        let _ = writeln!(
            out,
            "- Current nametable address: 0x{:04X}",
            cur_name_table_addr
        );
        let _ = writeln!(out, "- PPUCTRL: 0x{:02X}", self.registers.ctrl.into_bits());
        let _ = writeln!(out, "- PPUMASK: 0x{:02X}", self.registers.mask.into_bits());
        let _ = writeln!(
            out,
            "- PPUSTATUS: 0x{:02X}",
            self.registers.status.into_bits()
        );
        let _ = writeln!(out, "- OAMADDR: 0x{:02X}", self.registers.oam_addr);
        let _ = writeln!(out, "- VRAM address: 0x{:04X}", self.registers.vram_addr);
        let _ = writeln!(
            out,
            "- Temporary VRAM address: 0x{:04X}",
            self.registers.temp_vram_addr
        );
        let _ = writeln!(out, "- Fine X: 0x{:02X}", self.registers.fine_x);
        let _ = writeln!(out, "- Write toggle: {}", self.registers.write_toggle);
        let _ = writeln!(out, "- Scroll X/Y: {:?}", self.scroll_xy());

        out
    }

    pub fn rendering_enabled(&self) -> bool {
        self.effective_mask.background_enabled() || self.effective_mask.sprite_enabled()
    }

    pub fn in_vblank(&self) -> bool {
        self.registers.status.v_blank()
    }

    pub fn oam_dma(&mut self, vals: &[u8; 256]) {
        for (addr, value) in vals.iter().copied().enumerate() {
            self.registers.oam_data[addr] = normalize_oam_byte(addr as u8, value);
        }
        // cache-free: nothing to mark
    }

    pub fn tick(&mut self, cartridge: &mut Cartridge) {
        self.ppu_ticks = self.ppu_ticks.wrapping_add(1);
        let rendering_enabled = self.rendering_enabled();

        if self.dot == 0 {
            self.rendering_enabled_at_scanline_start = rendering_enabled;
        }

        self.sprite.update_ctrl_status(&mut self.registers);

        // MMC3 scanline IRQs are driven by filtered PPU A12 rises.
        //
        // On the real NES the PPU address bus toggles A12 on each tile
        // fetch.  The MMC3 filters the rapid bouncing and effectively
        // clocks its IRQ counter once per scanline at the point where A12
        // rises from a sustained low period:
        //
        //   * When BG uses $0xxx and sprites use $1xxx: the rise occurs at
        //     the first sprite pattern fetch (~dot 260).
        //   * When BG uses $1xxx and sprites use $0xxx: the rise occurs at
        //     the first BG pattern fetch (~dot 4), with a second rise at
        //     the BG-prefetch after sprite fetches (~dot 324).
        //
        // We model this by emitting only pattern-table addresses (skipping
        // the nametable/attribute-table fetches that cause the filtered
        // bouncing).  BG pattern addresses are emitted on odd dots and
        // sprite pattern addresses on even dots, matching the real PPU's
        // memory-access phasing.
        //
        // Manual $2006/$2007 writes still reach the mapper directly.
        if rendering_enabled
            && (self.scanline < 240
                || (self.scanline == 261 && self.rendering_enabled_at_scanline_start))
        {
            let fetch_addr = if ((1..=256).contains(&self.dot) || (321..=336).contains(&self.dot))
                && self.dot % 2 == 1
            {
                // BG tile fetch: only pattern-table accesses (fetch types 2-3).
                // Notifications are shifted +2 dots (base 3/323 instead of 1/321)
                // so that the first BG pattern appears at dot 7 rather than dot 5.
                // Together with the sprite base (first pattern at dot 263), this
                // gives the real hardware's 256-dot gap between mode $10 and $08.
                let base_dot = if self.dot <= 256 { 3 } else { 323 };
                if self.dot < base_dot {
                    None
                } else {
                    let fetch_type = (((self.dot - base_dot) / 2) % 4) as usize;
                    if fetch_type >= 2 {
                        Some(if self.registers.ctrl.background_pattern_table() {
                            0x1000
                        } else {
                            0x0000
                        })
                    } else {
                        None
                    }
                }
            } else if (259..=320).contains(&self.dot) && self.dot % 2 == 1 {
                // Sprite tile fetch: only pattern-table accesses (fetch types 2-3).
                // Emitted on odd dots to align the MMC3 counter clock with the
                // observed ~dot-260 hardware timing.
                let fetch_type = (((self.dot - 259) / 2) % 4) as usize;
                if fetch_type >= 2 {
                    Some(
                        if self.registers.ctrl.sprite_size()
                            || self.registers.ctrl.sprite_pattern_table()
                        {
                            0x1000
                        } else {
                            0x0000
                        },
                    )
                } else {
                    None
                }
            } else {
                None
            };

            if let Some(fetch_addr) = fetch_addr {
                cartridge.notify_vram_address(fetch_addr);
            }
        }

        if self.scanline < 240 && self.dot == 0 {
            // compute background anchor at start of visible scanline
            self.background_anchor = Some(BackgroundActivation::snapshot(self, 0));
            self.pending_background_activation = None;
        }

        if self.scanline < 240 && self.dot == 65 {
            self.sprite.begin_sprite_overflow_eval(self.scanline);
        }

        if rendering_enabled
            && self.scanline < 240
            && (65..=256).contains(&self.dot)
            && self.dot % 2 == 1
        {
            self.sprite.step_sprite_overflow_eval(
                self.scanline,
                self.registers.ctrl.sprite_size(),
                &self.registers.oam_data,
            );
        }

        // Visible pixels are output on dots 1-256; dot 0 is the idle fetch slot.
        if self.scanline < 240 && (1..=256).contains(&self.dot) {
            let x = (self.dot - 1) as u8;
            let pixel = if rendering_enabled {
                self.render_pixel(x, cartridge)
            } else {
                self.palette
                    .render_disabled_color(self.registers.vram_addr % 0x4000)
            };
            let pixel = self.effective_mask.apply_effects(pixel);
            self.renderer
                .set_pixel(x as u32, self.scanline as u32, pixel.0);
        }

        // --- vram_addr management (rendering-enabled only) ---
        // These model the real NES PPU's internal v-register updates:
        if rendering_enabled {
            // Fine Y increment at dot 256 of each visible scanline
            if self.scanline < 240 && self.dot == 256 {
                self.increment_vram_y();
            }
            // Horizontal bits reload from temp at dot 257
            // (visible scanlines and pre-render scanline)
            if (self.scanline < 240 || self.scanline == VBLANK_CLEAR_SCANLINE) && self.dot == 257 {
                self.reload_horizontal_from_temp();
            }
            // Vertical bits reload from temp at dots 280-304 of pre-render scanline
            if self.scanline == VBLANK_CLEAR_SCANLINE && (280..=304).contains(&self.dot) {
                self.reload_vertical_from_temp();
            }
        }

        // At end of pre-render scanline (261), clear screen for next frame
        if rendering_enabled && self.scanline == VBLANK_CLEAR_SCANLINE && self.dot == 0 {
            let bg_color = self.palette.black_color();
            self.renderer.clear(bg_color.0);
        }

        if self.scanline == VBLANK_SET_SCANLINE && self.dot == VBLANK_SET_DOT {
            self.renderer.finish();
            if !self.suppress_vblank_for_current_frame {
                self.registers.status.set_v_blank(true);
            }
        }

        // VBlank clear: pre-render scanline 261, dot 1
        if self.scanline == VBLANK_CLEAR_SCANLINE && self.dot == VBLANK_CLEAR_DOT {
            self.registers.status.set_v_blank(false);
            self.registers.status.set_sprite_overflow(false);
            self.registers.status.set_sprite_zero_hit(false);
            self.sprite.clear_pending();
        }

        // On odd frames with rendering enabled, the pre-render scanline skips the
        // final idle dot and wraps directly to scanline 0 dot 0.
        if self.scanline == VBLANK_CLEAR_SCANLINE
            && self.dot == DOTS_PER_SCANLINE - 2
            && self.odd_frame
            && rendering_enabled
        {
            self.dot = 0;
            self.scanline = 0;
            self.odd_frame = !self.odd_frame;
            self.suppress_vblank_for_current_frame = false;
            self.suppress_nmi_for_current_frame = false;
            self.frame_no += 1;
            return;
        }

        // Advance dot/scanline counters
        self.dot += 1;
        if self.dot >= DOTS_PER_SCANLINE {
            self.dot = 0;
            self.scanline += 1;
            if self.scanline >= SCANLINES_PER_FRAME {
                self.scanline = 0;
                self.odd_frame = !self.odd_frame;
                self.suppress_vblank_for_current_frame = false;
                self.suppress_nmi_for_current_frame = false;
                self.frame_no += 1;
            }
        }

        self.effective_mask = self.registers.mask;
    }

    /// Return ppu nmi signal, it connect to Cpu nmi input line
    pub fn nmi_line_out(&self) -> bool {
        self.registers.status.v_blank()
            && self.registers.ctrl.nmi_enable()
            && !self.suppress_nmi_for_current_frame
    }

    /// Read by cpu memory bus. Uses Cartridge for CHR/name-table access.
    pub fn read(&mut self, address: u16, cartridge: &mut Cartridge) -> u8 {
        // PPU registers are mirrored every 8 bytes in range $2000-$3FFF
        let reg = normalize_ppu_addr(address);
        match reg {
            // PPUSTATUS: bits 7-5 from status, bits 4-0 from bus latch
            0x2002 => {
                let status = self.read_status();
                self.registers.write_toggle = false;
                let status_bits = status.into_bits();
                let result = (status_bits & 0xE0) | (self.current_bus_latch() & 0x1F);
                self.refresh_bus_latch_bits(0xE0, status_bits);
                result
            }
            // OAMDATA
            0x2004 => {
                let result = self.read_oam_data();
                self.refresh_bus_latch(result);
                result
            }
            // PPUDATA - read from VRAM or palette
            0x2007 => self.read_vram_and_inc(cartridge),
            // Other registers are write-only: return open bus (latch)
            _ => self.current_bus_latch(),
        }
    }

    pub fn peek(&self, address: u16, cartridge: &Cartridge) -> u8 {
        let reg = normalize_ppu_addr(address);
        match reg {
            0x2002 => self.registers.status.into_bits(),
            0x2004 => self.read_oam_data(),
            0x2007 => self.read_vram(self.registers.vram_addr, cartridge),
            _ => 0,
        }
    }

    /// Write by cpu memory bus. Uses Cartridge for CHR/name-table writes.
    pub fn write(&mut self, address: u16, value: u8, cartridge: &mut Cartridge) {
        self.refresh_bus_latch(value);
        let reg = normalize_ppu_addr(address);
        match reg {
            // PPUCTRL
            0x2000 => {
                cartridge.on_ppu_ctrl_write(value);
                self.set_control_flags(PpuCtrl::from_bits(value));
                self.schedule_background_activation_if_visible();
                // Update name table selection
            }
            // PPUMASK
            0x2001 => {
                cartridge.on_ppu_mask_write(value);
                self.registers.mask = PpuMask::from_bits(value);
                // mask changed; no cache to mark
            }
            // OAMADDR
            0x2003 => {
                self.registers.oam_addr = value;
            }
            // OAMDATA
            0x2004 => {
                let addr = self.registers.oam_addr as usize;
                self.registers.oam_data[addr] = normalize_oam_byte(self.registers.oam_addr, value);
                self.registers.oam_addr = self.registers.oam_addr.wrapping_add(1);
                // OAM mutated; no cache to mark
            }
            // PPUSCROLL
            0x2005 => {
                cartridge.on_ppu_scroll_write(value);
                self.write_scroll(value);
                // scroll changed; no cache to mark
            }
            // PPUADDR
            0x2006 => {
                self.write_vram_addr(value);
                // vram addr updated; no cache to mark
                cartridge.notify_vram_address(self.registers.vram_addr);
            }
            // PPUDATA
            0x2007 => {
                self.write_vram(self.registers.vram_addr, value, cartridge);
                self.registers
                    .ctrl
                    .inc_ppu_addr(&mut self.registers.vram_addr);
                cartridge.notify_vram_address(self.registers.vram_addr);
                // VRAM written; no cache to mark
            }
            _ => {} // Ignore other addresses
        }
    }

    fn read_vram(&self, address: u16, cartridge: &Cartridge) -> u8 {
        let mut addr = address % 0x4000;
        if (0x3000..0x3f00).contains(&addr) {
            addr -= 0x1000;
        }
        if addr < 0x3f00 {
            // Read from pattern table or name table
            if addr < 0x2000 {
                // Pattern table (CHR ROM/RAM)
                cartridge.read_chr(addr, PatternAccess::Cpu)
            } else {
                // Name table
                self.read_name_table_byte(addr, cartridge)
            }
        } else {
            // Palette
            self.palette.read(addr)
        }
    }

    fn write_vram(&mut self, address: u16, value: u8, cartridge: &mut Cartridge) {
        let mut addr = address % 0x4000;
        if (0x3000..0x3f00).contains(&addr) {
            addr -= 0x1000;
        }
        if addr < 0x3f00 {
            if addr >= 0x2000 {
                // Name table (pattern table 0x0000-0x1FFF is read-only for CHR ROM)
                cartridge.write_nametable(addr, value);
            } else {
                cartridge.write_pattern(addr, value);
            }
        } else {
            self.palette.write(addr, value);
        }
    }

    fn write_scroll(&mut self, value: u8) {
        self.registers.write_scroll(value);
    }

    fn write_vram_addr(&mut self, value: u8) {
        if self.registers.write_vram_addr(value) {
            self.schedule_background_activation_if_visible();
        }
    }

    fn read_name_table_byte(&self, address: u16, cartridge: &Cartridge) -> u8 {
        cartridge.read_nametable(address)
    }

    fn read_pattern_pixel(
        cartridge: &Cartridge,
        base_addr: u16,
        tile_idx: u8,
        tile_x: usize,
        tile_y: usize,
        access: PatternAccess,
    ) -> u8 {
        let tile_addr = base_addr + tile_idx as u16 * 16;
        let low = cartridge.read_chr(tile_addr + tile_y as u16, access);
        let high = cartridge.read_chr(tile_addr + tile_y as u16 + 8, access);
        let bit = 7 - tile_x;
        ((low >> bit) & 1) | (((high >> bit) & 1) << 1)
    }

    fn get_background_pixel(&mut self, cartridge: &Cartridge, screen_x: u8) -> (u8, u8) {
        self.apply_pending_background_activation(screen_x);

        // Check left-column clipping
        if !self.effective_mask.background_left_enabled() && screen_x < 8 {
            return (0, 0);
        }

        // On real hardware, the PPU fetches tiles from the position encoded in
        // vram_addr (the "v" register), which is maintained by fine-Y increments
        // and horizontal/vertical reloads from temp_vram_addr (the "t" register).
        let background = self
            .background_anchor
            .unwrap_or_else(|| BackgroundActivation::snapshot(self, 0));

        let coarse_x = (background.vram_addr & 0x001F) as u16;
        let coarse_y = ((background.vram_addr >> 5) & 0x001F) as u16;
        let fine_y = ((background.vram_addr >> 12) & 0x0007) as usize;
        let nt_select = ((background.vram_addr >> 10) & 0x03) as u8;

        // Background fetches are pipelined ahead of the pixel currently being output.
        let world_x = coarse_x * 8 + background.fine_x as u16 + screen_x as u16;
        // Vertical: directly from vram_addr (no screen_y addition; vram_addr is
        // incremented each scanline by the fine-Y increment at dot 256)
        let world_y = coarse_y * 8 + fine_y as u16;

        let nt_select_x = (((nt_select & 0x01) as u16) + (world_x / 256)) & 0x01;
        let nt_select_y = (((nt_select >> 1) as u16) + (world_y / 240)) & 0x01;
        let nt_idx = ((nt_select_y << 1) | nt_select_x) as u8;

        let nt_x = ((world_x % 256) / 8) as u8;
        let nt_y = ((world_y % 240) / 8) as u8;
        let tile_fine_x = (world_x % 8) as usize;
        let tile_fine_y = (world_y % 8) as usize;

        let nt_base = 0x2000 + nt_idx as u16 * 0x0400;
        let nt_addr = nt_base + nt_y as u16 * TILES_PER_ROW as u16 + nt_x as u16;
        let tile_idx = self.read_name_table_byte(nt_addr, cartridge);
        let attr_addr = nt_base + 0x03c0 + (nt_y as u16 / 4) * 8 + (nt_x as u16 / 4);
        let attr_byte = self.read_name_table_byte(attr_addr, cartridge);
        let shift = (((nt_y >> 1) & 0x01) << 2) | (((nt_x >> 1) & 0x01) << 1);
        let palette_idx = (attr_byte >> shift) & 0x03;

        let screen_y = self.scanline as u8;
        if let Some(override_pixel) = cartridge.background_override(
            screen_x,
            screen_y,
            nt_addr,
            tile_idx,
            palette_idx,
            tile_fine_x,
            tile_fine_y,
        ) {
            return (override_pixel.palette_idx, override_pixel.color_idx);
        }

        let base_addr = if background.ctrl.background_pattern_table() {
            0x1000
        } else {
            0x0000
        };
        let color_idx = Self::read_pattern_pixel(
            cartridge,
            base_addr,
            tile_idx,
            tile_fine_x,
            tile_fine_y,
            PatternAccess::Background,
        );

        (palette_idx, color_idx)
    }

    fn read_vram_and_inc(&mut self, cartridge: &mut Cartridge) -> u8 {
        let vram_addr = self.registers.vram_addr;
        let current = self.read_vram(vram_addr, cartridge);
        self.registers
            .ctrl
            .inc_ppu_addr(&mut self.registers.vram_addr);
        cartridge.notify_vram_address(self.registers.vram_addr);

        // Non-palette addresses use a read buffer (delayed by one read).
        // Palette addresses ($3F00-$3FFF) return immediately, but still
        // update the buffer with the nametable byte "underneath".
        let addr = vram_addr % 0x4000;
        if addr >= 0x3F00 {
            // Palette: fill buffer with the nametable data underneath
            // (mirrored from $2F00-$2FFF)
            self.registers.ppudata_buffer = self.read_vram(vram_addr - 0x1000, cartridge);
            let result = (current & 0x3F) | (self.current_bus_latch() & 0xC0);
            self.refresh_bus_latch_bits(0x3F, result);
            result
        } else {
            let buffered = self.registers.ppudata_buffer;
            self.registers.ppudata_buffer = current;
            self.refresh_bus_latch(buffered);
            buffered
        }
    }

    fn current_bus_latch(&mut self) -> u8 {
        self.registers.current_bus_latch(self.ppu_ticks)
    }

    fn refresh_bus_latch(&mut self, value: u8) {
        self.registers.refresh_bus_latch(value, self.ppu_ticks);
    }

    fn refresh_bus_latch_bits(&mut self, mask: u8, value: u8) {
        self.registers
            .refresh_bus_latch_bits(mask, value, self.ppu_ticks);
    }

    /// Read OAM data at current OAM address (for testing)
    fn read_oam_data(&self) -> u8 {
        self.registers.read_oam_data()
    }

    /// Get OAM data as a copy (for debugging/inspection)
    pub fn get_oam_data(&self) -> [u8; 256] {
        self.registers.oam_data
    }

    /// Set control flags (for testing)
    fn set_control_flags(&mut self, flags: PpuCtrl) {
        self.registers.set_control_flags(flags);
    }

    /// Read status register (for testing) - behaves like reading from 0x2002
    /// Returns the current status and clears the v_blank flag
    fn read_status(&mut self) -> PpuStatus {
        let r = self.registers.status;
        if self.scanline == VBLANK_SET_SCANLINE {
            match self.dot {
                1 => {
                    self.suppress_vblank_for_current_frame = true;
                    self.suppress_nmi_for_current_frame = true;
                }
                2..=4 => {
                    self.suppress_nmi_for_current_frame = true;
                }
                _ => {}
            }
        }
        // Clear v_blank flag on read
        self.registers.status.set_v_blank(false);
        self.registers.write_toggle = false; // Also reset write toggle on status read
        r
    }

    fn render_pixel(&mut self, x: u8, cartridge: &mut Cartridge) -> Pixel {
        // Compute background pixel on-the-fly (no cache) so that mid-scanline
        // register writes ($2000, $2001, $2005, $2006) take effect immediately.
        let (bg_palette_idx, bg_color_idx) = if self.effective_mask.background_enabled() {
            self.get_background_pixel(cartridge, x)
        } else {
            (0, 0) // transparent
        };

        let sprite_pixel = if self.effective_mask.sprite_enabled() {
            // compute sprite pixel on-the-fly for this x and current scanline
            self.sprite.find_sprite_pixel(
                &self.registers.oam_data,
                self.registers.ctrl.sprite_size(),
                self.registers.ctrl.sprite_pattern_table(),
                self.effective_mask.sprite_left_enabled(),
                cartridge,
                x,
                self.scanline as u8,
                |cartridge, base_addr, tile_idx, tile_x, tile_y, access| {
                    Self::read_pattern_pixel(cartridge, base_addr, tile_idx, tile_x, tile_y, access)
                },
            )
        } else {
            None
        };

        if self.effective_mask.background_enabled()
            && self.effective_mask.sprite_enabled()
            && bg_color_idx != 0
            && x != 255
            && (x >= 8 || self.effective_mask.background_left_enabled())
            && (x >= 8 || self.effective_mask.sprite_left_enabled())
            && self.sprite.sprite_zero_opaque_at(
                &self.registers.oam_data,
                self.registers.ctrl.sprite_size(),
                self.registers.ctrl.sprite_pattern_table(),
                cartridge,
                x,
                self.scanline as u8,
                |cartridge, base_addr, tile_idx, tile_x, tile_y, access| {
                    Self::read_pattern_pixel(cartridge, base_addr, tile_idx, tile_x, tile_y, access)
                },
            )
        {
            self.sprite.set_zero_hit_pending();
        }

        match sprite_pixel {
            Some(sprite_pixel) => {
                if bg_color_idx == 0 || !sprite_pixel.behind_bg {
                    // Background is transparent OR sprite has priority
                    self.palette
                        .get_sprit_color(sprite_pixel.palette_idx, sprite_pixel.color_idx)
                } else {
                    // Background has priority and is opaque
                    self.palette
                        .get_background_color(bg_palette_idx, bg_color_idx)
                }
            }
            None => {
                // No sprite, just show background
                self.palette
                    .get_background_color(bg_palette_idx, bg_color_idx)
            }
        }
    }
}

#[cfg(test)]
mod tests;
