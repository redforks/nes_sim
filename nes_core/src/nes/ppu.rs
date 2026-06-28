mod nametable;
mod oam;
pub mod palette;
mod registers;
mod sprite;

use crate::{
    mcu::Mcu,
    nes::{
        mapper::{Cartridge, CartridgeOperation, Mirroring, PpuCapabilities},
        ppu::{
            palette::ColorTheme,
            sprite::SpriteManager,
        },
    },
    render::Render,
};
use nametable::Nametable;
use oam::{Oam, TilePosition};
use palette::{Palette, Pixel};
use registers::{PpuCtrl, PpuMask, PpuStatus, Registers};

// Pixel, palette data and COLORS are defined in the submodule `palette`.

// PPU Timing Constants
const PPU_OPEN_BUS_DECAY_TICKS: u64 = 3_221_591 * crate::SYSTEM_CYCLES_PER_PPU_CYCLE;
const VBLANK_CLEAR_SCANLINE: u16 = 261;

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

#[derive(Copy, Clone, Default)]
pub struct Timing {
    scanline: u16,
    dot: u16,
    odd_frame: bool,
    frame_no: usize,
}

impl Timing {
    fn new() -> Self {
        Self {
            scanline: 0,
            dot: 0,
            odd_frame: false,
            frame_no: 0,
        }
    }

    fn reset(&mut self) {
        self.odd_frame = false;
    }

    fn next_visible_x(&self) -> Option<u8> {
        self.is_visible().then(|| (self.dot - 1) as u8)
    }

    /// Return true if scanline and dot is in visible area
    fn is_visible(&self) -> bool {
        self.scanline < 240 && (1..=256).contains(&self.dot)
    }

    fn advance(&mut self, rendering_enabled: bool) {
        match self.dot {
            ..339 => self.dot += 1,
            339 if self.scanline != 261 || (!self.odd_frame || !rendering_enabled) => self.dot += 1,
            _ => {
                self.dot = 0;
                self.scanline += 1;
                if self.scanline >= 262 {
                    self.scanline = 0;
                    self.odd_frame = !self.odd_frame;
                    self.frame_no += 1;
                }
            }
        }
    }

    /// Return true if just enter vblank timing, scanline 241, dot 1, should set vblank flag
    fn enter_vblank(&self) -> bool {
        self.scanline == 241 && self.dot == 1
    }

    /// Return true if just leave vblank timing, scanline 261, dot 1, should clear vblank flag
    fn leave_vblank(&self) -> bool {
        self.scanline == 261 && self.dot == 1
    }

    pub fn frame_no(&self) -> usize {
        self.frame_no
    }

    pub fn scanline(&self) -> u16 {
        self.scanline
    }

    pub fn dot(&self) -> u16 {
        self.dot
    }
}

pub struct Ppu<R: Render = ()> {
    registers: Registers,
    palette: Palette,
    oam: Oam,
    oam_addr: u8,
    color_theme: ColorTheme,
    nametable: Nametable,
    renderer: R,
    cartridge: Box<dyn Cartridge>,
    cartridge_caps: PpuCapabilities,

    timing: Timing,

    background_anchor: Option<BackgroundActivation>,
    pending_background_activation: Option<BackgroundActivation>,
    sprite: SpriteManager,
    /// system clock when suppress nmi by reading status register
    suppressed_vblank_at: Option<u64>,

    /// PPU mask register changes are not visible until the next PPU tick;
    /// this delayed copy models the 1-dot internal pipeline delay.
    effective_mask: PpuMask,
    rendering_enabled_at_scanline_start: bool,

    /// Cumulative system cycle counter, incremented each tick.
    /// Replaces the global `get_system_cycles()` for PPU-internal timing.
    cycle: u64,
}

/// PPU registers are mirrored every 8 bytes in range $2000-$3FFF
/// this function normalized $2000, $2008, .. $3ff8 to $2000
fn normalize_ppu_addr(addr: u16) -> u16 {
    addr & 0x2007
}

impl<R: Render> Ppu<R> {
    pub fn new(renderer: R, mirroring: Mirroring, cartridge: Box<dyn Cartridge>) -> Self {
        let cartridge_caps = cartridge.ppu_capabilities();
        Ppu {
            registers: Registers::new(),
            palette: Palette::default(),
            oam: Oam::default(),
            oam_addr: 0,
            color_theme: ColorTheme::default(),
            nametable: Nametable::new(mirroring),
            renderer,
            cartridge,
            cartridge_caps,
            timing: Timing::new(),
            background_anchor: None,
            pending_background_activation: None,
            sprite: SpriteManager::new(),
            suppressed_vblank_at: None,
            effective_mask: PpuMask::new(),
            rendering_enabled_at_scanline_start: false,
            cycle: 0,
        }
    }

    pub fn set_color_theme(&mut self, theme: ColorTheme) {
        self.color_theme = theme;
    }

    pub fn reset(&mut self) {
        // https://www.nesdev.org/wiki/PPU_power_up_state
        self.registers.reset();
        self.effective_mask = PpuMask::new();
        self.background_anchor = None;
        self.pending_background_activation = None;
        self.timing.reset();
        self.sprite.reset();
        self.registers.write_scroll(0);
        self.rendering_enabled_at_scanline_start = false;
    }

    pub fn timing(&self) -> &Timing {
        &self.timing
    }

    pub fn cartridge_irq_pending(&self) -> bool {
        if !self.cartridge_caps.irq_pending {
            return false;
        }
        self.cartridge.irq_pending()
    }

    pub fn renderer(&self) -> &R {
        &self.renderer
    }

    pub fn renderer_mut(&mut self) -> &mut R {
        &mut self.renderer
    }

    fn schedule_background_activation_if_visible(&mut self) {
        if self.rendering_enabled()
            && let Some(screen_x) = self.timing.next_visible_x()
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

    pub fn rendering_enabled(&self) -> bool {
        self.effective_mask.background_enabled() || self.effective_mask.sprite_enabled()
    }

    pub fn in_vblank(&self) -> bool {
        self.registers.status.v_blank()
    }

    pub fn write_oam_data(&mut self, value: u8) {
        fn normalize_oam_byte(addr: u8, value: u8) -> u8 {
            if addr & 0x03 == 0x02 {
                value & 0xE3
            } else {
                value
            }
        }

        let addr = self.oam_addr;
        self.oam.as_bytes_mut()[(addr as usize) & 0xff] = normalize_oam_byte(addr, value);
        self.oam_addr = addr.wrapping_add(1);
    }

    pub fn tick(&mut self) {
        self.cycle += 1;
        let rendering_enabled = self.rendering_enabled();
        let (prev_scanline, _prev_dot) = (self.timing.scanline, self.timing.dot);

        if self.timing.dot == 0 {
            self.rendering_enabled_at_scanline_start = rendering_enabled;
        }

        self.sprite.update_ctrl_status(&mut self.registers.status);

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
        if self.cartridge_caps.notify_vram_address
            && rendering_enabled
            && (self.timing.scanline < 240
                || (self.timing.scanline == 261 && self.rendering_enabled_at_scanline_start))
            && self.timing.dot % 2 == 1
        {
            match self.timing.dot {
                1..=256 | 321..=336 => {
                    // BG tile fetch: only pattern-table accesses (fetch types 2-3).
                    // Notifications are shifted +2 dots (base 3/323 instead of 1/321)
                    // so that the first BG pattern appears at dot 7 rather than dot 5.
                    // Together with the sprite base (first pattern at dot 263), this
                    // gives the real hardware's 256-dot gap between mode $10 and $08.
                    let base_dot = if self.timing.dot <= 256 { 3 } else { 323 };
                    if self.timing.dot >= base_dot {
                        let fetch_type = (((self.timing.dot - base_dot) / 2) % 4) as usize;
                        if fetch_type >= 2 {
                            self.cartridge.notify_vram_address(
                                if self.registers.ctrl.background_pattern_table() {
                                    0x1000
                                } else {
                                    0x0000
                                },
                            );
                        }
                    }
                }
                259..=320 => {
                    // Sprite tile fetch: only pattern-table accesses (fetch types 2-3).
                    // Emitted on odd dots to align the MMC3 counter clock with the
                    // observed ~dot-260 hardware timing.
                    let fetch_type = (((self.timing.dot - 259) / 2) % 4) as usize;
                    if fetch_type >= 2 {
                        self.cartridge.notify_vram_address(
                            if self.registers.ctrl.sprite_size_16()
                                || self.registers.ctrl.sprite_pattern_table()
                            {
                                0x1000
                            } else {
                                0x0000
                            },
                        );
                    }
                }
                _ => {}
            }
        }

        if self.timing.scanline < 240 && self.timing.dot == 0 {
            // compute background anchor at start of visible scanline
            self.background_anchor = Some(BackgroundActivation::snapshot(self, 0));
            self.pending_background_activation = None;
        }

        if self.timing.dot == 0 {
            self.sprite.swap_secondary_oam();
        }

        if rendering_enabled
            && (self.timing.scanline < 240 || self.timing.scanline == 261)
            && self.timing.dot == 65 {
            self.sprite.begin_sprite_overflow_eval(self.timing.scanline);
        }

        if rendering_enabled
            && (self.timing.scanline < 240 || self.timing.scanline == 261)
            && (65..=256).contains(&self.timing.dot)
            && self.timing.dot % 2 == 1
        {
            self.sprite.step_sprite_overflow_eval(
                self.timing.scanline,
                self.registers.ctrl,
                &self.oam,
            );
        }

        // Visible pixels are output on dots 1-256; dot 0 is the idle fetch slot.
        if self.timing.is_visible() {
            let x = (self.timing.dot - 1) as u8;
            let pixel_idx = if rendering_enabled {
                self.render_pixel(x)
            } else {
                self.palette.disabled_color_index(self.registers.vram_addr)
            };
            let pixel = self
                .effective_mask
                .apply_effects(self.color_theme.color(pixel_idx));
            self.renderer
                .set_pixel(x as u32, self.timing.scanline as u32, pixel.0);
        }

        // --- vram_addr management (rendering-enabled only) ---
        // These model the real NES PPU's internal v-register updates:
        if rendering_enabled {
            // Fine Y increment at dot 256 of each visible scanline
            if self.timing.scanline < 240 && self.timing.dot == 256 {
                self.increment_vram_y();
            }
            // Horizontal bits reload from temp at dot 257
            // (visible scanlines and pre-render scanline)
            if (self.timing.scanline < 240 || self.timing.scanline == VBLANK_CLEAR_SCANLINE)
                && self.timing.dot == 257
            {
                self.reload_horizontal_from_temp();
            }
            // Vertical bits reload from temp at dots 280-304 of pre-render scanline
            if self.timing.scanline == VBLANK_CLEAR_SCANLINE
                && (280..=304).contains(&self.timing.dot)
            {
                self.reload_vertical_from_temp();
            }
        }

        if self.timing.enter_vblank() {
            self.renderer.finish();
            if self
                .suppressed_vblank_at
                .take()
                .is_none_or(|clock| self.cycle - clock > 1)
            {
                self.registers.status.set_v_blank(true);
            }
        }

        if self.timing.leave_vblank() {
            self.registers.status.set_v_blank(false);
            self.registers.status.set_sprite_overflow(false);
            self.registers.status.set_sprite_zero_hit(false);
            self.sprite.clear_pending();
        }

        self.timing.advance(rendering_enabled);

        self.effective_mask = self.registers.mask;
        if self.cartridge_caps.on_ppu_tick {
            self.cartridge.on_ppu_tick(prev_scanline);
        }
    }

    /// Return ppu nmi signal, it connect to Cpu nmi input line
    pub fn nmi_line_out(&self) -> bool {
        self.registers.status.v_blank() && self.registers.ctrl.nmi_enable()
    }

    fn read_ppureg(&mut self, address: u16) -> u8 {
        let reg = normalize_ppu_addr(address);
        match reg {
            0x2002 => {
                let status = self.read_status();
                self.registers.write_toggle = false;
                let status_bits = status.into_bits();
                let result = (status_bits & 0xE0) | (self.current_bus_latch() & 0x1F);
                self.refresh_bus_latch_bits(0xE0, status_bits);
                result
            }
            0x2004 => {
                let result = self.read_oam_data();
                self.refresh_bus_latch(result);
                result
            }
            0x2007 => self.read_vram_and_inc(),
            _ => self.current_bus_latch(),
        }
    }

    pub fn peek(&self, address: u16) -> u8 {
        match address {
            0x2000..=0x3fff => {
                let reg = normalize_ppu_addr(address);
                match reg {
                    0x2002 => self.registers.status.into_bits(),
                    0x2004 => self.read_oam_data(),
                    0x2007 => self.read_vram(self.registers.vram_addr),
                    _ => 0,
                }
            }
            0x4100..=0xffff => self.cartridge.read(address),
            _ => 0,
        }
    }

    fn write_ppureg(&mut self, address: u16, value: u8) {
        self.refresh_bus_latch(value);
        let reg = normalize_ppu_addr(address);
        match reg {
            // PPUCTRL
            0x2000 => {
                self.set_control_flags(PpuCtrl::from_bits(value));
                self.schedule_background_activation_if_visible();
            }
            // PPUMASK
            0x2001 => {
                self.registers.mask = PpuMask::from_bits(value);
            }
            // OAMADDR
            0x2003 => {
                self.oam_addr = value;
            }
            // OAMDATA
            0x2004 => {
                self.write_oam_data(value);
            }
            // PPUSCROLL
            0x2005 => {
                self.write_scroll(value);
            }
            // PPUADDR
            0x2006 => {
                self.write_vram_addr(value);
                if self.cartridge_caps.notify_vram_address {
                    self.cartridge.notify_vram_address(self.registers.vram_addr);
                }
            }
            // PPUDATA
            0x2007 => {
                self.write_vram(self.registers.vram_addr, value);
                self.registers
                    .ctrl
                    .inc_ppu_addr(&mut self.registers.vram_addr);
                if self.cartridge_caps.notify_vram_address {
                    self.cartridge.notify_vram_address(self.registers.vram_addr);
                }
            }
            _ => {}
        }
    }

    pub fn read_vram(&self, address: u16) -> u8 {
        let mut addr = address % 0x4000;
        if (0x3000..0x3f00).contains(&addr) {
            addr -= 0x1000;
        }
        if addr < 0x3f00 {
            if addr < 0x2000 {
                self.cartridge.read_chr(addr)
            } else {
                self.nametable.read(addr)
            }
        } else {
            self.palette.read(addr)
        }
    }

    fn write_vram(&mut self, address: u16, value: u8) {
        let mut addr = address % 0x4000;
        if (0x3000..0x3f00).contains(&addr) {
            addr -= 0x1000;
        }
        if addr < 0x3f00 {
            if addr >= 0x2000 {
                self.nametable.write(addr, value);
            } else {
                self.cartridge.write_chr(addr, value);
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

    fn get_background_pixel(&mut self, screen_x: u8) -> (u8, u8) {
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

        let coarse_x = background.vram_addr & 0x001F ;
        let coarse_y = (background.vram_addr >> 5) & 0x001F ;
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
        let tile_fine_x = (world_x % 8) as u8;
        let tile_fine_y = (world_y % 8) as u8;

        let nt_base = 0x2000 + nt_idx as u16 * 0x0400;
        let nt_addr = nt_base + nt_y as u16 * 32 + nt_x as u16;
        let tile_idx = self.nametable.read(nt_addr);
        let attr_addr = nt_base + 0x03c0 + (nt_y as u16 / 4) * 8 + (nt_x as u16 / 4);
        let attr_byte = self.nametable.read(attr_addr);
        let shift = (((nt_y >> 1) & 0x01) << 2) | (((nt_x >> 1) & 0x01) << 1);
        let palette_idx = (attr_byte >> shift) & 0x03;

        let tile_position = background.ctrl.background_tile_position(tile_idx);
        let color_idx =
            read_pattern_pixel(&*self.cartridge, tile_position, tile_fine_x, tile_fine_y);

        (palette_idx, color_idx)
    }

    fn read_vram_and_inc(&mut self) -> u8 {
        let vram_addr = self.registers.vram_addr;
        let current = self.read_vram(vram_addr);
        self.registers
            .ctrl
            .inc_ppu_addr(&mut self.registers.vram_addr);
        if self.cartridge_caps.notify_vram_address {
            self.cartridge.notify_vram_address(self.registers.vram_addr);
        }

        // Non-palette addresses use a read buffer (delayed by one read).
        // Palette addresses ($3F00-$3FFF) return immediately, but still
        // update the buffer with the nametable byte "underneath".
        let addr = vram_addr % 0x4000;
        if addr >= 0x3F00 {
            // Palette: fill buffer with the nametable data underneath
            // (mirrored from $2F00-$2FFF)
            self.registers.ppudata_buffer = self.read_vram(vram_addr - 0x1000);
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
        self.registers.current_bus_latch(self.cycle)
    }

    fn refresh_bus_latch(&mut self, value: u8) {
        self.registers.refresh_bus_latch(value, self.cycle);
    }

    fn refresh_bus_latch_bits(&mut self, mask: u8, value: u8) {
        self.registers
            .refresh_bus_latch_bits(mask, value, self.cycle);
    }

    /// Read OAM data at current OAM address
    fn read_oam_data(&self) -> u8 {
        self.oam.as_bytes()[(self.oam_addr as usize) & 0xff]
    }

    /// Set control flags (for testing)
    fn set_control_flags(&mut self, flags: PpuCtrl) {
        self.registers.set_control_flags(flags);
    }

    /// Read status register (for testing) - behaves like reading from 0x2002
    /// Returns the current status and clears the v_blank flag
    fn read_status(&mut self) -> PpuStatus {
        let r = self.registers.status;
        if self.timing.enter_vblank() {
            self.suppressed_vblank_at = Some(self.cycle);
        }

        // Clear v_blank flag on read
        self.registers.status.set_v_blank(false);
        self.registers.write_toggle = false; // Also reset write toggle on status read
        r
    }

    fn render_pixel(&mut self, x: u8) -> u8 {
        let (bg_palette_idx, bg_color_idx) = if self.effective_mask.background_enabled() {
            self.get_background_pixel(x)
        } else {
            (0, 0)
        };

        let sprite_pixel = if self.effective_mask.sprite_enabled() {
            self.sprite.find_sprite_pixel(
                self.registers.ctrl,
                self.effective_mask,
                &*self.cartridge,
                x,
                self.timing.scanline as u8,
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
            && SpriteManager::sprite_zero_opaque_at(
                &self.oam,
                self.registers.ctrl,
                &*self.cartridge,
                x,
                self.timing.scanline as u8,
            ) {
                self.sprite.set_zero_hit_pending();
            }

        match sprite_pixel {
            Some(sprite_pixel) => {
                if bg_color_idx == 0 || !sprite_pixel.behind_bg {
                    self.palette
                        .get_sprite_color_index(sprite_pixel.palette_idx, sprite_pixel.color_idx)
                } else {
                    self.palette
                        .get_background_color_index(bg_palette_idx, bg_color_idx)
                }
            }
            None => self
                .palette
                .get_background_color_index(bg_palette_idx, bg_color_idx),
        }
    }
}

/// Read a pattern pixel from CHR data using a tile position.
///
/// For 8×8 tiles, `tile_y` is the vertical pixel offset within the tile (0..7).
/// For 16×8 tiles, `tile_y` can span two tiles (0..15).
fn read_pattern_pixel(
    cartridge: &dyn Cartridge,
    tile_position: TilePosition,
    tile_x: u8,
    tile_y: u8,
) -> u8 {
    let (low_addr, high_addr) = tile_position.resolve_pixel_addr(tile_y);
    let low = cartridge.read_chr(low_addr);
    let high = cartridge.read_chr(high_addr);
    let bit = 7 - tile_x;
    ((low >> bit) & 1) | (((high >> bit) & 1) << 1)
}

impl<R: Render> Mcu for Ppu<R> {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x2000..=0x3fff => self.read_ppureg(address),
            0x4100..=0xffff => self.cartridge.read(address),
            _ => 0,
        }
    }

    fn peek(&self, address: u16) -> u8 {
        self.peek(address)
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x2000..=0x3fff => self.write_ppureg(address, value),
            0x4020..=0x40ff => {}
            0x4100..=0xffff => {
                if let CartridgeOperation::UpdateNametableMirroring(mirroring) =
                    self.cartridge.write(address, value)
                {
                    self.nametable.set_mirroring(mirroring);
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests;
