use crate::mcu::Mcu;
use crate::render::{ImageRender, Render};
use crate::to_from_u8;
use image::Rgba;
use log::{debug, info};
use modular_bitfield::prelude::*;

mod name_table;
mod pattern;

pub use name_table::*;
pub use pattern::*;

type Pixel = Rgba<u8>;

/// Apply grayscale and emphasis effects to a pixel color.
/// - Grayscale: Converts color to grayscale by averaging RGB channels
/// - Emphasis: Attenuates non-emphasized color channels (e.g., red emphasis darkens G and B)
fn apply_effects(
    pixel: Pixel,
    grayscale: bool,
    red_tint: bool,
    green_tint: bool,
    blue_tint: bool,
) -> Pixel {
    let Rgba([r, g, b, a]) = pixel;

    // Apply emphasis (color emphasis darkens the non-emphasized channels)
    // Using ~0.75 attenuation factor (192/256)
    let r = if red_tint || (!green_tint && !blue_tint) {
        r
    } else {
        (r as u16 * 192 / 256) as u8
    };
    let g = if green_tint || (!red_tint && !blue_tint) {
        g
    } else {
        (g as u16 * 192 / 256) as u8
    };
    let b = if blue_tint || (!red_tint && !green_tint) {
        b
    } else {
        (b as u16 * 192 / 256) as u8
    };

    // Apply grayscale by averaging RGB channels
    let (r, g, b) = if grayscale {
        // Use luminance weights (ITU-R BT.601): 0.299R + 0.587G + 0.114B
        let gray = (r as u16 * 77 + g as u16 * 150 + b as u16 * 29) / 256;
        (gray as u8, gray as u8, gray as u8)
    } else {
        (r, g, b)
    };

    Rgba([r, g, b, a])
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct PpuCtrl {
    // Field declaration is LSB-first (first declared field maps to bit 0).
    // Arrange fields so they match the hardware PPUCTRL layout (bits 0..7):
    // bits 0-1: name_table_select
    // bit 2: increment_mode
    // bit 3: sprite_pattern_table
    // bit 4: background_pattern_table
    // bit 5: sprite_size
    // bit 6: ppu_master
    // bit 7: nmi_enable
    pub name_table_select: B2,
    pub increment_mode: bool,
    pub sprite_pattern_table: bool,
    pub background_pattern_table: bool,
    pub sprite_size: bool,
    pub ppu_master: bool,
    pub nmi_enable: bool,
}
to_from_u8!(PpuCtrl);

impl Default for PpuCtrl {
    fn default() -> Self {
        0u8.into()
    }
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct PpuMask {
    pub grayscale: bool,
    pub background_left_enabled: bool,
    pub sprite_left_enabled: bool,
    pub background_enabled: bool,
    pub sprite_enabled: bool,
    pub red_tint: bool,
    pub green_tint: bool,
    pub blue_tint: bool,
}
to_from_u8!(PpuMask);

impl Default for PpuMask {
    fn default() -> Self {
        0u8.into()
    }
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct PpuStatus {
    #[skip]
    __: B5,
    pub sprite_overflow: bool,
    pub sprite_zero_hit: bool,
    pub v_blank: bool,
}
to_from_u8!(PpuStatus);

impl Default for PpuStatus {
    fn default() -> Self {
        0u8.into()
    }
}

const PALETTE_MEM_START: u16 = 0x3f00;
const NAME_TABLE_MEM_START: u16 = 0x2000;
const TILES_PER_ROW: u8 = 32;
const TILES_PER_COL: u8 = 30;

// PPU Timing Constants
const SCANLINES_PER_FRAME: u16 = 262;
const DOTS_PER_SCANLINE: u16 = 341;
const VBLANK_SET_SCANLINE: u16 = 241;
const VBLANK_CLEAR_SCANLINE: u16 = 261;
const VBLANK_CLEAR_DOT: u16 = 1;

/// Result of a single PPU tick.
#[derive(Default)]
pub struct PpuTickResult {
    /// True if NMI should be triggered (VBlank started and NMI is enabled in PPUCTRL).
    pub nmi: bool,
    /// True when VBlank period begins (scanline 241, dot 1), regardless of NMI enable.
    pub vblank_started: bool,
}

const fn rgb(v: [u8; 3]) -> Pixel {
    let [r, g, b] = v;
    Rgba::<u8>([r, g, b, 0xff])
}

#[rustfmt::skip]
const COLORS: [Pixel; 64] = [
    rgb([117, 117, 117]), rgb([39, 27, 143]), rgb([0, 0, 171]), rgb([71, 0, 159]),
    rgb([143, 0, 119]), rgb([171, 0, 19]), rgb([167, 0, 0]), rgb([127, 11, 0]),
    rgb([67, 47, 0]), rgb([0, 71, 0]), rgb([0, 81, 0]), rgb([0, 63, 23]),
    rgb([27, 63, 95]), rgb([0, 0, 0]), rgb([0, 0, 0]), rgb([0, 0, 0]),

    rgb([188, 188, 188]), rgb([0, 115, 239]), rgb([35, 59, 239]), rgb([131, 0, 243]),
    rgb([191, 0, 191]), rgb([231, 0, 91]), rgb([219, 43, 0]), rgb([203, 79, 15]),
    rgb([139, 115, 0]), rgb([0, 151, 0]), rgb([0, 171, 0]), rgb([0, 147, 59]),
    rgb([0, 131, 139]), rgb([0, 0, 0]), rgb([0, 0, 0]), rgb([0, 0, 0]),

    rgb([255, 255, 255]), rgb([63, 191, 255]), rgb([95, 151, 255]), rgb([167, 139, 253]),
    rgb([247, 123, 255]), rgb([255, 119, 183]), rgb([255, 119, 99]), rgb([255, 155, 59]),
    rgb([243, 191, 63]), rgb([131, 211, 19]), rgb([79, 223, 75]), rgb([88, 248, 152]),
    rgb([0, 235, 219]), rgb([0, 0, 0]), rgb([0, 0, 0]), rgb([0, 0, 0]),

    rgb([255, 255, 255]), rgb([171, 231, 255]), rgb([199, 215, 255]), rgb([215, 203, 255]),
    rgb([255, 199, 255]), rgb([255, 199, 219]), rgb([255, 191, 179]), rgb([255, 219, 171]),
    rgb([255, 231, 163]), rgb([227, 255, 163]), rgb([171, 243, 191]), rgb([179, 255, 207]),
    rgb([159, 255, 243]), rgb([0, 0, 0]), rgb([0, 0, 0]), rgb([0, 0, 0]),
];

#[derive(Default)]
pub struct Palette {
    data: [u8; 0x20],
}

impl Palette {
    fn get_addr(&self, addr: u16) -> usize {
        // NES palette mirroring:
        // 0x3f00-0x3f0f: background palette
        // 0x3f10-0x3f1f: sprite palette, mirrors to 0x3f00-0x3f0f
        // 0x3f20-0x3fff: mirror down to 0x3f00-0x3f1f

        // First, mirror addresses >= 0x3f20
        let addr = if addr >= 0x3f20 {
            ((addr - 0x3f20) % 32) + 0x3f00
        } else {
            addr
        };

        // Then, handle sprite palette mirroring (0x3f10-0x3f1f -> 0x3f00-0x3f0f)
        // NOTE: Only addresses 0x3f10, 0x3f14, 0x3f18, 0x3f1c actually mirror
        // Other addresses in 0x3f10-0x3f1f range are not used on real hardware
        let addr = match addr {
            0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => addr - 0x10,
            _ => addr,
        };

        // Convert to index in the 32-byte palette array
        let addr = addr - PALETTE_MEM_START;

        // Clamp to valid range [0, 31]
        let addr = addr as usize;
        if addr >= 32 {
            31
        } else {
            addr
        }
    }

    fn _get_color_idx(&self, start: usize, palette_idx: u8, idx: u8) -> Pixel {
        let offset = if idx == 0 {
            0
        } else {
            start
                + idx as usize
                + match palette_idx {
                    0 => 0x00,
                    1 => 0x04,
                    2 => 0x08,
                    3 => 0x0c,
                    _ => unreachable!(),
                }
        };
        COLORS[self.data[offset] as usize]
    }

    fn get_background_color(&self, palette_idx: u8, idx: u8) -> Pixel {
        self._get_color_idx(0, palette_idx, idx)
    }

    fn _get_sprit_color(&self, palette_idx: u8, idx: u8) -> Pixel {
        let offset = 0x10
            + idx as usize
            + match palette_idx {
                0 => 0x00,
                1 => 0x04,
                2 => 0x08,
                3 => 0x0c,
                _ => unreachable!(),
            };
        let color_val = self.data[offset];
        debug!(
            "_get_sprit_color: palette_idx={}, idx={}, offset={}, val=0x{:02x}",
            palette_idx, idx, offset, color_val
        );
        self._get_color_idx(0x10, palette_idx, idx)
    }
}

impl Mcu for Palette {
    fn read(&mut self, address: u16) -> u8 {
        self.data[self.get_addr(address)]
    }

    fn write(&mut self, address: u16, value: u8) {
        info!("set palette ${:x}: {:x}", address, value);
        self.data[self.get_addr(address)] = value;
    }
}

pub struct Ppu {
    ctrl_flags: PpuCtrl,
    status: PpuStatus,
    name_table: NameTableControl, // name table
    cur_name_table_addr: u16,     // current active name table start address
    palette: Palette,             // palette memory
    cur_pattern_table_idx: u8,    // index of current active pattern table, 0 or 1

    mask: PpuMask,

    oam_addr: u8,
    oam: [u8; 0x100], // object attribute memory

    // VRAM address registers per NES PPU: v (current), t (temporary), x (fine X), w (write toggle)
    vram_addr: u16,      // v - current VRAM address
    temp_vram_addr: u16, // t - temporary VRAM address
    fine_x: u8,          // x - fine X scroll (3 bits)
    write_toggle: bool,  // w - first/second write toggle
    renderer: Box<dyn Render>,

    // PPU timing
    scanline: u16, // 0-261
    dot: u16,      // 0-340
    odd_frame: bool,
    cancel_nmi_pending: bool,
}

impl Default for Ppu {
    fn default() -> Self {
        Ppu {
            ctrl_flags: 0.into(),
            status: 0.into(),
            name_table: NameTableControl::default(),
            cur_name_table_addr: 0x2000,
            palette: Palette::default(),
            cur_pattern_table_idx: 0,
            mask: 0.into(),
            oam_addr: 0,
            oam: [0; 0x100],
            vram_addr: 0,
            temp_vram_addr: 0,
            fine_x: 0,
            write_toggle: false,
            renderer: Box::new(ImageRender::new(256, 240)),
            scanline: 0,
            dot: 0,
            odd_frame: false,
            cancel_nmi_pending: false,
        }
    }
}

impl Ppu {
    /// Set v-blank state.
    /// Return status before change
    pub fn set_v_blank(&mut self, v_blank: bool) -> PpuStatus {
        let status = self.status;
        if v_blank {
            debug!("set v_blank: {}", v_blank);
        }
        self.status = status.with_v_blank(v_blank);
        status
    }

    pub fn oam_dma(&mut self, vals: &[u8; 256]) {
        self.oam.copy_from_slice(vals);
    }

    /// Advance PPU by one dot, rendering a pixel if on a visible scanline.
    /// Returns a [`PpuTickResult`] indicating whether NMI should fire and/or VBlank has started.
    ///
    /// - `result.nmi` is `true` only when VBlank starts **and** NMI is enabled in PPUCTRL ($2000).
    /// - `result.vblank_started` is `true` whenever VBlank begins (scanline 241, dot 1),
    ///   regardless of whether NMI is enabled.
    ///
    /// # Parameters
    /// - `pattern`: CHR ROM pattern data for tile/sprite lookup
    pub fn tick(&mut self, pattern: &[u8]) -> PpuTickResult {
        let scanline = self.scanline;
        let dot = self.dot;

        // Render pixel during visible scanlines (0-239) and visible dots (0-255)
        if scanline < 240 && dot < 256 {
            let pixel = Self::render_pixel(self, pattern, dot as u8, scanline as u8);
            self.renderer
                .set_pixel(dot as u32, scanline as u32, pixel.0);
        }

        // At end of pre-render scanline (261), clear screen for next frame
        if scanline == VBLANK_CLEAR_SCANLINE && dot == 0 {
            let bg_color = COLORS[self.palette.data[0] as usize];
            self.renderer.clear(bg_color.0);
        }

        // Call finish() at end of visible area (scanline 240, dot 0)
        if scanline == 240 && dot == 0 {
            self.renderer.finish();
        }

        // VBlank set: normally scanline 241, dot 1.
        let mut vblank_started = false;
        let mut trigger_nmi = false;
        if self.scanline == VBLANK_SET_SCANLINE && self.dot == 1 {
            let status = self.status;
            if !status.v_blank() {
                debug!(
                    "PPU: VBLANK SET at scanline={}, dot={}",
                    self.scanline, self.dot
                );
                self.status = status.with_v_blank(true);
                trigger_nmi = self.ctrl_flags.nmi_enable();
                vblank_started = true;
            }
        }

        // VBlank clear: pre-render scanline 261, dot 1
        if self.scanline == VBLANK_CLEAR_SCANLINE && self.dot == VBLANK_CLEAR_DOT {
            let status = self.status;
            if status.v_blank() {
                debug!(
                    "PPU: VBLANK CLEAR at scanline={}, dot={}",
                    self.scanline, self.dot
                );
                self.status = status.with_v_blank(false);
            }
            // Also clear sprite zero hit at pre-render scanline
            let status = self.status;
            self.status = status.with_sprite_zero_hit(false);
        }

        // Advance dot/scanline counters
        self.dot += 1;
        if self.dot >= DOTS_PER_SCANLINE {
            self.dot = 0;
            self.scanline += 1;
            if self.scanline >= SCANLINES_PER_FRAME {
                debug!("PPU: Frame complete, scanline={} -> 0", self.scanline);
                self.scanline = 0;
                self.odd_frame = !self.odd_frame;
            }
        }

        PpuTickResult {
            nmi: trigger_nmi,
            vblank_started,
        }
    }

    pub fn set_mirroring(&mut self, mirroring: Mirroring) {
        self.name_table.set_mirroring(mirroring);
    }

    pub fn mirroring(&self) -> Mirroring {
        self.name_table.mirroring()
    }

    pub fn set_renderer(&mut self, renderer: Box<dyn Render>) {
        self.renderer = renderer;
    }

    pub fn take_cancel_nmi_pending(&mut self) -> bool {
        let v = self.cancel_nmi_pending;
        self.cancel_nmi_pending = false;
        v
    }

    pub fn nmi_signal(&self) -> bool {
        self.status.v_blank() && self.ctrl_flags.nmi_enable()
    }

    pub fn scanline_and_dot(&self) -> (u16, u16) {
        (self.scanline, self.dot)
    }

    /// Read from PPU registers or VRAM/palette data
    /// Returns the value read from the given address
    pub fn read(&mut self, pattern: &[u8], address: u16) -> u8 {
        // PPU registers are mirrored every 8 bytes in range $2000-$3FFF
        let reg = 0x2000 + ((address - 0x2000) % 8);
        match reg {
            // PPUSTATUS
            0x2002 => {
                let status = self.read_status();
                self.write_toggle = false;
                status.into()
            }
            // OAMDATA
            0x2004 => self.read_oam_data(),
            // PPUDATA - read from VRAM or palette
            0x2007 => self.read_vram_and_inc(pattern),
            _ => 0, // Other registers are write-only
        }
    }

    /// Write to PPU registers
    pub fn write(&mut self, address: u16, value: u8) {
        // PPU registers are mirrored every 8 bytes in range $2000-$3FFF
        let reg = 0x2000 + ((address - 0x2000) % 8);
        match reg {
            // PPUCTRL
            0x2000 => {
                self.set_control_flags(value.into());
                // Update name table address from control bits
                self.cur_name_table_addr =
                    0x2000 + (self.ctrl_flags.name_table_select() as u16 * 0x400);
            }
            // PPUMASK
            0x2001 => {
                self.mask = value.into();
            }
            // OAMADDR
            0x2003 => {
                self.oam_addr = value;
            }
            // OAMDATA
            0x2004 => {
                let addr = self.oam_addr as usize;
                self.oam[addr] = value;
                self.oam_addr = self.oam_addr.wrapping_add(1);
            }
            // PPUSCROLL
            0x2005 => {
                Self::write_scroll(self, value);
            }
            // PPUADDR
            0x2006 => {
                Self::write_vram_addr(self, value);
            }
            // PPUDATA
            0x2007 => {
                let vram_addr = self.vram_addr;
                Self::write_vram_data(self, vram_addr, value);
                // Increment VRAM address based on control flag
                let increment = if self.ctrl_flags.increment_mode() {
                    32
                } else {
                    1
                };
                self.vram_addr = self.vram_addr.wrapping_add(increment);
            }
            _ => {} // Ignore other addresses
        }
    }

    // Helper methods for VRAM operations
    fn read_vram_data(inner: &mut Ppu, pattern: &[u8], address: u16) -> u8 {
        let addr = address % 0x4000;
        if addr < 0x3f00 {
            // Read from pattern table or name table
            if addr < 0x2000 {
                // Pattern table (CHR ROM/RAM)
                pattern[addr as usize]
            } else {
                // Name table
                inner.name_table.read(addr)
            }
        } else {
            // Palette
            inner.palette.read(addr)
        }
    }

    fn write_vram_data(inner: &mut Ppu, address: u16, value: u8) {
        let addr = address % 0x4000;
        if addr < 0x3f00 {
            if addr >= 0x2000 {
                // Name table (pattern table 0x0000-0x1FFF is read-only for CHR ROM)
                inner.name_table.write(addr, value);
            }
            // else: pattern table writes ignored (CHR ROM is read-only)
        } else {
            // Palette
            inner.palette.write(addr, value);
        }
    }

    fn write_scroll(inner: &mut Ppu, value: u8) {
        if !inner.write_toggle {
            // First write - X scroll
            inner.fine_x = value & 0x07;
            inner.temp_vram_addr = (inner.temp_vram_addr & 0xFFE0) | ((value as u16 >> 3) & 0x001F);
            inner.write_toggle = true;
        } else {
            // Second write - Y scroll
            inner.temp_vram_addr = (inner.temp_vram_addr & 0x8FFF)
                | (((value as u16 & 0x07) << 12) & 0x7000)
                | (((value as u16 >> 3) & 0x001F) << 5);
            inner.write_toggle = false;
        }
    }

    fn write_vram_addr(inner: &mut Ppu, value: u8) {
        if !inner.write_toggle {
            // First write - high byte
            inner.temp_vram_addr =
                (inner.temp_vram_addr & 0x80FF) | (((value as u16 & 0x3F) << 8) & 0x3F00);
            inner.write_toggle = true;
        } else {
            // Second write - low byte
            inner.temp_vram_addr = (inner.temp_vram_addr & 0x7F00) | (value as u16 & 0x00FF);
            inner.vram_addr = inner.temp_vram_addr;
            inner.write_toggle = false;
        }
    }

    fn get_background_pixel(inner: &Ppu, pattern: &[u8], screen_x: u8, screen_y: u8) -> (u8, u8) {
        // Check left-column clipping
        if !inner.mask.background_left_enabled() && screen_x < 8 {
            return (0, 0);
        }

        let vram_addr = inner.vram_addr;
        let coarse_x = (vram_addr & 0x001f) as u8;
        let coarse_y = ((vram_addr >> 5) & 0x001f) as u8;
        let fine_y = ((vram_addr >> 12) & 0x0007) as u8;
        let fine_x = inner.fine_x;

        // Calculate which pixel we're rendering factoring in scroll
        let pixel_x = screen_x as u16 + fine_x as u16;
        let tile_x = coarse_x as u16 + (pixel_x / 8);
        let tile_fine_x = (pixel_x % 8) as usize;

        let pixel_y = screen_y as u16 + (coarse_y as u16 * 8) + fine_y as u16;
        let tile_y = pixel_y / 8;
        let tile_fine_y = (pixel_y % 8) as usize;

        // Determine which nametable based on wrapping
        let nt_x = tile_x & 0x1f;
        let nt_y = tile_y & 0x1f;
        // Clamp nt_y to valid range (0-29) since NES screen is only 240px = 30 tiles high
        let nt_y = if nt_y >= 30 { nt_y - 30 } else { nt_y };
        let nt_select_x = (tile_x >> 5) & 0x1;
        let nt_select_y = (tile_y >> 5) & 0x1;
        let nt_idx = (nt_select_y << 1) | nt_select_x;

        let pattern_table = PatternBand::new(pattern).pattern(inner.cur_pattern_table_idx as usize);
        let nt_idx = nt_idx as u8;

        let (tile, palette_idx) = inner.name_table.with_nth(nt_idx, |name_table| {
            let tile = name_table.tile(pattern_table, nt_x as u8, nt_y as u8);
            let palette_idx = inner.name_table.with_attribute_table(nt_idx, |attr_table| {
                attr_table.palette_idx(nt_x as u8, nt_y as u8)
            });
            (tile, palette_idx)
        });

        let color_idx = tile.pixel(tile_fine_x, tile_fine_y);

        (palette_idx, color_idx)
    }

    fn get_sprite_pixel(
        inner: &Ppu,
        pattern: &[u8],
        screen_x: u8,
        screen_y: u8,
    ) -> Option<(u8, u8, bool, bool)> {
        // Check left-column clipping
        if !inner.mask.sprite_left_enabled() && screen_x < 8 {
            return None;
        }

        // Determine which pattern table to use for sprites
        let pattern_table_idx = if inner.ctrl_flags.sprite_pattern_table() {
            1
        } else {
            0
        };

        // Render sprites in reverse order (priority 0->63)
        for sprite_idx in (0..=63).rev() {
            let byte_idx = sprite_idx * 4;
            let y = inner.oam[byte_idx];
            let tile_idx = inner.oam[byte_idx + 1];
            let attributes = inner.oam[byte_idx + 2];
            let x = inner.oam[byte_idx + 3];

            let sprite_y = screen_y as i16 - y as i16;
            let sprite_x = screen_x as i16 - x as i16;

            // Check if sprite is visible at current position
            if (0..8).contains(&sprite_x) && (0..8).contains(&sprite_y) {
                // Check sprite flipping and get pixel
                let flip_vertical = (attributes & 0x80) != 0;
                let flip_horizontal = (attributes & 0x40) != 0;
                let sprite_y = if flip_vertical {
                    7 - sprite_y
                } else {
                    sprite_y
                } as u8;
                let sprite_x = if flip_horizontal {
                    7 - sprite_x
                } else {
                    sprite_x
                } as u8;

                let pattern_table =
                    crate::nes::ppu::PatternBand::new(pattern).pattern(pattern_table_idx);
                let tile = pattern_table.tile(tile_idx);
                let color_idx = tile.pixel(sprite_x as usize, sprite_y as usize);

                if color_idx != 0 {
                    let palette_idx = attributes & 0x03;
                    let behind_bg = (attributes & 0x20) != 0;
                    let is_sprite_zero = sprite_idx == 0;
                    return Some((palette_idx, color_idx, behind_bg, is_sprite_zero));
                }
            }
        }
        None
    }

    /// Read VRAM at current address and increment (for testing)
    fn read_vram_and_inc(&mut self, pattern: &[u8]) -> u8 {
        let vram_addr = self.vram_addr;
        let value = Self::read_vram_data(self, pattern, vram_addr);
        // Increment VRAM address based on control flag
        let increment = if self.ctrl_flags.increment_mode() {
            32
        } else {
            1
        };
        self.vram_addr = self.vram_addr.wrapping_add(increment);
        value
    }

    /// Read OAM data at current OAM address (for testing)
    fn read_oam_data(&self) -> u8 {
        self.oam[self.oam_addr as usize]
    }

    /// Set control flags (for testing)
    fn set_control_flags(&mut self, flags: PpuCtrl) {
        self.ctrl_flags = flags;
        // Update pattern table index based on background_pattern_table flag
        self.cur_pattern_table_idx = if self.ctrl_flags.background_pattern_table() {
            1
        } else {
            0
        };
    }

    /// Read status register (for testing) - behaves like reading from 0x2002
    /// Returns the current status and clears the v_blank flag
    fn read_status(&mut self) -> PpuStatus {
        if self.scanline == VBLANK_SET_SCANLINE && (self.dot == 1 || self.dot == 2) {
            self.cancel_nmi_pending = true;
        }
        let status = self.status;
        // Clear v_blank flag on read (like real hardware)
        self.status = status.with_v_blank(false);
        status
    }

    /// Render a single pixel (for testing)
    fn render_pixel(&mut self, pattern: &[u8], x: u8, y: u8) -> Pixel {
        let bg_pixel = if self.mask.background_enabled() {
            Self::get_background_pixel(self, pattern, x, y)
        } else {
            (0, 0) // transparent
        };

        let sprite_pixel = if self.mask.sprite_enabled() {
            Self::get_sprite_pixel(self, pattern, x, y)
        } else {
            None
        };

        let (bg_palette_idx, bg_color_idx) = bg_pixel;

        let color = match sprite_pixel {
            Some((spr_palette_idx, spr_color_idx, behind_bg, is_sprite_zero)) => {
                // Sprite zero hit detection:
                // Set when an opaque sprite 0 pixel overlaps an opaque background pixel
                if is_sprite_zero && bg_color_idx != 0 && spr_color_idx != 0 && x != 255 {
                    self.status = self.status.with_sprite_zero_hit(true);
                }

                if spr_color_idx == 0 {
                    // Sprite pixel is transparent, show background
                    self.palette
                        .get_background_color(bg_palette_idx, bg_color_idx)
                } else if bg_color_idx == 0 || !behind_bg {
                    // Background is transparent OR sprite has priority
                    self.palette
                        ._get_sprit_color(spr_palette_idx, spr_color_idx)
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
        };

        // Apply grayscale and emphasis effects from PPUMASK
        apply_effects(
            color,
            self.mask.grayscale(),
            self.mask.red_tint(),
            self.mask.green_tint(),
            self.mask.blue_tint(),
        )
    }
}

#[cfg(test)]
mod tests;
