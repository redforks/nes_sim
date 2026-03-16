use crate::mcu::Mcu;
use crate::render::{ImageRender, Render};
use crate::to_from_u8;
use image::Rgba;
use log::{debug, info};
use modular_bitfield::prelude::*;
use std::cell::RefCell;

mod name_table;
mod pattern;

pub use name_table::*;
pub use pattern::*;

type Pixel = Rgba<u8>;

#[derive(Copy, Clone)]
#[bitfield]
pub struct PpuCtrl {
    pub nmi_enable: bool,
    pub ppu_master: bool,
    pub sprite_size: bool,
    pub background_pattern_table: bool,
    pub sprite_pattern_table: bool,
    pub increment_mode: bool,
    pub name_table_select: B2,
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
    pub v_blank: bool,
    pub sprite_zero_hit: bool,
    pub sprite_overflow: bool,
    #[skip]
    __: B5,
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
struct Palette {
    data: [u8; 0x20],
}

impl Palette {
    fn get_addr(&self, addr: u16) -> usize {
        let addr = match addr {
            // these address are mirrored
            0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => addr - 0x10,
            _ => addr,
        };

        (addr - PALETTE_MEM_START) as usize
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
        self._get_color_idx(0x10, palette_idx, idx)
    }
}

impl Mcu for Palette {
    fn read(&self, address: u16) -> u8 {
        self.data[self.get_addr(address)]
    }

    fn write(&mut self, address: u16, value: u8) {
        info!("set palette ${:x}: {:x}", address, value);
        self.data[self.get_addr(address)] = value;
    }
}

pub trait PpuTrait {
    /// Returns true if should trigger nmi at the start of v-blank.
    fn should_nmi(&self) -> bool;

    /// Trigger nmi at the start of v-blank, if should_nmi() returns true.
    /// Return status before change
    fn set_v_blank(&self, v_blank: bool) -> PpuStatus;
}

pub struct Ppu {
    ctrl_flags: PpuCtrl,
    status: RefCell<PpuStatus>,
    name_table: NameTableControl, // name table
    cur_name_table_addr: u16,     // current active name table start address
    palette: Palette,             // palette memory
    cur_pattern_table_idx: u8,    // index of current active pattern table, 0 or 1

    mask: PpuMask,

    oam_addr: u8,
    oam: [u8; 0x100], // object attribute memory

    // VRAM address registers per NES PPU: v (current), t (temporary), x (fine X), w (write toggle)
    vram_addr: RefCell<u16>,     // v - current VRAM address
    temp_vram_addr: u16,         // t - temporary VRAM address
    fine_x: u8,                  // x - fine X scroll (3 bits)
    write_toggle: RefCell<bool>, // w - first/second write toggle
    renderer: Box<dyn Render>,

    // PPU timing
    scanline: u16, // 0-261
    dot: u16,      // 0-340
}

impl Default for Ppu {
    fn default() -> Self {
        Ppu {
            ctrl_flags: 0.into(),
            status: RefCell::new(0.into()),
            name_table: NameTableControl::default(),
            cur_name_table_addr: 0x2000,
            palette: Palette::default(),
            cur_pattern_table_idx: 0,
            mask: PpuMask::new(),
            oam_addr: 0,
            oam: [0; 0x100],
            vram_addr: RefCell::new(0),
            temp_vram_addr: 0,
            fine_x: 0,
            write_toggle: RefCell::new(false),
            renderer: Box::new(ImageRender::new(256, 240)),
            scanline: 0,
            dot: 0,
        }
    }
}

impl PpuTrait for Ppu {
    fn should_nmi(&self) -> bool {
        self.ctrl_flags.nmi_enable() && self.status.borrow().v_blank()
    }

    fn set_v_blank(&self, v_blank: bool) -> PpuStatus {
        let status = *self.status.borrow();
        if v_blank {
            debug!("set v_blank: {}", v_blank);
        }
        *self.status.borrow_mut() = status.with_v_blank(v_blank);
        status
    }
}

impl Ppu {
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
            self.render_pixel(pattern, dot as u8, scanline as u8);
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

        // Advance dot/scanline counters
        self.dot += 1;
        if self.dot >= DOTS_PER_SCANLINE {
            self.dot = 0;
            self.scanline += 1;
            if self.scanline >= SCANLINES_PER_FRAME {
                self.scanline = 0;
            }
        }

        // VBlank set: scanline 241, dot 1
        if self.scanline == VBLANK_SET_SCANLINE && self.dot == 1 {
            let status = *self.status.borrow();
            let was_vblank = status.v_blank();
            if !was_vblank {
                *self.status.borrow_mut() = status.with_v_blank(true);
                return PpuTickResult {
                    nmi: self.ctrl_flags.nmi_enable(),
                    vblank_started: true,
                };
            }
        }

        // VBlank clear: scanline 261, dot 1
        if self.scanline == VBLANK_CLEAR_SCANLINE && self.dot == 1 {
            let status = *self.status.borrow();
            if status.v_blank() {
                *self.status.borrow_mut() = status.with_v_blank(false);
            }
            // Also clear sprite zero hit at pre-render scanline
            let status = *self.status.borrow();
            *self.status.borrow_mut() = status.with_sprite_zero_hit(false);
        }

        PpuTickResult::default()
    }

    /// Render a single pixel at the given screen coordinates.
    /// Handles both background and sprite rendering with priority.
    fn render_pixel(&mut self, pattern: &[u8], screen_x: u8, screen_y: u8) {
        let bg_pixel = if self.mask.background_enabled() {
            self.get_background_pixel(pattern, screen_x, screen_y)
        } else {
            (0, 0) // transparent
        };

        let sprite_pixel = if self.mask.sprite_enabled() {
            self.get_sprite_pixel(pattern, screen_x, screen_y)
        } else {
            None
        };

        let (bg_palette_idx, bg_color_idx) = bg_pixel;

        let color = match sprite_pixel {
            Some((spr_palette_idx, spr_color_idx, behind_bg, is_sprite_zero)) => {
                // Sprite zero hit detection:
                // Set when an opaque sprite 0 pixel overlaps an opaque background pixel
                if is_sprite_zero && bg_color_idx != 0 && spr_color_idx != 0 && screen_x != 255 {
                    let status = *self.status.borrow();
                    *self.status.borrow_mut() = status.with_sprite_zero_hit(true);
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

        self.renderer
            .set_pixel(screen_x as u32, screen_y as u32, color.0);
    }

    /// Get background pixel color index at the given screen position.
    /// Returns (palette_idx, color_idx) where color_idx 0 means transparent/universal bg.
    fn get_background_pixel(&self, pattern: &[u8], screen_x: u8, screen_y: u8) -> (u8, u8) {
        // Check left-column clipping
        if !self.mask.background_left_enabled() && screen_x < 8 {
            return (0, 0);
        }

        let vram_addr = *self.vram_addr.borrow();
        let coarse_x = (vram_addr & 0x001f) as u8;
        let coarse_y = ((vram_addr >> 5) & 0x001f) as u8;
        let fine_y = ((vram_addr >> 12) & 0x0007) as u8;
        let fine_x = self.fine_x;

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

        let pattern_table = PatternBand::new(pattern).pattern(self.cur_pattern_table_idx as usize);
        let name_table = self.name_table.nth(nt_idx as u8);
        let attr_table = self.name_table.attribute_table(nt_idx as u8);

        let tile = name_table.tile(pattern_table, nt_x as u8, nt_y as u8);
        let palette_idx = attr_table.palette_idx(nt_x as u8, nt_y as u8);
        let color_idx = tile.pixel(tile_fine_x, tile_fine_y);

        (palette_idx, color_idx)
    }

    /// Get sprite pixel at the given screen position.
    /// Returns Some((palette_idx, color_idx, behind_bg, is_sprite_zero)) if a sprite is present,
    /// or None if no sprite covers this pixel.
    fn get_sprite_pixel(
        &self,
        pattern: &[u8],
        screen_x: u8,
        screen_y: u8,
    ) -> Option<(u8, u8, bool, bool)> {
        // Check left-column clipping for sprites
        if !self.mask.sprite_left_enabled() && screen_x < 8 {
            return None;
        }

        let sprite_height: u8 = if self.ctrl_flags.sprite_size() { 16 } else { 8 };
        let sprite_pattern_table = if self.ctrl_flags.sprite_size() {
            0 // For 8x16 sprites, pattern table is determined by tile index bit 0
        } else {
            self.ctrl_flags.sprite_pattern_table() as usize
        };

        // Scan OAM for sprites on this scanline (max 8, lower index = higher priority)
        for i in 0..64u8 {
            let oam_offset = (i as usize) * 4;
            let sprite_y = self.oam[oam_offset]; // Y position (top of sprite)
            let tile_index = self.oam[oam_offset + 1];
            let attributes = self.oam[oam_offset + 2];
            let sprite_x = self.oam[oam_offset + 3];

            // Check if sprite is on this scanline
            let y_diff = screen_y.wrapping_sub(sprite_y.wrapping_add(1));
            if y_diff >= sprite_height {
                continue;
            }

            // Check if sprite covers this X position
            if screen_x < sprite_x || screen_x >= sprite_x.wrapping_add(8) {
                continue;
            }

            let flip_h = attributes & 0x40 != 0;
            let flip_v = attributes & 0x80 != 0;
            let behind_bg = attributes & 0x20 != 0;
            let palette_idx = attributes & 0x03;

            let mut pixel_row = y_diff;
            if flip_v {
                pixel_row = sprite_height - 1 - pixel_row;
            }

            let mut pixel_col = (screen_x - sprite_x) as usize;
            if flip_h {
                pixel_col = 7 - pixel_col;
            }

            // Get tile data
            let (actual_pattern_table, actual_tile_index) = if self.ctrl_flags.sprite_size() {
                // 8x16 mode: bit 0 of tile index selects pattern table
                let pt = (tile_index & 0x01) as usize;
                let ti = tile_index & 0xFE;
                if pixel_row < 8 {
                    (pt, ti)
                } else {
                    (pt, ti + 1)
                }
            } else {
                (sprite_pattern_table, tile_index)
            };

            let pattern_data = PatternBand::new(pattern).pattern(actual_pattern_table);
            let tile = pattern_data.tile(actual_tile_index);
            let color_idx = tile.pixel(pixel_col, (pixel_row % 8) as usize);

            if color_idx != 0 {
                // Found an opaque sprite pixel
                return Some((palette_idx, color_idx, behind_bg, i == 0));
            }
        }

        None
    }

    pub fn set_mirroring(&mut self, mirroring: Mirroring) {
        self.name_table.set_mirroring(mirroring);
    }

    /// Set a custom renderer for the PPU
    ///
    /// This allows injecting different rendering backends (image, markdown, composite, etc.)
    /// instead of using the default ImageRender.
    ///
    /// # Parameters
    /// - `renderer`: The renderer to use (must implement the Render trait)
    pub fn set_renderer(&mut self, renderer: Box<dyn Render>) {
        self.renderer = renderer;
    }

    /// Set PPU control flags. Returns true if NMI should be triggered
    /// by enabling NMI during an active VBlank.
    fn set_control_flags(&mut self, flag: PpuCtrl) -> bool {
        let old_nmi_enable = self.ctrl_flags.nmi_enable();
        self.ctrl_flags = flag;
        let new_val = match flag.name_table_select() {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2c00,
            _ => unreachable!(),
        };
        if new_val != self.cur_name_table_addr {
            info!("switch name table to {:04x}", new_val);
            self.cur_name_table_addr = new_val;
        }
        let new_val = flag.background_pattern_table() as u8;
        if new_val != self.cur_pattern_table_idx {
            info!("switch pattern table to {}", new_val);
            self.cur_pattern_table_idx = new_val;
        }
        !old_nmi_enable && flag.nmi_enable() && self.status.borrow().v_blank()
    }

    fn set_ppu_mask(&mut self, mask: PpuMask) {
        self.mask = mask;
    }

    fn set_oma_addr(&mut self, addr: u8) {
        self.oam_addr = addr;
    }

    fn read_oam_data(&self) -> u8 {
        self.oam[self.oam_addr as usize]
    }

    fn write_oam_data_and_inc(&mut self, value: u8) {
        self.oam[self.oam_addr as usize] = value;
        self.oam_addr = self.oam_addr.wrapping_add(1);
    }

    fn read_status(&self) -> PpuStatus {
        // reading status resets the write toggle (w) and clears vblank
        *self.write_toggle.borrow_mut() = false;
        let status = self.set_v_blank(false);
        debug!("read_status: {:02x}", u8::from(status));
        status
    }

    pub fn read(&self, pattern: &[u8], address: u16) -> u8 {
        // Mirror PPU register space (0x2000-0x3FFF) down to 0x2000-0x2007
        let addr = if (0x2000..=0x3fff).contains(&address) {
            0x2000 + (address - 0x2000) % 8
        } else {
            address
        };

        match addr {
            0x2002 => self.read_status().into(),
            0x2004 => self.read_oam_data(),
            0x2007 => self.read_vram_and_inc(pattern),
            _ => 0x55,
        }
    }

    /// Write to PPU register. Returns true if NMI should be triggered.
    pub fn write(&mut self, address: u16, val: u8) -> bool {
        // Mirror PPU register space (0x2000-0x3FFF) down to 0x2000-0x2007
        let addr = if (0x2000..=0x3fff).contains(&address) {
            0x2000 + (address - 0x2000) % 8
        } else {
            address
        };

        match addr {
            0x2000 => self.set_control_flags(PpuCtrl::from(val)),
            0x2001 => {
                self.set_ppu_mask(PpuMask::from(val));
                false
            }
            // 0x2002 is a read-only status register; writes are ignored
            0x2002 => false,
            0x2003 => {
                self.set_oma_addr(val);
                false
            }
            0x2004 => {
                self.write_oam_data_and_inc(val);
                false
            }
            0x2005 => {
                // $2005 - Scroll register: first write: coarse X + fine X; second write: coarse Y + fine Y
                if !*self.write_toggle.borrow() {
                    // first write
                    let coarse_x = (val >> 3) as u16 & 0x1f;
                    self.temp_vram_addr = (self.temp_vram_addr & !0x001f) | coarse_x;
                    self.fine_x = val & 0x07;
                    *self.write_toggle.borrow_mut() = true;
                } else {
                    // second write
                    let coarse_y = (val >> 3) as u16 & 0x1f;
                    let fine_y = (val & 0x07) as u16;
                    // set bits 5-9 to coarse_y
                    self.temp_vram_addr = (self.temp_vram_addr & !0x03e0) | (coarse_y << 5);
                    // set bits 12-14 to fine_y
                    self.temp_vram_addr = (self.temp_vram_addr & !0x7000) | (fine_y << 12);
                    *self.write_toggle.borrow_mut() = false;
                }
                false
            }
            0x2006 => {
                // $2006 - Set VRAM address: two writes (high then low) into temp_vram_addr (t).
                if !*self.write_toggle.borrow() {
                    // first write: high 6 bits
                    self.temp_vram_addr = ((val as u16) & 0x3f) << 8;
                    *self.write_toggle.borrow_mut() = true;
                } else {
                    // second write: low 8 bits -> commit t to v
                    self.temp_vram_addr |= val as u16;
                    *self.vram_addr.borrow_mut() = self.temp_vram_addr & 0x7fff;
                    *self.write_toggle.borrow_mut() = false;
                }
                false
            }
            0x2007 => {
                self.write_vram_and_inc(val);
                false
            }
            _ => panic!("Can not write to Ppu at address ${:x}", address),
        }
    }

    pub fn mirroring(&self) -> Mirroring {
        self.name_table.mirroring()
    }
}

impl Ppu {
    fn read_vram(&self, pattern: &[u8], address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => pattern[address as usize],
            0x2000..=0x2fff => self.name_table.read(address),
            0x3000..=0x3eff => self.read_vram(pattern, address - 0x1000),
            0x3f00..=0x3f1f => self.palette.read(address),
            0x3f20..=0x3fff => self.read_vram(pattern, address - 0x20),
            _ => unreachable!(),
        }
    }

    fn write_vram(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1fff => {
                info!("Ignore write to pattern rom ${:x} {:x}", address, value)
            }
            0x2000..=0x2fff => self.name_table.write(address, value),
            0x3000..=0x3eff => self.write_vram(address - 0x1000, value),
            0x3f00..=0x3f1f => self.palette.write(address, value),
            0x3f20..=0x3fff => self.write_vram(address - 0x20, value),
            _ => unreachable!(),
        }
    }

    fn inc_data_rw_addr(&self) {
        let delta = if self.ctrl_flags.increment_mode() {
            32
        } else {
            1
        };
        let mut addr = self.vram_addr.borrow_mut();
        *addr = (*addr).wrapping_add(delta);
    }

    fn read_vram_and_inc(&self, pattern: &[u8]) -> u8 {
        let addr = *self.vram_addr.borrow();
        let value = self.read_vram(pattern, addr);
        self.inc_data_rw_addr();
        value
    }

    fn write_vram_and_inc(&mut self, v: u8) {
        let addr = *self.vram_addr.borrow();
        self.write_vram(addr, v);
        self.inc_data_rw_addr();
    }
}

#[cfg(test)]
mod tests;
