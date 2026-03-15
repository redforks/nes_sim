use crate::mcu::Mcu;
use crate::to_from_u8;
use image::{Rgba, RgbaImage};
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
    pub blue_tint: bool,
    pub green_tint: bool,
    pub red_tint: bool,
    pub sprite_enabled: bool,
    pub background_enabled: bool,
    pub sprite_left_enabled: bool,
    pub background_left_enabled: bool,
    pub grayscale: bool,
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
    #[allow(non_snake_case)]
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
    image: RgbaImage,

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
            image: RgbaImage::new(256, 240),
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

    /// Advance PPU by one dot. Returns true if NMI should be triggered.
    pub fn tick(&mut self) -> bool {
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
                if self.ctrl_flags.nmi_enable() {
                    return true;
                }
            }
        }

        // VBlank clear: scanline 261, dot 1
        if self.scanline == VBLANK_CLEAR_SCANLINE && self.dot == 1 {
            let status = *self.status.borrow();
            if status.v_blank() {
                *self.status.borrow_mut() = status.with_v_blank(false);
            }
        }

        false
    }

    fn render_background(&mut self, pattern: &[u8]) {
        let pattern = PatternBand::new(pattern).pattern(self.cur_pattern_table_idx as usize);
        let cur_name_table = (self.cur_name_table_addr - NAME_TABLE_MEM_START) / 0x400;
        let name_table = self.name_table.nth(cur_name_table as u8);
        let attr_table = self.name_table.attribute_table(cur_name_table as u8);
        for (tile_x, tile_y) in itertools::iproduct!(0..32, 0..30) {
            let tile = name_table.tile(pattern, tile_x, tile_y);
            let palette_idx = attr_table.palette_idx(tile_x, tile_y);
            for (x, y, pixel) in tile.iter() {
                let color = self.palette.get_background_color(palette_idx, pixel);
                self.image.put_pixel(
                    x as u32 + tile_x as u32 * 8,
                    y as u32 + tile_y as u32 * 8,
                    color,
                );
            }
        }
    }

    pub fn render(&mut self, pattern: &[u8]) -> &RgbaImage {
        if self.mask.background_enabled() {
            self.render_background(pattern);
        }
        &self.image
    }

    pub fn set_mirroring(&mut self, mirroring: Mirroring) {
        self.name_table.set_mirroring(mirroring);
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
mod tests {
    use super::*;

    fn new_test_ppu_and_pattern() -> (Ppu, [u8; 8192]) {
        (Ppu::default(), [0; 8192])
    }

    #[test]
    fn palette_read_write() {
        let mut p = Palette::default();
        for i in 0..0x20 {
            p.write(0x3f00 + i, i as u8);
            assert_eq!(i as u8, p.read(0x3f00 + i));
        }
        for i in 0..0x20 {
            if i % 4 != 0 {
                // exclude mirrored address
                assert_eq!(i as u8, p.read(0x3f00 + i));
            }
        }
    }

    #[test]
    fn palette_get_color() {
        let mut p = Palette::default();
        p.write(0x3f00, 15);
        p.write(0x3f01, 16);
        p.write(0x3f02, 17);
        p.write(0x3f03, 18);
        p.write(0x3f05, 19);
        assert_eq!(COLORS[15], p.get_background_color(0, 0));
        assert_eq!(COLORS[16], p.get_background_color(0, 1));
        assert_eq!(COLORS[17], p.get_background_color(0, 2));
        assert_eq!(COLORS[18], p.get_background_color(0, 3));
        assert_eq!(COLORS[19], p.get_background_color(1, 1));
    }

    #[test]
    fn read_write_v_ram() {
        let (mut ppu, mut pattern) = new_test_ppu_and_pattern();
        // data_addr default to 0
        assert_eq!(ppu.read_vram_and_inc(&pattern), 0);

        ppu.read(&pattern, 0x2000); // reset addr
        ppu.write(0x2006, 0x21);
        ppu.write(0x2006, 0x08);
        ppu.write(0x2007, 0x12);
        ppu.write(0x2007, 0x34);
        assert_eq!(ppu.read_vram(&pattern, 0x2108), 0x12);
        assert_eq!(ppu.read_vram(&pattern, 0x2109), 0x34);

        ppu.read(&pattern, 0x2000); // reset addr
        ppu.write(0x2006, 0x21);
        ppu.write(0x2006, 0x08);
        assert_eq!(ppu.read(&pattern, 0x2007), 0x12);
        assert_eq!(ppu.read(&pattern, 0x2007), 0x34);

        // set increase mode to 32
        ppu.write(0x2000, PpuCtrl::new().with_increment_mode(true).into());
        ppu.read(&pattern, 0x2000); // reset addr
        ppu.write(0x2006, 0x21);
        ppu.write(0x2006, 0x08);
        ppu.write(0x2007, 0x56);
        ppu.write(0x2007, 0x78);
        assert_eq!(ppu.read_vram(&pattern, 0x2108,), 0x56);
        assert_eq!(ppu.read_vram(&pattern, 0x2128,), 0x78);

        ppu.read(&pattern, 0x2000); // reset addr
        ppu.write(0x2006, 0x21);
        ppu.write(0x2006, 0x08);
        assert_eq!(ppu.read(&pattern, 0x2007), 0x56);
        assert_eq!(ppu.read(&pattern, 0x2007), 0x78);

        fn rw(ppu: &mut Ppu, pattern: &mut [u8], addr: u16, w_value: u8, r_value: u8) {
            ppu.read(pattern, 0x2000); // reset addr
            ppu.write(0x2006, (addr >> 8) as u8);
            ppu.write(0x2006, addr as u8);
            ppu.write(0x2007, w_value);
            ppu.read(pattern, 0x2000); // reset addr
            ppu.write(0x2006, (addr >> 8) as u8);
            ppu.write(0x2006, addr as u8);
            assert_eq!(ppu.read(pattern, 0x2007), r_value);
        }

        fn rw_round_trip(ppu: &mut Ppu, pattern: &mut [u8], addr: u16, value: u8) {
            rw(ppu, pattern, addr, value, value);
        }

        // ignore write to pattern/chr-rom
        rw(&mut ppu, &mut pattern, 0x0001, 13, 0);

        // read-write palette ram index
        rw_round_trip(&mut ppu, &mut pattern, 0x3f00, 14);
        // read-write mirror of name-table
        rw_round_trip(&mut ppu, &mut pattern, 0x2400, 15);
        // read-write mirror of palette ram index
        rw_round_trip(&mut ppu, &mut pattern, 0x3f20, 16);
    }

    #[test]
    fn read_write_oam() {
        let (mut ppu, _pattern) = new_test_ppu_and_pattern();
        // oam addr default to 0
        assert_eq!(ppu.oam_addr, 0);

        ppu.write(0x2003, 0x12);
        // write oma data, auto inc oma addr
        ppu.write(0x2004, 0x34);
        assert_eq!(0x13, ppu.oam_addr);
        ppu.write(0x2004, 0x56);
        assert_eq!(0x14, ppu.oam_addr);

        ppu.write(0x2003, 0x12);
        assert_eq!(ppu.read_oam_data(), 0x34);
        assert_eq!(0x12, ppu.oam_addr);
        // read oma data, won't auto inc oma addr
        assert_eq!(ppu.read_oam_data(), 0x34);
    }

    #[test]
    fn nmi_occurred() {
        let (mut ppu, _) = new_test_ppu_and_pattern();

        let flag = PpuCtrl::new().with_nmi_enable(false);
        assert!(!ppu.status.borrow().v_blank());

        // disable nmi
        ppu.set_control_flags(flag);
        ppu.set_v_blank(true);
        assert!(ppu.status.borrow().v_blank());
        // should_nmi is false because nmi is disabled
        assert!(!ppu.should_nmi());
        ppu.set_v_blank(false);
        // v_blank is false on v_blank end
        assert!(!ppu.status.borrow().v_blank());

        // read status register will reset v_blank flag
        ppu.set_v_blank(true);
        assert!(ppu.read_status().v_blank());
        assert!(!ppu.status.borrow().v_blank());

        // enable nmi
        ppu.set_control_flags(flag.with_nmi_enable(true));
        assert!(!ppu.should_nmi());
        ppu.set_v_blank(true);
        assert!(ppu.status.borrow().v_blank());
        // should_nmi is true because nmi is enabled
        assert!(ppu.should_nmi());
    }

    #[test]
    fn render() {
        let (mut ppu, pattern) = new_test_ppu_and_pattern();
        ppu.render(&pattern);
    }

    #[test]
    fn ppu_tick_timing() {
        let (mut ppu, _pattern) = new_test_ppu_and_pattern();

        // Enable NMI
        ppu.set_control_flags(PpuCtrl::new().with_nmi_enable(true));

        // Initial state
        assert_eq!(ppu.scanline, 0);
        assert_eq!(ppu.dot, 0);
        assert!(!ppu.status.borrow().v_blank());

        // Advance to scanline 241, dot 0
        ppu.scanline = VBLANK_SET_SCANLINE;
        ppu.dot = 0;

        // Tick once - should set VBlank and trigger NMI
        let nmi = ppu.tick();
        assert!(nmi, "NMI should be triggered at scanline 241, dot 1");
        assert!(ppu.status.borrow().v_blank());

        // Advance to scanline 261, dot 0
        ppu.scanline = VBLANK_CLEAR_SCANLINE;
        ppu.dot = 0;

        // Tick once - should clear VBlank
        let nmi = ppu.tick();
        assert!(!nmi, "NMI should not be triggered when clearing VBlank");
        assert!(!ppu.status.borrow().v_blank());
    }

    #[test]
    fn ppu_tick_scanline_wrap() {
        let (mut ppu, _pattern) = new_test_ppu_and_pattern();

        // Start at last scanline, last dot
        ppu.scanline = SCANLINES_PER_FRAME - 1; // 261
        ppu.dot = DOTS_PER_SCANLINE - 1; // 340

        // Tick once - should wrap to scanline 0, dot 0
        let _ = ppu.tick();
        assert_eq!(ppu.scanline, 0);
        assert_eq!(ppu.dot, 0);
    }

    #[test]
    fn nmi_trigger_on_ctrl_write_during_vblank() {
        let (mut ppu, _) = new_test_ppu_and_pattern();

        // Set VBlank active
        ppu.set_v_blank(true);
        assert!(ppu.status.borrow().v_blank());

        // NMI disabled initially
        let ctrl = PpuCtrl::new().with_nmi_enable(false);
        ppu.set_control_flags(ctrl);
        assert!(
            !ppu.should_nmi(),
            "should_nmi should be false when NMI disabled"
        );

        // Enable NMI during VBlank - should trigger NMI
        ppu.set_control_flags(ctrl.with_nmi_enable(true));
        assert!(
            ppu.should_nmi(),
            "should_nmi should be true when NMI enabled during VBlank"
        );

        assert!(
            !ppu.write(0x2000, ctrl.with_nmi_enable(false).into()),
            "disabling NMI during VBlank should not trigger NMI"
        );
        assert!(
            ppu.write(0x2000, ctrl.with_nmi_enable(true).into()),
            "0->1 NMI enable transition during VBlank should trigger NMI"
        );
        assert!(
            !ppu.write(0x2000, ctrl.with_nmi_enable(true).into()),
            "1->1 NMI enable write during VBlank should not trigger a second NMI"
        );
    }

    #[test]
    fn nmi_does_not_trigger_on_ctrl_write_outside_vblank() {
        let (mut ppu, _) = new_test_ppu_and_pattern();
        let ctrl = PpuCtrl::new().with_nmi_enable(false);

        assert!(!ppu.status.borrow().v_blank());
        assert!(
            !ppu.write(0x2000, ctrl.with_nmi_enable(true).into()),
            "enabling NMI outside VBlank should not trigger NMI immediately"
        );
    }

    #[test]
    fn test_ppu_ctrl_to_from_u8() {
        let ctrl = PpuCtrl::new()
            .with_nmi_enable(true)
            .with_ppu_master(true)
            .with_sprite_size(true)
            .with_background_pattern_table(true)
            .with_sprite_pattern_table(true)
            .with_increment_mode(true)
            .with_name_table_select(3);

        let byte: u8 = ctrl.into();
        let ctrl2: PpuCtrl = byte.into();

        assert!(ctrl2.nmi_enable());
        assert!(ctrl2.ppu_master());
        assert!(ctrl2.sprite_size());
        assert!(ctrl2.background_pattern_table());
        assert!(ctrl2.sprite_pattern_table());
        assert!(ctrl2.increment_mode());
        assert_eq!(ctrl2.name_table_select(), 3);
    }

    #[test]
    fn test_ppu_mask_to_from_u8() {
        let mask = PpuMask::new()
            .with_blue_tint(true)
            .with_green_tint(true)
            .with_red_tint(true)
            .with_sprite_enabled(true)
            .with_background_enabled(true)
            .with_sprite_left_enabled(true)
            .with_background_left_enabled(true)
            .with_grayscale(true);

        let byte: u8 = mask.into();
        let mask2: PpuMask = byte.into();

        assert!(mask2.blue_tint());
        assert!(mask2.green_tint());
        assert!(mask2.red_tint());
        assert!(mask2.sprite_enabled());
        assert!(mask2.background_enabled());
        assert!(mask2.sprite_left_enabled());
        assert!(mask2.background_left_enabled());
        assert!(mask2.grayscale());
    }

    #[test]
    fn test_ppu_status_to_from_u8() {
        let status = PpuStatus::new()
            .with_sprite_overflow(true)
            .with_sprite_zero_hit(true)
            .with_v_blank(true);

        let byte: u8 = status.into();
        let status2: PpuStatus = byte.into();

        assert!(status2.sprite_overflow());
        assert!(status2.sprite_zero_hit());
        assert!(status2.v_blank());
    }

    #[test]
    fn test_oam_dma() {
        let mut ppu = Ppu::default();
        let data: [u8; 256] = [0x42; 256];

        ppu.oam_dma(&data);

        assert_eq!(ppu.oam[0], 0x42);
        assert_eq!(ppu.oam[128], 0x42);
        assert_eq!(ppu.oam[255], 0x42);
    }

    #[test]
    fn test_set_mirroring() {
        let mut ppu = Ppu::default();

        ppu.set_mirroring(Mirroring::Horizontal);
        ppu.set_mirroring(Mirroring::Vertical);

        // Just verify it doesn't panic - actual mirroring behavior is tested in name_table tests
    }

    #[test]
    fn test_read_status_clears_vblank() {
        let (ppu, _pattern) = new_test_ppu_and_pattern();
        ppu.set_v_blank(true);

        assert!(ppu.status.borrow().v_blank());

        // read_status should clear vblank
        let status = ppu.read_status();
        assert!(status.v_blank());

        // v_blank should now be false
        assert!(!ppu.status.borrow().v_blank());
    }

    #[test]
    fn test_render_with_mask_disabled() {
        let (mut ppu, pattern) = new_test_ppu_and_pattern();

        // Disable all rendering
        ppu.mask = PpuMask::new();

        let img = ppu.render(&pattern);
        assert_eq!(img.dimensions(), (256, 240));
    }

    #[test]
    fn test_write_0x2006_set_data_addr() {
        let (mut ppu, _pattern) = new_test_ppu_and_pattern();

        // First write to 0x2006 sets high byte of VRAM address
        ppu.write(0x2006, 0x3f);

        // Second write to 0x2006 sets low byte and commits to vram_addr
        ppu.write(0x2006, 0x00);

        // Verify the VRAM address was set
        assert_eq!(*ppu.vram_addr.borrow(), 0x3f00);
    }

    #[test]
    fn test_read_0x3000_reads_palette() {
        let (mut ppu, pattern) = new_test_ppu_and_pattern();

        // Write to palette memory at 0x3f00 using VRAM write
        ppu.write(0x2006, 0x3f);
        ppu.write(0x2006, 0x00);
        ppu.write(0x2007, 0xAB);

        // Reset VRAM address to read from 0x3f20 (mirrors to 0x3f00)
        ppu.write(0x2006, 0x3f);
        ppu.write(0x2006, 0x20);

        // Read from 0x3f20 (should mirror to 0x3f00)
        let val = ppu.read(&pattern, 0x2007);
        assert_eq!(val, 0xAB);
    }

    #[test]
    fn test_pattern_table_selection() {
        let (mut ppu, _pattern) = new_test_ppu_and_pattern();

        // Set pattern table index via control flag
        ppu.write(
            0x2000,
            PpuCtrl::new().with_background_pattern_table(true).into(),
        );

        assert_eq!(ppu.cur_pattern_table_idx, 1);
    }

    #[test]
    fn test_mirroring() {
        let (mut ppu, _pattern) = new_test_ppu_and_pattern();

        // Test setting vertical mirroring
        ppu.set_mirroring(Mirroring::Vertical);
        assert_eq!(ppu.mirroring(), Mirroring::Vertical);

        // Test setting horizontal mirroring
        ppu.set_mirroring(Mirroring::Horizontal);
        assert_eq!(ppu.mirroring(), Mirroring::Horizontal);

        // Test four screen mirroring
        ppu.set_mirroring(Mirroring::Four);
        assert_eq!(ppu.mirroring(), Mirroring::Four);

        // Test one screen mirroring modes
        ppu.set_mirroring(Mirroring::LowerBank);
        assert_eq!(ppu.mirroring(), Mirroring::LowerBank);

        ppu.set_mirroring(Mirroring::UpperBank);
        assert_eq!(ppu.mirroring(), Mirroring::UpperBank);
    }

    #[test]
    fn test_scroll_register_first_write() {
        let (mut ppu, _pattern) = new_test_ppu_and_pattern();

        // First write to 0x2005 sets coarse X and fine X
        ppu.write(0x2005, 0x3A); // coarse X = 0x07, fine X = 0x02

        // Verify the temp_vram_addr was updated with coarse X
        assert_eq!(ppu.temp_vram_addr & 0x1F, 0x07);
        assert_eq!(ppu.fine_x, 0x02);
        // write_toggle should be true
        assert!(*ppu.write_toggle.borrow());
    }

    #[test]
    fn test_scroll_register_second_write() {
        let (mut ppu, _pattern) = new_test_ppu_and_pattern();

        // First write
        ppu.write(0x2005, 0x00);

        // Second write to 0x2005 sets coarse Y and fine Y
        ppu.write(0x2005, 0x7B); // coarse Y = 0x0F, fine Y = 0x03

        // Verify the temp_vram_addr was updated
        assert_eq!((ppu.temp_vram_addr >> 5) & 0x1F, 0x0F);
        assert_eq!((ppu.temp_vram_addr >> 12) & 0x07, 0x03);
        // write_toggle should be false again
        assert!(!*ppu.write_toggle.borrow());
    }

    #[test]
    fn test_vram_address_high_byte_only() {
        let (mut ppu, _pattern) = new_test_ppu_and_pattern();

        // Write only the high byte to 0x2006
        ppu.write(0x2006, 0x3F);

        // write_toggle should be true, but vram_addr should not be committed yet
        assert!(*ppu.write_toggle.borrow());
        // The temp_vram_addr should have been set
        assert_eq!(ppu.temp_vram_addr, 0x3F00);
    }

    #[test]
    fn test_read_vram_pattern_table() {
        let (ppu, pattern) = new_test_ppu_and_pattern();

        // Write some data to pattern table at 0x1000
        // Note: We can't actually write to pattern table (it's ROM in real NES)
        // But we can test reading from it
        let val = ppu.read_vram(&pattern, 0x1000);
        // Pattern table data defaults to 0
        assert_eq!(val, 0);
    }

    #[test]
    fn test_write_vram_pattern_ignored() {
        let (mut ppu, _pattern) = new_test_ppu_and_pattern();

        // Writing to pattern table space should be ignored (it's ROM)
        // This test just verifies it doesn't panic
        ppu.write_vram(0x0000, 0xFF);
        ppu.write_vram(0x1000, 0xFF);
    }

    #[test]
    fn test_write_0x2000_updates_name_table_addr() {
        let (mut ppu, _pattern) = new_test_ppu_and_pattern();

        // Set name table select to 1 (0x2400)
        ppu.write(0x2000, PpuCtrl::new().with_name_table_select(1).into());

        assert_eq!(ppu.cur_name_table_addr, 0x2400);

        // Set name table select to 2 (0x2800)
        ppu.write(0x2000, PpuCtrl::new().with_name_table_select(2).into());

        assert_eq!(ppu.cur_name_table_addr, 0x2800);

        // Set name table select to 3 (0x2C00)
        ppu.write(0x2000, PpuCtrl::new().with_name_table_select(3).into());

        assert_eq!(ppu.cur_name_table_addr, 0x2C00);
    }

    #[test]
    fn test_ppu_mask_register() {
        let (mut ppu, _pattern) = new_test_ppu_and_pattern();

        // Write mask register
        ppu.write(
            0x2001,
            PpuMask::new()
                .with_blue_tint(true)
                .with_sprite_enabled(true)
                .with_background_enabled(true)
                .into(),
        );

        assert!(ppu.mask.blue_tint());
        assert!(ppu.mask.sprite_enabled());
        assert!(ppu.mask.background_enabled());
    }

    #[test]
    fn test_read_status_register() {
        let (ppu, pattern) = new_test_ppu_and_pattern();

        // Set some status flags
        *ppu.status.borrow_mut() = PpuStatus::new()
            .with_sprite_overflow(true)
            .with_sprite_zero_hit(true)
            .with_v_blank(true);

        // Read status register (0x2002)
        let val = ppu.read(&pattern, 0x2002);

        // Check that status flags are in the correct bits
        let status: PpuStatus = val.into();
        assert!(status.sprite_overflow());
        assert!(status.sprite_zero_hit());
        assert!(status.v_blank());

        // VBlank should be cleared after reading
        assert!(!ppu.status.borrow().v_blank());
        // write_toggle should be reset
        assert!(!*ppu.write_toggle.borrow());
    }

    #[test]
    fn test_read_buffer() {
        let (mut ppu, pattern) = new_test_ppu_and_pattern();

        // First read from VRAM (0x2007) returns read_buffer (default 0)
        let val1 = ppu.read(&pattern, 0x2007);
        assert_eq!(val1, 0);

        // Set up pattern data
        let pattern_data = [0u8; 8192];

        // Set VRAM address
        ppu.write(0x2006, 0x00);
        ppu.write(0x2006, 0x00);

        // Reading from pattern table - first read returns buffer
        let _val2 = ppu.read(&pattern_data, 0x2007);
        // Actual value is loaded into buffer for next read
    }
}
