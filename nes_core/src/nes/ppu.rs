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

#[derive(Copy, Clone)]
#[bitfield]
pub struct PpuStatus {
    pub v_blank: bool,
    pub sprite_zero_hit: bool,
    pub sprite_overflow: bool,
    #[allow(non_snake_case)]
    #[skip]
    __: B5,
}
to_from_u8!(PpuStatus);

const PALETTE_MEM_START: u16 = 0x3f00;
const NAME_TABLE_MEM_START: u16 = 0x2000;
const TILES_PER_ROW: u8 = 32;
const TILES_PER_COL: u8 = 30;

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

    data_rw_addr: RefCell<u16>, // None after reset
    image: RgbaImage,
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
            data_rw_addr: RefCell::new(0),
            image: RgbaImage::new(256, 240),
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

    fn set_control_flags(&mut self, flag: PpuCtrl) {
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
        *self.data_rw_addr.borrow_mut() = 0;
        self.set_v_blank(false)
    }

    pub fn read(&self, pattern: &[u8], address: u16) -> u8 {
        // todo: mirror to 0x3fff
        match address {
            0x2002 => self.read_status().into(),
            0x2004 => self.read_oam_data(),
            0x2007 => self.read_vram_and_inc(pattern),
            _ => 0x55,
        }
    }

    pub fn write(&mut self, address: u16, val: u8) {
        // todo: mirror to 0x3fff
        match address {
            0x2000 => self.set_control_flags(PpuCtrl::from(val)),
            0x2001 => self.set_ppu_mask(PpuMask::from(val)),
            0x2003 => self.set_oma_addr(val),
            0x2004 => self.write_oam_data_and_inc(val),
            0x2005 => {
                // todo: scroll
            }
            0x2006 => self.set_data_rw_addr(val),
            0x2007 => self.write_vram_and_inc(val),
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

    fn set_data_rw_addr(&mut self, address: u8) {
        let addr = *self.data_rw_addr.borrow();
        self.data_rw_addr = RefCell::new(addr << 8 | address as u16);
    }

    fn inc_data_rw_addr(&self) {
        let delta = if self.ctrl_flags.increment_mode() {
            32
        } else {
            1
        };
        let mut addr = self.data_rw_addr.borrow_mut();
        *addr = (*addr).wrapping_add(delta);
    }

    fn read_vram_and_inc(&self, pattern: &[u8]) -> u8 {
        let addr = *self.data_rw_addr.borrow();
        let value = self.read_vram(pattern, addr);
        self.inc_data_rw_addr();
        value
    }

    fn write_vram_and_inc(&mut self, v: u8) {
        let addr = *self.data_rw_addr.borrow();
        self.write_vram(addr, v);
        self.inc_data_rw_addr();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new_test_ppu_and_pattern() -> (Ppu, [u8; 8192]) {
        let pattern = [0; 8192];
        (Ppu::default(), pattern)
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
        assert_eq!(ppu.status.borrow().v_blank(), false);

        // disable nmi
        ppu.set_control_flags(flag);
        ppu.set_v_blank(true);
        assert_eq!(ppu.status.borrow().v_blank(), true);
        // should_nmi is false because nmi is disabled
        assert_eq!(ppu.should_nmi(), false);
        ppu.set_v_blank(false);
        // v_blank is false on v_blank end
        assert_eq!(ppu.status.borrow().v_blank(), false);

        // read status register will reset v_blank flag
        ppu.set_v_blank(true);
        assert_eq!(ppu.read_status().v_blank(), true);
        assert_eq!(ppu.status.borrow().v_blank(), false);

        // enable nmi
        ppu.set_control_flags(flag.with_nmi_enable(true));
        assert_eq!(ppu.should_nmi(), false);
        ppu.set_v_blank(true);
        assert_eq!(ppu.status.borrow().v_blank(), true);
        // should_nmi is true because nmi is enabled
        assert_eq!(ppu.should_nmi(), true);
    }

    #[test]
    fn render() {
        let (mut ppu, pattern) = new_test_ppu_and_pattern();
        ppu.render(&pattern);
    }
}
