use crate::mcu::Mcu;
use crate::to_from_u8;
use image::{Rgb, RgbImage};
use log::debug;
use modular_bitfield::prelude::*;
use std::cell::RefCell;

mod pattern;

pub use pattern::*;

type RGB = Rgb<u8>;

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
    pub sprite_enable: bool,
    pub background_enable: bool,
    pub grayscale: bool,
    pub sprite_left: bool,
    pub background_left: bool,
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

const fn rgb(v: [u8; 3]) -> RGB {
    Rgb::<u8>(v)
}

#[rustfmt::skip]
const COLORS: [RGB; 64] = [
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

    fn _get_color_idx(&self, start: usize, palette_idx: u8, idx: u8) -> RGB {
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

    fn get_background_color(&self, palette_idx: u8, idx: u8) -> RGB {
        self._get_color_idx(0, palette_idx, idx)
    }

    fn get_sprit_color(&self, palette_idx: u8, idx: u8) -> RGB {
        self._get_color_idx(0x10, palette_idx, idx)
    }
}

impl Mcu for Palette {
    fn read(&self, address: u16) -> u8 {
        self.data[self.get_addr(address)]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.data[self.get_addr(address)] = value;
    }
}

struct NameTable<'a> {
    table: &'a [u8],
    pattern: Pattern<'a>,
}

impl<'a> NameTable<'a> {
    fn new(table: &'a [u8], pattern: Pattern<'a>) -> Self {
        Self { table, pattern }
    }

    fn tile(&self, x: u8, y: u8) -> Tile<'a> {
        let idx = (y as usize * 32) + x as usize;
        self.pattern.tile(self.table[idx] as usize)
    }

    fn palette_idx(&self, x: u8, y: u8) -> u8 {
        let idx = (y as usize * 32) + x as usize;
        self.table[idx + 0x3c0]
    }
}

pub trait PpuTrait: Mcu {
    fn oam_dma(&mut self, vals: &[u8; 256]);

    /// Returns true if should trigger nmi at the start of v-blank.
    fn should_nmi(&self) -> bool;

    /// Trigger nmi at the start of v-blank, if should_nmi() returns true.
    fn set_v_blank(&mut self, v_blank: bool);
}

pub struct Ppu<PM, NM> {
    ctrl_flags: PpuCtrl,
    status: RefCell<PpuStatus>,
    pattern: PM,               // pattern memory
    name_table: NM,            // name table
    cur_name_table_addr: u16,  // current active name table start address
    palette: Palette,          // palette memory
    cur_pattern_table_idx: u8, // index of current active pattern table, 0 or 1

    oam_addr: u8,
    oam: [u8; 0x100], // object attribute memory

    data_rw_addr: RefCell<Option<u16>>, // None after reset
}

impl<PM: Mcu, NM: Mcu> PpuTrait for Ppu<PM, NM> {
    fn oam_dma(&mut self, vals: &[u8; 256]) {
        self.oam.copy_from_slice(vals);
    }

    fn should_nmi(&self) -> bool {
        self.ctrl_flags.nmi_enable() && self.status.borrow().v_blank()
    }

    fn set_v_blank(&mut self, v_blank: bool) {
        let status = *self.status.borrow();
        self.status = RefCell::new(status.with_v_blank(v_blank));
    }
}

impl<PM, NM> Ppu<PM, NM> {
    pub fn new(pattern: PM, name_table: NM) -> Self {
        Ppu {
            ctrl_flags: 0.into(),
            status: RefCell::new(0.into()),
            pattern,
            name_table,
            cur_name_table_addr: 0x2000,
            palette: Palette::default(),
            cur_pattern_table_idx: 0,
            oam_addr: 0,
            oam: [0; 0x100],
            data_rw_addr: RefCell::new(None),
        }
    }

    fn set_control_flags(&mut self, flag: PpuCtrl) {
        self.ctrl_flags = flag;
        self.cur_name_table_addr = match flag.name_table_select() {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2c00,
            _ => unreachable!(),
        };
        self.cur_pattern_table_idx = flag.background_pattern_table() as u8;
    }

    fn set_ppu_mask(&mut self, _: PpuMask) {
        debug!("TODO: set ppu mask");
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
        // reset data_rw_addr on read status register
        *self.data_rw_addr.borrow_mut() = None;
        let r = *self.status.borrow();
        *self.status.borrow_mut() = r.with_v_blank(false);
        r
    }
}

impl<PM: Mcu, NM: Mcu> Ppu<PM, NM> {
    fn read_vram(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.pattern.read(address),
            0x2000..=0x2fff => self.name_table.read(address),
            0x3000..=0x3eff => self.read_vram(address - 0x1000),
            0x3f00..=0x3f1f => self.palette.read(address),
            0x3f20..=0x3fff => self.read_vram(address - 0x20),
            _ => unreachable!(),
        }
    }

    fn write_vram(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1fff => self.pattern.write(address, value),
            0x2000..=0x2fff => self.name_table.write(address, value),
            0x3000..=0x3eff => self.write_vram(address - 0x1000, value),
            0x3f00..=0x3f1f => self.palette.write(address, value),
            0x3f20..=0x3fff => self.write_vram(address - 0x20, value),
            _ => unreachable!(),
        }
    }

    fn set_data_rw_addr(&mut self, address: u8) {
        let addr = *self.data_rw_addr.borrow();
        match addr {
            None => {
                self.data_rw_addr = RefCell::new(Some(address as u16));
            }
            Some(addr) => {
                self.data_rw_addr = RefCell::new(Some((addr as u16) << 8 | address as u16));
            }
        }
    }

    fn inc_data_rw_addr(&self, addr: u16) {
        let delta = if self.ctrl_flags.increment_mode() {
            32
        } else {
            1
        };
        *self.data_rw_addr.borrow_mut() = Some(addr.wrapping_add(delta));
    }

    fn read_vram_for_cpu(&self) -> u8 {
        let addr = self.data_rw_addr.borrow().expect("data_rw addr not set");
        let value = self.read_vram(addr);
        self.inc_data_rw_addr(addr);
        value
    }

    fn write_vram_for_cpu(&mut self, v: u8) {
        let addr = self.data_rw_addr.borrow().expect("data_rw addr not set");
        self.write_vram(addr, v);
        self.inc_data_rw_addr(addr);
    }
}

impl<PM, NM> Ppu<PM, NM>
where
    PM: Mcu + AsRef<[u8]>,
    NM: Mcu + AsRef<[u8]>,
{
    pub fn render(&self, image: &mut RgbImage) {
        let pattern =
            PatternBand::new(self.pattern.as_ref()).pattern(self.cur_pattern_table_idx as usize);
        let cur_name_table = self.cur_name_table_addr as usize;
        let name_table = NameTable::new(
            &self.name_table.as_ref()[cur_name_table..cur_name_table + 0x0400],
            pattern,
        );
        for (tile_x, tile_y) in itertools::iproduct!(0..32, 0..30) {
            let tile = name_table.tile(tile_x, tile_y);
            let palette_idx = name_table.palette_idx(tile_x, tile_y);
            for (x, y, pixel) in tile.iter() {
                let color = self.palette.get_background_color(palette_idx, pixel);
                image.put_pixel(x as u32, y as u32, color);
            }
        }
    }
}

impl<PM, NM> Mcu for Ppu<PM, NM>
where
    PM: Mcu,
    NM: Mcu,
{
    fn read(&self, address: u16) -> u8 {
        // todo: mirror to 0x3fff
        match address {
            0x2002 => self.read_status().into(),
            0x2004 => todo!(),
            0x2006 => self.read_oam_data(),
            0x2007 => self.read_vram_for_cpu(),
            _ => 0x55,
        }
    }

    fn write(&mut self, address: u16, val: u8) {
        // todo: mirror to 0x3fff
        match address {
            0x2000 => self.set_control_flags(PpuCtrl::from(val)),
            0x2001 => self.set_ppu_mask(PpuMask::from(val)),
            0x2003 => self.set_oma_addr(val),
            0x2004 => self.write_oam_data_and_inc(val),
            0x2005 => {
                todo!()
            }
            0x2006 => self.set_data_rw_addr(val),
            0x2007 => self.write_vram_for_cpu(val),
            0x4014 => todo!(),
            _ => panic!("Can not write to Ppu at address ${:x}", address),
        }
    }
}

/// Create Ppu. PM and NM are MCU work in PPU address space.
pub fn new<PM, NM>(pm: PM, nm: NM) -> impl Mcu
where
    PM: Mcu + AsRef<[u8]>,
    NM: Mcu + AsRef<[u8]>,
{
    Ppu::new(pm, nm)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mcu::RamMcu;

    #[test]
    fn read_write_v_ram() {
        let mut ppu = Ppu::new(RamMcu::new([0; 0x4000]), RamMcu::new([0; 0x4000]));
        ppu.read(0x2000); // reset addr
        ppu.write(0x2006, 0x21);
        ppu.write(0x2006, 0x08);
        ppu.write(0x2007, 0x12);
        ppu.write(0x2007, 0x34);
        assert_eq!(ppu.read_vram(0x2108), 0x12);
        assert_eq!(ppu.read_vram(0x2109), 0x34);

        ppu.read(0x2000); // reset addr
        ppu.write(0x2006, 0x21);
        ppu.write(0x2006, 0x08);
        assert_eq!(ppu.read(0x2007), 0x12);
        assert_eq!(ppu.read(0x2007), 0x34);

        // set increase mode to 32
        ppu.write(0x2000, PpuCtrl::new().with_increment_mode(true).into());
        ppu.read(0x2000); // reset addr
        ppu.write(0x2006, 0x21);
        ppu.write(0x2006, 0x08);
        ppu.write(0x2007, 0x56);
        ppu.write(0x2007, 0x78);
        assert_eq!(ppu.read_vram(0x2108), 0x56);
        assert_eq!(ppu.read_vram(0x2128), 0x78);

        ppu.read(0x2000); // reset addr
        ppu.write(0x2006, 0x21);
        ppu.write(0x2006, 0x08);
        assert_eq!(ppu.read(0x2007), 0x56);
        assert_eq!(ppu.read(0x2007), 0x78);
    }

    #[test]
    fn read_write_oam() {
        let mut ppu = Ppu::new(RamMcu::new([0; 0x4000]), RamMcu::new([0; 0x4000]));
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
        let mut ppu = Ppu::new(RamMcu::new([0; 0x4000]), RamMcu::new([0; 0x4000]));

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
}
