use crate::mcu::{DefinedRegion, Mcu};
use crate::to_from_u8;
use modular_bitfield::prelude::*;

mod pattern;

pub use pattern::*;

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

#[derive(Default)]
struct Palette {
    data: [u8; 0x20],
}

impl Palette {
    fn get_addr(&self, addr: u16) -> usize {
        (addr - PALETTE_MEM_START) as usize
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

pub struct Ppu<PM, NM> {
    pattern: PM, // pattern memory 
    name_table: NM, // name table
    palette: Palette, // palette memory
}

impl<PM, NM> Ppu<PM, NM> {
    pub fn new(pattern: PM, name_table: NM) -> Self {
        Ppu {
            pattern,
            name_table,
            palette: Palette::default(),
        }
    }
}

impl <PM, NM> Mcu for Ppu<PM, NM> {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x2002 => {
                todo!()
            }
            0x2004 => todo!(),
            0x2007 => todo!(),
            _ => 0x55,
        }
    }

    fn write(&mut self, address: u16, _: u8) {
        match address {
            0x2000 => todo!(),
            0x2001 => todo!(),
            0x2003 => todo!(),
            0x2004 => todo!(),
            0x2005 => {
                todo!()
            }
            0x2006 => {
                todo!()
            }
            0x2007 => todo!(),
            0x4014 => todo!(),
            _ => panic!("Can not write to Ppu at address ${:x}", address),
        }
    }
}

impl <PM, NM> DefinedRegion for Ppu<PM, NM> {
    fn region(&self) -> (u16, u16) {
        // TODO: how about 4014 ?
        (0x2000, 0x2008)
    }
}

/// Create Ppu. PM and NM are MCU work in PPU address space.
pub fn new<PM, NM>(pm: PM, nm: NM) -> impl Mcu + DefinedRegion {
    Ppu::new(pm, nm)
}
