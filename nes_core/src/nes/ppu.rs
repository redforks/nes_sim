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

pub struct Ppu {}

impl Ppu {
    pub fn new() -> Self {
        Self {}
    }
}

impl Mcu for Ppu {
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

impl DefinedRegion for Ppu {
    fn region(&self) -> (u16, u16) {
        // TODO: how about 4014 ?
        (0x2000, 0x2008)
    }
}

pub fn new() -> impl Mcu + DefinedRegion {
    Ppu::new()
}
