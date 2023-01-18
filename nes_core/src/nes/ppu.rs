use crate::mcu::{DefinedRegion, Mcu};
use crate::to_from_u8;
use modular_bitfield::prelude::*;
use std::cell::Cell;

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

pub trait PpuDriver {
    fn set_ctrl(&mut self, ctrl: PpuCtrl);
    fn set_mask(&mut self, mask: PpuMask);
    fn get_status(&self) -> PpuStatus;
    fn get_oma_address(&self) -> u8;
    fn set_oma_address(&mut self, address: u8);
    fn get_oma_data(&self) -> u8;
    fn set_oma_data(&mut self, data: u8);
    fn set_scroll_position(&mut self, x: u8, y: u8);
    fn set_address(&mut self, address: u16); // ppu address
    fn get_data(&self) -> u8;
    fn set_data(&mut self, data: u8);
    fn set_dma(&mut self, page_addr: u8);
}

pub struct Ppu<D: PpuDriver> {
    driver: D,
    scroll_x: Cell<Option<u8>>, // set to None after read status register.
    data_addr_high: Cell<Option<u8>>, // set to None after read status register.
}

impl<D: PpuDriver> Ppu<D> {
    pub fn new(driver: D) -> Self {
        Self {
            driver,
            scroll_x: Cell::new(None),
            data_addr_high: Cell::new(None),
        }
    }
}

impl<D: PpuDriver> Mcu for Ppu<D> {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x2002 => {
                let status = self.driver.get_status();
                self.scroll_x.set(None);
                self.data_addr_high.set(None);
                status.into()
            }
            0x2004 => self.driver.get_oma_data(),
            0x2007 => self.driver.get_data(),
            _ => 0x55,
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x2000 => self.driver.set_ctrl(value.into()),
            0x2001 => self.driver.set_mask(value.into()),
            0x2003 => self.driver.set_oma_address(value),
            0x2004 => self.driver.set_oma_data(value),
            0x2005 => {
                if let Some(x) = self.scroll_x.get() {
                    self.driver.set_scroll_position(x, value);
                    self.scroll_x.set(None);
                } else {
                    self.scroll_x.set(Some(value));
                }
            }
            0x2006 => {
                if let Some(high) = self.data_addr_high.get() {
                    self.driver.set_address((high as u16) << 8 | value as u16);
                    self.data_addr_high.set(None);
                } else {
                    self.data_addr_high.set(Some(value));
                }
            }
            0x2007 => self.driver.set_data(value),
            0x4014 => self.driver.set_dma(value),
            _ => panic!("Can not write to Ppu at address ${:x}", address),
        }
    }
}

impl<D: PpuDriver> DefinedRegion for Ppu<D> {
    fn region(&self) -> (u16, u16) {
        // TODO: how about 4014 ?
        (0x2000, 0x2008)
    }
}

pub fn new<D: PpuDriver>(d: D) -> impl Mcu + DefinedRegion {
    Mappers(Ppu::new(d))
}

struct Mappers<D: PpuDriver>(Ppu<D>);

impl<D: PpuDriver> Mappers<D> {
    fn addr(&self, addr: u16) -> u16 {
        0x2000 + (addr % 8)
    }
}

impl<D: PpuDriver> DefinedRegion for Mappers<D> {
    fn region(&self) -> (u16, u16) {
        (0x2000, 0x3fff)
    }
}

impl<D: PpuDriver> Mcu for Mappers<D> {
    fn read(&self, address: u16) -> u8 {
        self.0.read(self.addr(address))
    }

    fn write(&mut self, address: u16, value: u8) {
        self.0.write(self.addr(address), value)
    }
}
