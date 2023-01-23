mod mapping;
mod ram;

use crate::nes::ppu::PpuTrait;
use image::RgbaImage;
pub use mapping::*;
pub use ram::RamMcu;

pub trait MachineMcu {
    fn render(&self) -> &RgbaImage;
}

/// Nes 6502 Mcu.
///
/// Note: addr is absolute address, not the offset from start of the memory region.
/// Make it easy to implement memory mapped devices..
pub trait Mcu {
    fn read(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, value: u8);

    // TODO: remove this hack
    fn get_ppu(&mut self) -> &mut dyn PpuTrait {
        panic!("not implemented");
    }

    // TODO: remove this hack
    fn get_machine_mcu(&self) -> &dyn MachineMcu {
        todo!()
    }
}
