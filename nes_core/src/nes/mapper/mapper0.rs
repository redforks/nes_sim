use crate::mcu::Mcu;
use crate::nes::mapper::Cartridge;

pub struct Mapper0 {
    // todo
}

impl Mcu for Mapper0 {
    fn read(&self, address: u16) -> u8 {
        todo!()
    }

    fn write(&mut self, address: u16, value: u8) {
        todo!()
    }
}

impl Cartridge for Mapper0 {
    fn pattern_ref(&self) -> &[u8] {
        todo!()
    }

    fn pattern_mut(&mut self) -> &mut [u8] {
        todo!()
    }
}
