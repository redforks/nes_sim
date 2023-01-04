use crate::mcu::Mcu;

pub struct RamMcu<const SIZE: usize> {
    ram: [u8; SIZE],
}

impl<const SIZE: usize> RamMcu<SIZE> {
    pub fn new(ram: [u8; SIZE]) -> RamMcu<SIZE> {
        RamMcu { ram }
    }
}

// impl super::Mcu trait
impl<const SIZE: usize> Mcu for RamMcu<SIZE> {
    fn read(&self, address: u16) -> u8 {
        self.ram[address as usize]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.ram[address as usize] = value;
    }
}
