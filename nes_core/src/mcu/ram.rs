use crate::mcu::{DefinedRegion, Mcu};

pub struct RamMcu<const SIZE: usize> {
    /// Start address of the memory region
    start: usize,
    ram: [u8; SIZE],
}

impl<const SIZE: usize> RamMcu<SIZE> {
    pub fn new(ram: [u8; SIZE]) -> RamMcu<SIZE> {
        RamMcu { start: 0, ram }
    }

    pub fn start_from(start: usize) -> Self {
        RamMcu {
            start,
            ram: [0; SIZE],
        }
    }
}

// impl super::Mcu trait
impl<const SIZE: usize> Mcu for RamMcu<SIZE> {
    fn read(&self, address: u16) -> u8 {
        self.ram[address as usize - self.start]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.ram[address as usize - self.start] = value;
    }
}

impl<const SIZE: usize> DefinedRegion for RamMcu<SIZE> {
    fn region(&self) -> (u16, u16) {
        (self.start as u16, (self.start - 1 + SIZE) as u16)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn start_from() {
        let mut mcu = RamMcu::<0x100>::start_from(0x8000);
        mcu.write(0x8000, 0x12);
        assert_eq!(mcu.read(0x8000), 0x12);
    }
}
