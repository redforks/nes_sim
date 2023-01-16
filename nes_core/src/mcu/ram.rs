use crate::mcu::{DefinedRegion, Mcu};

pub struct RamMcu<const SIZE: usize> {
    /// Start address of the memory region
    start: u16,
    ram: [u8; SIZE],
}

impl<const SIZE: usize> RamMcu<SIZE> {
    pub fn new(ram: [u8; SIZE]) -> RamMcu<SIZE> {
        RamMcu { start: 0, ram }
    }

    pub fn start_from(start: u16, ram: [u8; SIZE]) -> Self {
        RamMcu { start, ram }
    }

    fn to_index(&self, address: u16) -> usize {
        (address - self.start) as usize
    }
}

// impl super::Mcu trait
impl<const SIZE: usize> Mcu for RamMcu<SIZE> {
    fn read(&self, address: u16) -> u8 {
        self.ram[self.to_index(address)]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.ram[self.to_index(address)] = value;
    }
}

impl<const SIZE: usize> DefinedRegion for RamMcu<SIZE> {
    fn region(&self) -> (u16, u16) {
        if self.start == 0 {
            (0, (SIZE - 1) as u16)
        } else {
            (self.start, self.start - 1 + (SIZE as u16))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn start_from() {
        let mut mcu = RamMcu::start_from(0x8000, [0; 0x8000]);
        mcu.write(0x8000, 0x12);
        assert_eq!(mcu.read(0x8000), 0x12);
        mcu.write(0xfffc, 0x12);
        assert_eq!(mcu.read(0xfffc), 0x12);

        let mut mcu = RamMcu::new([0; 0x100]);
        mcu.write(0x80, 0x12);
        assert_eq!(mcu.read(0x80), 0x12);
    }
}
