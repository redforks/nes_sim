use crate::mcu::Mcu;

pub struct RamMcu<const SIZE: usize> {
    /// Start address of the memory region
    start: u16,
    ram: [u8; SIZE],
    address_latch: Option<u16>,
}

impl<const SIZE: usize> RamMcu<SIZE> {
    pub fn new(ram: [u8; SIZE]) -> RamMcu<SIZE> {
        RamMcu {
            start: 0,
            ram,
            address_latch: None,
        }
    }

    pub fn start_from(start: u16, ram: [u8; SIZE]) -> Self {
        RamMcu {
            start,
            ram,
            address_latch: None,
        }
    }

    fn to_index(&self, address: u16) -> usize {
        (address - self.start) as usize
    }
}

// impl super::Mcu trait
impl<const SIZE: usize> Mcu for RamMcu<SIZE> {
    fn peek(&self, address: u16) -> u8 {
        self.ram[self.to_index(address)]
    }

    fn prepare_read(&mut self, address: u16) {
        debug_assert!(
            self.address_latch.is_none(),
            "address latch should be empty when prepare_read"
        );
        self.address_latch = Some(address);
    }

    fn new_read(&mut self) -> u8 {
        let address = self
            .address_latch
            .take()
            .expect("address latch should have value when new_read");
        self.ram[self.to_index(address)]
    }

    fn prepare_write(&mut self, address: u16) {
        debug_assert!(
            self.address_latch.is_none(),
            "address latch should be empty when prepare_write"
        );
        self.address_latch = Some(address);
    }

    fn new_write(&mut self, value: u8) {
        let address = self
            .address_latch
            .take()
            .expect("address latch should have value when new_write");
        let index = self.to_index(address);
        self.ram[index] = value;
    }
}

impl<const SIZE: usize> RamMcu<SIZE> {
    pub fn region(&self) -> (u16, u16) {
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
        mcu.prepare_write(0x8000);
        mcu.new_write(0x12);
        assert_eq!(mcu.peek(0x8000), 0x12);
        mcu.prepare_write(0xfffc);
        mcu.new_write(0x12);
        assert_eq!(mcu.peek(0xfffc), 0x12);

        let mut mcu = RamMcu::new([0; 0x100]);
        mcu.prepare_write(0x80);
        mcu.new_write(0x12);
        assert_eq!(mcu.peek(0x80), 0x12);
    }

    #[test]
    fn test_new_with_non_zero_start() {
        let mcu = RamMcu::new([0; 0x100]);
        // start should be 0 for new()
        let (start, end) = mcu.region();
        assert_eq!(start, 0);
        assert_eq!(end, 0xFF); // SIZE - 1
    }

    #[test]
    fn test_region_with_start() {
        let mcu = RamMcu::start_from(0x8000, [0; 0x100]);
        let (start, end) = mcu.region();
        assert_eq!(start, 0x8000);
        assert_eq!(end, 0x80FF); // 0x8000 + 0x100 - 1
    }

    #[test]
    fn test_to_index() {
        let mut mcu = RamMcu::start_from(0x8000, [0; 0x100]);
        // to_index is private, but we can test it indirectly through read/write
        mcu.prepare_write(0x8000);
        mcu.new_write(0xAB);
        assert_eq!(mcu.peek(0x8000), 0xAB);

        mcu.prepare_write(0x80FF);
        mcu.new_write(0xCD);
        assert_eq!(mcu.peek(0x80FF), 0xCD);
    }
}
