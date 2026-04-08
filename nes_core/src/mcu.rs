mod ram;

pub use ram::RamMcu;

/// Nes 6502 Mcu.
///
/// Note: addr is absolute address, not the offset from start of the memory region.
/// Make it easy to implement memory mapped devices..
pub trait Mcu {
    // Changed to &mut self to allow implementations to mutate state during reads
    fn read(&mut self, address: u16) -> u8;
    fn peek(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, value: u8);

    fn take_dmc_dma_address(&mut self) -> Option<u16> {
        None
    }

    fn perform_dmc_dma_read(&mut self, sample_addr: u16, _cpu_read_addr: u16) -> u8 {
        self.read(sample_addr)
    }

    fn supply_dmc_dma_byte(&mut self, _byte: u8) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;

    struct MockMcu {
        data: RefCell<u8>,
    }

    impl MockMcu {
        fn new() -> Self {
            MockMcu {
                data: RefCell::new(0),
            }
        }
    }

    impl Mcu for MockMcu {
        fn read(&mut self, _address: u16) -> u8 {
            *self.data.borrow()
        }

        fn peek(&self, _address: u16) -> u8 {
            *self.data.borrow()
        }

        fn write(&mut self, _address: u16, value: u8) {
            *self.data.borrow_mut() = value;
        }
    }

    #[test]
    fn test_read_write() {
        let mut mcu = MockMcu::new();
        mcu.write(0x0000, 0x42);
        assert_eq!(mcu.read(0x0000), 0x42);
    }
}
