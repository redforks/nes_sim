mod ram;

pub use ram::RamMcu;

/// Nes 6502 Mcu.
///
/// Note: addr is absolute address, not the offset from start of the memory region.
/// Make it easy to implement memory mapped devices..
pub trait Mcu {
    /// Changed to &mut self to allow implementations to mutate state during reads.
    /// deprecated by `prepare_read()` and `new_read()`.
    fn read(&mut self, address: u16) -> u8;
    /// deprated by `prepare_write()` and `new_write()`.
    fn write(&mut self, address: u16, value: u8);

    /// Return the value of specific address, not triggers side-effect, only used in debug/log.
    fn peek(&self, address: u16) -> u8;

    /// Notify mcu to prepare for read, normally prepare in first phase of Microcode
    fn prepare_read(&mut self, address: u16);
    /// Perform read, normally perform in second phase of Microcode, return the value to be used by cpu
    fn new_read(&mut self) -> u8;

    /// Notify mcu to prepare for write, normally prepare in first phase of Microcode
    fn prepare_write(&mut self, address: u16);
    /// Perform write, normally perform in second phase of Microcode, value is the value to be written to the address
    fn new_write(&mut self, value: u8);

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
        address_latch: Option<u16>,
    }

    impl MockMcu {
        fn new() -> Self {
            MockMcu {
                data: RefCell::new(0),
                address_latch: None,
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

        fn prepare_read(&mut self, address: u16) {
            debug_assert!(
                self.address_latch.is_none(),
                "address latch should be empty when prepare_read"
            );
            self.address_latch = Some(address);
        }

        fn new_read(&mut self) -> u8 {
            let addr = self
                .address_latch
                .take()
                .expect("address latch should have value when new_read");
            self.read(addr)
        }

        fn prepare_write(&mut self, address: u16) {
            debug_assert!(
                self.address_latch.is_none(),
                "address latch should be empty when prepare_write"
            );
            self.address_latch = Some(address);
        }

        fn new_write(&mut self, value: u8) {
            let addr = self
                .address_latch
                .take()
                .expect("address latch should have value when new_write");
            self.write(addr, value);
        }
    }

    #[test]
    fn test_read_write() {
        let mut mcu = MockMcu::new();
        mcu.write(0x0000, 0x42);
        assert_eq!(mcu.read(0x0000), 0x42);
    }
}
