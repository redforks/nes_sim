mod mapping;
mod ram;

pub use mapping::*;
pub use ram::RamMcu;

/// Nes 6502 Mcu.
///
/// Note: addr is absolute address, not the offset from start of the memory region.
/// Make it easy to implement memory mapped devices..
pub trait Mcu {
    fn read(&self, address: u16) -> u8;
    fn write(&self, address: u16, value: u8);

    fn request_irq(&self) -> bool {
        panic!("request_irq() not implemented");
    }
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
        fn read(&self, _address: u16) -> u8 {
            *self.data.borrow()
        }

        fn write(&self, _address: u16, value: u8) {
            *self.data.borrow_mut() = value;
        }
    }

    #[test]
    fn test_read_write() {
        let mcu = MockMcu::new();
        mcu.write(0x0000, 0x42);
        assert_eq!(mcu.read(0x0000), 0x42);
    }

    #[test]
    #[should_panic(expected = "request_irq() not implemented")]
    fn test_request_irq_default_panics() {
        let mcu = MockMcu::new();
        let _ = mcu.request_irq();
    }
}
