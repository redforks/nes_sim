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
    fn write(&mut self, address: u16, value: u8);

    fn request_irq(&self) -> bool {
        panic!("request_irq() not implemented");
    }

    /// Tick PPU by one dot. Returns true if NMI should be triggered.
    /// Pattern data is passed through from the cartridge for rendering.
    fn tick_ppu(&mut self) -> bool {
        false
    }

    /// Returns true if VBlank started since the last call to this method.
    /// Used by `Machine::process_frame()` to detect the natural frame boundary.
    fn take_vblank(&mut self) -> bool {
        false
    }

    /// Tick APU frame counter. Returns true if frame IRQ should be triggered.
    fn tick_apu(&mut self) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MockMcu;

    impl Mcu for MockMcu {
        fn read(&self, _address: u16) -> u8 {
            0
        }

        fn write(&mut self, _address: u16, _value: u8) {}
    }

    #[test]
    fn test_tick_ppu_default() {
        let mut mcu = MockMcu;
        // Default implementation returns false
        assert!(!mcu.tick_ppu());
    }

    #[test]
    #[should_panic(expected = "request_irq() not implemented")]
    fn test_request_irq_default_panics() {
        let mcu = MockMcu;
        let _ = mcu.request_irq();
    }
}
