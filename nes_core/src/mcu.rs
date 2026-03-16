mod mapping;
mod ram;

use crate::nes::ppu::PpuTrait;
pub use mapping::*;
pub use ram::RamMcu;

pub trait MachineMcu {
    fn render(&mut self);
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
    fn get_machine_mcu(&mut self) -> &mut dyn MachineMcu {
        panic!("not implemented");
    }

    fn request_irq(&self) -> bool {
        panic!("request_irq() not implemented");
    }

    /// Tick PPU by one dot. Returns true if NMI should be triggered.
    fn tick_ppu(&mut self) -> bool {
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
    #[should_panic(expected = "not implemented")]
    fn test_get_ppu_default_panics() {
        let mut mcu = MockMcu;
        let _ppu = mcu.get_ppu();
    }

    #[test]
    #[should_panic(expected = "not implemented")]
    fn test_get_machine_mcu_default_panics() {
        let mut mcu = MockMcu;
        let _machine_mcu = mcu.get_machine_mcu();
    }

    #[test]
    #[should_panic(expected = "request_irq() not implemented")]
    fn test_request_irq_default_panics() {
        let mcu = MockMcu;
        let _ = mcu.request_irq();
    }
}
