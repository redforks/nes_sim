use crate::SystemClock;

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
    /// Bus write arriving at `clock`, the dot it was issued on. Cartridges
    /// timestamp writes against the dot (MMC1's consecutive-cycle filter) —
    /// see GLOSSARY "Per-dot contract".
    fn write(&mut self, address: u16, value: u8, clock: SystemClock);

    /// Fast-path: read from zero page ($0000-$00FF).
    /// `address` is the direct zero-page offset ($00..=$FF).
    /// Default falls back to read(), but implementations can bypass address-range dispatch.
    fn read_zero_page(&mut self, address: u8) -> u8 {
        self.read(address as u16)
    }

    /// Fast-path: read from stack page ($0100-$01FF).
    /// `address` is the offset from $0100 ($00..=$FF).
    /// Default falls back to read(), but implementations can bypass address-range dispatch.
    fn read_stack_page(&mut self, address: u8) -> u8 {
        self.read(0x100 + address as u16)
    }

    /// Fast-path: write to zero page ($0000-$00FF).
    /// `address` is the direct zero-page offset ($00..=$FF).
    /// Default falls back to write(), but implementations can bypass address-range dispatch.
    fn write_zero_page(&mut self, address: u8, value: u8, clock: SystemClock) {
        self.write(address as u16, value, clock)
    }

    /// Fast-path: write to stack page ($0100-$01FF).
    /// `address` is the offset from $0100 ($00..=$FF).
    /// Default falls back to write(), but implementations can bypass address-range dispatch.
    fn write_stack_page(&mut self, address: u8, value: u8, clock: SystemClock) {
        self.write(0x100 + address as u16, value, clock)
    }

    /// Read-only observation of the VBL bit a skipped `$2002` read would
    /// return. Default `false`: only the NES memory map has a PPU status
    /// register. This is a flag query, not a bus read; the CPU uses it to
    /// resume a $2002 wait loop at the dot where its next real read would
    /// observe VBL (ticket #135).
    fn status_register_has_vbl(&self) -> bool {
        false
    }

    /// Replay the destructive side effects of a `$2002` read that a frozen
    /// CPU spin skipped. The default is a no-op: only the NES memory map
    /// has a PPU status register. Implementations must not drive the CPU
    /// bus, return a value, or change any other device state. The CPU calls
    /// this on its own at the spin's back-edge cadence (ticket #135).
    fn replay_status_read_side_effects(&mut self, _address: u16) {}
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

        fn write(&mut self, _address: u16, value: u8, _clock: SystemClock) {
            *self.data.borrow_mut() = value;
        }
    }

    #[test]
    fn test_read_write() {
        let mut mcu = MockMcu::new();
        mcu.write(0x0000, 0x42, SystemClock::default());
        assert_eq!(mcu.read(0x0000), 0x42);
    }
}
