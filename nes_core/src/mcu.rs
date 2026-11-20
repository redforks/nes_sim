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
    /// resume a $2002 wait loop as soon as its next real read would observe
    /// VBL (ticket #135). The replay cadence is applied to side effects
    /// after this check; the peek itself never clears the flag.
    fn status_register_has_vbl(&self) -> bool {
        false
    }

    /// Classification query: whether an address names a status register
    /// whose read has destructive device-side effects. Default `false`.
    /// The CPU uses this to decide whether a frozen poll must replay those
    /// effects; no bus read or device mutation happens in this query.
    /// It is a naming query like `status_register_has_vbl`, not a new
    /// bus/DMA channel state seam (ADR-0018): the CPU still owns the one
    /// freeze/wake path. Ticket #135 needed the replay hook; this hook only
    /// decides which polls use it.
    fn has_destructive_status_read(&self, _address: u16) -> bool {
        false
    }

    /// Classification query: whether a readable address names a device
    /// register whose read is itself an action (a controller shift, an APU
    /// interrupt acknowledge, a zapper sample). Default `false`. Such a
    /// poll may declare a spin: its device action is replayed at the loop
    /// cadence while frozen, and the real read after wake returns the value.
    /// Device-sourced writes never wake a spin by decision.
    fn is_device_register(&self, _address: u16) -> bool {
        false
    }

    /// Classification query: whether a readable address is a data port whose
    /// read mutates device state rather than returning status. A PPUDATA
    /// read refills the read buffer and advances v; no status-effect replay
    /// can stand in for it, so a poll of such a port must never declare a
    /// spin. This exclusion is permanent, independent of the device-register
    /// classification. Default `false`.
    fn is_action_data_port(&self, _address: u16) -> bool {
        false
    }

    /// Replay the device-side effects of a read that a frozen CPU spin
    /// skipped: PPU status flag clears and `$4015`-`$4017` device actions.
    /// The default is a no-op. Implementations must not drive the CPU bus,
    /// return a value, or change any other device state. The CPU calls this
    /// on its own at the spin's back-edge cadence (tickets #135, #150).
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
