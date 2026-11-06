// Test utilities shared across all test modules

use crate::SystemClock;
use crate::mcu::Mcu;
use std::cell::{Cell, RefCell};

/// A configurable mock MCU for testing
///
/// # Examples
///
/// ```
/// // Simple 64KB MCU
/// let mut mcu = MockMcu::new();
///
/// // With initial program data
/// let mut mcu = MockMcu::new().with_program(0x8000, &[0xA9, 0x42]);
///
/// // Regional MCU (for mapping tests)
/// let mcu = MockMcu::regional(0x2000, 0x20FF);
///
/// // With IRQ/PPU controls
/// let mut mcu = MockMcu::new()
///     .with_tick_ppu_result(true);
/// ```
pub struct MockMcu {
    memory: RefCell<[u8; 0x10000]>,
    tick_ppu_result: Cell<bool>,
    is_regional: Cell<bool>,
    region_start: Cell<u16>,
    region_end: Cell<u16>,
}

impl MockMcu {
    /// Creates a new full 64KB address space MCU
    pub fn new() -> Self {
        MockMcu {
            memory: RefCell::new([0; 0x10000]),
            tick_ppu_result: Cell::new(false),
            is_regional: Cell::new(false),
            region_start: Cell::new(0),
            region_end: Cell::new(0xFFFF),
        }
    }

    /// Creates a regional MCU that only responds to addresses in [start, end]
    pub fn regional(start: u16, end: u16) -> Self {
        MockMcu {
            memory: RefCell::new([0; 0x10000]),
            tick_ppu_result: Cell::new(false),
            is_regional: Cell::new(true),
            region_start: Cell::new(start),
            region_end: Cell::new(end),
        }
    }

    /// Sets program data at the specified address (builder pattern)
    pub fn with_program(self, addr: u16, program: &[u8]) -> Self {
        for (i, &byte) in program.iter().enumerate() {
            self.memory.borrow_mut()[(addr as usize) + i] = byte;
        }
        self
    }

    /// Sets the PPU tick result (builder pattern)
    pub fn with_tick_ppu_result(self, result: bool) -> Self {
        self.tick_ppu_result.set(result);
        self
    }

    /// Writes a 16-bit value in little-endian format
    pub fn write_word(&self, addr: u16, value: u16) {
        self.memory.borrow_mut()[addr as usize] = (value & 0xFF) as u8;
        self.memory.borrow_mut()[(addr + 1) as usize] = ((value >> 8) & 0xFF) as u8;
    }

    /// Returns the region bounds for regional MCUs
    pub fn region(&self) -> Option<(u16, u16)> {
        if self.is_regional.get() {
            Some((self.region_start.get(), self.region_end.get()))
        } else {
            None
        }
    }
}

impl Mcu for MockMcu {
    fn read(&mut self, addr: u16) -> u8 {
        self.memory.borrow()[addr as usize]
    }

    fn peek(&self, addr: u16) -> u8 {
        self.memory.borrow()[addr as usize]
    }

    fn write(&mut self, addr: u16, value: u8, _clock: SystemClock) {
        self.memory.borrow_mut()[addr as usize] = value;
    }
}

impl MockMcu {
    /// Inherent method to emulate ticking the PPU. Kept as an inherent method
    /// so tests and callers that know the concrete type can still invoke it.
    pub fn tick_ppu(&self) -> bool {
        self.tick_ppu_result.get()
    }
}

/// Which `Mcu` method class an access took: the generic range-dispatch
/// path (`read`/`write`) or one of the four zero-page/stack-page fast
/// paths.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GenericAccessKind {
    Read,
    Write,
}

/// One bus access observed by [`RecordingMcu`].
///
/// The generic and fast-path variants are distinct even when they address
/// the same byte: telling them apart is the whole point of the double, so
/// the fast-path methods never delegate to `read`/`write` here (that would
/// double-record the call and hide a CPU that used the generic path).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum McuCall {
    /// Generic `Mcu::read`.
    Read { addr: u16, value: u8 },
    /// Generic `Mcu::write`.
    Write { addr: u16, value: u8 },
    /// `Mcu::read_zero_page`; `addr` is the zero-page offset.
    ReadZeroPage { addr: u8, value: u8 },
    /// `Mcu::read_stack_page`; `addr` is the offset from $0100.
    ReadStackPage { addr: u8, value: u8 },
    /// `Mcu::write_zero_page`; `addr` is the zero-page offset.
    WriteZeroPage { addr: u8, value: u8 },
    /// `Mcu::write_stack_page`; `addr` is the offset from $0100.
    WriteStackPage { addr: u8, value: u8 },
}

impl McuCall {
    /// The access kind when it took the generic `read`/`write` path.
    pub fn generic(&self) -> Option<GenericAccessKind> {
        match self {
            McuCall::Read { .. } => Some(GenericAccessKind::Read),
            McuCall::Write { .. } => Some(GenericAccessKind::Write),
            _ => None,
        }
    }

    /// The absolute address the access touched, whatever the route.
    pub fn address(&self) -> u16 {
        match *self {
            McuCall::Read { addr, .. } | McuCall::Write { addr, .. } => addr,
            McuCall::ReadZeroPage { addr, .. } | McuCall::WriteZeroPage { addr, .. } => addr as u16,
            McuCall::ReadStackPage { addr, .. } | McuCall::WriteStackPage { addr, .. } => {
                0x100 + addr as u16
            }
        }
    }

    pub fn is_read_zero_page(&self) -> bool {
        matches!(self, McuCall::ReadZeroPage { .. })
    }

    pub fn is_write_zero_page(&self) -> bool {
        matches!(self, McuCall::WriteZeroPage { .. })
    }

    pub fn is_read_stack_page(&self) -> bool {
        matches!(self, McuCall::ReadStackPage { .. })
    }

    pub fn is_write_stack_page(&self) -> bool {
        matches!(self, McuCall::WriteStackPage { .. })
    }
}

/// An `Mcu` that records every access it receives, for tests that must
/// observe *which* method the CPU chose rather than only the data.
///
/// Unlike [`MockMcu`], the four fast-path methods are implemented on their
/// own instead of delegating to `read`/`write`: a call recorded as
/// `ReadZeroPage` proves the CPU took the fast path, and no generic call is
/// synthesised behind its back.
///
/// ```
/// let mcu = RecordingMcu::filled(0x10).with_program(0x8000, &[0xA9, 0x42]);
/// assert!(mcu.calls().is_empty());
/// ```
pub struct RecordingMcu {
    memory: RefCell<[u8; 0x10000]>,
    calls: RefCell<Vec<McuCall>>,
}

impl RecordingMcu {
    /// Creates a 64KB MCU whose every byte reads as `value`.
    ///
    /// A uniform filler makes the opcode walk deterministic: operand bytes
    /// and pointer bytes are the same value everywhere, so the address an
    /// instruction computes is a function of the filler alone.
    pub fn filled(value: u8) -> Self {
        RecordingMcu {
            memory: RefCell::new([value; 0x10000]),
            calls: RefCell::new(Vec::new()),
        }
    }

    /// Sets program data at the specified address (builder pattern).
    pub fn with_program(self, addr: u16, program: &[u8]) -> Self {
        for (i, &byte) in program.iter().enumerate() {
            self.memory.borrow_mut()[addr as usize + i] = byte;
        }
        self
    }

    /// Forgets every call recorded so far, keeping memory intact.
    pub fn clear_calls(&self) {
        self.calls.borrow_mut().clear();
    }

    /// Everything recorded so far, in call order.
    pub fn calls(&self) -> Vec<McuCall> {
        self.calls.borrow().clone()
    }
}

impl Mcu for RecordingMcu {
    fn read(&mut self, addr: u16) -> u8 {
        let value = self.memory.borrow()[addr as usize];
        self.calls.borrow_mut().push(McuCall::Read { addr, value });
        value
    }

    fn peek(&self, addr: u16) -> u8 {
        self.memory.borrow()[addr as usize]
    }

    fn write(&mut self, addr: u16, value: u8, _clock: SystemClock) {
        self.memory.borrow_mut()[addr as usize] = value;
        self.calls.borrow_mut().push(McuCall::Write { addr, value });
    }

    fn read_zero_page(&mut self, address: u8) -> u8 {
        let value = self.memory.borrow()[address as usize];
        self.calls.borrow_mut().push(McuCall::ReadZeroPage {
            addr: address,
            value,
        });
        value
    }

    fn read_stack_page(&mut self, address: u8) -> u8 {
        let value = self.memory.borrow()[0x100 + address as usize];
        self.calls.borrow_mut().push(McuCall::ReadStackPage {
            addr: address,
            value,
        });
        value
    }

    fn write_zero_page(&mut self, address: u8, value: u8, _clock: SystemClock) {
        self.memory.borrow_mut()[address as usize] = value;
        self.calls.borrow_mut().push(McuCall::WriteZeroPage {
            addr: address,
            value,
        });
    }

    fn write_stack_page(&mut self, address: u8, value: u8, _clock: SystemClock) {
        self.memory.borrow_mut()[0x100 + address as usize] = value;
        self.calls.borrow_mut().push(McuCall::WriteStackPage {
            addr: address,
            value,
        });
    }
}

/// Extension trait adding word read/write operations to any MCU
pub trait McuUtils: Mcu {
    /// Reads a 16-bit little-endian value from addr
    fn read_word_le(&mut self, addr: u16) -> u16 {
        let lo = self.read(addr) as u16;
        let hi = self.read(addr + 1) as u16;
        hi << 8 | lo
    }

    /// Writes a 16-bit little-endian value to addr
    fn write_word_le(&mut self, addr: u16, value: u16, clock: SystemClock) {
        self.write(addr, (value & 0xFF) as u8, clock);
        self.write(addr + 1, ((value >> 8) & 0xFF) as u8, clock);
    }
}

// Blanket implementation for all Mcu types
impl<T: Mcu> McuUtils for T {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mock_mcu_basic() {
        let mut mcu = MockMcu::new();
        mcu.write(0x1000, 0x42, SystemClock::default());
        assert_eq!(mcu.read(0x1000), 0x42);
    }

    #[test]
    fn test_mock_mcu_with_program() {
        let mut mcu = MockMcu::new().with_program(0x8000, &[0xA9, 0x42, 0x00]);
        assert_eq!(mcu.read(0x8000), 0xA9);
        assert_eq!(mcu.read(0x8001), 0x42);
    }

    #[test]
    fn test_mock_mcu_write_word() {
        let mut mcu = MockMcu::new();
        mcu.write_word(0x2000, 0x1234);
        assert_eq!(mcu.read(0x2000), 0x34);
        assert_eq!(mcu.read(0x2001), 0x12);
    }

    #[test]
    fn test_mcu_utils_word_rw() {
        let mut mcu = MockMcu::new();
        mcu.write_word_le(0x3000, 0xABCD, SystemClock::default());
        assert_eq!(mcu.read_word_le(0x3000), 0xABCD);
    }

    #[test]
    fn test_mock_mcu_regional() {
        let mut mcu = MockMcu::regional(0x2000, 0x20FF);
        assert_eq!(mcu.region(), Some((0x2000, 0x20FF)));
        mcu.write(0x2050, 0xAB, SystemClock::default());
        assert_eq!(mcu.read(0x2050), 0xAB);
    }

    #[test]
    fn test_mock_mcu_flags() {
        let mcu = MockMcu::new().with_tick_ppu_result(true);
        assert!(mcu.tick_ppu());
    }

    #[test]
    fn recording_mcu_distinguishes_routes() {
        let mut mcu = RecordingMcu::filled(0).with_program(0x8000, &[0x42]);

        assert_eq!(mcu.read(0x8000), 0x42);
        mcu.write(0x4000, 0x11, SystemClock::default());
        assert_eq!(mcu.read_zero_page(0x10), 0);
        mcu.write_zero_page(0x10, 0x22, SystemClock::default());
        assert_eq!(mcu.read_stack_page(0xF0), 0);
        mcu.write_stack_page(0xF0, 0x33, SystemClock::default());

        assert_eq!(
            mcu.calls(),
            vec![
                McuCall::Read {
                    addr: 0x8000,
                    value: 0x42
                },
                McuCall::Write {
                    addr: 0x4000,
                    value: 0x11
                },
                McuCall::ReadZeroPage {
                    addr: 0x10,
                    value: 0
                },
                McuCall::WriteZeroPage {
                    addr: 0x10,
                    value: 0x22
                },
                McuCall::ReadStackPage {
                    addr: 0xF0,
                    value: 0
                },
                McuCall::WriteStackPage {
                    addr: 0xF0,
                    value: 0x33
                },
            ]
        );

        // Memory is shared whichever route wrote it.
        assert_eq!(mcu.read_zero_page(0x10), 0x22);
        assert_eq!(mcu.read_stack_page(0xF0), 0x33);

        mcu.clear_calls();
        assert!(mcu.calls().is_empty());
    }
}
