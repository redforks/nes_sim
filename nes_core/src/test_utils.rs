// Test utilities shared across all test modules

use crate::mcu::Mcu;
use std::cell::{Cell, RefCell};

/// A configurable mock MCU for testing
///
/// # Examples
///
/// ```
/// // Simple 64KB MCU
/// let mcu = MockMcu::new();
///
/// // With initial program data
/// let mcu = MockMcu::new().with_program(0x8000, &[0xA9, 0x42]);
///
/// // Regional MCU (for mapping tests)
/// let mcu = MockMcu::regional(0x2000, 0x20FF);
///
/// // With IRQ/PPU controls
/// let mcu = MockMcu::new()
///     .with_tick_ppu_result(true)
///     .with_irq_request(false);
/// ```
pub struct MockMcu {
    memory: RefCell<[u8; 0x10000]>,
    tick_ppu_result: Cell<bool>,
    irq_request: Cell<bool>,
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
            irq_request: Cell::new(false),
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
            irq_request: Cell::new(false),
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

    /// Sets the IRQ request state (builder pattern)
    pub fn with_irq_request(self, request: bool) -> Self {
        self.irq_request.set(request);
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
    fn read(&self, addr: u16) -> u8 {
        self.memory.borrow()[addr as usize]
    }

    fn write(&self, addr: u16, value: u8) {
        self.memory.borrow_mut()[addr as usize] = value;
    }
    fn request_irq(&self) -> bool {
        self.irq_request.get()
    }
}

impl MockMcu {
    /// Inherent method to emulate ticking the PPU. Kept as an inherent method
    /// so tests and callers that know the concrete type can still invoke it.
    pub fn tick_ppu(&self) -> bool {
        self.tick_ppu_result.get()
    }
}

/// Extension trait adding word read/write operations to any MCU
pub trait McuUtils: Mcu {
    /// Reads a 16-bit little-endian value from addr
    fn read_word_le(&self, addr: u16) -> u16 {
        let lo = self.read(addr) as u16;
        let hi = self.read(addr + 1) as u16;
        hi << 8 | lo
    }

    /// Writes a 16-bit little-endian value to addr
    fn write_word_le(&self, addr: u16, value: u16) {
        self.write(addr, (value & 0xFF) as u8);
        self.write(addr + 1, ((value >> 8) & 0xFF) as u8);
    }
}

// Blanket implementation for all Mcu types
impl<T: Mcu> McuUtils for T {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mock_mcu_basic() {
        let mcu = MockMcu::new();
        mcu.write(0x1000, 0x42);
        assert_eq!(mcu.read(0x1000), 0x42);
    }

    #[test]
    fn test_mock_mcu_with_program() {
        let mcu = MockMcu::new().with_program(0x8000, &[0xA9, 0x42, 0x00]);
        assert_eq!(mcu.read(0x8000), 0xA9);
        assert_eq!(mcu.read(0x8001), 0x42);
    }

    #[test]
    fn test_mock_mcu_write_word() {
        let mcu = MockMcu::new();
        mcu.write_word(0x2000, 0x1234);
        assert_eq!(mcu.read(0x2000), 0x34);
        assert_eq!(mcu.read(0x2001), 0x12);
    }

    #[test]
    fn test_mcu_utils_word_rw() {
        let mcu = MockMcu::new();
        mcu.write_word_le(0x3000, 0xABCD);
        assert_eq!(mcu.read_word_le(0x3000), 0xABCD);
    }

    #[test]
    fn test_mock_mcu_regional() {
        let mcu = MockMcu::regional(0x2000, 0x20FF);
        assert_eq!(mcu.region(), Some((0x2000, 0x20FF)));
        mcu.write(0x2050, 0xAB);
        assert_eq!(mcu.read(0x2050), 0xAB);
    }

    #[test]
    fn test_mock_mcu_flags() {
        let mcu = MockMcu::new()
            .with_tick_ppu_result(true)
            .with_irq_request(true);
        assert!(mcu.tick_ppu());
        assert!(mcu.request_irq());
    }
}
