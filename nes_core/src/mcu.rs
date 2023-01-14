mod mapping;
mod mirror;
mod ram;

pub use mapping::*;
pub use mirror::*;
pub use ram::RamMcu;

/// Nes 6502 Mcu.
///
/// Note: addr is absolute address, not the offset from start of the memory region.
/// Make it easy to implement memory mapped devices..
pub trait Mcu {
    fn read(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, value: u8);

    fn read_word(&self, addr: u16) -> u16 {
        let low = self.read(addr) as u16;
        let high = self.read(addr.wrapping_add(1)) as u16;
        (high << 8) | low
    }

    fn write_word(&mut self, addr: u16, value: u16) {
        let low = value as u8;
        let high = (value >> 8) as u8;
        self.write(addr, low);
        self.write(addr.wrapping_add(1), high);
    }

    fn read_zero_page_word(&self, addr: u8) -> u16 {
        self.read_word(addr as u16)
    }
}