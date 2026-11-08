use super::{Mapper, memory::CartridgeMemory};
use crate::SystemClock;

/// NROM: no banking at all. Both 16 KB PRG slots are fixed, 16 KB images
/// mirror into the upper half, CHR is one flat 8 KB window.
pub struct NRom;

impl NRom {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        memory.map_prg_16k(0, 0);
        let upper = if memory.prg_bank_count_16k() == 1 {
            0
        } else {
            1
        };
        memory.map_prg_16k(1, upper);
        Self
    }
}

impl Mapper for NRom {
    fn cpu_write(
        &mut self,
        address: u16,
        value: u8,
        _cycle: SystemClock,
        memory: &mut CartridgeMemory,
    ) {
        if (0x6000..=0x7fff).contains(&address) {
            memory.write_prg_ram(address, value);
        }
    }
}

impl Default for NRom {
    fn default() -> Self {
        Self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nes::mapper::CartridgeMemory;

    #[test]
    fn reads_and_writes_cartridge_ram() {
        let mut memory = CartridgeMemory::test(&[], &[]);
        let mut mapper = NRom::new(&mut memory);

        mapper.cpu_write(0x6000, 0x01, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg_ram(0x6000), 0x01);
        mapper.cpu_write(0x7fff, 0x03, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg_ram(0x7fff), 0x03);

        // ROM is read-only: mapper registers ignore the write.
        let rom_before = memory.read_prg(0x8000);
        mapper.cpu_write(0x8000, 0x03, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), rom_before);
    }

    #[test]
    fn mirrors_16k_prg_into_upper_bank() {
        let mut prg = [0u8; 0x4000];
        prg[0x3ffc] = 0x00;
        prg[0x3ffd] = 0x80;

        let mut memory = CartridgeMemory::test(&prg, &[]);
        NRom::new(&mut memory);

        assert_eq!(memory.read_prg(0xfffc), 0x00);
        assert_eq!(memory.read_prg(0xfffd), 0x80);
    }

    #[test]
    fn reads_32k_prg_without_mirroring() {
        let mut prg = [0u8; 0x8000];
        prg[0] = 0x11;
        prg[0x7fff] = 0x22;

        let mut memory = CartridgeMemory::test(&prg, &[]);
        NRom::new(&mut memory);

        assert_eq!(memory.read_prg(0x8000), 0x11);
        assert_eq!(memory.read_prg(0xffff), 0x22);
    }
}
