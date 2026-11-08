use super::{Mapper, memory::CartridgeMemory};
use crate::SystemClock;

/// BxROM (mapper 34, BNROM): one 32 KB PRG bank over the whole window,
/// flat 8 KB CHR.
pub struct BxRom {
    selected_prg_bank: usize,
    prg_bank_count: usize,
}

impl BxRom {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        let prg_bank_count = memory.prg_bank_count_32k();
        let mut mapper = Self {
            selected_prg_bank: 0,
            prg_bank_count,
        };
        memory.map_prg_32k(0);
        memory.map_chr_8k(0);
        mapper.selected_prg_bank = 0;
        mapper
    }
}

impl Mapper for BxRom {
    fn cpu_write(
        &mut self,
        address: u16,
        value: u8,
        _cycle: SystemClock,
        memory: &mut CartridgeMemory,
    ) {
        match address {
            0x6000..=0x7fff => memory.write_prg_ram(address, value),
            0x8000..=0xffff => {
                self.selected_prg_bank = value as usize % self.prg_bank_count;
                memory.map_prg_32k(self.selected_prg_bank as u16);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const PRG_BANK_SIZE: usize = 0x8000;

    #[test]
    fn switches_32k_prg_banks() {
        let mut prg = [0u8; PRG_BANK_SIZE * 4];
        prg[0] = 0x10;
        prg[PRG_BANK_SIZE] = 0x20;
        prg[PRG_BANK_SIZE * 2] = 0x30;
        prg[PRG_BANK_SIZE * 3] = 0x40;

        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = BxRom::new(&mut memory);

        assert_eq!(memory.read_prg(0x8000), 0x10);

        mapper.cpu_write(0x8000, 0x02, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0x30);

        mapper.cpu_write(0xffff, 0x03, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0x40);
    }

    #[test]
    fn chr_flat_access() {
        let mut memory = CartridgeMemory::test(&[], &[]);
        BxRom::new(&mut memory);
        memory.write_vram(0x0000, 0x12);
        memory.write_vram(0x1fff, 0x34);
        assert_eq!(memory.read_vram(0), 0x12);
        assert_eq!(memory.read_vram(0x1fff), 0x34);
    }
}
