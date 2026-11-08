use super::{Mapper, memory::CartridgeMemory};
use crate::SystemClock;

/// NINA-001 (mapper 34, CHR larger than 8 KB): one 32 KB PRG bank over the
/// whole window, CHR in two 4 KB banks selected by registers at `$7FFE`
/// and `$7FFF`, PRG bank selected by `$7FFD` bit 0 and by
/// `$8000-$FFFF`.
pub struct Nina001 {
    selected_prg_bank: usize,
    prg_bank_count: usize,
    chr_bank0: u16,
    chr_bank1: u16,
}

impl Nina001 {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        memory.map_prg_32k(0);
        memory.map_chr_4k(0, 0);
        memory.map_chr_4k(1, 0);
        let prg_bank_count = memory.prg_bank_count_32k();
        Self {
            selected_prg_bank: 0,
            prg_bank_count,
            chr_bank0: 0,
            chr_bank1: 0,
        }
    }
}

impl Mapper for Nina001 {
    fn cpu_write(
        &mut self,
        address: u16,
        value: u8,
        _cycle: SystemClock,
        memory: &mut CartridgeMemory,
    ) {
        match address {
            0x6000..=0x7fff => {
                memory.write_prg(address, value);
                match address {
                    0x7ffd => {
                        self.selected_prg_bank = (value & 0x01) as usize;
                        memory.map_prg_32k(self.selected_prg_bank as u16);
                    }
                    0x7ffe => {
                        self.chr_bank0 = (value & 0x0f) as u16;
                        memory.map_chr_4k(0, self.chr_bank0);
                    }
                    0x7fff => {
                        self.chr_bank1 = (value & 0x0f) as u16;
                        memory.map_chr_4k(1, self.chr_bank1);
                    }
                    _ => {}
                }
            }
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

    fn make_chr() -> Vec<u8> {
        let mut data = vec![0u8; 0x4000];
        data[0x0000] = 0xa1;
        data[0x1000] = 0xb1;
        data[0x2000] = 0xa2;
        data[0x3000] = 0xb2;
        // Only the first two 4 KB banks are reachable through the two
        // registers in this test.
        data
    }

    #[test]
    fn chr_banking() {
        let mut memory = CartridgeMemory::test(&[0; PRG_BANK_SIZE], &make_chr());
        let mut mapper = Nina001::new(&mut memory);

        mapper.cpu_write(0x7ffe, 0x00, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x7fff, 0x01, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0x0000), 0xa1);
        assert_eq!(memory.read_vram(0x1000), 0xb1);

        // 4 KB granularity: bank 1 is the second 4 KB half of the image.
        mapper.cpu_write(0x7ffe, 0x01, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x7fff, 0x00, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0x0000), 0xb1);
        assert_eq!(memory.read_vram(0x1000), 0xa1);
    }

    #[test]
    fn prg_bank_register_selects_32k_bank() {
        let mut prg = vec![0u8; PRG_BANK_SIZE * 2];
        prg[0] = 0x11;
        prg[PRG_BANK_SIZE] = 0x22;

        let mut memory = CartridgeMemory::test(&prg, &make_chr());
        let mut mapper = Nina001::new(&mut memory);

        assert_eq!(memory.read_prg(0x8000), 0x11);
        mapper.cpu_write(0x8000, 0x01, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0x22);
        mapper.cpu_write(0x7ffd, 0x00, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0x11);
    }
}
