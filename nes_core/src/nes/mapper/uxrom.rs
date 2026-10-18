use super::{Mapper, memory::CartridgeMemory};
use crate::SystemClock;

/// UxROM: a switchable 16 KB lower PRG bank, the last 16 KB bank fixed at
/// `$C000`, flat 8 KB CHR.
pub struct UxRom {
    selected_prg_bank: usize,
    prg_bank_count: usize,
}

impl UxRom {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        let prg_bank_count = memory.prg_bank_count_16k();
        let mapper = Self {
            selected_prg_bank: 0,
            prg_bank_count,
        };
        mapper.install(memory);
        mapper
    }

    fn install(&self, memory: &mut CartridgeMemory) {
        memory.map_prg_16k(0, self.selected_prg_bank as u16);
        memory.map_prg_16k(1, (self.prg_bank_count - 1) as u16);
    }
}

impl Mapper for UxRom {
    fn cpu_write(
        &mut self,
        address: u16,
        value: u8,
        _cycle: SystemClock,
        memory: &mut CartridgeMemory,
    ) {
        match address {
            0x6000..=0x7fff => memory.write_prg(address, value),
            0x8000..=0xffff => {
                self.selected_prg_bank = (value as usize) % self.prg_bank_count;
                memory.map_prg_16k(0, self.selected_prg_bank as u16);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const PRG_BANK_SIZE: usize = 0x4000;

    fn mapper_for(prg: &[u8]) -> (UxRom, CartridgeMemory) {
        let mut memory = CartridgeMemory::test(prg, &[]);
        let mapper = UxRom::new(&mut memory);
        (mapper, memory)
    }

    #[test]
    fn reads_and_writes_cartridge_ram() {
        let (mut mapper, mut memory) = mapper_for(&[0; PRG_BANK_SIZE * 2]);

        mapper.cpu_write(0x6000, 0x12, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x7fff, 0x34, SystemClock::default(), &mut memory);

        assert_eq!(memory.read_prg(0x6000), 0x12);
        assert_eq!(memory.read_prg(0x7fff), 0x34);
    }

    #[test]
    fn switches_lower_prg_bank_and_keeps_last_bank_fixed() {
        let mut prg = [0u8; PRG_BANK_SIZE * 4];
        prg[0] = 0x10;
        prg[PRG_BANK_SIZE] = 0x20;
        prg[PRG_BANK_SIZE * 2] = 0x30;
        prg[PRG_BANK_SIZE * 3] = 0x40;
        prg[(PRG_BANK_SIZE * 4) - 1] = 0x4f;

        let (mut mapper, mut memory) = mapper_for(&prg);

        assert_eq!(memory.read_prg(0x8000), 0x10);
        assert_eq!(memory.read_prg(0xc000), 0x40);
        assert_eq!(memory.read_prg(0xffff), 0x4f);

        mapper.cpu_write(0x8000, 0x02, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0x30);
        assert_eq!(memory.read_prg(0xbfff), prg[(PRG_BANK_SIZE * 3) - 1]);
        assert_eq!(memory.read_prg(0xc000), 0x40);
        assert_eq!(memory.read_prg(0xffff), 0x4f);
    }
}
