use super::{Mapper, Mirroring, memory::CartridgeMemory};
use crate::SystemClock;

/// AxROM: one 32 KB PRG bank over the whole window, flat 8 KB CHR, and the
/// single-screen mirroring bit written with every bank select.
pub struct AxRom {
    selected_prg_bank: usize,
    prg_bank_count: usize,
}

impl AxRom {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        let prg_bank_count = memory.prg_bank_count_32k();
        let mapper = Self {
            selected_prg_bank: 0,
            prg_bank_count,
        };
        mapper.install(memory);
        mapper
    }

    fn install(&self, memory: &mut CartridgeMemory) {
        memory.map_prg_32k(self.selected_prg_bank as u16);
    }
}

impl Mapper for AxRom {
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
                self.selected_prg_bank = (value & 0x0f) as usize % self.prg_bank_count;
                memory.map_prg_32k(self.selected_prg_bank as u16);
                let mirroring = if value & 0x10 == 0 {
                    Mirroring::LowerBank
                } else {
                    Mirroring::UpperBank
                };
                memory.apply_mirroring(mirroring);
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
        let mut mapper = AxRom::new(&mut memory);

        assert_eq!(memory.read_prg(0x8000), 0x10);

        mapper.cpu_write(0x8000, 0x02, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0x30);

        mapper.cpu_write(0xffff, 0x03, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0x40);
    }

    #[test]
    fn writes_single_screen_mirroring() {
        let mut memory = CartridgeMemory::test(&[], &[]);
        let mut mapper = AxRom::new(&mut memory);

        mapper.cpu_write(0x8000, 0x10, SystemClock::default(), &mut memory);
        memory.write_vram(0x2000, 0x5a);
        assert_eq!(memory.read_vram(0x2000), 0x5a);
        // Single-screen upper bank: every slot is the same physical bank.
        assert_eq!(memory.read_vram(0x2400), 0x5a);
        assert_eq!(memory.read_vram(0x2800), 0x5a);
        assert_eq!(memory.read_vram(0x2c00), 0x5a);
    }

    #[test]
    fn mirrors_16k_prg_across_32k() {
        // oamtest3/oam3.nes: mapper 7 header with a single 16K bank.
        let mut prg = [0u8; 0x4000];
        prg[0] = 0x42;
        prg[0x3ffc] = 0x00;
        prg[0x3ffd] = 0x80;

        let mut memory = CartridgeMemory::test(&prg, &[]);
        AxRom::new(&mut memory);

        assert_eq!(memory.read_prg(0x8000), 0x42);
        assert_eq!(memory.read_prg(0xc000), 0x42);
        assert_eq!(memory.read_prg(0xfffc), 0x00);
        assert_eq!(memory.read_prg(0xfffd), 0x80);
    }
}
