use super::{Mapper, memory::CartridgeMemory};
use crate::SystemClock;

/// J87 (mapper 87): one fixed 32 KB PRG image (16 KB images mirror) and CHR
/// switched in four 8 KB bands by any write in `$6000-$FFFF`.
pub struct J87;

impl J87 {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        debug_assert!(
            memory.prg_len_bytes() == 32 * 1024 || memory.prg_len_bytes() == 16 * 1024,
            "mapper 87 expects a 16 KB or 32 KB PRG image"
        );
        memory.map_prg_32k(0);
        memory.map_chr_8k(0);
        Self
    }
}

impl Mapper for J87 {
    fn cpu_write(
        &mut self,
        address: u16,
        value: u8,
        _cycle: SystemClock,
        memory: &mut CartridgeMemory,
    ) {
        if address >= 0x6000 {
            let band = ((value & 0x01) << 1) | ((value & 0x02) >> 1);
            memory.map_chr_8k(band as u16);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chr_band_switching() {
        let mut data = vec![0u8; 0x8000];
        data[0x0000] = 0x10;
        data[0x2000] = 0x20;
        data[0x4000] = 0x30;
        data[0x6000] = 0x40;

        let prg = vec![0u8; 32 * 1024];
        let mut memory = CartridgeMemory::test(&prg, &data);
        let mut mapper = J87::new(&mut memory);

        assert_eq!(memory.read_vram(0), 0x10);
        mapper.cpu_write(0x6000, 0x02, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0), 0x20);
        mapper.cpu_write(0x6000, 0x01, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0), 0x30);
        mapper.cpu_write(0x6000, 0x03, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0), 0x40);
    }

    #[test]
    fn mirrors_16k_prg_across_32k() {
        let mut prg = vec![0u8; 0x4000];
        prg[0] = 0x42;

        let mut memory = CartridgeMemory::test(&prg, &[]);
        J87::new(&mut memory);

        assert_eq!(memory.read_prg(0x8000), 0x42);
        assert_eq!(memory.read_prg(0xc000), 0x42);
    }
}
