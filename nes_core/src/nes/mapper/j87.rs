use super::{Mapper, MapperState, SnapshotError, memory::CartridgeMemory};
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
    fn snapshot(&self) -> Result<MapperState, SnapshotError> {
        Ok(MapperState::J87(J87State))
    }
    fn restore(&mut self, state: &MapperState) -> Result<(), SnapshotError> {
        match state {
            MapperState::J87(_) => Ok(()),
            _ => Err(SnapshotError::Mismatch),
        }
    }
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

/// Saved mutable state for [`J87`] (ticket #187). The chip keeps no
/// mutable state of its own: the CHR band lives in the shared
/// `CartridgeMemory` window capture (ticket #186), so the tag check
/// carries the mapper save.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct J87State;

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

    /// Ticket #187: snapshot/restore round-trip. The mapper keeps no
    /// mutable state of its own (the CHR band lives in
    /// `CartridgeMemory`), so the tag check plus the shared memory
    /// capture carry the whole save.
    #[test]
    fn snapshot_round_trip_preserves_bus_output() {
        let mut chr = vec![0u8; 0x8000];
        for (i, b) in chr.iter_mut().enumerate() {
            *b = ((i >> 13) as u8) ^ (i as u8);
        }
        let prg = vec![0u8; 32 * 1024];
        let mut memory = CartridgeMemory::test(&prg, &chr);
        let mut mapper = J87::new(&mut memory);
        mapper.cpu_write(0x6000, 0x01, SystemClock::default(), &mut memory);
        memory.write_vram(0x2005, 0x8f);

        let mapper_state = mapper.snapshot().expect("J87 snapshot");
        let mem_state = memory.capture_snapshot();

        let mut memory2 = CartridgeMemory::test(&prg, &chr);
        let mut mapper2 = J87::new(&mut memory2);
        mapper2.cpu_write(0x6000, 0x03, SystemClock::default(), &mut memory2);
        mapper2.restore(&mapper_state).expect("J87 restore");
        memory2.restore_snapshot(&mem_state);

        assert_eq!(memory.mirroring(), memory2.mirroring());
        for addr in 0x6000u16..=0x7fff {
            assert_eq!(
                memory.read_prg_ram(addr),
                memory2.read_prg_ram(addr),
                "prg-ram {addr:#06x}"
            );
        }
        for addr in 0x8000u16..=0xffff {
            assert_eq!(
                memory.read_prg(addr),
                memory2.read_prg(addr),
                "prg {addr:#06x}"
            );
        }
        for addr in 0x0000u16..0x3000 {
            assert_eq!(
                memory.read_vram(addr),
                memory2.read_vram(addr),
                "vram {addr:#06x}"
            );
        }
    }

    #[test]
    fn restore_rejects_foreign_mapper_tag() {
        let prg = vec![0u8; 32 * 1024];
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = J87::new(&mut memory);
        let foreign = MapperState::NRom(crate::nes::mapper::NRomState);
        assert_eq!(mapper.restore(&foreign), Err(SnapshotError::Mismatch));
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
