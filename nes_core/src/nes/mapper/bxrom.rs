use super::{Mapper, MapperState, SnapshotError, memory::CartridgeMemory};
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
    fn snapshot(&self) -> Result<MapperState, SnapshotError> {
        Ok(MapperState::BxRom(BxRomState {
            selected_prg_bank: self.selected_prg_bank as u16,
        }))
    }
    fn restore(&mut self, state: &MapperState) -> Result<(), SnapshotError> {
        match state {
            MapperState::BxRom(saved) => {
                self.selected_prg_bank = saved.selected_prg_bank as usize;
                Ok(())
            }
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

/// Saved mutable state for [`BxRom`] (ticket #187). Only the selected
/// 32 KB PRG bank; `prg_bank_count` is config rebuilt from the ROM.
/// Windows, RAM, and mirroring ride in the shared `CartridgeMemory`
/// capture (ticket #186).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct BxRomState {
    pub selected_prg_bank: u16,
}

#[cfg(test)]
mod tests {
    use super::*;

    const PRG_BANK_SIZE: usize = 0x8000;

    /// Ticket #187: snapshot/restore round-trip. Only the selected bank
    /// is mapper state (`prg_bank_count` is config); windows, RAM, and
    /// mirroring ride in the shared `CartridgeMemory` capture.
    #[test]
    fn snapshot_round_trip_preserves_bus_output() {
        let mut prg = vec![0u8; PRG_BANK_SIZE * 4];
        for (i, b) in prg.iter_mut().enumerate() {
            *b = ((i >> 15) as u8) ^ (i as u8);
        }
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = BxRom::new(&mut memory);
        mapper.cpu_write(0x8000, 0x03, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x6004, 0x9a, SystemClock::default(), &mut memory);
        memory.write_vram(0x2005, 0xc1);
        memory.write_vram(0x0100, 0x3d);

        let mapper_state = mapper.snapshot().expect("BxRom snapshot");
        let mem_state = memory.capture_snapshot();

        let mut memory2 = CartridgeMemory::test(&prg, &[]);
        let mut mapper2 = BxRom::new(&mut memory2);
        mapper2.cpu_write(0x8000, 0x01, SystemClock::default(), &mut memory2);
        mapper2.restore(&mapper_state).expect("BxRom restore");
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

    /// Config is never saved: the same selection on different-sized ROMs
    /// produces identical snapshots.
    #[test]
    fn snapshot_omits_bank_count_config() {
        let prg1 = vec![0u8; PRG_BANK_SIZE];
        let prg4 = vec![0u8; PRG_BANK_SIZE * 4];
        let mut m1 = CartridgeMemory::test(&prg1, &[]);
        let mapper1 = BxRom::new(&mut m1);
        let mut m2 = CartridgeMemory::test(&prg4, &[]);
        let mut mapper2 = BxRom::new(&mut m2);
        mapper2.cpu_write(0x8000, 0x04, SystemClock::default(), &mut m2);
        assert_eq!(
            mapper1.snapshot().expect("snapshot"),
            mapper2.snapshot().expect("snapshot")
        );
    }

    #[test]
    fn restore_rejects_foreign_mapper_tag() {
        let mut memory = CartridgeMemory::test(&[], &[]);
        let mut mapper = BxRom::new(&mut memory);
        let foreign = MapperState::AxRom(crate::nes::mapper::AxRomState::default());
        assert_eq!(mapper.restore(&foreign), Err(SnapshotError::Mismatch));
    }

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
