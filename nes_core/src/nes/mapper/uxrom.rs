use super::{Mapper, MapperState, SnapshotError, memory::CartridgeMemory};
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
    fn snapshot(&self) -> Result<MapperState, SnapshotError> {
        Ok(MapperState::UxRom(UxRomState {
            selected_prg_bank: self.selected_prg_bank as u16,
        }))
    }
    fn restore(&mut self, state: &MapperState) -> Result<(), SnapshotError> {
        match state {
            MapperState::UxRom(saved) => {
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
                self.selected_prg_bank = (value as usize) % self.prg_bank_count;
                memory.map_prg_16k(0, self.selected_prg_bank as u16);
            }
            _ => {}
        }
    }
}

/// Saved mutable state for [`UxRom`] (ticket #187). Only the selected
/// 16 KB PRG bank; `prg_bank_count` is config rebuilt from the ROM.
/// Windows, RAM, and mirroring ride in the shared `CartridgeMemory`
/// capture (ticket #186).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct UxRomState {
    pub selected_prg_bank: u16,
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

        assert_eq!(memory.read_prg_ram(0x6000), 0x12);
        assert_eq!(memory.read_prg_ram(0x7fff), 0x34);
    }

    /// Ticket #187: snapshot/restore round-trip. Only the selected bank
    /// is mapper state (`prg_bank_count` is config); the shared
    /// `CartridgeMemory` capture carries windows, RAM, and mirroring.
    #[test]
    fn snapshot_round_trip_preserves_bus_output() {
        let mut prg = vec![0u8; PRG_BANK_SIZE * 4];
        for (i, b) in prg.iter_mut().enumerate() {
            *b = ((i >> 14) as u8) ^ (i as u8);
        }
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = UxRom::new(&mut memory);
        mapper.cpu_write(0x8000, 0x02, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x6001, 0x77, SystemClock::default(), &mut memory);
        memory.write_vram(0x2805, 0x3c);
        memory.write_vram(0x0100, 0x99);

        let mapper_state = mapper.snapshot().expect("UxRom snapshot");
        let mem_state = memory.capture_snapshot();

        let mut memory2 = CartridgeMemory::test(&prg, &[]);
        let mut mapper2 = UxRom::new(&mut memory2);
        mapper2.cpu_write(0x8000, 0x03, SystemClock::default(), &mut memory2);
        mapper2.restore(&mapper_state).expect("UxRom restore");
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
        let prg2 = vec![0u8; PRG_BANK_SIZE * 2];
        let prg4 = vec![0u8; PRG_BANK_SIZE * 4];
        let mut m1 = CartridgeMemory::test(&prg2, &[]);
        let mut mapper1 = UxRom::new(&mut m1);
        mapper1.cpu_write(0x8000, 0x01, SystemClock::default(), &mut m1);
        let mut m2 = CartridgeMemory::test(&prg4, &[]);
        let mut mapper2 = UxRom::new(&mut m2);
        mapper2.cpu_write(0x8000, 0x01, SystemClock::default(), &mut m2);
        assert_eq!(
            mapper1.snapshot().expect("snapshot"),
            mapper2.snapshot().expect("snapshot")
        );
    }

    #[test]
    fn restore_rejects_foreign_mapper_tag() {
        let (mut mapper, _) = mapper_for(&[0; PRG_BANK_SIZE * 2]);
        let foreign = MapperState::CnRom(crate::nes::mapper::CnRomState);
        assert_eq!(mapper.restore(&foreign), Err(SnapshotError::Mismatch));
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
