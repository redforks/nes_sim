use super::{Mapper, MapperState, SnapshotError, memory::CartridgeMemory};
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
    fn snapshot(&self) -> Result<MapperState, SnapshotError> {
        Ok(MapperState::Nina001(Nina001State {
            selected_prg_bank: self.selected_prg_bank as u8,
            chr_bank0: self.chr_bank0 as u8,
            chr_bank1: self.chr_bank1 as u8,
        }))
    }
    fn restore(&mut self, state: &MapperState) -> Result<(), SnapshotError> {
        match state {
            MapperState::Nina001(saved) => {
                self.selected_prg_bank = saved.selected_prg_bank as usize;
                self.chr_bank0 = saved.chr_bank0 as u16;
                self.chr_bank1 = saved.chr_bank1 as u16;
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
            0x6000..=0x7fff => {
                memory.write_prg_ram(address, value);
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

/// Saved mutable state for [`Nina001`] (ticket #187). PRG and both CHR
/// bank selections; `prg_bank_count` is config rebuilt from the ROM.
/// Windows, RAM, and mirroring ride in the shared `CartridgeMemory`
/// capture (ticket #186).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Nina001State {
    pub selected_prg_bank: u8,
    pub chr_bank0: u8,
    pub chr_bank1: u8,
}

#[cfg(test)]
mod tests {
    use super::*;

    const PRG_BANK_SIZE: usize = 0x8000;

    /// Ticket #187: snapshot/restore round-trip. PRG and both CHR banks
    /// are mapper state (`prg_bank_count` is config); windows, RAM, and
    /// mirroring ride in the shared `CartridgeMemory` capture.
    #[test]
    fn snapshot_round_trip_preserves_bus_output() {
        let mut prg = vec![0u8; PRG_BANK_SIZE * 2];
        for (i, b) in prg.iter_mut().enumerate() {
            *b = ((i >> 15) as u8) ^ (i as u8);
        }
        let mut memory = CartridgeMemory::test(&prg, &make_chr());
        let mut mapper = Nina001::new(&mut memory);
        mapper.cpu_write(0x7ffe, 0x01, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x7fff, 0x02, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x8000, 0x01, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x6000, 0x5b, SystemClock::default(), &mut memory);
        memory.write_vram(0x2c05, 0xe4);

        let mapper_state = mapper.snapshot().expect("Nina001 snapshot");
        let mem_state = memory.capture_snapshot();

        let mut memory2 = CartridgeMemory::test(&prg, &make_chr());
        let mut mapper2 = Nina001::new(&mut memory2);
        mapper2.cpu_write(0x7ffe, 0x03, SystemClock::default(), &mut memory2);
        mapper2.cpu_write(0x8000, 0x00, SystemClock::default(), &mut memory2);
        mapper2.restore(&mapper_state).expect("Nina001 restore");
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

    /// Config is never saved: the same selection on different-sized PRG
    /// images produces identical snapshots.
    #[test]
    fn snapshot_omits_bank_count_config() {
        let prg1 = vec![0u8; PRG_BANK_SIZE];
        let prg2 = vec![0u8; PRG_BANK_SIZE * 2];
        let mut m1 = CartridgeMemory::test(&prg1, &make_chr());
        let mut mapper1 = Nina001::new(&mut m1);
        mapper1.cpu_write(0x7ffe, 0x01, SystemClock::default(), &mut m1);
        let mut m2 = CartridgeMemory::test(&prg2, &make_chr());
        let mut mapper2 = Nina001::new(&mut m2);
        mapper2.cpu_write(0x7ffe, 0x01, SystemClock::default(), &mut m2);
        assert_eq!(
            mapper1.snapshot().expect("snapshot"),
            mapper2.snapshot().expect("snapshot")
        );
    }

    #[test]
    fn restore_rejects_foreign_mapper_tag() {
        let mut memory = CartridgeMemory::test(&[], &make_chr());
        let mut mapper = Nina001::new(&mut memory);
        let foreign = MapperState::J87(crate::nes::mapper::J87State);
        assert_eq!(mapper.restore(&foreign), Err(SnapshotError::Mismatch));
    }

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
