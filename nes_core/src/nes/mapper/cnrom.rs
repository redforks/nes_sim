use super::{Mapper, MapperState, SnapshotError, memory::CartridgeMemory};
use crate::SystemClock;

/// CNROM: 32 KB PRG fixed (16 KB images mirror), CHR banked in 8 KB units.
pub struct CnRom {
    chr_bank_count: usize,
}

impl CnRom {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        memory.map_prg_16k(0, 0);
        let upper = if memory.prg_bank_count_16k() == 1 {
            0
        } else {
            1
        };
        memory.map_prg_16k(1, upper);
        memory.map_chr_8k(0);
        Self {
            chr_bank_count: memory.chr_bank_count_8k(),
        }
    }
}

impl Mapper for CnRom {
    fn snapshot(&self) -> Result<MapperState, SnapshotError> {
        Ok(MapperState::CnRom(CnRomState))
    }
    fn restore(&mut self, state: &MapperState) -> Result<(), SnapshotError> {
        match state {
            MapperState::CnRom(_) => Ok(()),
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
                let bank = (value as usize) % self.chr_bank_count;
                memory.map_chr_8k(bank as u16);
            }
            _ => {}
        }
    }
}

/// Saved mutable state for [`CnRom`] (ticket #187). The chip keeps no
/// mutable state of its own: the selected CHR bank lives in the shared
/// `CartridgeMemory` window capture (ticket #186), so the tag check
/// carries the mapper save.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct CnRomState;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_and_writes_cartridge_ram() {
        let mut memory = CartridgeMemory::test(&[], &[]);
        let mut mapper = CnRom::new(&mut memory);

        mapper.cpu_write(0x6000, 0x12, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x7fff, 0x34, SystemClock::default(), &mut memory);

        assert_eq!(memory.read_prg_ram(0x6000), 0x12);
        assert_eq!(memory.read_prg_ram(0x7fff), 0x34);
    }

    #[test]
    fn mirrors_16k_prg_into_upper_bank() {
        let mut prg = [0u8; 0x4000];
        prg[0x0000] = 0x11;
        prg[0x3fff] = 0x22;

        let mut memory = CartridgeMemory::test(&prg, &[]);
        CnRom::new(&mut memory);

        assert_eq!(memory.read_prg(0x8000), 0x11);
        assert_eq!(memory.read_prg(0xbfff), 0x22);
        assert_eq!(memory.read_prg(0xc000), 0x11);
        assert_eq!(memory.read_prg(0xffff), 0x22);
    }

    /// Ticket #187: snapshot/restore round-trip. The mapper keeps no
    /// mutable state of its own (the CHR window lives in
    /// `CartridgeMemory`), so the tag check plus the shared memory
    /// capture carry the whole save.
    #[test]
    fn snapshot_round_trip_preserves_bus_output() {
        let mut memory = CartridgeMemory::test(&[], &make_chr());
        let mut mapper = CnRom::new(&mut memory);
        mapper.cpu_write(0x8000, 0x01, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x6002, 0x55, SystemClock::default(), &mut memory);
        memory.write_vram(0x2405, 0x6d);

        let mapper_state = mapper.snapshot().expect("CnRom snapshot");
        let mem_state = memory.capture_snapshot();

        let mut memory2 = CartridgeMemory::test(&[], &make_chr());
        let mut mapper2 = CnRom::new(&mut memory2);
        mapper2.cpu_write(0x8000, 0x00, SystemClock::default(), &mut memory2);
        mapper2.restore(&mapper_state).expect("CnRom restore");
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
        let mut memory = CartridgeMemory::test(&[], &[]);
        let mut mapper = CnRom::new(&mut memory);
        let foreign = MapperState::NRom(crate::nes::mapper::NRomState);
        assert_eq!(mapper.restore(&foreign), Err(SnapshotError::Mismatch));
    }

    fn make_chr() -> Vec<u8> {
        let mut data = vec![0u8; 0x4000];
        data[0] = 0x10;
        data[0x2000] = 0x20;
        data
    }

    #[test]
    fn chr_initial_state_reads_bank_0() {
        let mut memory = CartridgeMemory::test(&[], &make_chr());
        CnRom::new(&mut memory);
        assert_eq!(memory.read_vram(0), 0x10);
    }

    #[test]
    fn chr_write_register_switches_bank() {
        let mut memory = CartridgeMemory::test(&[], &make_chr());
        let mut mapper = CnRom::new(&mut memory);
        mapper.cpu_write(0x8000, 1, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0), 0x20);
    }

    #[test]
    fn chr_bank_wraps_at_bank_count() {
        let mut memory = CartridgeMemory::test(&[], &make_chr());
        let mut mapper = CnRom::new(&mut memory);
        mapper.cpu_write(0x8000, 2, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0), 0x10);
    }

    #[test]
    fn chr_ram_writes_and_reads_through_the_bank() {
        let mut memory = CartridgeMemory::test(&[], &[]);
        CnRom::new(&mut memory);
        memory.write_vram(0x100, 0xab);
        assert_eq!(memory.read_vram(0x100), 0xab);
    }

    #[test]
    fn chr_ram_writes_survive_a_bank_switch() {
        // The old window cache copied from the ROM image on every switch and
        // lost writes; the bank itself is now the storage.
        let mut memory = CartridgeMemory::test(&[], &[]);
        let mut mapper = CnRom::new(&mut memory);
        memory.write_vram(0x100, 0xab);
        mapper.cpu_write(0x8000, 3, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x8000, 0, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0x100), 0xab);
    }

    #[test]
    fn chr_rom_ignores_writes() {
        let mut memory = CartridgeMemory::test(&[], &make_chr());
        CnRom::new(&mut memory);
        memory.write_vram(0x100, 0xab);
        assert_eq!(memory.read_vram(0x100), 0x00);
    }
}
