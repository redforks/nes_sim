use super::{Mapper, MapperState, SnapshotError, memory::CartridgeMemory};
use crate::SystemClock;

/// NROM: no banking at all. Both 16 KB PRG slots are fixed, 16 KB images
/// mirror into the upper half, CHR is one flat 8 KB window.
pub struct NRom;

impl NRom {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        memory.map_prg_16k(0, 0);
        let upper = if memory.prg_bank_count_16k() == 1 {
            0
        } else {
            1
        };
        memory.map_prg_16k(1, upper);
        Self
    }
}

impl Mapper for NRom {
    fn snapshot(&self) -> Result<MapperState, SnapshotError> {
        Ok(MapperState::NRom(NRomState))
    }
    fn restore(&mut self, state: &MapperState) -> Result<(), SnapshotError> {
        match state {
            MapperState::NRom(_) => Ok(()),
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
        if (0x6000..=0x7fff).contains(&address) {
            memory.write_prg_ram(address, value);
        }
    }
}

impl Default for NRom {
    fn default() -> Self {
        Self
    }
}

/// Saved mutable state for [`NRom`] (ticket #187). NROM holds no mutable
/// state, so the tag check carries the mapper save; PRG-RAM, nametables,
/// and windows ride in the shared `CartridgeMemory` capture (#186).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct NRomState;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nes::mapper::CartridgeMemory;

    #[test]
    fn reads_and_writes_cartridge_ram() {
        let mut memory = CartridgeMemory::test(&[], &[]);
        let mut mapper = NRom::new(&mut memory);

        mapper.cpu_write(0x6000, 0x01, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg_ram(0x6000), 0x01);
        mapper.cpu_write(0x7fff, 0x03, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg_ram(0x7fff), 0x03);

        // ROM is read-only: mapper registers ignore the write.
        let rom_before = memory.read_prg(0x8000);
        mapper.cpu_write(0x8000, 0x03, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), rom_before);
    }

    #[test]
    fn mirrors_16k_prg_into_upper_bank() {
        let mut prg = [0u8; 0x4000];
        prg[0x3ffc] = 0x00;
        prg[0x3ffd] = 0x80;

        let mut memory = CartridgeMemory::test(&prg, &[]);
        NRom::new(&mut memory);

        assert_eq!(memory.read_prg(0xfffc), 0x00);
        assert_eq!(memory.read_prg(0xfffd), 0x80);
    }

    /// Ticket #187: snapshot/restore round-trip. The mapper holds no
    /// mutable state, so the tag check plus the shared `CartridgeMemory`
    /// capture carry the whole save; bus output must match afterwards.
    #[test]
    fn snapshot_round_trip_preserves_bus_output() {
        let mut prg = vec![0u8; 0x8000];
        for (i, b) in prg.iter_mut().enumerate() {
            *b = (i >> 8) as u8;
        }
        let chr: Vec<u8> = (0..0x2000).map(|i| (i & 0xff) as u8).collect();
        let mut memory = CartridgeMemory::test(&prg, &chr);
        let mut mapper = NRom::new(&mut memory);
        mapper.cpu_write(0x6000, 0x11, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x7fff, 0x22, SystemClock::default(), &mut memory);
        memory.write_vram(0x2005, 0x5a);
        memory.write_vram(0x2405, 0xa5);

        let mapper_state = mapper.snapshot().expect("NRom snapshot");
        let mem_state = memory.capture_snapshot();

        let mut memory2 = CartridgeMemory::test(&prg, &chr);
        let mut mapper2 = NRom::new(&mut memory2);
        memory2.write_vram(0x2005, 0x00);
        mapper2.restore(&mapper_state).expect("NRom restore");
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
        let mut mapper = NRom::new(&mut memory);
        let foreign = MapperState::UxRom(crate::nes::mapper::UxRomState::default());
        assert_eq!(mapper.restore(&foreign), Err(SnapshotError::Mismatch));
    }

    #[test]
    fn reads_32k_prg_without_mirroring() {
        let mut prg = [0u8; 0x8000];
        prg[0] = 0x11;
        prg[0x7fff] = 0x22;

        let mut memory = CartridgeMemory::test(&prg, &[]);
        NRom::new(&mut memory);

        assert_eq!(memory.read_prg(0x8000), 0x11);
        assert_eq!(memory.read_prg(0xffff), 0x22);
    }
}
