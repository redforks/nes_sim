//! MMC5 (ExROM, mapper 5) skeleton for ticket #76.
//!
//! Mode-3 PRG banking with power-on vectors visible, so MMC5 ROMs construct
//! and run frames without panic. The full PRG subsystem (modes 0-2, WRAM
//! windows, write-protect sequence) arrives in ticket #77; CHR, ExRAM,
//! nametables, IRQ, split, multiplier and audio arrive in later tickets.

use super::{Mapper, memory::CartridgeMemory};
use crate::SystemClock;

/// PRG bank number bits in `$5114-$5117`: 8 KB banks, up to 128 (1 MB).
const BANK_MASK: u8 = 0x7f;
/// Power-on PRG bank for `$E000-$FFFF`: bank `$7F` wraps to the last 8 KB
/// bank of any power-of-two ROM up to 1 MB, so the CPU vectors at
/// `$FFFA-$FFFF` are visible with no register setup (map #70).
const POWER_ON_E000_BANK: u8 = 0xff;

/// MMC5 (ExROM, mapper 5) skeleton: mode-3 PRG banking only.
///
/// Power-on state is PRG mode 3 with `$5117 = $FF` (map #70). `$5114-$5116`
/// bit 7 is the ROM/WRAM select (`1` = ROM); the skeleton maps ROM for every
/// window and WRAM windows arrive with the full PRG subsystem (#77).
pub struct MMC5 {
    /// Last values written to `$5114-$5117`: one 8 KB bank per mode-3 window
    /// at `$8000/$A000/$C000/$E000`.
    prg_banks: [u8; 4],
    prg_bank_count_8k: usize,
}

impl MMC5 {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        let mapper = Self {
            prg_banks: [0, 0, 0, POWER_ON_E000_BANK],
            prg_bank_count_8k: memory.prg_bank_count_8k(),
        };
        mapper.sync_prg_banks(memory);
        mapper
    }

    fn normalize_bank(&self, value: u8) -> u16 {
        ((value & BANK_MASK) as usize % self.prg_bank_count_8k) as u16
    }

    /// Mode 3: four independent 8 KB windows at `$8000/$A000/$C000/$E000`.
    fn sync_prg_banks(&self, memory: &mut CartridgeMemory) {
        for (slot, &value) in self.prg_banks.iter().enumerate() {
            memory.map_prg_8k(slot as u16, self.normalize_bank(value));
        }
    }
}

impl Mapper for MMC5 {
    fn cpu_write(
        &mut self,
        address: u16,
        value: u8,
        _cycle: SystemClock,
        memory: &mut CartridgeMemory,
    ) {
        match address {
            0x6000..=0x7fff => memory.write_prg_ram(address, value),
            0x5114..=0x5117 => {
                self.prg_banks[(address - 0x5114) as usize] = value;
                self.sync_prg_banks(memory);
            }
            // Skeleton: every other MMC5 register (`$5100` PRG mode, `$5101`
            // CHR mode, `$5104` ExRAM mode, `$5105` nametables, `$5120-$512B`
            // CHR banks, `$5130` CHR high, `$5200-$5206` split/IRQ/multiplier,
            // `$5010/$5015` audio, `$5C00-$5FFF` ExRAM) is accepted and
            // ignored; later tickets implement them.
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nes::mapper::Mirroring;

    const PRG_8K: usize = 0x2000;

    /// 8 KB PRG banks with the bank index as the first byte of each bank,
    /// plus `vectors` as the last 6 bytes (NMI/reset/IRQ).
    fn board(banks_8k: usize, vectors: [u8; 6]) -> (MMC5, CartridgeMemory) {
        let mut prg = vec![0u8; PRG_8K * banks_8k];
        for (i, chunk) in prg.chunks_mut(PRG_8K).enumerate() {
            chunk[0] = i as u8;
        }
        let tail = prg.len() - vectors.len();
        prg[tail..].copy_from_slice(&vectors);
        let mut memory = CartridgeMemory::from_rom(&prg, &[], None, Mirroring::Horizontal);
        let mapper = MMC5::new(&mut memory);
        (mapper, memory)
    }

    // Power-on is mode 3 with `$5117 = $FF`: the last 8 KB bank covers
    // `$E000-$FFFF`, so the CPU vectors at `$FFFA-$FFFF` read correctly with
    // no register setup (spec #74 user story 4, map #70).
    #[test]
    fn power_on_shows_vectors_from_last_bank() {
        let vectors = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66];
        let (_, memory) = board(2, vectors);

        assert_eq!(memory.read_prg(0xfffa), 0x11);
        assert_eq!(memory.read_prg(0xfffb), 0x22);
        assert_eq!(memory.read_prg(0xfffc), 0x33);
        assert_eq!(memory.read_prg(0xfffd), 0x44);
        assert_eq!(memory.read_prg(0xfffe), 0x55);
        assert_eq!(memory.read_prg(0xffff), 0x66);
    }

    #[test]
    fn power_on_maps_bank_0_at_8000_and_last_bank_at_e000() {
        let (_, memory) = board(2, [0; 6]);

        assert_eq!(memory.read_prg(0x8000), 0);
        assert_eq!(memory.read_prg(0xa000), 0);
        assert_eq!(memory.read_prg(0xc000), 0);
        assert_eq!(memory.read_prg(0xe000), 1);
    }

    // Mode 3 (spec #74 user story 2): `$5114-$5117` select one 8 KB ROM bank
    // each for `$8000/$A000/$C000/$E000`. Bit 7 is the ROM select
    // (tetanes `m005_exrom.rs` `ROM_SELECT_MASK`); `$5117` always selects ROM.
    #[test]
    fn mode_3_registers_select_each_8k_window() {
        let (mut mapper, mut memory) = board(4, [0; 6]);

        mapper.cpu_write(0x5114, 0x80 | 3, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5115, 0x80 | 2, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5116, 0x80 | 1, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5117, 0x80, SystemClock::default(), &mut memory);

        assert_eq!(memory.read_prg(0x8000), 3);
        assert_eq!(memory.read_prg(0xa000), 2);
        assert_eq!(memory.read_prg(0xc000), 1);
        assert_eq!(memory.read_prg(0xe000), 0);
    }

    #[test]
    fn bank_number_wraps_past_end_of_rom() {
        let (mut mapper, mut memory) = board(4, [0; 6]);

        // `$7F` is the power-on `$5117` value: 127 wraps to the last of 4 banks.
        mapper.cpu_write(0x5117, 0xff, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0xe000), 3);

        mapper.cpu_write(0x5114, 0x80 | 6, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 2);
    }

    // The boot code of the local MMC5 test ROMs writes CHR, nametable, IRQ
    // and audio registers (`$5101/$5104/$5105/$5127/$5200/$5204/$5015`):
    // the skeleton accepts and ignores them, leaving PRG mapping intact.
    #[test]
    fn other_mmc5_registers_leave_prg_mapping_intact() {
        let (mut mapper, mut memory) = board(4, [0; 6]);
        let before = [
            memory.read_prg(0x8000),
            memory.read_prg(0xa000),
            memory.read_prg(0xc000),
            memory.read_prg(0xe000),
        ];

        for address in [
            0x5100, 0x5101, 0x5104, 0x5105, 0x5120, 0x5127, 0x512b, 0x5130, 0x5200, 0x5204, 0x5010,
            0x5015, 0x8000, 0xc000,
        ] {
            mapper.cpu_write(address, 0x03, SystemClock::default(), &mut memory);
        }

        assert_eq!(
            [
                memory.read_prg(0x8000),
                memory.read_prg(0xa000),
                memory.read_prg(0xc000),
                memory.read_prg(0xe000),
            ],
            before
        );
    }

    #[test]
    fn prg_ram_window_round_trips() {
        let (mut mapper, mut memory) = board(2, [0; 6]);

        mapper.cpu_write(0x6000, 0x5a, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg_ram(0x6000), 0x5a);
    }
}
