//! MMC5 (ExROM, mapper 5): full PRG subsystem (ticket #77) plus CHR
//! banking (ticket #78).
//!
//! All four PRG banking modes (`$5100` modes 0-3) with per-size ignore bits
//! and ROM/RAM select (`$5113-$5117`), header-sized WRAM up to 128K mapped at
//! the low (`$6000`) and high (`$8000-$DFFF`) windows, header-driven battery
//! backing with the ETROM first-bank-only rule, and the exact two-step
//! `$5102=$02,$5103=$01` write-protect sequence gating both register and
//! PRG-RAM data writes.
//!
//! CHR banking covers all granularities (`$5101` modes 0-3: 8K/4K/2K/1K),
//! upper address bits (`$5130`), and both 8x8 and 8x16 sprite/background
//! table sets (`$5120-$5127` sprite, `$5128-$512B` background) with
//! last-written-set semantics for CPU `$2007` reads (wiki §3.5). The
//! redirect travels through the [`Mapper::ppu_chr_bank`] hook; the 1K
//! windows mirror the sprite set (re-synced on sprite writes and `$5101`)
//! as the write-routing fallback.
//!
//! Power-on state is PRG mode 3 with `$5117 = $FF`, so the last ROM bank
//! covers `$E000-$FFFF` and the CPU vectors are visible with no register
//! setup (map #70).
//!
//! ExRAM, nametables, IRQ, split, multiplier and audio registers are
//! accepted and ignored here; later tickets implement them. The `ppu_read`
//! byte-override hook stays at its default (`None`): this chip serves CHR
//! through the bank redirect only.

use super::{CartridgeCaps, Mapper, PpuFetchKind, PpuRenderState, memory::CartridgeMemory};
use crate::SystemClock;
use crate::ines::{FileVersion, Header};

/// 8 KB PRG window size in bytes.
const PRG_8K: usize = 0x2000;
/// PRG bank number bits in `$5114-$5117`: 8 KB banks, up to 128 (1 MB).
const BANK_MASK: u8 = 0x7f;
/// ROM/RAM select bit: `1` = ROM, `0` = WRAM (`$5114-$5116` only).
const ROM_SELECT_MASK: u8 = 0x80;
/// Power-on PRG bank for `$E000-$FFFF`: `$FF` masks to bank `$7F`, which wraps
/// to the last 8 KB bank of any power-of-two ROM up to 1 MB, so the CPU
/// vectors at `$FFFA-$FFFF` are visible with no register setup (map #70).
const POWER_ON_E000_BANK: u8 = 0xff;
/// Exact unlock values: writes to `$5102`/`$5103` are masked to two bits, so
/// only `$02` then `$01` unlocks (wiki "PRG RAM Protect", Mesen `_prgRamProtect1/2`).
const PROTECT1_UNLOCK: u8 = 0x02;
const PROTECT2_UNLOCK: u8 = 0x01;
/// Largest WRAM board: 128K single SRAM (wiki "PRG RAM capacity").
const MAX_WRAM_BYTES: usize = 128 * 1024;
/// CHR banking modes from `$5101` bits 1-0 (wiki §3.5 "CHR Bankswitching").
const CHR_MODE_8K: u8 = 0;
const CHR_MODE_4K: u8 = 1;
const CHR_MODE_2K: u8 = 2;
const CHR_MODE_1K: u8 = 3;

/// Which CHR register range was written last (ticket #78): in 8x16 mode the
/// last-written set serves CPU `$2007` reads (wiki §3.5). Power-on is Sprite,
/// matching the 8x8 behavior where `$2007` always reads the sprite set.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum ChrSet {
    #[default]
    Sprite,
    Bg,
}

/// MMC5 (ExROM, mapper 5): full PRG banking.
///
/// WRAM lives in [`CartridgeMemory`] (ADR-0015: the mapper owns no memory):
/// the constructor sizes it from the image header, `$5113` selects its
/// `$6000` bank, and cleared ROM/RAM-select bits map it into the `$8000+`
/// windows. Battery backing is tracked per 8 KB bank for future save/load;
/// with no persistence layer yet it has no read/write effect.
pub struct MMC5 {
    /// PRG banking mode from `$5100` bits 1-0.
    prg_mode: u8,
    /// Last values written to `$5113-$5117`: `$5113` is the 8 KB WRAM bank at
    /// `$6000`, `$5114-$5117` are the mode-dependent `$8000+` windows.
    prg_banks: [u8; 5],
    /// `$5102`/`$5103` write-protect keys (stored masked to two bits).
    protect1: u8,
    protect2: u8,
    prg_bank_count_8k: usize,
    wram_bank_count_8k: usize,
    /// Bit `i` set when the `i`-th 8 KB WRAM bank is battery-backed (16 bits
    /// cover the 128K maximum).
    battery_mask: u16,
    /// CHR banking mode from `$5101` bits 1-0: 0=8K, 1=4K, 2=2K, 3=1K.
    chr_mode: u8,
    /// Raw values written to `$5120-$5127` (sprite set, ticket #78).
    chr_spr: [u16; 8],
    /// Raw values written to `$5128-$512B` (background set, ticket #78).
    chr_bg: [u16; 4],
    /// Upper CHR bits from `$5130` bits 1-0, prepended to *subsequent* CHR
    /// register writes to reach 1MB in 1K/2K modes (wiki §3.5).
    chr_high: u8,
    /// Which CHR range was written last; serves `$2007` reads in 8x16 mode.
    last_chr_set: ChrSet,
}

impl MMC5 {
    pub fn new(memory: &mut CartridgeMemory, wram_size_bytes: usize, battery_bytes: usize) -> Self {
        memory.add_wram_banks(wram_size_bytes.min(MAX_WRAM_BYTES));
        let wram_bank_count_8k = memory.wram_bank_count_8k();
        // Battery covers whole 8 KB banks from bank 0 up; the ETROM
        // first-bank-only rule arrives here as `battery_bytes = 8K` on a
        // 16K board (see `wram_config`).
        let battery_banks = (battery_bytes / PRG_8K).min(wram_bank_count_8k).min(16);
        let battery_mask = if battery_banks >= 16 {
            0xffff
        } else {
            (1u16 << battery_banks) - 1
        };
        let mut mapper = Self {
            prg_mode: 3,
            prg_banks: [0, 0, 0, 0, POWER_ON_E000_BANK],
            protect1: 0,
            protect2: 0,
            prg_bank_count_8k: memory.prg_bank_count_8k(),
            wram_bank_count_8k,
            battery_mask,
            chr_mode: CHR_MODE_8K,
            chr_spr: [0; 8],
            chr_bg: [0; 4],
            chr_high: 0,
            last_chr_set: ChrSet::Sprite,
        };
        if wram_bank_count_8k == 0 {
            // ELROM (0K): `$6000-$7FFF` reads as open bus through the
            // `NesMcu` disabled-RAM path; RAM-selected `$8000+` windows keep
            // their mapping (see `map_select_8k`).
            memory.set_prg_ram_enabled(false);
        }
        mapper.sync_prg_banks(memory);
        mapper.sync_chr_windows(memory);
        mapper
    }

    /// Ticket #77 acceptance: register writes (and PRG-RAM data writes) take
    /// effect only after the exact unlock sequence `$5102=$02` then
    /// `$5103=$01`. Any other value written to either key re-locks.
    ///
    /// Hardware note (wiki/Mesen gate only PRG-RAM *data* writes this way and
    /// always honor bank-register writes); the ticket explicitly extends the
    /// gate to the PRG registers so stray writes cannot corrupt mappings.
    /// Revisit if a commercial game sets banks before unlocking.
    fn unlocked(&self) -> bool {
        self.protect1 == PROTECT1_UNLOCK && self.protect2 == PROTECT2_UNLOCK
    }

    /// True when WRAM `bank_8k` is battery-backed (ETROM: bank 0 only).
    ///
    /// No persistence layer reads this yet; it pins the header rule for the
    /// future save/load path.
    #[allow(dead_code)]
    pub fn wram_bank_is_battery_backed(&self, bank_8k: usize) -> bool {
        bank_8k < 16 && self.battery_mask & (1 << bank_8k) != 0
    }

    fn map_rom_8k(&self, memory: &mut CartridgeMemory, slot: u16, bank_8k: usize) {
        memory.map_prg_8k(slot, (bank_8k % self.prg_bank_count_8k) as u16);
    }

    /// One 8 KB `$8000+` window honoring the ROM/RAM select bit
    /// (`$5114-$5116`; `$5113`/`$5117` never reach here). RAM selected on a
    /// WRAM-less board keeps the window's previous mapping.
    fn map_select_8k(&self, memory: &mut CartridgeMemory, slot: u16, value: u8) {
        if value & ROM_SELECT_MASK == 0 {
            if self.wram_bank_count_8k == 0 {
                return;
            }
            let bank = (value & BANK_MASK) as usize % self.wram_bank_count_8k;
            memory.map_prg_wram_8k(slot, bank as u16);
        } else {
            self.map_rom_8k(memory, slot, (value & BANK_MASK) as usize);
        }
    }

    /// One 16 KB `$8000+` window (`$5115` in modes 1-2, `$5117` in mode 1):
    /// bit 0 is ignored and CPU A13 picks the half (wiki ignore-bits).
    fn map_select_16k(&self, memory: &mut CartridgeMemory, slot: u16, value: u8, rom_only: bool) {
        let ram = !rom_only && value & ROM_SELECT_MASK == 0;
        if ram && self.wram_bank_count_8k == 0 {
            return;
        }
        let base = (value & BANK_MASK & 0x7e) as usize;
        for half in 0..2u16 {
            let bank = base + half as usize;
            if ram {
                memory.map_prg_wram_8k(slot + half, (bank % self.wram_bank_count_8k) as u16);
            } else {
                self.map_rom_8k(memory, slot + half, bank);
            }
        }
    }

    /// Expands a raw CHR register value to one 1K bank number: the full
    /// bank number in current-mode units is `(chr_high << 8) | raw`, then
    /// `* size_in_1k + sub` (wiki §3.5; high bits are the top bits of the
    /// full 10-bit bank number). The cartridge pool modulo caps the 1MB max.
    fn chr_expand(&self, raw: u16, sub: u16) -> u16 {
        let size_1k = match self.chr_mode {
            CHR_MODE_8K => 8,
            CHR_MODE_4K => 4,
            CHR_MODE_2K => 2,
            _ => 1,
        };
        let full = ((self.chr_high as u16) << 8) | (raw & 0x3ff);
        full * size_1k + sub
    }

    /// Effective 1K bank of the sprite set (`$5120-$5127`) for pattern-table
    /// slot 0-7 (wiki §3.5 register→range table).
    fn spr_bank_1k(&self, slot: u16) -> u16 {
        match self.chr_mode {
            CHR_MODE_1K => self.chr_expand(self.chr_spr[slot as usize], 0),
            CHR_MODE_2K => {
                let reg = (slot / 2) * 2 + 1;
                self.chr_expand(self.chr_spr[reg as usize], slot % 2)
            }
            CHR_MODE_4K => {
                let reg = if slot < 4 { 3 } else { 7 };
                self.chr_expand(self.chr_spr[reg], slot % 4)
            }
            _ => self.chr_expand(self.chr_spr[7], slot),
        }
    }

    /// Effective 1K bank of the background set (`$5128-$512B`) for slot 0-7:
    /// each BG register covers two 1K slots (wiki §3.5).
    fn bg_bank_1k(&self, slot: u16) -> u16 {
        match self.chr_mode {
            CHR_MODE_1K => self.chr_expand(self.chr_bg[(slot % 4) as usize], 0),
            CHR_MODE_2K => {
                let reg = if slot % 4 < 2 { 1 } else { 3 };
                self.chr_expand(self.chr_bg[reg], slot % 2)
            }
            CHR_MODE_4K => self.chr_expand(self.chr_bg[3], slot % 4),
            _ => self.chr_expand(self.chr_bg[3], slot),
        }
    }

    /// Reprograms all eight 1K windows to the last-written set's effective
    /// mapping: the `$2007`/fallback plane (ticket #78). Background writes
    /// deliberately skip this (wiki: BG regs are ignored in 8x8, so windows
    /// always follow the sprite set until a sprite write), while render-time
    /// reads go through [`Mapper::ppu_chr_bank`] and see the right set in
    /// both modes.
    fn sync_chr_windows(&self, memory: &mut CartridgeMemory) {
        for slot in 0..8u16 {
            let bank = match self.last_chr_set {
                ChrSet::Sprite => self.spr_bank_1k(slot),
                ChrSet::Bg => self.bg_bank_1k(slot),
            };
            memory.map_chr_1k(slot, bank);
        }
    }

    /// Modes (wiki "Banks"): `$5113` always selects RAM at `$6000` (bit 7
    /// ignored); `$5117` always selects ROM (bit 7 ignored).
    fn sync_prg_banks(&mut self, memory: &mut CartridgeMemory) {
        memory.set_wram_6000_bank((self.prg_banks[0] & BANK_MASK) as u16);
        match self.prg_mode {
            // Mode 0: 32 KB ROM at `$8000-$FFFF` from `$5117`, bits 1-0 ignored.
            0 => {
                let base = (self.prg_banks[4] & BANK_MASK & 0x7c) as usize;
                for slot in 0..4u16 {
                    self.map_rom_8k(memory, slot, base + slot as usize);
                }
            }
            // Mode 1: 16 KB ROM/RAM at `$8000-$BFFF` (`$5115`), 16 KB ROM at
            // `$C000-$FFFF` (`$5117`).
            1 => {
                self.map_select_16k(memory, 0, self.prg_banks[2], false);
                self.map_select_16k(memory, 2, self.prg_banks[4], true);
            }
            // Mode 2 (Castlevania III): 16 KB ROM/RAM at `$8000-$BFFF`
            // (`$5115`), 8 KB ROM/RAM at `$C000-$DFFF` (`$5116`), 8 KB ROM at
            // `$E000-$FFFF` (`$5117`).
            2 => {
                self.map_select_16k(memory, 0, self.prg_banks[2], false);
                self.map_select_8k(memory, 2, self.prg_banks[3]);
                self.map_rom_8k(memory, 3, (self.prg_banks[4] & BANK_MASK) as usize);
            }
            // Mode 3: four independent 8 KB windows at `$8000/$A000/$C000/$E000`.
            _ => {
                self.map_select_8k(memory, 0, self.prg_banks[1]);
                self.map_select_8k(memory, 1, self.prg_banks[2]);
                self.map_select_8k(memory, 2, self.prg_banks[3]);
                self.map_rom_8k(memory, 3, (self.prg_banks[4] & BANK_MASK) as usize);
            }
        }
    }
}

/// WRAM size and battery bytes from the image header (map #70):
/// ELROM 0K / EKROM 8K / ETROM 2x8K / EWROM 32K, over-32K boards up to 128K.
///
/// Battery follows the iNES battery bit, except a 16K two-bank board
/// (ETROM) battery-backs only the first 8K bank (wiki "Other PRG-RAM notes").
/// NES 2.0 headers size RAM/NVRAM separately: NVRAM bytes are the battery
/// bytes. Sizes clamp to the 128K hardware maximum.
pub(crate) fn wram_config(header: &Header, version: FileVersion) -> (usize, usize) {
    let total = match version {
        FileVersion::Nes20 => header.prg_ram_size.unwrap_or(0) + header.prg_nvram_size.unwrap_or(0),
        FileVersion::INes => header.prg_ram_size.unwrap_or(8 * 1024),
    };
    let total = total.min(MAX_WRAM_BYTES);
    let battery = match version {
        FileVersion::Nes20 => header.prg_nvram_size.unwrap_or(0).min(total),
        FileVersion::INes => {
            if header.battery_backed_ram {
                if total == 2 * PRG_8K { PRG_8K } else { total }
            } else {
                0
            }
        }
    };
    (total, battery)
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
            0x5100 => {
                if self.unlocked() {
                    self.prg_mode = value & 0x03;
                    self.sync_prg_banks(memory);
                }
            }
            0x5102 => self.protect1 = value & 0x03,
            0x5103 => self.protect2 = value & 0x03,
            // Ticket #78: CHR mode needs no unlock (unlike the PRG
            // registers): games set banks freely, and the boot ROMs write
            // these before any unlock sequence.
            0x5101 => {
                self.chr_mode = value & 0x03;
                self.sync_chr_windows(memory);
            }
            0x5120..=0x5127 => {
                self.chr_spr[(address - 0x5120) as usize] = value as u16;
                self.last_chr_set = ChrSet::Sprite;
                self.sync_chr_windows(memory);
            }
            0x5128..=0x512b => {
                self.chr_bg[(address - 0x5128) as usize] = value as u16;
                self.last_chr_set = ChrSet::Bg;
            }
            // Upper CHR bits apply to *subsequent* CHR writes only (wiki
            // §3.5): store without reprogramming; the next CHR write picks
            // them up. In ExAttr mode these act as global MSBs (ticket #79).
            0x5130 => self.chr_high = value & 0x03,
            0x5113..=0x5117 => {
                if self.unlocked() {
                    self.prg_banks[(address - 0x5113) as usize] = value;
                    self.sync_prg_banks(memory);
                }
            }
            // `$6000-$7FFF` data write through the `$5113` WRAM bank.
            0x6000..=0x7fff => {
                if self.unlocked() {
                    memory.write_prg_ram(address, value);
                }
            }
            // `$8000-$FFFF` carries no MMC5 registers: a data write through
            // a WRAM-mapped window (Bandit Kings writes back through
            // `$8000+`); ROM-mapped windows ignore it.
            0x8000..=0xffff if self.unlocked() => {
                memory.write_prg(address, value);
            }
            // Every other MMC5 register (`$5104` ExRAM mode, `$5105`
            // nametables, `$5200-$5206` split/IRQ/multiplier, `$5010/$5015`
            // audio, `$5C00-$5FFF` ExRAM) is accepted and ignored; later
            // tickets implement them.
            _ => {}
        }
    }

    /// Ticket #78 CHR bank redirect, gated by the shared `ppu_read` flag:
    /// `$0000-$1FFF` maps to the sprite set in 8x8 mode; in 8x16 mode
    /// background fetches use the background set, sprite fetches the sprite
    /// set, and CPU `$2007` reads use whichever set was written last
    /// (wiki §3.5). `$2000+` declines: the nametable plane belongs to #79.
    fn ppu_chr_bank(&self, addr: u16, render: &PpuRenderState) -> Option<u16> {
        if addr >= 0x2000 {
            return None;
        }
        let slot = (addr >> 10) & 7;
        let bank = if !render.sprite_8x16 {
            self.spr_bank_1k(slot)
        } else {
            match render.fetch {
                PpuFetchKind::Background => self.bg_bank_1k(slot),
                PpuFetchKind::Sprite => self.spr_bank_1k(slot),
                PpuFetchKind::Cpu => match self.last_chr_set {
                    ChrSet::Sprite => self.spr_bank_1k(slot),
                    ChrSet::Bg => self.bg_bank_1k(slot),
                },
            }
        };
        Some(bank)
    }

    fn caps(&self) -> CartridgeCaps {
        CartridgeCaps {
            ppu_read: true,
            ..CartridgeCaps::default()
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
    fn board(
        banks_8k: usize,
        vectors: [u8; 6],
        wram_size_bytes: usize,
        battery_bytes: usize,
    ) -> (MMC5, CartridgeMemory) {
        let mut prg = vec![0u8; PRG_8K * banks_8k];
        for (i, chunk) in prg.chunks_mut(PRG_8K).enumerate() {
            chunk[0] = i as u8;
        }
        let tail = prg.len() - vectors.len();
        prg[tail..].copy_from_slice(&vectors);
        let mut memory = CartridgeMemory::from_rom(&prg, &[], None, Mirroring::Horizontal);
        let mapper = MMC5::new(&mut memory, wram_size_bytes, battery_bytes);
        (mapper, memory)
    }

    /// Default board: 8K WRAM, no battery (plain old-iNES header default).
    fn board_8k(banks_8k: usize, vectors: [u8; 6]) -> (MMC5, CartridgeMemory) {
        board(banks_8k, vectors, 8 * 1024, 0)
    }

    fn unlock(mapper: &mut MMC5, memory: &mut CartridgeMemory) {
        mapper.cpu_write(0x5102, 0x02, SystemClock::default(), memory);
        mapper.cpu_write(0x5103, 0x01, SystemClock::default(), memory);
    }

    // Power-on is mode 3 with `$5117 = $FF`: the last 8 KB bank covers
    // `$E000-$FFFF`, so the CPU vectors at `$FFFA-$FFFF` read correctly with
    // no register setup (spec #74 user story 4, map #70).
    #[test]
    fn power_on_shows_vectors_from_last_bank() {
        let vectors = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66];
        let (_, memory) = board_8k(2, vectors);

        assert_eq!(memory.read_prg(0xfffa), 0x11);
        assert_eq!(memory.read_prg(0xfffb), 0x22);
        assert_eq!(memory.read_prg(0xfffc), 0x33);
        assert_eq!(memory.read_prg(0xfffd), 0x44);
        assert_eq!(memory.read_prg(0xfffe), 0x55);
        assert_eq!(memory.read_prg(0xffff), 0x66);
    }

    #[test]
    fn power_on_maps_wram_bank_0_at_8000_and_last_bank_at_e000() {
        let (_, memory) = board_8k(2, [0; 6]);

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
        let (mut mapper, mut memory) = board_8k(4, [0; 6]);
        unlock(&mut mapper, &mut memory);

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
        let (mut mapper, mut memory) = board_8k(4, [0; 6]);
        unlock(&mut mapper, &mut memory);

        // `$7F` is the power-on `$5117` value: 127 wraps to the last of 4 banks.
        mapper.cpu_write(0x5117, 0xff, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0xe000), 3);

        mapper.cpu_write(0x5114, 0x80 | 6, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 2);
    }

    // The boot code of the local MMC5 test ROMs writes CHR, nametable, IRQ
    // and audio registers (`$5101/$5104/$5105/$5127/$5200/$5204/$5015`):
    // CHR writes take effect in the CHR unit (ticket #78) while the rest are
    // accepted and ignored, leaving PRG mapping intact either way.
    #[test]
    fn other_mmc5_registers_leave_prg_mapping_intact() {
        let (mut mapper, mut memory) = board_8k(4, [0; 6]);
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
    fn prg_ram_window_round_trips_after_unlock() {
        let (mut mapper, mut memory) = board_8k(2, [0; 6]);
        unlock(&mut mapper, &mut memory);

        mapper.cpu_write(0x6000, 0x5a, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg_ram(0x6000), 0x5a);
    }

    // Ticket #77 acceptance: register writes take effect only after the exact
    // unlock sequence `$5102=$02` then `$5103=$01` (spec #74 user story 8).
    #[test]
    fn bank_registers_ignored_until_exact_unlock_sequence() {
        let (mut mapper, mut memory) = board_8k(8, [0; 6]);

        // Locked at power-on: mode and bank writes have no effect. Mode 1
        // would show ROM bank 6 at `$C000` (`$5117=$FF`); locked it stays
        // WRAM bank 0, and `$8000` stays WRAM instead of ROM bank 3.
        mapper.cpu_write(0x5100, 0x01, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5114, 0x80 | 3, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0);
        assert_eq!(memory.read_prg(0xc000), 0);
        assert_eq!(memory.read_prg(0xe000), 7);

        // Half the sequence (`$5102` only) still locks.
        mapper.cpu_write(0x5102, 0x02, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5114, 0x80 | 3, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0);

        // Wrong second value locks.
        mapper.cpu_write(0x5103, 0x03, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5114, 0x80 | 3, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0);

        // Exact sequence unlocks: the new values now take effect (mode
        // register included: mode 1 shows ROM at `$8000/$C000`).
        unlock(&mut mapper, &mut memory);
        mapper.cpu_write(0x5100, 0x01, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5115, 0x80 | 2, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5117, 0x80 | 4, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 2);
        assert_eq!(memory.read_prg(0xc000), 4);

        // Any other value to either key re-locks.
        mapper.cpu_write(0x5103, 0x00, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5117, 0x80 | 6, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0xc000), 4);
    }

    // Mode 0: 32 KB ROM at `$8000-$FFFF` from `$5117`, low two bits ignored.
    #[test]
    fn mode_0_maps_32k_rom_ignoring_low_two_bits() {
        let (mut mapper, mut memory) = board_8k(8, [0; 6]);
        unlock(&mut mapper, &mut memory);

        mapper.cpu_write(0x5100, 0x00, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5117, 0x80 | 4, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 4);
        assert_eq!(memory.read_prg(0xa000), 5);
        assert_eq!(memory.read_prg(0xc000), 6);
        assert_eq!(memory.read_prg(0xe000), 7);

        // `$85` differs only in the ignored bits: identical mapping.
        mapper.cpu_write(0x5117, 0x80 | 5, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 4);
        assert_eq!(memory.read_prg(0xe000), 7);
    }

    // Mode 1: 16 KB ROM/RAM at `$8000-$BFFF` (`$5115`), 16 KB ROM at
    // `$C000-$FFFF` (`$5117`); bit 0 ignored on both.
    #[test]
    fn mode_1_maps_two_16k_windows() {
        let (mut mapper, mut memory) = board_8k(8, [0; 6]);
        unlock(&mut mapper, &mut memory);

        mapper.cpu_write(0x5100, 0x01, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5115, 0x80 | 2, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5117, 0x80 | 4, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 2);
        assert_eq!(memory.read_prg(0xa000), 3);
        assert_eq!(memory.read_prg(0xc000), 4);
        assert_eq!(memory.read_prg(0xe000), 5);

        // Odd `$5115` drops bit 0: `$83` selects the same 16K as `$82`.
        mapper.cpu_write(0x5115, 0x80 | 3, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 2);
        assert_eq!(memory.read_prg(0xa000), 3);
    }

    // Mode 2 (Castlevania III, spec #74 user story 3): 16 KB ROM/RAM at
    // `$8000-$BFFF` (`$5115`), 8 KB ROM/RAM at `$C000-$DFFF` (`$5116`), 8 KB
    // ROM at `$E000-$FFFF` (`$5117`).
    #[test]
    fn mode_2_maps_16k_plus_two_8k_windows() {
        let (mut mapper, mut memory) = board_8k(8, [0; 6]);
        unlock(&mut mapper, &mut memory);

        mapper.cpu_write(0x5100, 0x02, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5115, 0x80 | 2, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5116, 0x80 | 5, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5117, 0x80 | 7, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 2);
        assert_eq!(memory.read_prg(0xa000), 3);
        assert_eq!(memory.read_prg(0xc000), 5);
        assert_eq!(memory.read_prg(0xe000), 7);
    }

    // Cleared bit 7 on `$5114-$5116` maps WRAM into the `$8000+` windows
    // (modes 1-3); `$5117` always selects ROM even with bit 7 clear (wiki).
    #[test]
    fn ram_select_maps_wram_at_high_windows_but_last_window_stays_rom() {
        let (mut mapper, mut memory) = board_8k(4, [0; 6]);
        unlock(&mut mapper, &mut memory);

        // Mode 3: `$C000` from RAM bank 0, `$E000` stays ROM despite bit 7 clear.
        mapper.cpu_write(0x5116, 0x00, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5117, 0x01, SystemClock::default(), &mut memory);
        mapper.cpu_write(0xc000, 0xa5, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0xc000), 0xa5);
        // The `$E000` write hit ROM and was ignored; the window shows ROM bank 1.
        assert_eq!(memory.read_prg(0xe000), 1);

        // Mode 1: `$8000-$BFFF` from 16K RAM bank 0.
        mapper.cpu_write(0x5100, 0x01, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5115, 0x00, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x8000, 0x5a, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0x5a);
        assert_eq!(memory.read_prg(0xa000), 0x5a);
    }

    // `$5113` selects the 8 KB WRAM bank at `$6000-$7FFF` in every mode,
    // bit 7 ignored (wiki: "`$5113` always maps RAM").
    #[test]
    fn wram_6000_window_bankable_in_every_mode() {
        let (mut mapper, mut memory) = board(4, [0; 6], 32 * 1024, 0);
        unlock(&mut mapper, &mut memory);

        for mode in [0u8, 1, 2, 3] {
            mapper.cpu_write(0x5100, mode, SystemClock::default(), &mut memory);
            mapper.cpu_write(0x5113, 0x02, SystemClock::default(), &mut memory);
            mapper.cpu_write(0x6000, mode + 0x10, SystemClock::default(), &mut memory);
            mapper.cpu_write(0x5113, 0x80 | 0x03, SystemClock::default(), &mut memory);
            mapper.cpu_write(0x6000, mode + 0x20, SystemClock::default(), &mut memory);

            mapper.cpu_write(0x5113, 0x02, SystemClock::default(), &mut memory);
            assert_eq!(
                memory.read_prg_ram(0x6000),
                mode + 0x10,
                "mode {mode} bank 2"
            );
            mapper.cpu_write(0x5113, 0x03, SystemClock::default(), &mut memory);
            assert_eq!(
                memory.read_prg_ram(0x6000),
                mode + 0x20,
                "mode {mode} bank 3"
            );
        }
    }

    // Over-32K WRAM (ticket #77: up to 128K) mappable at a high window:
    // Uncharted Waters banks RAM at `$8000+`.
    #[test]
    fn over_32k_wram_maps_at_high_window() {
        let (mut mapper, mut memory) = board(4, [0; 6], 64 * 1024, 0);
        unlock(&mut mapper, &mut memory);

        mapper.cpu_write(0x5114, 0x06, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x8000, 0x77, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0x77);

        // A different high RAM bank holds different contents.
        mapper.cpu_write(0x5114, 0x07, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0x00);
        mapper.cpu_write(0x5114, 0x06, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0x77);
    }

    // PRG-RAM data writes need the unlock sequence too (wiki "PRG RAM
    // Protect", Mesen access gating): locked writes are dropped, on both the
    // low `$6000` window and a RAM-mapped `$8000+` window (Bandit Kings
    // writes back through `$8000+`).
    #[test]
    fn ram_data_writes_need_unlock() {
        let (mut mapper, mut memory) = board_8k(4, [0; 6]);

        mapper.cpu_write(0x6000, 0x5a, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg_ram(0x6000), 0);

        unlock(&mut mapper, &mut memory);
        mapper.cpu_write(0x5114, 0x00, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x6000, 0x5a, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg_ram(0x6000), 0x5a);

        mapper.cpu_write(0x8000, 0xa5, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0xa5);
    }

    // Bank numbers wrap modulo the WRAM size (wiki-compatible superset:
    // no game probes distinct values for the same cell).
    #[test]
    fn wram_bank_wraps_modulo_board_size() {
        let (mut mapper, mut memory) = board_8k(2, [0; 6]);
        unlock(&mut mapper, &mut memory);

        mapper.cpu_write(0x5113, 0x00, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x6000, 0x3c, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5113, 0x03, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg_ram(0x6000), 0x3c);
    }

    // ELROM (0K WRAM): `$6000` is disabled (open bus via `NesMcu`), and a
    // RAM-selected `$8000+` window keeps its mapping instead of panicking.
    #[test]
    fn zero_wram_disables_6000_window_and_never_panics() {
        let (mut mapper, mut memory) = board(2, [0; 6], 0, 0);
        unlock(&mut mapper, &mut memory);

        assert!(!memory.prg_ram_enabled());
        mapper.cpu_write(0x5114, 0x00, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0);
        mapper.cpu_write(0x8000, 0xff, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0);
    }

    // Battery backing follows the header with the ETROM first-bank-only rule
    // (map #70, wiki "Games with 16K PRG-RAM only battery-save the first 8K").
    #[test]
    fn battery_first_bank_only_on_two_bank_board() {
        let (etrom, _) = board(2, [0; 6], 16 * 1024, 8 * 1024);
        assert!(etrom.wram_bank_is_battery_backed(0));
        assert!(!etrom.wram_bank_is_battery_backed(1));

        let (ekrom, _) = board(2, [0; 6], 8 * 1024, 8 * 1024);
        assert!(ekrom.wram_bank_is_battery_backed(0));

        let (no_battery, _) = board(2, [0; 6], 8 * 1024, 0);
        assert!(!no_battery.wram_bank_is_battery_backed(0));

        let (big, _) = board(2, [0; 6], 128 * 1024, 128 * 1024);
        assert!(big.wram_bank_is_battery_backed(15));
        assert!(!big.wram_bank_is_battery_backed(16));
    }

    // `wram_config`: board sizes and battery rules from the image header.
    fn header_with(
        battery: bool,
        prg_ram_size: Option<usize>,
        prg_nvram_size: Option<usize>,
    ) -> Header {
        Header {
            n_prg_pages: 1,
            n_chr_pages: 1,
            prg_rom_size: 16 * 1024,
            chr_rom_size: 8 * 1024,
            mapper_no: 5,
            submapper_no: None,
            ignore_mirror_control: false,
            has_trainer: false,
            battery_backed_ram: battery,
            nametable_arrangement: crate::ines::NametableArrangement::Vertical,
            console_type: 0,
            prg_ram_size,
            prg_nvram_size,
            chr_ram_size: None,
            chr_nvram_size: None,
            timing_mode: None,
            vs_ppu_type: None,
            vs_hardware_type: None,
            extended_console_type: None,
            misc_rom_count: None,
            default_expansion_device: None,
        }
    }

    #[test]
    fn wram_config_follows_header_with_etrom_first_bank_only() {
        // Old iNES EKROM: flags8 8K + battery backs the whole bank.
        assert_eq!(
            wram_config(&header_with(true, Some(8 * 1024), None), FileVersion::INes),
            (8 * 1024, 8 * 1024)
        );
        // Old iNES ETROM: 16K + battery backs only the first 8K bank.
        assert_eq!(
            wram_config(&header_with(true, Some(16 * 1024), None), FileVersion::INes),
            (16 * 1024, 8 * 1024)
        );
        // No battery bit: no battery bytes whatever the size.
        assert_eq!(
            wram_config(
                &header_with(false, Some(32 * 1024), None),
                FileVersion::INes
            ),
            (32 * 1024, 0)
        );
        // NES 2.0: RAM and NVRAM sized separately; NVRAM bytes are battery.
        assert_eq!(
            wram_config(
                &header_with(false, Some(64 * 1024), Some(64 * 1024)),
                FileVersion::Nes20
            ),
            (128 * 1024, 64 * 1024)
        );
        // NES 2.0 with no RAM fields: ELROM (0K).
        assert_eq!(
            wram_config(&header_with(false, None, None), FileVersion::Nes20),
            (0, 0)
        );
        // Clamp to the 128K hardware maximum.
        assert_eq!(
            wram_config(
                &header_with(false, Some(256 * 1024), None),
                FileVersion::Nes20
            ),
            (128 * 1024, 0)
        );
    }

    // ---- Ticket #78 CHR banking ----

    const CHR_1K: usize = 0x400;

    /// CHR board: `banks_1k` 1K banks with bank `i` filled with byte `i`, so
    /// the last byte of each bank — what the matrix ROM's `ReadData` samples
    /// via `$2006`/`$2007` (`$03FF..$1FFF`) — reads back as the bank index.
    fn chr_board(banks_1k: usize) -> (MMC5, CartridgeMemory) {
        let mut chr = vec![0u8; CHR_1K * banks_1k];
        for (i, chunk) in chr.chunks_mut(CHR_1K).enumerate() {
            chunk.fill(i as u8);
        }
        let prg = vec![0u8; 2 * PRG_8K];
        let mut memory = CartridgeMemory::from_rom(&prg, &chr, None, Mirroring::Horizontal);
        let mapper = MMC5::new(&mut memory, 8 * 1024, 0);
        (mapper, memory)
    }

    fn chr_write(mapper: &mut MMC5, memory: &mut CartridgeMemory, addr: u16, value: u8) {
        mapper.cpu_write(addr, value, SystemClock::default(), memory);
    }

    /// PPU-built render state for one fetch kind (ticket #78): the CPU
    /// `$2007` path builds [`PpuFetchKind::Cpu`], background fetches
    /// [`PpuFetchKind::Background`], sprite fetches [`PpuFetchKind::Sprite`].
    fn chr_render(fetch: PpuFetchKind, sprite_8x16: bool) -> PpuRenderState {
        PpuRenderState {
            rendering_enabled: true,
            sprite_8x16,
            fetch_phase: 0,
            fetch,
        }
    }

    fn bank_of(mapper: &MMC5, slot: u16, fetch: PpuFetchKind, tall: bool) -> Option<u16> {
        mapper.ppu_chr_bank(slot * 0x400, &chr_render(fetch, tall))
    }

    // 1K mode: each `$5120-$5127` register maps its own 1K slot, and the
    // windows follow the last-written (sprite) set.
    #[test]
    fn chr_1k_mode_each_sprite_reg_maps_one_slot() {
        let (mut mapper, mut memory) = chr_board(8);
        chr_write(&mut mapper, &mut memory, 0x5101, 3);
        for slot in 0..8u16 {
            chr_write(&mut mapper, &mut memory, 0x5120 + slot, slot as u8);
        }

        for slot in 0..8u16 {
            assert_eq!(
                bank_of(&mapper, slot, PpuFetchKind::Sprite, true),
                Some(slot)
            );
            assert_eq!(
                memory.read_vram(slot * 0x400),
                slot as u8,
                "window slot {slot} follows the sprite set"
            );
        }
    }

    // 1K mode: each `$5128-$512B` register covers two 1K slots (wiki §3.5).
    #[test]
    fn chr_1k_bg_regs_cover_two_slots_each() {
        let (mut mapper, mut memory) = chr_board(8);
        chr_write(&mut mapper, &mut memory, 0x5101, 3);
        for slot in 0..8u16 {
            chr_write(&mut mapper, &mut memory, 0x5120 + slot, 7);
        }
        for reg in 0..4u16 {
            chr_write(&mut mapper, &mut memory, 0x5128 + reg, reg as u8);
        }

        // `$5128` covers slots 0 and 4, `$5129` slots 1 and 5, and so on.
        for slot in 0..8u16 {
            assert_eq!(
                bank_of(&mapper, slot, PpuFetchKind::Background, true),
                Some(slot % 4),
                "bg slot {slot}"
            );
        }
        // Sprite fetches still see the sprite set on the same addresses.
        for slot in 0..8u16 {
            assert_eq!(bank_of(&mapper, slot, PpuFetchKind::Sprite, true), Some(7));
        }
    }

    // 2K mode: only the odd sprite registers (`$5121/$5123/$5125/$5127`)
    // map, each covering two 1K slots; BG uses `$5129`/`$512B` the same way.
    #[test]
    fn chr_2k_mode_uses_odd_regs_for_two_slots_each() {
        let (mut mapper, mut memory) = chr_board(8);
        chr_write(&mut mapper, &mut memory, 0x5101, 2);
        // Even registers are ignored: `$5120` must not move slots 0-1.
        chr_write(&mut mapper, &mut memory, 0x5120, 7);
        chr_write(&mut mapper, &mut memory, 0x5121, 2);
        chr_write(&mut mapper, &mut memory, 0x5123, 0);
        chr_write(&mut mapper, &mut memory, 0x5125, 1);
        chr_write(&mut mapper, &mut memory, 0x5127, 3);

        for (slot, want) in [
            (0, 4),
            (1, 5),
            (2, 0),
            (3, 1),
            (4, 2),
            (5, 3),
            (6, 6),
            (7, 7),
        ] {
            assert_eq!(
                bank_of(&mapper, slot, PpuFetchKind::Sprite, true),
                Some(want),
                "sprite slot {slot}"
            );
        }

        chr_write(&mut mapper, &mut memory, 0x5129, 1);
        chr_write(&mut mapper, &mut memory, 0x512b, 0);
        // `$5129` covers slots {0,1,4,5}, `$512B` covers {2,3,6,7}.
        for (slot, want) in [
            (0, 2),
            (1, 3),
            (2, 0),
            (3, 1),
            (4, 2),
            (5, 3),
            (6, 0),
            (7, 1),
        ] {
            assert_eq!(
                bank_of(&mapper, slot, PpuFetchKind::Background, true),
                Some(want),
                "bg slot {slot}"
            );
        }
    }

    // 4K mode: `$5123` maps `$0000-$0FFF`, `$5127` maps `$1000-$1FFF`;
    // BG uses `$512B` for both halves.
    #[test]
    fn chr_4k_mode_two_regs_cover_halves() {
        let (mut mapper, mut memory) = chr_board(16);
        chr_write(&mut mapper, &mut memory, 0x5101, 1);
        chr_write(&mut mapper, &mut memory, 0x5123, 1);
        chr_write(&mut mapper, &mut memory, 0x5127, 0);

        for slot in 0..8u16 {
            let want = if slot < 4 { 4 + slot } else { slot - 4 };
            assert_eq!(
                bank_of(&mapper, slot, PpuFetchKind::Sprite, true),
                Some(want)
            );
        }

        chr_write(&mut mapper, &mut memory, 0x512b, 2);
        for slot in 0..8u16 {
            assert_eq!(
                bank_of(&mapper, slot, PpuFetchKind::Background, true),
                Some(8 + slot % 4)
            );
        }
    }

    // 8K mode: `$5127` alone maps the whole `$0000-$1FFF` (BG: `$512B`).
    #[test]
    fn chr_8k_mode_single_reg_maps_all_slots() {
        let (mut mapper, mut memory) = chr_board(16);
        chr_write(&mut mapper, &mut memory, 0x5101, 0);
        chr_write(&mut mapper, &mut memory, 0x5127, 1);

        for slot in 0..8u16 {
            assert_eq!(
                bank_of(&mapper, slot, PpuFetchKind::Sprite, true),
                Some(8 + slot)
            );
        }

        chr_write(&mut mapper, &mut memory, 0x512b, 0);
        for slot in 0..8u16 {
            assert_eq!(
                bank_of(&mapper, slot, PpuFetchKind::Background, true),
                Some(slot)
            );
        }
    }

    // `$5130` upper bits prepend to *subsequent* CHR writes only (wiki §3.5):
    // writing it never reprograms the current mapping.
    #[test]
    fn chr_high_bits_shift_subsequent_writes_only() {
        let (mut mapper, mut memory) = chr_board(8);
        chr_write(&mut mapper, &mut memory, 0x5101, 3);
        chr_write(&mut mapper, &mut memory, 0x5120, 5);
        chr_write(&mut mapper, &mut memory, 0x5121, 6);
        assert_eq!(bank_of(&mapper, 0, PpuFetchKind::Sprite, true), Some(5));

        chr_write(&mut mapper, &mut memory, 0x5130, 1);
        // The hook is a pure function of the stored regs (design 7), so the
        // high bits shift reads immediately — like hardware, where the bank
        // decode is combinational. The windows intentionally lag: they only
        // reprogram on CHR writes (the write-routing fallback).
        assert_eq!(bank_of(&mapper, 0, PpuFetchKind::Sprite, true), Some(0x105));
        assert_eq!(bank_of(&mapper, 1, PpuFetchKind::Sprite, true), Some(0x106));
        assert_eq!(memory.read_vram(0x0000), 5);

        // The next CHR write picks the high bits up: `(1 << 8) | 5`.
        chr_write(&mut mapper, &mut memory, 0x5120, 5);
        assert_eq!(bank_of(&mapper, 0, PpuFetchKind::Sprite, true), Some(0x105));
        // The pool modulo wraps the 1MB bank onto the 8-bank test board.
        assert_eq!(memory.read_vram(0x0000), 5);
    }

    // 8x8 sprites: `$5128-$512B` are completely ignored (wiki §3.5) — every
    // fetch kind and the windows see the sprite set.
    #[test]
    fn chr_8x8_ignores_bg_regs_everywhere() {
        let (mut mapper, mut memory) = chr_board(8);
        chr_write(&mut mapper, &mut memory, 0x5101, 3);
        for slot in 0..8u16 {
            chr_write(&mut mapper, &mut memory, 0x5120 + slot, slot as u8);
        }
        for reg in 0..4u16 {
            chr_write(&mut mapper, &mut memory, 0x5128 + reg, 7 - reg as u8);
        }

        for slot in 0..8u16 {
            for fetch in [
                PpuFetchKind::Background,
                PpuFetchKind::Sprite,
                PpuFetchKind::Cpu,
            ] {
                assert_eq!(
                    bank_of(&mapper, slot, fetch, false),
                    Some(slot),
                    "8x8 slot {slot} fetch {fetch:?} reads the sprite set"
                );
            }
            assert_eq!(
                memory.read_vram(slot * 0x400),
                slot as u8,
                "8x8 window slot {slot} unchanged by BG writes"
            );
        }
    }

    // 8x16 `$2007` reads use the last-written set: after a sprite write
    // then a background write the CPU sees the background mapping, and a
    // fresh sprite write flips it back.
    #[test]
    fn chr_last_written_set_serves_cpu_reads_in_8x16() {
        let (mut mapper, mut memory) = chr_board(8);
        chr_write(&mut mapper, &mut memory, 0x5101, 3);
        for slot in 0..8u16 {
            chr_write(&mut mapper, &mut memory, 0x5120 + slot, slot as u8);
        }
        // Last set is sprite: CPU reads see the sprite mapping.
        for slot in 0..8u16 {
            assert_eq!(bank_of(&mapper, slot, PpuFetchKind::Cpu, true), Some(slot));
        }

        for reg in 0..4u16 {
            chr_write(&mut mapper, &mut memory, 0x5128 + reg, 7);
        }
        for slot in 0..8u16 {
            assert_eq!(
                bank_of(&mapper, slot, PpuFetchKind::Cpu, true),
                Some(7),
                "CPU slot {slot} follows the background set after a BG write"
            );
        }
        // Render fetches are unaffected by the last-written latch.
        assert_eq!(bank_of(&mapper, 2, PpuFetchKind::Sprite, true), Some(2));
        assert_eq!(bank_of(&mapper, 2, PpuFetchKind::Background, true), Some(7));

        chr_write(&mut mapper, &mut memory, 0x5122, 4);
        assert_eq!(bank_of(&mapper, 2, PpuFetchKind::Cpu, true), Some(4));
        assert_eq!(bank_of(&mapper, 5, PpuFetchKind::Cpu, true), Some(5));
    }

    // Nametable addresses never take the CHR redirect (ExRAM tickets own
    // the `$2000+` plane).
    #[test]
    fn chr_hook_declines_nametable_plane() {
        let (mapper, _) = chr_board(8);
        for addr in [0x2000, 0x2400, 0x3eff] {
            assert_eq!(
                mapper.ppu_chr_bank(addr, &chr_render(PpuFetchKind::Background, true)),
                None
            );
        }
    }

    // Cartridge-level (ticket #78 acceptance): programmed through
    // `cpu_write`, `read_vram` serves the pool byte from the bank each fetch
    // kind selects — the same `$03FF..$1FFF`-style sampling the matrix ROM's
    // `ReadData` performs through `$2007` (which builds the Cpu render).
    #[test]
    fn chr_cartridge_read_vram_routes_each_fetch_kind() {
        let (mut mapper, mut memory) = chr_board(8);
        chr_write(&mut mapper, &mut memory, 0x5101, 3);
        for slot in 0..8u16 {
            chr_write(&mut mapper, &mut memory, 0x5120 + slot, slot as u8);
        }
        // Background set: `$5128-$512B` = 4,5,6,7 → slots read 4,5,6,7,4,5,6,7.
        for reg in 0..4u16 {
            chr_write(&mut mapper, &mut memory, 0x5128 + reg, 4 + reg as u8);
        }
        let cart = crate::nes::mapper::Cartridge::new(Box::new(mapper), memory);

        // Sprite fetches read the sprite banks (fill byte == bank index).
        for slot in 0..8u16 {
            assert_eq!(
                cart.read_vram(
                    slot * 0x400 + 0x3ff,
                    &chr_render(PpuFetchKind::Sprite, true)
                ),
                slot as u8,
                "sprite slot {slot}"
            );
        }
        // Background fetches read the background banks on the same addresses.
        for (slot, want) in [4u8, 5, 6, 7, 4, 5, 6, 7].iter().enumerate() {
            assert_eq!(
                cart.read_vram(
                    slot as u16 * 0x400 + 0x3ff,
                    &chr_render(PpuFetchKind::Background, true)
                ),
                *want,
                "background slot {slot}"
            );
        }
        // CPU reads (the `$2007` path) see the last-written (background) set.
        for (slot, want) in [4u8, 5, 6, 7, 4, 5, 6, 7].iter().enumerate() {
            assert_eq!(
                cart.read_vram(
                    slot as u16 * 0x400 + 0x3ff,
                    &chr_render(PpuFetchKind::Cpu, true)
                ),
                *want,
                "cpu slot {slot}"
            );
        }
    }
}
