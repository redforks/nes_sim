//! Cartridge memory: a pool of 1 KB banks plus the two bank windows that map
//! them into the CPU and PPU address spaces. The mapper only rewrites window
//! slots; every access is one table lookup. See `docs/adr/0015`.

use super::Mirroring;

/// The unit of switchable cartridge memory.
pub const BANK_SIZE: usize = 0x400;
/// Physical nametable banks (CIRAM): `$2000`, `$2400`, `$2800`, `$2C00`.
const NAMETABLE_BANK_COUNT: usize = 4;
/// PRG-RAM banks: 8 × 1 KB, always at `$6000-$7FFF` when present.
const PRG_RAM_BANK_COUNT: usize = 8;
/// Slots covering `$0000-$3FFF`: 8 pattern-table slots + 4 nametable slots,
/// plus the four `$3000-$3EFF` alias slots. The window is a power of two so
/// any 14-bit PPU address yields a provably in-range index with no fold and no
/// bounds check; the alias slots hold the same banks as the slots they repeat.
const PPU_WINDOW_SLOTS: usize = 16;
/// Slots covering `$8000-$FFFF`: 32 PRG-ROM slots. The window is a power of
/// two so the slot index can be masked into range for free. PRG RAM is not
/// part of this window; it lives in its own bank array.
const PRG_WINDOW_SLOTS: usize = 32;
/// First nametable slot inside the PPU window.
const NAMETABLE_SLOT_BASE: usize = 8;
/// First `$3000-$3EFF` alias slot; each repeats `NAMETABLE_SLOT_BASE + i`.
const NAMETABLE_ALIAS_SLOT_BASE: usize = 12;

/// 1 KB of cartridge memory.
pub struct Bank([u8; BANK_SIZE]);

impl Bank {
    fn empty() -> Self {
        Self([0; BANK_SIZE])
    }

    fn fill_from(&mut self, data: &[u8]) {
        let len = data.len().min(BANK_SIZE);
        self.0[..len].copy_from_slice(&data[..len]);
    }

    fn read(&self, address: u16) -> u8 {
        self.0[(address & 0x03ff) as usize]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.0[(address & 0x03ff) as usize] = value;
    }
}

fn bank_count(len_bytes: usize) -> usize {
    len_bytes.div_ceil(BANK_SIZE).max(1)
}

/// The cartridge's memory: one pool of banks, laid out as
/// `[4 nametables][N CHR][M PRG-ROM]`, plus the two bank windows, and a
/// separate PRG-RAM bank array (8 × 1 KB) for `$6000-$7FFF`.
pub struct CartridgeMemory {
    pool: Box<[Bank]>,
    prg_ram: Box<[Bank; PRG_RAM_BANK_COUNT]>,
    /// MMC5 WRAM extension: first pool bank of the WRAM area appended after
    /// PRG-ROM by [`Self::add_wram_banks`]. Zero-sized for every other board.
    wram_base: usize,
    /// MMC5 WRAM size in 1 KB banks. Zero for every board but MMC5-with-WRAM;
    /// [`Self::read_prg_ram`]/[`Self::write_prg_ram`] only consult the WRAM
    /// area when this is nonzero, so other mappers keep the fixed 8 KB bank
    /// array with no behavior change.
    wram_bank_count: usize,
    /// `$6000-$7FFF` bank offset (in 1 KB banks) into the WRAM area, selected
    /// by MMC5 `$5113`. Only consulted when `wram_bank_count` is nonzero.
    wram_6000_base: usize,
    chr_base: usize,
    prg_rom_base: usize,
    chr_bank_count: usize,
    prg_bank_count: usize,
    chr_is_ram: bool,
    prg_ram_enabled: bool,
    mirroring: Mirroring,
    chr_len_bytes: usize,
    prg_len_bytes: usize,
    ppu_window: [u16; PPU_WINDOW_SLOTS],
    prg_window: [u16; PRG_WINDOW_SLOTS],
}

impl CartridgeMemory {
    /// Builds the pool from ROM images. An empty `chr_rom` means CHR RAM of
    /// `chr_ram_size` bytes (8 KB when the header does not say).
    pub fn from_rom(
        prg_rom: &[u8],
        chr_rom: &[u8],
        chr_ram_size: Option<usize>,
        mirroring: Mirroring,
    ) -> Self {
        let chr_is_ram = chr_rom.is_empty();
        let chr_len_bytes = if chr_is_ram {
            chr_ram_size.unwrap_or(0x2000)
        } else {
            chr_rom.len()
        };
        let chr_bank_count = bank_count(chr_len_bytes);
        let prg_bank_count = bank_count(prg_rom.len());

        let chr_base = NAMETABLE_BANK_COUNT;
        let prg_rom_base = chr_base + chr_bank_count;

        let mut memory = Self {
            pool: (0..prg_rom_base + prg_bank_count)
                .map(|_| Bank::empty())
                .collect(),
            prg_ram: Box::new(std::array::from_fn(|_| Bank::empty())),
            chr_base,
            prg_rom_base,
            chr_bank_count,
            prg_bank_count,
            wram_base: 0,
            wram_bank_count: 0,
            wram_6000_base: 0,
            chr_is_ram,
            prg_ram_enabled: true,
            mirroring,
            chr_len_bytes,
            prg_len_bytes: prg_rom.len(),
            ppu_window: [0; PPU_WINDOW_SLOTS],
            prg_window: [0; PRG_WINDOW_SLOTS],
        };

        for (i, chunk) in chr_rom.chunks(BANK_SIZE).enumerate() {
            memory.pool[chr_base + i].fill_from(chunk);
        }
        for (i, chunk) in prg_rom.chunks(BANK_SIZE).enumerate() {
            memory.pool[prg_rom_base + i].fill_from(chunk);
        }

        memory.apply_mirroring(mirroring);
        memory.map_chr_8k(0);
        memory.map_prg_32k(0);
        memory
    }

    /// Zeroed 32 KB PRG ROM, 8 KB CHR RAM, horizontal mirroring.
    #[cfg(test)]
    pub fn test(prg_rom: &[u8], chr_rom: &[u8]) -> Self {
        let prg = if prg_rom.is_empty() {
            vec![0; 0x8000]
        } else {
            prg_rom.to_vec()
        };
        Self::from_rom(&prg, chr_rom, None, Mirroring::Horizontal)
    }

    /// PRG-ROM access for `$8000-$FFFF`. The slot index is masked rather
    /// than bounds-checked: the subtraction wraps for addresses below `$8000`
    /// (which `Cartridge::read` never passes) and the mask maps the result
    /// into the window.
    pub fn read_prg(&self, address: u16) -> u8 {
        let slot = (address.wrapping_sub(0x8000) >> 10) as usize & (PRG_WINDOW_SLOTS - 1);
        let bank = self.prg_window[slot] as usize;
        debug_assert!(bank < self.pool.len(), "PRG slot {slot} escapes the pool");
        // SAFETY: `prg_window` is private and written only by `map_prg_run`
        // below, which adds `prg_rom_base` to a bank taken modulo
        // `prg_bank_count` — every entry is inside the pool.
        unsafe { self.pool.get_unchecked(bank).read(address) }
    }

    /// PRG-RAM access for `$6000-$7FFF`: direct bank hit, no window lookup.
    ///
    /// Boards with MMC5 WRAM (see [`Self::add_wram_banks`]) instead read the
    /// 8 KB WRAM bank selected by `$5113` (via [`Self::set_wram_6000_bank`]).
    pub fn read_prg_ram(&self, address: u16) -> u8 {
        let bank =
            ((address.wrapping_sub(0x6000) & 0x1fff) >> 10) as usize & (PRG_RAM_BANK_COUNT - 1);
        if self.wram_bank_count > 0 {
            // SAFETY: `wram_6000_base + bank` is reduced modulo the nonzero
            // `wram_bank_count`, so the pool index stays inside the WRAM area
            // appended by `add_wram_banks`.
            let wram_bank = (self.wram_6000_base + bank) % self.wram_bank_count;
            return unsafe {
                self.pool
                    .get_unchecked(self.wram_base + wram_bank)
                    .read(address)
            };
        }
        // SAFETY: `prg_ram` has exactly PRG_RAM_BANK_COUNT banks and the masked
        // index is always in range.
        unsafe { self.prg_ram.get_unchecked(bank).read(address) }
    }

    /// PRG-RAM writes for `$6000-$7FFF`: direct bank hit, no window lookup.
    /// Boards with MMC5 WRAM write the `$5113`-selected WRAM bank instead.
    pub fn write_prg_ram(&mut self, address: u16, value: u8) {
        let bank =
            ((address.wrapping_sub(0x6000) & 0x1fff) >> 10) as usize & (PRG_RAM_BANK_COUNT - 1);
        if self.wram_bank_count > 0 {
            // SAFETY: as in `read_prg_ram`.
            let wram_bank = (self.wram_6000_base + bank) % self.wram_bank_count;
            return unsafe {
                self.pool
                    .get_unchecked_mut(self.wram_base + wram_bank)
                    .write(address, value);
            };
        }
        // SAFETY: as in `read_prg_ram`.
        unsafe {
            self.prg_ram.get_unchecked_mut(bank).write(address, value);
        }
    }

    /// Appends MMC5 WRAM to the pool (called once by `MMC5::new`):
    /// `size_bytes` rounds up to whole 1 KB banks. Boards without MMC5 WRAM
    /// never call this, so every other mapper keeps its existing layout.
    pub fn add_wram_banks(&mut self, size_bytes: usize) {
        let banks = size_bytes.div_ceil(BANK_SIZE);
        if banks == 0 {
            return;
        }
        self.wram_base = self.pool.len();
        self.wram_bank_count = banks;
        let pool = std::mem::replace(&mut self.pool, Box::new([]));
        let mut pool = pool.into_vec();
        pool.extend((0..banks).map(|_| Bank::empty()));
        self.pool = pool.into_boxed_slice();
    }

    /// Points the `$6000-$7FFF` window at an 8 KB WRAM run — a run of 8
    /// consecutive 1 KB banks (GLOSSARY Bank; MMC5 `$5113`). Bit 7 of the
    /// register is the caller's to mask: `$5113` always selects RAM.
    /// No-op without WRAM (ELROM): `$6000` stays disabled via
    /// [`Self::set_prg_ram_enabled`].
    pub fn set_wram_6000_run(&mut self, run_8k: u16) {
        if self.wram_bank_count == 0 {
            return;
        }
        self.wram_6000_base = (run_8k as usize * 8) % self.wram_bank_count;
    }

    /// Points an 8 KB `$8000`-window slot (0 at `$8000` .. 3 at `$E000`) at an
    /// 8 KB WRAM run: the MMC5 ROM/RAM-select bit cleared on `$5114-$5116`.
    /// No-op without WRAM: the window keeps its previous mapping.
    pub fn map_prg_wram_run(&mut self, slot: u16, run_8k: u16) {
        if self.wram_bank_count == 0 {
            return;
        }
        self.map_wram_run(slot as usize * 8, run_8k as usize * 8, 8);
    }

    /// Data write through the `$8000-$FFFF` bank window: only affects windows
    /// currently mapped at MMC5 WRAM (Bandit Kings writes back through
    /// `$8000+`). ROM-mapped windows ignore the write.
    pub fn write_prg(&mut self, address: u16, value: u8) {
        let slot = (address.wrapping_sub(0x8000) >> 10) as usize & (PRG_WINDOW_SLOTS - 1);
        let bank = self.prg_window[slot] as usize;
        if bank < self.wram_base || bank >= self.wram_base + self.wram_bank_count {
            return;
        }
        // SAFETY: bounds-checked against the WRAM area just above.
        unsafe {
            self.pool.get_unchecked_mut(bank).write(address, value);
        }
    }

    fn map_wram_run(&mut self, first_slot: usize, start_bank: usize, len: usize) {
        for i in 0..len {
            let bank = (start_bank + i) % self.wram_bank_count;
            self.prg_window[first_slot + i] = (self.wram_base + bank) as u16;
        }
    }

    /// Cartridge PPU space for `$0000-$3EFF`. The slot index is masked, not
    /// bounds-checked: `& 0x3fff` drops the $4000 wrap and lands inside the
    /// 16-slot window, so the window lookup needs no `unsafe` at all.
    pub fn read_vram(&self, address: u16) -> u8 {
        let slot = ((address & 0x3fff) >> 10) as usize;
        let bank = self.ppu_window[slot] as usize;
        debug_assert!(bank < self.pool.len(), "PPU slot {slot} escapes the pool");
        // SAFETY: `ppu_window` is private and written only by
        // `apply_mirroring` / `map_chr_run` below, which write nametable bank
        // numbers 0..4 or `chr_base` plus a bank taken modulo
        // `chr_bank_count` — every entry is inside the pool.
        unsafe { self.pool.get_unchecked(bank).read(address) }
    }

    /// CHR bank-redirect read (ticket #78): serves pool bank `bank_1k`
    /// (modulo the CHR bank count, mirroring `map_chr_run`) at `address`.
    /// The low 10 bits select the byte, so this shares pool banks with the
    /// window reads/writes — CHR-RAM writes via windows stay visible here.
    pub fn read_chr_bank(&self, bank_1k: u16, address: u16) -> u8 {
        let bank = bank_1k as usize % self.chr_bank_count;
        // SAFETY: as in `read_vram` — the modulo keeps the pool index inside
        // the CHR area.
        unsafe { self.pool.get_unchecked(self.chr_base + bank).read(address) }
    }

    /// Pattern-table writes are dropped when the banks hold CHR ROM; the
    /// nametable banks are always writable.
    pub fn write_vram(&mut self, address: u16, value: u8) {
        if address < 0x2000 && !self.chr_is_ram {
            return;
        }
        let slot = ((address & 0x3fff) >> 10) as usize;
        let bank = self.ppu_window[slot] as usize;
        debug_assert!(bank < self.pool.len(), "PPU slot {slot} escapes the pool");
        // SAFETY: as in `read_vram`.
        unsafe {
            self.pool.get_unchecked_mut(bank).write(address, value);
        }
    }

    pub fn apply_mirroring(&mut self, mirroring: Mirroring) {
        self.mirroring = mirroring;
        for (i, bank) in mirroring.nt_slots().iter().enumerate() {
            self.ppu_window[NAMETABLE_SLOT_BASE + i] = *bank as u16;
            self.ppu_window[NAMETABLE_ALIAS_SLOT_BASE + i] = *bank as u16;
        }
    }

    /// Points one 1 KB pattern-table slot at a 1 KB CHR bank.
    pub fn map_chr_1k(&mut self, slot: u16, bank: u16) {
        self.map_chr_run(slot as usize, bank as usize, 1);
    }

    /// Points a 4 KB pattern-table half (slot 0 or 1) at a 4 KB CHR bank.
    pub fn map_chr_4k(&mut self, slot: u16, bank: u16) {
        self.map_chr_run(slot as usize * 4, bank as usize * 4, 4);
    }

    /// Points the whole 8 KB pattern table at an 8 KB CHR bank.
    pub fn map_chr_8k(&mut self, bank: u16) {
        self.map_chr_run(0, bank as usize * 8, 8);
    }

    /// Points an 8 KB PRG slot (0..=3 from `$8000`) at an 8 KB PRG bank.
    pub fn map_prg_8k(&mut self, slot: u16, bank: u16) {
        self.map_prg_run(slot as usize * 8, bank as usize * 8, 8);
    }

    /// Points a 16 KB PRG slot (0 at `$8000`, 1 at `$C000`) at a 16 KB bank.
    pub fn map_prg_16k(&mut self, slot: u16, bank: u16) {
        self.map_prg_run(slot as usize * 16, bank as usize * 16, 16);
    }

    /// Points the whole 32 KB PRG window at a 32 KB bank.
    pub fn map_prg_32k(&mut self, bank: u16) {
        self.map_prg_run(0, bank as usize * 32, 32);
    }

    fn map_chr_run(&mut self, first_slot: usize, start_bank: usize, len: usize) {
        for i in 0..len {
            let bank = (start_bank + i) % self.chr_bank_count;
            self.ppu_window[first_slot + i] = (self.chr_base + bank) as u16;
        }
    }

    fn map_prg_run(&mut self, first_slot: usize, start_bank: usize, len: usize) {
        for i in 0..len {
            let bank = (start_bank + i) % self.prg_bank_count;
            self.prg_window[first_slot + i] = (self.prg_rom_base + bank) as u16;
        }
    }

    pub fn mirroring(&self) -> Mirroring {
        self.mirroring
    }

    pub fn chr_is_ram(&self) -> bool {
        self.chr_is_ram
    }

    pub fn prg_ram_enabled(&self) -> bool {
        self.prg_ram_enabled
    }

    pub fn set_prg_ram_enabled(&mut self, enabled: bool) {
        self.prg_ram_enabled = enabled;
    }

    pub fn chr_len_bytes(&self) -> usize {
        self.chr_len_bytes
    }

    pub fn prg_len_bytes(&self) -> usize {
        self.prg_len_bytes
    }

    /// CHR banks in 1 KB units (never zero).
    pub fn chr_bank_count(&self) -> usize {
        self.chr_bank_count
    }

    /// PRG banks in 1 KB units (never zero).
    pub fn prg_bank_count(&self) -> usize {
        self.prg_bank_count
    }

    pub fn chr_bank_count_4k(&self) -> usize {
        (self.chr_len_bytes / 0x1000).max(1)
    }

    pub fn chr_bank_count_8k(&self) -> usize {
        (self.chr_len_bytes / 0x2000).max(1)
    }

    pub fn prg_bank_count_8k(&self) -> usize {
        (self.prg_len_bytes / 0x2000).max(1)
    }

    pub fn prg_bank_count_16k(&self) -> usize {
        (self.prg_len_bytes / 0x4000).max(1)
    }

    pub fn prg_bank_count_32k(&self) -> usize {
        (self.prg_len_bytes / 0x8000).max(1)
    }

    /// MMC5 WRAM size in 8 KB runs (a run is 8 consecutive 1 KB banks;
    /// zero for every board without MMC5 WRAM).
    pub fn wram_8k_run_count(&self) -> usize {
        self.wram_bank_count / 8
    }
}
