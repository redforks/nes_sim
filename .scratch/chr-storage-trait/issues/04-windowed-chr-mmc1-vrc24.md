Status: ready-for-agent

# Slice 4: Convert MMC1 and VRC24 to WindowedChr

## What to build

Convert MMC1 and VRC24 to use `WindowedChr` instead of their inline CHR storage fields.

**MMC1** — Replace `chr: Vec<u8>`, `chr_window: [u8; CHR_WINDOW_SIZE]`, `has_chr_ram` with `chr_storage: WindowedChr`.
- `read_chr` delegates to `chr_storage.read_chr(addr)`
- `write_chr` gates on `has_chr_ram`, computes source offset via `chr_index(address)` + bank registers, calls `chr_storage.write_chr_with_source(addr, val, source_offset)`
- `refresh_chr_window()` calls `chr_storage.refresh(computed_offset)` based on `chr_bank0` / `chr_bank1` (2×4KB banks — source offset for bank0 is `bank0 * 0x1000`, for bank1 is `bank1 * 0x1000`; but MMC1 uses an 8KB window so a single `refresh(offset_into_source)` won't handle two separate 4KB banks within the same 8KB window. The mapper will need to either do two sequential 4KB refreshes, or the mapper copies from source Vec to cache itself.)

**Important design nuance**: MMC1's window isn't a single contiguous chunk of source — it's two 4KB banks (from `chr_bank0` and `chr_bank1`) concatenated into the 8KB window. This means `WindowedChr::refresh(offset)` (which copies a contiguous 8KB segment) doesn't directly apply. Two approaches:
1. MMC1 writes the window itself by indexing into `chr_storage.source_mut()` if we expose that
2. `WindowedChr` gains an inherent `refresh_slot(&mut self, slot: usize, bank_idx: usize, bank_size: usize)` method

This slice must decide and implement the approach for non-contiguous windows.

**VRC24** — Replace `chr_mem: Vec<u8>`, `current_chr: [u8; CHR_WINDOW_SIZE]`, `has_chr_ram` with `chr_storage: WindowedChr`.
- `read_chr` delegates to `chr_storage.read_chr(addr)`
- `write_chr` gates on `has_chr_ram`, computes source offset from the 8-slot bank mapping (including VRC4/VRC2 address-line decoding), calls `chr_storage.write_chr_with_source(addr, val, source_offset)`
- `refresh_chr_banks()` iterates 8 slots, computes each slot's source offset, and either calls `chr_storage` methods or directly copies to the cache

VRC24's window IS a contiguous copy from 8 sequential 1KB slots in source — so a single `chr_storage.refresh(0)` won't work unless all 8 banks happen to be contiguous in source. The mapper must refresh each slot individually.

## Acceptance criteria

- [ ] MMC1's inline CHR fields replaced by `chr_storage: WindowedChr`
- [ ] MMC1's `read_chr` delegates to chr_storage
- [ ] MMC1's `write_chr` gates on has_chr_ram, uses write-through with computed source offset
- [ ] MMC1's `refresh_chr_window` uses WindowedChr's window directly (either via an inherent method or direct window access)
- [ ] VRC24's inline CHR fields replaced by `chr_storage: WindowedChr`
- [ ] VRC24's `read_chr` delegates to chr_storage
- [ ] VRC24's `write_chr` gates on has_chr_ram, uses write-through with mapper-computed source offset
- [ ] VRC24's `refresh_chr_banks` uses WindowedChr window (slot-by-slot)
- [ ] All existing MMC1 unit tests pass
- [ ] All existing VRC24 unit tests pass
- [ ] All Cartridge-level tests pass
- [ ] `cargo test` is green

## Blocked by

- #02: WindowedChr + MMC3 (WindowedChr must exist)
