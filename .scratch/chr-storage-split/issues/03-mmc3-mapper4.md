Status: ready-for-agent

# S3: MMC3 (Mapper 4)

## Parent

PRD: `.scratch/chr-storage-split/PRD.md`

## What to build

Add a companion `Mmc3ChrStorage` for mapper 4 (MMC3). MMC3 has 8 CHR bank registers controlling 8 × 1 KB slots, with a CHR-mode bit (bit 7 of the bank select register at $8000 even) that remaps which bank register controls each slot.

**Changes:**

1. **`Mmc3ChrStorage`**: New struct holding a source `Vec<u8>` and 8 bank offsets (or the raw bank registers + mode to compute offsets on the fly). Implements `ChrStorage`:
   - `read_chr(addr)`: translates address through banking: `slot = addr / 0x400`, `offset = addr % 0x400`, read from `source[bank_offsets[slot] + offset]`.
   - `write_chr(addr, value)`: if CHR-RAM (source is writable), write-through at the banking-offset position.
   - `write_register(addr, value)`: decodes MMC3's register protocol independently.
     - `$8000` (even): saves `bank_select` (bits 0-2 = target, bit 7 = CHR mode).
     - `$8001` (odd): if target is 0-5, updates the corresponding bank register and recomputes the 8 bank offsets using the same formula as the current MMC3 `sync_chr_banks()`.
   - Only CHR-relevant registers are decoded; IRQ, PRG, and mirroring writes are ignored.
2. **`Mapper3` → `Mapper4`**: `MMC3` constructor no longer takes `chr_rom`. Remove internal `chr_storage`, `has_chr_ram`, `chr_offsets`, `sync_chr_banks`, `read_chr`, `write_chr`. Remove `chr_bank_count`, `normalize_chr_bank`. Keep `bank_select` and `bank_registers` only for PRG-relevant bits (bit 6 for PRG mode, registers 6-7).
3. **Factory**: For mapper 4, creates `Mmc3ChrStorage`.
4. **Tests**: Comprehensive `mmc3_chr_storage` test module:
   - Bank switching in both CHR modes (0 and 1)
   - Writes to CHR RAM write through to the correct source bank
   - Register writes outside CHR range ($A000-$FFFF) are ignored
   - Initial state matches the ROM data
   - MMC3 mapper tests: remove CHR tests, keep PRG/IRQ/mirroring tests.

## Acceptance criteria

- [ ] `Mmc3ChrStorage` exists, implements `ChrStorage`, overrides `write_register`
- [ ] Bank register writes via `write_register` produce correct CHR data for all 8 slots in both CHR modes
- [ ] CHR RAM writes go to the correct source bank position
- [ ] `MMC3` constructor no longer takes `chr_rom`; CHR fields removed
- [ ] Factory creates `Mmc3ChrStorage` for mapper 4
- [ ] `cargo test` passes
- [ ] MMC3 games (SMB3, Megaman) render correctly

## Blocked by

- #01: Infrastructure + non-banked mappers
