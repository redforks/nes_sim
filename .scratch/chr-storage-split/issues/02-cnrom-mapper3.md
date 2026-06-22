Status: ready-for-agent

# S2: CNROM (Mapper 3)

## Parent

PRD: `.scratch/chr-storage-split/PRD.md`

## What to build

Add a companion `CnromChrStorage` for mapper 3 (CNROM). CNROM selects one 8 KB CHR bank via writes to any address in $8000-$FFFF. The ChrStorage wraps a `WindowedChr` (source pool + cached window) and overrides `write_register` to refresh the window when the bank select register is written.

**Changes:**

1. **`CnromChrStorage`**: New struct holding a `WindowedChr` (source + cache). Constructor takes `chr_rom` slice. `read_chr`/`write_chr` delegate to the inner `WindowedChr`. `write_register` decodes writes: extracts a bank number from `value` modulo bank count, computes the source offset (`bank * 0x2000`), and calls `refresh()` on the inner `WindowedChr`.
2. **`Mapper3`**: Constructor no longer takes `chr_rom`. Remove internal `chr_storage`, `selected_chr_bank`, `chr_bank_count`, `refresh_chr_window`. Remove `read_chr`/`write_chr` impls. The `write` method no longer calls `refresh_chr_window` — that responsibility moves to `CnromChrStorage::write_register`.
3. **Factory**: For mapper 3, creates `CnromChrStorage` instead of DirectChr (or stub).
4. **Tests**: New `cnrom_chr_storage` test module: bank selection produces correct data, bank wrapping, writes ignored for ROM (protected by `has_chr_ram` logic in the storage). Mapper 3 tests: remove CHR-related tests, keep PRG/ram tests.

## Acceptance criteria

- [ ] `CnromChrStorage` exists, implements `ChrStorage`, overrides `write_register`
- [ ] `CnromChrStorage::write_register` with a bank number causes subsequent `read_chr` to return the correct bank's data
- [ ] `Mapper3` constructor no longer takes `chr_rom`; CHR fields removed
- [ ] Factory creates `CnromChrStorage` for mapper 3
- [ ] `cargo test` passes
- [ ] CNROM games (e.g., Arkista's Ring) render correctly

## Blocked by

- #01: Infrastructure + non-banked mappers
