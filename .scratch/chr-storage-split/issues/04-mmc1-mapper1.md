Status: ready-for-agent

# S4: MMC1 (Mapper 1)

## Parent

PRD: `.scratch/chr-storage-split/PRD.md`

## What to build

Add a companion `Mmc1ChrStorage` for mapper 1 (MMC1). MMC1 has two CHR bank registers (bank0 at $A000, bank1 at $C000) written via a 5-bit serial shift register protocol, and a control register ($8000) with a `chr_in_4k` bit that selects between 2 × 4 KB and 1 × 8 KB banking modes.

**Changes:**

1. **`Mmc1ChrStorage`**: New struct holding a source `Vec<u8>`, two bank register values (`chr_bank0`, `chr_bank1`), a `chr_in_4k` flag, and the 8 KB window cache. Implements `ChrStorage`:
   - `write_register(addr, value)`: receives the **already-decoded** register value (the Cartridge handles the serial shift protocol and emits the final 5-bit value). Decodes the address to determine which register was written ($8000 = control, $A000 = chr_bank0, $C000 = chr_bank1, $E000 = PRG — ignored). Updates internal state and recomputes the 8 KB window via `copy_from_source`.
   - `read_chr(addr)`: reads from the 8 KB window (cache).
   - `write_chr(addr, value)`: write-through to source at the correct banking offset, computed via the same `chr_index` logic as the current MMC1.
   - `has_chr_ram` gating: if the source was allocated as RAM (empty `chr_rom`), writes are permitted; otherwise writes are ignored.
2. **`MMC1`**: Constructor no longer takes `chr_rom`. Remove internal `chr_storage`, `has_chr_ram`, `refresh_chr_window`, `chr_index`, `chr_bank_base`, `chr_bank_count_4k`, `chr_bank_size`, `select_chr_bank0/1`, `read_chr`, `write_chr`. Keep `chr_bank0`, `chr_bank1`, `control` only for `uses_outer_prg_bank()` which checks `chr_bank0` bit 4 for PRG banking.
3. **Factory**: For mapper 1, creates `Mmc1ChrStorage`.
4. **Tests**: `mmc1_chr_storage` test module: bank switching in both 4K and 8K modes, CHR RAM write-through, control register handling. MMC1 mapper tests: remove CHR tests, keep PRG/mirroring tests.

## Acceptance criteria

- [ ] `Mmc1ChrStorage` exists, implements `ChrStorage`, overrides `write_register`
- [ ] Register writes produce correct CHR data in both 4K and 8K modes
- [ ] CHR RAM writes work correctly
- [ ] `MMC1` constructor no longer takes `chr_rom`; CHR fields removed
- [ ] Factory creates `Mmc1ChrStorage` for mapper 1
- [ ] `cargo test` passes
- [ ] MMC1 games (Zelda, Megaman 2) render correctly

## Blocked by

- #01: Infrastructure + non-banked mappers
