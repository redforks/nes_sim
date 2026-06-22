Status: ready-for-agent

# S6: Mapper 34 and Mapper 87

## Parent

PRD: `.scratch/chr-storage-split/PRD.md`

## What to build

Add companion ChrStorage implementations for mappers 34 and 87 (the two remaining special-case mappers).

**Mapper 34 (BxROM / Nina-001):**

`Mapper34ChrStorage` holds a `Vec<u8>` source. Two board modes:
- BxROM: flat CHR (8 KB window, no banking). `write_register` is no-op. `write_chr` permitted only if CHR-RAM.
- Nina-001: 2 × 4 KB banking via $7FFE/$7FFF register writes. `write_register` decodes these writes (address is in cartridge RAM range, so NesMcu routes it to ChrStorage) and updates two bank offsets.

`Mapper34` loses `chr`, `has_chr_ram`, `selected_chr_bank_0/1`, `read_chr`, `write_chr`. Constructor no longer takes `chr_rom`. The `Nina001` bank detection logic (checking `chr_rom.len() > 0x2000`) moves into `Mapper34ChrStorage`'s constructor.

**Mapper 87 (J87 - Jaleco):**

`J87ChrStorage` holds 4 × 8 KB band arrays (copied from `chr_rom`). `write_register` decodes $6000-$FFFF writes using `extract_band_selector_value` to select the active band. `read_chr` reads from the active band.

`MapperJ87` loses `chr_rom_bands`, `cur_chr_band`, `read_chr`, `write_chr`. No longer copies `chr_rom` on construction.

**Factory**: For mapper 34, creates `Mapper34ChrStorage`. For mapper 87, creates `J87ChrStorage`.

**Tests**: Separate test modules for each storage: bank switching, CHR RAM (Mapper34), band selection (J87). Mapper tests: remove CHR tests, keep PRG tests.

## Acceptance criteria

- [ ] `Mapper34ChrStorage` exists, handles both BxROM and Nina-001 modes
- [ ] `J87ChrStorage` exists, handles band switching
- [ ] Both mappers' constructors no longer take `chr_rom`; CHR fields removed
- [ ] Factory creates the correct ChrStorage for each
- [ ] `cargo test` passes
- [ ] All existing ROM tests pass (mapper 34 and 87 specific)

## Blocked by

- #01: Infrastructure + non-banked mappers
