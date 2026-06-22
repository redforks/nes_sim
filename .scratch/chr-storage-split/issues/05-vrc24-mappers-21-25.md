Status: ready-for-agent

# S5: VRC24 (Mappers 21-25)

## Parent

PRD: `.scratch/chr-storage-split/PRD.md`

## What to build

Add a companion `Vrc24ChrStorage` for mappers 21, 22, 23, 25 (VRC4 and VRC2 variants). These use 8 × 1 KB CHR banking with 9-bit bank registers (split across two $8000-$FFFF writes), and pin-variant address decoding depending on the specific board.

**Changes:**

1. **`Vrc24ChrStorage`**: New struct holding a source `Vec<u8>`, 8 × 9-bit bank register values (stored as 16 `u8`s for the low/high pairs), and the pin-variant's address decoding parameters (`s0`, `s1` bit positions). Implements `ChrStorage`:
   - Constructor takes `chr_rom` and `VrcVariant` (for pin config).
   - `write_register(addr, value)`: decodes the address using the variant's pin positions to determine which register and whether it's the low or high nibble, then updates the appropriate bank register and recomputes the 8 × 1 KB window via `copy_from_source`.
   - `read_chr(addr)`: reads from the 8 KB window.
   - `write_chr(addr, value)`: write-through to source with banking offset (if CHR-RAM).
   - `has_chr_ram` gating: if source was allocated as RAM, writes permitted.
2. **`Vrc24`**: Constructor no longer takes `chr_rom`. Remove internal `chr_storage`, `chr_bank` array, and all CHR-related logic. Keep PRG registers, mirroring, and IRQ logic.
3. **Factory**: For mappers 21, 22, 23, 25, creates `Vrc24ChrStorage` with the appropriate variant.
4. **Tests**: `vrc24_chr_storage` test module for each variant: bank switching, pin-variant address decoding, CHR RAM writes. VRC24 mapper tests: remove CHR tests, keep PRG/IRQ/mirroring tests.

## Acceptance criteria

- [ ] `Vrc24ChrStorage` exists, implements `ChrStorage`, handles all VRC4/VRC2 variants
- [ ] Pin-variant address decoding is correct for each variant
- [ ] `Vrc24` constructor no longer takes `chr_rom`; CHR fields removed
- [ ] Factory creates `Vrc24ChrStorage` for mappers 21, 22, 23, 25
- [ ] `cargo test` passes
- [ ] VRC24 games (if available) render correctly

## Blocked by

- #01: Infrastructure + non-banked mappers
