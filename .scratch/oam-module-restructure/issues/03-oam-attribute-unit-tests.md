# 03 — OAM/Attribute Unit Tests

**Status:** `ready-for-agent`

## Parent

[PRD: OAM Module Restructure](../PRD.md)

## What to build

Add unit tests for the `Oam` struct and `Attribute` bitfield to verify type-level representation matches NES hardware byte layout. Ensures the typed OAM abstraction is sound before and after future changes.

- `Oam::as_bytes()` round-trip: write raw bytes via `as_bytes_mut()`, verify `as_bytes()` returns them; write typed `Sprite` struct fields, verify `as_bytes()` reflects those writes
- `Attribute` bitfield layout: verify palette is at bits 0-1, behind-background priority at bit 5, horizontal flip at bit 6, vertical flip at bit 7; verify unused bits (2-4) read as 0 and are masked on construction
- `Oam` size and alignment: verify `size_of::<Oam>() == 256` and `size_of::<Sprite>() == 4`
- Existing `tile_position()` tests in `oam.rs` already cover 8x8 and 8x16 tile decoding — extend if gaps found

## Acceptance criteria

- [ ] `Oam::as_bytes()` / `as_bytes_mut()` round-trip test passes
- [ ] `Attribute` bitfield layout matches NES hardware: palette (0-1), __ (2-4), behind_bg (5), flip_h (6), flip_v (7)
- [ ] `size_of` assertions confirm `Oam` is 256 bytes and `Sprite` is 4 bytes
- [ ] All 546 existing unit tests pass
- [ ] Tests are in `oam.rs`'s `mod tests` block, following existing `test_case` patterns

## Blocked by

- `02-spritemanager-typed-interface`
