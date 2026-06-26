# 01 — Registers OAM Type Swap

**Status:** `ready-for-agent`

## Parent

[PRD: OAM Module Restructure](../PRD.md)

## What to build

Replace the raw `oam_data: [u8; 0x100]` field in `Registers` with the typed `oam: Oam` struct. This is a pure prefactor — the only code that changes is inside `Registers` and its call sites. `SpriteManager` still receives raw `&[u8; 256]` for now (call sites pass `registers.oam.as_bytes()` as a bridge).

- `registers.oam_data` becomes `registers.oam: Oam`
- `read_oam_data()` reads `oam.as_bytes()[(oam_addr as usize) & 0xFF]`
- `write_oam_data()` writes through `oam.as_bytes_mut()[(addr as usize) & 0xFF]` with `normalize_oam_byte` mask intact
- `Registers::new()` and `Registers::reset()` initialize `oam` with zeroed sprites (derive or implement `Default` on `Oam` if not already present)
- Call sites that pass `&self.registers.oam_data` now pass `self.registers.oam.as_bytes()`
- Test helpers (`setup_sprite`, `create_test_ppu_with_mask`) use `oam.as_bytes_mut()` for byte-level setup

## Acceptance criteria

- [ ] `Registers` stores `oam: Oam` instead of `oam_data: [u8; 0x100]`
- [ ] OAM register read via $2004 returns correct byte at current `oam_addr`
- [ ] OAM register write via $2004 stores byte with 0xE3 attribute masking applied
- [ ] OAM DMA write path (`NesMcu` → `write_oam_dma_byte`) works unchanged
- [ ] All 546 existing unit tests pass

## Blocked by

None — can start immediately
