# 02 — SpriteManager Typed Interface + read_sprite_color Refactor

**Status:** `ready-for-agent`

## Parent

[PRD: OAM Module Restructure](../PRD.md)

## What to build

Convert `SpriteManager` methods from raw byte-array access to typed `Sprite` field access. Replace extracted bool parameters with `PpuCtrl` + `PpuMask` structs. Refactor `read_sprite_color` to accept `TilePosition` instead of duplicated tile-addressing logic.

- Method signatures change: `(oam_data: &[u8; 256], sprite_size_16: bool, sprite_pattern_table: bool, sprite_left_enabled: bool)` → `(oam: &Oam, ctrl: PpuCtrl, mask: PpuMask)`
- All field access uses typed `Sprite`: `sprite.y`, `sprite.x`, `sprite.tile_idx`, `sprite.attributes.palette()`, `sprite.attributes.flip_vertically()`, `sprite.attributes.flip_horizontally()`, `sprite.attributes.behind_background()`
- Iteration: `for sprite in &oam.sprites` instead of `for sprite_idx in 0..64` with `* 4` offset arithmetic. Keep the 8-sprites-per-scanline limit inline.
- Extract shared `evaluate_sprite(sprite, ctrl, cartridge, screen_x, screen_y, read_fn) -> Option<SpritePixel>` from `find_sprite_pixel` and `sprite_zero_opaque_at`
- `step_sprite_overflow_eval` uses typed sprite fields for Y comparison; keep the byte-level state machine modes (CopySprite, OverflowSearch) intact
- `read_sprite_color` accepts `TilePosition` from `sprite.tile_position(ctrl)` instead of `(tile_idx, src_y, sprite_size_16, sprite_pattern_table)` booleans
- Ppu call sites in `render_pixel` and overflow eval pass `&self.registers.oam`, `self.registers.ctrl`, `self.effective_mask`

## Acceptance criteria

- [ ] `SpriteManager` no longer imports or references `registers::Registers` (receives `&Oam` + `PpuCtrl` + `PpuMask` directly)
- [ ] No raw byte-offset arithmetic (`* 4`, `+ 1`, `+ 2`, `+ 3`) remains in `sprite.rs`
- [ ] No bit-masking of attributes (`& 0x80`, `& 0x40`, `& 0x20`, `& 0x03`) remains in `sprite.rs`
- [ ] `read_sprite_color` accepts `TilePosition` and no longer duplicates tile-addressing logic already in `Sprite::tile_position()`
- [ ] All sprite rendering tests pass: pixel color output, sprite zero-hit, sprite overflow, 8x16 tile selection, left-column clipping, sprite priority ordering
- [ ] All 546 existing unit tests pass

## Blocked by

- `01-registers-oam-type-swap`
