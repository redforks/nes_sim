# PRD: OAM Module Restructure

**Status:** `ready-for-agent`

## Problem Statement

The PPU's OAM (Object Attribute Memory) is currently stored as a raw `[u8; 0x100]` byte array in `Registers`. All sprite evaluation, rendering, and overflow logic in `SpriteManager` accesses this data through manual byte-offset arithmetic (`sprite_idx * 4`, `byte_idx + 2`, bit-masking attributes with `& 0x80`, etc.). The `oam.rs` module was introduced with typed `Sprite`, `Attribute`, `Oam`, `TilePosition`, and `PatternBank` structs, but the rest of the PPU codebase doesn't use them. This duplication makes the rendering code harder to read, verify against hardware documentation, and extend with new sprite features.

## Solution

Integrate the typed `Oam` module throughout `Registers`, `SpriteManager`, and `Ppu` call sites. Replace raw byte access with typed `Sprite` field access everywhere sprite evaluation logic runs. This eliminates magic offsets and bit-masking, making the code self-documenting against the NES OAM hardware specification.

## User Stories

1. As a developer reading sprite rendering code, I want `sprite.attributes.flip_vertically()` instead of `(attributes & 0x80) != 0`, so that I can understand sprite behavior without memorizing bit positions.
2. As a developer debugging sprite overflow, I want `sprite.y` instead of `oam_data[oam_index * 4]`, so that I can see which sprite field is being compared to the scanline.
3. As a developer tracing sprite zero-hit logic, I want `for sprite in &oam.sprites` instead of `for sprite_idx in 0..64` with `* 4` offset arithmetic, so that the iteration corresponds directly to the NES sprite priority order.
4. As a developer writing PPU tests, I want to set up OAM state by writing typed `Sprite` structs with named fields, so that test setup is clear about which sprite properties are being configured.
5. As a developer working with 8x16 sprites, I want `sprite.tile_position(ctrl)` to decode pattern bank and tile index in one call, so that the tile-addressing rules are centralized in one place instead of duplicated across rendering functions.
6. As a developer reading `Registers`, I want `oam: Oam` alongside `ctrl: PpuCtrl` and `mask: PpuMask`, so that all PPU register state has consistent typed representation.
7. As a developer maintaining the OAM DMA path, I want `Oam::as_bytes()` to bridge between byte-level DMA writes and typed sprite access, so that DMA correctness is maintained while rendering code uses typed fields.
8. As a developer reviewing sprite evaluation, I want `SpriteManager` methods to accept `PpuCtrl` + `PpuMask` structs instead of extracted bools, so that call sites don't manually destructure register fields.
9. As a developer new to the codebase, I want `Oam`, `Sprite`, `Attribute`, `TilePosition`, and `PatternBank` defined in the project glossary, so that I can build a mental model of the PPU domain before reading code.

## Implementation Decisions

### Domain Model

- **OAM** is a collection of 64 Sprites (256 bytes). It provides `as_bytes()`/`as_bytes_mut()` for byte-level I/O paths (register access, DMA) and `pub sprites: [Sprite; 64]` for typed field access by rendering logic.
- **Sprite** is a 4-byte OAM entry: Y position, tile index, Attribute flags, X position. Its `tile_position(ctrl)` method decodes the tile location based on sprite size (8x8 or 8x16) and pattern table selection.
- **Attribute** is a bitfield representing sprite attribute flags: palette index, behind-background priority, horizontal flip, vertical flip.
- **TilePosition** is an enum representing the decoded tile location (pattern bank + tile index) for both 8x8 and 8x16 sprite sizes.
- **PatternBank** is an enum (First at $0000, Second at $1000) with `start_addr()` returning the VRAM base address.
- `oam_addr` remains on `Registers` as a PPU I/O register — it is not part of the `Oam` data structure.

### Registers Changes

- `registers.oam_data: [u8; 0x100]` replaced by `registers.oam: Oam`.
- `read_oam_data()` and `write_oam_data()` access `oam.as_bytes()`/`oam.as_bytes_mut()` at `oam_addr & 0xFF`.
- `normalize_oam_byte` (the 0xE3 attribute mask applied when writing via $2004) stays inline at the register-write level. The mask belongs to the register-access path (not OAM DMA), not to `Attribute` itself.
- `Registers::new()` and `Registers::reset()` initialize `oam` with zeroed sprites (via `Oam`'s `Default` or constructor).

### SpriteManager Changes

- Method signatures change from `(oam_data: &[u8; 256], sprite_size_16: bool, sprite_pattern_table: bool)` to `(oam: &Oam, ctrl: PpuCtrl, mask: PpuMask)`.
- Field access uses typed `Sprite` throughout:
  - `sprite.y` instead of `oam_data[byte_idx]`
  - `sprite.attributes.flip_vertically()` instead of `(attributes & 0x80) != 0`
  - `sprite.attributes.flip_horizontally()` instead of `(attributes & 0x40) != 0`
  - `sprite.attributes.behind_background()` instead of `(attributes & 0x20) != 0`
  - `sprite.attributes.palette()` instead of `attributes & 0x03`
- Iteration becomes `for sprite in &oam.sprites` with explicit up-to-8-sprite limit kept inline.
- `sprite_zero_opaque_at` and `find_sprite_pixel` share an extracted helper: `evaluate_sprite(sprite, ctrl, cartridge, screen_x, screen_y, read_fn) -> Option<SpritePixel>`.
- `step_sprite_overflow_eval` uses typed sprite fields for Y comparison, keeping the byte-level state machine modes (CopySprite, OverflowSearch) intact.
- `read_sprite_color` is refactored to accept `TilePosition` instead of `(tile_idx, src_y, sprite_size_16, sprite_pattern_table)` booleans. It becomes `TilePosition::read_color(cartridge, src_x, src_y, read_pattern_pixel_fn) -> u8` or a standalone function taking `TilePosition`. This eliminates the duplicated tile-addressing logic that already exists in `Sprite::tile_position()`.

### Ppu Call-Site Changes

- `render_pixel` and related methods pass `&self.registers.oam`, `self.registers.ctrl`, `self.effective_mask` to `SpriteManager`.
- `write_oam_dma_byte` delegates to `self.registers.write_oam_data()` unchanged.
- `read_oam_data` delegates to `self.registers.read_oam_data()` unchanged.

### Test Changes

- `setup_sprite` helper in `tests.rs` writes through `oam.as_bytes_mut()` to maintain byte-level hardware fidelity — tests should mirror how bytes arrive at OAM (DMA or $2004 writes).
- `create_test_ppu_with_mask` initializes `oam_data` via `as_bytes_mut()` instead of raw array indexing.
- All existing pixel-rendering and sprite-behavior tests continue to pass without semantic changes.

### Domain Documentation

Four new terms added to `CONTEXT.md`:
- **OAM (Object Attribute Memory)**: 256-byte memory holding 64 Sprites, accessed byte-wise via $2003/$2004 and DMA, typed via `Sprite` struct.
- **Sprite**: 4-byte OAM entry containing Y position, tile index, Attribute, X position.
- **TilePosition**: Decoded sprite tile location encoding size (8x8 or 8x16), PatternBank, and tile index.
- **PatternBank**: First ($0000) or second ($1000) pattern table in VRAM.

## Testing Decisions

### What makes a good test

Tests verify external sprite rendering behavior — pixel color output, sprite zero-hit flag, sprite overflow flag, priority ordering — not internal data structure layout. The refactor is a pure mechanical transformation; all existing tests must pass unchanged.

### Existing test seam

The highest seam is `Ppu::render_pixel()`, `Ppu::read()`, `Ppu::write()`, and `Ppu::tick()`. The 26 tests in `nes_core/src/nes/ppu/tests.rs` exercise sprite rendering, zero-hit detection, overflow evaluation, left-column clipping, 8x16 sprite tile selection, and sprite flipping. These tests cover the full sprite pipeline and serve as regression protection.

### New unit tests

- `Oam::as_bytes()` round-trip: write bytes, verify `as_bytes()` returns them; write via `as_bytes_mut()`, verify `Sprite` fields reflect the writes.
- `Attribute` bitfield layout: verify that byte-level representation matches NES hardware bit positions (palette at bits 0-1, behind-bg at bit 5, horizontal flip at bit 6, vertical flip at bit 7). Existing tests in `oam.rs` cover `tile_position()`; extend with attribute round-trip.

### Prior art

The codebase uses `test_case` for parameterized tests and constructs `Ppu` structs with test cartridges (`TestCartridge`). Existing sprite tests like `test_render_pixel_returns_sprite_color_when_background_disabled` demonstrate the pattern of setting up OAM via `setup_sprite`, running `render_pixel`, and asserting the output color index.

## Out of Scope

- Changing the OAM DMA path in `NesMcu` — it remains byte-level and continues to call `ppu.write_oam_dma_byte()`.
- Extracting a `visible_sprites_on_scanline()` iterator helper — keeping iteration inline in `find_sprite_pixel`.
- Regrouping `Registers` fields into sub-structs (e.g., `ScrollRegisters`).
- Making `sprites` field private or adding iterator methods — `pub sprites: [Sprite; 64]` stays.
- Changing the sprite evaluation timing model or overflow eval state machine.
- Adding `TilePosition::read_color()` as a method — the function remains standalone or stays in `sprite.rs`; the signature change to accept `TilePosition` is sufficient.

## Further Notes

- All 546 existing unit tests must continue to pass after the refactor.
- The `oam.rs` module already has tests for `tile_position()` with 8x8 and 8x16 sprite sizes — these serve as the foundation for the typed refactor and validate the `Sprite::tile_position()` logic matches the hardware behavior currently duplicated in `sprite.rs::read_sprite_color`.
