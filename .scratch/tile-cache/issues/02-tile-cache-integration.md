Status: ready-for-agent

## What to build

Implement `TileCache` and wire it into background rendering so pattern data is fetched once per tile (8 pixels) instead of once per pixel. This matches the real PPU's tile-fetch cadence — mapper side effects from `read_chr` fire at the hardware-accurate frequency.

Four parts:

1. **`TileCache` struct** — a `Copy` type holding four bytes: nametable byte (tile index), attribute byte (palette-select quadrant), and both bitplane rows (low plane, high plane) at the current fine_y. Has a fill method that accepts NT/AT/bitplane data, and a pixel-consume method that extracts a color index for a given sub-tile x (0..7).

2. **`Ppu` field** — `tile_cache: Option<TileCache>`, initialized to `None`.

3. **Invalidation** — clear `tile_cache` to `None` on:
   - Dot 0 of each visible scanline (new fine_y)
   - `$2000` write (pattern bank may change)
   - `$2006` write (scroll position may change)

4. **`get_background_pixel` rewrite** — use the tile-fetch helper from slice 1. When `tile_cache` is `None`, fill it by calling the helper for the tile-aligned position (not per-pixel). Serve 8 pixels from the cache, then refill unconditionally. The first batch on each scanline is shorter: `8 - fine_x` pixels (where fine_x is from `BackgroundActivation`). Subsequent batches are full 8 pixels.

Palette index is derived from the cached attribute byte and the tile's quadrant position — constant per tile, computed once on fill.

## Acceptance criteria

- [ ] `TileCache` struct defined with fill and pixel-extraction methods
- [ ] `tile_cache` field added to `Ppu`, cleared to `None` on dot 0, `$2000` writes, and `$2006` writes
- [ ] `get_background_pixel` fills cache once per tile batch (8 pixels, first batch shorter by fine_x) and serves pixels from it
- [ ] Background rendering no longer calls `cartridge.read_chr()` per pixel during visible scanlines
- [ ] All existing PPU unit tests pass
- [ ] `just cpu-test` and `just rom-test` pass
- [ ] Mapper `read_chr` side effects fire at once-per-tile cadence (matching hardware)

## Blocked by

- #01-extract-tile-fetch
