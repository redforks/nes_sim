# TileCache: once-per-tile pattern fetch for background rendering

`get_background_pixel()` previously called `cartridge.read_chr()` on every pixel — two dynamic dispatches per pixel across 256 columns. This meant mapper `read_chr` side effects fired 256 times per scanline instead of the hardware-accurate ~34 times (one fetch per tile).

We decided to cache the four bytes relevant to a single tile row in a `TileCache` struct on `Ppu`: nametable byte (tile index), attribute byte (palette select), and both bitplane rows (low, high) at the current fine_y. The cache is filled once per tile via `read_chr` and `nametable.read`, then consumed for up to 8 horizontal pixels before unconditional refill. The first tile batch is shorter by `fine_x` to handle sub-tile scroll offsets. The cache is invalidated at dot 0 and on scroll/ctrl register writes.

This matches the real PPU's tile-fetch cadence: the mapper sees one pattern-table access per tile, not per pixel.

**Rejected alternatives:**
- Full 2-tile shift register pipeline: models the real PPU's 16-pixel lookahead with shift-register bit rotation. More hardware-accurate for pipeline latency but adds complexity without improving mapper-side-effect timing — the 1-tile cache already achieves the correctness dimension that matters.
- Per-pixel `read_chr` (status quo): mapper side effects fire at the wrong frequency. Also slow.
- Cache only bitplane bytes without nametable/attribute: still saves `read_chr` calls but leaves nametable reads on the hot path.

**Consequences:**
- Background rendering no longer hits `dyn Cartridge::read_chr` per pixel. Mapper side effects fire once per tile, matching hardware.
- `TileCache` is scoped to background only; the sprite path now has its own row cache (follow-up below).
- **Discharged follow-up (spec #38, ticket #44):** sprite pattern reads no longer stay per-pixel. `SpriteManager` owns a `SpriteRow` cache parallel to the Secondary OAM buffer, filled lazily on the first pixel request of a scanline: each evaluated Sprite resolves its vertical source row (applying vertical flip and 8x16 height) and reads both pattern planes once. The per-pixel path then decodes from the cached planes, and the sprite-0 hit reads the cached row of the evaluated zero Sprite. Cartridge pattern reads drop from up to 8 Sprites x 2 planes x 256 pixels to at most 8 Sprites x 2 planes per scanline, matching the hardware sprite-fetch window (dots 257-320). The row cache carries the same invalidation fidelity as `TileCache`: the dot-0 Secondary OAM swap, `$2000` control writes, PPUDATA writes below the `$2000` pattern-table boundary, and reset. A mapper CHR bank switch during a visible scanline therefore becomes visible on the next scanline, as on hardware.
- Invalidated by `$2000` writes (pattern bank change), `$2006` writes (scroll change), and dot 0 (new scanline). Invalidated eagerly in `tick()` register-write paths.
- `get_background_pixel` switches from "compute addresses from vram_addr per pixel" to "serve from cache, refill on tile boundary."
