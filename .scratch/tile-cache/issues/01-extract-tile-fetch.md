Status: ready-for-agent

## What to build

Extract the world-coordinate computation and data fetch out of `get_background_pixel` into a standalone helper. Currently `get_background_pixel` does everything inline: decodes `vram_addr` into coarse/fine scroll coordinates, computes world_x/world_y, reads nametable (tile index + attribute), and calls `read_pattern_pixel` for the bitplane bytes — all per pixel.

Pull the coordinate math and the data fetch (NT byte, AT byte, low/high bitplanes) into a helper that returns the four bytes needed for a tile batch. `get_background_pixel` delegates to it with no caching — same `read_chr` calls per pixel, same behavior. The helper accepts the `BackgroundActivation` anchor and a pixel x-position.

This creates a clean seam where `TileCache` slots in during the next slice.

## Acceptance criteria

- [ ] A helper function/method fetches all 4 tile bytes (NT index, AT byte, bitplane low, bitplane high) for a given screen_x scroll position
- [ ] `get_background_pixel` delegates to the helper — no change in pixel output
- [ ] All existing PPU unit tests pass unchanged
- [ ] `just cpu-test` and `just rom-test` pass

## Blocked by

None — can start immediately
