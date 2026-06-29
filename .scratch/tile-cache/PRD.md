# TileCache for background rendering

Cache the four bytes relevant to a single background tile row — nametable byte, attribute byte, and both bitplane rows — to avoid per-pixel `cartridge.read_chr()` calls during background rendering. This matches the real PPU's tile-fetch cadence where mapper side effects fire once per tile, not once per pixel.

See [ADR-0002](../../docs/adr/0002-tile-cache.md) and [CONTEXT.md](../../CONTEXT.md#language--ppu-background-rendering) for the full domain vocabulary.

## Design decisions (from grilling session)

- `TileCache` is a separate struct, stored as `Option<TileCache>` on `Ppu`
- Holds 4 bytes: NT byte, AT byte, low bitplane, high bitplane
- Serves 8 pixels per fill, refills unconditionally on tile boundary
- First batch shorter by `fine_x` to handle sub-tile scroll offset
- Invalidated eagerly at dot 0, on `$2000` writes, and on `$2006` writes
- Background only — sprites stay per-pixel

## Slices

1. **Extract tile fetch** — prefactor to separate coordinate computation + data fetch from pixel decoding
2. **TileCache integration** — struct, Ppu field, invalidation, batch-serving in `get_background_pixel`
