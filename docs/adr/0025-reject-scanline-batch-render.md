# Reject per-scanline batch render of PPU pixels

## Status

accepted

## Context

`Ppu::tick_visible` calls `render_pixel` once per visible dot. The proposal was to render all 256 pixels of each visible scanline at dot 1, then re-render the rest of the line on a **Render-state change** (see `GLOSSARY.md`). Measurements on the warped SMB1 movie (2000 frames, `nescli run --no-throttle --headless`, `perf stat`, interleaved runs, Ryzen 7 9700X):

- Skipping `render_pixel` entirely: −35.7% wall. Invalid as a ceiling: sprite-0 hit never sets, so game flow changes.
- Flow-preserving skip (background fetch, sprite row fill, and sprite-0 check kept; per-pixel sprite scan and palette lookup skipped): −8.1% wall (2537 → 2331 ms median), −8.9% instructions.
- Batch prototype, no re-render, per-pixel `timing.dot` bookkeeping: −2.8% wall (2544 → 2473 ms), +2.2% instructions.
- Batch prototype, no re-render, explicit `x` and fetch phase: +1.4% wall (2526 → 2562 ms), +4.9% instructions.

## Decision

Do not build scanline batch render. The bar was a median wall-time reduction of at least 5%. The batch removes only per-dot dispatch overhead. It cannot remove the background fetch, sprite scan, or palette lookup, and the prototypes measured no gain.

## Considered Options

- **Full design**: batch at dot 1, snapshot pipeline state at each 8-pixel tile boundary, re-render from the nearest snapshot on a render-state change. Gate it by mapper capability so side-effect mappers stay per-dot (MMC5 counts render-enabled reads in `Cell`s). Differential test against the per-dot path. Rejected: more code and state than the measured ceiling justifies. Re-renders also repeat mapper reads.
- **Re-run the whole line on each change**: rejected, because it repeats every mapper read per change.

## Consequences

- Per-dot `render_pixel` stays the only render path.
- Remaining per-pixel work (sprite scan, palette lookup) is bounded at about 8% by the flow-preserving ablation. Any future work there needs its own measurement.
- The prototype also failed the existing test `palette_write_mid_frame_takes_effect_from_beam_position` and diverged from baseline frames at Jackal frame 102, as expected without mid-line re-render. Not investigated, since the design was rejected.
