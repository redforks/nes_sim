# Save point: end of the first CPU cycle at or after scanline 261

## Status

Superseded by [ADR 0024](0024-save-point-at-vblank-entry.md). The VBlank-entry boundary is restored. The text below records the superseded decision.

## Decision

`process_frame()` returns at the end of the first CPU cycle at or after the
PPU reaches scanline 261. The PPU dot there is 0, 1, or 2, because one CPU
cycle is three PPU dots (`nes_core/src/nes_machine.rs` `tick()` calls
`tick_ppu` three times). The stored dot is validated on load to be in
`0..=2` with scanline 261.

`process_frame()` runs in two phases. Phase 1 ticks until the scanline leaves
261 (the wrap to 0). Phase 2 ticks until the scanline returns to 261, then
returns. The check reads the existing `Timing` scanline accessor. No new PPU
event flag is added.

Phase 1 is needed because `Timing::new()` starts at scanline 261 (ADR 0003),
and a restored state also starts in 261. A single check after each tick would
stop after one tick and return no frame.

## Considered options

- **Exact dot 0 save point.** Rejected. Stopping at dot 0 needs stepping
  inside a CPU cycle, which changes `tick()`, the per-dot contract in
  ADR 0010, and the interrupt sampling phase. The cost falls on the CPU
  path, and the save only needs a deterministic frame boundary.
- **New PPU event flag set at the 261 entry, like `vblank_entered`.**
  Rejected. The flag would need the same clearing care before save that
  `vblank_entered` needs. The scanline accessor already holds the state.
- **Keep the VBL-entry break (241.1).** Rejected by the map (#175). The
  saved frame then ends in the middle of vblank, which leaves the vblank
  phase outside the save.

## Consequences

- The save-point dot is part of the snapshot contract. Stored dot must be
  `0..=2`; a load outside that range is refused. Changing the rule changes
  the stored range and the format version.
- Each `process_frame()` call covers about 20 more scanlines of audio than
  before. Frame bytes are unchanged, because the palette flush still runs
  at 241.1.
- `tick_frames` and `take_vblank_entered` keep the VBL-entry break. They are
  not the save point.
- `nes_core/AGENTS.md` and `nescli/AGENTS.md` describe the VBL boundary and
  need updating when the refactor lands.
