# Save point: end of the CPU cycle containing the VBlank set dot

## Status

Accepted. Supersedes
[ADR 0022](0022-save-point-at-cpu-cycle-end-after-scanline-261.md).

## Decision

`process_frame()` returns at the end of the CPU cycle containing the PPU
VBlank set dot (scanline 241, dot 1). The returned PPU timing is scanline
241, dot 2, 3, or 4: a CPU cycle advances three PPU dots, and the set dot
may fall in any of that cycle's sub-dots.

`process_frame()` consumes `Ppu::take_vblank_entered()` before returning.
The event is therefore unset at the save point. The VBlank set dot also
consumes `suppress_vblank_pending`, whether it suppresses the `$2002` status
bit or not.

Snapshot format version 3 accepts only scanline 241 and dot 2..=4.
Version 2 is refused because its scanline-261 save-point rule changes the
meaning of the stored timing fields.

## Considered options

- **Keep the scanline-261 save point.** Rejected. It moves `process_frame()`
  away from the restored frame-output boundary and makes saves taken after
  a frame fail the snapshot loader unless callers emulate another partial
  frame.
- **Stop at exact VBlank set dot 241.1.** Rejected. It would require stepping
  inside a CPU cycle and changing the per-dot contract.
- **Keep the set-dot event pending for the next frame.** Rejected. The event
  marks the boundary already consumed by `process_frame()`; storing or
  restoring it would repeat the same VBlank entry on resume.

## Consequences

- Save/load resumes just inside VBlank, before scanlines 242–260 and the
  pre-render line.
- The framebuffer is complete because the palette flush runs at the VBlank
  set dot.
- The audio buffer remains excluded from the snapshot: the next
  `process_frame()` clears it before producing continuation audio.
- The frame counter advances at the next 261→0 wrap, inside the next
  `process_frame()` call.
- Existing version-2 state files are refused rather than migrated.
