# Per-frame benchmark: timed scope, fixtures, and ROM sourcing

## Decision

`nes_core/benches/frame.rs` (criterion, `harness = false`) times exactly
one `NesMachine::process_frame()` per sample, with the rendered bytes
folded through `black_box`. SDL presentation, file IO, and save-state
restore stay outside the timed section: each sample restores the same
in-memory JSON save-state string in `iter_batched` setup, so every sample
times the identical busy frame.

Fixtures are generated at bench startup by advancing each ROM from
power-on with a fixed input script (Start past the title, then held
gameplay inputs) to a sprite-heavy gameplay frame; no save-state files
are committed. ROMs are read from `$NES_BENCH_ROM_DIR` and never vendored:
a missing dir or file skips that game with a message, keeping the exit
green.

## Considered options

- **Time `process_frame()` plus restore.** Rejected. Restore parses the
  full JSON state and would dominate the number.
- **Commit JSON fixtures.** Rejected. Faster startup, but large generated
  files in git for no measurement benefit.
- **Vendor ROMs in the repo.** Rejected. Licensed binaries do not belong
  in the tree; the env-var dir keeps commercial ROMs out of git.

## Consequences

- Numbers report emulation cost against the 16.6ms frame budget.
- Startup costs a few seconds advancing ~900/1200/700 power-on frames.
- Report-only: no regression threshold in CI.
