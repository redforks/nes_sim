# Perf validation: batch PPU colour (issue #66, spec #60)

Measurement only; no feature code changed. Verdict: **batch colour is a
small real instruction-count win (~2.6% fewer instructions/frame);
the SIMD tier over the scalar flush is a wash** (0.04% fewer
instructions/frame, wall time within run-to-run spread), and the flush
does not enter the Hot Path. Numbers below are not a claim of a SIMD win.

## States measured

| State | Code | Commit |
|---|---|---|
| (a) before batch colour | main, per-dot colour commit | `7ccb7ce` |
| (b) scalar flush | integration tip with SIMD dispatch forced off (`NES_SIM_NO_SSSE3=1`: Tier-0 LUT + scalar gather) | `907a93e` |
| (c) SIMD flush | integration tip: #62 + #63 segments + #64 brightness + #65 Tier-0 LUT + Tier-1 SSSE3 gather | `907a93e` |

Notes on method (revised ticket #67): the former (b) was the #62-tip
commit `0150615`, which lacks #63/#64, so the delta conflated LUT+SSSE3
with segment/brightness logic. (b) and (c) are now the same integration
code — only the runtime dispatch differs (`NES_SIM_NO_SSSE3=1` forces
the scalar Tier-0 gather, read once per process via a `OnceLock`), so
the (b)->(c) comparison isolates the SSSE3 gather alone. The old
numbers are superseded by the re-measurement below (the original table
is kept at the end of the file for provenance).

Build hazard found and avoided: all worktrees share
`CARGO_TARGET_DIR=/tmp/nes-sim-target` via `.cargo/config.toml`, and
cargo then silently reuses the previously built `nes_core` rlib when
switching worktrees (only `nes_sdl`/`nescli` recompile). Every binary
below was built with a **separate** `CARGO_TARGET_DIR` per state and
verified distinct (`sha256sum` all different; `batch_convert` symbols
present only in (c)).

## Harness (identical for all three states)

- ROM: `../Super Mario Bros. (Japan, USA).nes`
- Movie: `../happylee-supermariobros,warped.fm2` (17,868 frames)
- Window: `--trim-after 3000` (first 3,000 frames)
- Command:
  `nescli "<rom>" play-movie "<movie>" --no-throttle --headless --trim-after 3000`
- Build: `cargo build -p nescli --release` (workspace `profile.release`,
  `debug = 1`; `.cargo/config.toml` `target-cpu=znver5`, mold linker)
- Machine: AMD Ryzen 7 9700X (SSSE3/AVX2/AVX-512 present, so the SSSE3
  tier really dispatched — `perf record` shows non-zero
  `batch_convert::convert` self samples in (c)), rustc 1.99.1, Linux.

## Results

### Wall time (5 interleaved runs per state, 3,000 frames)

| State | Median | Mean | FPS (median) |
|---|---|---|---|
| (a) baseline `7ccb7ce` | 4.0154 s | 4.0135 s | 747.1 |
| (b) dispatch off `907a93e` | 3.9506 s | 3.9392 s | 759.4 |
| (c) dispatch on `907a93e` | 3.9048 s | 3.9158 s | 768.3 |

(c) is 2.8% faster than (a) by median and 1.2% faster than (b) —
inside the run-to-run spread (individual runs: (b) 3.92-3.96 s, (c)
3.90-3.95 s). Wall time alone is not conclusive; the instruction
counts below are the deterministic signal.

### Instructions per frame (`perf stat -e instructions:u`, same window)

Deterministic to within <2k instructions across 3 repetitions per state
(headless deterministic workload):

| State | Total instructions | Instructions/frame |
|---|---|---|
| (a) baseline | 92,709,423,050 | 30,903,141 |
| (b) dispatch off | 90,819,941,245 | 30,273,314 |
| (c) dispatch on | 91,211,212,673 | 30,403,738 |

Batching saves ~630k instructions/frame (dispatch off, -2.04%) and
~499k (dispatch on, -1.61%) vs baseline. Dispatch on costs ~130k
instructions/frame *more* than dispatch off (+0.43%) — the SSSE3 gather
is a wash on this workload/hardware (see also the instruction-count
reading below). Cycles/frame (single `perf stat` sample): 7.31M / 7.10M / 7.27M — noisy, not used for the verdict.

Reading: the ~2.6% instruction saving comes from batching itself
(deferred colour, one flush instead of per-dot commit), already present
in the scalar flush. The Tier-0 LUT + SSSE3 gather adds nothing
measurable on this workload/hardware.

### Hot Path check (`perf record -F 997`, `--no-children`, 3,000 frames)

Top Self (SIMD state, 3 samples each 3K samples): `Ppu::tick` 39.2-39.5%,
`Apu::tick` ~8.9%, `NesMachine::tick` ~8.5%, `find_sprite_pixel` ~8.4%,
`Microcode::exec` ~8.4%, `fetch_tile_data` ~4.9% — background/sprite
evaluation still dominates, as the SIMD research note predicted.
`batch_convert::convert` Self: **0.36% / 0.52% / 0.80%** across three
samples. The Hot Path bar used in this repo is Self > 3% (GLOSSARY);
the flush is well below it, and the flush fn itself does not appear
as a separate Self row (inlined under `Ppu::tick`). Baseline top Self
for contrast: `Ppu::tick` 53.2% (per-dot colour commit inside tick).
Conclusion: **the flush does not enter the Hot Path; report the SIMD
tier as a wash, not a win.**

### Allocation check (no per-frame allocation regression)

- Code: `nes_core/src/nes/ppu.rs:231,291` — `palette_index_buffer:
  Box<[u8]>` allocated once in `Ppu::new` as
  `vec![0; VISIBLE_WIDTH * VISIBLE_HEIGHT].into_boxed_slice()`
  (61,440 bytes); per frame it is only `.fill(0)`-cleared
  (`ppu.rs:317` in `reset()`, and overwritten by beam position during
  the frame). No realloc.
- The flush (`flush_palette_index_buffer`, `ppu.rs:671`) and
  `batch_convert.rs` allocate nothing on the heap in non-test code:
  LUT and `[0u32; 16]` chunk are stack values; `Vec` uses in
  `batch_convert.rs` are all inside `#[cfg(test)]`.
- Measurement (child max RSS via `wait4`, 500 vs 3,000 frames):
  all three states ~11.5-11.7 MiB regardless of frame count
  (a 61,440 B/frame leak would add ~185 MB at 3,000 frames). No
  growth with frames; no regression vs baseline.

### Sanity: tests at the measured tip

`cargo test -p nes_core` at `fac886e`: 784 passed, 0 failed
(+1 integration test `sprdma_and_dmc_dma_rom_terminates`).

## Reproduce

```bash
# in three clean worktrees at 7ccb7ce / 0150615 / fac886e respectively,
# each with its own target dir:
CARGO_TARGET_DIR=/tmp/bench66/<state>-target \
  cargo build -p nescli --release
/tmp/bench66/<state>-target/release/nescli \
  "../Super Mario Bros. (Japan, USA).nes" play-movie \
  "../happylee-supermariobros,warped.fm2" \
  --no-throttle --headless --trim-after 3000   # wall time
perf stat -e instructions:u -- <same command>  # instructions/frame
perf record -F 997 -- <same command>           # Hot Path (Self)
```

## Superseded first measurement (ticket #66, pre-#67)

The original (b)/(c) rows measured (b) at the #62 tip `0150615` (no
#63/#64) vs (c) at `fac886e`, conflating LUT+SSSE3 with the
segment/brightness logic. Superseded by the re-measurement above:

| State | Wall median | Total instructions |
|---|---|---|
| (a) baseline `7ccb7ce` | 4.0154 s | 92,709,423,050 |
| (b) scalar `0150615` | 3.9232 s | 90,336,566,513 |
| (c) SIMD `fac886e` | 3.9422 s | 90,296,070,903 |
