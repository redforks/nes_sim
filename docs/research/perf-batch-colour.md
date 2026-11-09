# Perf validation: batch PPU colour (issue #66, spec #60)

Measurement only; no feature code changed. Verdict: **batch colour is a
small real instruction-count win (~2.6% fewer instructions/frame);
the SIMD tier over the scalar drain is a wash** (0.04% fewer
instructions/frame, wall time within run-to-run spread), and the drain
does not enter the Hot Path. Numbers below are not a claim of a SIMD win.

## States measured

| State | Code | Commit |
|---|---|---|
| (a) before batch colour | main, per-dot colour commit | `7ccb7ce` |
| (b) scalar drain | #62 tip (flat buffer + vblank drain, per-pixel resolve in drain, no LUT, no SIMD, no #63/#64 split/zapper logic) | `0150615` |
| (c) SIMD drain | integration tip: #62 + #63 segments + #64 brightness + #65 Tier-0 LUT + Tier-1 SSSE3 gather | `fac886e` |

Notes on method: #65 provides no env-var/flag to disable SIMD dispatch
(`batch_convert::convert` dispatches on `is_x86_feature_detected!("ssse3")`
only), so (b) is the #62-tip commit as instructed. State (b) therefore
also lacks the #63 split-point and #64 Zapper code; the (b)->(c)
comparison conflates LUT+SSSE3 with those changes, which is another
reason to read (b) vs (c) as a wash, not a regression.

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
| (b) scalar `0150615` | 3.9232 s | 3.9342 s | 764.7 |
| (c) SIMD `fac886e` | 3.9422 s | 3.9426 s | 761.0 |

(b) is 2.3% faster than (a) by median; (c) is 1.8% faster than (a) and
0.5% slower than (b) by median (0.2% by mean) — inside the run-to-run
spread (individual runs range 3.91-4.04 s). Wall time alone is not
conclusive; the instruction counts below are the deterministic signal.

### Instructions per frame (`perf stat -e instructions:u`, same window)

Deterministic to within <10 instructions across 3 repetitions per state
(headless deterministic workload):

| State | Total instructions | Instructions/frame |
|---|---|---|
| (a) baseline | 92,709,443,xxx | 30,903,14x |
| (b) scalar | 90,336,55x,xxx | 30,112,19x |
| (c) SIMD | 90,296,09x,xxx | 30,098,69x |

Exact totals (rep 1): (a) 92,709,423,050; (b) 90,336,566,513;
(c) 90,296,070,903. So batching saves ~791k instructions/frame
(scalar, -2.56%) and ~804k (SIMD, -2.60%) vs baseline; SIMD vs scalar
saves ~13.5k/frame (-0.045%). Cycles/frame (single `perf stat` sample):
7.31M / 7.10M / 7.27M — noisy, not used for the verdict.

Reading: the ~2.6% instruction saving comes from batching itself
(deferred colour, one drain instead of per-dot commit), already present
in the scalar drain. The Tier-0 LUT + SSSE3 gather adds nothing
measurable on this workload/hardware.

### Hot Path check (`perf record -F 997`, `--no-children`, 3,000 frames)

Top Self (SIMD state, 3 samples each 3K samples): `Ppu::tick` 39.2-39.5%,
`Apu::tick` ~8.9%, `NesMachine::tick` ~8.5%, `find_sprite_pixel` ~8.4%,
`Microcode::exec` ~8.4%, `fetch_tile_data` ~4.9% — background/sprite
evaluation still dominates, as the SIMD research note predicted.
`batch_convert::convert` Self: **0.36% / 0.52% / 0.80%** across three
samples. The Hot Path bar used in this repo is Self > 3% (GLOSSARY);
the drain is well below it, and the drain fn itself does not appear
as a separate Self row (inlined under `Ppu::tick`). Baseline top Self
for contrast: `Ppu::tick` 53.2% (per-dot colour commit inside tick).
Conclusion: **the drain does not enter the Hot Path; report the SIMD
tier as a wash, not a win.**

### Allocation check (no per-frame allocation regression)

- Code: `nes_core/src/nes/ppu.rs:231,291` — `palette_index_buffer:
  Box<[u8]>` allocated once in `Ppu::new` as
  `vec![0; VISIBLE_WIDTH * VISIBLE_HEIGHT].into_boxed_slice()`
  (61,440 bytes); per frame it is only `.fill(0)`-cleared
  (`ppu.rs:317` in `reset()`, and overwritten by beam position during
  the frame). No realloc.
- The drain (`drain_palette_index_buffer`, `ppu.rs:671`) and
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
