# SIMD batch conversion of palette indices to RGBA in Rust

Research-only. No code changed. This note recommends a concrete SIMD approach
for the vblank drain proposed in spec #60 (batch PPU palette-index rendering
to end of frame), for the child ticket #65 (SIMD-optimized batch conversion
in the vblank drain). It was written under TDD skill guidance (see
/home/forks/.agents/skills/tdd/SKILL.md), but this ticket is research: no
red-green loop applies, no tests were written, and the deliverable is this
recommendation only.

## 0. Primary sources

Each claim that depends on Rust platform behaviour cites an official source
below. Code behaviour cites the local repository. Where a claim could not be
verified from a primary source it is labelled **unverified**.

- Rust `core::simd` / `std::simd` (portable SIMD), Rust 1.99.1 docs:
  https://doc.rust-lang.org/core/simd/ and https://doc.rust-lang.org/std/simd/
  — the module carries the banner "This is a nightly-only experimental API
  (portable_simd #86656)". It states portable SIMD "compiles for every
  target", that "operations use the best instructions available", and that
  "in those cases, regular scalar operations are generated instead."
- Rust `std::arch` / `core::arch` (vendor intrinsics), Rust 1.99.1 docs:
  https://doc.rust-lang.org/std/arch/ — target-specific, not portable;
  intrinsics are `unsafe` unless the caller enables the required target
  features; the sanctioned pattern is `is_x86_feature_detected!` runtime
  dispatch into a `#[target_feature(enable = "...")] unsafe fn` with a
  scalar fallback function. The module's own examples implement a
  hex-encode lookup table (`static TABLE: &[u8]`) as the scalar fallback.
- `core::arch::wasm32` SIMD128, Rust 1.99.1 docs:
  https://doc.rust-lang.org/core/arch/wasm32/ — stable intrinsics over a
  `v128` type (e.g. `u8x16_swizzle`, `u8x16_shuffle`, `v128_load8_splat`);
  per-function opt-in via `#[target_feature(enable = "simd128")]` or a
  whole-program `-Ctarget-feature=+simd128`. Explicitly: "WebAssembly does
  not currently have dynamic detection at runtime as to whether SIMD is
  supported ... your binary will either have SIMD and can only run on
  engines which support SIMD, or it will not have SIMD at all", and "the
  standard library itself does not use any SIMD internally."
- `_mm_shuffle_epi8` (SSSE3 byte-shuffle LUT), core::arch::x86_64 docs:
  https://doc.rust-lang.org/stable/core/arch/x86_64/fn._mm_shuffle_epi8.html
  — "Shuffles bytes from `a` according to the content of `b`"; the low 4
  bits of each byte of `b` address the 16 bytes of `a`, high bit zeroes.
- `_mm256_i32gather_epi32` (AVX2 gather), core::arch::x86_64 docs:
  https://doc.rust-lang.org/beta/core/arch/x86_64/fn._mm256_i32gather_epi32.html
  — "Available on x86-64 and target feature avx2 only"; gathers 8 x 32-bit
  elements by register offsets.
- Local code: `nes_core/src/nes/ppu.rs` (commit path, lines 551-562),
  `nes_core/src/nes/ppu/registers.rs` (`PpuMask::grayscale_index` /
  `apply_effects`), `nes_core/src/nes/ppu/palette.rs` (`ColorTheme`,
  `Palette`, `Pixel`), `nes_core/src/nes/ppu/pixel_pipeline.rs`
  (`PendingPixel`), `nes_core/src/render.rs` (`Render`),
  `nes_core/src/render/image_render.rs` (`ImageRender<N>`),
  `nes_web/src/canvas_render.rs` (`CanvasRender`),
  `docs/research/ppu-delayed-rendering.md`, ADR-0012
  (`docs/adr/0012-framebuffer-const-generic-zoom.md`), ADR-0016
  (`docs/adr/0016-presentation-plan-and-texture-reuse.md`), GLOSSARY.md
  (Hot Path entry), `Cargo.toml`, `justfile`, `nes_web/Cargo.toml`.

## 1. The conversion the drain must do

Spec #60 defers only the palette-index -> RGB step to the VBL set dot: the
per-dot path keeps background/sprite evaluation, sprite-0 hit, and writes a
6-bit palette index into a flat `[u8; 61440]` buffer indexed by beam position
(`y * 256 + x`); the drain replays PPUMASK colour split points segment-wise
and emits pixels in raster order through `renderer.set_pixel`, before
`finish()`. The frame is 256 x 240 = 61,440 pixels; at 60 fps that is
3,686,400 pixels/s, or 61,440 index bytes read + 245,760 RGBA bytes written
per frame (approx. 14.7 MB/s write rate at 60 fps — **unverified** whether
this exceeds memory bandwidth on any target in this repo; it is stated here
only as arithmetic).

The current per-pixel commit (the work the drain batches) is, from
`nes_core/src/nes/ppu.rs:551-562`:

1. `pixel_idx = mask.grayscale_index(pending.pixel_index)` — if grayscale
   is set, `index & 0x30` (`registers.rs`).
2. `color_theme.color(pixel_idx)` — `ColorTheme::color` masks with `& 0x3F`
   and returns a `Pixel([u8; 4])` from the 64-entry table
   (`palette.rs`; alpha is always `0xFF`).
3. `mask.apply_effects(pixel)` — if no emphasis bit is set, returns the
   pixel unchanged; otherwise each channel whose tint bit is clear is scaled
   by `* 192 / 256` via `u16` math (`registers.rs::do_apply_effects`).
4. `renderer.set_pixel(x, y, pixel.0)` (`render.rs` contract).

Key facts that drive the SIMD design:

- **The input domain is 64 values, not 256.** `pending.pixel_index` is a
  6-bit value, `0x3F` max (`pixel_pipeline.rs`, see the
  `highest_palette_index_round_trips` test; `0xFF` is the empty-slot
  sentinel, never a real index). So the whole conversion is a 64-entry
  table lookup plus an optional per-channel scale. There is no colour
  computation to vectorize beyond the scale.
- **Grayscale is index-domain, emphasis is RGB-domain.** Grayscale ANDs the
  index with `0x30` *before* the lookup; emphasis scales RGB *after* the
  lookup. Neither depends on pixel position — only on the segment's PPUMASK
  colour configuration. All three stages can therefore be folded into one
  64-entry final-colour table per segment (section 2).
- **The `Render` seam is per-pixel** (`set_pixel(x, y, [u8; 4])`,
  `render.rs`). `ImageRender<N>` replicates each logical `set_pixel` into
  an `N x N` block (Zoom Factor N, ADR-0012); playback renders
  `ImageRender<1>`, the `--output` recording path upscales host-side, and
  `CanvasRender` (web) writes a `Vec<u8>` buffer flushed by `putImageData`
  at `finish()` (`canvas_render.rs`). Whatever the drain computes, it must
  still call `set_pixel` per logical pixel, or produce byte-identical
  `ImageRender<N>` / canvas buffers through the same calls. SIMD must never
  bypass the seam.
- **The drain must not disturb Zoom Factor replication or the Presentation
  Plan.** ADR-0012 revision: playback is `ImageRender<1>` with one
  nearest-neighbour SDL copy to the window; ADR-0016: the plan reads
  `frame.as_bytes()` after the frame completes, one `Texture::update` + one
  `Canvas::copy` per presented frame. A faster drain does not change either
  downstream step, and anything that writes past the logical buffer into the
  N x N block layout would corrupt them.

## 2. Recommended approach

**Do the work in three tiers, in this order. Ship tier 0 now; tiers 1-2 only
with measured justification.**

### Tier 0 (recommended; ship first): per-segment 64-entry LUT + scalar gather

For each drain segment (the maximal run of pixels sharing one PPUMASK
colour configuration), build a 64-entry final-colour table once:

```
final[idx] = apply_effects(color_theme.color(grayscale_index(idx)))
            for idx in 0..64
```

stored as `[u32; 64]` (one `Pixel` per entry, LE bytes = RGBA order) so the
inner loop is a single indexed load + store. The drain then becomes a pure
gather: `out[i] = final[indices[i]]` for the segment, emitting via
`set_pixel` in raster order.

Why this is the right first step:

- It collapses all three stages (grayscale, lookup, emphasis) into one table
  build per segment. Segment count is bounded by the number of mid-frame
  `$2001` writes in a frame — zero in a static frame (one table per frame),
  a handful in split-screen games. The emphasis `* 192 / 256` multiply then
  runs at most 64 x 3 times per segment instead of 61,440 x 3 per frame.
- It preserves `apply_effects` bit-for-bit: the table build calls the same
  `grayscale_index` / `color` / `apply_effects` code as today, so the scalar
  per-pixel path and the batched path agree by construction (U1 test story:
  baseline frame byte-identical to pre-change output).
- It is backend-neutral: no target features, no nightly, no `unsafe`, works
  in `nes_core` and in `nes_web` (wasm32) as-is.
- It gives the compiler a gather loop it can autovectorize where the target
  allows (see the `std::arch` docs example: LLVM auto-vectorization behind a
  `#[target_feature]` function). Whether rustc actually autovectorizes this
  gather on this codebase's toolchains is **unverified** — the LLVM loop can
  carry a gather with aliasing unknown, and the standard library of a
  `wasm32` build "does not use any SIMD internally" (core::arch::wasm32
  docs), so no free SIMD is promised there.

Guard for pathological split counts: if a game writes `$2001` so often that
segments become a few pixels long, rebuilding a 64-entry table per segment
costs more than it saves. The drain should fall back to the current
per-pixel `grayscale_index` + `apply_effects` application when a segment is
too short to amortize the table (suggested threshold: **unverified**, decide
by measurement; a reasonable starting guess is a segment shorter than 64
pixels). This bounds the worst case at today's per-pixel cost.

### Tier 1: stable target intrinsics behind runtime dispatch (x86_64 + wasm32)

If profiling (Health of the drain measured before/after, per spec #60
testing decision) shows the gather loop is a Hot Path, add a SIMD tier
behind the sanctioned dispatch pattern from the `std::arch` docs:
`is_x86_feature_detected!` / `#[cfg(target_arch ...)]` checks at the drain
entry, dispatch into a `#[target_feature] unsafe fn`, scalar fallback
otherwise — exactly the shape of the docs' `hex_encode` example, including
its static-table scalar fallback.

- **x86_64 (native CLI/SDL): `_mm_shuffle_epi8` (SSSE3) LUT.** The documented
  semantics ("the last 4 bits of each byte of `b` are used as addresses
  into the 16 bytes of `a`" — `_mm_shuffle_epi8` docs) make it a 16-entry
  byte-table lookup per vector. The 64-entry final palette is 4 x 16-byte
  tables; a lane whose index is 0..63 selects the quarter from its high
  bits and shuffles within it (blend or a second shuffle on the high bits).
  This processes 16 pixels per shuffle. SSSE3 is the baseline of the
  repository's profiling fallback (`-C target-cpu=x86-64-v2` in the
  `justfile` profiling recipe), so SSSE3 availability on the developer's
  machine is near-universal but must still be runtime-detected, not assumed.
- **x86_64 (optional tier 2): AVX2 gather `_mm256_i32gather_epi32`.** The
  docs gate this on "target feature avx2 only". A gather of 8 x u32
  final-colour words per instruction is the most direct expression of the
  tier-0 loop, but it requires AVX2-capable CPUs and a `0x80`-masked-off
  default element; x86 AVX2 gathers are historically slow (microcoded on
  some microarchitectures) — that tuning claim is **unverified** for this
  codebase and should be measured before choosing gather over shuffle.
- **wasm32 (web): `core::arch::wasm32` SIMD128.** The `v128` intrinsics
  are stable (core::arch::wasm32 docs) and include byte shuffle/swizzle
  (`u8x16_swizzle` / `u8x16_shuffle`) that can express the same 16-entry
  LUT strategy as `_mm_shuffle_epi8`. Constraints from the primary source
  to respect: enablement is compile-time only (`#[target_feature(enable =
  "simd128")]` per function or `-Ctarget-feature=+simd128`), there is
  **no runtime detection** ("your binary will either have SIMD ... or it
  will not have SIMD at all"), and the build via `wasm-pack build --release`
  (`justfile`) currently passes only `--enable-bulk-memory` /
  `--enable-nontrapping-float-to-int` (`nes_web/Cargo.toml`
  `wasm-pack.profile.release`), so SIMD128 is off by default. This means:
  the wasm tier must be a separate opt-in build flavour (or a
  `#[cfg]`-gated function compiled with the feature), never the default
  `nes_web` artefact, because a SIMD128 wasm module will trap on engines
  without SIMD support. The scalar tier-0 path remains the default web
  build. Whether browser engines in the project's audience support SIMD128
  is **unverified**.

### Explicitly not recommended: portable SIMD (`core::simd`)

`core::simd` is the best API for this from a correctness standpoint — the
docs state it "compiles for every target", "can be thought of as a
parallelized `[T; N]`", and falls back to "regular scalar operations" where
no SIMD exists — but the same docs banner it nightly-only
(`#![feature(portable_simd)]`, tracking issue #86656). This repository has
no `rust-toolchain` file and builds on the stable toolchain (rustc 1.99.1
observed; `wasm-pack build --release` for web), so adopting portable SIMD
would require a nightly toolchain move for the whole workspace. Revisit if
and when portable SIMD stabilizes, or if the workspace moves to nightly for
other reasons. Its `Swizzle` / `simd_swizzle` machinery would map naturally
onto the 64-entry LUT, but that shape is **unverified** against this code
base — no usage was prototyped in this ticket.

## 3. Expected gain and limitations

- **Amdahl ceiling.** The GLOSSARY Hot Path entry (2000-frame
  `warped.fm2`, `--no-throttle`, profiling profile) puts `Ppu::tick` at
  47.7% Self of the baseline sample (CPU + PPU + APU/DMA + upload together).
  The palette conversion is a small subset of that — background/sprite
  evaluation dominates (`render_pixel`, `find_sprite_pixel`,
  TileCache/SpriteRow fills, `fill_tile_cache`) — and ADR-0012 revision
  already removed the large 4x pixel-replication win from playback
  (51.0 M instructions/frame, 2.36 ms task-clock on the reference movie).
  Any SIMD gain on a 61,440-pixel gather is bounded by the share of those
  instructions that the gather loop represents. A plausible estimate is
  **unverified**: even a 4x faster drain is likely to move frame-level
  throughput by a few percent at most. Measure with the repo's
  `--no-throttle --headless` harness before moving beyond tier 0.
- **Memory, not ALUs.** Tier 0 reads 61,440 index bytes and writes 245,760
  RGBA bytes per frame. On any target with vector ALUs the loop is
  memory/store-bound, not multiply-bound (the emphasis multiply is already
  amortized to 64 entries per segment). SIMD mainly shortens the gather
  instruction sequence; it cannot shrink the write bytes. Benefits show up
  as reduced instruction count, not reduced memory traffic.
- **Gather vs shuffle cost.** `_mm_shuffle_epi8`-style 16-lane LUTs have no
  gather-cache behaviour to miss; AVX2 gather's cost is microarchitecture
  dependent (**unverified** here). For a 64-entry palette that fits in
  registers/cache lines, shuffle typically wins over gather on modern CPUs —
  but this too must be measured on the actual hardware before choosing.
- **`nes_web` (wasm32) gets no free SIMD.** The `core::arch::wasm32` docs
  state the standard library uses no SIMD internally, and `nes_web` builds
  without `+simd128`. Tier 0 is the web story unless a separate
  SIMD128-enabled wasm build is accepted as a deliberate product decision
  (with the no-runtime-detection caveat above). Expected wasm gain from
  tier 1 is **unverified**; do not claim web parity with native.
- **Zoom Factor / presentation cost is outside the drain.** `ImageRender<N>`
  replication happens inside `set_pixel` (ADR-0012) and the texture upload
  happens after `finish()` (ADR-0016: one `Texture::update` + one
  `Canvas::copy` + `putImageData` on web). SIMD-ing the gather does not
  reduce those. A tempting but rejected variant is writing the drained
  palette straight into the `ImageRender<N>` N x N blocks to skip
  replication — rejected because it bypasses the `Render` trait, breaks
  `CanvasRender`, and couples the PPU to the framebuffer layout. The drain
  must stay behind `set_pixel`.

## 4. Fallback behaviour

| Situation | Behaviour |
|---|---|
| No SIMD feature detected (x86_64 without SSSE3/AVX2), non-SIMD wasm build, or a target `std::arch` does not cover | Tier 0 scalar gather (`out[i] = final[indices[i]]`), byte-identical output. This is the only path that runs on the default `nes_web` build. |
| Target with SIMD but feature check fails at runtime | Same scalar path, selected once per drain (or once per process via a cached flag), by the `is_x86_feature_detected!` dispatch pattern. No per-pixel branching. |
| Tiny segment (pathological `$2001` write rate, **unverified** threshold) | Bypass the 64-entry table build; apply `grayscale_index` + `apply_effects` per pixel (today's behaviour). Bounds worst case at current cost. |
| `nes_web` SIMD128 unavailable at runtime | No runtime detection exists (core::arch::wasm32 docs); the scalar build is the default artefact. A SIMD128 build is opt-in only and must not be assumed by the default server config. |
| Nightly instead of stable | Not a fallback — portable SIMD is rejected on the current stable toolchain (section 2). |

Correctness invariant across all paths: output bytes must equal the current
per-pixel sequence `grayscale_index` -> `color` -> `apply_effects` ->
`set_pixel` for the segment's configuration. The 64-entry table is built
from the same three functions, so every tier is a pure rearrangement, and
the existing "baseline frame byte-identical" + per-segment
grayscale/emphasis/palette test cases in spec #60 cover all tiers without
modification.

## 5. Applicability to 256 x 240 = 61,440 pixels/frame

- The gather loop is `for i in 0..61440` index loads + stores per frame,
  3,840 iterations of a 16-lane shuffle, or 7,680 iterations of an 8-lane
  AVX2 gather. Per frame this is trivially small next to the 51 M
  instructions/frame baseline; per second (60 fps) it is 221 M pixels/s,
  well within any vectorizable loop's budget. The design is not
  throughput-limited at this resolution; it is limited by how much of the
  frame the drain is (section 3).
- Because the buffer is flat (`y * 256 + x`) and segments are contiguous
  runs, the loop is linear and alignment-friendly — no per-pixel coordinate
  math in the SIMD tier. (Spec #60 user story 12 requires this shape.)
- `nes_web` buffers the 256 x 240 RGBA frame in a `Vec<u8>` and pushes it
  with `putImageData`; the drain must emit in raster order so the buffer
  needs no reordering afterwards. Byte-identical `set_pixel` emission is
  what guarantees this in every tier.

## 6. What this note deliberately does not decide

- The split-point recording and segment iteration are spec #60's concern,
  not this ticket's; the table build assumes a segment boundary list exists.
- Whether tier 1/2 is ever worth it: that decision needs the
  headless no-throttle before/after measurement spec #60's testing
  decisions describe, run on the maintainer's hardware. The author of this
  note had no profile evidence for the drain (the delayed-rendering
  research note already marks this as unverified), and made no code change,
  so no numbers are claimed.

## 7. Suggested ticket-#65 follow-ups (for the implementer, not done here)

1. Land tier 0 (per-segment `[u32; 64]` table + scalar gather) inside
   spec #60's drain behind the `Render` seam; verify byte-identical frames.
2. Profile: `perf record -F 997 -g` on the `--no-throttle --headless`
   harness (repo `just` profiling recipes), comparing per-dot commit vs
   tier-0 drain self-time. Proceed to tier 1 only if the drain registers
   as a Hot Path (Self > 3% per GLOSSARY).
3. If profiled hot: add the `is_x86_feature_detected!` /
   `#[target_feature]` SSSE3 shuffle tier with the documented scalar
   fallback, keep the wasm build scalar, and only then evaluate AVX2
   gather vs shuffle empirically.
4. If the workspace ever moves to nightly, re-evaluate `core::simd`
   (portable SIMD) for tier 1+.
