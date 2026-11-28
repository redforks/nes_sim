# Frame-batch performance: slot-reuse microcode queue and sprite column pre-check

`cargo bench -p nes_core --bench frame` (one `process_frame` per game from a
fixed snapshot) had plateaued after tick fusion, inlining, and hot-tick
batching. The goal was another 2% with `just passed` green.

We decided on three changes, all bit-exact and snapshot-compatible:

1. CPU microcode queue as `[Microcode; 8]` with `len`/`pos` (`nes_core/src/cpu.rs`).
   `pop` bumps `pos`; `push_microcodes` parks to 0 and `copy_from_slice`s;
   `push_front` reuses a consumed slot, else shifts, else silently drops —
   the same overflow behavior the `ArrayDeque` release path had. This removes
   the deque head/tail loads and bounds checks from every CPU cycle. Pinned
   A/B: smb 663.1 vs 677.8 (-2.2%), contra -0.6%, galaxian -0.2%.
2. Sprite column pre-check (`SpriteManager::mask_line` +
   `column_may_hit`, `nes_core/src/nes/ppu/sprite.rs`, wired in
   `Ppu::render_pixel`). Before the first sprite fetch of a scanline the mask
   is stale, so the check falls through and the fetch builds it — the same
   truth value as the in-`find` mask test. Covered lines still win because
   coverage caps at 8 sprites x 8 columns by hardware: most pixels of every
   line are empty, and empty columns now also skip the render-state build.
   Pattern writes do not touch X-coverage, so no generation counter is
   needed; `invalidate_rows` clears the key.
3. `PpuRenderState::fetch_phase` constant 0: informational only, no mapper or
   core path reads it (the fetch kind is re-tagged before any mapper sees
   it). Saves one modulo per state build (~69k/frame).

Combined pinned A/B (`taskset -c 14,15`, same window): smb 653.6 vs 679.9
(-3.9%), contra 834.0 vs 858.6 (-2.9%), galaxian 740.0 vs 766.1 (-3.4%).
`just passed` is green.

**Rejected alternatives:**
- APU frame-sequencer deadline gate (batch `advance(elapsed)`, skip quiet
  cycles): measured ~-0.2% (check-op removal; the superscalar absorbs it)
  and broke `cpu_interrupts_v2` test 3 (`nmi_and_irq`, `E463FD9F`) — exact
  event timing, not just frequency. Fully reverted.
- 1-entry render-state memo keyed `(mask_bits, ctrl)`: smb +6%. The memo
  hit (~6 ops) costs as much as the pure build (~5 ops).
- Line-emptiness gate: wins smb-only (-1%) but loses sprite-dense games
  (contra +2.6%, galaxian +5%) — dense lines pay the check with no savings.
  The column check wins on every line instead.
- NMI quiescent skip: waking on a CPU `$2000` write needs a cross-object
  poke into `Cpu`; too risky for ~2%.
- Per-pixel micro-edits (`get_unchecked`, branch reorders in the machine
  loop): each nets ~0 and proved layout-sensitive (a `ppu.rs` micro-edit
  batch regressed smb +3% and had to be fully reverted). No micro-edit lands
  without its own pinned A/B.

**Consequences:**
- Queue helpers (`push_microcodes`, `push_front`, `pop_front`,
  `clear_queue`) replace the `ArrayDeque` sites; the DTO stays `Vec`.
- The column pre-check keys on `rows_scanline`: rows and mask are
  rebuilt and cleared together (`ensure_rows`, `invalidate_rows`,
  save-state restore, `set_test_rows`), so no second key field exists
  to drift out of sync.
- Measurement protocol: same-window pinned A/B only. A sustained co-tenant
  (~15% CPU) inflates absolutes while preserving relatives; cross-window
  absolute comparisons read as phantom ±3% swings.
