# Delete interrupt tracing and weightless code from the CPU hot path

`nes_core::cpu::Cpu` carries `track_interrupt: bool` (initialized from `NES_INTERRUPT_TRACK` at `Cpu::new:370`), `IrqDetector::irq_line_changed_at` and `NmiDetector::nmi_line_changed_at`, `OPCODE_MNEMONICS: [&str; 256]` (260 lines, ~20% of `cpu.rs:1335-1594`), and `init_microtable.inc.rs` (289 identical `ArrayVec::from_array_empty([Microcode::Kill; 7])` lines at `microcode.rs:456`). The tracing touches ~8 `println!`/`dbg!` branches in `tick` (frozen `738`, halted `748`), `do_detect_interrupt` (`dbg!` `810`, `Enter NMI` `823`), `switch_to_nmi_vector` (`hijack` `1194`), and `dump_interrupt_track` (`789`, `1296-1326`) where only the env-var build diverges — no test pins it. After ADR-0006 (recognition window as table data: `InterruptWindow` + `consumed_through` edge protocol) and ADR-0008 (`InterruptLines { NmiLines {level,race_cancel}, irq_level}` + `CartridgeIrqLatch` + `ApuIrqSampler` + third writer `cancel_rising_edge_at`), latency uses `pending_asserted_since`/`asserted_since`, not the stamp fields. The mnemonics are read only in the dump (`1311`), and the include file is overwritten by the next ~380 assignments.

## Considered options

- **Delete (this ADR)**: Remove `track_interrupt` + env var, all `println!`/`dbg!` branches, both `*_line_changed_at` fields, `OPCODE_MNEMONICS`, and `init_microtable.inc.rs` → `[ArrayVec::from_array_empty([Microcode::Kill; 7]); 256]` (const-compatible single expression; `std::array::from_fn` is not yet const-stable in Rust 1.97, #143874). No new seam. Future debugging via deterministic goldens (per-opcode poll-cycle golden + `cpu_interrupts_v2` from ADR-0006, per-opcode `BusCycle` golden from ADR-0007) and a temporary local patch that adds `log::trace!` or a local `MachineView` sink. Hygiene, no behavioral change.

- **Trace sink behind the Plugin seam** (`PluginView::on_interrupt_trace {clock, irq, nmi, opcode, q_len}`): structured, testable, no global. Rejected — widens the seam just narrowed by ADR-0010 (`Plugin` → `MachineView` read-model, private regs), publishes `IrqDetector`/`NmiDetector` internals through the read-model, keeps a per-tick branch + virtual call even when `EmptyPlugin`. Fails depth test (deleting tracing deepens `Cpu`; adding a sink shallows it).

- **`log` crate path**: Replace `NES_INTERRUPT_TRACK` + `println!` with `log::trace!`/`debug!` behind `RUST_LOG` (already `workspace.dependencies log 0.4` with `release_max_level_info`). Zero-cost when filtered, but still adds a per-tick level check, retains `OPCODE_MNEMONICS` for formatting, and keeps hot-path noise. Useful as a *temporary* local patch, not a permanent seam. Not adopted.

- **Feature-gated module** (`#[cfg(feature = "interrupt-trace")]`): compilation removes the branch, but adds two build variants, `cfg` sprawl, and retains the 260-line table + stamp fields behind `cfg`. Same depth cost as above with extra build complexity. Rejected.

Weightless alternatives likewise rejected: keep stamps "just in case" (superseded by `pending_asserted_since` + goldens), keep `OPCODE_MNEMONICS` behind `#[cfg(test)]` or in `nes_cpu_test` (no load-bearing consumer — re-add a 10-line helper if a debugger needs it), keep `init_microtable.inc.rs` for explicitness (array repeat is the boring standard, no duplication).

## Decision

**Delete** all tracing and the two weightless artifacts. Spec (implementation after map hand-off on `wayfinder:map #20`, Candidate 7 `https://github.com/redforks/nes_sim/issues/28`):

1. **State**
   - `Cpu::track_interrupt: bool` + `std::env::var("NES_INTERRUPT_TRACK").is_ok()` at `Cpu::new` removed.
   - `IrqDetector::irq_line_changed_at: Option<SystemClock>` removed. `fn update_irq_input(&mut self, v: bool, clock: SystemClock, track: bool)` → `fn update_irq_input(&mut self, v: bool)` (`self.irq_input = v`).
   - `NmiDetector::nmi_line_changed_at: Option<SystemClock>` removed. `update_nmi_input` drops `self.nmi_line_changed_at = Some(clock)`. `cancel_rising_edge_at` changes guard from `self.nmi_line_changed_at.map(cycles)==Some(clock.cycles()) && self.nmi_input` to `self.asserted_since == Some(clock.cycles()) && self.nmi_input`; `self.nmi_line_changed_at = None` cleanup deleted. `mark_consumed` + `nmi_input=false` + `last_sampled_level=false` (unified `consumed_through` third writer) untouched.

2. **Hot-path branches**
   - `Cpu::set_irq(enabled, clock)` → `pub(crate) fn set_irq(&mut self, enabled: bool)` (drops `clock`); `update_interrupt_lines` calls `self.set_irq(lines.irq_level)` (no clock for IRQ).
   - Remove `if self.track_interrupt` in `tick` (frozen, halted, `dump_interrupt_track` call), in `do_detect_interrupt` (`dbg!`, `Enter NMI println!`), in `switch_to_nmi_vector` (`hijack println!`). Delete `fn dump_interrupt_track(&self, clock: SystemClock)` entirely. `do_detect_interrupt` latency stays `pending_asserted_since.is_some_and(|t0| (clock.0 - t0) > 1)`.

3. **`OPCODE_MNEMONICS`** — `const OPCODE_MNEMONICS: [&str; 256]` (`cpu.rs:1335-1594`) deleted (`rg OPCODE_MNEMONICS → 0`).

4. **`init_microtable.inc.rs`** — file `nes_core/src/cpu/init_microtable.inc.rs` deleted (`rg init_microtable → 0`). At `microcode.rs:456`:
   ```rust
   // before
   let mut r = include!("init_microtable.inc.rs");
   // after — const-compatible single expression (std::array::from_fn is not yet const-stable in 1.97, see #143874)
   let mut r = [ArrayVec::from_array_empty([Microcode::Kill; 7]); 256];
   ```

No new interface; per-dot contract from ADR-0010 (`tick→TickOutcome`, `update_interrupt_lines`, `reset`, `is_halted`, `microcodes_empty`, `run_to_instruction_boundary` + `MachineView`) unchanged and intentionally not widened.

## Consequences

- **Depth & hot path.** `Cpu` loses a branch per `tick` + per `set_irq`, and two `Option<SystemClock>` fields; construction becomes deterministic (no `env::var`). Deleting `view.rs` still scatters the four plugin walks (ADR-0010 depth test); deleting tracing deepens `Cpu` instead of shallowing it.
- **Correctness gate is goldens, not prints.** NMI recognition uses stamped `pending_asserted_since`/`asserted_since` + `consumed_through`; short-assertion case (blargg `ppu_vbl_nmi` 07-nmi_on_timing, NMI enabled a few dots before vblank ends) is pinned by the poll-cycle golden, not by a falling-edge re-stamp. No behavioral change — hygiene refactor, sequencing (#30) treats it as unit-gated per north-star.
- **Future debugging.** Add a temporary `log::trace!` or `MachineView` probe locally; do not resurrect `NmiLines` internals as a permanent seam. If a debugger needs mnemonics, build a small map outside `nes_core::cpu`.
- **Verification.**
  1. `rg -n "track_interrupt|NES_INTERRUPT_TRACK" nes_core/src` → 0.
  2. `rg -n "irq_line_changed_at|nmi_line_changed_at" nes_core/src` → 0 outside ADR prose.
  3. `rg -n "OPCODE_MNEMONICS" nes_core` → 0.
  4. `rg -n "init_microtable" nes_core` → 0; `rg -n "ArrayVec::from_array_empty.*Microcode::Kill" nes_core/src/cpu/microcode.rs` → 1.
  5. `rg -n "dump_interrupt_track" nes_core` → 0; no `println! "[` in `cpu.rs` hot path; `rg "dbg!\(" nes_core/src/cpu.rs` finds no `dbg!((clock`.
  6. `cargo check -p nes_core` + `cargo test -p nes_core --lib` green; `nes_cpu_test` suites (`blargg_nes_cpu_test5`, `cpu_interrupts_v2`, `ppu_vbl_nmi` 06/07) identical before/after.
  7. `rg "Cpu::new.*env" nes_core` → 0.
- **Vocabulary.** No new `CONTEXT.md` terms; sweep any lingering "interrupt tracing" commentary — `MachineView`, `BusOwner`, `InterruptLines`, `consumed_through` from ADR-0006/0008/0009/0010 remain canonical. Next ADR is `0012`.
