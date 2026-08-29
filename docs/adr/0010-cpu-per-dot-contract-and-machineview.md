# Narrow Cpu's interface to its per-dot contract

Cpu exposes `pub a/x/y/sp/status` fields (writable!), hands `&Cpu<M>` to every `Plugin` (which then walks `cpu.mcu().ppu().in_vblank()` / `timing().frame_no()` / `renderer().borrow_image()` in `nametable_console.rs:104`, `frame_png_dump.rs:44`, `png_frame_match.rs:91`), and keeps `pub(crate) can_pause/dma_halt_bus_addr/frozen` only for the DMC-DMA seam (`dmc_dma.rs:47`). The status byte's I-flag invariant is unenforceable while `status` is writable, and every new tool pulls more internals public. Candidate 1 (interrupt window as table data) and Candidate 3 (`run_to_instruction_boundary`) have landed, so the drain no longer needs hand-copying, and Bus (Candidate 5) will privatize the DMA helpers. The question is whether and how to narrow the surface to the per-dot contract plus a read-model snapshot.

## Considered options

- **Adopt — staged narrow to per-dot contract + MachineView (this ADR)**: `Cpu<M>`'s pub surface becomes `{tick → TickOutcome, update_interrupt_lines, reset, is_halted, microcodes_empty, run_to_instruction_boundary}` plus read-only `a()/x()/y()/sp()/status()/pc()/flag()` and `snapshot()`/`view()`. Fields `a/x/y/sp/status` become private; `mcu()` becomes `pub(crate)`; DMA helpers collapse into `Bus`; `Plugin<M>` becomes `Plugin<M> { fn start(&MachineView<M>); fn end(&MachineView<M>) }` where `MachineView<'a, M: Mcu> {cpu: CpuSnapshot, mcu: &M, clock}` is built in `Cpu::tick` as `MachineView::new(snapshot, &mcu, clock)` and for `NesMcu` exposes `ppu_in_vblank()`, `read_vram()`, `borrow_image()` without exposing `&Cpu` or `&Mcu` walks. `TickOutcome {control, instruction_complete}` replaces `(ExecuteResult, bool)`. CONTEXT staleness (`CpuClockPhase` gone) is swept in the same pass. Justified by invariant protection (I-flag) and depth (one seam instead of `mcu().ppu()` walks).

- **Adopt — minimal now, snapshot later**: Narrow regs and make DMA helpers private, but keep `Plugin(&Cpu)` for now; snapshot introduced only when first consumer migrates. Less churn, but leaves the over-broad `mcu()->ppu()` walk open longer and defers the I-flag write protection. Rejected as it keeps the god-object while still paying the field-privatization churn.

- **Reject — keep current surface**: Keep `pub` regs, `&Cpu` in Plugin, `pub(crate)` DMA. Document the intended surface in CONTEXT only. No migration cost, but every new tool keeps pulling more internals public and the status-byte invariant stays unprotected. Rejected as it preserves the evidence that triggered the ticket.

- **Defer until Bus/handshake land**: Wait until Bus (Candidate 5) and interrupt handshake (Candidate 4) are implemented. Avoids specifying snapshot while those seams settle, but blocks sequencing — Candidate 6 is on the critical path for ordering the spec. Rejected as Candidates 1 and 3 are already closed, so the drain seam no longer blocks.

## Decision

**Adopt** staged narrow (ADR-0010) with the following interface direction (implementation happens after map hand-off; this ADR is the spec).

### 1. Per-dot contract

```rust
// nes_core/src/cpu.rs
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CpuSnapshot { pub a: u8, pub x: u8, pub y: u8, pub sp: u8, pub status: u8, pub pc: u16, pub halt: bool }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TickOutcome { pub control: ExecuteResult, pub instruction_complete: bool }

impl<M: Mcu> Cpu<M> {
    pub fn new(mcu: M) -> Self;
    pub fn reset(&mut self);
    pub fn tick(&mut self, plugin: &mut dyn Plugin<M>, clock: SystemClock) -> TickOutcome;
    pub fn update_interrupt_lines(&mut self, lines: InterruptLines, clock: SystemClock);
    pub fn is_halted(&self) -> bool;
    pub fn microcodes_empty(&self) -> bool;
    pub fn run_to_instruction_boundary(&mut self, plugin: &mut dyn Plugin<M>, clock: &mut SystemClock);
    pub fn a(&self) -> u8; pub fn x(&self) -> u8; pub fn y(&self) -> u8;
    pub fn sp(&self) -> u8; pub fn status(&self) -> u8; pub fn pc(&self) -> u16;
    pub fn flag(&self, f: Flag) -> bool;
    pub fn peek_byte(&self, addr: u16) -> u8;
    pub fn snapshot(&self) -> CpuSnapshot;
    pub fn view(&self, clock: SystemClock) -> MachineView<'_, M>;
    // pub(crate) mcu/mcu_mut, private fields, pub(crate) DMA projections, test-only set_a/set_x/set_y/set_sp/set_status
}
```

Fields `a/x/y/sp/status` become private; `pub(crate) mcu/mcu_mut`; `pub(crate) can_pause/dma_halt_bus_addr/pending_bus_cycle/bus_cycle/last_read_addr/frozen/next_microcode` remain `pub(crate)` for the Bus seam (not public). Deletion test: removing `cpu.rs`'s register file or `view.rs` scatters the four plugin walks plus `report.rs:94` + `tcp_server.rs:229` across `Cpu` again.

### 2. MachineView read-model

```rust
// nes_core/src/view.rs
pub struct MachineView<'a, M: Mcu> {
    pub cpu: CpuSnapshot,
    mcu: &'a M,
    pub clock: SystemClock,
}
impl<'a, M: Mcu> MachineView<'a, M> {
    pub fn new(cpu: CpuSnapshot, mcu: &'a M, clock: SystemClock) -> Self;
    pub fn cpu(&self) -> &CpuSnapshot;
    pub fn peek(&self, addr: u16) -> u8;
}
impl<'a, R: Render, D: AudioDriver> MachineView<'a, NesMcu<R,D>> {
    pub fn ppu_in_vblank(&self) -> bool;
    pub fn ppu_rendering_enabled(&self) -> bool;
    pub fn ppu_frame_no(&self) -> usize;
    pub fn read_vram(&self, addr: u16) -> u8;
}
impl<'a, D: AudioDriver> MachineView<'a, NesMcu<ImageRender, D>> {
    pub fn borrow_image(&self) -> &RgbaImage;
}

pub trait Plugin<M: Mcu> {
    fn start(&mut self, view: &MachineView<M>, clock: SystemClock);
    fn end(&mut self, view: &MachineView<M>, clock: SystemClock);
    fn should_stop(&self) -> ExecuteResult;
}
```

`Cpu::tick` builds the view as `MachineView::new(self.snapshot(), &self.mcu, clock)` for both `start` (before dispatch) and `end` (after execution, when `microcode_queue.is_empty()`). Deletion test: deleting `view.rs` scatters the `nametable_console`/`frame_png_dump`/`png_frame_match`/`report` walks across `Cpu` again.

Migration:

- `ReportPlugin::start/end` read `view.cpu.a/x/y/sp/status/pc` + `view.peek(0x0002)`.
- `NametableConsole::end` reads `view.ppu_in_vblank()`, `view.ppu_rendering_enabled()`, `view.read_vram(0x2000+off)`.
- `FramePngDump`/`PngFrameMatch` read `view.ppu_in_vblank()`, `view.ppu_frame_no()`, `view.borrow_image()`.
- `CompositePlugin`, `QuietPlugin`, `MonitorTestStatus`, `DetectDeadLoop`, `Timeout`, `MaxInstructions`, `ImageExit` migrate to `&MachineView`.
- `tcp_server` builds `MachineStatus/CpuRegisters` from `view.cpu` or `cpu.a()` accessors.
- Tests that today write `cpu.a = 0x42` migrate to `cpu.set_a(0x42)` via `#[cfg(test)]` setters; reads become `cpu.a()`.

### 3. Register invariant

`a/x/y/sp/status` are write-protected: `set_flag`, `set_register` stay private; external `cpu.status |= 0x04` is impossible. Tests use `#[cfg(test)] pub fn set_a(&mut self, v: u8)` etc (or `with_registers` builder). `grep -rn "cpu\.(a|x|y|sp|status)\s*=" --include="*.rs" | grep -v "cpu.rs" | grep -v "microcode.rs"` goes to 0 outside the builder.

### 4. TickOutcome

Replaces `-> (ExecuteResult, bool)` (`cpu.rs:660`). `instruction_complete` is `microcode_queue.is_empty()` after the tick — the instruction boundary. Callers `while !cpu.tick(...).1` become `while !cpu.tick(...).instruction_complete`; `match .0` becomes `.control`. `Halt` stays inside `ExecuteResult`, not folded.

### 5. CONTEXT sweep (same pass)

- Delete `CpuClockPhase` entry (lib.rs:30 `is_cpu_clock` helpers replace enum).
- Refresh `Tick` to name `Bus` as DMA owner and `update_interrupt_lines` as single interrupt entry (ADR-0008/0009).
- Mint: `MachineView`, `CpuSnapshot`, `TickOutcome`, `per-dot contract` (`TickOutcome` section).

## Consequences

- **Invariant & depth.** I-flag protected; one seam (`MachineView`) replaces `mcu().ppu().in_vblank()` walks in 4 plugins plus `report`/`tcp_server`. Deleting `view.rs` scatters that knowledge.
- **Testability.** `Cpu::view(clock)` is constructible in unit tests via `MachineView::new` or `cpu.view(clock)` without booting PPU/APU; `DetectDeadLoop` tests build views from `cpu.snapshot()` + `cpu.mcu()`.
- **Acceptance gates.**
  1. `grep -rn "pub a:\|pub x:\|pub y:\|pub sp:\|pub status:" nes_core/src/cpu.rs` == 0; `rg "cpu\.(a|x|y|sp|status)\s*=" -g '!cpu.rs' -g '!microcode.rs'` == 0.
  2. `grep -rn "Plugin.*&Cpu" nes_core nes_cpu_test` == 0; `grep -rn "mcu().ppu()" nes_cpu_test` == 0 — plugins/tools use `MachineView`.
  3. `Cpu::mcu/mcu_mut` are `pub(crate)`; `rg "\.mcu\(\)" -g '!nes.rs' -g '!nes_machine.rs'` finds 0 outside `view.rs`/`bus.rs` crate internals (NesMachine's `mcu()` remains pub).
  4. `grep -rn "dma_halt_bus_addr\|can_pause" nes_core/src --include="*.rs" | grep -v "cpu.rs.*pub(crate)"` == 0 outside `bus.rs`.
  5. `grep -rn "tick.*ExecuteResult.*bool" nes_core` == 0; `rg "TickOutcome"` >0 and `cargo test -p nes_core --lib` uses `.instruction_complete`/`.control`.
  6. `CONTEXT.md` contains MachineView/TickOutcome/per-dot contract, no CpuClockPhase enum; `cargo test -p nes_core` + `/tmp/nes-sim-target/release/nes_cpu_test --quiet -f blargg_nes_cpu_test5` + `vbl_nmi_timing` green (no behavioral change).
  7. No behavioral change gate: `cargo test -p nes_core` and full `nes_cpu_test` suite green; capture PNG/WAV gates untouched.

- **Vocabulary.** CONTEXT.md gains `MachineView`, `CpuSnapshot`, `TickOutcome`, `per-dot contract` under "System Timing"; `CpuClockPhase` removed.

- **Seam with Bus.** `Cpu` narrowing retains `can_pause`/`dma_halt_bus_addr` as `pub(crate)` for the `Bus ↔ Cpu` seam; Bus remains the one owner for `is_busy`/`suppress`/`reset`.

- **Ordering.** Implementable immediately after Candidates 1,3,5; if Bus lands first, `Cpu` dependency adapts. No new prerequisite beyond `InterruptSequences` table data.

## Alternatives revisited

Keeping the split with `&Cpu` in Plugin is safe but keeps the god-object and the I-flag hole; deferring leaves the untested `mcu().ppu()` walks while Candidates 7 proceeds. The adopted narrow pays one view module for the concentration and the invariant.
