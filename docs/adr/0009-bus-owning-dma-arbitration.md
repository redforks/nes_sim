# DMA arbitration as a Bus module

The DMA bus was split across three owners with no single seam: `DmcDma` lives in `NesMachine` (`nes_machine.rs:18`, 7-state machine `dmc_dma.rs:89-104`, mockall-tested via `NesDmaSupport`), `OamDmaState` lives in `NesMcu` (`nes.rs:18-26`, `tick_oam_dma` `nes.rs:137-191`), and the DMC request queue lives in `Apu` (`take_dmc_dma_request`). `NesMachine::tick` threads a bare `dmc_drove_bus: bool` (`nes_machine.rs:117-118` `dmc_dma.tick(...) -> bool` → `tick_oam_dma(clock, dmc_drove_bus)`), and `NesMcu` interprets it at exactly one condition (`nes.rs:143-150` `dmc_drove_bus && startup_cycles==0 && transfer_cycle%2==0 → pause_cycles=1`). Reset must query two owners and poke suppression per dot (`nes_machine.rs:160-167` `while dmc_dma.is_busy() || mcu.oam_dma_active()` + `suppress_new_dmc_dma_requests()` each iteration) with cross-module ordering asserts (`nes.rs:83-84`). Collision tests hand-feed the bool (`nes/tests.rs:263` `tick_oam_dma(clock, true)`), so the machine-level wiring that produces it is untested — only the heavy harness suites `dmc_dma_during_read4` / `sprdma_and_dmc_dma` pin it end-to-end.

## Considered options

- **Adopt a Bus/DmaCoordinator owning arbitration and active transfer** (this ADR): new module concentrates the `DMC-wins` policy, the one `is_busy` aggregation, and the one `suppress`/`reset` path behind `tick(clock) -> BusOwner`. The internal `dmc_drove_bus` threading and the `NesMcu` collision predicate become private to the module. Justified by locality + testability, not by a second adapter.

- **Reject — keep the split and add a targeted integration pin**: leave `DmcDma` in `NesMachine`, `OamDmaState` in `NesMcu`, bool threading in `Tick`. Cover the wiring with a new `NesMachine`-level test that drives a `$4014` write plus a DMC request through `tick()` and asserts the doc timelines, without introducing a module. Keeps churn lowest, but locality stays scattered and every future DMA change (e.g., PAL alignment differences, savestate quiescence) must be fixed in two places plus `Tick`.

- **Defer until Cpu narrowing (Candidate 6) or a second arbitration policy appears**: defer the seam until `Cpu`'s `try_freeze`/`dma_halt_bus_addr` surface is locked or until a variant policy (e.g., 2A07 PPU DMA differences) forces a seam. Preserves optionality, but the untested wiring and two-owner `Reset` quiescence remain while Candidates 6–7 proceed around them, and ADR-0008's Bus assumption (`CartridgeIrqLatch` stays in `interrupt`, Bus owns arbitration + reset quiescence) would remain an open forward reference.

- **Bus as a stateless coordinator (shallow function)**: keep `OamDmaState` in `NesMcu` and `DmcDma` in `NesMachine`, introduce a free function `arbitrate(dmc_drove, oam_state) -> pause` to name the predicate but not the ownership. Rejected as shallow — it names the predicate but does not fix the two-owner `is_busy`/`reset`/`suppress` scatter, and the deletion test fails (removing the function scatters no complexity; callers still own the states).

## Decision

**Adopt** with the following interface direction (implementation happens after map hand-off; this ADR is the spec).

### 1. New module `nes_core::bus` (or `crate::nes::bus` if crate layout prefers)

Owns the **active** transfer for both channels. Deletion test: removing `bus.rs` scatters arbitration, `is_busy` aggregation, and reset quiescence across `NesMachine` + `NesMcu` again.

```rust
// nes_core/src/bus.rs  (names illustrative, keep DmcDma's seam private)
pub enum BusOwner { Idle, Oam, Dmc } // who drove/held the bus this dot

pub struct Bus {
    dmc: DmcDma,                 // 7-state machine stays private (dmc_dma.rs:89-104)
    oam: Option<OamActive>,      // active OAM transfer, private
    // OAM pending ($4014 page) stays as a request queue in the producer
    // for borrow reasons — see §2. Bus drains it on the next APU tick.
}

struct OamActive { page: u8, startup_cycles: usize, transfer_cycle: usize, latch: u8, pause_cycles: usize }

impl Bus {
    pub fn new() -> Self;
    /// Advance one dot. Called only on APU clocks from `NesMachine::tick`
    /// (or every dot with an internal no-op). Drives at most one bus access
    /// and returns who held the bus this dot. Internally:
    /// `let dmc_drove = dmc.tick(cpu, clock);` then `oam_tick(clock, dmc_drove)`.
    /// The `dmc_drove_bus` bool never leaves this method, and the `NesMcu`
    /// predicate (`nes.rs:143-150`) lives here as a private branch.
    pub fn tick(&mut self, cpu: &mut Cpu<NesMcu<R,D>>, clock: SystemClock) -> BusOwner;

    /// True while any DMA is in flight — the single quiescence predicate for
    /// `NesMachine::reset`. Implemented as `dmc.is_busy() || oam.is_some()`
    /// plus the producer's pending queue when drained (see §2). Callers do
    /// not query `dmc_dma` or `mcu.oam_dma_active()` separately.
    pub fn is_busy(&self, mcu: &NesMcu<R,D>) -> bool; // parameter documents the pending-queue borrow (alternatively pure self if $4014 routing moves)

    /// Single suppression path for reset drain: discard freshly generated DMC
    /// fetch requests so the drain terminates even with DMC still playing.
    pub fn suppress_new_dmc_requests(&mut self, mcu: &mut NesMcu<R,D>);

    /// Single reset path: `dmc.reset()` + `oam = None` (plus pending clear).
    /// Time-relative state re-anchors to the running `SystemClock` (clock keeps
    /// ticking across resets, as today).
    pub fn reset(&mut self);
}
```

`DmcDma`'s 7-state machine remains a private implementation detail behind `Bus`; its mockall seam `NesDmaSupport` (`dmc_dma.rs:18-45`) stays private to `bus`/`dmc_dma` and continues to be exercised by `dmc_dma/tests.rs:329-338` (`TestStruct` with `MockNesDmaSupport`). No second adapter is introduced for the arbitration policy itself — the seam is justified by testability and locality, not by a variant policy (one-adapter caution, stated honestly).

### 2. What replaces the `dmc_drove_bus` bool threading and `NesMcu` collision predicate

- The bool becomes a **private local** `dmc_drove` inside `Bus::tick`. `NesMachine::tick` no longer receives or threads it:
  ```rust
  // before
  let dmc_drove_bus = self.dmc_dma.tick(&mut self.cpu, clock);
  if self.cpu.mcu_mut().tick_oam_dma(clock, dmc_drove_bus) { return Continue; }
  // after
  let owner = self.bus.tick(&mut self.cpu, clock);
  if !matches!(owner, BusOwner::Idle) { return Continue; } // or bus-owns stall uniformly
  ```
  `grep -rn dmc_drove_bus nes_core/src --include="*.rs"` is `0` outside `bus.rs` after landing.

- The `NesMcu` predicate (`nes.rs:143-150`) moves inside `Bus::oam_tick` as:
  ```rust
  if dmc_drove && oam.startup_cycles==0 && oam.transfer_cycle.is_multiple_of(2) { oam.pause_cycles = 1; }
  ```
  `NesMcu::tick_oam_dma` is deleted; `NesMcu` no longer stores `oam_dma`/`oam_dma_pending` as active state. The request queue for OAM remains the `$4014` write in `NesMcu::write` — `Bus::tick` drains it via `mcu.take_oam_dma_pending() -> Option<u8>` on the first APU tick after the write (mirroring how `DmcDma` drains `Apu::take_dmc_dma_request`). This preserves the natural `$4014` write path without threading `Bus` into `Mcu::write` via shared interior mutability, at the cost that `Bus::is_busy(&mcu)` takes `&mcu` to see the not-yet-drained pending. If borrow simplification is preferred, an alternative lands `$4014` handling through `Bus::request_oam_dma(page)` and makes `is_busy()` pure `self`; both satisfy the "one owner" criterion — the ADR locks the former (drain) variant and leaves the latter as a mechanical follow-up if the borrow proves noisy.

- Halt unification left explicit: DMC halt stays via `Cpu::try_freeze` → `cpu.frozen` (checked at `cpu.rs:663`), OAM halt stays via `BusOwner != Idle` causing `NesMachine::tick` to skip `cpu.tick`/`update_interrupt_lines` that dot (as today `tick_oam_dma` returning `true` did). A future unifying change may route OAM halt through `cpu.frozen` as well, but is not required for this seam and would be a separate decision.

### 3. Reset quiescence then queries one owner

```rust
// before (nes_machine.rs:160-177)
while (self.dmc_dma.is_busy() || self.cpu.mcu().oam_dma_active()) && drained < Self::RESET_DRAIN_LIMIT_DOTS {
    self.cpu.mcu_mut().suppress_new_dmc_dma_requests();
    self.tick(); drained += 1;
}
self.cpu.mcu_mut().reset(clock); self.dmc_dma.reset(); self.cpu.reset();

// after
while self.bus.is_busy(self.cpu.mcu()) && drained < Self::RESET_DRAIN_LIMIT_DOTS {
    self.bus.suppress_new_dmc_requests(self.cpu.mcu_mut());
    self.tick(); drained += 1;
}
debug_assert!(!self.bus.is_busy(self.cpu.mcu()), "DMA bus did not quiesce");
let clock = self.clock;
self.cpu.mcu_mut().reset(clock); // debug_asserts on oam_dma removed — Bus owns it
self.bus.reset();
self.cpu.reset();
self.cart_latch.reset();
```

`NesMcu::reset` debug asserts (`nes.rs:83-84` `assert!(oam_dma.is_none())`) are removed or moved into `Bus::reset`'s post-condition; `NesMcu::reset` retains only PPU/APU/deferred-write re-anchoring. `NesMachine` no longer has bare `cartridge_irq_latched/next`-style DMA bools; `BUS::is_busy` is the single quiescence query.

### 4. Module placement and locality with interrupt

`Bus` lives beside `interrupt` (`nes_core::bus` alongside `nes_core::interrupt`). `interrupt` owns line sampling (`CartridgeIrqLatch`, `ApuIrqSampler`, `InterruptLines`), `Bus` owns arbitration + reset quiescence and queries `CartridgeIrqLatch::level()` only if needed; neither module reaches into the other's state (ADR-0008 seam). `NesMachine::tick` threads them in order: `tick_ppu` → `cart_latch.capture_next` → `apu_sampler.sample` → `bus.tick` → `InterruptLines` → `cpu.update_interrupt_lines`.

## Consequences

- **Locality & leverage.** DMA knowledge concentrates behind one interface: arbitration policy (`DMC-wins`, `pause_cycles = 1` and its single condition), `is_busy` aggregation, and the one `suppress`/`reset` path. Deleting `bus.rs` would scatter that knowledge across `NesMachine::tick`/`reset` and `NesMcu::tick_oam_dma` again (deletion test). OAM cadence derived from get/put parity and write-on-get vs write-on-put diagrams remains documented in `bus.rs` next to the code it pins.

- **Testability (honest one-adapter seam).** The only arbitration policy is `DMC-wins`; no second adapter is expected. The seam is justified because the current integration wiring (`dmc_drove_bus` threading) is untested at unit level — collision tests hand-feed the bool (`nes/tests.rs:263` `tick_oam_dma(clock, true)`). After landing, collisions are exercised through `Bus::tick` with a fake `Cpu/Mcu` (or `MockNesDmaSupport` extended with an OAM fixture), pinning the doc/dma.md timelines without booting PPU/APU:

  - `oam_dma_pauses_on_dmc_read_collision` → Bus-level `pauses_on_dmc_read_collision`
  - `dmc_collision_at_start_of_oam_write_on_get/put` → Bus-level timeline
  - `oam_dma_completion_spans_follow_doc_diagrams` → `Bus` completion spans (514 vs 513)

  No second arbitration policy is invented to justify the seam.

- **DmcDma depth preserved.** The 7-state machine stays internal to `Bus` (or `bus/dmc_dma.rs`); its existing `NesDmaSupport` mock tests remain green without change. `Bus` is the only adapter over that internal seam in production; tests remain the second adapter keeping the seam real.

- **Acceptance gates.** Accuracy-critical: `cargo test --lib` smoke + `dmc_dma_during_read4` + `sprdma_and_dmc_dma` (DMA arbitration, doc/dma.md) + `cpu_interrupts_v2` subtest `4-irq_and_dma` (IRQ-relative DMA end positions) + `mmc3`/`vrc2-and-4` (A12 quantization not disturbed). Hygiene structural pins after landing:
  - `grep -rn dmc_drove_bus` is `0` outside `bus.rs`/`dmc_dma.rs`
  - `grep -rn tick_oam_dma` is `0` (deleted)
  - `grep -rn "oam_dma_active\|oam_dma_pending"` is `0` outside `bus.rs` (or only as drained queue)
  - Exactly one `bus.tick` call site in `NesMachine::tick` on the APU-clock path, one `bus.is_busy` call site in `NesMachine::reset`, one `bus.suppress_new_dmc_requests` call site in the drain loop, one `bus.reset` call site.
  - `cargo test --lib oam_dma` suites still green, now via `bus` paths. No capture re-bless (`dpcmletterbox`/`nmi_sync` frames untouched — bus path does not touch render).

- **Vocabulary.** CONTEXT.md gains under a new "DMA Bus" section (or "System Bus"): *Bus / DMA Bus*, *BusOwner*, *DMA arbitration (DMC-wins)*, *bus quiescence* — see ADR-0008 terms for interrupt-line sibling vocabulary.

- **Seam with Candidates 6 & 7.** `Cpu` narrowing (Candidate 6) retains `try_freeze`/`dma_halt_bus_addr` as `pub(crate)` implementation detail of the `Bus ↔ Cpu` seam; the public per-dot contract stays `{tick, update_interrupt_lines, reset, is_halted, microcodes_empty}`. Tracing cleanup (Candidate 7) does not touch `Bus`.

- **Ordering.** Implementable immediately after ADR-0008 (no new prerequisite beyond `InterruptSequences` table data, already landed). Parallelizable with Candidate 6; if 6 lands first, `Bus`'s `Cpu` dependency adapts to the narrowed surface but does not change shape.

## Alternatives revisited

Keeping the split is safe but keeps the scattered `is_busy`/`suppress`/`reset` and the hand-fed-bool gap; deferring leaves ADR-0008's forward reference open while friction accumulates. The adopted `Bus` pays one module for the concentration and the testability gap, explicitly as a one-adapter seam.
