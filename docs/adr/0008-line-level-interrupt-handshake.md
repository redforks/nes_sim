# Line-level interrupt handshake as a typed interface

The PPU↔CPU NMI/IRQ handshake was three mechanisms with no shared type: PPU set a bare `nmi_race_cancel: bool` during `tick_ppu` (`ppu.rs:615` on `$2000`/$2002 race, `ppu.rs:823-824` on `$2002` read) and `NesMachine::tick` consumed it ~50 lines later (`nes_machine.rs:129-139`) as `if take_nmi_race_cancel() { cancel_nmi_rising_edge(clock) }` after a separate `nmi_line_out()->bool → update_nmi_line(bool,clock)` call — nothing ties the flag to the level it races; APU IRQ was a `bool` sampled BEFORE `tick_apu` (`nes_machine.rs:118-121`) with a 15-line prose comment pinning the one-dot skew (flag transition dot → CPU-visible `dot+1`, `cpu_interrupts_v2` 3-nmi_and_irq); cartridge IRQ was a two-stage latch of two free bools in `NesMachine` (`cartridge_irq_next` every dot post-`tick_ppu`, `cartridge_irq_latched` on `cpu_tick` dots only, `nes_machine.rs:93/100/118`) while mapper counters (`MMC3::clock_irq`, `Vrc24`) are PPU-dot-clocked and `mmc3_irq_tests` pins the CPU-cycle quantization. Five files (`ppu.rs`, `nes_machine.rs`, `cpu.rs`, `apu.rs`, `mapper.rs`) had to agree with only ordering and comments enforcing the contract. We decided to make the handshake a real interface: one `InterruptLines` value per dot, produced atomically, consumed atomically, with the skew and latch as named structure.

## Considered options

- **`tick_ppu` returns `InterruptLines { nmi, race_cancel, irq }` event only (PPU-centric bundle)**: rejected as half-measure — fixes the PPU-level/flag tie but leaves `NesMachine::tick` doing two CPU calls (`update_nmi_line` then `cancel_nmi_rising_edge`) that can still be reordered or partially forgotten. Depth requires atomicity at both ends.
- **`Cpu::update_interrupt_lines(lines,clock)` owns same-tick retract internally but PPU keeps two getters**: rejected — fixes call-order at CPU seam but PPU can still emit `nmi_race_cancel` without a level (or a new PPU site could forget the flag). The type tie must live at the producer.
- **Machine helper `sync_nmi(clock)` with no new types**: rejected — shallow wrapper; hides ordering inside one function but introduces no type enforcement; a future caller (savestate replay, headless tick) can still bypass the helper.
- **Keep prose-pinned ordering inside `NesMachine::tick` (reject candidate)**: rejected — the five-file agreement has no locality; the APU `+1` and cartridge quantization are load-bearing for `cpu_interrupts_v2`/`mmc3_irq_tests` but the compiler cannot enforce them; three prior tickets (Candidates 1–3) already moved sibling implicit contracts into table data / exhaustive matches for the same reason.
- **Defer until DMA Bus (Candidate 5) or Cpu narrowing (Candidate 6) lands**: rejected — Candidate 1's unified `consumed_through` edge protocol (ADR-0006) is the only prerequisite, and it is settled; the handshake seam is orthogonal to Bus ownership and Cpu surface and can be specified now.

## Decision

**Adopt**, with the following interface direction (implementation happens after map hand-off; this ADR is the spec):

1. **New module `nes_core::interrupt` (or `nes::interrupt` if crate layout prefers)** owns the interrupt-line vocabulary and tiny state machines. It exports:
   - `struct NmiLines { level: bool, race_cancel: bool }` — atomically produced by PPU; `race_cancel` means the level's same-dot assertion never asserted (vblank-race suppression at `vbl_set_cycle == cycle`).
   - `struct InterruptLines { nmi: NmiLines, irq_level: bool }` — per-dot bundle drove into CPU. `irq_level` is already resolved to "visible this dot" (APU `+1` and cartridge quantization applied before construction). Field names keep edge vs level visible (`nmi: NmiLines` + `irq_level: bool`).
   - `struct ApuIrqSampler` (or free function `sample_apu_irq_for_cpu`) — samples `apu.request_irq()` *before* `tick_apu(clock)` and documents/names the `pre-tick → visible next dot` mapping; `Tick` calls `sample` then `tick_apu`, builds `irq_level = sampled_apu || cart_latched`.
   - `struct CartridgeIrqLatch { next: bool, latched: bool }` — owns the two-stage latch: `capture_next(level)` every dot (post-`tick_ppu`), `latch_on_cpu_tick(cpu_tick)` copies `next→latched` on CPU dots, `level()` returns `latched`. The two bools leave `NesMachine`; quantization lives in one place.

2. **PPU produces `NmiLines` atomically.** Replace `pub fn nmi_line_out()->bool` + `pub fn take_nmi_race_cancel()->bool` with one producer, e.g. `pub fn nmi_lines(&mut self)->NmiLines` (or `tick_ppu()->NmiLines`). `tick_ppu` still sets/clears `v_blank` but also sets `nmi_race_cancel` inside that call; the return bundles them so they cannot diverge. Internally `nmi_race_cancel` remains a field or becomes local to the return, but is never exposed alone.

3. **CPU consumes `InterruptLines` atomically.** Replace the published seam `set_irq(bool,clock)` + `update_nmi_line(bool,clock)` + `cancel_nmi_rising_edge(clock)` with one published method `pub fn update_interrupt_lines(&mut self, lines: InterruptLines, clock: SystemClock)` that (a) calls `irq_detector.update_irq_input(lines.irq_level, clock, track)` and (b) calls `nmi_detector.update_nmi_input(lines.nmi.level, clock)` then *inside the same call* `if lines.nmi.race_cancel { cancel_rising_edge_at(clock) }`. Old methods become `pub(crate)` (or deleted) — one external seam. The two-step `update`+`cancel` cannot be partially called.

4. **Race-retract integrates with the unified edge-consumption path (ADR-0006).** `cancel_rising_edge_at(clock)` is the third writer of `consumed_through` alongside `consume_hijack_edge`/`consume_latched_edge`. It does `mark_consumed()` (stamping `consumed_through` to `max(pending, input)`) and clears `nmi_input`/`last_sampled_level`/`nmi_line_changed_at`. Guard remains `nmi_line_changed_at == clock.cycles() && nmi_input` (or equivalently `asserted_since == clock`). Do not model retract as a fake hijack site deadline — keep it as a named third writer at the Tick/NMI seam, documented alongside the two table-declared sites.

5. **`Tick` (`NesMachine::tick`) threads the bundle:**
   ```rust
   let clock = self.clock;
   self.cpu.mcu_mut().tick_ppu();
   if cpu_tick { self.cpu.mcu_mut().tick_zapper(); }
   // sample cartridge next every dot, latch on CPU dots
   self.cart_latch.capture_next(self.cpu.mcu().cartridge_irq_pending());
   if cpu_tick { self.cart_latch.latch_on_cpu_tick(); }

   if !self.reset_requested {
       let apu_sampled = self.apu_sampler.sample_before_tick(&self.cpu.mcu()); // pre-tick
       let irq_level = apu_sampled || self.cart_latch.level();
       self.cpu.mcu_mut().tick_apu(clock);
       // ... DMC/OAM DMA as before ...
       let nmi = self.cpu.mcu().ppu().nmi_lines(); // {level, race_cancel} atomically
       let lines = InterruptLines { nmi, irq_level };
       self.cpu.update_interrupt_lines(lines, clock);
       let result = if clock.is_cpu_clock() { self.cpu.tick(&mut self.p, clock).0 } else { ... };
       // no separate take_nmi_race_cancel() call
       result
   }
   ```
   The APU comment becomes the sampler's interface doc; the cartridge comment becomes `CartridgeIrqLatch`'s invariant; the PPU flag comment becomes `NmiLines`'s field doc.

## Consequences

- **Locality & leverage.** Interrupt knowledge concentrates in one module behind one `update_interrupt_lines` interface. Deleting `interrupt.rs` would scatter line sampling across 5 files again (deletion test). `CartridgeIrqLatch` is testable without booting a PPU; `ApuIrqSampler`'s `+1` is a named unit test (`sample before tick vs after`).
- **Seam with Candidates 5 & 6.** `CartridgeIrqLatch` stays in `interrupt`, Bus queries `level()` / `latch_on_cpu_tick()` — Bus owns arbitration + reset quiescence (`is_busy`, suppression), interrupt owns line sampling. `Cpu` narrows to `tick` + `update_interrupt_lines` + `reset` + `is_halted` + `microcodes_empty` (+ snapshot) as the published per-dot contract (Candidate 6); old `set_irq`/`update_nmi_line`/`cancel_nmi_rising_edge` become `pub(crate)` for internal wiring only.
- **Reset.** Latch clears on `NesMachine::reset` alongside DMA quiesce; clock time `SystemClock` keeps running so `consumed_through` stamps remain monotonic.
- **Acceptance gates.** `cargo test` + `just`-equivalents: `cpu_interrupts_v2` (hijack + APU+1 pin), `vbl_nmi_timing` + `ppu_vbl_nmi` 06/07 (race-retract pin), `mmc3` + `vrc2-and-4` + `dmc_dma_during_read4`/`sprdma_and_dmc_dma` (latch quantization), plus structural pins: `grep -rn take_nmi_race_cancel` is 0 outside `ppu.rs` (or deleted), `grep -rn cartridge_irq_latched/next` is 0 inside `nes_machine.rs` (moved to module), one `update_interrupt_lines` call site in `tick`.
- **Golden invariants unchanged.** Deadlines stay table-declared `deadline_dots` from sequence start (ADR-0006); `GOLDEN_BUS_CYCLE` unchanged; capture baselines (`dpcmletterbox`, `nmi_sync` frames) untouched (render path not moved).
- **Vocabulary.** CONTEXT.md gains under "CPU Interrupt Handshake" (or "System Interrupt Lines"): *interrupt lines*, *NMI line bundle / NmiLines*, *APU IRQ skew / sampled IRQ*, *cartridge IRQ latch*, *same-tick retract / race-retract* — see ADR-0006 terms for sibling window/poll/hijack vocabulary.
