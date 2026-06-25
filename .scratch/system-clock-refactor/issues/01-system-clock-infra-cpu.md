Status: ready-for-agent

## What to build

Add `SystemClock` infrastructure and thread it through the CPU call chain, while keeping the global `SYSTEM_CLOCK` ticking in parallel (dual-run migration).

### Infrastructure

- Add `SystemClock::inc(self) -> SystemClock` — consumes self, returns new value (clock + 1)
- Add `clock: SystemClock` field to `NesMachine`, initialized to `SystemClock::default()`
- Add `pub fn clock(&self) -> SystemClock` getter on `NesMachine`

### NesMachine wiring

- `NesMachine::tick()` advances `self.clock = self.clock.inc()` at the top of the method
- Pass `self.clock` to `self.machine.tick(clock)` (which calls CPU)
- Keep `inc_system_clock()` in `process_frame()` — both clocks advance in parallel during transition

### CPU module

- `Cpu::tick(&mut self, clock: SystemClock)` — gate execution on `clock.cpu_clock_phase()` instead of `get_system_clock()`
- `Cpu::detect_interrupt(&mut self, clock: SystemClock)` — gate on `clock.cpu_clock_phase()` instead of `get_system_clock()`
- `IrqDetector::update_irq_input(&mut self, line: bool, clock: SystemClock)` — use `clock.0` instead of `get_system_cycles()` for the assertion timestamp
- `Machine::tick(&mut self, clock: SystemClock)` — passes clock through to `self.cpu.tick(clock)` and `self.cpu.detect_interrupt(clock)`
- Remove `use crate::{get_system_clock, get_system_cycles}` imports from `cpu.rs`
- `drain_microcodes()`: pass `clock` through; the method stays but no longer calls `inc_system_clock()`

### PPU

PPU does NOT receive a clock parameter — it does not need one. Add only when actually needed (YAGNI).

## Acceptance criteria

- [ ] `SystemClock::inc()` added and returns `SystemClock(self.0 + 1)` or wrapping_add(1)
- [ ] `NesMachine` has `clock` field and `clock()` getter
- [ ] `NesMachine::tick()` advances `self.clock` and passes it to CPU via `Machine::tick(clock)`
- [ ] `process_frame()` still calls `inc_system_clock()` (global kept in sync)
- [ ] CPU, IrqDetector, Machine tick methods all accept `SystemClock` parameter and use it instead of the global
- [ ] CPU-related tests pass using local `SystemClock` counters instead of `inc_system_clock()`
- [ ] `just unit-test` passes
- [ ] `just cpu-test` passes

## Blocked by

None — can start immediately.
