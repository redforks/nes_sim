Status: ready-for-agent

## What to build

Remove the global `SYSTEM_CLOCK` AtomicU64 and all its accessor functions. By this point all modules receive `SystemClock` as a parameter, so the global is dead code kept only for the parallel transition.

### Delete from nes_core

- Remove `static SYSTEM_CLOCK: AtomicU64` from `lib.rs`
- Remove `inc_system_clock()` from `lib.rs`
- Remove `get_system_clock()` from `lib.rs`
- Remove `get_system_cycles()` from `lib.rs`
- Remove `#[cfg(test)] set_system_cycles()` from `lib.rs`
- Remove `use std::sync::atomic::{AtomicU64, Ordering}` from `lib.rs` imports
- Remove `inc_system_clock()` call from `NesMachine::process_frame()` — now only `self.tick()` advances the clock internally

### Delete from nes_cpu_test

- Remove `use nes_core::{get_system_cycles, inc_system_clock}` from `image.rs`
- Remove `inc_system_clock()` call from `MachineWrapper::tick()` — `NesMachine::tick()` handles it internally
- Remove `use nes_core::get_system_cycles` from `monitor_test_status.rs`
- Replace `get_system_cycles()` calls in `MonitorTestStatus::end()` with `self.machine.clock().0`
- Remove `use nes_core::get_system_cycles` from `report.rs`
- Replace `get_system_cycles()` calls in `ReportPlugin::start()` and `ReportNesTestResult::end()` with `self.machine.clock().0`

### Cleanup tests

- Remove any remaining `inc_system_clock()` or `get_system_cycles()` references from `nes_core` tests
- All tests must use local `SystemClock` counters or `NesMachine::clock()`

## Acceptance criteria

- [ ] `SYSTEM_CLOCK` static removed from `lib.rs`
- [ ] All clock accessor functions removed
- [ ] No `inc_system_clock()` in `process_frame()`
- [ ] No `inc_system_clock()` in `MachineWrapper::tick()`
- [ ] `nes_cpu_test` plugins use `NesMachine::clock()` accessor
- [ ] Zero references to `SYSTEM_CLOCK`, `inc_system_clock`, `get_system_clock`, `get_system_cycles`, `set_system_cycles` in the entire workspace
- [ ] `just unit-test` passes
- [ ] `just cpu-test` passes
- [ ] `just rom-test` passes

## Blocked by

- [#1 — SystemClock infrastructure + CPU wiring](01-system-clock-infra-cpu.md)
- [#2 — Wire SystemClock through APU, DMC DMA, OAM DMA](02-wire-apu-dma.md)
- [#3 — Remove Cpu::drain_microcodes()](03-remove-drain-microcodes.md)
