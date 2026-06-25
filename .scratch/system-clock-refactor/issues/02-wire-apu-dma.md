Status: ready-for-agent

## What to build

Thread `SystemClock` through APU, DMC DMA, and OAM DMA tick methods. These currently read the global clock directly; switch them to receive the clock as a parameter from the caller.

### APU module

- `Apu::tick(&mut self, clock: SystemClock)` — call `clock.is_apu_clock()` instead of `get_system_clock().is_apu_clock()`
- Remove `use crate::get_system_clock` import from `apu.rs`

### DMC DMA module

- `DmcDma::tick(&mut self, cpu: &mut Cpu, clock: SystemClock)` — call `clock.is_apu_get_clock()` instead of `get_system_clock().is_apu_get_clock()`
- Remove `use crate::get_system_clock` import from `dmc_dma.rs`

### OAM DMA (in NesMcu)

- `NesMcu::tick_oam_dma(&mut self, clock: SystemClock)` — call `clock.is_even_cpu_cycle()` instead of `get_system_clock().is_even_cpu_cycle()`
- Remove `use crate::get_system_clock` import from `nes.rs`

### NesMachine wiring

- `NesMachine::tick()` passes `self.clock` to `self.machine.mcu_mut().tick_apu(clock)`, `self.dmc_dma.tick(cpu, clock)`, and `self.machine.mcu_mut().tick_oam_dma(clock)`
- `NesMcu::tick_apu(&mut self, clock: SystemClock)` forwards to `self.apu.tick(clock)`
- Global `inc_system_clock()` still called in `process_frame()` (parallel transition)

### PPU

PPU does NOT receive a clock parameter — it does not need one.

## Acceptance criteria

- [ ] `Apu::tick()` accepts `clock: SystemClock` and uses it instead of the global
- [ ] `DmcDma::tick()` accepts `clock: SystemClock` and uses it instead of the global
- [ ] `NesMcu::tick_oam_dma()` accepts `clock: SystemClock` and uses it instead of the global
- [ ] APU/DMC/OAM modules no longer import global clock functions
- [ ] `NesMachine::tick()` passes `self.clock` to all APU/DMA methods
- [ ] `just unit-test` passes
- [ ] `just cpu-test` passes
- [ ] `just rom-test` passes

## Blocked by

- [#1 — SystemClock infrastructure + CPU wiring](01-system-clock-infra-cpu.md)
