# The memory `Bus` owns the master `SystemClock`

Move `SystemClock` into `Bus`, so we do not need to pass both `bus` and
`clock` parameters. See ADR-0017.

## Decision

`Bus<M>` (`nes_core/src/bus.rs`) gains a `clock: SystemClock` field, next
to `mcu` (ADR-0015/0017). It is the single home of the master clock: the
machine loop advances it through `Bus::advance()`, and machine-level,
CPU-level, DMA-level and plugin seams read dots *from the bus* instead of
threading a separate `clock` value:

- `Bus` API: `clock() -> SystemClock` (current dot), `advance() ->
  SystemClock` (capture + increment, machine-loop authority),
  `set_clock()` (test/harness staging hatch, e.g. DMC put/get alignment
  tests).
- `Cpu::tick(&mut self, bus: &mut Bus<M>, plugin)` — the executed dot is
  `bus.clock()`. The drain `Cpu::run_to_instruction_boundary(bus, plugin)`
  advances the same clock one dot per microcode.
- `Machine::tick()` / `run_to_instruction_boundary()` /
  `drain_to_instruction_boundary(plugin)` drop their `clock` parameters;
  `Machine` no longer exposes clock plumbing.
- `NesMachine` deletes its `clock: SystemClock` field. Its tick loop
  captures `bus.clock()` at the top for the device ticks (PPU, APU
  sampler, interrupt lines, `NesMcu::set_clock`) and advances once after
  the dot completes — bus clock reads *inside* the dot therefore see the
  dot in progress, matching the previous two-line
  `let clock = self.clock; self.clock = self.clock.inc();` capture
  semantics.
- `DmaBus::tick(&mut self, cpu, bus)` reads its gate
  (`clock.is_apu_clock()`) off the bus; `DmcDma::tick(&mut self, cpu,
  bus: &mut Bus<M>)` (bound `M: Mcu + DmcDmaMem`) reads its put/get phase
  off `bus.clock()`.
- `Plugin::start(view)` / `end(view)` lose the trailing `SystemClock`;
  plugins read `view.clock()` (`MachineView` already carries it).

Seams that receive *only* a clock (the device layer: `Apu::write`,
`Mapper::cpu_write`, `NesMcu::set_clock`/`reset(clock)`/`tick_apu(clock)`,
`Cpu::update_interrupt_lines`, and the test helper
`DmaBus::tick_oam_for_test`) keep their value parameter — there is no bus
in their call chain to read from.

## Considered options

- `Cpu` owns the clock (`Cpu { clock }`): the machine's master time would
  live on the instruction processor, contradicting the domain language
  (the CPU drives *into* the bus; it does not time it) and making the
  advance authority murky across `run_to_instruction_boundary`.
- Value-threading everywhere (status quo before this ADR): every seam grew
  `(bus, clock)` parameter pairs that always came from the same master —
  the redundancy is what this ADR removes.
- Advance at the top of the machine dot: `Bus::advance()` early made
  in-dot `bus.clock()` reads observe the *next* dot, silently skewing DMA
  phase logic (caught by `nes_machine::tests::reset_drains_in_flight_…`);
  the increment must happen after the dot's device work.

## Consequences

- There is exactly one master clock in the system, held by the bus a CPU
  (or DMA arbiter) acts *through*; "pass both `bus` and `clock`" seams are
  gone.
- GLOSSARY.md *SystemClock / Tick / Run to instruction boundary /
  Per-dot contract / MachineView* updated to the bus-owned wording.
- Tests stage dots with `bus.set_clock(SystemClock(t))`
  (harness `tick` helpers stage once); `tick_oam_for_test` keeps an
  explicit clock for hand-fed phase sweeps.
- The device layer still receives `SystemClock` values; a future change
  there would need the same ownership pass.
