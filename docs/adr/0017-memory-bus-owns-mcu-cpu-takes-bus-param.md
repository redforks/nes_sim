# Memory `Bus` owns the `Mcu`; CPU methods take `&mut Bus`

The CPU's memory access previously lived inside `Cpu<M>` as a generic `Mcu`
field, which forced the whole CPU surface (`Cpu`, its ~60 memory-touching
methods, `Microcode::exec`, every `ValueSourceTrait` helper) to thread `M`
through generics, and made `Cpu` look like a bus owner in a crate where the
bus is a named architectural concept (ADR-0009). We split the two: `Cpu` is
now non-generic and holds no memory; a new `Bus<M>` (`nes_core::bus`) owns
the address-space `Mcu`, and every CPU method that drives memory takes
`&mut Bus<M>` as an explicit parameter (`Cpu::tick(&mut self, bus:
&mut Bus<M>, plugin, clock)`). Owners hold `cpu: Cpu` and `bus: Bus<M>` as
paired fields (`Machine<P, M> {cpu, bus, p}`, `NesMachine {cpu, bus:
Bus<NesMcu>, dma_bus, ...}`).

Two renames make the vocabulary unambiguous: the former DMA-arbitration
`Bus` (ADR-0009) is renamed `DmaBus` (`nes_core::dma_bus`), keeping `tick`
(`DmaBus::tick(&mut self, cpu: &mut Cpu, bus: &mut Bus<NesMcu<R, D>>,
clock) -> BusOwner`), and the `NesDmaSupport` mega-trait it used to see the
CPU through is deleted: CPU freeze operations (`try_freeze`/`unfreeze`) are
direct `Cpu` methods, while memory/APU operations move to a new narrow
`DmcDmaMem` trait (`read_mem` / `take_dmc_dma_request` /
`supply_dmc_byte`) implemented by `NesMcu`, so the DMC seam no longer
mocks the CPU wholesale.

**Considered options**

- Keep the zero-copy `Mcu`-inside-`Cpu` generic: no extra parameter noise,
  but `Cpu<M>` generics infect tests, plugins, and every helper; and the
  ownership story contradicts the `Bus`-owns-the-bus domain language.
- A `Memory` trait object inside `Cpu`: dynamic dispatch on every access,
  worse than the static `&mut Bus` parameter it replaces.

**Consequences**

- `Machine::mcu()` / `NesMachine::mcu()` forward to `Bus`; there is no
  `Cpu::mcu()` anymore, and `Cpu::view` is gone (build views through
  `MachineView::new(cpu.snapshot(), bus.mcu(), clock)`).
- `Machine` gains `drain_to_instruction_boundary(plugin, clock)` (the
  setup-time drain with an explicit plugin) next to
  `run_to_instruction_boundary`.
- `NesMachine` holds a third field (`dma_bus: DmaBus`) next to
  `cpu`/`bus`; reset quiescence talks to `dma_bus`.
- Test harnesses model the pair: `_core` tests use a local
  `Harness<M: Mcu> {cpu, bus}` (deref to `Cpu`, `mcu()`/`mcu_mut()`
  forwarded), and DMC DMA unit tests stage the pending bus cycle on a real
  `Cpu` (`stage_pending_cycle_for_test`) plus a mocked `DmcDmaMem`.
