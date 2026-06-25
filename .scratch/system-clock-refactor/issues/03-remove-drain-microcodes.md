Status: ready-for-agent

## What to build

Remove `Cpu::drain_microcodes()` and replace it with caller-driven loops. This fixes a design bug: `drain_microcodes()` was advancing multiple CPU microcode steps without ticking PPU or APU between steps. A properly interleaved loop must advance the clock and tick all devices between each microcode step.

### New CPU method

- Add `Cpu::microcodes_empty(&self) -> bool` — returns true when the microcode queue is empty

### Assertion in set_pc()

- Add an assertion in `Cpu::set_pc()` that the microcode queue is empty. Because `set_pc()` changes the program counter, any pending microcodes from a previous PC would be invalid.
- The assertion ensures callers have drained before changing PC.

### Caller changes

Replace all `drain_microcodes()` call sites with:

```rust
while !cpu.microcodes_empty() {
    machine.tick(clock);
    clock = clock.inc();
    // tick PPU, APU, DMA here too
}
```

The exact call sites depend on the caller context — each site must ensure full device interleaving (PPU, APU, DMA, cartridge IRQ) between microcode steps.

### Remove drain_microcodes

- Delete `Cpu::drain_microcodes()` method entirely

## Acceptance criteria

- [ ] `Cpu::microcodes_empty()` added
- [ ] `Cpu::set_pc()` asserts microcode queue is empty
- [ ] All `drain_microcodes()` call sites replaced with interleaved loop
- [ ] `drain_microcodes()` method deleted from `Cpu`
- [ ] `just unit-test` passes
- [ ] `just cpu-test` passes
- [ ] `just rom-test` passes

## Blocked by

- [#1 — SystemClock infrastructure + CPU wiring](01-system-clock-infra-cpu.md)
