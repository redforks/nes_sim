Status: ready-for-agent

# PRD: Flatten `Machine` out of `NesMachine`

## Problem Statement

`NesMachine<P, R, D>` currently wraps `Machine<P, NesMcu<R, D>>` as a private `machine` field. `Machine<P, M>` is itself a thin wrapper holding only a `Cpu<M>` and a `P` (plugin), with every method being a one-line delegation to the inner `Cpu`. This means every field access in `NesMachine` goes through two levels of indirection — `self.machine.cpu()`, `self.machine.mcu()`, `self.machine.tick(clock)` — when there is no meaningful abstraction being provided by the intermediate `Machine` layer. The chain through `Machine` adds boilerplate without adding value, and prevents `NesMachine` from directly accessing `Cpu` internals that are already public.

Additionally, downstream consumers (`nes_cpu_test` with `tcp-server`, `nes_web`) need direct access to `cpu()`, `mcu()`, and `mcu_mut()` on `NesMachine`, but these are currently unavailable because `Machine`'s accessors aren't re-exported. Flattening makes these naturally available.

## Solution

Remove the `machine: Machine<P, NesMcu<R, D>>` field from `NesMachine` and instead store `cpu: Cpu<NesMcu<R, D>>` and `p: P` directly. Every delegation through `self.machine` becomes a direct call. The `Machine<P, M>` struct remains in `machine.rs` as a standalone convenience for non-NES use cases (e.g., `BinMachine` in `nes_cpu_test`), but `NesMachine` no longer depends on it.

## User Stories

1. As a developer reading `NesMachine::tick()`, I want to see the CPU tick and interrupt detection inline rather than hidden behind `self.machine.tick(clock)`, so that the execution flow is more transparent.

2. As a developer maintaining `NesMachine`, I want to access `self.cpu` and `self.p` directly instead of routing through `self.machine.cpu()` / `self.machine.cpu_mut()`, so that the code is simpler to navigate and refactor.

3. As a developer adding new features to `NesMachine`, I want to avoid adding yet another delegation method on `Machine` or re-export accessor on `NesMachine`, so that each new feature touches fewer files.

4. As a developer working on the `tcp-server` feature in `nes_cpu_test`, I want `NesMachine` to expose `cpu()`, `cpu_mut()`, `mcu()`, and `mcu_mut()` directly, so that the TCP server can read CPU registers and NES memory without workarounds.

5. As a developer using `Machine<P, M>` for non-NES testing (`BinMachine` with `RamMcu`), I want `Machine` to remain available and unchanged, so that CPU test ROMs continue to work without modification.

6. As a developer debugging PPU/APU/DMA timing edge cases, I want the `tick()` method to be as flat and explicit as possible, so that I can reason about clock-by-clock execution without mentally unwrapping delegation layers.

7. As a reviewer, I want the flattening to be a purely mechanical refactor with zero behavioral change, so that I can verify correctness by confirming the call graph is preserved.

## Implementation Decisions

### `NesMachine` gains `cpu` and `p` fields directly

```rust
pub struct NesMachine<P, R: Render, D: AudioDriver> {
    cpu: Cpu<NesMcu<R, D>>,
    p: P,
    cartridge_irq_latched: bool,
    cartridge_irq_next: bool,
    dmc_dma: DmcDma,
    clock: SystemClock,
}
```

The `machine: Machine<P, NesMcu<R, D>>` field is removed. Every existing `self.machine.something()` call is replaced:

| Before | After |
|--------|-------|
| `self.machine.cpu()` | `&self.cpu` |
| `self.machine.cpu_mut()` | `&mut self.cpu` |
| `self.machine.mcu()` | `self.cpu.mcu()` |
| `self.machine.mcu_mut()` | `self.cpu.mcu_mut()` |
| `self.machine.tick(clock)` | `self.cpu.tick(&mut self.p, clock); self.cpu.detect_interrupt(clock);` |
| `self.machine.reset()` | `self.cpu.reset()` |
| `self.machine.set_pc(pc, clock)` | `self.cpu.set_pc(pc, clock)` |
| `self.machine.microcodes_empty()` | `self.cpu.microcodes_empty()` |

### `NesMachine` exposes `cpu()`, `cpu_mut()`, `mcu()`, `mcu_mut()`

These accessors become available on `NesMachine` since the `Cpu` is now a direct field. Downstream consumers (`nes_cpu_test` TCP server, `nes_web`) can use them directly.

### `Machine<P, M>` remains unchanged

The struct stays in `machine.rs` with its full API. Only `NesMachine` no longer wraps it. Uses of `Machine` directly (e.g. `BinMachine` in `nes_cpu_test` for CPU test ROMs with `RamMcu`) are unaffected.

### Construction simplified

`NesMachine::new()` no longer creates a `Machine` intermediary — it constructs `cpu: Cpu::new(mcu)` and `p: plugin` directly in the struct literal.

### No change to `NesMcu`, `Cpu`, `Plugin`, or `Mcu` traits

These types are unaffected. The `Cpu` struct already has `pub` register fields and public `mcu()`, `mcu_mut()` methods. No trait or data structure changes are needed at lower layers.

## Testing Decisions

A good test for this refactor verifies that execution traces (register state, memory state, cycle counts) are identical before and after the change. Since this is a pure structural refactor, the behavior must not change at all.

### Seam 1: Existing `NesMachine` tests (primary, sole seam)

The existing test module in `nes_machine.rs` exercises `tick()`, `process_frame()`, `reset()`, `set_pc()`, and controller interaction. These tests should pass as-is after flattening with zero modifications. No new tests are needed; the existing coverage is sufficient to verify no behavioral regression.

Prior art: existing `nes_machine.rs` test module, and `Machine`'s own test module (`machine/tests.rs`).

### `Machine` test module is unaffected

The existing `Machine` tests continue to exercise standalone `Machine` behavior unchanged.

## Out of Scope

- **Removing `Machine` entirely**: `Machine<P, M>` remains for non-NES use cases (e.g., `BinMachine` in `nes_cpu_test`). Only its relationship to `NesMachine` changes.
- **Changing `Machine`'s API**: No methods added, removed, or renamed on `Machine`.
- **Adding new capabilities to `NesMachine`**: This refactor only flattens the struct. Any new accessors (`cpu()`, `mcu()`, etc.) already exist on `Cpu` and become naturally accessible, but no new feature-level functionality is introduced.
- **Fixing the `nes_web` compilation errors** (`flush_audio` visibility, `press_button`/`release_button` naming): These are separate pre-existing issues in the WASM build and are addressed by their own fixes, not this refactor.

## Further Notes

- The `Machine` abstraction was originally useful when `NesMachine` was simpler. As `NesMachine` has grown to own its clock, DMA state, and IRQ latching, `Machine` has become a redundant pass-through. This refactor aligns the code with the current complexity of `NesMachine`.
- This refactor is a prerequisite for the `tcp-server` feature to work with `NesMachine` — the TCP server needs `cpu()` and `mcu()` accessors that the flattened struct naturally exposes.
- The single-seam testing strategy (existing `NesMachine` integration tests) is sufficient because the refactor is entirely mechanical with no new runtime paths.
