# PRD: Cartridge PPU Capability Query

Status: ready-for-agent

## Problem Statement

The `Cartridge` trait defines three PPU-interaction methods — `on_ppu_tick`, `notify_vram_address`, and `irq_pending` — that are called through `Box<dyn Cartridge>` trait objects on every PPU tick and every CPU cycle. Only 2 of 11 cartridge implementations (MMC3, Vrc24) actually implement these methods; the other 9 rely on the no-op defaults. Each call goes through the vtable, even when the cartridge does nothing.

## Solution

Add a capability-query method `ppu_capabilities()` to the `Cartridge` trait that returns a `PpuCapabilities` struct — three booleans indicating whether each PPU-interaction method is meaningfully implemented. The PPU caches this struct once in its constructor, then gates every vtable call behind a branch read of the cached bool. For the 9 cartridges using defaults, no vtable call occurs.

## User Stories

1. As an emulator developer, I want the PPU to skip `on_ppu_tick` vtable calls for cartridges that don't implement scanline IRQ, so that tick-loop performance improves for the vast majority of games.
2. As an emulator developer, I want the PPU to skip `notify_vram_address` vtable calls — including the surrounding rendering-loop arithmetic — for cartridges that don't monitor A12, so that per-dot overhead is eliminated.
3. As an emulator developer, I want the IRQ-polling bridge to short-circuit without a vtable call when the cartridge has no IRQ hardware, so that the per-CPU-cycle IRQ check is essentially free.
4. As a mapper author, I want overriding `ppu_capabilities()` to be optional, so that the 9 simple mappers need zero changes and carry no additional boilerplate.
5. As a tester, I want the existing MMC3 and Vrc24 CPU test ROMs to continue passing, so that IRQ-dependent games are not regressed.

## Implementation Decisions

### `PpuCapabilities` struct

A new public struct in the `nes_core::nes::mapper` module, defined alongside the `Cartridge` trait:

```rust
#[derive(Debug, Clone, Copy, Default)]
pub struct PpuCapabilities {
    pub on_ppu_tick: bool,
    pub notify_vram_address: bool,
    pub irq_pending: bool,
}
```

All three fields are `pub`, `Copy`, `Clone`, and `Default` (all-false).

### New `Cartridge` trait method

```rust
fn ppu_capabilities(&self) -> PpuCapabilities {
    PpuCapabilities::default()
}
```

Default implementation returns all-false. Only mappers with IRQ hardware override it.

### PPU caches capabilities in constructor

`Ppu::new()` calls `cartridge.ppu_capabilities()` once and stores the result as a new field `cartridge_caps: PpuCapabilities`.

### Guarded call sites

**`notify_vram_address` rendering block:** The entire block in `tick()` (lines 313-358) — which computes fetch types and calls `notify_vram_address` — is wrapped in `if self.cartridge_caps.notify_vram_address { ... }`. This block has no other side effects.

**`notify_vram_address` register r/w:** The three call sites in `write_ppureg()` ($2006, $2007 write) and `read_vram_and_inc()` ($2007 read) are each wrapped in `if self.cartridge_caps.notify_vram_address { ... }`.

**`on_ppu_tick`:** The single call at the end of `tick()` is wrapped in `if self.cartridge_caps.on_ppu_tick { ... }`. The `prev_scanline` variable is computed regardless (trivial cost, already computed for other use).

**`cartridge_irq_pending()` bridge:** Adds an early return: `if !self.cartridge_caps.irq_pending { return false; }` before the vtable call.

### Mapper overrides

- **MMC3**: `ppu_capabilities()` returns `PpuCapabilities { on_ppu_tick: true, notify_vram_address: true, irq_pending: true }`
- **Vrc24**: `ppu_capabilities()` returns `PpuCapabilities { on_ppu_tick: true, notify_vram_address: false, irq_pending: true }`
- All other 9 mappers use the trait default (all-false)

## Testing Decisions

- **What makes a good test**: Verify behavior, not vtable call counts. The regression safety net is the existing CPU test ROM suite.
- **Testing approach**: Run `just cpu-test` and `just rom-test` — MMC3 IRQ test ROMs exercise the guarded call paths end-to-end. A guard placed incorrectly would break IRQ counter behavior and be caught.
- **Unit test**: Add a test verifying `PpuCapabilities::default()` returns `(false, false, false)`.
- **Prior art**: The existing test suite uses the `nes_cpu_test` binary with ROMs from test-data/ to assert on exit codes.

## Out of Scope

- Removing or renaming the three `Cartridge` trait methods — they remain as-is.
- Performance benchmarking or measurement of the improvement.
- A separate trait or downcasting approach for capability detection.
- Changing any MMC3 or Vrc24 IRQ logic — this is purely a dispatch optimization.
- Adding support for mappers not yet implemented (e.g., MMC5).

## Further Notes

- The `PpuCapabilities` struct intentionally mirrors the method names rather than using more descriptive but different names, to keep the mapping trivial and grep-friendly.
- The outer guard on the `notify_vram_address` rendering block is safe because that block's sole purpose is A12 pattern-table notification — it has no rendering side effects.
- The `cartridge_irq_pending()` bridge is the highest-frequency call site (once per CPU cycle, i.e., once per 3 PPU ticks) and therefore the biggest win.
