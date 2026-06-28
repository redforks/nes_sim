# 01 — Define PpuCapabilities struct and ppu_capabilities() on Cartridge trait

Status: ready-for-agent

## Parent

PRD: `.scratch/cartridge-capability-query/PRD.md`

## What to build

Add the vocabulary for cartridge PPU capability queries. Define a new `PpuCapabilities` struct in the mapper module with three public boolean fields matching the three rarely-overridden Cartridge methods. Add a `ppu_capabilities()` method to the `Cartridge` trait with a default returning all-false. Override this method in the two mappers that have IRQ hardware: MMC3 (all-true) and Vrc24 (on_ppu_tick + irq_pending true, notify_vram_address false). No PPU call-site changes in this slice.

## Acceptance criteria

- [ ] `PpuCapabilities` struct defined in `nes_core::nes::mapper` with `#[derive(Debug, Clone, Copy, Default)]` and three `pub` bool fields: `on_ppu_tick`, `notify_vram_address`, `irq_pending`
- [ ] `PpuCapabilities::default()` returns all-false
- [ ] `Cartridge` trait has `fn ppu_capabilities(&self) -> PpuCapabilities { PpuCapabilities::default() }`
- [ ] MMC3 overrides `ppu_capabilities()` returning all-true
- [ ] Vrc24 overrides `ppu_capabilities()` returning `{ on_ppu_tick: true, irq_pending: true, notify_vram_address: false }`
- [ ] All other 9 cartridge implementations compile unchanged (rely on trait default)
- [ ] `cargo build` succeeds for `nes_core`
- [ ] Unit test verifies default returns `(false, false, false)`

## Blocked by

None — can start immediately.
