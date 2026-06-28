# 02 — PPU caches capabilities and guards all call sites

Status: ready-for-agent

## Parent

PRD: `.scratch/cartridge-capability-query/PRD.md`

## What to build

Integrate the capability cache into the PPU. In `Ppu::new()`, call `cartridge.ppu_capabilities()` once and store the result as a new field `cartridge_caps: PpuCapabilities`. Then guard every vtable call to the three rarely-implemented methods:

- **`notify_vram_address` rendering block** — wrap the entire block in `tick()` (the match on dot ranges that computes fetch types) behind `if self.cartridge_caps.notify_vram_address`
- **`notify_vram_address` register r/w** — guard each of the three call sites in $2006, $2007 write, and $2007 read handlers
- **`on_ppu_tick`** — guard the single call at the end of `tick()`
- **`cartridge_irq_pending()` bridge** — add early return `if !self.cartridge_caps.irq_pending { return false; }` before the vtable call

The full test suite (`just cpu-test && just rom-test`) must pass, confirming MMC3 and Vrc24 IRQ behavior is not regressed.

## Acceptance criteria

- [ ] `Ppu` struct gains `cartridge_caps: PpuCapabilities` field, populated in `Ppu::new()` via `cartridge.ppu_capabilities()`
- [ ] `notify_vram_address` rendering block (the match on dot/fetch-type in `tick()`) is guarded by `self.cartridge_caps.notify_vram_address`
- [ ] Three `notify_vram_address` register r/w call sites are each guarded by `self.cartridge_caps.notify_vram_address`
- [ ] `on_ppu_tick` call site is guarded by `self.cartridge_caps.on_ppu_tick`
- [ ] `cartridge_irq_pending()` bridge has early return on `!self.cartridge_caps.irq_pending`
- [ ] `cargo build` succeeds for all crates
- [ ] `just cpu-test` passes
- [ ] `just rom-test` passes

## Blocked by

- `01-define-capabilities`
