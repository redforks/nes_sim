# MMC5 PPU–cartridge seam: where per-tile fetch interception lives (#69)

Decision-ready recommendation. See also: #67 (map), #70/#71/#72/#73 (blocked children).

## Current seams (nes_core)

- `nes/mapper.rs:215-246` — `Mapper` trait: `cpu_write`, `read_prg` ($6000-$7FFF
  only), `on_ppu_tick(scanline)`, `notify_vram_address(addr)`, `irq_pending`,
  gated by `CartridgeCaps` snapshot + `wants_vram_notify()`. The mapper owns no
  memory and performs **no data read**: `Cartridge::read_vram` (`mapper.rs:353`)
  goes straight to `CartridgeMemory::read_vram` (1 KB-bank window lookup).
- `nes/mapper/memory.rs` — 16-slot PPU window (8 CHR + 4 NT + 4 `$3000` aliases),
  NT slots bound to physical CIRAM banks by `apply_mirroring`. Fixed per-bank
  mapping **cannot express** ExRAM-as-NT / Fill / vertical-split, which are
  per-fetch overrides, not per-bank mappings.
- `nes/mapper/mmc3.rs` — `on_ppu_tick` maintains the A12 low-pass filter
  (reset on scanlines 261/239); `notify_vram_address` watches `$0000-$2FFF`,
  clocks IRQ on A12 rise after ≥12 low ticks (`MMC3_A12_LOW_FILTER_TICKS`).
  No render gating — pure address-stream filter.
- `nes/ppu.rs:538-577` — synthetic A12 notifies, only when
  `wants_vram_notify()`, only when `rendering_enabled()` (PPUMASK bg|spr from
  `$2001`), odd dots, speculative pattern addresses (`$0000`/`$1000` from
  PPUCTRL `$2000` bg/spr-table bits), plus sprite-prefetch dots 257-320.
  `$2006`/`$2007` writes also notify. `read_vram` (`ppu.rs:956-963`) and the
  render fetch `fetch_tile_data` (`ppu.rs:1207-1219`: NT, AT, plane-lo, plane-hi
  via `cartridge.read_vram`) have **no mapper callback** — side-effect-free.
- `view.rs` — `MachineView` is a read-model (CPU snapshot + shared `Mcu` borrow
  for peek/read); a mapper-side fetch hook inside the PPU render path does not
  touch it.

## How the references intercept NT/AT fetches

**tetanes `m005_exrom.rs`** — one side-effecting entry `chr_read(addr, ciram)`
plus pure `chr_peek`. `chr_read`: `$0000-$1FFF` bumps `fetch_count` (8×16 bank
swap at SPR_FETCH_START/END); `$2000-$3EFF` caches `tile_cache = addr & $3FF`
per NT fetch (ExAttr: later AT/pattern fetches derive from it), runs
3-consecutive-same-address scanline/in-frame detection
(`match_count == 2` → `in_frame = true, scanline = 0`, else `scanline++`, IRQ
compare), sets `reading`. NMI-vector fetch (`$FFFA/B`) clears `in_frame`.
`chr_peek` `$2000-$3EFF`: vsplit-region → ExRAM tile; ExAttr mode → AT replaced
by `ExRAM[tile_cache]>>6` replicated (`ATTR_MIRROR`), pattern bank from
`ExRAM&$3F | $5130<<6`; else per-slot enum `{ScreenA, ScreenB, ExRam, Fill}`
gated by ExRAM-mode nametable bit (else returns 0), Fill returns tile/attr.
Render gate: `$2000` 8×16 bit (`sprite8x16`) + fetch-count windows + exram mode.
**In-frame counter owned by the mapper.**

**Mesen2 `MMC5.h`** — one override `MapperReadVram(addr, op)` does everything:
`isNtFetch = $2000-$2FFF && (addr&$3FF)<$3C0`; `_splitTileNumber++` per NT fetch;
in-frame via `_needInFrame → _ppuInFrame` handshake in `DetectScanlineStart`
(3 identical NT reads), exit via `_ppuIdleCounter = 3` M2-without-/RD timeout in
`ProcessCpuClock` (NOT NMI-clear); split column `(_splitTileNumber+2)%42`,
scanline `_scanlineCounter` (+1 when tile ≥ 41); vsplit NT/AT/CHR overrides
returned inline; ExAttr mode 1 latches `_exAttributeLastNametableFetch` + 3-fetch
counter on NT fetch, overrides the next 3 fetches (palette, lo, hi). CHR A/B
bank select reads `$2000` bit 5 directly + `_ppuInFrame`. **Same shape:
mapper owns in-frame/split counters; PPU owns the render gate.**

## Recommended seam

Add a mapper-side PPU-read hook; keep detection where the references put it:

```rust
// Mapper trait (mapper.rs), default = None (fall through to window):
fn ppu_read(&mut self, addr: u16, render: PpuRenderState) -> Option<u8> { None }
// Cartridge::read_vram consults it BEFORE the memory-window lookup,
// gated by a new CartridgeCaps::ppu_read flag (zero cost when unset,
// same pattern as wants_vram_notify).
```

- **Who owns the in-frame counter: the mapper (MMC5 impl).** Mesen-style
  `{need_in_frame, ppu_in_frame, scanline, split_tile_number}` + 3-identical-NT
  detection on the NT-fetch stream; exit via idle timeout driven from the
  existing `on_ppu_tick` (no new CPU-cycle hook needed — PPU ticks every dot).
  tetanes' NMI-clear is Mesen-contradicted; prefer the idle-timeout exit.
- **NT/AT override return: `Option<u8>` per fetch address.** `None` = fall
  through to the 1 KB window (CIRAM/Fill/ExRAM all handled inside the mapper;
  ExAttr latches NT-fetch addr and overrides the following AT + 2 pattern
  fetches exactly like Mesen's 3-fetch counter). Keep the existing 4-slot NT
  enum untouched — MMC5 bypasses it per-fetch.
- **Rendering-gate source: the PPU.** Pass a small `PpuRenderState` (or two
  bools): `rendering_enabled` (PPUMASK `$2001` bg|spr, already computed in
  `tick()`), PPUCTRL `$2000` 8×16 bit, and scanline/dot or fetch phase.
  The mapper must NOT read `$2000` itself (Mesen does; we pass it instead —
  cheaper and keeps MachineView intact).
- **Per-dot-contract impact:** gate the hook behind `caps.ppu_read` so
  non-MMC5 boards pay nothing; call it only on genuine render fetches
  (`fetch_tile_data`'s 4 reads; later the dot-phase fetch if the PPU is
  refactored to per-dot). Our PPU renders per-pixel, not per-dot shifter, so
  "per-tile" = the 4 `read_vram` calls per `fetch_tile_data`, and the MMC5
  split-column math must count NT fetches, not dots. No `&mut` leaks into
  `MachineView` (render path only).
- **Files to touch:** `nes/mapper.rs` (trait method + `CartridgeCaps::ppu_read`
  + `Cartridge::read_vram` hook + `wants_ppu_read`), `nes/ppu.rs`
  (build `PpuRenderState` in `tick()`/fetch path, pass through `read_vram` /
  `fetch_tile_data`), new `nes/mapper/mmc5.rs` (counters, ExRAM/Fill/split/
  ExAttr, scanline IRQ state for #72), `nes/mapper/memory.rs` (ExRAM 1 KB
  backing + Fill constants; no window-slot changes).

Rejected: extending the 1 KB window slots to cover ExRAM/Fill (per-bank slots
cannot express per-tile/per-fetch overrides); putting in-frame state in the PPU
(both references own it in the mapper; PPU stays renderer).
