Status: ready-for-agent

# PRD: Split ChrStorage from Cartridge

> **Contradicts ADR-0001** in several ways: that ADR rejected `dyn ChrStorage` dispatch, rejected `write_register` on the trait, and kept `read_chr`/`write_chr` on the `Cartridge` trait. This PRD supersedes those decisions for the reasons given below.

## Problem Statement

The `Cartridge` trait currently bundles two concerns: CPU-visible PRG/register logic (bank switching, IRQ, mirroring) and PPU-visible CHR data access (pattern table reads/writes with mapper-specific banking). This coupling forces the PPU to accept a `&mut dyn Cartridge` parameter just to read tile data, even though most of the Cartridge interface is irrelevant to rendering. It also prevents reusing CHR storage logic across mappers that share a banking scheme (e.g., CNROM uses the same 8 KB windowed pattern as some VRC variants), and makes `Cartridge` harder to mock in PPU tests — every test needs a full `Cartridge` impl even when only CHR access is exercised.

## Solution

Split CHR data access into an independent `ChrStorage` trait that the PPU owns directly. The `Cartridge` trait loses `read_chr` and `write_chr`. Each mapper that has CHR banking provides a companion `ChrStorage` implementation that independently decodes CPU bus writes (via `write_register`) to manage its internal banking state. The factory function `create_cartridge()` returns both a `Box<dyn Cartridge>` and a `Box<dyn ChrStorage>`; the `NesMcu` routes cartridge-range CPU writes to both components independently.

## User Stories

1. As a PPU implementor, I want the PPU to own its CHR storage directly, so that `tick()` / `read_vram()` / `write_vram()` access pattern data without routing through the `Cartridge` trait.

2. As a mapper implementor, I want `Cartridge` to only handle PRG banking, IRQ, and mirroring, so that I can focus on CPU-side logic without also managing CHR storage internals.

3. As a developer adding a new mapper, I want to reuse existing `ChrStorage` implementations (e.g., `DirectChr` for flat 8 KB, a parameterized banked window for CNROM-like patterns), so that I don't reimplement the same storage logic.

4. As a developer testing a mapper, I want to test PRG banking and CHR banking independently, so that each test exercises only one concern and I can construct minimal test fixtures.

5. As a developer debugging PPU rendering, I want to inject a known `ChrStorage` into the PPU without constructing a full cartridge, so that I can isolate rendering bugs from cartridge logic.

6. As a developer maintaining the PPU, I want to remove the `&mut dyn Cartridge` parameter from PPU internals, so that the PPU's external coupling is reduced.

7. As a NesMcu implementor, I want to route cartridge-range CPU writes to both the Cartridge and the ChrStorage, so that the ChrStorage can snoop register writes and update its banking state without the Cartridge needing to forward them.

8. As a developer of a mapper with entangled PRG/CHR register layouts (MMC3, MMC1), I want both the Cartridge and the ChrStorage to independently decode the same register write, so that the decoupling is clean even when the real ASIC had a shared register protocol.

## Implementation Decisions

### ChrStorage trait gains `write_register`

```rust
pub trait ChrStorage {
    fn read_chr(&self, address: u16) -> u8;
    fn write_chr(&mut self, address: u16, value: u8);
    fn write_register(&mut self, addr: u16, value: u8) {}  // default no-op
}
```

The `write_register` method receives raw CPU bus addresses and values. Each `ChrStorage` implementation decodes the subset of addresses relevant to its banking logic and ignores the rest. The default empty implementation keeps simple storages (DirectChr) zero-cost.

### Cartridge trait loses `read_chr` / `write_chr`

These methods are removed from the `Cartridge` trait. Every existing mapper implementation must delete its `read_chr` and `write_chr` methods. The `has_chr_ram` gating policy moves into the companion `ChrStorage` implementation.

### Factory creates both independently

`create_cartridge()` signature changes from:

```rust
pub fn create_cartridge(f: &INesFile) -> (Box<dyn Cartridge>, Mirroring)
```

to:

```rust
pub fn create_cartridge(f: &INesFile) -> (Box<dyn Cartridge>, Box<dyn ChrStorage>, Mirroring)
```

Each match arm in the factory constructs the mapper from `prg_rom` only and the companion `ChrStorage` from `chr_rom` only. The mapper and its ChrStorage are paired by construction but never share a direct reference.

### PPU owns the ChrStorage

`Ppu<R>` gains a `chr_storage: Box<dyn ChrStorage>` field, set at construction. All internal CHR access (`read_vram`, `write_vram`, `read_pattern_pixel`, `get_background_pixel`, `render_pixel`) goes through `self.chr_storage` instead of a `cartridge` parameter.

The `cartridge: &mut dyn Cartridge` parameter is **kept** in `Ppu::tick`, `Ppu::read`, and `Ppu::write` — but only for `cartridge.notify_vram_address()`, which is A12 monitoring for MMC3 IRQ timing. The cartridge parameter is no longer used for data access.

`Ppu::peek` loses its cartridge parameter entirely (it was only used for `read_chr`).

### NesMcu routes cartridge writes to both

When `NesMcu::write` receives a CPU write in the cartridge address range (`>= 0x4020`), it calls:

1. `self.cartridge.write(addr, value)` — handles PRG banking, IRQ, returns `CartridgeOperation`
2. `self.ppu.write_chr_register(addr, value)` — forwards to `ChrStorage::write_register`

No `CartridgeOperation` variant is needed for CHR — the routing is orthogonal.

### CartridgeOperation unchanged

`CartridgeOperation` keeps only its existing variants (`None`, `UpdateNametableMirroring`). No new variant for CHR is added.

### Mapper companions

Each mapper with CHR banking gets a named `ChrStorage` companion:

| Mapper | Companion | Banking scheme |
|--------|-----------|----------------|
| 0, 2, 7 | `DirectChr` | Flat 8 KB, no banking |
| 3 | New `CnromChrStorage` | 1 × 8 KB bank select via $8000-$FFFF writes |
| 1 | New `Mmc1ChrStorage` | 2 × 4 KB (or 1 × 8 KB) via serial register protocol |
| 4 | New `Mmc3ChrStorage` | 8 × 1 KB via bank-select + bank-data registers |
| 21-25 | New `Vrc24ChrStorage` | 8 × 1 KB with pin-variant address decoding |
| 34 | New `Mapper34ChrStorage` | BxRom (flat) / Nina001 (2 × 4 KB RAM) |
| 87 | New `J87ChrStorage` | 4 × 8 KB band switching |

Each companion independently tracks the subset of register state relevant to CHR banking (e.g., `Mmc3ChrStorage` tracks `bank_select` bit 7 for CHR mode and bits 0-2 for target register index, plus bank registers 0-5).

### Mapper constructors simplified

Mapper constructors no longer take `chr_rom`. Example:

```rust
// Before:
MMC3::new(prg_rom, chr_rom, mirroring_locked, alt_irq)
// After:
MMC3::new(prg_rom, mirroring_locked, alt_irq)
```

The mapper no longer allocates or manages CHR source data.

### Test independence

Mapper unit tests are trimmed to only PRG/IRQ tests. All CHR-banking tests move to their companion ChrStorage's test module.

## Testing Decisions

A good test for this feature:
- Exercises external behavior (what CHR data comes back after register configuration), not internal caching state.
- Constructs the ChrStorage directly without involving the Cartridge.
- Tests each ChrStorage implementation as an isolated unit through its trait API.

### Seam 1: ChrStorage API (primary, extends existing `chr_storage.rs` tests)

Each ChrStorage implementation gets a test module exercising `read_chr`, `write_chr`, and `write_register`. Tests verify:
- Bank switching produces the correct pattern data from the expected source bank
- Writes to CHR RAM persist and don't corrupt other banks
- Register writes outside the CHR-relevant address range are ignored
- Initial state matches the ROM's CHR data

Prior art: existing `windowed_tests` module in `chr_storage.rs` that tests `WindowedChr` via `read_chr`/`write_chr`.

### Seam 2: Mapper API (existing, trimmed)

Mapper tests that previously tested `read_chr`/`write_chr` are removed. Remaining tests cover PRG banking, mirroring control, IRQ timing, and cartridge RAM.

Prior art: existing `mapper0::tests`, `mmc3::tests`, etc.

### No integration seam (deferred)

No NesMcu-level routing tests. The routing is simple enough (two independent `write` calls for the same address range) that unit tests on the individual components provide sufficient coverage.

## Out of Scope

- **Performance optimization**: The PPU accesses CHR through a `dyn ChrStorage` vtable call per pattern byte. This is acceptable for now; inlining or monomorphization can be pursued separately.
- **Removing `&mut dyn Cartridge` from PPU entirely**: The `notify_vram_address` dependency remains. A future refactor could move A12 monitoring into ChrStorage or a callback, but that's deferred.
- **Unified banking DSL**: Each companion ChrStorage implements its own register decoding. A shared banking-configuration protocol is out of scope.
- **Supporting additional mappers**: Only the mappers listed in the companion table above are in scope. Others (if any exist) are deferred.

## Further Notes

- `WindowedChr`'s inherent `write_chr_with_source`, `refresh`, and `copy_from_source` methods become part of the companion ChrStorage's internal implementation. The trait only exposes `read_chr`, `write_chr`, and `write_register`.
- `DirectChr` stays as-is: `write_register` is a no-op. Mappers 0, 2, and 7 all use `DirectChr` constructed from the full `chr_rom`.
- The CONTEXT.md domain glossary needs updating: "Cartridge" no longer dispatches CHR operations; "ChrStorage" gains `write_register`; "Mapper" no longer owns `has_chr_ram` policy (it moves to the companion ChrStorage). ADR-0001 is superseded.
