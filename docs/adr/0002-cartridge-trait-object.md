# ADR-0002: Cartridge as trait object, ChrStorage split to PPU

## Status

Accepted (commit `34cd9f8`, `a0a46e2`, `3763dd5`)

## Context

`Cartridge` was a 10-variant enum (`Mapper0`, `Mapper2`, `Mapper3`, `Mapper7`,
`Mapper34`, `MapperJ87`, `MMC1`, `MMC3`, `Vrc24`, `Test`) with every method
dispatched through a match arm per variant — ~163 lines of repetitive
boilerplate with no semantics. Adding a new mapper meant touching every method.
CHR storage was tangled inside each mapper, meaning the PPU had to route
pattern-table reads/writes through `Cartridge` even though the PPU is the sole
consumer.

## Decision

Two independent migrations landed in sequence:

### 1. Cartridge enum → trait object (`34cd9f8`)

Replace the enum with:

```rust
pub trait Cartridge {
    fn read(&mut self, address: u16) -> u8;
    fn peek(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, value: u8) -> CartridgeOperation;
    fn on_ppu_tick(&mut self, _scanline: u16, _dot: u16, _rendering_enabled: bool) {}
    fn notify_vram_address(&mut self, _addr: u16) {}
    fn irq_pending(&self) -> bool { false }
}
```

Sensible defaults remove the need for every mapper to stub no-op methods.
`create_cartridge` returns `Box<dyn Cartridge>`. Callers use `&dyn Cartridge`
/ `&mut dyn Cartridge`. The `TestCartridge` struct implements the trait
directly instead of being a variant.

### 2. ChrStorage split from Cartridge (`a0a46e2`, preceded by `3763dd5`)

CHR read/write was removed from the `Cartridge` trait entirely and extracted
into a separate `ChrStorage` trait:

```rust
pub trait ChrStorage {
    fn read_chr(&self, address: u16) -> u8;
    fn write_chr(&mut self, address: u16, value: u8);
    fn write_register(&mut self, _addr: u16, _value: u8) {}
}
```

Base implementations:
- `DirectChr` — fixed 8KB array, direct addressing (for Mapper0, Mapper2, Mapper7)
- `WindowedChr` — source `Vec<u8>` + cached 8KB read window, with inherent
  refresh/write-through methods (not on the trait)

Mapper-specific implementations: `CnromChrStorage`, `Mmc1ChrStorage`,
`Mmc3ChrStorage`, `Vrc24ChrStorage`, `Mapper34ChrStorage`, `J87ChrStorage`.

The PPU now owns `Box<dyn ChrStorage>` directly — `Ppu::new` takes it as a
parameter, and `create_cartridge` returns a triple `(Box<dyn Cartridge>,
Box<dyn ChrStorage>, Mirroring)`. The Cartridge trait no longer has any CHR
concerns; it handles only PRG-ROM access, mapper register writes, IRQ
generation, and PPU tick callbacks.

`has_chr_ram` gating stays in each mapper, not in the storage layer.

## Considered Options

- **Generic parameter on PPU** (rejected): `Ppu<R, C: ChrStorage>` avoids the
  vtable call but forces monomorphisation of all PPU code per mapper, ballooning
  binary size and compile time. `dyn ChrStorage` has negligible overhead for
  the call frequency.
- **Keep read_chr/write_chr on Cartridge trait** (rejected): Every call required
  `&mut` access to the whole Cartridge just for CHR, preventing the ownership
  split where PPU holds CHR and Cartridge holds PRG/IRQ.
- **Single ChrStorage with config** (rejected): Mapper-specific bank switching
  patterns (MMC1 vs MMC3 vs VRC24) are too divergent for a unified
  configuration model.
- **Cartridge-level trait dispatch for CHR** (rejected, in ADR-0001): Would
  require `Box<dyn ChrStorage>` inside Cartridge and lose mapper-specific
  bank-switching logic — the split went further by moving ownership to PPU.

## Consequences

- Adding a new mapper is now a single `impl Cartridge for ...` block; no enum
  variant or match arm to touch.
- PPU owns CHR storage directly, eliminating the indirection of routing
  pattern-table access through the cartridge.
- `CartridgeOperation` (the write return type) is now the only communication
  channel from mapper to the rest of the system (currently just mirroring
  updates).
- Sensible trait defaults (`{}` methods, `false` return) keep mapper
  implementations concise.
- vtable dispatch on hot paths (PRG reads, PPU ticks) introduces minimal
  overhead; the `ChrStorage` trait is also `dyn` but its two methods
  (`read_chr`, `write_chr`) are fast and branch-predictable.
- `TestCartridge` is trivial — just a `prg_rom` array and a three-method impl
  block.
