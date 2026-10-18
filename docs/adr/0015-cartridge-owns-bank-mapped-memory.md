# Cartridge owns bank-mapped memory; the mapper only rewires slots

## Status

accepted

## Context

`Cartridge` was a trait implemented by ten mappers, and each mapper owned its own ROM/RAM: `prg_rom` as a fixed array or `Vec`, CHR as `DirectChr` or a `Vec` plus an inline cache window (CnRom `chr_cache`/`chr_source`, J87 `chr_bands`, MMC3 `chr_source`+`chr_bank_offsets`, MMC1 `source`), and the four nametables lived in `Ppu` behind an address transform (`Mirroring::name_table_offset`). Mapper mirroring changes travelled out of `Cartridge::write` as `CartridgeOperation::UpdateNametableMirroring` for `NesMcu` to apply to the PPU.

That shape duplicated memory (a ROM slice read through a window copy), duplicated modulo arithmetic (`bank % count` in every mapper), put the board's expensive memory inside the chip, and made one access path split across three owners (PPU → mapper read → cache → source).

## Decision

The board model moves into the cartridge:

- `Cartridge` is a **struct** (`memory`, `mapper: Box<dyn Mapper>`, `caps`). It owns **all** cartridge memory: PRG ROM, PRG RAM, CHR (ROM or RAM) and the four nametables.
- All cartridge memory is a pool of **1 KB `Bank`s**, uniform for PRG and PPU alike; larger banking (4/8/16/32 KB) is a run of consecutive slots.
- Access is **table-driven**: `CartridgeMemory` holds a 16-slot PPU window and a 64-slot PRG window of bank numbers. A read is one table lookup plus `bank[addr & 0x3FF]`.
- Both windows are sized as powers of two so the slot index is masked into range rather than bounds-checked: the PPU window covers `$0000-$3FFF` (8 pattern-table slots + 4 nametable slots + the 4 `$3000-$3EFF` alias slots, which hold the same bank numbers as the slots they repeat), and the PRG window keeps 24 padding slots above the 40 that `$6000-$FFFF` maps to. The only `unsafe` in the accessors is the pool lookup, which relies on the invariant that every window entry is written by the setters in `memory.rs` (`debug_assert!` guards it).
- `Mapper` is the chip: `cpu_write(addr, value, cycle, &mut CartridgeMemory)` plus PPU clock/address notifications and IRQ pending. **Mappers own no memory and read none.** `CartridgeOperation` is deleted. The one exception is `Mapper::read_prg`, consulted only for `$6000-$7FFF`, because VRC2 boards carry no PRG-RAM and answer those reads with the bus byte; CPU fetches never pay for it.
- **Mirroring is slot entries**, not an address transform: `Mirroring` maps to four nametable slot entries, so `name_table_offset`, `Nametable::set_mirroring`, `Ppu::set_mirroring` and `Ppu.nametable` disappear.
- CHR ROM/RAM gating is one bit on the cartridge (`chr_is_ram`), not a per-mapper `has_chr_ram`; PRG-RAM enable is a flag on the memory struct that mappers toggle, while open bus stays a CPU-bus decision in `NesMcu`.
- `read_chr`/`write_chr` become `Cartridge::read_vram`/`write_vram` over the whole cartridge PPU space (pattern tables + nametables), so no caller distinguishes CHR from nametable.
- `NesMcu` owns `Cartridge` **by value**; `Ppu` takes `&Cartridge`/`&mut Cartridge` and calls the cartridge's PPU hooks unconditionally. `Cartridge` holds the mapper's `caps` snapshot and forwards a hook only when the chip declares it: there is no `Cartridge::caps()`, so no caller reads capability bits (the `NesMcu.cartridge_caps` copy is deleted).

## Considered Options

- **Mapper computes the mapping per access** (`fn map_addr(addr) -> usize`): no table to keep in sync, but a dyn call and per-access arithmetic on the tile hot path, and every mapper keeps its own read code. Rejected in favour of eager table rewriting on register writes (MMC3 already worked this way via `chr_sync_banks`).
- **Keep memory in mappers, only rename the trait**: minimal diff, but leaves the duplication and the window caches that lose CHR-RAM writes on bank switch (CnRom).
- **Mapper returns bank commands for `Cartridge` to apply** instead of receiving `&mut CartridgeMemory`: that is `CartridgeOperation` generalised — the indirection this refactor exists to delete. Rejected in favour of disjoint borrows through a separate memory struct.

## Consequences

- CnRom's banked CHR-RAM writes now land in the bank instead of a window copy, so they survive switching away and back. J87's four 8KB bands become 32 mapped slots.
- Every mapper that overrides `read`/`read_chr`/`write_chr` loses those methods; the only per-mapper state left is registers, IRQ counters, PRG-RAM enable and the mirroring latch.
- **There is no `Mapper::reset`.** The reset button does not rewind a mapper on real hardware; bank windows are power-on state mutated only by mapper writes. Do not add one for symmetry.
- **The MMC3 A12 notifier stays synthetic.** `Ppu::tick` keeps emitting modelled addresses by dot parity; deriving IRQ clocking from actual cartridge VRAM accesses would look cleaner and silently retime scanline IRQs.
- Mapper unit tests no longer exercise read paths directly; they assert window/slot state and read back through the cartridge.
- **The capability snapshot is consulted inside `Cartridge`.** `on_ppu_tick`/`notify_vram_address`/`irq_pending` gate the mapper call on it, and a mapper must declare a hook in `Mapper::caps` for its override to be called at all. Because the PPU keeps no copy, the per-dot A12 notification block in `Ppu::tick` now runs its dot-phase arithmetic on every board instead of short-circuiting; that is the deliberate cost of this shape, and the rationale for the snapshot in the first place is kept in `.scratch/cartridge-capability-query/PRD.md`.

## Migration

Three steps, each green under `just unit-test` and `just passed`:

1. Rename the trait `Cartridge` → `Mapper`; add the `Cartridge` struct delegating to `Box<dyn Mapper>`; drop `Box<dyn Cartridge>`, the `dyn` parameters and the duplicated `cartridge_caps` field (`Cartridge` consults its own snapshot; no caller reads `caps`).
2. Move `Nametable` into `Cartridge` and rename `read_chr`/`write_chr` → `Cartridge::read_vram`/`write_vram` in the same step: once the nametable lives in the cartridge, one accessor for `$0000-$2EFF` is the natural seam, so the rename has nothing left to be done in.
3. Introduce `Bank`/`CartridgeMemory`/windows, convert the mappers, delete `CartridgeOperation`, `Mirroring::name_table_offset`, `DirectChr` and every per-mapper window cache.

The plan originally put the nametable move first; it moved to step 2 because `Cartridge` was still a trait in step 1, so "move the nametable into the cartridge" would have meant adding nametable storage to all ten mappers."
