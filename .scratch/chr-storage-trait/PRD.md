Status: ready-for-agent

# ChrStorage Trait — Extract Shared CHR Read/Write Logic Into Reusable Types

## Problem Statement

Each mapper struct in the codebase independently manages its CHR (Character Data) storage with its own field names, storage strategies, and duplicated read/write logic. There is no shared abstraction, so adding a new mapper requires reimplementing the same CHR access patterns from scratch. This makes the codebase harder to maintain and more error-prone.

## Solution

Introduce a `ChrStorage` trait with two concrete implementations — `DirectChr` (fixed 8KB array, direct addressing) and `WindowedChr` (source `Vec<u8>` + cached 8KB read window) — to serve as reusable CHR storage components. Each mapper composes with the appropriate concrete `ChrStorage` type, delegating CHR read/write operations to it while keeping mapper-specific logic (bank-switching, writability gating) in the mapper itself.

## User Stories

1. As a developer adding a new mapper, I want to reuse `DirectChr` or `WindowedChr` instead of writing CHR storage from scratch, so that I can focus on mapper-specific PRG bank switching and interrupt logic.

2. As a code reviewer, I want a single source of truth for CHR access patterns, so that I don't have to verify the same read/write semantics across every mapper file.

3. As a developer refactoring an existing mapper, I want to replace its inline CHR fields with a composed `ChrStorage` type, so that the mapper's CHR surface area is reduced to a delegation call.

4. As a tester, I want `DirectChr` and `WindowedChr` to have their own unit tests in isolation, so that CHR storage correctness is verified once rather than implicitly through every mapper's tests.

5. As a developer working on the PPU, I want the `cartridge.read_chr()` / `write_chr()` API to remain unchanged, so that my PPU code doesn't need any modifications.

6. As a maintainer, I want the `Cartridge` enum dispatch to remain unchanged, so that no mapper-match-arm logic needs to be touched during this extraction.

7. As a developer, I want `has_chr_ram` write gating to remain in each mapper, so that CHR storage types stay policy-free and different mappers can express different write policies (ignore, gate, unconditional).

8. As a developer using `WindowedChr`, I want window refresh and write-through to be inherent methods (not on the trait), so that `DirectChr` doesn't need no-op stubs for refresh logic that only applies to bank-switched mappers.

## Implementation Decisions

### Trait definition

The `ChrStorage` trait exposes exactly two methods:

- `fn read_chr(&self, address: u16) -> u8`
- `fn write_chr(&mut self, address: u16, value: u8)`

No constructor methods. No refresh hooks. No writability flags.

### DirectChr

A fixed-size `[u8; 0x2000]` array. `read_chr` and `write_chr` index directly by `address as usize % 0x2000`. No caching, no bank logic. Construction via `DirectChr::new(chr_rom: &[u8])` — panics if not exactly 8KB. A `DirectChr::empty()` constructor creates zeroed CHR RAM.

### WindowedChr

Two storage layers:
- Source `Vec<u8>` holding the full CHR memory (potentially up to 128KB or more)
- Cached `[u8; 0x2000]` window for fast read access

`read_chr` reads from the cache. `write_chr` writes to the cache only (satisfying the trait).

Inherent methods (not on trait):
- `write_chr_with_source(&mut self, address: u16, value: u8, source_offset: usize)` — write-through to both cache and source Vec
- `refresh(&mut self, offset_into_source: usize)` — copy `[offset_into_source..offset_into_source + 0x2000]` from source into cache
- `source_len(&self) -> usize` — for mapper to validate bank counts

Construction via `WindowedChr::new(chr_mem: Vec<u8>)` — takes ownership of the full CHR memory. Window is initially zeroed; the mapper must call `refresh` after construction or on bank register changes.

### Composition, not inheritance

Each mapper holds a concrete `DirectChr` or `WindowedChr` as a named field. The mapper's `read_chr`/`write_chr` methods delegate to the storage type. The mapper may wrap the call with its own policy (e.g. `has_chr_ram` gating).

Mappers never use `Box<dyn ChrStorage>` or generic parameters — the concrete type is known at compile time.

### No Cartridge-level changes

The `Cartridge` enum and its `read_chr`/`write_chr` match dispatch stay exactly as they are. Each mapper variant still exposes `pub fn read_chr(&self, address: u16) -> u8` and `pub fn write_chr(&mut self, address: u16, value: u8)`.

### Mapper-specific writability gates stay

`has_chr_ram` and the associated write guard remain in each mapper. The `ChrStorage` types always write when called. Mapper3 (CNROM) expresses its "never write CHR" policy by never calling `write_chr` at all.

### Special mappers unchanged

Mapper34 (BNROM/NINA-001) and Mapper87 (J87) keep their bespoke CHR storage. They are not converted to use `DirectChr` or `WindowedChr`.

## Testing Decisions

### What makes a good test

- Test external behavior (what value does `read_chr` return?), not implementation details (which internal field was accessed)
- Existing mapper-level tests must pass without changes — the refactor is internal to each mapper
- ChrStorage unit tests test the contract of each concrete type in isolation

### Test modules

| Module | Type | Location | What it covers |
|--------|------|----------|----------------|
| `ChrStorage` unit tests | New | `nes_core/src/nes/mapper/` (new file `chr_storage.rs`) | `DirectChr`: basic read/write, wrap at 0x2000; `WindowedChr`: read from cache, write to cache, write-through, refresh |
| Mapper-level tests | Existing | `mapper0.rs`, `mmc3.rs`, etc. | Each mapper's existing `read_chr`/`write_chr` tests (unchanged) |
| Cartridge-level tests | Existing | `nes_core/src/nes/mapper/tests.rs` | Full `create_cartridge` + CHR read/write round-trips (unchanged) |

### Prior art

- `mapper0.rs` line 106-122: tests `read_chr` with known values and `write_chr` when CHR ROM is absent
- `mmc3.rs` lines 413-467: tests CHR bank switching via `read_chr` after multiple register writes
- `mmc3.rs` lines 470-478: tests CHR RAM write-through via `write_chr` + `read_chr`
- `tests.rs` lines 96-104: tests `cartridge.write_chr`/`read_chr` with CHR RAM (mapper 1)

## Out of Scope

- Changing the `Cartridge` enum or its dispatch
- Changing the PPU's CHR access code (`ppu.rs` lines 526, 546, 571)
- Extracting or refactoring PRG ROM storage
- Extracting Mapper34 or Mapper87 CHR storage
- Changing mapper construction (`new()` signatures)
- Performance optimization — same storage strategy as current code
- Adding support for new mappers — reuse is the goal, not adding new hardware

## Further Notes

- The ADR at `docs/adr/0001-chr-storage-trait.md` captures the architectural decision and rejected alternatives
- The glossary at `CONTEXT.md` defines the terminology (ChrStorage, DirectChr, WindowedChr)
- Each mapper conversion can be done incrementally as a separate PR — this PRD covers the extraction of the types and conversion of at least one mapper of each pattern (e.g. Mapper0 for DirectChr, MMC3 for WindowedChr) to validate the design
