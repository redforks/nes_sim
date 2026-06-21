Status: ready-for-agent

# Slice 2: WindowedChr + MMC3 conversion

## What to build

Implement `WindowedChr` in `chr_storage.rs` and convert `MMC3` to compose with it instead of its inline `chr_mem: Vec<u8>`, `current_chr: [u8; 0x2000]`, and `has_chr_ram` fields.

`WindowedChr` holds two storage layers:
- Source `Vec<u8>` — the full CHR memory (up to 128KB or more)
- Cached `[u8; 0x2000]` window — fast read path

Construction via `WindowedChr::new(chr_mem: Vec<u8>)` — takes ownership. Window is initially zeroed; the mapper calls `refresh` after construction.

Trait methods (from `ChrStorage`):
- `read_chr` — reads from the cached window
- `write_chr` — writes to the cache only

Inherent methods (not on trait):
- `write_chr_with_source(&mut self, address: u16, value: u8, source_offset: usize)` — write-through to both cache and source Vec
- `refresh(&mut self, offset_into_source: usize)` — copies `[offset..offset + 0x2000]` from source Vec into cache
- `source_len(&self) -> usize` — for mapper bank-count validation

Mapper-side logic that stays in MMC3:
- `has_chr_ram` write gating
- Bank mapping computation (8×1KB slots, mode-dependent ordering)
- Calls to `chr_storage.refresh(offset)` on bank register changes (in `sync_chr_banks`)
- Calls to `chr_storage.write_chr_with_source(addr, val, source_offset)` in `write_chr` (mapper computes source offset from slot mapping)

## Acceptance criteria

- [ ] `WindowedChr` defined in `chr_storage.rs`, publicly exported
- [ ] `WindowedChr::new()` allocates source from given Vec, window zeroed
- [ ] `WindowedChr::read_chr` reads from the cached window
- [ ] `WindowedChr::write_chr` writes to the cached window only
- [ ] `WindowedChr::write_chr_with_source` writes both cache and source Vec
- [ ] `WindowedChr::refresh(offset)` copies source segment into cache
- [ ] `WindowedChr::source_len()` returns Vec length
- [ ] MMC3 fields `chr_mem`, `current_chr`, `has_chr_ram` replaced by `chr_storage: WindowedChr`
- [ ] `sync_chr_banks` calls `chr_storage.refresh(...)` for each slot
- [ ] MMC3's `write_chr` gates on `has_chr_ram`, computes source offset, calls `chr_storage.write_chr_with_source(addr, val, source_offset)`
- [ ] MMC3's `read_chr` delegates to `chr_storage.read_chr(addr)`
- [ ] Unit tests for WindowedChr verify: cache read hit, write-through writes both layers, refresh copies source to cache, source_len
- [ ] All existing MMC3 unit tests pass unchanged (bank switching, CHR RAM write-through, IRQ, mirroring)
- [ ] All Cartridge-level tests pass unchanged
- [ ] `cargo test` is green

## Blocked by

- #01: ChrStorage trait + DirectChr + Mapper0 (trait must exist first)
