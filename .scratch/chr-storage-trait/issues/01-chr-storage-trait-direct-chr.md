Status: ready-for-agent

# Slice 1: ChrStorage trait + DirectChr + Mapper0 conversion

## What to build

Create the `ChrStorage` trait and `DirectChr` implementation in a new `chr_storage.rs` module under `nes_core/src/nes/mapper/`. Convert `Mapper0` to compose with `DirectChr` instead of its inline `chr_rom: [u8; 0x2000]` and `has_chr_ram: bool` fields.

The `ChrStorage` trait exposes exactly two methods:
- `fn read_chr(&self, address: u16) -> u8`
- `fn write_chr(&mut self, address: u16, value: u8)`

No constructor, no refresh, no writability flags on the trait.

`DirectChr` is a fixed-size `[u8; 0x2000]` array. `read_chr` and `write_chr` index by `address as usize % 0x2000`. Two constructors:
- `DirectChr::new(chr_rom: &[u8])` — panics if not exactly 8KB
- `DirectChr::empty()` — zeroed CHR RAM (for when no CHR ROM is present)

Mapper0's existing `chr_rom` field and `has_chr_ram` are replaced by a single `chr_storage: DirectChr` field. The `has_chr_ram` write gate stays in `Mapper0::write_chr` (calls `self.chr_storage.write_chr(...)` only when writable). Mapper0's `new()` and `default()` constructors are updated to initialize the storage.

## Acceptance criteria

- [ ] `ChrStorage` trait defined in `chr_storage.rs` module, publicly exported from `mapper/mod.rs`
- [ ] `DirectChr::new()` constructs from an 8KB slice; panics otherwise
- [ ] `DirectChr::empty()` returns zeroed CHR RAM
- [ ] `DirectChr::read_chr` wraps at 0x2000 boundary
- [ ] `DirectChr::write_chr` stores unconditionally
- [ ] Mapper0 fields `chr_rom` and `has_chr_ram` replaced by `chr_storage: DirectChr`
- [ ] Mapper0's `read_chr` delegates to `self.chr_storage.read_chr(addr)`
- [ ] Mapper0's `write_chr` gates on `has_chr_ram` (still a mapper field), then calls `self.chr_storage.write_chr(addr, val)`
- [ ] Unit tests for DirectChr verify: basic read/write, wrapping, empty construction
- [ ] All existing Mapper0 unit tests in `mapper0.rs` pass unchanged
- [ ] All existing Cartridge-level tests in `tests.rs` pass unchanged
- [ ] `cargo test` is green

## Blocked by

None — can start immediately
