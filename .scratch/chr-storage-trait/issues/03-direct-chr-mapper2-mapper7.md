Status: ready-for-agent

# Slice 3: Convert Mapper2 and Mapper7 to DirectChr

## What to build

Apply the same `DirectChr` composition pattern from Slice 1 to Mapper2 (UxROM) and Mapper7 (AxROM). Both use a fixed 8KB CHR array with `has_chr_ram` gating — identical structure to Mapper0.

Replace each mapper's inline CHR fields with a `chr_storage: DirectChr` field:
- **Mapper2**: Replace `chr: [u8; CHR_ROM_SIZE]` and `has_chr_ram: bool` → `chr_storage: DirectChr`
- **Mapper7**: Replace `chr: [u8; CHR_SIZE]` and `has_chr_ram: bool` → `chr_storage: DirectChr`

Write gating (`has_chr_ram`) stays in each mapper as a bool field or is delegated to DirectChr's always-writes behavior with the mapper calling `write_chr` only when writable.

## Acceptance criteria

- [ ] Mapper2's `chr` and `has_chr_ram` replaced by `chr_storage: DirectChr`
- [ ] Mapper2's `read_chr` delegates to `chr_storage.read_chr(addr)`
- [ ] Mapper2's `write_chr` gates on `has_chr_ram`, calls `chr_storage.write_chr(addr, val)`
- [ ] Mapper7's `chr` and `has_chr_ram` replaced by `chr_storage: DirectChr`
- [ ] Mapper7's `read_chr` delegates to `chr_storage.read_chr(addr)`
- [ ] Mapper7's `write_chr` gates on `has_chr_ram`, calls `chr_storage.write_chr(addr, val)`
- [ ] All existing Mapper2 unit tests pass
- [ ] All existing Mapper7 unit tests pass
- [ ] All Cartridge-level tests pass
- [ ] `cargo test` is green

## Blocked by

- #01: ChrStorage trait + DirectChr + Mapper0 (DirectChr must exist)
