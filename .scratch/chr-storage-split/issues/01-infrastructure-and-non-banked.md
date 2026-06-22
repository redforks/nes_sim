Status: ready-for-agent

# S1: Infrastructure + non-banked mappers (0, 2, 7)

## Parent

PRD: `.scratch/chr-storage-split/PRD.md`

## What to build

This is the core slice. It changes every integration layer to split ChrStorage from Cartridge and makes it work for non-banked mappers (0, 2, 7).

**Changes include:**

1. **`Cartridge` trait**: Remove `read_chr` and `write_chr` methods.
2. **`ChrStorage`**: `DirectChr` gains a constructor from `chr_rom` slice (or `from_chr_rom(&[u8])`). The existing `DirectChr::empty()` stays.
3. **`Ppu<R>`**: Gains `chr_storage: Box<dyn ChrStorage>` field, set via constructor. All internal CHR access (`read_vram`, `write_vram`, `read_pattern_pixel`, `get_background_pixel`, `render_pixel`, `read_vram_and_inc`) switches from `cartridge.read_chr()`/`cartridge.write_chr()` to `self.chr_storage.read_chr()`/`self.chr_storage.write_chr()`. The `cartridge: &mut dyn Cartridge` parameter stays in `tick()`/`read()`/`write()` for `notify_vram_address` only.
4. **`Ppu::peek`**: Removes the `cartridge` parameter entirely (was only used for CHR access).
5. **`create_cartridge()`**: Return type changes from `(Box<dyn Cartridge>, Mirroring)` to `(Box<dyn Cartridge>, Box<dyn ChrStorage>, Mirroring)`. For mappers 0, 2, 7, creates a `DirectChr` populated with the full `chr_rom`. For other mappers (which will be migrated in later slices), creates a stub — see the Out of Scope note.
6. **`NesMcu`**: In `write()`, for cartridge-range addresses (`>= 0x4020`), additionally calls `self.ppu.write_chr_register(addr, value)` which forwards to `ChrStorage::write_register`. A new method `Ppu::write_chr_register` is added.
7. **Mapper 0, 2, 7**: Constructors no longer take `chr_rom`. Remove internal `chr_storage` field, `has_chr_ram` field, and `read_chr`/`write_chr` impls. These methods become dead code and are deleted.
8. **Tests**: New tests for `DirectChr` construction from slice. Existing mapper tests for mappers 0, 2, 7: remove CHR-related tests (`writes_to_chr_ram_when_chr_rom_is_absent`, pattern tests). Keep PRG/ram/mirroring tests.

**Out of scope for this slice**: Banked mappers (1, 3, 4, 21-25, 34, 87). For these, the factory returns a `DirectChr` populated with the raw `chr_rom` data (no banking applied). This means games using banked mappers will display incorrect graphics until their companion `ChrStorage` is added in later slices. Their existing inherent `read_chr`/`write_chr` methods become dead code but are not yet removed — they will be removed when each mapper is migrated.

## Acceptance criteria

- [ ] `Cartridge` trait no longer has `read_chr` or `write_chr`
- [ ] `Ppu` has `chr_storage` field; all internal CHR access goes through it
- [ ] `create_cartridge()` returns `(Box<dyn Cartridge>, Box<dyn ChrStorage>, Mirroring)`
- [ ] `NesMcu::write` calls `ppu.write_chr_register()` for cartridge-range addresses
- [ ] `Ppu::peek` takes no cartridge parameter
- [ ] Mapper 0, 2, 7 constructors take only `prg_rom`; CHR fields removed
- [ ] `DirectChr` has a constructor from `&[u8]`
- [ ] All tests pass: `cargo test`
- [ ] Non-banked mapper games render correctly (same as before)

## Blocked by

- #00: Add `write_register` to `ChrStorage` trait
