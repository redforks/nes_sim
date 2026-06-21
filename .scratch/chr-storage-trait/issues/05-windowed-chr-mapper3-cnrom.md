Status: ready-for-agent

# Slice 5: Convert Mapper3 (CNROM) to WindowedChr

## What to build

Convert Mapper3 (CNROM) to use `WindowedChr` instead of its inline `chr_rom: Vec<u8>`, `chr_window: [u8; CHR_WINDOW_SIZE]`, `selected_chr_bank: usize`, and `chr_bank_count: usize` fields.

Mapper3 is the simplest windowed mapper — it selects a single 8KB bank from the CHR ROM and caches the entire window. No CHR RAM, no writes (write_chr is a no-op — the Cartridge dispatch does `Cartridge::Mapper3(_) => {}`).

Mapper3's `refresh_chr_window` copies the selected 8KB bank into the cache:
```
let bank_start = selected_chr_bank * 0x2000;
chr_window.copy_from_slice(&chr_rom[bank_start..bank_start + 0x2000]);
```

This maps directly to a single `chr_storage.refresh(selected_chr_bank * 0x2000)` call.

Because Mapper3 has no CHR writes:
- `read_chr` delegates to `chr_storage.read_chr(addr)`
- `write_chr` is never called (Cartridge dispatch arm stays `=> {}`)
- `has_chr_ram` is always false — no writability concern

The mapper's `write()` method (CPU-side, not CHR-side) writes to `0x8000..=0xffff` to set `selected_chr_bank` and calls `refresh`. This CPU-side write remains unchanged — it just calls `chr_storage.refresh(...)` instead of the inline copy.

## Acceptance criteria

- [ ] Mapper3's `chr_rom`, `chr_window`, `selected_chr_bank`, `chr_bank_count` replaced by `chr_storage: WindowedChr`
- [ ] `selected_chr_bank` may remain as a bare usize field (CPU-side register state) — only the storage layer is extracted
- [ ] Mapper3's `read_chr` delegates to `chr_storage.read_chr(addr)`
- [ ] Mapper3's `write_chr` stays absent (Cartridge dispatch arm remains `=> {}`)
- [ ] `refresh_chr_window` calls `chr_storage.refresh(selected_chr_bank * 0x2000)`
- [ ] All existing Mapper3 unit tests pass
- [ ] All Cartridge-level tests pass
- [ ] `cargo test` is green

## Blocked by

- #02: WindowedChr + MMC3 (WindowedChr must exist)
