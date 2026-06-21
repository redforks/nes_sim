# ADR-0001: ChrStorage trait with DirectChr and WindowedChr implementations

We introduced a `ChrStorage` trait with two concrete implementations — `DirectChr` (fixed 8KB array, direct addressing) and `WindowedChr` (source `Vec<u8>` + cached 8KB read window) — to eliminate duplicated CHR read/write logic across mappers. The trait exposes only `read_chr` and `write_chr`; window-refresh and write-through are inherent methods on `WindowedChr` only. `has_chr_ram` gating stays in each mapper, not in the storage layer.

Each mapper composes with a concrete `ChrStorage` type (never `dyn` or generic). The `Cartridge` enum dispatch is unchanged — mappers still expose `read_chr`/`write_chr` as before.

Special mappers (Mapper34, Mapper87) keep bespoke storage; only the two dominant patterns (direct and windowed) were extracted.

## Considered Options

- **Cartridge-level trait dispatch** (rejected): Making `ChrStorage` the dispatch layer in `Cartridge` itself would require `Box<dyn ChrStorage>` and lose mapper-specific bank-switching logic.
- **Trait includes refresh** (rejected): Refresh is mapper-specific (bank sizes, slot counts). A generic `refresh()` on the trait would force one-size-fits-all parameters on both `DirectChr` (no-op) and `WindowedChr`.
- **Separate write-to-cache and write-to-source** (rejected in favor of combined write-through): Combined `write_chr_with_source` keeps cache and source in sync atomically.

## Consequences

- New mappers can reuse `DirectChr` or `WindowedChr` instead of reimplementing CHR storage.
- Windowed mappers retain full control of bank-mapping logic; `WindowedChr` is purely a storage container with a read-through cache.
- Special mappers remain unchanged, avoiding premature abstraction.
