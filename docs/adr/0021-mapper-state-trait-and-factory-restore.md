# Mapper state: trait methods over a tagged enum, restore by rebuilding through the factory

## Decision

Each mapper saves and restores its own state through two required methods on
`Mapper`: `snapshot(&self) -> MapperState` and `restore(&mut self, state:
&MapperState) -> Result<(), SnapshotError>`. `MapperState` is a plain core
enum with one variant per concrete mapper type; each variant wraps a struct
defined in that mapper's file. The variant name is the mapper tag.

On load, the cartridge is rebuilt through the same factory as a cold boot,
from the ROM being loaded, and the saved state is applied on top. Config
(values set from the ROM header or CLI and never changed by emulation) is not
saved. A load whose saved tag differs from the rebuilt mapper's tag is refused.

## Considered options

- **Central enum with all mapper state structs in `mapper.rs`, no trait
  methods.** Rejected. Every mapper's field layout would live in one file,
  and `Cartridge` would dispatch on the concrete type. Trait methods keep each
  struct next to the code that mutates it.
- **Save config and restore it verbatim.** Rejected. The header and CLI
  override already determine config, and the DTO excludes `irq_override`.
  A save that carried config could silently override the header of the ROM
  it was loaded against.
- **Default trait bodies for `snapshot` and `restore`.** Rejected. A new mapper
  would ship with empty state and no compile error.

## Consequences

- Adding a mapper requires a `MapperState` variant. Missing it is a compile
  error, the same guarantee the mirror-enum rule gives for the DTO.
- The tag check catches a header change (mapper or submapper number) that
  keeps the same PRG and CHR, because `rom_sha256` covers only PRG and CHR.
- Restore depends on the factory staying deterministic for a given ROM.
  Changes to how config is derived change what a load rebuilds, so they must
  be treated as a save-format concern.
- The `Cell` fields in MMC5 are captured through `get`/`set`, so `snapshot`
  takes `&self` and interior mutability stays inside the mapper.
