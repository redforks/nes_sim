# Save state is a versioned snapshot DTO; core types get no serde derives

## Decision

Emulator state is saved through a separate, versioned snapshot DTO. The DTO
carries the serde derives. Core types (`Cpu`, `Ppu`, `Apu`, `Bus`, and the
rest of `nes_core`) get none. Core enums are mirrored in the DTO as enums with
string variant names, converted by exhaustive `From`/`TryFrom` matches. A
top-level `format_version` is bumped on any layout change or change to a
saved field's meaning. A load with a different version is refused, not
migrated. The file is serde_json text, with byte arrays as hex strings and
`f32` fields stored as their `u32` bit pattern.

## Considered options

- **Derive serde on core types.** Rejected. It couples every internal field
  and enum to the file layout, so an internal refactor silently changes the
  save format. It also puts serde into `nes_core`, which has no serde
  dependency today.
- **Integer discriminants for enums.** Rejected. Reordering or inserting a
  variant changes the meaning of old saves with no compile-time signal.
- **Recompute caches and derived state on load instead of storing them.**
  Rejected. It adds a second code path that must match the first, and bit-exact
  resume is easier to guarantee when the stored value is restored verbatim.
- **Migrate old format versions.** Rejected. The map scopes save compatibility
  to the build that wrote the file; refusing is simpler and never loads a
  wrong state.

## Consequences

- An exhaustive match on each mirror enum makes a new core variant a compile
  error until it is mirrored. The DTO cannot silently drift from core state.
- A layout change costs a version bump and a test update. Emulation-behavior
  changes do not bump the version, so a save is valid only for the build that
  wrote it.
- Mirror enums and the DTO structs duplicate the core field list. The
  duplication is the price of keeping `nes_core` free of serde.

## Clarification (save-state build)

"Free of serde" above means core *types* carry no serde derives — only
the DTO structs in `nes_core/src/snapshot.rs` derive
`Serialize`/`Deserialize`. `nes_core` does depend on `serde` (with derive)
and `serde_json`; see ADR 0023 for the DTO-in-core / file-I/O-in-nescli
split.
