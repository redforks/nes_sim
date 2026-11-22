# Save state: DTO and serde in nes_core, file paths and I/O in nescli

## Decision

The snapshot DTO structs, their serde derives, the JSON string conversion,
validation, and the factory-based restore live in `nes_core`. The string
functions take and return `String`, so no file access happens in core.

`nescli` owns the path rules (`<rom-dir>/<rom-stem>.state` for F5/F7, the
explicit `--load-state FILE`) and all `std::fs` reads and writes.

`nes_core` gains one new dependency, `serde` with the derive feature.
`serde_json` is already a `nes_core` dependency (`nes_core/src/bk2.rs:32,174`).
`nescli` gains no new dependency for this feature.

The ROM identity function is `pub fn rom_sha256(rom: &INesFile) -> [u8; 32]`
in `nes_core`. It hashes `read_prg_rom()` then `read_chr_rom()`. The private
`rom_hashes` (`nes_core/src/bk2.rs:651`) is left unchanged.

## Considered options

- **DTO and file I/O both in nescli.** Rejected. nescli would need `serde`,
  `serde_json`, and `sha2`. `nes_core` would have to expose its internal state
  through `pub` accessors so that nescli could build the DTO. That widens the
  core API and leaks fields ADR 0020 keeps private.
- **File I/O in nes_core.** Rejected. `nes_web` compiles `nes_core` for WASM
  in the browser, where there is no `std::fs`. Save/load is out of scope for
  `nes_web`, but the core crate must stay usable there.
- **Reuse `rom_hashes` as `pub`.** Rejected. Each save and load would also
  compute CRC32, MD5, and SHA-1, and the public API would expose a BK2-shaped
  four-tuple.

## Consequences

- ADR 0020 says "`nes_core`, which has no serde dependency today". That is
  only partly true. `serde_json` is already present, and the new piece is the
  `serde` derive crate.
- The string functions can be unit-tested in `nes_core` without disk.
- A later move of the I/O seam into core, or of the DTO into nescli, is a
  refactor of the DTO module and of the call sites in `nescli/src/actions/run.rs`
  and `nescli/src/sdl/event_loop.rs`.
