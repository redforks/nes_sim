# Decode into NesMcu, delete Ppu-as-Mcu

`Ppu` owned `Box<dyn Cartridge>` and implemented `Mcu` (`read`/`write` for `$4100-$FFFF` plus `$2000-$3FFF` registers). `NesMcu::read` delegated `0x2000-0x3FFF | 0x4100-0xFFFF` to `Ppu::read`, so CPU address decode was split between `NesMcu` and `Ppu`. Undriven CPU reads (`$4100-$5FFF`, disabled PRG-RAM) incorrectly drove `0` from `Cartridge::read` instead of refreshing open bus; the PPU register path and cartridge path were interleaved in `Ppu`.

This ADR moves cartridge ownership and all CPU address decode into `NesMcu`.

## Considered options

- **Keep `Ppu: Mcu`**: preserves split decode, keeps undriven-read and PRG-RAM-disable open-bus bugs, and forces `&mut dyn Cartridge` to be threaded through `Ppu::tick` anyway for A12/MMC3.
- **Thread `&mut dyn Cartridge` through `Ppu` but keep `Mcu`**: still leaves decode in two places and keeps the `Ppu-as-Mcu` indirection with zero consumers (verified via `lsp references`).
- **This ADR (decode into NesMcu)**: `NesMcu` owns `cartridge: Box<dyn Cartridge> + cartridge_caps: CartridgeCaps + current_clock: SystemClock`; `Ppu` owns no cartridge and takes `&mut dyn Cartridge, CartridgeCaps` only where it needs it (`tick` for A12/`on_ppu_tick`, `read_vram`/`write_vram`/`read_vram_and_inc`/`fill_tile_cache`/`render_pixel` for CHR, `read_ppureg`/`write_ppureg` for `$2006/$2007` notify). `Ppu::new` no longer takes a cartridge.

## Decision

- `Ppu` fields `cartridge` and `cartridge_caps` removed; `Ppu::new(renderer, mirroring)` only; `Ppu::tick(&mut dyn Cartridge, CartridgeCaps)`, `Ppu::read_vram(&self, u16, &dyn Cartridge)`, `Ppu::write_vram(&mut self, u16, u8, &mut dyn Cartridge)`, `Ppu::read_ppureg`/`write_ppureg`/`peek` take `&mut dyn Cartridge, CartridgeCaps` where needed; `Ppu::set_mirroring` exposed for `NesMcu` to apply `CartridgeOperation::UpdateNametableMirroring`; `impl Mcu for Ppu` deleted.
- `NesMcu` decode: `0x0000-0x1FFF` LowerRam, `0x2000-0x3FFF` `Ppu::read_ppureg`/`write_ppureg`, `0x4000-0x401F` APU/IO (`$4014` OAM DMA, `$4016` controller, length-halt deferred writes), `0x4020-0x5FFF` open bus, `0x6000-0x7FFF` PRG-RAM if `cartridge.prg_ram_enabled()` else open bus, `0x8000-0xFFFF` PRG-ROM via `cartridge.read`/`write(cycle)`. `Cartridge::write` now takes `SystemClock` (only `Cartridge::write`, not `Mcu::write`); `NesMcu` stamps `current_clock` from `NesMachine::tick` via `set_clock` and passes it. `NesMcu::peek` mirrors the same decode. Undriven reads return/refresh `open_bus` instead of `0`.
- `Cartridge::prg_ram_enabled() -> bool` (default `true`) added for MMC1 (`prg_bank & 0x10 ==0`), MMC3 (`prg_ram_enabled`), VRC24 (`!is_vrc4() || prg_ram_enabled`) to model disabled WRAM as open bus.
- `NesMachine::tick` calls `mcu.set_clock(clock)` before `bus.tick`/`cpu.tick` so `Mcu::write` sees the correct `SystemClock`.

## Consequences

- No `Ppu`-as-`Mcu` indirection; all CPU decode lives in `NesMcu` and is testable via `NesMcu`'s `Mcu` impl.
- Undriven CPU reads correctly refresh open bus; disabled PRG-RAM no longer drives `0`.
- `Ppu` tests construct `Ppu::new((), mirroring)` and thread a `TestCartridge` through `tick`/`read_vram`/`render_pixel`; `NesMcu` tests construct `NesMcu` with `cartridge`/`cartridge_caps`/`current_clock`.
- Vocabulary lives in CONTEXT.md under "Bus Decoding": decode into NesMcu (not Ppu).
