# NES Emulator

An NES (Nintendo Entertainment System) emulator implemented in Rust with a modular architecture.

## Workspace Structure

| Crate | Purpose | Doc |
|---|---|---|
| `nes_core/` | Core emulation library (no std dependency except image/log) | [nes_core/AGENTS.md](nes_core/AGENTS.md) |
| `nes_cpu_test/` | CLI tool for running CPU test ROMs with exit code detection | [nes_cpu_test/AGENTS.md](nes_cpu_test/AGENTS.md) |
| `nes_web/` | WebAssembly build for browser-based emulation | - (no dedicated doc; see `nes_web/src/` and `www/`) |
| `nescli/` | CLI tools with SDL2 display (SDL driver, render, and event loop live under `nescli/src/sdl/`) | [nescli/AGENTS.md](nescli/AGENTS.md) |
| `www/` | Frontend for WASM build | - |

Read the crate doc when touching that crate:

- `nes_core/AGENTS.md` for CPU/PPU/APU, mapper, timing, audio, and the per-frame `FrameOutput` / drain contract.
- `nescli/AGENTS.md` for `run` / `info` / `convert`, SDL controls, and presentation.
- `nes_cpu_test/AGENTS.md` for the test ROM harness, plugins, and blessing captures (`--dump-frame`, `--dump-audio`, `--press`).

## Quick Start

### Building

```bash
cargo build --release              # Build all crates in release mode
```

### Testing

```bash
just unit-test                     # lib unit tests
just passed                        # unit tests + full ROM gate (unit-test + passed_rom_tests)
```

`just --list` and the `justfile` are canonical for all recipes.

### Web Development

```bash
just wasm-debug-build              # Build WASM
just web-start                     # Start dev server
```

### CLI Tools

See [nescli/AGENTS.md](nescli/AGENTS.md) for `run` (interactive + `--movie` playback), `info`, and `convert`.

## Architecture

See [nes_core/AGENTS.md](nes_core/AGENTS.md) for the emulation architecture (CPU, MCU, NES hardware, machine/frame execution, cartridge, PPU, APU, iNES). Domain language lives in `GLOSSARY.md`; decisions live in `docs/adr/`.

## Build System

This project uses [`just`](https://github.com/casey/just) as a command runner. See `justfile` for all available commands.

```bash
just --list                         # List all available commands
```

## Agent skills

### Issue tracker

Issues live in GitHub Issues (`redforks/nes_sim`) via the `gh` CLI. See `docs/agents/issue-tracker.md`.

### Triage labels

Default canonical roles (`needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, `wontfix`). See `docs/agents/triage-labels.md`.

### Domain docs

Single-context — one `GLOSSARY.md` and `docs/adr/` at the repo root. See `docs/agents/domain.md`.
