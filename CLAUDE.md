# NES Emulator

An NES (Nintendo Entertainment System) emulator implemented in Rust with a modular architecture.

## Workspace Structure

| Crate | Purpose | CLAUDE.md |
|-------|---------|-----------|
| `nes_core/` | Core emulation library (no std dependency except image/log) | [nes_core/CLAUDE.md](nes_core/CLAUDE.md) |
| `nes_cpu_test/` | CLI tool for running CPU test ROMs with exit code detection | [nes_cpu_test/CLAUDE.md](nes_cpu_test/CLAUDE.md) |
| `nes_web/` | WebAssembly build for browser-based emulation | [nes_web/CLAUDE.md](nes_web/CLAUDE.md) |
| `nescli/` | CLI tools with SDL2 display and CHR ROM inspection | [nescli/CLAUDE.md](nescli/CLAUDE.md) |
| `www/` | Frontend for WASM build | - |

## Quick Start

### Building
```bash
cargo build --release              # Build all crates in release mode
```

### Testing
```bash
just cpu-test                      # Run all CPU tests via just
just rom-test                      # Run comprehensive test suite
just unit-test                     # Run unit tests
```

### Web Development
```bash
just wasm-debug-build              # Build WASM
just web-start                     # Start dev server
```

### CLI Tools
```bash
# Run ROM with SDL2 display
cargo run -p nescli --release -- run <file.nes>

# Show ROM information
cargo run -p nescli --release -- info <file.nes>

# Extract and view CHR ROM patterns
cargo run -p nescli --release -- read-chr <file.nes> | feh -Z --force-aliasing -
```

## Architecture

See [nes_core/CLAUDE.md](nes_core/CLAUDE.md) for detailed architecture documentation including:
- CPU Layer (6502 microcode-based emulation)
- MCU Abstraction (memory-mapped I/O)
- NES Hardware (RAM, PPU, APU, cartridge, controller)
- Machine & Frame Execution
- Cartridge System (mappers: NROM, UxROM, CNROM, MMC1, MMC3, MMC5)
- PPU (scanline-accurate rendering)
- APU (audio with 5 channels: 2 pulse, triangle, noise, DMC)
- ROM Loading (iNES format)

## Build System

This project uses [`just`](https://github.com/casey/just) as a command runner. See `justfile` for all available commands.

```bash
just --list                         # List all available commands
```
