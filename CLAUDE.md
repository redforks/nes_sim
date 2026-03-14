# NES Emulator

An NES (Nintendo Entertainment System) emulator implemented in Rust with a modular architecture.

## Workspace Structure

| Crate | Purpose | CLAUDE.md |
|-------|---------|-----------|
| `nes_core/` | Core emulation library (no std dependency except image/log) | [nes_core/CLAUDE.md](nes_core/CLAUDE.md) |
| `nes_cpu_test/` | CLI tool for running CPU test ROMs with exit code detection | [nes_cpu_test/CLAUDE.md](nes_cpu_test/CLAUDE.md) |
| `nes_web/` | WebAssembly build for browser-based emulation | [nes_web/CLAUDE.md](nes_web/CLAUDE.md) |
| `nescli/` | Additional CLI tool | [nescli/CLAUDE.md](nescli/CLAUDE.md) |
| `www/` | Frontend for WASM build | - |

## Quick Start

### Building
```bash
cargo build --release              # Build all crates in release mode
```

### Testing
```bash
make cpu-test                      # Run all CPU tests
```

### Web Development
```bash
make wasm-debug-build              # Build WASM
make web-start                     # Start dev server
```

## Architecture

See [nes_core/CLAUDE.md](nes_core/CLAUDE.md) for detailed architecture documentation including:
- CPU Layer (6502 emulation)
- MCU Abstraction (memory-mapped I/O)
- NES Hardware (RAM, PPU, cartridge, APU)
- Machine & Frame Execution
- Cartridge System (mappers)
- PPU (rendering)
- ROM Loading (iNES format)
