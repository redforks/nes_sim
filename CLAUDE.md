# NES Emulator

An NES (Nintendo Entertainment System) emulator implemented in Rust with a modular architecture.

## Workspace Structure

| Crate | Purpose | CLAUDE.md |
|-------|---------|-----------|
| `nes_core/` | Core emulation library (no std dependency except image/log) | [nes_core/CLAUDE.md](nes_core/CLAUDE.md) |
| `nes_cpu_test/` | CLI tool for running CPU test ROMs with exit code detection | [nes_cpu_test/CLAUDE.md](nes_cpu_test/CLAUDE.md) |
| `nes_web/` | WebAssembly build for browser-based emulation | [nes_web/CLAUDE.md](nes_web/CLAUDE.md) |
| `nescli/` | CLI tools with SDL2 display and CHR ROM inspection | [nescli/CLAUDE.md](nescli/CLAUDE.md) |
| `nes_mcp_protocol/` | MCP protocol definitions for NES emulator communication | - |
| `nes_mcp_server/` | MCP server exposing NES emulator as tools and resources | - |
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

### MCP Server

The `nes_mcp_server` crate provides an MCP (Model Context Protocol) server for inspecting and controlling the NES emulator. It exposes the emulator state as tools and resources.

```bash
# Start the MCP server (communicates with nes_cpu_test via TCP)
cargo run -p nes_mcp_server

# The server is configured in .mcp.json and automatically starts nes_cpu_test
# in TCP server mode (port 28800)
```

#### Available MCP Tools

- `start` - Start the NES emulator with a ROM file
- `tick` - Execute N CPU instruction cycles
- `forward_to_vblank` - Run until next VBlank (scanline 241, dot 1)
- `read_memory` - Read NES memory (hex start/end addresses, max 4KB range)
- `read_cpu_registers` - Read current CPU register state
- `read_ppu_status` - Read current PPU status
- `read_apu_status` - Read current APU status
- `restart_nes` - Restart the NES emulator

#### Available MCP Resources

- `nes://memory` - NES memory access
- `nes://cpu` - CPU state and registers
- `nes://ppu` - PPU (Picture Processing Unit) status
- `nes://apu` - APU (Audio Processing Unit) status

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

### MCP Integration

The `nes_mcp_protocol` and `nes_mcp_server` crates provide a Model Context Protocol interface for the emulator:
- **TCP Server Mode**: `nes_cpu_test` can run as a TCP server (port 28800) for remote control
- **MCP Protocol**: `nes_mcp_protocol` defines request/response types for NES operations
- **MCP Server**: `nes_mcp_server` implements the MCP spec, exposing NES state as tools and resources
- **Process Management**: Automatic lifecycle management with signal handling for graceful shutdown

## Build System

This project uses [`just`](https://github.com/casey/just) as a command runner. See `justfile` for all available commands.

```bash
just --list                         # List all available commands
```
