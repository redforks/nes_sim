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
just unit-test                     # Run unit tests
just passed                        # Run unit tests and passed rom tests
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

## Agent skills

### Issue tracker

Issues and PRDs live as local markdown files under `.scratch/<feature-slug>/`. See `docs/agents/issue-tracker.md`.

### Domain docs

Single-context — one `CONTEXT.md` and `docs/adr/` at the repo root. See `docs/agents/domain.md`.

## graphify

This project has a knowledge graph at graphify-out/ with god nodes, community structure, and cross-file relationships.

Rules:
- For codebase questions, first run `graphify query "<question>"` when graphify-out/graph.json exists. Use `graphify path "<A>" "<B>"` for relationships and `graphify explain "<concept>"` for focused concepts. These return a scoped subgraph, usually much smaller than GRAPH_REPORT.md or raw grep output.
- If graphify-out/wiki/index.md exists, use it for broad navigation instead of raw source browsing.
- Read graphify-out/GRAPH_REPORT.md only for broad architecture review or when query/path/explain do not surface enough context.
- After modifying code, run `graphify update .` to keep the graph current (AST-only, no API cost).
