# NES Core Library

Core emulation library for the NES emulator. This crate has no std dependency except for image/log.

## Architecture

### CPU Layer (`src/cpu.rs`, `cpu/`)
- 6502 CPU emulation with **microcode-based implementation**
- `Cpu` struct holds registers (A, X, Y, PC, SP, status)
- **Microcode queue**: Instructions execute as a sequence of micro-operations
- `Microcode` enum defines all micro-operations (fetch, decode, ALU ops, memory access)
- `microcode.rs` contains opcode-to-microcode mapping (generated from `init_microtable.inc.rs`)
- All official and unofficial opcodes supported
- Cycle-accurate timing with proper interrupt handling
- `Plugin` trait allows hooking into instruction execution for test result detection

### MCU Abstraction (`src/mcu.rs`)
- `Mcu` trait provides memory-mapped I/O interface (`read`/`write`)
- All addresses are absolute (16-bit) for easy device mapping
- `RamMcu` provides simple RAM implementation for address ranges

### NES Hardware (`src/nes.rs`, `nes/`)
- `NesMcu<R, D>` implements `Mcu` for full NES hardware:
  - `lower_ram`: 2KB internal RAM (mirrored across $0000-$1FFF)
  - `ppu`: Picture Processing Unit with scanline-accurate timing
  - `cartridge`: PRG/CHR ROM with mapper support
  - `apu`: Audio Processing Unit with 5 channels
  - `controller`: Game controller input
- Memory map routing in `read()`/`write()` methods

### Machine & Frame Execution (`src/machine.rs`, `nes_machine.rs`)
- `Machine<P, M>` wraps CPU with a `Plugin` for extensibility
- `NesMachine<P, R, D>` combines CPU + PPU + APU coordination
- `process_frame()` executes until VBlank (scanline 241, dot 1)
- PPU:CPU ratio of 3:1 (one CPU instruction = 3 PPU ticks)
- NMI generation from PPU VBlank
- Safety limit of 60,000 ticks per frame to prevent infinite loops

### Cartridge System (`src/nes/mapper.rs`, `mapper/`)
- `Cartridge` enum for different mappers

### ROM Loading (`src/ines.rs`, `ines/`)
- `INesFile` parser for iNES .nes format
- `Header` struct with PRG/CHR page counts, mapper number, mirroring flags
- Supports iNES 2.0 header extensions (PRG RAM/NVRAM sizes, mirroring control)

### Rendering (`src/render.rs`, `render/`)
- `Render` trait for output backends
- `ImageRender`: Renders to image buffer (for SDL2, file output)
- Palette handling with grayscale and color emphasis effects

## Key Patterns

- **Microcode architecture**: Instructions decompose into micro-operations for accurate timing
- **Plugin system**: `Plugin` trait hooks into instruction execution for test result detection
- **MCU trait**: Uniform memory-mapped I/O interface for routing addresses to devices
- **AudioDriver trait**: Abstract audio output for different platforms
- **Render trait**: Abstract rendering for different backends
- **IRQ/NMI handling**: CPU has `set_irq()` and `update_nmi_line()` methods; checks on each instruction
- **Cycle accuracy**: Microcode queue tracks execution state; total_cycles counts elapsed cycles
- **Memory mapping**: `Mcu` trait allows routing addresses to different devices based on range

## Interrupt Handling

The CPU implements special interrupt behavior for CLI/PLP/SEI instructions:
- `irq_inhibit` stores pending I flag changes
- IRQ check happens at instruction start, applying pending flag changes first
- This implements the "CLI delay" behavior where interrupts are blocked for one instruction after CLI
