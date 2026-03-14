# NES Core Library

Core emulation library for the NES emulator. This crate has no std dependency except for image/log.

## Architecture

### CPU Layer (`src/cpu.rs`, `cpu/`)
- 6502 CPU emulation with cycle-accurate instruction timing
- `Cpu` struct holds registers (A, X, Y, PC, SP, status)
- `execute_next()` decodes opcodes via pattern matching on (c, a, b) tuples
- Instruction implementations in `cpu/instruction.rs`
- Addressing modes in `cpu/addressing.rs` - implements `Address` trait for all modes

### MCU Abstraction (`src/mcu.rs`)
- `Mcu` trait provides memory-mapped I/O interface (`read`/`write`)
- All addresses are absolute (16-bit) for easy device mapping
- `MachineMcu` trait adds rendering capability

### NES Hardware (`src/nes/`)
- `NesMcu` implements `Mcu` for full NES hardware:
  - `lower_ram`: 2KB internal RAM (mirrored across $0000-$1FFF)
  - `ppu`: Picture Processing Unit
  - `cartridge`: PRG/CHR ROM with mapper support
  - `after_ppu`: APU and I/O registers ($4000-$401F)
- Memory map routing in `read()`/`write()` methods

### Machine & Frame Execution (`src/machine.rs`)
- `Machine<P>` wraps CPU with a `Plugin` for extensibility
- `process_frame(ms)` executes cycles based on elapsed time
- V-Blank handling with NMI generation
- Constants: `CYCLES_PER_FRAME`, `CYCLES_PER_MS`, `V_BLANK_CYCLES`

### Cartridge System (`src/nes/mapper/`)
- `Cartridge` trait for different mappers
- `mapper0.rs`: NROM (no bank switching)
- `mmc1.rs`: MMC1 with shift register and PRG/CHR swapping

### PPU (`src/nes/ppu.rs`, `ppu/`)
- `Ppu` struct handles rendering and memory-mapped registers
- `PpuTrait` for MCU integration
- `pattern.rs`: CHR pattern table decoding
- `name_table.rs`: Name table and attribute table handling

### ROM Loading (`src/ines.rs`)
- `INesFile` parser for iNES .nes format
- `Header` struct with PRG/CHR page counts and mapper number

## Key Patterns

- **Plugin system**: `Plugin` trait hooks into instruction execution for test result detection
- **Address trait**: Uniform interface for all addressing modes with `get()`/`set()` returning tick counts
- **IRQ/NMI handling**: CPU has `irq_pending` flag and `nmi()` method; checks for interrupt on each instruction
- **Cycle accuracy**: Instructions return cycle counts; CPU uses `remain_clocks` counter
- **Memory mapping**: `Mcu` trait allows routing addresses to different devices based on range

## Interrupt Handling

The CPU implements special interrupt behavior for CLI/PLP instructions:
- `change_irq_flag_pending` stores pending I flag changes
- IRQ check happens at instruction start, applying pending flag changes first
- This implements the "CLI delay" behavior where interrupts are blocked for one instruction after CLI
