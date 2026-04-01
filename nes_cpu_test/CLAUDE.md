# NES CPU Test Runner

CLI tool for running CPU test ROMs with automatic test result detection and dead loop prevention.

## Running Tests

```bash
# Run all CPU tests via just (uses test ROMs from nes-test-roms/)
just cpu-test                      # 6502 functional test
just rom-test                      # Full test suite

# Run individual test categories
just instr_misc                    # Misc instruction tests
just instr_test-v5                 # Instruction test v5
just nestest                       # Classic nestest

# Run with custom options
cargo run -p nes_cpu_test --release -- -f path/to/test.nes
cargo run -p nes_cpu_test --release -- -f path/to/test.nes --max-instructions 1000000
cargo run -p nes_cpu_test --release -- -f path/to/test.nes --start-pc 0xC000

# Run unit tests
cargo test -p nes_cpu_test
```

## Features

### Exit Code Detection
Test ROMs write a status code to memory to indicate pass/fail. The test runner:
- Monitors memory writes to detect test completion
- Exits with the detected status code
- Supports both `.nes` format and raw binary test images

### Dead Loop Detection
The `DetectDeadLoop` plugin prevents infinite loops by:
- Detecting repeated PC values (indicating a jump loop)
- Detecting repeated instruction sequences
- Configurable threshold before halting

### Console Output
The `Console` plugin provides:
- Disassembly of executed instructions
- CPU state display (registers, flags)
- Memory read/write logging
- Configurable output levels

### Test Status Monitoring
The `MonitorTestStatus` plugin:
- Tracks writes to test status memory locations
- Detects test completion conditions
- Extracts test result codes

### Instruction Limits
The `MaxInstructions` plugin:
- Halts execution after a specified number of instructions
- Prevents runaway test execution
- Configurable via `--max-instructions` flag

### Custom Start Address
The `--start-pc` flag allows:
- Starting CPU execution at a custom address
- Useful for tests like nestest that start at $C000
- Supports hex (0x prefix) or decimal values

## Building

```bash
cargo build -p nes_cpu_test --release    # Build the test runner
cargo build -p nes_cpu_test              # Build in debug mode
```

## Plugin Architecture

The test runner uses a modular plugin system (`src/plugin.rs`):
- `MachineWrapper` combines multiple plugins
- Each plugin hooks into instruction execution (`start()`, `end()`, `should_stop()`)
- Plugins communicate via shared state in `MachineWrapper`

### Available Plugins

- `Console`: Disassembly and logging
- `DetectDeadLoop`: Infinite loop detection
- `ImgExit`: Image-based test exit detection (for visual tests)
- `MonitorTestStatus`: Memory-based test result detection
- `MaxInstructions`: Instruction count limiting
- `Report`: Test result reporting

## Test ROMs

Test ROMs should be placed in `../nes-test-roms/`:
- `6502_functional_test.bin` - Comprehensive CPU test
- `instr_misc/instr_misc.nes` - Misc instruction tests
- `instr_test-v5/all_instrs.nes` - Instruction test v5
- `other/nestest.nes` - Classic nestest
- `cpu_interrupts_v2/rom_singles/*.nes` - Interrupt behavior tests
