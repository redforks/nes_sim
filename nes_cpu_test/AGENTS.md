# NES CPU Test Runner

CLI tool for running CPU test ROMs with automatic test result detection and dead loop prevention. Read this file when touching the test harness, its plugins, or ROM blessing captures. For the `FrameOutput` drain contract and `Apu::SAMPLE_RATE`, see `nes_core/AGENTS.md` (canonical there). `just --list` and the `justfile` are canonical for the full recipe list.

## Running Tests

```bash
# Entry points (examples; full list lives in the justfile)
just cpu-test                      # 6502 functional test
just instr_misc                    # Misc instruction tests
just instr_test-v5                 # Instruction test v5
just nestest                       # Classic nestest (uses --start-pc 0xC000)
just unit-test                     # lib unit tests (all crates)
just passed                        # full gate: unit-test + passed_rom_tests
just passed_rom_tests              # all ROM groups without lib unit tests

# Direct harness invocation
cargo run -p nes_cpu_test --release -- -f path/to/test.nes
cargo run -p nes_cpu_test --release -- -f path/to/test.nes --max-instructions 1000000
cargo run -p nes_cpu_test --release -- -f path/to/test.nes --start-pc 0xC000
```

Harness flags (`-f` selects the ROM; `--help` lists all):

- `-f <path>`: `.nes` ROM or raw binary test image (required)
- `-q, --quiet`: reduce console output
- `--max-instructions <n>`: halt after n instructions (`0` = disabled)
- `--start-pc <addr>`: start execution at a custom address (hex `0x` prefix or decimal); e.g. nestest starts at `$C000`
- `--press <button>@<frame>`: hold controller-1 button from a PPU frame onward (repeatable); frames count PPU frames from reset, matching `--dump-frame`
- `--dump-frame <n> --dump-out <path>`: dump the rendered frame at frame n (first vblank at or after it) to a PNG and exit; the blessing tool for `PngFrameMatch` expected images
- `--dump-audio <path> [--frames <n>]`: run for n PPU frames (default 600, ~10 s NTSC), then write every mixed sample as 44.1 kHz mono 16-bit WAV and exit. Tick-stepped capture: steps per dot with `tick()`, never `process_frame()`, so the APU buffer accumulates the whole run — see the drain contract in `nes_core/AGENTS.md`
- `--force-mmc3-rev-a`: force MMC3 revision-A IRQ semantics regardless of detection; required by blargg's `mmc3_irq_tests/5.MMC3_rev_A.nes`

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
- Image-based expectation machines (`src/image.rs`): `NametableConsole` magic-word verdicts (blargg CPU/PPU/APU sets), `PngFrameMatch` blessed-snapshot comparison, `FramePngDump` blessing output, `WavRecorder` audio captures
- `ZapperTest` (`src/zapper_test.rs`): scripted light-gun sessions per ROM

## Test ROMs

External ROMs live in `../nes-test-roms/` (git-ignored); local imports live in `test-roms/`. The `justfile` is the source of truth for exact paths. Representative examples:

- `nes_cpu_test/src/6502_functional_test.bin` (via `just cpu-test`)
- `instr_misc/instr_misc.nes`, `instr_test-v5/all_instrs.nes`
- `other/nestest.nes` (via `just nestest`, needs `--start-pc 0xC000`)
- `cpu_interrupts_v2/`, blargg PPU/APU sets, MMC3/MMC5/VRC suites (see `just --list`)
