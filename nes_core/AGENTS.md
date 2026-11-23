# NES Core Library

Core emulation library for the NES emulator. This crate has no std dependency except for image/log. Read this file when touching CPU/APU/PPU, mapper, timing, audio, or the per-frame contract. Domain language lives in `GLOSSARY.md`; this file is canonical for behavior.

## Architecture

### CPU Layer (`src/cpu.rs`, `cpu/`)

- 6502 CPU emulation with **microcode-based implementation**
- `Cpu` struct holds registers (A, X, Y, PC, SP, status)
- **Microcode queue**: Instructions execute as a sequence of micro-operations
- `Microcode` enum defines all micro-operations (fetch, decode, ALU ops, memory access)
- `microcode.rs` contains opcode-to-microcode mapping (`build_opcode_table` via `[ArrayVec::from_array_empty([Microcode::Kill; 7]); 256]`)
- All official and unofficial opcodes supported
- Cycle-accurate timing with proper interrupt handling
- `Plugin` trait allows hooking into instruction execution for test result detection
- CLI/PLP/SEI corner: `irq_inhibit` stores the pending I-flag change, the IRQ check at instruction start applies it first — the "CLI delay" where interrupts stay blocked for one instruction after CLI.

### MCU Abstraction (`src/mcu.rs`)

- `Mcu` trait provides memory-mapped I/O interface (`read`/`write`)
- All addresses are absolute (16-bit) for easy device mapping
- `RamMcu` provides simple RAM implementation for address ranges

### NES Hardware (`src/nes.rs`, `nes/`)

- `NesMcu` implements `Mcu` for full NES hardware (concrete type, no generics):
  - `lower_ram`: 2KB internal RAM (mirrored across $0000-$1FFF)
  - `ppu`: Picture Unit with scanline-accurate timing; owns the RGBA framebuffer (see Rendering below)
  - `cartridge`: PRG/CHR ROM with mapper support
  - `apu`: Audio Unit with 5 channels; owns the mono mix buffer (see Machine & Frame Execution below)
  - `controller`: Game controller input
- Memory map routing in `read()`/`write()` methods
- The old per-sample `AudioDriver` `push_sample`/`flush` callbacks are retired: sinks borrow each frame's owned buffers instead (see the drain contract under Machine & Frame Execution)

### Machine & Frame Execution (`src/machine.rs`, `nes_machine.rs`)

- `Machine<P, M>` is the generic CPU + address-space harness (used with test MCUs); `NesMachine<P>` is the concrete NES machine with a single plugin generic over `NesMcu`
- `process_frame()` ticks until the PPU scanline returns to 261 — the save point (ADR 0022): the end of the first CPU cycle at or after scanline 261 (PPU dot 0, 1, or 2) — via the existing `Timing` scanline accessor in two phases (leave 261, then return to it), with no new PPU flag, and returns `FrameOutput { status, frame, audio }`. The mid-frame vblank-entry event is cleared at the boundary instead of breaking the loop
- `FrameOutput` is the per-frame read: `frame` is the PPU RGBA bytes (`256*240*4`, row-major), `audio` is the mixed mono `f32` samples for exactly that frame, and `audio_bytes()` is the zero-copy little-endian `f32` byte view of `audio` for encoders. Consumers that step with `process_frame()` take pixels and samples from here and never re-borrow the machine:

```rust
let output = machine.process_frame();
render.present_frame_bytes(output.frame);
audio.push_samples(output.audio);
audio.flush_frame();
// Encoder path: output.audio_bytes() — same samples, no re-borrow.
```

- Drain contract (per-frame drain is canonical): the APU buffer is cleared at the start of each `process_frame()`, so every call yields exactly one frame's samples and sinks drain the returned `FrameOutput` the same frame (SDL event loop and `nes_web::Machine` both `push_samples(output.audio)` + `flush_frame()` per frame). Tick-stepped capture (`nes_cpu_test --dump-audio`) is the deliberate exception: it steps per dot with `tick()`, never calls `process_frame()`, so the buffer accumulates the whole run for one end-of-run export via `NesMachine::audio_samples()`. Stepping that loop via `process_frame()` without a per-frame drain truncates the dump to the last frame.
- Reset semantics: soft reset (front-panel RESET) preserves the screen — the PPU keeps running mid-frame and the owned framebuffer is untouched. Hard reset (power cycle) returns the framebuffer to its power-up contents, opaque black `[0, 0, 0, 255]`, while the host color theme is preserved.
- PPU:CPU ratio of 3:1 (one CPU instruction = 3 PPU ticks)
- NMI generation from PPU VBlank
- No tick-count guard on `process_frame()`: PPU timing always advances in `tick`, so the scanline-261 boundary is reached within one frame (~89k dots)
- Fixed audio rate at `Apu::SAMPLE_RATE` (`44_100`, no setter); SDL and WAV-header rates derive from it. The PPU owns its framebuffer (`Box<[[u8; 4]; 256 * 240]>`, row-major), written once per frame at the VBL set dot and served zero-copy via `frame_bytes()` / `frame_pixels()`. The APU owns its mono mix buffer (`Vec<f32>` at `Apu::SAMPLE_RATE`), filled by `tick`, cleared at each `process_frame` start, served via `audio_samples()` / `audio_bytes()`.

### Cartridge System (`src/nes/mapper.rs`, `mapper/`)

- `Cartridge` owns the bank-mapped cartridge memory plus the `Mapper` chip (one implementation per board); the chip's declared capabilities gate which PPU hooks reach it

### ROM Loading (`src/ines.rs`, `ines/`)

- `INesFile` parser for iNES .nes format
- `Header` struct with PRG/CHR page counts, mapper number, mirroring flags
- Supports iNES 2.0 header extensions (PRG RAM/NVRAM sizes, mirroring control)

### Rendering (owned buffers, no render traits)

- There is no `Render`/`ImageRender` trait layer: the PPU owns its framebuffer and hosts read it out per frame via `FrameOutput.frame`, `Ppu::frame_bytes()` / `frame_pixels()`, or the `MachineView` `borrow_image_bytes()` / `borrow_pixels()` / `borrow_image()` (compat alias) accessors
- `MachineView` exposes its `NesMcu` extensions (`ppu_in_vblank()`, `ppu_rendering_enabled()`, `ppu_frame_no()`, `read_vram()`, image borrows, dimensions) from a single `impl` block in `src/view.rs`
- Palette handling with grayscale and color emphasis effects
- Allocator note: `palette_index_buffer` + `framebuffer` are `Box`ed once at `Ppu::new()` and reused — no per-frame ~60KB allocation. The APU mix buffer is a `Vec<f32>` that is `clear()`ed per frame (capacity kept); the SDL recording scratch frame is likewise allocated once and reused
