# NES CLI

CLI tools for the NES emulator with SDL2 display and CHR ROM inspection.

## Building

```bash
cargo build -p nescli --release    # Build in release mode
cargo build -p nescli              # Build in debug mode
```

## Commands

### `info` - Display ROM Information

```bash
cargo run -p nescli --release -- info <file.nes>
```

Shows:
- iNES header information
- Mapper number
- PRG/CHR ROM sizes
- Mirroring mode
- TV system (NTSC/PAL)

### `read-chr` - Extract and View CHR ROM

```bash
# Output CHR patterns as PNG to stdout
cargo run -p nescli --release -- read-chr <file.nes> > output.png

# View directly with feh
cargo run -p nescli --release -- read-chr <file.nes> | feh -Z --force-aliasing -
```

Extracts the CHR ROM (pattern tables) and renders as a PNG image showing all 512 tiles (2 pattern tables × 256 tiles).

### `run` - Run with SDL2 Display

```bash
cargo run -p nescli --release -- run <file.nes>
```

Opens an SDL2 window with:
- 2x scaled display (512×480)
- 60 FPS frame rate limiting
- Keyboard input for controller
- Audio output

## Controls (SDL2 Runner)

| Key | Button |
|-----|--------|
| Arrow Keys | D-Pad |
| Z | A |
| X | B |
| Space | Select |
| Enter | Start |
| F2 | Reset |
| F4 | Quit |
| Escape | Quit |

## Audio

The SDL2 runner implements the `AudioDriver` trait:
- 44.1 kHz sample rate
- 16-bit audio output
- Proper APU channel mixing
- Audio buffering to prevent underruns

## Dependencies

- `sdl2`: Cross-platform graphics/audio/input
- `image`: PNG encoding for CHR output
- `clap`: Command-line argument parsing
- `nes_core`: Core emulation library
