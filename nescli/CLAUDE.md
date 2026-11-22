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

### `convert` - Convert an FM2 Movie to a Mesen Movie

```bash
cargo run -p nescli --release -- convert movie.fm2
cargo run -p nescli --release -- convert movie.fm2 -o movie.mmo --rom game.nes
```

Converts an FCEUX `.fm2` movie into a Mesen 2 `.mmo` movie (a zip holding
`GameSettings.txt` and `Input.txt`). Output defaults to the input path with a
`.mmo` extension. `--rom` is optional: it supplies the `GameFile` and `SHA1`
metadata and warns when the ROM does not match the FM2 checksum. Four-score,
FDS, and VS movies are rejected because a Mesen movie cannot represent them.

### `run` - Run with SDL2 Display

```bash
cargo run -p nescli --release -- run <file.nes>
```

Opens an SDL2 window with:
- Scaled display of the logical 1x 256x240 frame (interactive `run` window is 2x, 512x480, via one SDL nearest-neighbour scale-copy; movie playback uses a 4x 1024x960 window)
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

The SDL2 runner is a per-frame sink: each frame it runs `process_frame()`, presents `output.frame`, and pushes `output.audio` through chunked queueing (`AUDIO_CHUNK_SAMPLES = 1024`, drops on queue pressure) plus `flush_frame()`. Recording additionally forwards the frame's samples to the encoder channel:

```rust
let output = machine.process_frame();
render.present_frame_bytes(output.frame);
audio.push_samples(output.audio);
audio.flush_frame();
```

- Sample rate is `Apu::SAMPLE_RATE` (44.1 kHz mono `f32`, host resamples if needed)
- Mixing happens in the core APU; the SDL side only transports frames of samples
- Audio buffering to prevent underruns

## Dependencies

- `sdl2`: Cross-platform graphics/audio/input
- `image`: PNG encoding for CHR output
- `clap`: Command-line argument parsing
- `nes_core`: Core emulation library
