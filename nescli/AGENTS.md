# NES CLI

CLI tools for the NES emulator with SDL2 display. Read this file when touching `run` / `info` / `convert`, SDL controls, or presentation.

For the per-frame contract (`process_frame()` save-point boundary, `FrameOutput`, drain cadence, `Apu::SAMPLE_RATE`), see `nes_core/AGENTS.md` — canonical there, not repeated here. Run `cargo run -p nescli -- <cmd> --help` for the full flag list.

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
cargo run -p nescli --release -- run <file.nes> --movie <movie.fm2|movie.bk2> [--headless] [--no-throttle] [--output out.mp4]
```

- Interactive mode (no `--movie`): opens an SDL2 window with the logical 1x 256x240 frame scaled to a 2x 512x480 window via one SDL nearest-neighbour scale-copy, 60 FPS frame limiting, keyboard input, and audio output. `--load-state <path>` loads a save file before the window opens (same code as F7); a missing/corrupt file fails pre-window, non-zero. F5/F7 save/load target stays `<rom-dir>/<rom-stem>.state`.
- Movie mode (`--movie`): plays `.fm2` or `.bk2` (parser selected by extension) in a 4x 1024x960 window. Movie-only flags: `--headless`, `--no-throttle`, `--output` (ffmpeg recording), `--auto-fast-forward`, `--gamepad-overlay`, `--trim-frames`, `--trim-after`, `--extra-frames`, `--idle-frames`, `--crf`, `--play-from-power-on` (BK2 only), `--load-state` (resume at the state's frame; savestate-anchored BK2 is refused). `--output` recording upscales the 1x frame host-side into a reused 1024x960 scratch frame.
- `--palette <file.pal>` overrides the color theme in both modes.
- `--video-out <sdl|sixel|kitty>` (default `sdl`) selects the video sink in both modes: `sdl` opens the SDL2 window, `sixel`/`kitty` write escape sequences to stdout on the alternate screen (cursor-home per frame, logs stay on stderr). Terminal video bakes the 4x overlay frame then downsamples to 1x 256x240, keeps 60 FPS throttle, has no keyboard input (interactive warns on stderr, Ctrl-C stops).
- `--audio-out <sdl|none>` (default `sdl`) selects live audio in both modes: `none` skips SDL audio init and mutes live sound while `--output` recording keeps its track.

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

The SDL2 runner is a per-frame sink over the `nes_core` frame contract: each frame it presents `output.frame` and pushes `output.audio` through chunked queueing (`AUDIO_CHUNK_SAMPLES = 1024`, drops on queue pressure) plus `flush_frame()`. Recording additionally forwards the frame's samples to the encoder channel. Sample rate is `Apu::SAMPLE_RATE` (44.1 kHz mono `f32`, host resamples if needed); mixing happens in the core APU and the SDL side only transports frames of samples.
