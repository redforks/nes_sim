use crate::actions::{load_palette, read_ines_file};
use crate::sdl::{
    FastForwardState, INTERACTIVE_WINDOW, InputSource, LoopOptions, NesAudioDriver, NesRender,
    PLAYBACK_WINDOW, WindowSpec, create_sdl_drivers, run_event_loop, sdl2::Sdl,
};
use anyhow::{Context as _, Result};
use nes_core::{
    EmptyPlugin,
    ines::INesFile,
    movie::{self},
    nes::ppu::palette::ColorTheme,
    nes_machine::NesMachine,
};
use std::{cell::Cell, fs, io::Write, path::PathBuf, rc::Rc, sync::mpsc, thread};

#[derive(clap::Args)]
pub struct RunAction {
    /// Path to the .nes file
    nes_file: PathBuf,
    /// Path to a .pal palette file (64 RGB entries, 192 bytes)
    #[arg(long)]
    palette: Option<PathBuf>,
    #[arg(long)]
    movie: Option<PathBuf>,
    #[arg(short, long, requires = "movie")]
    output: Option<PathBuf>,
    #[arg(long, requires = "movie")]
    no_throttle: bool,
    #[arg(long, default_value_t = 600, requires = "movie")]
    extra_frames: u32,
    #[arg(long, requires = "movie")]
    headless: bool,
    #[arg(long, requires = "movie")]
    auto_fast_forward: bool,
    #[arg(long, default_value_t = 60, requires = "movie")]
    idle_frames: u32,
    #[arg(long, requires = "movie")]
    gamepad_overlay: bool,
    #[arg(long, default_value_t = 18, requires = "movie")]
    crf: u32,
    #[arg(long, default_value_t = 0, requires = "movie")]
    trim_frames: u32,
    /// Stop after N frames, using absolute frame numbers: loading a state
    /// at frame 900 with `--trim-after 1000` plays 100 frames.
    #[arg(long, default_value_t = 0, requires = "movie")]
    trim_after: u32,
    /// BK2 only: play savestate-anchored movies from power-on with a warning
    /// instead of refusing them.
    #[arg(long, requires = "movie")]
    play_from_power_on: bool,
    /// Start the interactive run from a saved state file. The file loads
    /// before the loop through the same code as F7; a missing or corrupt
    /// file fails before the window opens. Never changes the F5/F7 target
    /// (`<rom-dir>/<rom-stem>.state`).
    #[arg(long)]
    load_state: Option<PathBuf>,
}

/// Load the `--load-state` file into `machine` before the loop, through the
/// same shared helper as F7. On success returns the stderr line to print;
/// on failure returns the exact #184 error text so the caller can print it
/// and exit non-zero before the window opens. `None` is a no-op.
pub(crate) fn apply_load_state<P>(
    machine: &mut NesMachine<P>,
    load_state: Option<&std::path::Path>,
) -> Result<Option<String>, String>
where
    P: nes_core::Plugin<nes_core::nes::NesMcu>,
{
    let Some(path) = load_state else {
        return Ok(None);
    };
    crate::save_state::load_machine_from_path(machine, path).map(Some)
}

/// Refusal text when a savestate-anchored BK2 meets `--load-state` (#198).
/// `Some` always refuses, including with `--play-from-power-on`: frame 1 is
/// not power-on, so a cursor into power-on prefix entries cannot sync.
/// `None` plays on through the normal `check_movie` path.
pub(crate) fn anchored_load_state_error(movie: &nes_core::bk2::Bk2Movie) -> Option<String> {
    nes_core::bk2::savestate_anchor(movie).map(|_| {
        "savestate-anchored BK2 movie cannot be resumed with --load-state: frame 1 is not power-on"
            .to_owned()
    })
}

/// Cursor index for `--load-state --movie` (#198): the state's frame.
/// Entries before the cursor are skipped, since the state already holds
/// their effects; reset entries at or after the cursor play as today. A
/// movie shorter than the stored frame cannot resume: `Err` carries the
/// pre-window exit text. No movie identity is stored; the cursor is
/// trusted, not verified.
pub(crate) fn movie_cursor_index(movie_len: usize, state_frame: usize) -> Result<usize, String> {
    if state_frame > movie_len {
        Err(format!(
            "movie is shorter than the saved state (movie has {movie_len} frames, state is at frame {state_frame}); cannot resume with --load-state"
        ))
    } else {
        Ok(state_frame)
    }
}

/// True when the movie path selects the BK2 parser (by `.bk2` extension).
fn movie_file_is_bk2(path: &std::path::Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("bk2"))
}

struct FfmpegCleanup {
    ffmpeg: Option<std::process::Child>,
    temp_dir: Option<PathBuf>,
}

impl Drop for FfmpegCleanup {
    fn drop(&mut self) {
        if let Some(ref mut child) = self.ffmpeg {
            let _ = child.wait();
        }
        if let Some(ref dir) = self.temp_dir {
            let _ = fs::remove_dir_all(dir);
        }
    }
}

/// Prescan input_logs to find fast-forward ranges.
/// A frame at index `i` is fast-forward eligible if frames `i..i+idle_frames`
/// all have empty input on both ports.
fn compute_fast_forward_ranges(
    input_logs: &[movie::InputLog],
    idle_frames: usize,
) -> Vec<(usize, usize)> {
    if input_logs.len() < idle_frames {
        return Vec::new();
    }
    let mut ranges = Vec::new();
    let mut i = 0;
    while i < input_logs.len() {
        if input_logs[i].port0.is_empty() && input_logs[i].port1.is_empty() {
            let stretch_start = i;
            while i < input_logs.len()
                && input_logs[i].port0.is_empty()
                && input_logs[i].port1.is_empty()
            {
                i += 1;
            }
            let stretch_len = i - stretch_start;
            if stretch_len >= idle_frames {
                ranges.push((stretch_start, i - (idle_frames - 1)));
            }
        } else {
            i += 1;
        }
    }
    ranges
}

/// Build the unified SDL drivers for either mode: interactive passes the
/// interactive window, `headless = false`, no channels and `trim_frames = 0`;
/// movie playback passes its own window/flags/channels. With no recording
/// senders and the overlay disabled, `NesRender` presents exactly what the old
/// interactive renderer presented.
fn init_sdl_drivers(
    sdl_context: &Sdl,
    window: &WindowSpec,
    headless: bool,
    video_tx: Option<mpsc::Sender<Vec<u8>>>,
    audio_tx: Option<mpsc::Sender<Vec<u8>>>,
    ff_skip: Rc<Cell<bool>>,
    trim_frames: u32,
) -> Result<(NesRender, NesAudioDriver)> {
    let (canvas, audio_driver) = create_sdl_drivers(
        sdl_context,
        window,
        headless,
        audio_tx,
        ff_skip,
        trim_frames,
    )?;

    let render = NesRender::new(canvas, video_tx, trim_frames);

    Ok((render, audio_driver))
}

impl RunAction {
    pub fn run(&self) -> Result<()> {
        let f = read_ines_file(&self.nes_file)?;
        let palette = load_palette(self.palette.as_deref())?;
        if self.movie.is_none() {
            return self.run_interactive(&f, palette);
        }
        self.run_movie(&f, palette)
    }

    fn run_interactive(&self, f: &INesFile, palette: Option<ColorTheme>) -> Result<()> {
        // Create NES machine first so `--load-state` can load before the
        // window opens: a missing or corrupt file errors out here, before
        // any SDL initialization, with the exact #184 text and a non-zero
        // exit. This never changes the F5/F7 target, which stays
        // `<rom-dir>/<rom-stem>.state` below.
        let mut machine = NesMachine::new(f, EmptyPlugin::new());
        if let Some(theme) = palette {
            machine.set_color_theme(theme);
        }
        match apply_load_state(&mut machine, self.load_state.as_deref()) {
            Ok(None) => {}
            Ok(Some(message)) => eprintln!("{message}"),
            Err(message) => {
                eprintln!("{message}");
                anyhow::bail!("{message}");
            }
        }
        // Initialize SDL2
        let sdl_context = crate::sdl::sdl2::init().map_err(|e| anyhow::anyhow!(e))?;
        // Interactive mode never records, so the unified driver setup gets
        // no channels, a dummy never-set skip flag and zero trim frames.
        let (render, audio_driver) = init_sdl_drivers(
            &sdl_context,
            &INTERACTIVE_WINDOW,
            false,
            None,
            None,
            Rc::new(Cell::new(false)),
            0,
        )?;

        // SDL render/audio live beside the machine and consume the owned
        // framebuffer/audio buffers each frame.
        let (mut render, mut audio_driver) = (render, audio_driver);

        // Initialize event pump
        let mut event_pump = sdl_context.event_pump().map_err(|e| anyhow::anyhow!(e))?;

        // One loop body serves both modes: with no movie, the recording and
        // overlay slots of `NesRender` stay inactive and the interactive
        // input source keeps today's keys, window, throttle and exit path.
        let mut source = InputSource::Interactive;
        let opts = LoopOptions::interactive();
        let state_path = crate::save_state::state_path_for_rom(&self.nes_file);
        run_event_loop(
            &mut machine,
            &mut render,
            &mut audio_driver,
            Some(&mut event_pump),
            &mut source,
            &opts,
            None,
            Some(&state_path),
        );

        Ok(())
    }

    fn run_movie(&self, f: &INesFile, palette: Option<ColorTheme>) -> Result<()> {
        let movie_path = self.movie.as_ref().expect("--movie is present");
        let input_logs: Box<[movie::InputLog]> = if movie_file_is_bk2(movie_path) {
            let bk2_data = fs::read(movie_path)?;
            let movie = nes_core::bk2::parse(&bk2_data).context("parse bk2 file")?;
            // A savestate-anchored BK2 with `--load-state` is always refused
            // (#198), even with `--play-from-power-on`: frame 1 is not
            // power-on, so the cursor cannot sync. Before the window opens,
            // non-zero exit.
            if self.load_state.is_some()
                && let Some(error) = anchored_load_state_error(&movie)
            {
                eprintln!("{error}");
                anyhow::bail!("{error}");
            }
            let warnings = nes_core::bk2::check_movie(&movie, f, self.play_from_power_on)?;
            for warning in warnings {
                eprintln!("{warning}");
            }
            eprintln!(">> Loaded BK2 movie with {} frames", movie.input_logs.len());
            movie.input_logs
        } else {
            let fm2_data = fs::read(movie_path)?;
            let fm2 = movie::parse(&fm2_data).context("parse fm2 file")?;
            if !fm2.valid_rom_file(f)? {
                eprintln!("Warning: rom file checksum not match");
            }
            eprintln!(">> Loaded movie with {} frames", fm2.input_logs.len());
            fm2.input_logs
        };

        if input_logs.is_empty() {
            anyhow::bail!("Movie file has no input logs");
        }

        let (video_tx, audio_tx, cleanup) = if let Some(output_path) = &self.output {
            let temp_dir =
                std::env::temp_dir().join(format!("nes_recording_{}", std::process::id()));
            fs::create_dir_all(&temp_dir)?;
            let video_pipe = temp_dir.join("video");
            let audio_pipe = temp_dir.join("audio");

            std::process::Command::new("mkfifo")
                .arg(&video_pipe)
                .status()
                .context("failed to create video pipe (is mkfifo available?)")?;
            std::process::Command::new("mkfifo")
                .arg(&audio_pipe)
                .status()
                .context("failed to create audio pipe (is mkfifo available?)")?;

            let ffmpeg = std::process::Command::new("ffmpeg")
                .args([
                    "-y",
                    "-f",
                    "rawvideo",
                    "-pix_fmt",
                    "rgba",
                    "-s",
                    "1024x960",
                    "-r",
                    "60",
                    "-i",
                    video_pipe.to_str().unwrap(),
                    "-f",
                    "f32le",
                    "-ar",
                    "44100",
                    "-ac",
                    "1",
                    "-i",
                    audio_pipe.to_str().unwrap(),
                    "-c:v",
                    "libx264",
                    "-crf",
                    &self.crf.to_string(),
                    "-pix_fmt",
                    "yuv420p",
                    "-c:a",
                    "aac",
                    output_path.to_str().unwrap(),
                ])
                .spawn()
                .context("failed to spawn ffmpeg (is ffmpeg installed?)")?;

            let mut vf = fs::OpenOptions::new()
                .read(true)
                .write(true)
                .open(&video_pipe)?;
            let mut af = fs::OpenOptions::new()
                .read(true)
                .write(true)
                .open(&audio_pipe)?;

            let (video_tx, video_rx) = mpsc::channel::<Vec<u8>>();
            let (audio_tx, audio_rx) = mpsc::channel::<Vec<u8>>();

            thread::spawn(move || {
                for data in video_rx {
                    if vf.write_all(&data).is_err() {
                        break;
                    }
                }
            });

            thread::spawn(move || {
                for data in audio_rx {
                    if af.write_all(&data).is_err() {
                        break;
                    }
                }
            });

            let cleanup = FfmpegCleanup {
                ffmpeg: Some(ffmpeg),
                temp_dir: Some(temp_dir),
            };

            (Some(video_tx), Some(audio_tx), cleanup)
        } else {
            (
                None,
                None,
                FfmpegCleanup {
                    ffmpeg: None,
                    temp_dir: None,
                },
            )
        };

        let ff_skip = Rc::new(Cell::new(false));

        let sdl_context = crate::sdl::sdl2::init().map_err(|e| anyhow::anyhow!(e))?;
        let (render, audio_driver) = init_sdl_drivers(
            &sdl_context,
            &PLAYBACK_WINDOW,
            self.headless,
            video_tx,
            audio_tx,
            ff_skip.clone(),
            self.trim_frames,
        )?;

        // Build the machine before SDL opens so `--load-state` failures
        // (missing/corrupt file, short movie) exit pre-window, non-zero.
        let mut machine = NesMachine::new(f, EmptyPlugin::new());
        if let Some(theme) = palette {
            machine.set_color_theme(theme);
        }
        // The movie cursor starts at the state's frame: entries before it
        // are skipped, since the state already holds their effects. No
        // movie cursor goes into `NesMachine` or the DTO; `index: 0`
        // remains the no-load case.
        let start_index = match apply_load_state(&mut machine, self.load_state.as_deref()) {
            Ok(None) => 0,
            Ok(Some(message)) => {
                eprintln!("{message}");
                match movie_cursor_index(input_logs.len(), machine.frame_no()) {
                    Ok(index) => index,
                    Err(error) => {
                        eprintln!("{error}");
                        anyhow::bail!("{error}");
                    }
                }
            }
            Err(message) => {
                eprintln!("{message}");
                anyhow::bail!("{message}");
            }
        };
        let (mut render, mut audio_driver) = (render, audio_driver);
        let dual_player = input_logs.iter().any(|log| !log.port1.is_empty());
        render.set_dual_player(dual_player);
        render.set_overlay_enabled(self.gamepad_overlay);

        let mut event_pump = if !self.headless {
            Some(sdl_context.event_pump().map_err(|e| anyhow::anyhow!(e))?)
        } else {
            None
        };
        let fast_forward_ranges = if self.auto_fast_forward {
            compute_fast_forward_ranges(&input_logs, self.idle_frames as usize)
        } else {
            Vec::new()
        };

        // One loop body serves both modes: the movie input source feeds the
        // recorded logs with today's reset precedence (BK2 Power is a power
        // cycle; the BK2 Reset column and the FM2 reset flag are the
        // front-panel RESET button), gated throttle and verbatim
        // end-of-movie countdown.
        let mut source = InputSource::Movie {
            logs: input_logs,
            index: start_index,
        };
        let opts = LoopOptions {
            headless: self.headless,
            no_throttle: self.no_throttle,
            has_output: self.output.is_some(),
            auto_fast_forward: self.auto_fast_forward,
            trim_after: self.trim_after,
            extra_frames: self.extra_frames,
        };
        let fast_forward = FastForwardState {
            ranges: fast_forward_ranges,
            skip: ff_skip.clone(),
        };
        run_event_loop(
            &mut machine,
            &mut render,
            &mut audio_driver,
            event_pump.as_mut(),
            &mut source,
            &opts,
            Some(&fast_forward),
            None,
        );

        // Drop machine first to close channel senders, which lets pipe-writing
        // threads exit and close FIFOs so ffmpeg sees EOF and finishes.
        drop(machine);
        drop(cleanup);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use nes_core::movie::{Command, GamepadInput, InputLog};

    #[test]
    fn bk2_extension_selects_the_bk2_parser() {
        use super::movie_file_is_bk2;
        use std::path::Path;
        assert!(movie_file_is_bk2(Path::new("movie.bk2")));
        assert!(movie_file_is_bk2(Path::new("movie.BK2")));
        assert!(!movie_file_is_bk2(Path::new("movie.fm2")));
        assert!(!movie_file_is_bk2(Path::new("movie")));
    }

    #[test]
    fn bk2_reset_power_fixture_exposes_both_console_commands() {
        let data = include_bytes!("../../../testdata/bk2/bk2-reset-power-3f.bk2");
        let movie = nes_core::bk2::parse(data).expect("reset/power BK2 must parse");
        assert_eq!(movie.input_logs.len(), 3);
        assert!(movie.input_logs[1].command.soft_reset());
        assert!(movie.input_logs[2].command.hard_reset());
    }

    #[test]
    fn bk2_inputs_fixture_feeds_the_shared_fast_forward_seam() {
        // Flag parity (#103) is structural: BK2 parses to the same InputLog
        // type, so headless/overlay/fast-forward/trimming all run unchanged.
        let data = include_bytes!("../../../testdata/bk2/bk2-inputs-5f.bk2");
        let movie = nes_core::bk2::parse(data).expect("inputs BK2 must parse");
        let ranges = compute_fast_forward_ranges(&movie.input_logs, 60);
        assert!(
            ranges.is_empty(),
            "inputs fixture is never idle: {ranges:?}"
        );
        let blank = include_bytes!("../../../testdata/bk2/bk2-blank-3f.bk2");
        let blank = nes_core::bk2::parse(blank).unwrap();
        assert_eq!(
            compute_fast_forward_ranges(&blank.input_logs, 3),
            vec![(0, 1)],
            "all-idle BK2 fast-forwards like the FM2 equivalent"
        );
    }

    use super::compute_fast_forward_ranges;

    fn idle() -> InputLog {
        InputLog::new(
            Command::from_bytes([0]),
            GamepadInput::from_bytes([0]),
            GamepadInput::from_bytes([0]),
        )
    }

    fn active_port0() -> InputLog {
        InputLog::new(
            Command::from_bytes([0]),
            GamepadInput::from_bytes([1]),
            GamepadInput::from_bytes([0]),
        )
    }

    fn active_port1() -> InputLog {
        InputLog::new(
            Command::from_bytes([0]),
            GamepadInput::from_bytes([0]),
            GamepadInput::from_bytes([1]),
        )
    }

    const IDLE_FRAMES: usize = 60;

    #[test]
    fn empty_logs() {
        assert!(compute_fast_forward_ranges(&[], IDLE_FRAMES).is_empty());
    }

    #[test]
    fn less_than_idle_frames_all_idle() {
        let logs: Vec<_> = (0..59).map(|_| idle()).collect();
        assert!(compute_fast_forward_ranges(&logs, IDLE_FRAMES).is_empty());
    }

    #[test]
    fn exactly_idle_frames_idle() {
        let logs: Vec<_> = (0..60).map(|_| idle()).collect();
        assert_eq!(
            compute_fast_forward_ranges(&logs, IDLE_FRAMES),
            vec![(0, 1)]
        );
    }

    #[test]
    fn exactly_idle_plus_one_idle() {
        let logs: Vec<_> = (0..61).map(|_| idle()).collect();
        assert_eq!(
            compute_fast_forward_ranges(&logs, IDLE_FRAMES),
            vec![(0, 2)]
        );
    }

    #[test]
    fn hundred_idle_frames() {
        let logs: Vec<_> = (0..100).map(|_| idle()).collect();
        assert_eq!(
            compute_fast_forward_ranges(&logs, IDLE_FRAMES),
            vec![(0, 41)]
        );
    }

    #[test]
    fn no_idle_frames() {
        let logs: Vec<_> = (0..100).map(|_| active_port0()).collect();
        assert!(compute_fast_forward_ranges(&logs, IDLE_FRAMES).is_empty());
    }

    #[test]
    fn idle_stretch_too_short() {
        let mut logs: Vec<_> = (0..50).map(|_| idle()).collect();
        logs.extend((0..50).map(|_| active_port0()));
        assert!(compute_fast_forward_ranges(&logs, IDLE_FRAMES).is_empty());
    }

    #[test]
    fn active_frame_breaks_idle_stretch() {
        let mut logs: Vec<_> = (0..50).map(|_| idle()).collect();
        logs.push(active_port0());
        logs.extend((0..49).map(|_| idle()));
        assert!(compute_fast_forward_ranges(&logs, IDLE_FRAMES).is_empty());
    }

    #[test]
    fn two_separate_idle_ranges() {
        let mut logs: Vec<_> = (0..100).map(|_| idle()).collect();
        logs.push(active_port0());
        logs.extend((0..100).map(|_| idle()));
        assert_eq!(
            compute_fast_forward_ranges(&logs, IDLE_FRAMES),
            vec![(0, 41), (101, 142)]
        );
    }

    #[test]
    fn only_last_idle_frames_idle() {
        let mut logs: Vec<_> = (0..60).map(|_| active_port0()).collect();
        logs.extend((0..60).map(|_| idle()));
        assert_eq!(
            compute_fast_forward_ranges(&logs, IDLE_FRAMES),
            vec![(60, 61)]
        );
    }

    #[test]
    fn first_frame_active_rest_idle() {
        let mut logs = vec![active_port0()];
        logs.extend((0..99).map(|_| idle()));
        assert_eq!(
            compute_fast_forward_ranges(&logs, IDLE_FRAMES),
            vec![(1, 41)]
        );
    }

    #[test]
    fn active_frame_within_lookahead_window() {
        let mut logs: Vec<_> = (0..40).map(|_| idle()).collect();
        logs.push(active_port1());
        logs.extend((0..39).map(|_| idle()));
        assert!(compute_fast_forward_ranges(&logs, IDLE_FRAMES).is_empty());
    }

    #[test]
    fn port0_active_counts_as_non_idle() {
        let mut logs: Vec<_> = (0..60).map(|_| idle()).collect();
        logs.push(InputLog::new(
            Command::from_bytes([0]),
            GamepadInput::from_bytes([0b00001000]),
            GamepadInput::from_bytes([0]),
        ));
        assert_eq!(
            compute_fast_forward_ranges(&logs, IDLE_FRAMES),
            vec![(0, 1)]
        );
    }

    #[test]
    fn port1_active_counts_as_non_idle() {
        let mut logs: Vec<_> = (0..60).map(|_| idle()).collect();
        logs.push(InputLog::new(
            Command::from_bytes([0]),
            GamepadInput::from_bytes([0]),
            GamepadInput::from_bytes([0b00000010]),
        ));
        assert_eq!(
            compute_fast_forward_ranges(&logs, IDLE_FRAMES),
            vec![(0, 1)]
        );
    }

    #[test]
    fn both_ports_must_be_empty() {
        let mut logs: Vec<_> = (0..60).map(|_| idle()).collect();
        logs.push(InputLog::new(
            Command::from_bytes([0]),
            GamepadInput::from_bytes([0]),
            GamepadInput::from_bytes([0b00000001]),
        ));
        assert_eq!(
            compute_fast_forward_ranges(&logs, IDLE_FRAMES),
            vec![(0, 1)]
        );
    }

    #[test]
    fn custom_idle_frames() {
        let logs: Vec<_> = (0..30).map(|_| idle()).collect();
        assert_eq!(compute_fast_forward_ranges(&logs, 30), vec![(0, 1)]);
        assert!(compute_fast_forward_ranges(&logs, 31).is_empty());
    }

    mod load_state_cli {
        use super::super::{RunAction, apply_load_state};
        use clap::Parser;
        use nes_core::{EmptyPlugin, ines::INesFile, nes::NesMcu, nes_machine::NesMachine};
        use std::path::{Path, PathBuf};

        /// Clap harness: `RunAction` is `Args`, so it is flattened into a
        /// test parser to exercise `--load-state` parsing and help text.
        #[derive(clap::Parser)]
        struct TestCli {
            #[command(flatten)]
            run: RunAction,
        }

        /// Minimal valid NROM iNES file (mirrors the `save_state` fixture).
        fn nrom_file() -> INesFile {
            let mut rom = Vec::new();
            rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a, 0x01, 0x01, 0x00, 0x00]);
            rom.extend_from_slice(&[0; 8]);
            let prg_len = 16 * 1024;
            rom.extend_from_slice(&[0x4C, 0x00, 0x80]);
            rom.extend(std::iter::repeat_n(0, prg_len - 3));
            rom.extend(std::iter::repeat_n(0, 8 * 1024));
            INesFile::new(rom).unwrap()
        }

        fn machine_for(file: &INesFile) -> NesMachine<EmptyPlugin<NesMcu>> {
            NesMachine::new(file, EmptyPlugin::new())
        }

        fn scratch_dir(name: &str) -> PathBuf {
            let dir =
                std::env::temp_dir().join(format!("nescli_load_{}_{}", std::process::id(), name));
            let _ = std::fs::remove_dir_all(&dir);
            std::fs::create_dir_all(&dir).unwrap();
            dir
        }

        #[test]
        fn load_state_flag_parses_with_explicit_path_and_no_default() {
            let cli = TestCli::try_parse_from(["test", "game.nes"]).unwrap();
            assert!(cli.run.load_state.is_none(), "no default path");
            let cli =
                TestCli::try_parse_from(["test", "game.nes", "--load-state", "/saves/game.state"])
                    .unwrap();
            assert_eq!(cli.run.load_state, Some(PathBuf::from("/saves/game.state")));
        }

        #[test]
        fn load_state_flag_appears_in_help() {
            let mut help = Vec::new();
            use clap::CommandFactory;
            TestCli::command().write_long_help(&mut help).unwrap();
            let help = String::from_utf8(help).unwrap();
            assert!(
                help.contains("--load-state"),
                "run --help must show the flag: {help}"
            );
        }

        #[test]
        fn no_flag_is_a_no_op() {
            let file = nrom_file();
            let mut machine = machine_for(&file);
            machine.process_frame();
            assert_eq!(apply_load_state(&mut machine, None).unwrap(), None);
            assert_eq!(machine.frame_no(), 1);
        }

        #[test]
        fn loaded_run_starts_from_the_saved_frame() {
            let file = nrom_file();
            let mut reference = machine_for(&file);
            for _ in 0..5 {
                reference.process_frame();
            }
            let dir = scratch_dir("saved-frame");
            let path = dir.join("game.state");
            crate::save_state::save_machine_to_path(&reference, &path).unwrap();

            let mut loaded = machine_for(&file);
            let message = apply_load_state(&mut loaded, Some(&path))
                .expect("valid save must load")
                .expect("message on success");
            assert_eq!(message, format!("loaded state from {}", path.display()));
            assert_eq!(loaded.frame_no(), reference.frame_no());

            // Continuing matches the uninterrupted run frame-for-frame.
            for _ in 0..3 {
                let a = reference.process_frame();
                let b = loaded.process_frame();
                assert_eq!(b.frame.to_vec(), a.frame.to_vec());
                assert_eq!(b.audio.to_vec(), a.audio.to_vec());
            }
            let _ = std::fs::remove_dir_all(&dir);
        }

        #[test]
        fn missing_file_returns_the_spec_text_for_pre_window_exit() {
            let file = nrom_file();
            let mut machine = machine_for(&file);
            machine.process_frame();
            let path = PathBuf::from("/nonexistent_dir_nescli_load/game.state");

            let error =
                apply_load_state(&mut machine, Some(&path)).expect_err("missing file must fail");
            assert_eq!(error, format!("no save state at {}", path.display()));
            // run_interactive prints this text then bails, so `run`
            // returns Err and the process exits non-zero before SDL opens.
            assert_eq!(machine.frame_no(), 1, "machine untouched");
        }

        #[test]
        fn corrupt_file_returns_the_spec_text_for_pre_window_exit() {
            let file = nrom_file();
            let mut machine = machine_for(&file);
            machine.process_frame();
            let dir = scratch_dir("corrupt");
            let path = dir.join("game.state");
            std::fs::write(&path, "not json{{{").unwrap();

            let error =
                apply_load_state(&mut machine, Some(&path)).expect_err("corrupt file must fail");
            assert!(
                error.starts_with("save state is corrupt: "),
                "unexpected error line: {error}"
            );
            assert_eq!(machine.frame_no(), 1, "machine untouched");
            let _ = std::fs::remove_dir_all(&dir);
        }

        #[test]
        fn load_state_never_changes_the_f5_f7_target() {
            // The F5/F7 target is derived from the ROM path only; the
            // `--load-state` path is used solely for the pre-loop load.
            let rom = Path::new("/games/mario.nes");
            let custom = PathBuf::from("/saves/other.state");
            assert_eq!(
                crate::save_state::state_path_for_rom(rom),
                PathBuf::from("/games/mario.state")
            );
            assert_ne!(crate::save_state::state_path_for_rom(rom), custom);
        }
    }

    mod movie_cursor {
        use super::super::{anchored_load_state_error, movie_cursor_index};
        use clap::Parser;
        use nes_core::{
            EmptyPlugin,
            bk2::{AnchoredKind, Bk2Header, Bk2Movie},
            ines::INesFile,
            movie::{Command, GamepadInput, InputLog},
            nes::{NesMcu, controller::Button},
            nes_machine::NesMachine,
        };
        use std::path::PathBuf;

        /// Clap harness: `RunAction` is `Args`, so it is flattened into a
        /// test parser to exercise flag parsing and help text.
        #[derive(clap::Parser)]
        struct TestCli {
            #[command(flatten)]
            run: super::super::RunAction,
        }

        /// Minimal valid NROM iNES file (mirrors the `save_state` fixture).
        fn nrom_file() -> INesFile {
            let mut rom = Vec::new();
            rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a, 0x01, 0x01, 0x00, 0x00]);
            rom.extend_from_slice(&[0; 8]);
            let prg_len = 16 * 1024;
            rom.extend_from_slice(&[0x4c, 0x00, 0x80]);
            rom.extend(std::iter::repeat_n(0, prg_len - 3));
            rom.extend(std::iter::repeat_n(0, 8 * 1024));
            INesFile::new(rom).unwrap()
        }

        fn machine_for(file: &INesFile) -> NesMachine<EmptyPlugin<NesMcu>> {
            NesMachine::new(file, EmptyPlugin::new())
        }

        fn scratch_dir(name: &str) -> PathBuf {
            let dir =
                std::env::temp_dir().join(format!("nescli_movie_{}_{}", std::process::id(), name));
            let _ = std::fs::remove_dir_all(&dir);
            std::fs::create_dir_all(&dir).unwrap();
            dir
        }

        #[test]
        fn cursor_starts_at_the_state_frame() {
            assert_eq!(movie_cursor_index(12, 0), Ok(0), "no-load case stays 0");
            assert_eq!(movie_cursor_index(12, 5), Ok(5));
            assert_eq!(
                movie_cursor_index(12, 12),
                Ok(12),
                "state at the exact end resumes into the extra-frames countdown"
            );
        }

        #[test]
        fn short_movie_errors_with_exact_text() {
            assert_eq!(
                movie_cursor_index(3, 5),
                Err("movie is shorter than the saved state (movie has 3 frames, state is at frame 5); cannot resume with --load-state".to_owned())
            );
        }

        fn unanchored_movie() -> Bk2Movie {
            Bk2Movie {
                header: Bk2Header::default(),
                input_logs: Vec::new().into_boxed_slice(),
                sync_settings: None,
                anchored: None,
            }
        }

        #[test]
        fn clean_bk2_with_load_state_plays_on() {
            assert_eq!(anchored_load_state_error(&unanchored_movie()), None);
        }

        #[test]
        fn anchored_bk2_with_load_state_is_refused() {
            for kind in [
                AnchoredKind::CoreState,
                AnchoredKind::SaveRam,
                AnchoredKind::ClaimedFlag,
            ] {
                let movie = Bk2Movie {
                    anchored: Some(kind),
                    ..unanchored_movie()
                };
                assert_eq!(
                    anchored_load_state_error(&movie),
                    Some("savestate-anchored BK2 movie cannot be resumed with --load-state: frame 1 is not power-on".to_owned()),
                    "{kind:?} must refuse even with --play-from-power-on"
                );
            }
        }

        fn log_with(a: bool, b: bool, start: bool) -> InputLog {
            InputLog::new(
                Command::default(),
                GamepadInput::new().with_a(a).with_b(b).with_start(start),
                GamepadInput::default(),
            )
        }

        /// Mirror of the event-loop per-entry bookkeeping (reset applies with
        /// no frame, otherwise controllers latch then one frame emulates).
        fn emulate_log(
            machine: &mut NesMachine<EmptyPlugin<NesMcu>>,
            log: &InputLog,
        ) -> Option<(Vec<u8>, Vec<f32>)> {
            if log.command.hard_reset() {
                machine.hard_reset();
                return None;
            }
            if log.command.soft_reset() {
                machine.soft_reset();
                return None;
            }
            for (button, pressed) in [
                (Button::A, log.port0.a()),
                (Button::B, log.port0.b()),
                (Button::Select, log.port0.select()),
                (Button::Start, log.port0.start()),
                (Button::Up, log.port0.up()),
                (Button::Down, log.port0.down()),
                (Button::Left, log.port0.left()),
                (Button::Right, log.port0.right()),
            ] {
                if pressed {
                    machine.press_controller_a(button);
                } else {
                    machine.release_controller_a(button);
                }
            }
            let out = machine.process_frame();
            Some((out.frame.to_vec(), out.audio.to_vec()))
        }

        #[test]
        fn movie_with_load_state_matches_continuous_run_from_saved_frame() {
            // Twelve plain input entries, no resets: the spec assumes no
            // movie hard reset before the save, so index and frame stay in
            // lockstep and the cursor is the state's frame.
            let logs: Vec<InputLog> = (0..12)
                .map(|i| log_with(i % 2 == 0, i % 3 == 0, i == 7))
                .collect();
            let file = nrom_file();

            // Continuous reference: every entry from power-on.
            let mut reference = machine_for(&file);
            let mut expected = Vec::new();
            for log in &logs {
                if let Some(output) = emulate_log(&mut reference, log) {
                    expected.push((reference.frame_no(), output));
                }
            }
            assert_eq!(expected.len(), 12);
            assert_eq!(reference.frame_no(), 12);

            // Save after 5 frames, load fresh, resume at the cursor.
            let mut saver = machine_for(&file);
            for log in &logs[..5] {
                assert!(emulate_log(&mut saver, log).is_some());
            }
            let dir = scratch_dir("cursor");
            let path = dir.join("game.state");
            crate::save_state::save_machine_to_path(&saver, &path).unwrap();

            let mut loaded = machine_for(&file);
            crate::save_state::load_machine_from_path(&mut loaded, &path).unwrap();
            assert_eq!(loaded.frame_no(), 5);
            let cursor = movie_cursor_index(logs.len(), loaded.frame_no()).unwrap();
            assert_eq!(cursor, 5, "entries before the cursor are skipped");

            let mut resumed = Vec::new();
            for log in &logs[cursor..] {
                if let Some(output) = emulate_log(&mut loaded, log) {
                    resumed.push((loaded.frame_no(), output));
                }
            }
            assert_eq!(resumed, expected[5..]);
            let _ = std::fs::remove_dir_all(&dir);
        }

        #[test]
        fn trim_after_help_says_absolute_frame_numbers() {
            let mut help = Vec::new();
            use clap::CommandFactory;
            TestCli::command().write_long_help(&mut help).unwrap();
            let help = String::from_utf8(help).unwrap();
            let pos = help
                .find("--trim-after")
                .expect("run --help must show the flag");
            let window = &help[pos..(pos + 400).min(help.len())];
            assert!(
                window.contains("absolute"),
                "trim-after help must say it uses absolute frame numbers: {window}"
            );
            assert!(
                help.contains("900") && help.contains("1000"),
                "trim-after help must carry the load-at-900 example: {help}"
            );
        }

        #[test]
        fn trim_after_still_requires_movie() {
            // `RunAction` is not `Debug`, so no `expect_err`: assert on the
            // `Result` shape directly.
            match TestCli::try_parse_from(["test", "game.nes", "--trim-after", "1000"]) {
                Ok(_) => panic!("--trim-after without --movie must fail"),
                Err(error) => assert!(error.to_string().contains("--movie"), "{error}"),
            }
        }
    }
}
