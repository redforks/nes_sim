use crate::sdl::{
    FastForwardState, INTERACTIVE_WINDOW, InputSource, LoopOptions, NesRender, PLAYBACK_WINDOW,
    SdlAudioDriver, create_sdl_drivers, run_event_loop, sdl2::Sdl,
};
use anyhow::{Context as _, Result};
use nes_core::{
    EmptyPlugin,
    ines::INesFile,
    movie::{self},
    nes::{apu::AudioDriver, ppu::palette::ColorTheme},
    nes_machine::NesMachine,
};
use std::{cell::Cell, fs, io::Write, path::PathBuf, rc::Rc, sync::mpsc, thread};

fn create_interactive_drivers(
    sdl_context: &crate::sdl::sdl2::Sdl,
) -> Result<(NesRender, SdlAudioDriver)> {
    let (canvas, audio_driver) = create_sdl_drivers(sdl_context, &INTERACTIVE_WINDOW, false)?;
    // Interactive mode is always windowed, so the factory always returns a canvas.
    let canvas = canvas.expect("windowed SDL setup must return a canvas");

    // One render type serves both modes: with no recording sender and the
    // overlay disabled, `NesRender` presents exactly what `SdlRender` presented.
    let render = NesRender::new(Some(canvas), None, 0);
    Ok((render, audio_driver))
}

#[derive(clap::Args)]
pub struct RunAction {
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
    #[arg(long, default_value_t = 0, requires = "movie")]
    trim_after: u32,
    /// BK2 only: play savestate-anchored movies from power-on with a warning
    /// instead of refusing them.
    #[arg(long, requires = "movie")]
    play_from_power_on: bool,
}

/// True when the movie path selects the BK2 parser (by `.bk2` extension).
fn movie_file_is_bk2(path: &std::path::Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("bk2"))
}

struct RecordAudioDriver {
    inner: SdlAudioDriver,
    audio_tx: Option<mpsc::Sender<Vec<u8>>>,
    sample_buf: Vec<f32>,
    ff_skip: Rc<Cell<bool>>,
    send_policy: crate::recording_send::RecordingSendPolicy,
}

impl AudioDriver for RecordAudioDriver {
    fn sample_rate(&self) -> u32 {
        self.inner.sample_rate()
    }

    fn push_sample(&mut self, sample: f32) {
        self.inner.push_sample(sample);
        if self.audio_tx.is_some() {
            self.sample_buf.push(sample);
        }
    }

    fn flush(&mut self) {
        self.inner.flush();
        let send_audio = self.send_policy.should_send(self.ff_skip.get());
        if let Some(ref tx) = self.audio_tx {
            if !self.sample_buf.is_empty() && send_audio {
                let bytes: Vec<u8> = self
                    .sample_buf
                    .iter()
                    .flat_map(|s| s.to_le_bytes())
                    .collect();
                let _ = tx.send(bytes);
            }
            self.sample_buf.clear();
        }
    }
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

fn init_sdl_drivers(
    sdl_context: &Sdl,
    headless: bool,
    video_tx: Option<mpsc::Sender<Vec<u8>>>,
    audio_tx: Option<mpsc::Sender<Vec<u8>>>,
    ff_skip: Rc<Cell<bool>>,
    trim_frames: u32,
) -> Result<(NesRender, RecordAudioDriver)> {
    let (canvas, sdl_audio) = create_sdl_drivers(sdl_context, &PLAYBACK_WINDOW, headless)?;

    let render = NesRender::new(canvas, video_tx, trim_frames);

    let audio_driver = RecordAudioDriver {
        inner: sdl_audio,
        audio_tx,
        sample_buf: Vec::new(),
        ff_skip,
        send_policy: crate::recording_send::RecordingSendPolicy::new(trim_frames),
    };

    Ok((render, audio_driver))
}

impl RunAction {
    pub fn run(&self, f: &INesFile, palette: Option<ColorTheme>) -> Result<()> {
        if self.movie.is_none() {
            return self.run_interactive(f, palette);
        }
        self.run_movie(f, palette)
    }

    fn run_interactive(&self, f: &INesFile, palette: Option<ColorTheme>) -> Result<()> {
        // Initialize SDL2
        let sdl_context = crate::sdl::sdl2::init().map_err(|e| anyhow::anyhow!(e))?;
        let (render, audio_driver) = create_interactive_drivers(&sdl_context)?;

        // Create NES machine with SDL-backed renderer
        let mut machine = NesMachine::new(f, EmptyPlugin::new(), render, audio_driver);
        if let Some(theme) = palette {
            machine.set_color_theme(theme);
        }

        // Initialize event pump
        let mut event_pump = sdl_context.event_pump().map_err(|e| anyhow::anyhow!(e))?;

        // One loop body serves both modes: with no movie, the recording and
        // overlay slots of `NesRender` stay inactive and the interactive
        // input source keeps today's keys, window, throttle and exit path.
        let mut source = InputSource::Interactive;
        let opts = LoopOptions::interactive();
        run_event_loop(
            &mut machine,
            Some(&mut event_pump),
            &mut source,
            &opts,
            None,
        );

        Ok(())
    }

    fn run_movie(&self, f: &INesFile, palette: Option<ColorTheme>) -> Result<()> {
        let movie_path = self.movie.as_ref().expect("--movie is present");
        let input_logs: Box<[movie::InputLog]> = if movie_file_is_bk2(movie_path) {
            let bk2_data = fs::read(movie_path)?;
            let movie = nes_core::bk2::parse(&bk2_data).context("parse bk2 file")?;
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
            self.headless,
            video_tx,
            audio_tx,
            ff_skip.clone(),
            self.trim_frames,
        )?;

        let mut machine = NesMachine::new(f, EmptyPlugin::new(), render, audio_driver);
        if let Some(theme) = palette {
            machine.set_color_theme(theme);
        }
        let dual_player = input_logs.iter().any(|log| !log.port1.is_empty());
        machine.render_mut().set_dual_player(dual_player);
        machine
            .render_mut()
            .set_overlay_enabled(self.gamepad_overlay);

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
            index: 0,
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
            event_pump.as_mut(),
            &mut source,
            &opts,
            Some(&fast_forward),
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
}
