use anyhow::{Context as _, Result};
use image::{imageops, load_from_memory};
use nes_core::{
    EmptyPlugin,
    ines::INesFile,
    movie::{self, GamepadInput},
    nes::{NesMcu, apu::AudioDriver, controller::Button, ppu::palette::ColorTheme},
    nes_machine::NesMachine,
    render::{
        ImageRender, Render,
        image_render::{NES_HEIGHT, NES_WIDTH},
    },
};
use nes_sdl::{
    self, AUDIO_CHUNK_SAMPLES, AUDIO_SAMPLE_RATE, FramePresenter, SdlAudioDriver, present_frame,
    presentation::{OverlayIcon, OverlayPlacement},
    sdl2::{Sdl, audio::AudioSpecDesired, event::Event, render::Canvas, video::Window},
};
use std::{
    cell::Cell,
    fs,
    io::Write,
    path::PathBuf,
    rc::Rc,
    sync::mpsc,
    thread,
    time::{Duration, Instant},
};

fn set_controller_a_from_gamepad<P, R, D>(machine: &mut NesMachine<P, R, D>, input: &GamepadInput)
where
    P: nes_core::Plugin<NesMcu<R, D>>,
    R: Render,
    D: AudioDriver,
{
    for (button, pressed) in [
        (Button::A, input.a()),
        (Button::B, input.b()),
        (Button::Select, input.select()),
        (Button::Start, input.start()),
        (Button::Up, input.up()),
        (Button::Down, input.down()),
        (Button::Left, input.left()),
        (Button::Right, input.right()),
    ] {
        if pressed {
            if button == Button::Start {
                eprintln!("press {:?}", button);
            }
            machine.press_controller_a(button);
        } else {
            machine.release_controller_a(button);
        }
    }
}

fn set_controller_b_from_gamepad<P, R, D>(machine: &mut NesMachine<P, R, D>, input: &GamepadInput)
where
    P: nes_core::Plugin<NesMcu<R, D>>,
    R: Render,
    D: AudioDriver,
{
    for (button, pressed) in [
        (Button::A, input.a()),
        (Button::B, input.b()),
        (Button::Select, input.select()),
        (Button::Start, input.start()),
        (Button::Up, input.up()),
        (Button::Down, input.down()),
        (Button::Left, input.left()),
        (Button::Right, input.right()),
    ] {
        if pressed {
            if button == Button::Start {
                eprintln!("press {:?}", button);
            }
            machine.press_controller_b(button);
        } else {
            machine.release_controller_b(button);
        }
    }
}

#[derive(clap::Args)]
pub struct PlayMovieAction {
    movie_file: PathBuf,
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[arg(long)]
    no_throttle: bool,
    #[arg(long, default_value_t = 600)]
    extra_frames: u32,
    #[arg(long)]
    headless: bool,
    #[arg(long)]
    auto_fast_forward: bool,
    #[arg(long, default_value_t = 60)]
    idle_frames: u32,
    #[arg(long)]
    gamepad_overlay: bool,
    #[arg(long, default_value_t = 18)]
    crf: u32,
    #[arg(long, default_value_t = 0)]
    trim_frames: u32,
    #[arg(long, default_value_t = 0)]
    trim_after: u32,
    /// BK2 only: play savestate-anchored movies from power-on with a warning
    /// instead of refusing them.
    #[arg(long)]
    play_from_power_on: bool,
}

/// True when the movie path selects the BK2 parser (by `.bk2` extension).
fn movie_file_is_bk2(path: &std::path::Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("bk2"))
}

struct GamepadOverlay {
    a: bool,
    b: bool,
    up: bool,
    down: bool,
    left: bool,
    right: bool,
    disp_a: bool,
    disp_b: bool,
    disp_up: bool,
    disp_down: bool,
    disp_left: bool,
    disp_right: bool,
    hold_dir: u8,
    hold_ab: u8,
}

impl GamepadOverlay {
    fn new() -> Self {
        Self {
            a: false,
            b: false,
            up: false,
            down: false,
            left: false,
            right: false,
            disp_a: false,
            disp_b: false,
            disp_up: false,
            disp_down: false,
            disp_left: false,
            disp_right: false,
            hold_dir: 0,
            hold_ab: 0,
        }
    }

    fn set_input(&mut self, input: &GamepadInput) {
        self.a = input.a();
        self.b = input.b();
        self.up = input.up();
        self.down = input.down();
        self.left = input.left();
        self.right = input.right();

        let any_dir = self.up || self.down || self.left || self.right;
        if any_dir {
            self.disp_up = self.up;
            self.disp_down = self.down;
            self.disp_left = self.left;
            self.disp_right = self.right;
            self.hold_dir = 18;
        }

        let any_ab = self.a || self.b;
        if any_ab {
            self.disp_a = self.a;
            self.disp_b = self.b;
            self.hold_ab = 18;
        }
    }

    /// Advance the hold counters and project the icons to show this frame, in
    /// logical 1x 256x240 coordinates. Pure: it touches no pixels and needs no
    /// display, so the hold countdown and the icon table are unit-testable.
    fn project(&mut self, layout: OverlayIconLayout) -> Vec<OverlayPlacement> {
        let draw_dir = self.hold_dir > 0;
        let draw_ab = self.hold_ab > 0;
        if draw_dir {
            self.hold_dir -= 1;
        }
        if draw_ab {
            self.hold_ab -= 1;
        }
        let mut icons = Vec::with_capacity(3);
        if !draw_dir && !draw_ab {
            return icons;
        }
        let slot = |x: i32| (x, layout.y, OVERLAY_ICON_SIZE, OVERLAY_ICON_SIZE);
        if let Some(direction_idx) = Self::direction_icon_index(
            draw_dir && self.disp_up,
            draw_dir && self.disp_down,
            draw_dir && self.disp_left,
            draw_dir && self.disp_right,
        ) {
            icons.push(OverlayPlacement {
                icon: direction_idx,
                dst: slot(layout.dir_x),
            });
        }
        if draw_ab && self.disp_a {
            icons.push(OverlayPlacement {
                icon: 0,
                dst: slot(layout.ab_x),
            });
        }
        if draw_ab && self.disp_b {
            icons.push(OverlayPlacement {
                icon: 1,
                dst: slot(layout.b_x),
            });
        }
        icons
    }

    fn direction_icon_index(up: bool, down: bool, left: bool, right: bool) -> Option<usize> {
        match (up, down, left, right) {
            (false, false, false, false) => None,
            (false, false, false, true) => Some(4),
            (false, false, true, false) => Some(3),
            (false, false, true, true) => Some(6),
            (false, true, false, false) => Some(2),
            (false, true, false, true) => Some(9),
            (false, true, true, false) => Some(7),
            (false, true, true, true) => Some(12),
            (true, false, false, false) => Some(5),
            (true, false, false, true) => Some(10),
            (true, false, true, false) => Some(8),
            (true, false, true, true) => Some(14),
            (true, true, false, false) => Some(11),
            (true, true, false, true) => Some(15),
            (true, true, true, false) => Some(13),
            (true, true, true, true) => Some(16),
        }
    }
}

/// Logical 1x 256x240 x-positions of one player's three overlay icon slots.
#[derive(Debug, Clone, Copy)]
struct OverlayIconLayout {
    dir_x: i32,
    ab_x: i32,
    b_x: i32,
    y: i32,
}

/// Dual-player layout: player 1's icons on the left.
const OVERLAY_LAYOUT_LEFT: OverlayIconLayout = OverlayIconLayout {
    dir_x: 2,
    ab_x: 38,
    b_x: 20,
    y: 222,
};

/// Single-player layout, also player 2's side of the dual-player layout.
const OVERLAY_LAYOUT_RIGHT: OverlayIconLayout = OverlayIconLayout {
    dir_x: 205,
    ab_x: 238,
    b_x: 222,
    y: 222,
};

/// Sprite-sheet crop size in pixels: one square icon per artwork cell.
const OVERLAY_SHEET_ICON_SIZE: u32 = 64;

/// Overlay icon size in logical 1x pixels: a sprite-sheet crop divided by the
/// playback Zoom Factor of 4.
const OVERLAY_ICON_SIZE: u32 = OVERLAY_SHEET_ICON_SIZE / 4;

/// Which overlay icons to show this frame and where, in logical 1x
/// coordinates. Advances both players' hold counters exactly once per frame.
fn project_overlay_icons(
    p1: &mut GamepadOverlay,
    p2: &mut GamepadOverlay,
    dual_player: bool,
) -> Vec<OverlayPlacement> {
    let mut icons = Vec::new();
    if dual_player {
        icons.extend(p1.project(OVERLAY_LAYOUT_LEFT));
        icons.extend(p2.project(OVERLAY_LAYOUT_RIGHT));
    } else {
        icons.extend(p1.project(OVERLAY_LAYOUT_RIGHT));
    }
    icons
}

/// Recording frame size in pixels: the playback window, an exact 4x upscale
/// of the logical 1x frame.
const RECORD_WIDTH: usize = 1024;
const RECORD_HEIGHT: usize = 960;

/// Expand a logical 1x playback frame into the 1024x960 RGBA recording
/// scratch frame, replicating each logical pixel into a 4x4 block. This is
/// exactly the replication `ImageRender<4>::set_pixel` performs, so a
/// recording built from the 1x frame is byte-identical to one built from a
/// 4x frame.
fn upscale_into(src: &[u8], dst: &mut [u8]) {
    debug_assert_eq!(src.len(), NES_WIDTH * NES_HEIGHT * 4);
    debug_assert_eq!(dst.len(), RECORD_WIDTH * RECORD_HEIGHT * 4);
    let src_stride = NES_WIDTH * 4;
    let dst_stride = RECORD_WIDTH * 4;
    for y in 0..NES_HEIGHT {
        for x in 0..NES_WIDTH {
            let pixel = &src[y * src_stride + x * 4..][..4];
            for dy in 0..4 {
                let row = (y * 4 + dy) * dst_stride;
                for dx in 0..4 {
                    dst[row + (x * 4 + dx) * 4..][..4].copy_from_slice(pixel);
                }
            }
        }
    }
}

/// Composite overlay icons into a recording frame (RGBA bytes). The recording
/// artefact keeps today's shape: the 4x 1024x960 frame with the overlay baked
/// in, straight-alpha blended by the host.
fn composite_overlay_icons(
    bytes: &mut [u8],
    placements: &[OverlayPlacement],
    icons: &[OverlayIcon],
) {
    let stride = RECORD_WIDTH as i64;
    let height = RECORD_HEIGHT as i64;
    let scale = stride / NES_WIDTH as i64;
    for placement in placements {
        let Some(icon) = icons.get(placement.icon) else {
            continue;
        };
        blit_icon(
            bytes,
            stride,
            height,
            icon,
            placement.dst.0 as i64 * scale,
            placement.dst.1 as i64 * scale,
        );
    }
}

/// Blit one RGBA icon over an RGBA frame with straight-alpha blending.
fn blit_icon(
    bytes: &mut [u8],
    stride: i64,
    height: i64,
    icon: &OverlayIcon,
    dst_x: i64,
    dst_y: i64,
) {
    let iw = icon.width as i64;
    let ih = icon.height as i64;
    for iy in 0..ih {
        for ix in 0..iw {
            let dx = dst_x + ix;
            let dy = dst_y + iy;
            if dx < 0 || dy < 0 || dx >= stride || dy >= height {
                continue;
            }
            let src = &icon.bytes[((iy * iw + ix) * 4) as usize..][..4];
            if src[3] == 0 {
                continue;
            }
            let dst = &mut bytes[(dy * stride + dx) as usize * 4..][..4];
            if src[3] == 255 {
                dst.copy_from_slice(src);
            } else {
                // alpha blend src over dst (straight alpha, non-premul)
                let a = src[3] as u32;
                let inv = 255 - a;
                for c in 0..3 {
                    dst[c] = ((src[c] as u32 * a + dst[c] as u32 * inv) / 255) as u8;
                }
                dst[3] = 255;
            }
        }
    }
}

struct RecordRender {
    buffer: ImageRender<1>,
    /// 1024x960 recording frame: raw RGBA pixels in the format fed to ffmpeg
    /// (`RECORD_WIDTH * RECORD_HEIGHT * 4` bytes). The allocation is reused
    /// across frames; each frame actually sent is cloned into a fresh buffer
    /// for the encoder thread.
    record_scratch: Vec<u8>,
    canvas: Option<Canvas<Window>>,
    presenter: FramePresenter,
    video_tx: Option<mpsc::Sender<Vec<u8>>>,
    icons: Rc<[OverlayIcon]>,
    dual_player: bool,
    enabled: bool,
    p1: GamepadOverlay,
    p2: GamepadOverlay,
    ff_skip: bool,
    ff_counter: u32,
    trim_frames: u32,
    trim_counter: u32,
}

impl std::fmt::Debug for RecordRender {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RecordRender").finish_non_exhaustive()
    }
}

impl RecordRender {
    fn set_fast_forward(&mut self, enabled: bool) {
        self.ff_skip = enabled;
        if !enabled {
            self.ff_counter = 0;
        }
    }

    fn set_dual_player(&mut self, dual: bool) {
        self.dual_player = dual;
    }

    fn set_overlay_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    fn set_input(&mut self, port0: &GamepadInput, port1: &GamepadInput) {
        self.p1.set_input(port0);
        self.p2.set_input(port1);
    }
}

impl Render for RecordRender {
    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        self.buffer.set_pixel(x, y, color);
    }

    fn finish(&mut self) {
        // Decide whether this frame reaches the recording before building any
        // recording frame, so a trimmed or fast-forwarded frame is never
        // upscaled. The window still presents every frame.
        let send_video = if self.trim_counter < self.trim_frames {
            self.trim_counter += 1;
            false
        } else if self.ff_skip {
            let send = self.ff_counter.is_multiple_of(4);
            self.ff_counter += 1;
            send
        } else {
            true
        };

        // Advance the hold counters and derive the icon placements once per
        // frame. The window draws them as textures; recording composites them
        // into its own 4x scratch frame so the display frame stays clean.
        let overlays = if self.enabled {
            project_overlay_icons(&mut self.p1, &mut self.p2, self.dual_player)
        } else {
            Vec::new()
        };

        if let Some(ref mut canvas) = self.canvas {
            present_frame(canvas, &mut self.presenter, &self.buffer, &overlays);
        }

        if let Some(ref tx) = self.video_tx
            && send_video
        {
            upscale_into(self.buffer.as_bytes(), &mut self.record_scratch);
            if self.enabled {
                composite_overlay_icons(&mut self.record_scratch, &overlays, &self.icons);
            }
            let _ = tx.send(self.record_scratch.clone());
        }
    }
}

struct RecordAudioDriver {
    inner: SdlAudioDriver,
    audio_tx: Option<mpsc::Sender<Vec<u8>>>,
    sample_buf: Vec<f32>,
    ff_skip: Rc<Cell<bool>>,
    ff_counter: u32,
    trim_frames: u32,
    trim_counter: u32,
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
        let send_audio = if self.trim_counter < self.trim_frames {
            self.trim_counter += 1;
            false
        } else if self.ff_skip.get() {
            let send = self.ff_counter.is_multiple_of(4);
            self.ff_counter += 1;
            send
        } else {
            self.ff_counter = 0;
            true
        };
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
    temp_dir: PathBuf,
}

impl Drop for FfmpegCleanup {
    fn drop(&mut self) {
        if let Some(ref mut child) = self.ffmpeg {
            let _ = child.wait();
        }
        if self.temp_dir.as_os_str().is_empty() {
            return;
        }
        let _ = fs::remove_dir_all(&self.temp_dir);
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

/// Decode the gamepad sprite sheet into the fixed overlay icon table (17
/// square crops: 0 = A, 1 = B, 2..=16 = the held-direction combinations).
fn load_overlay_icons() -> Vec<OverlayIcon> {
    let sprite_sheet = load_from_memory(include_bytes!("../../design/gamepad_sprite.png"))
        .expect("failed to decode gamepad sprite")
        .into_rgba8();
    assert_eq!(sprite_sheet.width(), 17 * OVERLAY_SHEET_ICON_SIZE);
    assert_eq!(sprite_sheet.height(), OVERLAY_SHEET_ICON_SIZE);
    (0..17)
        .map(|i| {
            let icon = imageops::crop_imm(
                &sprite_sheet,
                i * OVERLAY_SHEET_ICON_SIZE,
                0,
                OVERLAY_SHEET_ICON_SIZE,
                OVERLAY_SHEET_ICON_SIZE,
            )
            .to_image();
            OverlayIcon {
                bytes: icon.into_raw(),
                width: OVERLAY_SHEET_ICON_SIZE,
                height: OVERLAY_SHEET_ICON_SIZE,
            }
        })
        .collect()
}

fn init_sdl_drivers(
    sdl_context: &Sdl,
    headless: bool,
    video_tx: Option<mpsc::Sender<Vec<u8>>>,
    audio_tx: Option<mpsc::Sender<Vec<u8>>>,
    ff_skip: Rc<Cell<bool>>,
    trim_frames: u32,
) -> Result<(RecordRender, RecordAudioDriver)> {
    let audio_subsystem = sdl_context.audio().map_err(|e| anyhow::anyhow!(e))?;

    let audio_spec = AudioSpecDesired {
        freq: Some(AUDIO_SAMPLE_RATE),
        channels: Some(1),
        samples: Some(AUDIO_CHUNK_SAMPLES as u16),
    };
    let audio_queue = audio_subsystem
        .open_queue::<f32, _>(None, &audio_spec)
        .map_err(|e| anyhow::anyhow!(e))?;
    audio_queue.resume();
    let sdl_audio = SdlAudioDriver::new(audio_queue, AUDIO_SAMPLE_RATE as u32);

    let canvas = if !headless {
        let video_subsystem = sdl_context.video().map_err(|e| anyhow::anyhow!(e))?;
        let window = video_subsystem
            .window("NES Simulator - Movie Playback", 1024, 960)
            .position_centered()
            .build()
            .map_err(|e| anyhow::anyhow!(e))?;
        Some(
            window
                .into_canvas()
                .build()
                .map_err(|e| anyhow::anyhow!(e))?,
        )
    } else {
        None
    };

    // One shared icon table: the presenter draws its texture sources while
    // the recording composite reads the same pixels, so no clone is needed.
    let icons: Rc<[OverlayIcon]> = Rc::from(load_overlay_icons());
    let mut presenter = FramePresenter::new();
    presenter.set_overlay_icons(icons.clone());

    let render = RecordRender {
        buffer: ImageRender::<1>::new(),
        record_scratch: vec![0u8; RECORD_WIDTH * RECORD_HEIGHT * 4],
        canvas,
        presenter,
        video_tx,
        icons,
        dual_player: false,
        enabled: false,
        p1: GamepadOverlay::new(),
        p2: GamepadOverlay::new(),
        ff_skip: false,
        ff_counter: 0,
        trim_frames,
        trim_counter: 0,
    };

    let audio_driver = RecordAudioDriver {
        inner: sdl_audio,
        audio_tx,
        sample_buf: Vec::new(),
        ff_skip,
        ff_counter: 0,
        trim_frames,
        trim_counter: 0,
    };

    Ok((render, audio_driver))
}

impl PlayMovieAction {
    pub fn run(&self, f: &INesFile, palette: Option<ColorTheme>) -> Result<()> {
        let input_logs: Box<[movie::InputLog]> = if movie_file_is_bk2(&self.movie_file) {
            let bk2_data = fs::read(&self.movie_file)?;
            let movie = nes_core::bk2::parse(&bk2_data).context("parse bk2 file")?;
            let warnings = nes_core::bk2::check_movie(&movie, f, self.play_from_power_on)?;
            for warning in warnings {
                eprintln!("{warning}");
            }
            eprintln!(">> Loaded BK2 movie with {} frames", movie.input_logs.len());
            movie.input_logs
        } else {
            let fm2_data = fs::read(&self.movie_file)?;
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
                temp_dir,
            };

            (Some(video_tx), Some(audio_tx), cleanup)
        } else {
            (
                None,
                None,
                FfmpegCleanup {
                    ffmpeg: None,
                    temp_dir: PathBuf::new(),
                },
            )
        };

        let ff_skip = Rc::new(Cell::new(false));

        let sdl_context = nes_sdl::sdl2::init().map_err(|e| anyhow::anyhow!(e))?;
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
        let target_frame_duration = Duration::new(0, 1_000_000_000u32 / 60);
        let mut extra_frames_remaining = self.extra_frames;
        let mut frame_index = 0;
        let fast_forward_ranges = if self.auto_fast_forward {
            compute_fast_forward_ranges(&input_logs, self.idle_frames as usize)
        } else {
            Vec::new()
        };

        'running: loop {
            let frame_start = Instant::now();
            if let Some(ref mut pump) = event_pump {
                for event in pump.poll_iter() {
                    if let Event::Quit { .. } = event {
                        break 'running;
                    }
                }
            }

            if let Some(input_log) = input_logs.get(frame_index) {
                // Soft-reset (FM2 reset flag / BK2 Reset column) and
                // fresh-machine restart (BK2 Power column) both return the
                // machine to its power-up contract before the frame runs.
                if input_log.command.soft_reset() || input_log.command.hard_reset() {
                    machine.reset();
                }
                set_controller_a_from_gamepad(&mut machine, &input_log.port0);
                set_controller_b_from_gamepad(&mut machine, &input_log.port1);
                machine
                    .render_mut()
                    .set_input(&input_log.port0, &input_log.port1);
            }

            if self.auto_fast_forward {
                let is_idle = if frame_index >= input_logs.len() {
                    true
                } else {
                    fast_forward_ranges
                        .iter()
                        .any(|&(start, end)| frame_index >= start && frame_index < end)
                };
                ff_skip.set(is_idle);
                machine.render_mut().set_fast_forward(is_idle);
            }

            match machine.process_frame() {
                nes_core::ExecuteResult::Continue => {}
                nes_core::ExecuteResult::ShouldReset => {
                    eprintln!("CPU reset requested");
                    machine.reset();
                }
                nes_core::ExecuteResult::Stop(code) => {
                    eprintln!("Execution stopped with code {}", code);
                    break 'running;
                }
                nes_core::ExecuteResult::Halt => {
                    eprintln!("CPU halt, possible because of an invalid instruction");
                    break 'running;
                }
            }

            if self.trim_after > 0 && machine.frame_no() >= self.trim_after as usize {
                eprintln!(">> Trim after {} frames", self.trim_after);
                break 'running;
            }

            if frame_index >= input_logs.len() {
                if extra_frames_remaining == 0 {
                    eprintln!(
                        ">> End of movie reached (after {} extra frames)",
                        self.extra_frames
                    );
                    break 'running;
                }
                extra_frames_remaining -= 1;
            }

            if !self.headless && !self.no_throttle && self.output.is_none() {
                let elapsed = frame_start.elapsed();
                if elapsed < target_frame_duration {
                    std::thread::sleep(target_frame_duration - elapsed);
                }
            }

            frame_index += 1;
        }

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
    use nes_core::render::{ImageRender, Render};
    use nes_sdl::presentation::{OverlayIcon, OverlayPlacement, plan};
    use std::rc::Rc;

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

    #[test]
    fn bk2_inputs_drive_the_shared_overlay_seam() {
        // Overlay parity (#103): BK2-parsed inputs project player-1 icons
        // left and player-2 icons right through the unchanged overlay path.
        // Frame 2 of the inputs fixture is P1 Up+A with P2 Left+Start.
        let data = include_bytes!("../../../testdata/bk2/bk2-inputs-5f.bk2");
        let movie = nes_core::bk2::parse(data).expect("inputs BK2 must parse");
        let mut p1 = GamepadOverlay::new();
        let mut p2 = GamepadOverlay::new();
        p1.set_input(&movie.input_logs[2].port0);
        p2.set_input(&movie.input_logs[2].port1);
        let icons = project_overlay_icons(&mut p1, &mut p2, true);
        // P1 Up projects direction icon 5 on the left; P1 A projects icon 0
        // beside it; P2 Left projects direction icon 3 on the right.
        assert!(
            icons.iter().any(|p| p.icon == 5 && p.dst.0 == 2),
            "{icons:?}"
        );
        assert!(
            icons.iter().any(|p| p.icon == 0 && p.dst.0 == 38),
            "{icons:?}"
        );
        assert!(
            icons.iter().any(|p| p.icon == 3 && p.dst.0 == 205),
            "{icons:?}"
        );
    }

    use super::{
        FramePresenter, GamepadOverlay, NES_WIDTH, OVERLAY_LAYOUT_RIGHT, RECORD_HEIGHT,
        RECORD_WIDTH, composite_overlay_icons, compute_fast_forward_ranges, load_overlay_icons,
        project_overlay_icons, upscale_into,
    };

    fn idle() -> InputLog {
        InputLog::new(
            Command::from(0),
            GamepadInput::from(0),
            GamepadInput::from(0),
        )
    }

    fn active_port0() -> InputLog {
        InputLog::new(
            Command::from(0),
            GamepadInput::from(1),
            GamepadInput::from(0),
        )
    }

    fn active_port1() -> InputLog {
        InputLog::new(
            Command::from(0),
            GamepadInput::from(0),
            GamepadInput::from(1),
        )
    }

    /// Serializes the SDL tests. The sdl2 crate allows `init` on only one
    /// thread per process lifetime, so two tests must not initialise SDL at
    /// the same time or the loser silently skips.
    static SDL_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

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
            Command::from(0),
            GamepadInput::from(0b00001000),
            GamepadInput::from(0),
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
            Command::from(0),
            GamepadInput::from(0),
            GamepadInput::from(0b00000010),
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
            Command::from(0),
            GamepadInput::from(0),
            GamepadInput::from(0b00000001),
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

    // --- gamepad overlay placement projection ------------------------------

    const PAD_A: u8 = 1 << 0;
    const PAD_B: u8 = 1 << 1;
    const PAD_UP: u8 = 1 << 4;
    const PAD_DOWN: u8 = 1 << 5;
    const PAD_LEFT: u8 = 1 << 6;
    const PAD_RIGHT: u8 = 1 << 7;

    fn pad(bits: u8) -> GamepadInput {
        GamepadInput::from(bits)
    }

    /// The sprite sheet's fixed icon ids: 0 = A, 1 = B, 2..=16 = the fifteen
    /// held-direction combinations. This is artwork, not a restatement of the
    /// implementation.
    const DIRECTION_ICON_TABLE: [(bool, bool, bool, bool, Option<usize>); 16] = [
        (false, false, false, false, None),
        (false, false, false, true, Some(4)),
        (false, false, true, false, Some(3)),
        (false, false, true, true, Some(6)),
        (false, true, false, false, Some(2)),
        (false, true, false, true, Some(9)),
        (false, true, true, false, Some(7)),
        (false, true, true, true, Some(12)),
        (true, false, false, false, Some(5)),
        (true, false, false, true, Some(10)),
        (true, false, true, false, Some(8)),
        (true, false, true, true, Some(14)),
        (true, true, false, false, Some(11)),
        (true, true, false, true, Some(15)),
        (true, true, true, false, Some(13)),
        (true, true, true, true, Some(16)),
    ];

    #[test]
    fn direction_icon_table_covers_all_sixteen_combinations_plus_none() {
        for (up, down, left, right, expected) in DIRECTION_ICON_TABLE {
            assert_eq!(
                GamepadOverlay::direction_icon_index(up, down, left, right),
                expected,
                "up={up} down={down} left={left} right={right}"
            );
        }
    }

    #[test]
    fn no_held_button_projects_nothing() {
        let mut p = GamepadOverlay::new();
        p.set_input(&pad(0));
        assert!(p.project(OVERLAY_LAYOUT_RIGHT).is_empty());
    }

    #[test]
    fn every_direction_combination_projects_its_table_icon() {
        for (up, down, left, right, expected) in DIRECTION_ICON_TABLE {
            let Some(expected) = expected else {
                continue;
            };
            let mut bits = 0u8;
            if up {
                bits |= PAD_UP;
            }
            if down {
                bits |= PAD_DOWN;
            }
            if left {
                bits |= PAD_LEFT;
            }
            if right {
                bits |= PAD_RIGHT;
            }
            let mut p = GamepadOverlay::new();
            p.set_input(&pad(bits));
            assert_eq!(
                p.project(OVERLAY_LAYOUT_RIGHT),
                vec![OverlayPlacement {
                    icon: expected,
                    dst: (205, 222, 16, 16),
                }],
                "up={up} down={down} left={left} right={right}"
            );
        }
    }

    #[test]
    fn direction_icon_holds_for_eighteen_frames_then_disappears() {
        let mut p = GamepadOverlay::new();
        p.set_input(&pad(PAD_UP));
        for frame in 0..18 {
            let icons = p.project(OVERLAY_LAYOUT_RIGHT);
            assert_eq!(icons.len(), 1, "icon still shown on frame {frame}");
            assert_eq!(icons[0].icon, 5);
        }
        assert!(
            p.project(OVERLAY_LAYOUT_RIGHT).is_empty(),
            "icon disappears on the 19th frame"
        );
    }

    #[test]
    fn a_and_b_icons_project_onto_their_slots() {
        let mut p = GamepadOverlay::new();
        p.set_input(&pad(PAD_A | PAD_B));
        assert_eq!(
            p.project(OVERLAY_LAYOUT_RIGHT),
            vec![
                OverlayPlacement {
                    icon: 0,
                    dst: (238, 222, 16, 16),
                },
                OverlayPlacement {
                    icon: 1,
                    dst: (222, 222, 16, 16),
                },
            ]
        );
    }

    #[test]
    fn a_alone_projects_the_a_icon_only() {
        let mut p = GamepadOverlay::new();
        p.set_input(&pad(PAD_A));
        assert_eq!(
            p.project(OVERLAY_LAYOUT_RIGHT),
            vec![OverlayPlacement {
                icon: 0,
                dst: (238, 222, 16, 16),
            }]
        );
    }

    #[test]
    fn b_alone_projects_the_b_icon_only() {
        let mut p = GamepadOverlay::new();
        p.set_input(&pad(PAD_B));
        assert_eq!(
            p.project(OVERLAY_LAYOUT_RIGHT),
            vec![OverlayPlacement {
                icon: 1,
                dst: (222, 222, 16, 16),
            }]
        );
    }

    #[test]
    fn ab_icons_hold_for_eighteen_frames_then_disappear() {
        let mut p = GamepadOverlay::new();
        p.set_input(&pad(PAD_A));
        for frame in 0..18 {
            assert_eq!(
                p.project(OVERLAY_LAYOUT_RIGHT).len(),
                1,
                "icon still shown on frame {frame}"
            );
        }
        assert!(p.project(OVERLAY_LAYOUT_RIGHT).is_empty());
    }

    #[test]
    fn single_player_projects_into_the_right_hand_layout() {
        let mut p1 = GamepadOverlay::new();
        p1.set_input(&pad(PAD_A));
        let icons = project_overlay_icons(&mut p1, &mut GamepadOverlay::new(), false);
        assert_eq!(
            icons,
            vec![OverlayPlacement {
                icon: 0,
                dst: (238, 222, 16, 16),
            }]
        );
    }

    #[test]
    fn dual_player_projects_player_one_left_and_player_two_right() {
        let mut p1 = GamepadOverlay::new();
        p1.set_input(&pad(PAD_A));
        let mut p2 = GamepadOverlay::new();
        p2.set_input(&pad(PAD_B));
        let icons = project_overlay_icons(&mut p1, &mut p2, true);
        assert_eq!(
            icons,
            vec![
                OverlayPlacement {
                    icon: 0,
                    dst: (38, 222, 16, 16),
                },
                OverlayPlacement {
                    icon: 1,
                    dst: (222, 222, 16, 16),
                },
            ]
        );
    }

    #[test]
    fn dual_player_with_no_input_projects_nothing() {
        let mut p1 = GamepadOverlay::new();
        let mut p2 = GamepadOverlay::new();
        assert!(project_overlay_icons(&mut p1, &mut p2, true).is_empty());
    }

    // --- recording frame construction --------------------------------------

    /// One logical 1x frame and the equivalent 4x frame, with a distinct
    /// colour per pixel so a mis-mapped block is caught.
    fn one_x_and_four_x_frames() -> (ImageRender<1>, ImageRender<4>) {
        let mut one = ImageRender::<1>::new();
        let mut four = ImageRender::<4>::new();
        for y in 0..240u32 {
            for x in 0..256u32 {
                let color = [
                    (x as u8).wrapping_mul(3),
                    (y as u8).wrapping_mul(5),
                    (x as u8) ^ (y as u8),
                    255,
                ];
                one.set_pixel(x, y, color);
                four.set_pixel(x, y, color);
            }
        }
        (one, four)
    }

    #[test]
    fn upscaling_a_one_x_frame_reproduces_the_four_x_frame() {
        let (one, four) = one_x_and_four_x_frames();
        let mut scratch = vec![0u8; 1024 * 960 * 4];
        upscale_into(one.as_bytes(), &mut scratch);
        assert_eq!(scratch.as_slice(), four.as_bytes());
    }

    #[test]
    fn compositing_over_the_upscaled_frame_matches_the_four_x_frame() {
        let (one, four) = one_x_and_four_x_frames();
        let icons = vec![OverlayIcon {
            bytes: {
                let mut b = vec![0u8; 4 * 4 * 4];
                for (i, px) in b.chunks_mut(4).enumerate() {
                    px.copy_from_slice(&[200, 100u8.wrapping_add(i as u8), 50, 200]);
                }
                b
            },
            width: 4,
            height: 4,
        }];
        let placements = [OverlayPlacement {
            icon: 0,
            dst: (2, 3, 16, 16),
        }];

        let mut scratch = vec![0u8; 1024 * 960 * 4];
        upscale_into(one.as_bytes(), &mut scratch);
        composite_overlay_icons(&mut scratch, &placements, &icons);

        let mut four_bytes = four.as_bytes().to_vec();
        let plain = four_bytes.clone();
        composite_overlay_icons(&mut four_bytes, &placements, &icons);
        assert_ne!(four_bytes, plain, "the composite must change the frame");
        assert_eq!(scratch, four_bytes);
    }

    // --- SDL overlay blend parity with the legacy integer blend -----------

    /// Renders a frame plus overlay icons through FramePresenter on a
    /// software renderer, reads the window back, and compares it with the
    /// legacy host-side composite (composite_overlay_icons). Zero delta is
    /// required outside the icon rectangles; anti-aliased icon edges are
    /// allowed at most 1 LSB per channel because SDL's BlendMode::Blend
    /// rounds while the legacy blend truncates.
    ///
    /// Self-skips when SDL cannot initialise a window and software renderer,
    /// so it is safe on a headless CI host.
    #[test]
    fn sdl_overlay_blend_matches_legacy_integer_blend() {
        use nes_sdl::sdl2::pixels::PixelFormatEnum;

        let _sdl_guard = SDL_TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        let Ok(sdl) = nes_sdl::sdl2::init() else {
            eprintln!("skip: SDL init failed");
            return;
        };
        let Ok(video) = sdl.video() else {
            eprintln!("skip: SDL video subsystem unavailable");
            return;
        };
        let dst_size = (RECORD_WIDTH as u32, RECORD_HEIGHT as u32);
        let Ok(window) = video
            .window("overlay-blend-test", dst_size.0, dst_size.1)
            .hidden()
            .build()
        else {
            eprintln!("skip: SDL window unavailable");
            return;
        };
        let Ok(mut canvas) = window.into_canvas().software().build() else {
            eprintln!("skip: SDL software renderer unavailable");
            return;
        };

        // A frame with a distinct colour per logical pixel, so any blend or
        // upscale mistake changes the comparison.
        let mut frame = ImageRender::<1>::new();
        for y in 0..240u32 {
            for x in 0..256u32 {
                frame.set_pixel(
                    x,
                    y,
                    [
                        (x.wrapping_mul(3)) as u8,
                        (y.wrapping_mul(5)) as u8,
                        (x ^ y) as u8,
                        255,
                    ],
                );
            }
        }

        // The real gamepad icons: their anti-aliased edges are exactly where
        // a rounded SDL blend could diverge from the truncating legacy blend.
        let icons: Rc<[OverlayIcon]> = Rc::from(load_overlay_icons());
        let placements = [
            OverlayPlacement {
                icon: 0,
                dst: (2, 3, 16, 16),
            },
            OverlayPlacement {
                icon: 5,
                dst: (200, 206, 16, 16),
            },
            OverlayPlacement {
                icon: 16,
                dst: (120, 100, 16, 16),
            },
        ];

        // SDL path: plan + presenter (software renderer, then read back).
        let presentation = plan(&frame, &placements, dst_size);
        let mut presenter = FramePresenter::new();
        presenter.set_overlay_icons(icons.clone());
        presenter.present(&mut canvas, &presentation);
        let sdl_pixels = canvas
            .read_pixels(None, PixelFormatEnum::ABGR8888)
            .expect("software renderer read-back");

        // Legacy path: the same 4x upscale plus the host integer blend.
        let mut expected = vec![0u8; RECORD_WIDTH * RECORD_HEIGHT * 4];
        upscale_into(frame.as_bytes(), &mut expected);
        composite_overlay_icons(&mut expected, &placements, &icons);
        let mut plain = vec![0u8; RECORD_WIDTH * RECORD_HEIGHT * 4];
        upscale_into(frame.as_bytes(), &mut plain);
        assert_ne!(plain, expected, "the icon composite must change the frame");

        let scale = (RECORD_WIDTH / NES_WIDTH) as u32;
        let inside = |px: u32, py: u32| {
            placements.iter().any(|p| {
                let x0 = p.dst.0 as u32 * scale;
                let y0 = p.dst.1 as u32 * scale;
                let x1 = x0 + p.dst.2 * scale;
                let y1 = y0 + p.dst.3 * scale;
                px >= x0 && px < x1 && py >= y0 && py < y1
            })
        };

        let mut max_inside = 0u8;
        let mut max_outside = 0u8;
        for y in 0..RECORD_HEIGHT as u32 {
            for x in 0..RECORD_WIDTH as u32 {
                let i = ((y * RECORD_WIDTH as u32 + x) * 4) as usize;
                for c in 0..4 {
                    let delta = sdl_pixels[i + c].abs_diff(expected[i + c]);
                    if inside(x, y) {
                        max_inside = max_inside.max(delta);
                    } else {
                        max_outside = max_outside.max(delta);
                    }
                }
            }
        }
        eprintln!(
            "SDL overlay blend delta: max inside icon pixels = {max_inside} LSB, max outside = {max_outside} LSB"
        );
        assert_eq!(max_outside, 0, "pixels outside the icons must be identical");
        assert!(
            max_inside <= 1,
            "icon pixels may differ by at most 1 LSB, got {max_inside}"
        );
    }

    /// `run` keeps SDL's default texture scale mode; the spec only needs
    /// nearest for playback. SDL documents Nearest as the default and this
    /// pins it: if a future SDL changed the default to linear, forcing
    /// nearest would start changing the `run` 2x picture.
    #[test]
    fn sdl_default_texture_scale_mode_is_nearest() {
        use nes_sdl::sdl2::pixels::PixelFormatEnum;
        use nes_sdl::sdl2::render::ScaleMode;

        let _sdl_guard = SDL_TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        let Ok(sdl) = nes_sdl::sdl2::init() else {
            eprintln!("skip: SDL init failed");
            return;
        };
        let Ok(video) = sdl.video() else {
            eprintln!("skip: SDL video subsystem unavailable");
            return;
        };
        let Ok(window) = video.window("scale-mode-test", 256, 240).hidden().build() else {
            eprintln!("skip: SDL window unavailable");
            return;
        };
        let Ok(canvas) = window.into_canvas().software().build() else {
            eprintln!("skip: SDL software renderer unavailable");
            return;
        };
        let texture = canvas
            .texture_creator()
            .create_texture_streaming(PixelFormatEnum::ABGR8888, 256, 240)
            .expect("create texture");
        assert!(
            matches!(texture.scale_mode(), ScaleMode::Nearest),
            "SDL default scale mode must be Nearest for the run path to be unchanged"
        );
    }
}
