use anyhow::{Context as _, Result};
use image::{EncodableLayout, RgbaImage, imageops, load_from_memory};
use nes_core::{
    EmptyPlugin,
    ines::INesFile,
    movie::{self, GamepadInput},
    nes::{NesMcu, apu::AudioDriver, controller::Button},
    nes_machine::NesMachine,
    render::{ImageRender, Render},
};
use nes_sdl::{
    self, AUDIO_CHUNK_SAMPLES, AUDIO_SAMPLE_RATE, SdlAudioDriver,
    sdl2::{
        Sdl, audio::AudioSpecDesired, event::Event, pixels::PixelFormatEnum, render::Canvas,
        video::Window,
    },
};
use std::{
    fs,
    io::Write,
    path::PathBuf,
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
    fm2_file: PathBuf,
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[arg(long)]
    no_throttle: bool,
    #[arg(long, default_value_t = 600)]
    extra_frames: u32,
    #[arg(long)]
    headless: bool,
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
            a: false, b: false, up: false, down: false, left: false, right: false,
            disp_a: false, disp_b: false, disp_up: false, disp_down: false,
            disp_left: false, disp_right: false,
            hold_dir: 0, hold_ab: 0,
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

    fn draw(&mut self, image: &mut RgbaImage, icons: &[RgbaImage], dir_x: i64, ab_x: i64, b_x: i64, y: i64) {
        let draw_dir = self.hold_dir > 0;
        let draw_ab = self.hold_ab > 0;
        if draw_dir { self.hold_dir -= 1; }
        if draw_ab { self.hold_ab -= 1; }
        if !draw_dir && !draw_ab {
            return;
        }
        if let Some(direction_idx) = Self::direction_icon_index(
            draw_dir && self.disp_up,
            draw_dir && self.disp_down,
            draw_dir && self.disp_left,
            draw_dir && self.disp_right,
        ) {
            imageops::overlay(image, &icons[direction_idx], dir_x, y);
        }
        if draw_ab && self.disp_a {
            imageops::overlay(image, &icons[0], ab_x, y);
        }
        if draw_ab && self.disp_b {
            imageops::overlay(image, &icons[1], b_x, y);
        }
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

struct RecordRender {
    buffer: ImageRender,
    canvas: Option<Canvas<Window>>,
    video_tx: Option<mpsc::Sender<Vec<u8>>>,
    icons: Vec<RgbaImage>,
    dual_player: bool,
    p1: GamepadOverlay,
    p2: GamepadOverlay,
}

impl std::fmt::Debug for RecordRender {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RecordRender").finish_non_exhaustive()
    }
}

impl RecordRender {
    fn set_dual_player(&mut self, dual: bool) {
        self.dual_player = dual;
    }

    fn set_input(&mut self, port0: &GamepadInput, port1: &GamepadInput) {
        self.p1.set_input(port0);
        self.p2.set_input(port1);
    }

    fn overlay_input_icons(&mut self) {
        let image = self.buffer.borrow_image_mut();
        if self.dual_player {
            self.p1.draw(image, &self.icons, 8, 152, 80, 888);
            self.p2.draw(image, &self.icons, 820, 952, 888, 888);
        } else {
            self.p1.draw(image, &self.icons, 820, 952, 888, 888);
        }
    }
}

impl Render for RecordRender {
    fn clear(&mut self, color: [u8; 4]) {
        self.buffer.clear(color);
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        let bx = x * 4;
        let by = y * 4;
        for dy in 0..4 {
            for dx in 0..4 {
                self.buffer.set_pixel(bx + dx, by + dy, color);
            }
        }
    }

    fn finish(&mut self) {
        self.overlay_input_icons();

        let image = self.buffer.borrow_image();
        let (width, height) = image.dimensions();

        if let Some(ref mut canvas) = self.canvas {
            let texture_creator = canvas.texture_creator();
            let mut texture = texture_creator
                .create_texture_streaming(PixelFormatEnum::ABGR8888, width, height)
                .expect("failed to create SDL texture");
            texture
                .update(None, image.as_bytes(), (width * 4) as usize)
                .expect("failed to upload SDL texture");
            canvas
                .copy(&texture, None, None)
                .expect("failed to copy SDL texture");
            canvas.present();
        }

        if let Some(ref tx) = self.video_tx {
            let _ = tx.send(image.as_bytes().to_vec());
        }
    }
}

struct RecordAudioDriver {
    inner: SdlAudioDriver,
    audio_tx: Option<mpsc::Sender<Vec<u8>>>,
    sample_buf: Vec<f32>,
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
        if let Some(ref tx) = self.audio_tx {
            if !self.sample_buf.is_empty() {
                let bytes: Vec<u8> = self
                    .sample_buf
                    .iter()
                    .flat_map(|s| s.to_le_bytes())
                    .collect();
                let _ = tx.send(bytes);
                self.sample_buf.clear();
            }
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

fn init_sdl_drivers(
    sdl_context: &Sdl,
    headless: bool,
    video_tx: Option<mpsc::Sender<Vec<u8>>>,
    audio_tx: Option<mpsc::Sender<Vec<u8>>>,
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

    let sprite_bytes = include_bytes!("../../design/gamepad_sprite.png");
    let sprite_sheet = load_from_memory(sprite_bytes)
        .expect("failed to decode gamepad sprite")
        .into_rgba8();
    assert_eq!(sprite_sheet.width(), 1088);
    assert_eq!(sprite_sheet.height(), 64);
    let mut icons = Vec::with_capacity(17);
    for i in 0..17 {
        let icon = imageops::crop_imm(&sprite_sheet, i * 64, 0, 64, 64).to_image();
        icons.push(icon);
    }

    let render = RecordRender {
        buffer: ImageRender::new(1024, 960),
        canvas,
        video_tx,
        icons,
        dual_player: false,
        p1: GamepadOverlay::new(),
        p2: GamepadOverlay::new(),
    };

    let audio_driver = RecordAudioDriver {
        inner: sdl_audio,
        audio_tx,
        sample_buf: Vec::new(),
    };

    Ok((render, audio_driver))
}

impl PlayMovieAction {
    pub fn run(&self, f: &INesFile) -> Result<()> {
        let fm2_data = fs::read(&self.fm2_file)?;
        let fm2 = movie::parse(&fm2_data).context("parse fm2 file")?;
        let input_logs = &fm2.input_logs;

        if !fm2.valid_rom_file(f)? {
            eprintln!("Warning: rom file checksum not match");
        }

        if input_logs.is_empty() {
            anyhow::bail!("FM2 file has no input logs");
        }
        eprintln!(">> Loaded movie with {} frames", input_logs.len());

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
                    "18",
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

        let sdl_context = nes_sdl::sdl2::init().map_err(|e| anyhow::anyhow!(e))?;
        let (render, audio_driver) =
            init_sdl_drivers(&sdl_context, self.headless, video_tx, audio_tx)?;

        let mut machine = NesMachine::new(f, EmptyPlugin::new(), render, audio_driver);
        let dual_player = input_logs.iter().any(|log| !log.port1.is_empty());
        machine.render_mut().set_dual_player(dual_player);

        let mut event_pump = if !self.headless {
            Some(sdl_context.event_pump().map_err(|e| anyhow::anyhow!(e))?)
        } else {
            None
        };
        let target_frame_duration = Duration::new(0, 1_000_000_000u32 / 60);
        let mut frame_offset = 0;
        let mut extra_frames_remaining = self.extra_frames;

        'running: loop {
            let frame_start = Instant::now();
            if let Some(ref mut pump) = event_pump {
                for event in pump.poll_iter() {
                    match event {
                        Event::Quit { .. } => {
                            break 'running;
                        }
                        _ => {}
                    }
                }
            }

            let frame_index = machine.frame_no();
            if let Some(input_log) = input_logs.get(frame_index + frame_offset) {
                if input_log.command.soft_reset() {
                    frame_offset += 2;
                }
                assert!(
                    !input_log.command.hard_reset(),
                    "unsupported command: hard_reset"
                );
                set_controller_a_from_gamepad(&mut machine, &input_log.port0);
                set_controller_b_from_gamepad(&mut machine, &input_log.port1);
                machine.render_mut().set_input(&input_log.port0, &input_log.port1);
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
        }

        // Drop machine first to close channel senders, which lets pipe-writing
        // threads exit and close FIFOs so ffmpeg sees EOF and finishes.
        drop(machine);
        drop(cleanup);
        Ok(())
    }
}
