use anyhow::Result;
use image::EncodableLayout;
use nes_core::EmptyPlugin;
use nes_core::ines::INesFile;
use nes_core::nes::apu::AudioDriver;
use nes_core::nes::controller::Button;
use nes_core::nes_machine::NesMachine;
use nes_core::render::{ImageRender, Render};
use sdl2::audio::{AudioQueue, AudioSpecDesired};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::Canvas;
use sdl2::video::Window;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

const AUDIO_SAMPLE_RATE: i32 = 44_100;
const AUDIO_CHUNK_SAMPLES: usize = 1024;
const AUDIO_MAX_QUEUED_BYTES: u32 = (AUDIO_SAMPLE_RATE as u32) * 4 / 2;

fn map_keycode_to_button(keycode: Keycode) -> Option<Button> {
    match keycode {
        Keycode::Z => Some(Button::A),
        Keycode::X => Some(Button::B),
        Keycode::Space => Some(Button::Select),
        Keycode::Return => Some(Button::Start),
        Keycode::Up => Some(Button::Up),
        Keycode::Down => Some(Button::Down),
        Keycode::Left => Some(Button::Left),
        Keycode::Right => Some(Button::Right),
        _ => None,
    }
}

struct SdlAudioDriver {
    queue: AudioQueue<f32>,
    buffer: Vec<f32>,
    sample_rate: u32,
}

struct SdlRender {
    image: Arc<Mutex<ImageRender>>,
    canvas: Canvas<Window>,
}

impl std::fmt::Debug for SdlRender {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SdlRender").finish_non_exhaustive()
    }
}

impl SdlRender {
    fn new(canvas: Canvas<Window>) -> Self {
        Self::with_image(
            Arc::new(Mutex::new(ImageRender::default_dimension())),
            canvas,
        )
    }

    fn with_image(image: Arc<Mutex<ImageRender>>, canvas: Canvas<Window>) -> Self {
        Self { image, canvas }
    }
}

impl Render for SdlRender {
    fn clear(&mut self, color: [u8; 4]) {
        if let Ok(mut img) = self.image.lock() {
            img.clear(color);
        }
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        if let Ok(mut img) = self.image.lock() {
            img.set_pixel(x, y, color);
        }
    }

    fn finish(&mut self) {
        // Lock image buffer and upload to SDL texture
        if let Ok(img) = self.image.lock() {
            let image = img.borrow_image();
            let (width, height) = image.dimensions();
            let texture_creator = self.canvas.texture_creator();
            let mut texture = texture_creator
                .create_texture_streaming(PixelFormatEnum::ABGR8888, width, height)
                .expect("failed to create SDL texture");

            texture
                .update(None, image.as_bytes(), (width * 4) as usize)
                .expect("failed to upload SDL texture");
            self.canvas
                .copy(&texture, None, None)
                .expect("failed to copy SDL texture");
            self.canvas.present();
        }
    }
}

impl SdlAudioDriver {
    fn new(queue: AudioQueue<f32>, sample_rate: u32) -> Self {
        Self {
            queue,
            buffer: Vec::with_capacity(AUDIO_CHUNK_SAMPLES),
            sample_rate,
        }
    }

    fn try_flush(&mut self) {
        if self.buffer.is_empty() {
            return;
        }

        if self.queue.size() >= AUDIO_MAX_QUEUED_BYTES {
            self.buffer.clear();
            return;
        }

        self.queue
            .queue_audio(&self.buffer)
            .expect("failed to queue SDL audio samples");
        self.buffer.clear();
    }
}

impl AudioDriver for SdlAudioDriver {
    fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    fn push_sample(&mut self, sample: f32) {
        self.buffer.push(sample);
        if self.buffer.len() >= AUDIO_CHUNK_SAMPLES {
            self.try_flush();
        }
    }

    fn flush(&mut self) {
        self.try_flush();
    }
}

#[derive(clap::Args)]
pub struct RunAction {}

impl RunAction {
    pub fn run(&self, f: &INesFile) -> Result<()> {
        // Initialize SDL2
        let sdl_context = sdl2::init().map_err(|e| anyhow::anyhow!(e))?;
        let video_subsystem = sdl_context.video().map_err(|e| anyhow::anyhow!(e))?;
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
        let audio_driver = Box::new(SdlAudioDriver::new(audio_queue, AUDIO_SAMPLE_RATE as u32));

        // Create window (2x scale for better visibility: 256x240 -> 512x480)
        let window = video_subsystem
            .window(
                "NES Simulator - Arrows move, Z/X A/B, Space Select, Enter Start",
                512,
                480,
            )
            .position_centered()
            .build()
            .map_err(|e| anyhow::anyhow!(e))?;

        // Create canvas/renderer
        let canvas = window
            .into_canvas()
            .build()
            .map_err(|e| anyhow::anyhow!(e))?;

        // Create shared image buffer so we can save screenshots from the event loop
        let image_arc = Arc::new(Mutex::new(ImageRender::default_dimension()));
        let sdl_render = SdlRender::with_image(image_arc.clone(), canvas);

        // Create NES machine with SDL-backed renderer
        let mut machine = NesMachine::new(f, EmptyPlugin::new(), sdl_render, audio_driver);

        // Initialize event pump
        let mut event_pump = sdl_context.event_pump().map_err(|e| anyhow::anyhow!(e))?;
        let target_frame_duration = Duration::new(0, 1_000_000_000u32 / 60);

        'running: loop {
            let frame_start = Instant::now();
            // Handle events
            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        repeat: false,
                        ..
                    } => {
                        break 'running;
                    }
                    // F1 markdown dump removed
                    Event::KeyDown {
                        keycode: Some(Keycode::F2),
                        repeat: false,
                        ..
                    } => {
                        // Reset the simulator
                        eprintln!(">> Resetting simulator");
                        machine.reset();
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::F1),
                        repeat: false,
                        ..
                    } => match machine.dump_ppu_state_to_file() {
                        Ok(path) => eprintln!(">> Dumped PPU state to {}", path),
                        Err(err) => eprintln!(">> Failed to dump PPU state: {}", err),
                    },
                    Event::KeyDown {
                        keycode: Some(Keycode::F4),
                        repeat: false,
                        ..
                    } => {
                        // Quit the application
                        eprintln!(">> Quitting application");
                        break 'running;
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::F12),
                        repeat: false,
                        ..
                    } => {
                        // Save current frame image with frame number in filename
                        let frame_no = machine.mcu().ppu().frame_no();
                        let path = format!("/tmp/nes-frame-{}.png", frame_no);
                        match image_arc.lock() {
                            Ok(img) => {
                                if let Err(e) = img.borrow_image().save(&path) {
                                    eprintln!(">> Failed to save frame to {}: {}", path, e);
                                } else {
                                    eprintln!(">> Saved frame to {}", path);
                                }
                            }
                            Err(_) => eprintln!(">> Failed to lock image buffer for saving"),
                        }
                    }
                    Event::KeyDown {
                        keycode: Some(keycode),
                        repeat: false,
                        ..
                    } => {
                        if let Some(button) = map_keycode_to_button(keycode) {
                            machine.press_button(button);
                        }
                    }
                    Event::KeyUp {
                        keycode: Some(keycode),
                        repeat: false,
                        ..
                    } => {
                        if let Some(button) = map_keycode_to_button(keycode) {
                            machine.release_button(button);
                        }
                    }
                    _ => {}
                }
            }

            // Run one frame; the renderer presents from `finish()`.
            let result = machine.process_frame();
            machine.flush_audio();

            // Check if execution should stop
            match result {
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
                    eprintln!("Cpu halt, possible because of an invalid instruction");
                    break 'running;
                }
            }

            // Frame rate limiting: sleep only for the remaining time to maintain 60 FPS
            let elapsed = frame_start.elapsed();
            if elapsed < target_frame_duration {
                std::thread::sleep(target_frame_duration - elapsed);
            }
        }

        Ok(())
    }
}
