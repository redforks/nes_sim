use anyhow::Result;
use image::EncodableLayout;
use nes_core::ines::INesFile;
use nes_core::nes::apu::AudioDriver;
use nes_core::nes::controller::Button;
use nes_core::nes_machine::NesMachine;
use nes_core::render::{CompositeRender, ImageRender, MarkdownRender, Render};
use sdl2::audio::{AudioQueue, AudioSpecDesired};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use std::cell::RefCell;
use std::rc::Rc;
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

/// Wrapper around Rc<RefCell<MarkdownRender>> that implements Render
#[derive(Debug)]
struct SharedMarkdownRender {
    inner: Rc<RefCell<MarkdownRender>>,
}

impl SharedMarkdownRender {
    fn new(inner: Rc<RefCell<MarkdownRender>>) -> Self {
        Self { inner }
    }
}

impl Render for SharedMarkdownRender {
    fn clear(&mut self, color: [u8; 4]) {
        self.inner.borrow_mut().clear(color);
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        self.inner.borrow_mut().set_pixel(x, y, color);
    }

    fn dimensions(&self) -> (u32, u32) {
        self.inner.borrow().dimensions()
    }

    fn finish(&mut self) {
        self.inner.borrow_mut().finish();
    }
}

/// Wrapper around Rc<RefCell<CompositeRender>> that implements Render
#[derive(Debug)]
struct SharedCompositeRender {
    inner: Rc<RefCell<CompositeRender>>,
}

impl SharedCompositeRender {
    fn new(inner: Rc<RefCell<CompositeRender>>) -> Self {
        Self { inner }
    }
}

impl Render for SharedCompositeRender {
    fn clear(&mut self, color: [u8; 4]) {
        self.inner.borrow_mut().clear(color);
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        self.inner.borrow_mut().set_pixel(x, y, color);
    }

    fn dimensions(&self) -> (u32, u32) {
        self.inner.borrow().dimensions()
    }

    fn finish(&mut self) {
        self.inner.borrow_mut().finish();
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
                "NES Simulator - Arrows move, Z/X A/B, Space Select, Enter Start, F1 dump",
                512,
                480,
            )
            .position_centered()
            .build()
            .map_err(|e| anyhow::anyhow!(e))?;

        // Create canvas/renderer
        let mut canvas = window
            .into_canvas()
            .build()
            .map_err(|e| anyhow::anyhow!(e))?;

        // Create texture for RGBA8888 pixel format
        let texture_creator = canvas.texture_creator();
        let mut texture = texture_creator
            .create_texture_streaming(PixelFormatEnum::ABGR8888, 256, 240)
            .map_err(|e| anyhow::anyhow!(e))?;

        // Create image renderer for display - keep a reference to access later
        let image_render = ImageRender::new(256, 240);

        // Create composite renderer with image + markdown
        let composite = Rc::new(RefCell::new(CompositeRender::new()));

        // Add image renderer (ImageRender is Clone and shares internal Rc)
        composite.borrow_mut().add(Box::new(image_render.clone()));

        // Create and wrap markdown renderer for shared access
        let markdown_render = Rc::new(RefCell::new(MarkdownRender::new(0, 256, 240)));
        composite
            .borrow_mut()
            .add(Box::new(SharedMarkdownRender::new(markdown_render.clone())));

        // Wrap composite for sharing
        let shared_composite = SharedCompositeRender::new(composite.clone());

        // Create NES machine with composite renderer
        let mut machine =
            NesMachine::with_renderer_and_audio(f, Some(Box::new(shared_composite)), audio_driver);

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
                    Event::KeyDown {
                        keycode: Some(Keycode::F1),
                        repeat: false,
                        ..
                    } => {
                        // Request markdown dump for next frame
                        markdown_render.borrow_mut().request_dump();
                        eprintln!(
                            ">> Markdown dump requested for next frame (frame {})",
                            markdown_render.borrow().frame_number()
                        );
                    }
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
                        keycode: Some(Keycode::F4),
                        repeat: false,
                        ..
                    } => {
                        // Quit the application
                        eprintln!(">> Quitting application");
                        break 'running;
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

            // Run one frame
            let result = machine.process_frame();
            machine.flush_audio();

            // Update texture with frame data
            texture.update(None, image_render.borrow_image().as_bytes(), 256 * 4)?;

            // Copy texture to canvas and present
            canvas
                .copy(&texture, None, None)
                .map_err(|e| anyhow::anyhow!(e))?;
            canvas.present();

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
