use image::EncodableLayout;
use nes_core::nes::apu::AudioDriver;
use nes_core::nes::controller::Button;
use nes_core::render::{ImageRender, Render};
use sdl2::audio::AudioQueue;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::Canvas;
use sdl2::video::Window;

/// Re-export sdl2 so downstream crates can use its types without a direct dependency.
pub use sdl2;

pub const AUDIO_SAMPLE_RATE: i32 = 44_100;
pub const AUDIO_CHUNK_SAMPLES: usize = 1024;
const AUDIO_MAX_QUEUED_BYTES: u32 = (AUDIO_SAMPLE_RATE as u32) * 4 / 2;

pub fn map_keycode_to_button(keycode: Keycode) -> Option<Button> {
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

pub struct SdlAudioDriver {
    queue: AudioQueue<f32>,
    buffer: Vec<f32>,
    sample_rate: u32,
}

impl SdlAudioDriver {
    pub fn new(queue: AudioQueue<f32>, sample_rate: u32) -> Self {
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

pub struct SdlRender {
    image: ImageRender,
    canvas: Canvas<Window>,
}

impl std::fmt::Debug for SdlRender {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SdlRender").finish_non_exhaustive()
    }
}

impl SdlRender {
    pub fn with_image(image: ImageRender, canvas: Canvas<Window>) -> Self {
        Self { image, canvas }
    }
}

impl Render for SdlRender {
    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        self.image.set_pixel(x, y, color);
    }

    fn finish(&mut self) {
        let image = self.image.borrow_image();
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
