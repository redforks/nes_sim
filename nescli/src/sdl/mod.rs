pub mod drivers;
pub mod presentation;
pub mod presenter;
pub mod render;

pub use drivers::{INTERACTIVE_WINDOW, PLAYBACK_WINDOW, create_sdl_drivers};
pub(crate) use render::NesRender;

use nes_core::nes::apu::AudioDriver;
use nes_core::nes::controller::Button;
use sdl2::audio::AudioQueue;
use sdl2::keyboard::Keycode;

/// Re-export sdl2 so downstream crates can use its types without a direct dependency.
pub use sdl2;

pub const AUDIO_SAMPLE_RATE: i32 = 44_100;
pub const AUDIO_CHUNK_SAMPLES: usize = 1024;
const AUDIO_MAX_QUEUED_BYTES: u32 = (AUDIO_SAMPLE_RATE as u32) * 4 / 2;

pub fn map_keycode_to_button(keycode: Keycode) -> Option<Button> {
    match keycode {
        Keycode::Z | Keycode::M => Some(Button::A),
        Keycode::X | Keycode::W => Some(Button::B),
        Keycode::Space => Some(Button::Select),
        Keycode::Return => Some(Button::Start),
        Keycode::Up | Keycode::E => Some(Button::Up),
        Keycode::Down | Keycode::J => Some(Button::Down),
        Keycode::Left | Keycode::Q => Some(Button::Left),
        Keycode::Right | Keycode::K => Some(Button::Right),
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
