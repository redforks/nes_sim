pub mod drivers;
pub mod event_loop;
pub mod presentation;
pub mod presenter;
pub mod render;

pub use drivers::{INTERACTIVE_WINDOW, PLAYBACK_WINDOW, WindowSpec, create_sdl_drivers};
pub(crate) use event_loop::{FastForwardState, InputSource, LoopOptions, run_event_loop};
pub(crate) use render::NesRender;

use nes_core::nes::apu::AudioDriver;
use nes_core::nes::controller::Button;
use sdl2::audio::AudioQueue;
use sdl2::keyboard::Keycode;
use std::{cell::Cell, rc::Rc, sync::mpsc};

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

/// Unified audio driver: SDL playback always runs; recording into
/// `audio_tx` is gated by `Option` exactly like `NesRender::video_tx`.
/// Interactive mode passes `audio_tx = None`, movie mode passes its channel.
/// The fast-forward skip flag is shared with `FastForwardState` via `Rc`
/// because `NesMachine` exposes `render_mut()` but no `audio_mut()`.
pub struct NesAudioDriver {
    queue: AudioQueue<f32>,
    buffer: Vec<f32>,
    sample_rate: u32,
    audio_tx: Option<mpsc::Sender<Vec<u8>>>,
    sample_buf: Vec<f32>,
    ff_skip: Rc<Cell<bool>>,
    send_policy: crate::recording_send::RecordingSendPolicy,
}

impl NesAudioDriver {
    pub fn new(
        queue: AudioQueue<f32>,
        sample_rate: u32,
        audio_tx: Option<mpsc::Sender<Vec<u8>>>,
        ff_skip: Rc<Cell<bool>>,
        trim_frames: u32,
    ) -> Self {
        Self {
            queue,
            buffer: Vec::with_capacity(AUDIO_CHUNK_SAMPLES),
            sample_rate,
            audio_tx,
            sample_buf: Vec::new(),
            ff_skip,
            send_policy: crate::recording_send::RecordingSendPolicy::new(trim_frames),
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

impl AudioDriver for NesAudioDriver {
    fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    fn push_sample(&mut self, sample: f32) {
        self.buffer.push(sample);
        if self.buffer.len() >= AUDIO_CHUNK_SAMPLES {
            self.try_flush();
        }
        if self.audio_tx.is_some() {
            self.sample_buf.push(sample);
        }
    }

    fn flush(&mut self) {
        self.try_flush();
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
