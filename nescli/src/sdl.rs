pub mod drivers;
pub mod event_loop;
pub mod presentation;
pub mod presenter;
pub mod render;

pub use drivers::{INTERACTIVE_WINDOW, PLAYBACK_WINDOW, WindowSpec, create_sdl_drivers};
pub(crate) use event_loop::{FastForwardState, InputSource, LoopOptions, run_event_loop};
pub(crate) use render::NesRender;

use nes_core::audio_chunk::ChunkedAudioPush;
use nes_core::nes::controller::Button;
use sdl2::audio::AudioQueue;
use sdl2::keyboard::Keycode;
use std::{cell::Cell, rc::Rc, sync::mpsc};

/// Re-export sdl2 so downstream crates can use its types without a direct dependency.
pub use sdl2;

pub const AUDIO_SAMPLE_RATE: i32 = 44_100;
/// Re-exported so queue sizing (`drivers.rs`) and thresholds name one cadence.
pub use nes_core::audio_chunk::AUDIO_CHUNK_SAMPLES;

#[cfg(test)]
mod tests {
    use super::*;

    /// SDL chunks at the shared desktop cadence: the chunk size the queue
    /// threshold derives from is the 1024-sample playback cadence, so a
    /// change here must be a deliberate cross-sink decision.
    #[test]
    fn sdl_chunks_at_the_shared_desktop_cadence() {
        assert_eq!(AUDIO_CHUNK_SAMPLES, 1024);
        assert_eq!(
            AUDIO_MAX_QUEUED_BYTES,
            (AUDIO_SAMPLE_RATE as u32) * 4 / 2,
            "queue-pressure threshold stays a half-second of mono f32 audio"
        );
    }
}
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
    chunker: ChunkedAudioPush,
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
            chunker: ChunkedAudioPush::new(),
            sample_rate,
            audio_tx,
            sample_buf: Vec::new(),
            ff_skip,
            send_policy: crate::recording_send::RecordingSendPolicy::new(trim_frames),
        }
    }
}

/// Queue one chunk unless the SDL queue is over the pressure threshold
/// (the old `try_flush` clear-on-full, applied at the shared chunk
/// boundary instead of a hand-rolled loop).
fn emit_chunk(queue: &AudioQueue<f32>, chunk: &[f32]) {
    if queue.size() >= AUDIO_MAX_QUEUED_BYTES {
        return;
    }
    queue
        .queue_audio(chunk)
        .expect("failed to queue SDL audio samples");
}

impl NesAudioDriver {
    pub fn push_samples(&mut self, samples: &[f32]) {
        let Self { chunker, queue, .. } = self;
        chunker.push(samples, |chunk| emit_chunk(queue, chunk));
        if self.audio_tx.is_some() {
            self.sample_buf.extend_from_slice(samples);
        }
    }

    pub fn flush_frame(&mut self) {
        let Self { chunker, queue, .. } = self;
        chunker.flush(|chunk| emit_chunk(queue, chunk));
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
