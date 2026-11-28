pub mod drivers;
pub mod event_loop;
pub mod presentation;
pub mod presenter;
pub mod render;
pub mod terminal;

pub use drivers::{INTERACTIVE_WINDOW, PLAYBACK_WINDOW, WindowSpec, create_sdl_drivers};
pub use terminal::TerminalVideo;

/// CLI video sink. Single source for `--video-out`.
#[derive(clap::ValueEnum, Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum VideoOut {
    /// SDL2 window (current behavior).
    #[default]
    Sdl,
    /// Sixel escape sequences on stdout.
    Sixel,
    /// Kitty graphics protocol on stdout.
    Kitty,
}

/// CLI audio sink. Single source for `--audio-out`.
#[derive(clap::ValueEnum, Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum AudioOut {
    /// SDL2 audio queue (current behavior).
    #[default]
    Sdl,
    /// No live audio. `--output` recording keeps its track.
    None,
}

impl VideoOut {
    pub fn is_terminal(self) -> bool {
        !matches!(self, VideoOut::Sdl)
    }

    pub fn terminal(self) -> Option<TerminalVideo> {
        match self {
            VideoOut::Sdl => None,
            VideoOut::Sixel => Some(TerminalVideo::Sixel),
            VideoOut::Kitty => Some(TerminalVideo::Kitty),
        }
    }
}
pub(crate) use event_loop::{
    FastForwardState, InputSource, LoopInputs, LoopOptions, run_event_loop,
};
pub(crate) use render::NesRender;

use nes_core::audio_chunk::ChunkedAudioPush;
use nes_core::nes::controller::Button;
use sdl2::audio::AudioQueue;
use sdl2::keyboard::Keycode;
use std::{cell::Cell, rc::Rc, sync::mpsc};

/// Re-export sdl2 so downstream crates can use its types without a direct dependency.
pub use sdl2;

pub const AUDIO_SAMPLE_RATE: i32 = nes_core::nes::apu::Apu::SAMPLE_RATE as i32;
/// Re-exported so queue sizing (`drivers.rs`) and thresholds name one cadence.
pub use nes_core::audio_chunk::AUDIO_CHUNK_SAMPLES;
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

/// Unified audio driver: SDL playback runs unless built without a queue
/// (`--audio-out none`); recording into `audio_tx` is gated by `Option`
/// exactly like `NesRender::video_tx`. A `None` queue skips the SDL audio
/// subsystem entirely so audio-less servers never fail on open, while the
/// `--output` ffmpeg track keeps flowing through `sample_buf`/`send_policy`.
/// Interactive mode passes `audio_tx = None`, movie mode passes its channel.
/// The fast-forward skip flag is shared with `FastForwardState` via `Rc`
/// because `NesMachine` exposes `render_mut()` but no `audio_mut()`.
pub struct NesAudioDriver {
    queue: Option<AudioQueue<f32>>,
    chunker: ChunkedAudioPush,
    audio_tx: Option<mpsc::Sender<Vec<u8>>>,
    sample_buf: Vec<f32>,
    ff_skip: Rc<Cell<bool>>,
    send_policy: crate::recording_send::RecordingSendPolicy,
}

impl NesAudioDriver {
    pub fn new(
        queue: Option<AudioQueue<f32>>,
        audio_tx: Option<mpsc::Sender<Vec<u8>>>,
        ff_skip: Rc<Cell<bool>>,
        trim_frames: u32,
    ) -> Self {
        Self {
            queue,
            chunker: ChunkedAudioPush::new(),
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
        if self.queue.is_some() {
            // Borrow once: queue outlives the closure call.
            let queue = self.queue.as_ref().expect("checked above");
            self.chunker.push(samples, |chunk| emit_chunk(queue, chunk));
        }
        if self.audio_tx.is_some() {
            self.sample_buf.extend_from_slice(samples);
        }
    }

    pub fn flush_frame(&mut self) {
        if self.queue.is_some() {
            let queue = self.queue.as_ref().expect("checked above");
            self.chunker.flush(|chunk| emit_chunk(queue, chunk));
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    /// SDL chunks at the shared desktop cadence: the chunk size the queue
    /// threshold derives from is the 1024-sample playback cadence, so a
    /// change here must be a deliberate cross-sink decision. The rate
    /// itself stays at the `Apu::SAMPLE_RATE` associated path (#142).
    #[test]
    fn sdl_chunks_at_the_shared_desktop_cadence() {
        assert_eq!(AUDIO_CHUNK_SAMPLES, 1024);
        assert_eq!(
            AUDIO_SAMPLE_RATE,
            nes_core::nes::apu::Apu::SAMPLE_RATE as i32
        );
        assert_eq!(
            AUDIO_MAX_QUEUED_BYTES,
            (AUDIO_SAMPLE_RATE as u32) * 4 / 2,
            "queue-pressure threshold stays a half-second of mono f32 audio"
        );
    }
}
