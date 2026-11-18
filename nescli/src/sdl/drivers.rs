//! Unified SDL driver factory (ticket #121).
//!
//! Merges the SDL halves of the interactive `run` driver setup
//! (windowed/interactive-only) and the pre-merge movie-playback driver setup
//! (windowed/headless x movie) into a single factory. Both callers share one `AudioSpecDesired`
//! construction and one window/canvas branch; only the [`WindowSpec`] (title
//! and size) and the `headless` flag differ.
//!
//! Recording is handled by the unified types themselves (`NesRender`'s video
//! transmitter and `NesAudioDriver`'s audio transmitter, each with its
//! fast-forward flag and trim policy). The unified render type lives in
//! `super::render` (#122); the unified audio type lives in `super`.

use super::{AUDIO_CHUNK_SAMPLES, AUDIO_SAMPLE_RATE, NesAudioDriver};
use anyhow::Result;
use sdl2::{Sdl, audio::AudioSpecDesired, render::Canvas, video::Window};
use std::{cell::Cell, rc::Rc, sync::mpsc};

/// Window title and size for one SDL consumer.
pub struct WindowSpec {
    pub title: &'static str,
    pub width: u32,
    pub height: u32,
}

/// Interactive `run` window: 2x scale for visibility (256x240 -> 512x480).
pub const INTERACTIVE_WINDOW: WindowSpec = WindowSpec {
    title: "NES Simulator - Arrows move, Z/X A/B, Space Select, Enter Start",
    width: 512,
    height: 480,
};

/// Movie playback window: 4x scale matching the recording frame (1024x960).
pub const PLAYBACK_WINDOW: WindowSpec = WindowSpec {
    title: "NES Simulator - Movie Playback",
    width: 1024,
    height: 960,
};

/// Open the shared SDL audio queue and, unless `headless`, the window canvas.
///
/// Returns the canvas (`None` when headless) plus the unified audio driver.
/// Titles, sizes and the headless-`None` canvas behavior are preserved verbatim
/// from the two merged constructors. Pass `audio_tx = None` and
/// `trim_frames = 0` for interactive mode; movie playback passes its channel
/// and trim count. The `ff_skip` flag is shared with `FastForwardState`.
pub fn create_sdl_drivers(
    sdl_context: &Sdl,
    window: &WindowSpec,
    headless: bool,
    audio_tx: Option<mpsc::Sender<Vec<u8>>>,
    ff_skip: Rc<Cell<bool>>,
    trim_frames: u32,
) -> Result<(Option<Canvas<Window>>, NesAudioDriver)> {
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
    let audio_driver = NesAudioDriver::new(
        audio_queue,
        AUDIO_SAMPLE_RATE as u32,
        audio_tx,
        ff_skip,
        trim_frames,
    );

    let canvas = if !headless {
        let video_subsystem = sdl_context.video().map_err(|e| anyhow::anyhow!(e))?;
        let window = video_subsystem
            .window(window.title, window.width, window.height)
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

    Ok((canvas, audio_driver))
}

#[cfg(test)]
mod tests {
    use super::{INTERACTIVE_WINDOW, PLAYBACK_WINDOW};

    #[test]
    fn interactive_window_spec_preserves_run_title_and_size() {
        assert_eq!(
            INTERACTIVE_WINDOW.title,
            "NES Simulator - Arrows move, Z/X A/B, Space Select, Enter Start"
        );
        assert_eq!(INTERACTIVE_WINDOW.width, 512);
        assert_eq!(INTERACTIVE_WINDOW.height, 480);
    }

    #[test]
    fn playback_window_spec_preserves_movie_title_and_size() {
        assert_eq!(PLAYBACK_WINDOW.title, "NES Simulator - Movie Playback");
        assert_eq!(PLAYBACK_WINDOW.width, 1024);
        assert_eq!(PLAYBACK_WINDOW.height, 960);
    }
}
