//! Unified SDL driver factory (ticket #121).
//!
//! Merges the SDL halves of the interactive `run` driver setup
//! (windowed/interactive-only) and the pre-merge movie-playback driver setup
//! (windowed/headless x movie) into a single factory. Both callers share one `AudioSpecDesired`
//! construction and one window/canvas branch; only the [`WindowSpec`] (title
//! and size) and the `headless` flag differ.
//!
//! The movie recording wrappers (`NesRender`'s video transmitter with its
//! fast-forward flag and trim policy, plus `RecordAudioDriver`) stay with the
//! movie-playback path of `run --movie`: they feed the render/audio wrappers, not SDL resource
//! creation. The unified render type itself lives in `super::render` (#122).

use super::{AUDIO_CHUNK_SAMPLES, AUDIO_SAMPLE_RATE, SdlAudioDriver};
use anyhow::Result;
use sdl2::{Sdl, audio::AudioSpecDesired, render::Canvas, video::Window};

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
/// Returns the canvas (`None` when headless) plus the live audio driver.
/// Titles, sizes and the headless-`None` canvas behavior are preserved verbatim
/// from the two merged constructors.
pub fn create_sdl_drivers(
    sdl_context: &Sdl,
    window: &WindowSpec,
    headless: bool,
) -> Result<(Option<Canvas<Window>>, SdlAudioDriver)> {
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
    let audio_driver = SdlAudioDriver::new(audio_queue, AUDIO_SAMPLE_RATE as u32);

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
