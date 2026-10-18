//! SDL adapter for the pure presentation plan.
//!
//! [FramePresenter] owns the frame texture. Under sdl2's `unsafe_textures`
//! feature a `Texture` is owned by the `Canvas` and carries no borrow, so one
//! texture can be created on first use and reused for the lifetime of the
//! canvas instead of being recreated every frame. No `unsafe` code is needed.

use crate::presentation::FramePresentation;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use sdl2::render::{Canvas, Texture};
use sdl2::video::Window;

/// Owns the reusable frame texture and performs one upload plus one scaled copy per frame.
pub struct FramePresenter {
    texture: Option<Texture>,
    size: (u32, u32),
}

impl std::fmt::Debug for FramePresenter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FramePresenter")
            .field("size", &self.size)
            .finish_non_exhaustive()
    }
}

impl Default for FramePresenter {
    fn default() -> Self {
        Self::new()
    }
}

impl FramePresenter {
    /// Create a presenter with no texture yet; the texture is created on the
    /// first `present`.
    pub fn new() -> Self {
        Self {
            texture: None,
            size: (0, 0),
        }
    }

    /// Upload the frame into the reused texture and scale-copy it to the
    /// plan's destination rect, then present.
    pub fn present(&mut self, canvas: &mut Canvas<Window>, frame: &FramePresentation<'_>) {
        let size = (frame.width, frame.height);
        if self.texture.is_none() || self.size != size {
            self.texture = Some(
                canvas
                    .create_texture_streaming(PixelFormatEnum::ABGR8888, size.0, size.1)
                    .expect("failed to create SDL frame texture"),
            );
            self.size = size;
        }
        let texture = self
            .texture
            .as_mut()
            .expect("frame texture is created on first present");
        texture
            .update(None, frame.bytes, (frame.width * 4) as usize)
            .expect("failed to upload SDL frame texture");
        canvas
            .copy(
                texture,
                None,
                Rect::new(frame.dst.0, frame.dst.1, frame.dst.2, frame.dst.3),
            )
            .expect("failed to copy SDL frame texture");
        canvas.present();
    }
}
