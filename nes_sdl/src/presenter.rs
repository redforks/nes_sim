//! SDL adapter for the pure presentation plan.
//!
//! [FramePresenter] owns the frame texture. Under sdl2's `unsafe_textures`
//! feature a `Texture` is owned by the `Canvas` and carries no borrow, so one
//! texture can be created on first use and reused for the lifetime of the
//! canvas instead of being recreated every frame. No `unsafe` code is needed.

use crate::presentation::{FramePresentation, OverlayIcon, OverlayPlacement};
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use sdl2::render::{BlendMode, Canvas, ScaleMode, Texture};
use sdl2::video::Window;

/// Owns the reusable frame texture plus one lazily created texture per overlay
/// icon, and performs one upload plus one scaled copy per frame.
pub struct FramePresenter {
    texture: Option<Texture>,
    size: (u32, u32),
    icons: Vec<OverlayIcon>,
    icon_textures: Vec<Option<Texture>>,
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
            icons: Vec::new(),
            icon_textures: Vec::new(),
        }
    }

    /// Provide the overlay icon pixel sources. The matching SDL textures are
    /// created lazily on first use and reused across frames.
    pub fn set_overlay_icons(&mut self, icons: Vec<OverlayIcon>) {
        self.icon_textures = icons.iter().map(|_| None).collect();
        self.icons = icons;
    }

    /// Upload the frame into the reused texture and scale-copy it to the
    /// plan's destination rect, then present.
    pub fn present(&mut self, canvas: &mut Canvas<Window>, frame: &FramePresentation<'_>) {
        let size = (frame.width, frame.height);
        if self.texture.is_none() || self.size != size {
            let mut texture = canvas
                .create_texture_streaming(PixelFormatEnum::ABGR8888, size.0, size.1)
                .expect("failed to create SDL frame texture");
            // The frame is an exact integer multiple of the logical 256x240
            // picture, so nearest sampling reproduces it exactly.
            texture.set_scale_mode(ScaleMode::Nearest);
            self.texture = Some(texture);
            self.size = size;
        }
        let texture = self
            .texture
            .as_mut()
            .expect("frame texture is created on first present");
        texture
            .update(None, frame.bytes, (frame.width * 4) as usize)
            .expect("failed to upload SDL frame texture");
        let dst = Rect::new(frame.dst.0, frame.dst.1, frame.dst.2, frame.dst.3);
        canvas
            .copy(texture, None, dst)
            .expect("failed to copy SDL frame texture");
        for placement in &frame.overlays {
            self.draw_icon(canvas, placement);
        }
        canvas.present();
    }

    /// Draw one overlay icon, creating its texture on first use.
    fn draw_icon(&mut self, canvas: &mut Canvas<Window>, placement: &OverlayPlacement) {
        let Some(icon) = self.icons.get(placement.icon) else {
            return;
        };
        if self.icon_textures[placement.icon].is_none() {
            let mut texture = canvas
                .create_texture_streaming(PixelFormatEnum::ABGR8888, icon.width, icon.height)
                .expect("failed to create SDL overlay icon texture");
            texture.set_blend_mode(BlendMode::Blend);
            texture
                .update(None, &icon.bytes, (icon.width * 4) as usize)
                .expect("failed to upload SDL overlay icon texture");
            self.icon_textures[placement.icon] = Some(texture);
        }
        let texture = self.icon_textures[placement.icon]
            .as_ref()
            .expect("overlay icon texture is created on first use");
        canvas
            .copy(
                texture,
                None,
                Rect::new(
                    placement.dst.0,
                    placement.dst.1,
                    placement.dst.2,
                    placement.dst.3,
                ),
            )
            .expect("failed to copy SDL overlay icon texture");
    }
}
