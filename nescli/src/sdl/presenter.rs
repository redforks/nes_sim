//! SDL adapter for the pure presentation plan.
//!
//! [FramePresenter] owns the frame texture. Under sdl2's `unsafe_textures`
//! feature a `Texture` is owned by the `Canvas` and carries no borrow, so one
//! texture can be created on first use and reused for the lifetime of the
//! canvas instead of being recreated every frame. No `unsafe` code is needed.

use crate::sdl::presentation::{FramePresentation, OverlayIcon, OverlayPlacement, plan};
use nes_core::render::ImageRender;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use sdl2::render::{BlendMode, Canvas, ScaleMode, Texture};
use sdl2::video::Window;
use std::rc::Rc;

/// Owns the reusable frame texture plus one lazily created texture per overlay
/// icon, and performs one upload plus one scaled copy per frame.
pub struct FramePresenter {
    texture: Option<Texture>,
    size: (u32, u32),
    icons: Rc<[OverlayIcon]>,
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
            icons: Rc::from(Vec::new()),
            icon_textures: Vec::new(),
        }
    }

    /// Provide the overlay icon pixel sources. The matching SDL textures are
    /// created lazily on first use and reused across frames. The table is
    /// shared with the caller (one `Rc`) rather than cloned.
    pub fn set_overlay_icons(&mut self, icons: Rc<[OverlayIcon]>) {
        self.icon_textures = icons.iter().map(|_| None).collect();
        self.icons = icons;
    }

    /// Upload the frame into the reused texture and scale-copy it to the
    /// plan's destination rect, then present.
    #[hotpath::measure(impl_type = "FramePresenter")]
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
        hotpath::measure_block!("sdl_texture_upload", {
            texture
                .update(None, frame.bytes, (frame.width * 4) as usize)
                .expect("failed to upload SDL frame texture");
        });
        let dst = Rect::new(frame.dst.0, frame.dst.1, frame.dst.2, frame.dst.3);
        hotpath::measure_block!("sdl_canvas_copy", {
            canvas
                .copy(texture, None, dst)
                .expect("failed to copy SDL frame texture");
        });
        hotpath::measure_block!("sdl_overlay_draw", {
            for placement in &frame.overlays {
                self.draw_icon(canvas, placement);
            }
        });
        hotpath::measure_block!("sdl_canvas_present", {
            canvas.present();
        });
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

/// The shared display sequence: query the renderer output size, build the pure
/// presentation plan, and present it through the reused texture. `NesRender`
/// uses this for both modes so the plan/present seam has a single call site.
#[hotpath::measure]
pub fn present_frame(
    canvas: &mut Canvas<Window>,
    presenter: &mut FramePresenter,
    frame: &ImageRender,
    overlays: &[OverlayPlacement],
) {
    let dst_size = canvas
        .output_size()
        .expect("failed to query SDL renderer output size");
    let presentation = plan(frame, overlays, dst_size);
    presenter.present(canvas, &presentation);
}

#[cfg(test)]
mod tests {
    use super::FramePresenter;
    use crate::sdl::presentation::{OverlayIcon, OverlayPlacement, plan};
    use crate::sdl::render::{
        RECORD_HEIGHT, RECORD_WIDTH, composite_overlay_icons, load_overlay_icons, upscale_into,
    };
    use nes_core::render::image_render::NES_WIDTH;
    use nes_core::render::{ImageRender, Render};
    use std::rc::Rc;

    /// Serializes the SDL tests. The sdl2 crate allows `init` on only one
    /// thread per process lifetime, so two tests must not initialise SDL at
    /// the same time or the loser silently skips.
    static SDL_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    #[test]
    fn sdl_overlay_blend_matches_legacy_integer_blend() {
        use crate::sdl::sdl2::pixels::PixelFormatEnum;

        let _sdl_guard = SDL_TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        let Ok(sdl) = crate::sdl::sdl2::init() else {
            eprintln!("skip: SDL init failed");
            return;
        };
        let Ok(video) = sdl.video() else {
            eprintln!("skip: SDL video subsystem unavailable");
            return;
        };
        let dst_size = (RECORD_WIDTH as u32, RECORD_HEIGHT as u32);
        let Ok(window) = video
            .window("overlay-blend-test", dst_size.0, dst_size.1)
            .hidden()
            .build()
        else {
            eprintln!("skip: SDL window unavailable");
            return;
        };
        let Ok(mut canvas) = window.into_canvas().software().build() else {
            eprintln!("skip: SDL software renderer unavailable");
            return;
        };

        // A frame with a distinct colour per logical pixel, so any blend or
        // upscale mistake changes the comparison.
        let mut frame = ImageRender::new();
        for y in 0..240u32 {
            for x in 0..256u32 {
                frame.set_pixel(
                    (y * 256 + x) as u16,
                    [
                        (x.wrapping_mul(3)) as u8,
                        (y.wrapping_mul(5)) as u8,
                        (x ^ y) as u8,
                        255,
                    ],
                );
            }
        }

        // The real gamepad icons: their anti-aliased edges are exactly where
        // a rounded SDL blend could diverge from the truncating legacy blend.
        let icons: Rc<[OverlayIcon]> = Rc::from(load_overlay_icons());
        let placements = [
            OverlayPlacement {
                icon: 0,
                dst: (2, 3, 16, 16),
            },
            OverlayPlacement {
                icon: 5,
                dst: (200, 206, 16, 16),
            },
            OverlayPlacement {
                icon: 16,
                dst: (120, 100, 16, 16),
            },
        ];

        // SDL path: plan + presenter (software renderer, then read back).
        let presentation = plan(&frame, &placements, dst_size);
        let mut presenter = FramePresenter::new();
        presenter.set_overlay_icons(icons.clone());
        presenter.present(&mut canvas, &presentation);
        let sdl_pixels = canvas
            .read_pixels(None, PixelFormatEnum::ABGR8888)
            .expect("software renderer read-back");

        // Legacy path: the same 4x upscale plus the host integer blend.
        let mut expected = vec![0u8; RECORD_WIDTH * RECORD_HEIGHT * 4];
        upscale_into(frame.as_bytes(), &mut expected);
        composite_overlay_icons(&mut expected, &placements, &icons);
        let mut plain = vec![0u8; RECORD_WIDTH * RECORD_HEIGHT * 4];
        upscale_into(frame.as_bytes(), &mut plain);
        assert_ne!(plain, expected, "the icon composite must change the frame");

        let scale = (RECORD_WIDTH / NES_WIDTH) as u32;
        let inside = |px: u32, py: u32| {
            placements.iter().any(|p| {
                let x0 = p.dst.0 as u32 * scale;
                let y0 = p.dst.1 as u32 * scale;
                let x1 = x0 + p.dst.2 * scale;
                let y1 = y0 + p.dst.3 * scale;
                px >= x0 && px < x1 && py >= y0 && py < y1
            })
        };

        let mut max_inside = 0u8;
        let mut max_outside = 0u8;
        for y in 0..RECORD_HEIGHT as u32 {
            for x in 0..RECORD_WIDTH as u32 {
                let i = ((y * RECORD_WIDTH as u32 + x) * 4) as usize;
                for c in 0..4 {
                    let delta = sdl_pixels[i + c].abs_diff(expected[i + c]);
                    if inside(x, y) {
                        max_inside = max_inside.max(delta);
                    } else {
                        max_outside = max_outside.max(delta);
                    }
                }
            }
        }
        eprintln!(
            "SDL overlay blend delta: max inside icon pixels = {max_inside} LSB, max outside = {max_outside} LSB"
        );
        assert_eq!(max_outside, 0, "pixels outside the icons must be identical");
        assert!(
            max_inside <= 1,
            "icon pixels may differ by at most 1 LSB, got {max_inside}"
        );
    }

    /// Both modes force nearest on the frame texture: the frame is an
    /// exact integer multiple of the logical 256x240 picture, so nearest
    /// sampling reproduces it exactly. This pins it: if a future SDL
    /// changed the default to linear, omitting this would start changing
    /// the 2x picture.
    #[test]
    fn sdl_default_texture_scale_mode_is_nearest() {
        use crate::sdl::sdl2::pixels::PixelFormatEnum;
        use crate::sdl::sdl2::render::ScaleMode;

        let _sdl_guard = SDL_TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        let Ok(sdl) = crate::sdl::sdl2::init() else {
            eprintln!("skip: SDL init failed");
            return;
        };
        let Ok(video) = sdl.video() else {
            eprintln!("skip: SDL video subsystem unavailable");
            return;
        };
        let Ok(window) = video.window("scale-mode-test", 256, 240).hidden().build() else {
            eprintln!("skip: SDL window unavailable");
            return;
        };
        let Ok(canvas) = window.into_canvas().software().build() else {
            eprintln!("skip: SDL software renderer unavailable");
            return;
        };
        let texture = canvas
            .texture_creator()
            .create_texture_streaming(PixelFormatEnum::ABGR8888, 256, 240)
            .expect("create texture");
        assert!(
            matches!(texture.scale_mode(), ScaleMode::Nearest),
            "SDL default scale mode must be Nearest for the run path to be unchanged"
        );
    }
}
