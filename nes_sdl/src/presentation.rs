//! Pure presentation planning: what one frame puts on screen.
//!
//! [`plan`] is SDL-free and unit-testable without a window. It turns a rendered
//! frame, the overlay placements the caller wants shown, and the destination
//! (window) size into a [`FramePresentation`]: the frame bytes and dimensions,
//! the destination rectangle (the whole window), and the overlay placements
//! scaled from logical 1x coordinates into destination pixels. The SDL adapter
//! (`FramePresenter`) owns the reused frame texture and performs the upload and
//! the scaled copy.

use nes_core::render::ImageRender;
use nes_core::render::image_render::{NES_HEIGHT, NES_WIDTH};

/// A destination rectangle `(x, y, width, height)` in destination pixels.
pub type DstRect = (i32, i32, u32, u32);

/// One overlay icon placement.
///
/// `icon` indexes the overlay icon table; `dst` is in the frame's logical
/// 1x 256x240 coordinate space. [`plan`] scales it to destination pixels.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OverlayPlacement {
    pub icon: usize,
    pub dst: DstRect,
}

/// Everything one frame puts on screen, ready for the SDL adapter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FramePresentation<'a> {
    /// Raw RGBA bytes of the frame (`width * height * 4`).
    pub bytes: &'a [u8],
    /// Frame width in pixels (Zoom Factor applied).
    pub width: u32,
    /// Frame height in pixels (Zoom Factor applied).
    pub height: u32,
    /// Destination rectangle: the whole window.
    pub dst: DstRect,
    /// Overlay icons scaled to destination pixels, in draw order.
    pub overlays: Vec<OverlayPlacement>,
}

/// Compute the presentation for one frame.
///
/// The frame is scaled by SDL, so the destination rect is the whole window and
/// no pixel replication happens here. Overlay placements are given in logical
/// 1x 256x240 coordinates and are scaled by the destination size, independent
/// of the frame's Zoom Factor `N`.
pub fn plan<'a, const N: usize>(
    frame: &'a ImageRender<N>,
    overlays: &[OverlayPlacement],
    dst_size: (u32, u32),
) -> FramePresentation<'a> {
    let scale_x = dst_size.0 as f64 / NES_WIDTH as f64;
    let scale_y = dst_size.1 as f64 / NES_HEIGHT as f64;
    let scaled_overlays = overlays
        .iter()
        .map(|p| OverlayPlacement {
            icon: p.icon,
            dst: (
                (p.dst.0 as f64 * scale_x).round() as i32,
                (p.dst.1 as f64 * scale_y).round() as i32,
                (p.dst.2 as f64 * scale_x).round() as u32,
                (p.dst.3 as f64 * scale_y).round() as u32,
            ),
        })
        .collect();
    FramePresentation {
        bytes: frame.as_bytes(),
        width: ImageRender::<N>::width(),
        height: ImageRender::<N>::height(),
        dst: (0, 0, dst_size.0, dst_size.1),
        overlays: scaled_overlays,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nes_core::render::Render;

    #[test]
    fn one_x_frame_bytes_and_dimensions() {
        let mut frame = ImageRender::<1>::new();
        frame.set_pixel(0, 0, [1, 2, 3, 255]);
        let p = plan(&frame, &[], (512, 480));
        assert_eq!(p.width, 256);
        assert_eq!(p.height, 240);
        assert_eq!(p.bytes.len(), 256 * 240 * 4);
        assert_eq!(p.bytes[0..4], [1, 2, 3, 255]);
    }

    #[test]
    fn four_x_frame_bytes_and_dimensions() {
        let frame = ImageRender::<4>::new();
        let p = plan(&frame, &[], (1024, 960));
        assert_eq!(p.width, 1024);
        assert_eq!(p.height, 960);
        assert_eq!(p.bytes.len(), 1024 * 960 * 4);
    }

    #[test]
    fn presentation_bytes_are_the_frame_bytes() {
        let mut frame = ImageRender::<4>::new();
        frame.set_pixel(2, 3, [9, 8, 7, 255]);
        let p = plan(&frame, &[], (1024, 960));
        assert_eq!(p.bytes, frame.as_bytes());
    }

    #[test]
    fn destination_rect_is_the_whole_window() {
        let frame = ImageRender::<1>::new();
        let p = plan(&frame, &[], (1024, 960));
        assert_eq!(p.dst, (0, 0, 1024, 960));
    }

    #[test]
    fn no_overlays_makes_an_empty_list() {
        let frame = ImageRender::<1>::new();
        let p = plan(&frame, &[], (512, 480));
        assert!(p.overlays.is_empty());
    }

    #[test]
    fn overlays_scale_from_logical_to_destination() {
        let frame = ImageRender::<1>::new();
        let overlays = [
            OverlayPlacement {
                icon: 3,
                dst: (205, 222, 16, 16),
            },
            OverlayPlacement {
                icon: 0,
                dst: (238, 222, 16, 16),
            },
            OverlayPlacement {
                icon: 1,
                dst: (222, 222, 16, 16),
            },
        ];
        let p = plan(&frame, &overlays, (1024, 960));
        assert_eq!(
            p.overlays[0],
            OverlayPlacement {
                icon: 3,
                dst: (820, 888, 64, 64)
            }
        );
        assert_eq!(
            p.overlays[1],
            OverlayPlacement {
                icon: 0,
                dst: (952, 888, 64, 64)
            }
        );
        assert_eq!(
            p.overlays[2],
            OverlayPlacement {
                icon: 1,
                dst: (888, 888, 64, 64)
            }
        );
    }

    #[test]
    fn overlay_scale_depends_on_destination_not_zoom_factor() {
        let one = ImageRender::<1>::new();
        let four = ImageRender::<4>::new();
        let overlays = [OverlayPlacement {
            icon: 2,
            dst: (205, 222, 16, 16),
        }];
        let a = plan(&one, &overlays, (1024, 960));
        let b = plan(&four, &overlays, (1024, 960));
        assert_eq!(a.overlays, b.overlays);
        assert_eq!(a.overlays[0].dst, (820, 888, 64, 64));
    }

    #[test]
    fn overlays_scale_at_two_x_window() {
        let frame = ImageRender::<1>::new();
        let overlays = [OverlayPlacement {
            icon: 5,
            dst: (205, 222, 16, 16),
        }];
        let p = plan(&frame, &overlays, (512, 480));
        assert_eq!(p.overlays[0].dst, (410, 444, 32, 32));
    }

    #[test]
    fn overlays_keep_draw_order() {
        let frame = ImageRender::<1>::new();
        let overlays = [
            OverlayPlacement {
                icon: 7,
                dst: (10, 10, 8, 8),
            },
            OverlayPlacement {
                icon: 11,
                dst: (20, 20, 8, 8),
            },
        ];
        let p = plan(&frame, &overlays, (1024, 960));
        assert_eq!(p.overlays[0].icon, 7);
        assert_eq!(p.overlays[1].icon, 11);
    }
}
