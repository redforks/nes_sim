//! Framebuffer-backed `Render` without the `image` crate.
//!
//! `ImageRender` owns a fixed-size array of `256*240` RGBA pixels
//! (row-major, `256 × 240`). The buffer stores raw RGBA bytes and exposes
//! zero-copy `as_bytes`/`as_pixels` without pulling `image`.

use crate::render::Render;

pub const NES_WIDTH: usize = 256;
pub const NES_HEIGHT: usize = 240;

/// RGBA framebuffer for the PPU.
///
/// Logical canvas is `256×240`. Does not implement `Clone` — copies are
/// `O(W*H)` multi-hundred-KiB.
pub struct ImageRender {
    image: Box<[[u8; 4]; NES_WIDTH * NES_HEIGHT]>,
}

impl std::fmt::Debug for ImageRender {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ImageRender")
            .field("width", &Self::width())
            .field("height", &Self::height())
            .finish_non_exhaustive()
    }
}

impl Default for ImageRender {
    fn default() -> Self {
        Self::new()
    }
}

impl ImageRender {
    /// Output width in pixels (`256`).
    #[inline]
    pub const fn width() -> u32 {
        NES_WIDTH as u32
    }

    /// Output height in pixels (`240`).
    #[inline]
    pub const fn height() -> u32 {
        NES_HEIGHT as u32
    }

    /// Output dimensions.
    #[inline]
    pub const fn dimensions() -> (u32, u32) {
        (Self::width(), Self::height())
    }

    /// Create a zeroed (black-transparent) framebuffer.
    pub fn new() -> Self {
        Self {
            image: Box::new([[0; 4]; NES_WIDTH * NES_HEIGHT]),
        }
    }

    /// Alias for `new()` — kept for call-site compatibility (`default_dimension`
    /// previously meant `256×240`).
    pub fn default_dimension() -> Self {
        Self::new()
    }

    /// Raw byte view (`W*H*4` bytes, row-major RGBA) for SDL `texture.update`.
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        bytemuck::cast_slice(&*self.image)
    }

    /// Typed pixel view (`W*H` entries of `[R,G,B,A]`).
    #[inline]
    pub fn as_pixels(&self) -> &[[u8; 4]] {
        &*self.image
    }

    /// Read a single output pixel (for tests). Caller must guarantee bounds.
    #[inline]
    pub fn get_pixel(&self, x: u32, y: u32) -> [u8; 4] {
        let w = Self::width() as usize;
        let idx = y as usize * w + x as usize;
        self.image[idx]
    }
}

impl Render for ImageRender {
    #[inline]
    fn set_pixel(&mut self, index: u16, color: [u8; 4]) {
        unsafe {
            *self.image.get_unchecked_mut(index as usize) = color;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_image_render_set_pixel() {
        let mut renderer = ImageRender::new();

        renderer.set_pixel(0, [255, 0, 0, 255]);
        renderer.set_pixel(5 * 256 + 5, [0, 255, 0, 255]);
        renderer.set_pixel(10 * 256 + 10, [0, 0, 255, 255]);

        assert_eq!(renderer.get_pixel(0, 0), [255, 0, 0, 255]);
        assert_eq!(renderer.get_pixel(5, 5), [0, 255, 0, 255]);
        assert_eq!(renderer.get_pixel(10, 10), [0, 0, 255, 255]);
        assert_eq!(renderer.as_pixels()[5 * 256 + 5], [0, 255, 0, 255]);
    }

    #[test]
    fn test_image_render_borrow() {
        let renderer = ImageRender::default_dimension();
        assert_eq!(ImageRender::dimensions(), (256, 240));
        assert_eq!(renderer.as_bytes().len(), 256 * 240 * 4);
        assert_eq!(renderer.as_pixels().len(), 256 * 240);
    }
}
