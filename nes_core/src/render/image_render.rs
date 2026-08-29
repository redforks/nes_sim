//! Framebuffer-backed `Render` without the `image` crate.
//!
//! `ImageRender<N>` owns a heap-allocated `Box<[[u8; 4]]>` of
//! `256*240*N*N` pixels row-major at `256*N × 240*N` output pixels.
//! `N` is the Zoom Factor (default `1`): each logical PPU
//! `set_pixel(x,y)` (`x<256,y<240`) replicates to an `N×N` block. The
//! buffer stores raw RGBA bytes and exposes zero-copy `as_bytes`/`as_pixels`
//! without pulling `image`. The boxed slice has the same heap footprint
//! as `Box<[[u8;4]; 256*240*N*N]>` but stays on stable Rust (avoids
//! `generic_const_exprs`).

use crate::render::Render;

pub const NES_WIDTH: usize = 256;
pub const NES_HEIGHT: usize = 240;

/// Heap-allocated RGBA framebuffer for the PPU.
///
/// `N` is the Zoom Factor (`N > 0`, default `1`). Logical canvas is
/// `256×240`; output canvas is `256*N × 240*N` pixels. Does not implement
/// `Clone` — copies are `O(W*H*N²)` multi-MiB.
pub struct ImageRender<const N: usize = 1> {
    image: Box<[[u8; 4]]>,
}

impl<const N: usize> std::fmt::Debug for ImageRender<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ImageRender")
            .field("width", &Self::width())
            .field("height", &Self::height())
            .field("zoom", &N)
            .finish_non_exhaustive()
    }
}

impl<const N: usize> Default for ImageRender<N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const N: usize> ImageRender<N> {
    /// Output width in pixels (`256 * N`).
    #[inline]
    pub const fn width() -> u32 {
        (NES_WIDTH * N) as u32
    }

    /// Output height in pixels (`240 * N`).
    #[inline]
    pub const fn height() -> u32 {
        (NES_HEIGHT * N) as u32
    }

    /// Output dimensions.
    #[inline]
    pub const fn dimensions() -> (u32, u32) {
        (Self::width(), Self::height())
    }

    /// Create a zeroed (black-transparent) framebuffer.
    pub fn new() -> Self {
        assert!(N > 0, "Zoom Factor N must be non-zero");
        let len = NES_WIDTH * NES_HEIGHT * N * N;
        let image = vec![[0; 4]; len].into_boxed_slice();
        Self { image }
    }

    /// Alias for `new()` — kept for call-site compatibility (`default_dimension`
    /// previously meant `256×240`).
    pub fn default_dimension() -> Self {
        Self::new()
    }

    /// Raw byte view (`W*H*4` bytes, row-major RGBA) for SDL `texture.update`.
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        bytemuck::cast_slice(&self.image)
    }

    /// Mutable raw bytes.
    #[inline]
    pub fn as_bytes_mut(&mut self) -> &mut [u8] {
        bytemuck::cast_slice_mut(&mut self.image)
    }

    /// Typed pixel view (`W*H` entries of `[R,G,B,A]`).
    #[inline]
    pub fn as_pixels(&self) -> &[[u8; 4]] {
        &self.image
    }

    /// Mutable typed pixels.
    #[inline]
    pub fn as_pixels_mut(&mut self) -> &mut [[u8; 4]] {
        &mut self.image
    }

    /// Compatibility alias — returns typed pixels (was `&RgbaImage`).
    #[inline]
    pub fn borrow_image(&self) -> &[[u8; 4]] {
        self.as_pixels()
    }

    /// Mutable compatibility alias.
    #[inline]
    pub fn borrow_image_mut(&mut self) -> &mut [[u8; 4]] {
        self.as_pixels_mut()
    }

    /// Read a single output pixel (for tests). Caller must guarantee bounds.
    #[inline]
    pub fn get_pixel(&self, x: u32, y: u32) -> [u8; 4] {
        let w = Self::width() as usize;
        let idx = y as usize * w + x as usize;
        self.image[idx]
    }
}

impl<const N: usize> Render for ImageRender<N> {
    #[inline]
    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        assert!(N > 0, "Zoom Factor N must be non-zero");
        // Silent-ignore out-of-logical-bounds; caller guarantees x<256,y<240.
        if x >= NES_WIDTH as u32 || y >= NES_HEIGHT as u32 {
            return;
        }
        if N == 1 {
            let idx = y as usize * NES_WIDTH + x as usize;
            self.image[idx] = color;
        } else {
            let base_x = x as usize * N;
            let base_y = y as usize * N;
            let stride = NES_WIDTH * N;
            for dy in 0..N {
                let row = (base_y + dy) * stride + base_x;
                for dx in 0..N {
                    self.image[row + dx] = color;
                }
            }
        }
    }

    #[inline]
    fn pixel_brightness(&self, x: u32, y: u32) -> u32 {
        if x >= Self::width() || y >= Self::height() {
            return 0;
        }
        let w = NES_WIDTH * N;
        let idx = y as usize * w + x as usize;
        let [r, g, b, _] = self.image[idx];
        u32::from(r) + u32::from(g) + u32::from(b)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_image_render_set_pixel() {
        let mut renderer = ImageRender::<1>::new();

        renderer.set_pixel(0, 0, [255, 0, 0, 255]);
        renderer.set_pixel(5, 5, [0, 255, 0, 255]);
        renderer.set_pixel(10, 10, [0, 0, 255, 255]);

        assert_eq!(renderer.get_pixel(0, 0), [255, 0, 0, 255]);
        assert_eq!(renderer.get_pixel(5, 5), [0, 255, 0, 255]);
        assert_eq!(renderer.get_pixel(10, 10), [0, 0, 255, 255]);
        assert_eq!(renderer.as_pixels()[5 * 256 + 5], [0, 255, 0, 255]);
    }

    #[test]
    fn test_image_render_borrow() {
        let renderer = ImageRender::<1>::default_dimension();
        assert_eq!(ImageRender::<1>::dimensions(), (256, 240));
        assert_eq!(renderer.as_bytes().len(), 256 * 240 * 4);
        assert_eq!(renderer.as_pixels().len(), 256 * 240);
    }

    #[test]
    fn test_zoom_replication() {
        let mut r = ImageRender::<2>::new();
        r.set_pixel(0, 0, [1, 2, 3, 255]);
        assert_eq!(r.get_pixel(0, 0), [1, 2, 3, 255]);
        assert_eq!(r.get_pixel(1, 0), [1, 2, 3, 255]);
        assert_eq!(r.get_pixel(0, 1), [1, 2, 3, 255]);
        assert_eq!(r.get_pixel(1, 1), [1, 2, 3, 255]);
        assert_eq!(r.get_pixel(2, 0), [0, 0, 0, 0]);
        assert_eq!(ImageRender::<2>::dimensions(), (512, 480));
    }

    #[test]
    fn test_zoom_4_replication() {
        let mut r = ImageRender::<4>::new();
        r.set_pixel(1, 1, [9, 9, 9, 255]);
        for dy in 0..4 {
            for dx in 0..4 {
                assert_eq!(r.get_pixel(4 + dx, 4 + dy), [9, 9, 9, 255]);
            }
        }
        assert_eq!(ImageRender::<4>::dimensions(), (1024, 960));
    }
}
