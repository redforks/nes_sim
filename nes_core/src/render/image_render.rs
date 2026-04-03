//! Image-based render implementation using the `image` crate
//!
//! This module provides a Render implementation for `RgbaImage` from the image crate,
//! maintaining backward compatibility with the existing PPU rendering code.

use crate::render::Render;
use image::{Rgba, RgbaImage};
use std::fmt::Debug;

/// Wrapper around `RgbaImage` that implements the `Render` trait
///
/// This type provides a bridge between the new Render trait and the existing
/// RgbaImage type, allowing the PPU to use either abstraction.
/// The image is stored directly as an `RgbaImage` buffer.
#[derive(Debug, Clone)]
pub struct ImageRender {
    image: RgbaImage,
}

impl ImageRender {
    /// Create a new ImageRender with the specified dimensions
    ///
    /// # Parameters
    /// - `width`: Width in pixels (typically 256 for NES)
    /// - `height`: Height in pixels (typically 240 for NES)
    pub fn new(width: u32, height: u32) -> Self {
        Self {
            image: RgbaImage::new(width, height),
        }
    }

    pub fn default_dimension() -> Self {
        Self::new(256, 240)
    }

    /// Get a reference to the underlying image buffer
    pub fn borrow_image(&self) -> &RgbaImage {
        &self.image
    }
}

impl Render for ImageRender {
    fn clear(&mut self, color: [u8; 4]) {
        let pixel = Rgba(color);
        for p in self.image.pixels_mut() {
            *p = pixel;
        }
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        self.image.put_pixel(x, y, Rgba(color));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_image_render_clear() {
        let mut renderer = ImageRender::new(10, 10);
        renderer.clear([255, 0, 0, 255]);

        // Check that all pixels are red
        let image = renderer.borrow_image();
        for pixel in image.pixels() {
            assert_eq!(*pixel, Rgba([255, 0, 0, 255]));
        }
    }

    #[test]
    fn test_image_render_set_pixel() {
        let mut renderer = ImageRender::new(10, 10);

        // Set some pixels
        renderer.set_pixel(0, 0, [255, 0, 0, 255]);
        renderer.set_pixel(5, 5, [0, 255, 0, 255]);
        renderer.set_pixel(9, 9, [0, 0, 255, 255]);

        let image = renderer.borrow_image();
        assert_eq!(image.get_pixel(0, 0), &Rgba([255, 0, 0, 255]));
        assert_eq!(image.get_pixel(5, 5), &Rgba([0, 255, 0, 255]));
        assert_eq!(image.get_pixel(9, 9), &Rgba([0, 0, 255, 255]));
    }

    #[test]
    fn test_image_render_borrow() {
        let renderer = ImageRender::default_dimension();

        // Test borrow_image
        let image = renderer.borrow_image();
        assert_eq!(image.dimensions(), (256, 240));
    }
}
