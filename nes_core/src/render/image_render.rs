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

    fn dimensions(&self) -> (u32, u32) {
        self.image.dimensions()
    }
}

/// Implement AsRef for convenient access to the image
impl AsRef<RgbaImage> for ImageRender {
    fn as_ref(&self) -> &RgbaImage {
        &self.image
    }
}

// Also implement Render directly for RgbaImage for convenience
// This allows existing code to work with minimal changes
impl Render for RgbaImage {
    fn clear(&mut self, color: [u8; 4]) {
        let pixel = Rgba(color);
        for p in self.pixels_mut() {
            *p = pixel;
        }
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        self.put_pixel(x, y, Rgba(color));
    }

    fn dimensions(&self) -> (u32, u32) {
        self.dimensions()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_image_render_new() {
        let renderer = ImageRender::new(256, 240);
        assert_eq!(renderer.dimensions(), (256, 240));
    }

    #[test]
    fn test_image_render_clear() {
        let mut renderer = ImageRender::new(10, 10);
        renderer.clear([255, 0, 0, 255]);

        // Check that all pixels are red
        for pixel in renderer.as_ref().pixels() {
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

        assert_eq!(renderer.as_ref().get_pixel(0, 0), &Rgba([255, 0, 0, 255]));
        assert_eq!(renderer.as_ref().get_pixel(5, 5), &Rgba([0, 255, 0, 255]));
        assert_eq!(renderer.as_ref().get_pixel(9, 9), &Rgba([0, 0, 255, 255]));
    }

    #[test]
    fn test_rgba_image_render_impl() {
        let mut image: RgbaImage = RgbaImage::new(10, 10);

        // Test that RgbaImage implements Render
        image.clear([128, 128, 128, 255]);
        assert_eq!(image.dimensions(), (10, 10));

        image.set_pixel(0, 0, [255, 255, 255, 255]);
        assert_eq!(image.get_pixel(0, 0), &Rgba([255, 255, 255, 255]));
    }

    #[test]
    fn test_image_render_as_ref() {
        let renderer = ImageRender::new(256, 240);

        // Test AsRef
        let image_ref = renderer.as_ref();
        assert_eq!(image_ref.dimensions(), (256, 240));
    }

    #[test]
    fn test_nes_dimensions() {
        // Test creating a NES-sized render buffer
        let mut renderer = ImageRender::new(256, 240);
        assert_eq!(renderer.dimensions(), (256, 240));

        // Verify we can set pixels across the entire screen
        renderer.clear([0, 0, 0, 0]);
        renderer.set_pixel(0, 0, [255, 255, 255, 255]);
        renderer.set_pixel(255, 239, [255, 255, 255, 255]);
        renderer.set_pixel(128, 120, [128, 128, 128, 255]);
    }
}
