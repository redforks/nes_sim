//! Abstract rendering interface for PPU output
//!
//! This module defines a trait-based rendering system that allows the PPU to output
//! to different backends such as image buffers, markdown debug logs, SDL2 textures,
//! WGPU surfaces, and more.

pub mod image_render;
pub use image_render::ImageRender;
// MarkdownRender and CompositeRender removed; ImageRender is the primary render target.

use std::fmt::Debug;

/// Abstract render target for PPU output
///
/// This trait defines the interface that all renderers must implement. The PPU
/// will call these methods to render each frame, allowing different backends
/// to handle the output in their own way (image, canvas, markdown, etc.).
///
/// # Required Methods
///
/// - `clear(color)` - Fill the entire render target with a solid color
/// - `set_pixel(x, y, color)` - Set a single pixel to the given RGBA color
///
/// # Optional Methods
///
/// - `finish()` - Called after all pixels for a frame have been set
///
/// # Example
///
/// ```rust
/// use nes_core::render::Render;
///
/// struct MyRenderer {
///     pixels: Vec<[u8; 4]>,
///     width: u32,
///     height: u32,
/// }
///
/// impl Render for MyRenderer {
///     fn clear(&mut self, color: [u8; 4]) {
///         self.pixels.fill(color);
///     }
///
///     fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
///         let idx = (y * self.width + x) as usize;
///         self.pixels[idx] = color;
///     }
///
///     // Implementations may expose their own accessors for dimensions if
///     // consumers need them (for example, ImageRender::borrow_image()).
/// }
/// ```
pub trait Render: Debug {
    /// Clear the entire render target with a solid color
    ///
    /// # Parameters
    /// - `color`: RGBA color as [R, G, B, A] where each value is 0-255
    fn clear(&mut self, color: [u8; 4]);

    /// Set a single pixel to the given color
    ///
    /// # Parameters
    /// - `x`: X coordinate (0 = left)
    /// - `y`: Y coordinate (0 = top)
    /// - `color`: RGBA color as [R, G, B, A] where each value is 0-255
    ///
    /// # Behavior
    /// Implementations may choose to ignore out-of-bounds coordinates
    /// rather than panic, for performance reasons.
    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]);

    /// Called after rendering a complete frame
    ///
    /// This method is called once after all `set_pixel()` calls for a frame
    /// are complete. Implementations can use this for any post-processing
    /// or finalization steps.
    ///
    /// The default implementation does nothing.
    fn finish(&mut self) {}
}

impl Render for () {
    fn clear(&mut self, _color: [u8; 4]) {}

    fn set_pixel(&mut self, _x: u32, _y: u32, _color: [u8; 4]) {}
}

impl<R> Render for Box<R>
where
    R: Render + ?Sized,
{
    fn clear(&mut self, color: [u8; 4]) {
        (**self).clear(color);
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        (**self).set_pixel(x, y, color);
    }

    fn finish(&mut self) {
        (**self).finish();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    fn test_x_out_of_bounds() {
        // This test verifies that out-of-bounds coordinates panic
        // Note: Individual implementations may choose to ignore bounds instead
        let mut renderer = crate::render::ImageRender::new(10, 10);
        renderer.set_pixel(999, 5, [0, 0, 0, 0]); // Should panic
    }

    #[test]
    #[should_panic]
    fn test_y_out_of_bounds() {
        // This test verifies that out-of-bounds coordinates panic
        let mut renderer = crate::render::ImageRender::new(10, 10);
        renderer.set_pixel(5, 999, [0, 0, 0, 0]); // Should panic
    }
}
