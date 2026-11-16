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
/// - `set_pixel(index, color)` - Set a single pixel to the given RGBA color
///
/// # Optional Methods
///
/// - `finish()` - Called after all pixels for a frame have been set
pub trait Render: Debug {
    /// Set a single pixel to the given color
    ///
    /// # Parameters
    /// - `index`: Linear pixel index in row-major order (0 = top-left),
    ///   where `x = index % 256` and `y = index / 256` for the NES frame.
    /// - `color`: RGBA color as [R, G, B, A] where each value is 0-255
    ///
    /// # Behavior
    /// Implementations may choose to ignore out-of-bounds indices
    /// rather than panic, for performance reasons.
    fn set_pixel(&mut self, index: u16, color: [u8; 4]);

    /// Called after rendering a complete frame
    ///
    /// This method is called once after all `set_pixel()` calls for a frame
    /// are complete. Implementations can use this for any post-processing
    /// or finalization steps.
    ///
    /// The default implementation does nothing.
    fn finish(&mut self) {}

    /// R+G+B luminance of the pixel at (x, y), for light-gun sensing.
    ///
    /// Out-of-bounds coordinates and backends without a readable
    /// framebuffer report 0 (black). The default suits such backends and
    /// keeps non-image renderers compiling unchanged.
    fn pixel_brightness(&self, _x: u32, _y: u32) -> u32 {
        0
    }
}

impl Render for () {
    fn set_pixel(&mut self, _index: u16, _color: [u8; 4]) {}
}
