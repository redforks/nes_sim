//! Composite renderer that delegates to multiple child renderers
//!
//! This module provides a Render implementation that coordinates multiple
//! child renderers, allowing operations like rendering to both an image buffer
//! and generating a debug log simultaneously.

use crate::render::Render;
use std::fmt::Debug;

/// Composite renderer that delegates operations to multiple child renderers
///
/// This allows you to combine multiple renderers, for example:
/// - Render to an image for display
/// - Generate markdown debug output
/// - Log to console
/// - Save to file
///
/// # Example
///
/// ```ignore
/// let mut composite = CompositeRender::new();
/// composite.add(Box::new(ImageRender::new(256, 240)));
/// composite.add(Box::new(MarkdownRender::new(0, 256, 240)));
///
/// // All operations are delegated to both renderers
/// composite.clear([0, 0, 0, 255]);
/// composite.set_pixel(100, 100, [255, 0, 0, 255]);
/// composite.finish();
/// ```
/// CompositeRender now wraps exactly two render targets.
///
/// This type is generic over the two concrete renderer types. It delegates
/// all Render calls to both children in the same order. The previous
/// Vec<Box<dyn Render>>-based API has been removed: callers must now pass
/// the two renderers to `new` and there are no add/remove operations.
#[derive(Debug)]
pub struct CompositeRender<A, B>
where
    A: Render,
    B: Render,
{
    a: A,
    b: B,
}

impl<A, B> CompositeRender<A, B>
where
    A: Render,
    B: Render,
{
    /// Create a new composite renderer from two render targets
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<A, B> Render for CompositeRender<A, B>
where
    A: Render,
    B: Render,
{
    fn clear(&mut self, color: [u8; 4]) {
        self.a.clear(color);
        self.b.clear(color);
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        self.a.set_pixel(x, y, color);
        self.b.set_pixel(x, y, color);
    }

    fn dimensions(&self) -> (u32, u32) {
        // Prefer the first renderer's dimensions. It's the caller's
        // responsibility to ensure these are compatible.
        self.a.dimensions()
    }

    fn finish(&mut self) {
        self.a.finish();
        self.b.finish();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::render::image_render::ImageRender;
    use crate::render::markdown_render::MarkdownRender;

    #[test]
    fn test_composite_basic_ops() {
        let image = ImageRender::new(10, 10);
        let image_clone = image.clone();
        let markdown = MarkdownRender::new(0, 10, 10);

        let mut composite = CompositeRender::new(image, markdown);

        // Basic operations should not panic and should affect the image buffer
        composite.clear([1, 2, 3, 4]);
        composite.set_pixel(5, 5, [9, 9, 9, 9]);
        composite.finish();

        // The underlying shared image should reflect the clear and pixel ops
        let img = image_clone.borrow_image();
        assert_eq!(img.get_pixel(0, 0)[0], 1);
        assert_eq!(img.get_pixel(5, 5)[0], 9);
    }

    #[test]
    fn test_composite_dimensions() {
        let image = ImageRender::new(20, 15);
        let markdown = MarkdownRender::new(0, 20, 15);
        let composite = CompositeRender::new(image, markdown);
        assert_eq!(composite.dimensions(), (20, 15));
    }
}
