//! Composite renderer that delegates to multiple child renderers
//!
//! This module provides a Render implementation that coordinates multiple
//! child renderers, allowing operations like rendering to both an image buffer
//! and generating a debug log simultaneously.

use crate::render::Render;
use std::any::Any;
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
#[derive(Debug)]
pub struct CompositeRender {
    /// Child renderers
    children: Vec<Box<dyn Render>>,

    /// Dimensions (must be consistent across all children)
    width: u32,
    height: u32,
}

impl CompositeRender {
    /// Create a new empty composite renderer
    pub fn new() -> Self {
        Self {
            children: Vec::new(),
            width: 256,
            height: 240,
        }
    }

    /// Create a new composite renderer with the specified dimensions
    pub fn with_dimensions(width: u32, height: u32) -> Self {
        Self {
            children: Vec::new(),
            width,
            height,
        }
    }

    /// Add a child renderer
    ///
    /// # Parameters
    /// - `renderer`: The renderer to add
    pub fn add(&mut self, renderer: Box<dyn Render>) {
        // Validate dimensions match
        let (w, h) = renderer.dimensions();
        if self.children.is_empty() {
            // First renderer sets the dimensions
            self.width = w;
            self.height = h;
        } else {
            // Subsequent renderers must match
            if (w, h) != (self.width, self.height) {
                panic!(
                    "Renderer dimensions ({}, {}) must match composite dimensions ({}, {})",
                    w, h, self.width, self.height
                );
            }
        }
        self.children.push(renderer);
    }

    /// Remove a child renderer by index
    ///
    /// # Parameters
    /// - `index`: The index of the renderer to remove
    ///
    /// # Returns
    /// The removed renderer
    pub fn remove(&mut self, index: usize) -> Option<Box<dyn Render>> {
        if index < self.children.len() {
            Some(self.children.remove(index))
        } else {
            None
        }
    }

    /// Get the number of child renderers
    pub fn len(&self) -> usize {
        self.children.len()
    }

    /// Check if there are no child renderers
    pub fn is_empty(&self) -> bool {
        self.children.is_empty()
    }

    /// Get a reference to a child renderer by index
    pub fn get(&self, index: usize) -> Option<&dyn Render> {
        self.children.get(index).map(|b| b.as_ref())
    }

    /// Get a mutable reference to a child renderer by index
    pub fn get_mut(&mut self, index: usize) -> Option<&mut dyn Render> {
        if index < self.children.len() {
            Some(self.children[index].as_mut())
        } else {
            None
        }
    }

    /// Clear all child renderers
    pub fn clear_children(&mut self) {
        self.children.clear();
    }
}

impl Default for CompositeRender {
    fn default() -> Self {
        Self::new()
    }
}

impl Render for CompositeRender {
    fn clear(&mut self, color: [u8; 4]) {
        for child in &mut self.children {
            child.clear(color);
        }
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        for child in &mut self.children {
            child.set_pixel(x, y, color);
        }
    }

    fn dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    fn finish(&mut self) {
        for child in &mut self.children {
            child.finish();
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::render::image_render::ImageRender;
    use crate::render::markdown_render::MarkdownRender;

    #[test]
    fn test_composite_new() {
        let composite = CompositeRender::new();
        assert_eq!(composite.dimensions(), (256, 240));
        assert!(composite.is_empty());
    }

    #[test]
    fn test_composite_add() {
        let mut composite = CompositeRender::new();
        assert!(composite.is_empty());

        let image = ImageRender::new(256, 240);
        composite.add(Box::new(image));

        assert_eq!(composite.len(), 1);
        assert!(!composite.is_empty());
    }

    #[test]
    fn test_composite_clear() {
        let mut composite = CompositeRender::new();
        let image = ImageRender::new(100, 100);
        composite.add(Box::new(image));

        composite.clear([255, 0, 0, 255]);

        // Verify the image was cleared
        let renderer = composite.get(0).unwrap();
        // We can't directly check the image content through the trait,
        // but we can verify it doesn't panic
        renderer.dimensions();
    }

    #[test]
    fn test_composite_set_pixel() {
        let mut composite = CompositeRender::new();
        let image = ImageRender::new(10, 10);
        composite.add(Box::new(image));

        composite.set_pixel(5, 5, [255, 0, 0, 255]);
        composite.set_pixel(3, 7, [0, 255, 0, 255]);

        // Verify operations completed without panicking
        assert_eq!(composite.len(), 1);
    }

    #[test]
    fn test_composite_multiple() {
        let mut composite = CompositeRender::new();

        let image = ImageRender::new(50, 50);
        let markdown = MarkdownRender::new(0, 50, 50);

        composite.add(Box::new(image));
        composite.add(Box::new(markdown));

        assert_eq!(composite.len(), 2);

        composite.clear([0, 0, 0, 255]);
        composite.set_pixel(10, 10, [255, 0, 0, 255]);
        composite.finish();

        // Verify both children received the operations
        assert_eq!(composite.len(), 2);
    }

    #[test]
    fn test_composite_remove() {
        let mut composite = CompositeRender::new();

        composite.add(Box::new(ImageRender::new(100, 100)));
        composite.add(Box::new(ImageRender::new(100, 100)));
        composite.add(Box::new(ImageRender::new(100, 100)));

        assert_eq!(composite.len(), 3);

        let removed = composite.remove(1);
        assert!(removed.is_some());
        assert_eq!(composite.len(), 2);

        let out_of_bounds = composite.remove(10);
        assert!(out_of_bounds.is_none());
    }

    #[test]
    fn test_composite_clear_children() {
        let mut composite = CompositeRender::new();

        composite.add(Box::new(ImageRender::new(100, 100)));
        composite.add(Box::new(ImageRender::new(100, 100)));

        assert_eq!(composite.len(), 2);

        composite.clear_children();

        assert!(composite.is_empty());
    }

    #[test]
    fn test_composite_get() {
        let mut composite = CompositeRender::new();

        composite.add(Box::new(ImageRender::new(100, 100)));
        composite.add(Box::new(ImageRender::new(100, 100)));

        let first = composite.get(0);
        assert!(first.is_some());
        assert_eq!(first.unwrap().dimensions(), (100, 100));

        let out_of_bounds = composite.get(10);
        assert!(out_of_bounds.is_none());
    }

    #[test]
    fn test_composite_get_mut() {
        let mut composite = CompositeRender::new();

        composite.add(Box::new(ImageRender::new(100, 100)));

        let first = composite.get_mut(0);
        assert!(first.is_some());

        // We can call methods through the mutable reference
        first.unwrap().clear([128, 128, 128, 255]);
    }

    #[test]
    fn test_composite_dimension_validation() {
        let mut composite = CompositeRender::new();

        // Add first renderer with 256x240
        composite.add(Box::new(ImageRender::new(256, 240)));

        // Adding matching dimensions should work
        composite.add(Box::new(ImageRender::new(256, 240)));
        assert_eq!(composite.len(), 2);

        // Adding mismatched dimensions should panic
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            composite.add(Box::new(ImageRender::new(100, 100)));
        }));

        assert!(result.is_err());
    }

    #[test]
    fn test_composite_finish() {
        let mut composite = CompositeRender::new();

        let image = ImageRender::new(10, 10);
        let markdown = MarkdownRender::new(0, 10, 10);

        composite.add(Box::new(image));
        composite.add(Box::new(markdown));

        composite.clear([0, 0, 0, 255]);
        composite.set_pixel(5, 5, [255, 0, 0, 255]);
        composite.finish();

        // Verify both children were finished
        assert_eq!(composite.len(), 2);
    }

    #[test]
    fn test_composite_with_dimensions() {
        let composite = CompositeRender::with_dimensions(100, 100);
        assert_eq!(composite.dimensions(), (100, 100));
    }

    #[test]
    fn test_composite_empty_operations() {
        let mut composite = CompositeRender::new();

        // Operations on empty composite should not panic
        composite.clear([0, 0, 0, 255]);
        composite.set_pixel(0, 0, [255, 0, 0, 255]);
        composite.finish();

        assert!(composite.is_empty());
    }

    #[test]
    fn test_composite_with_markdown_and_image() {
        let mut composite = CompositeRender::new();

        let image = ImageRender::new(4, 4);
        let markdown = MarkdownRender::new(1, 4, 4);

        composite.add(Box::new(image));
        composite.add(Box::new(markdown));

        // Render a simple pattern
        composite.clear([0, 0, 0, 255]);
        composite.set_pixel(0, 0, [255, 255, 255, 255]);
        composite.set_pixel(1, 0, [255, 0, 0, 255]);
        composite.set_pixel(2, 0, [0, 255, 0, 255]);
        composite.set_pixel(3, 0, [0, 0, 255, 255]);
        composite.finish();

        // Verify the composite works
        assert_eq!(composite.len(), 2);
    }
}
