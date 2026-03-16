//! Markdown-based debug renderer that logs render operations
//!
//! This module provides a Render implementation that logs all rendering operations
//! to a markdown document. This is useful for debugging PPU behavior and understanding
//! how frames are rendered.

use crate::render::Render;
use std::fmt::Debug;
use std::time::Instant;

/// A single pixel operation record
#[derive(Debug, Clone)]
struct PixelOp {
    x: u32,
    y: u32,
    color: [u8; 4],
}

/// A clear operation record
#[derive(Debug, Clone)]
struct ClearOp {
    color: [u8; 4],
    width: u32,
    height: u32,
}

/// Operations that can be logged
#[derive(Debug, Clone)]
enum RenderOp {
    Clear(ClearOp),
    Pixel(PixelOp),
}

/// Markdown debug renderer that logs render operations
///
/// This renderer buffers all render operations and generates a markdown document
/// when `finish()` is called and `dump_next_frame` is true.
#[derive(Debug)]
pub struct MarkdownRender {
    /// Frame number for this render
    frame_number: u64,

    /// Dimensions of the render target
    width: u32,
    height: u32,

    /// Buffer of render operations
    operations: Vec<RenderOp>,

    /// Start time for render duration tracking
    start_time: Instant,

    /// Counter for total pixel operations (for statistics)
    pixel_counter: usize,

    /// Whether to dump the next frame
    /// When true, finish() will export the file and reset this to false
    dump_next_frame: bool,
}

impl MarkdownRender {
    /// Create a new MarkdownRender for the given frame
    ///
    /// # Parameters
    /// - `frame_number`: The frame number being rendered
    /// - `width`: Width in pixels (typically 256)
    /// - `height`: Height in pixels (typically 240)
    pub fn new(frame_number: u64, width: u32, height: u32) -> Self {
        Self {
            frame_number,
            width,
            height,
            operations: Vec::new(),
            start_time: Instant::now(),
            pixel_counter: 0,
            dump_next_frame: false,
        }
    }

    /// Get the frame number
    pub fn frame_number(&self) -> u64 {
        self.frame_number
    }

    /// Request that the next frame be dumped to markdown
    ///
    /// This will cause the next call to `finish()` to export the markdown file,
    /// after which the flag is automatically reset to false.
    pub fn request_dump(&mut self) {
        self.dump_next_frame = true;
    }

    /// Check if the next frame should be dumped
    pub fn should_dump(&self) -> bool {
        self.dump_next_frame
    }

    /// Generate the markdown document
    fn generate_markdown(&self) -> String {
        let mut md = String::new();

        // Header
        md.push_str(&format!(
            "# NES Frame Render Log - Frame {}\n\n",
            self.frame_number
        ));

        // Timestamp
        md.push_str(&format!(
            "**Generated**: {:?}\n\n",
            self.start_time.elapsed()
        ));

        // Frame info
        md.push_str("## Frame Info\n\n");
        md.push_str(&format!(
            "- **Dimensions**: {}×{}\n",
            self.width, self.height
        ));
        md.push_str(&format!(
            "- **Total pixel operations**: {}\n\n",
            self.pixel_counter
        ));

        // Render Operations
        md.push_str("## Render Operations\n\n");

        let mut clear_count = 0;
        for op in &self.operations {
            match op {
                RenderOp::Clear(clear) => {
                    clear_count += 1;
                    md.push_str("### Clear Operation\n\n");
                    md.push_str(&format!(
                        "- **Color**: [R={}, G={}, B={}, A={}]\n",
                        clear.color[0], clear.color[1], clear.color[2], clear.color[3]
                    ));
                    md.push_str(&format!(
                        "- **Dimensions**: {}×{}\n\n",
                        clear.width, clear.height
                    ));
                }
                RenderOp::Pixel(pixel) => {
                    md.push_str(&format!(
                        "- `set_pixel({}, {}, [R={}, G={}, B={}, A={}])`\n",
                        pixel.x,
                        pixel.y,
                        pixel.color[0],
                        pixel.color[1],
                        pixel.color[2],
                        pixel.color[3]
                    ));
                }
            }
        }

        // Statistics
        md.push_str("\n## Statistics\n\n");
        md.push_str(&format!("- **Clear operations**: {}\n", clear_count));
        md.push_str(&format!(
            "- **Total pixel operations**: {}\n",
            self.pixel_counter
        ));

        // Calculate unique colors
        let mut colors = std::collections::HashSet::new();
        for op in &self.operations {
            if let RenderOp::Pixel(pixel) = op {
                colors.insert(pixel.color);
            }
        }
        md.push_str(&format!("- **Unique colors**: {}\n", colors.len()));

        // Render duration
        md.push_str(&format!(
            "- **Render time**: {:.2}ms\n",
            self.start_time.elapsed().as_secs_f64() * 1000.0
        ));

        md.push_str("\n---\n");
        md.push_str("*End of render log*\n");

        md
    }

    /// Get the markdown output as a string
    ///
    /// This returns the generated markdown document.
    pub fn get_markdown(&self) -> String {
        self.generate_markdown()
    }

    /// Save the markdown to a file
    ///
    /// # Parameters
    /// - `path`: File path to save to
    pub fn save_to_file(&self, path: &std::path::Path) -> std::io::Result<()> {
        std::fs::write(path, self.generate_markdown())
    }
}

impl Render for MarkdownRender {
    fn clear(&mut self, color: [u8; 4]) {
        self.operations.push(RenderOp::Clear(ClearOp {
            color,
            width: self.width,
            height: self.height,
        }));
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        self.pixel_counter += 1;
        self.operations
            .push(RenderOp::Pixel(PixelOp { x, y, color }));
    }

    fn dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    fn finish(&mut self) {
        // Auto-export markdown if dump is requested
        if self.dump_next_frame {
            let path = format!("/tmp/nes_frame_{:04}.md", self.frame_number);
            match self.save_to_file(std::path::Path::new(&path)) {
                Ok(()) => {
                    eprintln!(">> Dumped frame {} to {}", self.frame_number, path);
                }
                Err(e) => {
                    eprintln!("Failed to save markdown frame: {}", e);
                }
            }
        }

        // Reset internal state for next frame
        self.dump_next_frame = false;
        self.operations.clear();
        self.pixel_counter = 0;
        self.frame_number += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_markdown_render_new() {
        let renderer = MarkdownRender::new(0, 256, 240);
        assert_eq!(renderer.dimensions(), (256, 240));
        assert_eq!(renderer.frame_number(), 0);
    }

    #[test]
    fn test_markdown_render_clear() {
        let mut renderer = MarkdownRender::new(1, 10, 10);
        renderer.clear([255, 0, 0, 255]);

        let md = renderer.get_markdown();
        assert!(md.contains("Clear Operation"));
        assert!(md.contains("R=255"));
        // Markdown uses **Dimensions** for bold, so the pattern includes the asterisks
        assert!(md.contains("Dimensions**: 10×10"));
    }

    #[test]
    fn test_markdown_render_set_pixel() {
        let mut renderer = MarkdownRender::new(2, 10, 10);

        // Set some pixels
        renderer.set_pixel(0, 0, [255, 0, 0, 255]);
        renderer.set_pixel(5, 5, [0, 255, 0, 255]);
        renderer.set_pixel(9, 9, [0, 0, 255, 255]);

        let md = renderer.get_markdown();
        assert!(md.contains("set_pixel(0, 0"));
        assert!(md.contains("set_pixel(5, 5"));
        assert!(md.contains("set_pixel(9, 9"));
    }

    #[test]
    fn test_markdown_render_statistics() {
        let mut renderer = MarkdownRender::new(5, 10, 10);

        renderer.clear([0, 0, 0, 255]);
        renderer.set_pixel(0, 0, [255, 0, 0, 255]);
        renderer.set_pixel(1, 0, [255, 0, 0, 255]);
        renderer.set_pixel(2, 0, [0, 255, 0, 255]);

        // Get markdown before finish() since finish() clears state
        let md = renderer.get_markdown();
        assert!(md.contains("Statistics"));
        // Markdown uses ** for bold formatting
        assert!(md.contains("Clear operations**: 1"));
        assert!(md.contains("Total pixel operations**: 3"));
        assert!(md.contains("Unique colors**: 2"));

        renderer.finish();
    }

    #[test]
    fn test_markdown_render_save_to_file() {
        let mut renderer = MarkdownRender::new(6, 10, 10);
        renderer.set_pixel(5, 5, [128, 128, 128, 255]);

        let temp_path = std::path::Path::new("/tmp/test_nes_render.md");
        let result = renderer.save_to_file(temp_path);

        assert!(result.is_ok());
        assert!(temp_path.exists());

        // Clean up
        std::fs::remove_file(temp_path).ok();
    }

    #[test]
    fn test_markdown_render_full_frame() {
        // Simulate a small frame render
        let mut renderer = MarkdownRender::new(7, 4, 4);

        renderer.clear([0, 0, 0, 255]);

        // Draw a simple pattern
        for y in 0..4 {
            for x in 0..4 {
                let color = if (x + y) % 2 == 0 {
                    [255, 255, 255, 255]
                } else {
                    [0, 0, 0, 255]
                };
                renderer.set_pixel(x, y, color);
            }
        }

        // Get markdown before finish() since finish() clears state and increments frame number
        let md = renderer.get_markdown();
        assert!(md.contains("Frame 7"));
        assert!(md.contains("Dimensions**: 4×4"));
        assert!(md.contains("Total pixel operations**: 16"));

        renderer.finish();
    }

    #[test]
    fn test_markdown_render_finish_resets_state() {
        let mut renderer = MarkdownRender::new(10, 10, 10);

        renderer.set_pixel(0, 0, [255, 0, 0, 255]);
        renderer.set_pixel(1, 1, [0, 255, 0, 255]);

        assert_eq!(renderer.frame_number(), 10);

        renderer.finish();

        // State should be reset
        assert_eq!(renderer.frame_number(), 11);
        assert_eq!(renderer.pixel_counter, 0);

        // Operations should be cleared
        let md = renderer.get_markdown();
        assert!(!md.contains("set_pixel"));
    }

    #[test]
    fn test_markdown_render_request_dump() {
        let mut renderer = MarkdownRender::new(20, 10, 10);

        assert!(!renderer.should_dump());

        renderer.request_dump();
        assert!(renderer.should_dump());

        renderer.finish();

        // After finish, dump flag should be reset
        assert!(!renderer.should_dump());
        assert_eq!(renderer.frame_number(), 21);
    }

    #[test]
    fn test_nes_dimensions() {
        // Test creating a NES-sized render buffer
        let mut renderer = MarkdownRender::new(256, 256, 240);
        assert_eq!(renderer.dimensions(), (256, 240));

        // Verify we can set pixels across the entire screen
        renderer.clear([0, 0, 0, 0]);
        renderer.set_pixel(0, 0, [255, 255, 255, 255]);
        renderer.set_pixel(255, 239, [255, 255, 255, 255]);
        renderer.set_pixel(128, 120, [128, 128, 128, 255]);

        let md = renderer.get_markdown();
        assert!(md.contains("Total pixel operations**: 3"));
    }
}
