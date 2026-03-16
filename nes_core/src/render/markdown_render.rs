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
/// when `finish()` is called. The markdown includes:
/// - Frame metadata (timestamp, frame number)
/// - PPU state information
/// - All render operations (clear, set_pixel)
/// - Statistics (total operations, unique colors, etc.)
#[derive(Debug)]
pub struct MarkdownRender {
    /// Frame number for this render
    frame_number: u64,

    /// Dimensions of the render target
    width: u32,
    height: u32,

    /// Buffer of render operations
    operations: Vec<RenderOp>,

    /// PPU state snapshot (optional)
    ppu_mask: Option<u8>,
    vram_addr: Option<u16>,
    coarse_x: Option<u8>,
    coarse_y: Option<u8>,
    fine_x: Option<u8>,
    fine_y: Option<u8>,

    /// Start time for render duration tracking
    start_time: Instant,

    /// Whether to buffer all operations or sample
    /// If set, only log every Nth pixel operation
    sample_rate: Option<usize>,

    /// Counter for sampling
    pixel_counter: usize,

    /// Sampled pixel count
    pixels_logged: usize,

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
            ppu_mask: None,
            vram_addr: None,
            coarse_x: None,
            coarse_y: None,
            fine_x: None,
            fine_y: None,
            start_time: Instant::now(),
            sample_rate: None,
            pixel_counter: 0,
            pixels_logged: 0,
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

    /// Increment the frame number
    ///
    /// Call this when reusing the renderer for multiple frames.
    pub fn increment_frame(&mut self) {
        self.frame_number += 1;
        // Clear operations for the new frame
        self.operations.clear();
        self.pixel_counter = 0;
        self.pixels_logged = 0;
    }

    /// Reset the frame number
    pub fn set_frame_number(&mut self, frame_number: u64) {
        self.frame_number = frame_number;
    }

    /// Set PPU state information
    ///
    /// Call this before rendering starts to include PPU state in the output.
    pub fn set_ppu_state(
        &mut self,
        ppu_mask: u8,
        vram_addr: u16,
        coarse_x: u8,
        coarse_y: u8,
        fine_x: u8,
        fine_y: u8,
    ) {
        self.ppu_mask = Some(ppu_mask);
        self.vram_addr = Some(vram_addr);
        self.coarse_x = Some(coarse_x);
        self.coarse_y = Some(coarse_y);
        self.fine_x = Some(fine_x);
        self.fine_y = Some(fine_y);
    }

    /// Set sampling rate for pixel operations
    ///
    /// If `sample_rate` is Some(N), only every Nth pixel will be logged.
    /// This is useful for reducing output size and improving performance.
    /// Set to None to log all pixels.
    pub fn set_sample_rate(&mut self, sample_rate: Option<usize>) {
        self.sample_rate = sample_rate;
    }

    /// Get the number of pixel operations logged
    pub fn pixels_logged(&self) -> usize {
        self.pixels_logged
    }

    /// Get the total number of pixel operations (including sampled out)
    pub fn total_pixels(&self) -> usize {
        self.pixel_counter
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

        // PPU State
        md.push_str("## PPU State\n\n");
        if let Some(mask) = self.ppu_mask {
            md.push_str(&format!("- **PPUMASK**: 0x{:02X}\n", mask));
        }
        if let Some(addr) = self.vram_addr {
            md.push_str(&format!("- **VRAM Address**: 0x{:04X}\n", addr));
        }
        if let (Some(cx), Some(cy), Some(fx), Some(fy)) =
            (self.coarse_x, self.coarse_y, self.fine_x, self.fine_y)
        {
            md.push_str(&format!(
                "- **Scroll**: coarse_x={} coarse_y={} fine_x={} fine_y={}\n",
                cx, cy, fx, fy
            ));
        }
        md.push_str(&format!(
            "- **Dimensions**: {}×{}\n\n",
            self.width, self.height
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
        md.push_str(&format!("- **Pixels logged**: {}\n", self.pixels_logged));

        if let Some(rate) = self.sample_rate {
            if rate > 1 {
                md.push_str(&format!(
                    "- **Sample rate**: 1:{} (every {}th pixel)\n",
                    rate, rate
                ));
            }
        }

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
    /// This returns the generated markdown document. Call this after `finish()`.
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

        // Check sampling
        if let Some(rate) = self.sample_rate {
            if rate > 1 && (self.pixel_counter - 1) % rate != 0 {
                return; // Skip this pixel
            }
        }

        self.operations
            .push(RenderOp::Pixel(PixelOp { x, y, color }));
        self.pixels_logged += 1;
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
        self.pixels_logged = 0;
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
        let mut renderer = MarkdownRender::new(1, 100, 100);
        renderer.clear([255, 0, 0, 255]);

        let md = renderer.get_markdown();
        assert!(md.contains("Clear Operation"));
        assert!(md.contains("R=255"));
        // Markdown uses **Dimensions** for bold, so the pattern includes the asterisks
        assert!(md.contains("Dimensions**: 100×100"));
    }

    #[test]
    fn test_markdown_render_set_pixel() {
        let mut renderer = MarkdownRender::new(2, 10, 10);
        renderer.set_pixel(0, 0, [255, 0, 0, 255]);
        renderer.set_pixel(5, 5, [0, 255, 0, 255]);

        assert_eq!(renderer.pixels_logged(), 2);
        assert_eq!(renderer.total_pixels(), 2);

        let md = renderer.get_markdown();
        assert!(md.contains("set_pixel(0, 0"));
        assert!(md.contains("set_pixel(5, 5"));
    }

    #[test]
    fn test_markdown_render_ppu_state() {
        let mut renderer = MarkdownRender::new(3, 256, 240);
        renderer.set_ppu_state(0x1E, 0x2000, 0, 0, 0, 0);

        let md = renderer.get_markdown();
        assert!(md.contains("PPUMASK"));
        assert!(md.contains("0x1E"));
        assert!(md.contains("VRAM Address"));
        assert!(md.contains("0x2000"));
    }

    #[test]
    fn test_markdown_render_sampling() {
        let mut renderer = MarkdownRender::new(4, 10, 10);
        renderer.set_sample_rate(Some(10)); // Log every 10th pixel

        for y in 0..10 {
            for x in 0..10 {
                renderer.set_pixel(x, y, [128, 128, 128, 255]);
            }
        }

        assert_eq!(renderer.total_pixels(), 100);
        assert_eq!(renderer.pixels_logged(), 10);

        let md = renderer.get_markdown();
        // Markdown uses **Sample rate** for bold
        assert!(md.contains("Sample rate**: 1:10"));
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
}
