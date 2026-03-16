use anyhow::Result;
use image::EncodableLayout;
use nes_core::ines::INesFile;
use nes_core::machine::Machine;
use nes_core::render::{CompositeRender, ImageRender, Render};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use std::cell::RefCell;
use std::rc::Rc;
use std::time::Duration;

/// A Render implementation that delegates to a shared Rc<RefCell<ImageRender>>
#[derive(Debug)]
struct SharedRender {
    inner: Rc<RefCell<ImageRender>>,
}

impl SharedRender {
    fn new(inner: Rc<RefCell<ImageRender>>) -> Self {
        Self { inner }
    }
}

impl Render for SharedRender {
    fn clear(&mut self, color: [u8; 4]) {
        self.inner.borrow_mut().clear(color);
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        self.inner.borrow_mut().set_pixel(x, y, color);
    }

    fn dimensions(&self) -> (u32, u32) {
        self.inner.borrow().dimensions()
    }

    fn finish(&mut self) {
        self.inner.borrow_mut().finish();
    }
}

#[derive(clap::Args)]
pub struct RunAction {}

impl RunAction {
    pub fn run(&self, f: &INesFile) -> Result<()> {
        // Initialize SDL2
        let sdl_context = sdl2::init().map_err(|e| anyhow::anyhow!(e))?;
        let video_subsystem = sdl_context.video().map_err(|e| anyhow::anyhow!(e))?;

        // Create window (2x scale for better visibility: 256x240 -> 512x480)
        let window = video_subsystem
            .window(
                "NES Simulator - Press F1 to toggle markdown debug mode",
                512,
                480,
            )
            .position_centered()
            .build()
            .map_err(|e| anyhow::anyhow!(e))?;

        // Create canvas/renderer
        let mut canvas = window
            .into_canvas()
            .build()
            .map_err(|e| anyhow::anyhow!(e))?;

        // Create texture for RGBA8888 pixel format
        let texture_creator = canvas.texture_creator();
        let mut texture = texture_creator
            .create_texture_streaming(PixelFormatEnum::ABGR8888, 256, 240)
            .map_err(|e| anyhow::anyhow!(e))?;

        // Create image renderer for display - keep a reference to access later
        let image_render = Rc::new(RefCell::new(ImageRender::new(256, 240)));

        // Create composite renderer with image + markdown
        let mut composite = CompositeRender::new();

        // Add image renderer via shared wrapper
        composite.add(Box::new(SharedRender::new(image_render.clone())));

        // Add markdown renderer (disabled by default, enabled with F1)
        // We'll create it but won't add it until F1 is pressed
        let markdown_enabled = std::rc::Rc::new(std::cell::Cell::new(false));

        // Create NES machine with composite renderer
        let mcu = nes_core::nes::create_mcu_with_renderer(f, Some(Box::new(composite)));
        let mut machine = Machine::new(Box::new(mcu));

        // Initialize event pump
        let mut event_pump = sdl_context.event_pump().map_err(|e| anyhow::anyhow!(e))?;
        let mut frame_count = 0;
        let mut markdown_frame_number = 0;

        'running: loop {
            // Handle events
            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => {
                        break 'running;
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::F1),
                        ..
                    } => {
                        // Toggle markdown debug mode
                        let enabled = !markdown_enabled.get();
                        markdown_enabled.set(enabled);

                        if enabled {
                            eprintln!(
                                ">> Markdown debug ENABLED - dumping all frames to /tmp/nes_frame_*.md"
                            );
                        } else {
                            eprintln!(">> Markdown debug DISABLED");
                        }
                    }
                    _ => {}
                }
            }

            // Run one frame (16.67ms for 60 FPS)
            let result = machine.process_frame(16.67);

            // Get the rendered image directly from our image_render
            // Create a scope to keep the borrow alive
            {
                let borrowed = image_render.borrow();
                let img = borrowed.as_ref();

                // Save markdown if debug mode is enabled
                if markdown_enabled.get() {
                    Self::save_frame_markdown(markdown_frame_number, img)?;
                    markdown_frame_number += 1;
                }

                // Save first frame for debugging
                if frame_count == 0 {
                    let path = format!("/tmp/nes_frame_0.png");
                    img.save(&path)?;
                    eprintln!("Saved first frame to {}", path);
                }

                // Update texture with frame data
                texture.update(None, img.as_bytes(), 256 * 4)?;
            }

            frame_count += 1;

            // Copy texture to canvas and present
            canvas
                .copy(&texture, None, None)
                .map_err(|e| anyhow::anyhow!(e))?;
            canvas.present();

            // Check if execution should stop
            match result {
                nes_core::ExecuteResult::Continue => {}
                nes_core::ExecuteResult::Stop(code) => {
                    eprintln!("Execution stopped with code {}", code);
                    break 'running;
                }
                nes_core::ExecuteResult::ShouldReset => {
                    eprintln!("CPU reset requested");
                    machine.reset();
                }
            }

            // Frame rate limiting
            std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
        }

        Ok(())
    }

    /// Save a frame as markdown for debugging
    fn save_frame_markdown(frame_number: u64, img: &image::RgbaImage) -> Result<()> {
        let path = format!("/tmp/nes_frame_{:04}.md", frame_number);

        // Create markdown content
        let mut md_content = String::new();
        md_content.push_str(&format!("# NES Frame Dump - Frame {}\n\n", frame_number));
        md_content.push_str(&format!(
            "**Dimensions**: {}×{}\n\n",
            img.width(),
            img.height()
        ));
        md_content.push_str("## Frame Statistics\n\n");
        md_content.push_str(&format!("- Total pixels: {}\n", img.width() * img.height()));

        // Calculate color statistics
        let mut color_counts = std::collections::HashMap::new();
        for pixel in img.pixels() {
            *color_counts.entry(pixel.0).or_insert(0) += 1;
        }
        md_content.push_str(&format!("- Unique colors: {}\n", color_counts.len()));

        // Show most common colors
        md_content.push_str("\n## Top 10 Colors\n\n");
        let mut colors: Vec<_> = color_counts.into_iter().collect();
        colors.sort_by(|a, b| b.1.cmp(&a.1));
        for (i, (color, count)) in colors.iter().take(10).enumerate() {
            let percentage = (*count as f64 / (img.width() * img.height()) as f64) * 100.0;
            md_content.push_str(&format!(
                "{}. [R={},G={},B={},A={}] - {:.1}% ({})\n",
                i + 1,
                color[0],
                color[1],
                color[2],
                color[3],
                percentage,
                count
            ));
        }

        // Sample some pixels from the image (first 5 rows, first 10 pixels per row)
        md_content.push_str("\n## Sample Pixels (first 5×10 area)\n\n");
        md_content.push_str("```\n");
        for y in 0..5.min(img.height()) {
            for x in 0..10.min(img.width()) {
                let pixel = img.get_pixel(x, y);
                md_content.push_str(&format!(
                    "({:2},{:2})=[{:3},{:3},{:3},{:3}] ",
                    x, y, pixel[0], pixel[1], pixel[2], pixel[3]
                ));
            }
            md_content.push('\n');
        }
        md_content.push_str("```\n\n");

        md_content.push_str("---\n");
        md_content.push_str("*End of frame dump*\n");

        std::fs::write(&path, md_content)?;

        // Only print every 60 frames (once per second at 60fps) to avoid spam
        if frame_number % 60 == 0 {
            eprintln!(
                ">> Saved frame {} to {} ({} colors)",
                frame_number,
                path,
                colors.len()
            );
        }

        Ok(())
    }
}
