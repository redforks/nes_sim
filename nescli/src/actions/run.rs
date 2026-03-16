use anyhow::Result;
use image::EncodableLayout;
use nes_core::ines::INesFile;
use nes_core::machine::Machine;
use nes_core::render::{CompositeRender, ImageRender, MarkdownRender, Render};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use std::cell::RefCell;
use std::rc::Rc;
use std::time::Duration;

/// Wrapper around Rc<RefCell<CompositeRender>> that implements Render
#[derive(Debug)]
struct SharedCompositeRender {
    inner: Rc<RefCell<CompositeRender>>,
}

impl SharedCompositeRender {
    fn new(inner: Rc<RefCell<CompositeRender>>) -> Self {
        Self { inner }
    }
}

impl Render for SharedCompositeRender {
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

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
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
        let image_render = ImageRender::new(256, 240);

        // Create composite renderer with image + markdown
        let composite = Rc::new(RefCell::new(CompositeRender::new()));

        // Add image renderer (ImageRender is Clone and shares internal Rc)
        composite.borrow_mut().add(Box::new(image_render.clone()));

        // Create and add markdown renderer (disabled by default)
        let markdown_render = MarkdownRender::new(0, 256, 240);
        composite.borrow_mut().add(Box::new(markdown_render));

        // Wrap composite for sharing
        let shared_composite = SharedCompositeRender::new(composite.clone());

        // Create NES machine with composite renderer
        let mcu = nes_core::nes::create_mcu_with_renderer(f, Some(Box::new(shared_composite)));
        let mut machine = Machine::new(Box::new(mcu));

        // Initialize event pump
        let mut event_pump = sdl_context.event_pump().map_err(|e| anyhow::anyhow!(e))?;
        let mut frame_count = 0;

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
                        if let Some(md) = composite.borrow_mut().get_mut(1) {
                            if let Some(md_render) =
                                md.as_any_mut().downcast_mut::<MarkdownRender>()
                            {
                                let enabled = !md_render.is_enabled();
                                md_render.set_enabled(enabled);

                                if enabled {
                                    eprintln!(
                                        ">> Markdown debug ENABLED - dumping all frames to /tmp/nes_frame_*.md"
                                    );
                                } else {
                                    eprintln!(">> Markdown debug DISABLED");
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            // Run one frame (16.67ms for 60 FPS)
            let result = machine.process_frame(16.67);

            // Increment markdown frame number
            if let Some(md) = composite.borrow_mut().get_mut(1) {
                if let Some(md_render) = md.as_any_mut().downcast_mut::<MarkdownRender>() {
                    md_render.increment_frame();
                }
            }

            // Get the rendered image directly from our image_render
            // Create a scope to keep the borrow alive
            {
                let img = image_render.borrow_image();

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
}
