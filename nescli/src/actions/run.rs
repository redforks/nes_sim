use anyhow::Result;
use image::EncodableLayout;
use nes_core::ines::INesFile;
use nes_core::machine::Machine;
use nes_core::nes::nes_mcu;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use std::time::Duration;

#[derive(clap::Args)]
pub struct RunAction {}

impl RunAction {
    pub fn run(&self, f: &INesFile) -> Result<()> {
        // Initialize SDL2
        let sdl_context = sdl2::init().map_err(|e| anyhow::anyhow!(e))?;
        let video_subsystem = sdl_context.video().map_err(|e| anyhow::anyhow!(e))?;

        // Create window (2x scale for better visibility: 256x240 -> 512x480)
        let window = video_subsystem
            .window("NES Simulator", 512, 480)
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

        // Create NES machine
        let mcu = nes_mcu::build(f);
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
                    _ => {}
                }
            }

            // Run one frame (16.67ms for 60 FPS)
            let (_img, result) = machine.process_frame(16.67);

            // Save first frame for debugging
            if frame_count == 0 {
                _img.save("/tmp/nes_frame_0.png")?;
                eprintln!("Saved first frame to /tmp/nes_frame_0.png");
            }
            frame_count += 1;

            // Update texture with frame data
            texture.update(None, _img.as_bytes(), 256 * 4)?;

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
