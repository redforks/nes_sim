use crate::sdl::{
    INTERACTIVE_WINDOW, SdlRender, create_sdl_drivers, map_keycode_to_button,
    sdl2::{event::Event, keyboard::Keycode},
};
use anyhow::Result;
use nes_core::{
    EmptyPlugin,
    ines::INesFile,
    nes::{apu::AudioDriver, ppu::palette::ColorTheme},
    nes_machine::NesMachine,
    render::{ImageRender, Render},
};
use std::time::{Duration, Instant};

fn create_interactive_drivers(
    sdl_context: &crate::sdl::sdl2::Sdl,
) -> Result<(impl Render, impl AudioDriver)> {
    let (canvas, audio_driver) = create_sdl_drivers(sdl_context, &INTERACTIVE_WINDOW, false)?;
    // Interactive mode is always windowed, so the factory always returns a canvas.
    let canvas = canvas.expect("windowed SDL setup must return a canvas");

    // Create shared image buffer so we can save screenshots from the event loop
    let image_render = ImageRender::default_dimension();
    let sdl_render = SdlRender::with_image(image_render, canvas);
    Ok((sdl_render, audio_driver))
}

#[derive(clap::Args)]
pub struct RunAction;

impl RunAction {
    pub fn run(&self, f: &INesFile, palette: Option<ColorTheme>) -> Result<()> {
        // Initialize SDL2
        let sdl_context = crate::sdl::sdl2::init().map_err(|e| anyhow::anyhow!(e))?;
        let (render, audio_driver) = create_interactive_drivers(&sdl_context)?;

        // Create NES machine with SDL-backed renderer
        let mut machine = NesMachine::new(f, EmptyPlugin::new(), render, audio_driver);
        if let Some(theme) = palette {
            machine.set_color_theme(theme);
        }

        // Initialize event pump
        let mut event_pump = sdl_context.event_pump().map_err(|e| anyhow::anyhow!(e))?;
        let target_frame_duration = Duration::new(0, 1_000_000_000u32 / 60);

        'running: loop {
            let frame_start = Instant::now();
            // Handle events
            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        repeat: false,
                        ..
                    } => {
                        break 'running;
                    }
                    // F1 markdown dump removed
                    Event::KeyDown {
                        keycode: Some(Keycode::F2),
                        repeat: false,
                        ..
                    } => {
                        // Reset the simulator
                        eprintln!(">> Resetting simulator");
                        machine.soft_reset();
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::F4),
                        repeat: false,
                        ..
                    } => {
                        // Quit the application
                        eprintln!(">> Quitting application");
                        break 'running;
                    }
                    Event::KeyDown {
                        keycode: Some(keycode),
                        repeat: false,
                        ..
                    } => {
                        if let Some(button) = map_keycode_to_button(keycode) {
                            machine.press_controller_a(button);
                        }
                    }
                    Event::KeyUp {
                        keycode: Some(keycode),
                        repeat: false,
                        ..
                    } => {
                        if let Some(button) = map_keycode_to_button(keycode) {
                            machine.release_controller_a(button);
                        }
                    }
                    _ => {}
                }
            }

            // Run one frame; the renderer presents from `finish()`.
            match machine.process_frame() {
                nes_core::ExecuteResult::Continue => {}
                nes_core::ExecuteResult::ShouldReset => {
                    eprintln!("CPU reset requested");
                    machine.soft_reset();
                }
                nes_core::ExecuteResult::Stop(code) => {
                    eprintln!("Execution stopped with code {}", code);
                    break 'running;
                }
                nes_core::ExecuteResult::Halt => {
                    eprintln!("Cpu halt, possible because of an invalid instruction");
                    break 'running;
                }
            }

            // Frame rate limiting: sleep only for the remaining time to maintain 60 FPS
            let elapsed = frame_start.elapsed();
            if elapsed < target_frame_duration {
                std::thread::sleep(target_frame_duration - elapsed);
            }
        }

        Ok(())
    }
}
