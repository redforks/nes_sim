use anyhow::Result;
use nes_core::EmptyPlugin;
use nes_core::ines::INesFile;
use nes_core::nes_machine::NesMachine;
use nes_core::render::ImageRender;
use nes_sdl;
use nes_sdl::sdl2::audio::AudioSpecDesired;
use nes_sdl::sdl2::event::Event;
use nes_sdl::sdl2::keyboard::Keycode;
use nes_sdl::{
    AUDIO_CHUNK_SAMPLES, AUDIO_SAMPLE_RATE, SdlAudioDriver, SdlRender, map_keycode_to_button,
};
use std::time::{Duration, Instant};

#[derive(clap::Args)]
pub struct RunAction {}

impl RunAction {
    pub fn run(&self, f: &INesFile) -> Result<()> {
        // Initialize SDL2
        let sdl_context = nes_sdl::sdl2::init().map_err(|e| anyhow::anyhow!(e))?;
        let video_subsystem = sdl_context.video().map_err(|e| anyhow::anyhow!(e))?;
        let audio_subsystem = sdl_context.audio().map_err(|e| anyhow::anyhow!(e))?;

        let audio_spec = AudioSpecDesired {
            freq: Some(AUDIO_SAMPLE_RATE),
            channels: Some(1),
            samples: Some(AUDIO_CHUNK_SAMPLES as u16),
        };
        let audio_queue = audio_subsystem
            .open_queue::<f32, _>(None, &audio_spec)
            .map_err(|e| anyhow::anyhow!(e))?;
        audio_queue.resume();
        let audio_driver = SdlAudioDriver::new(audio_queue, AUDIO_SAMPLE_RATE as u32);

        // Create window (2x scale for better visibility: 256x240 -> 512x480)
        let window = video_subsystem
            .window(
                "NES Simulator - Arrows move, Z/X A/B, Space Select, Enter Start",
                512,
                480,
            )
            .position_centered()
            .build()
            .map_err(|e| anyhow::anyhow!(e))?;

        // Create canvas/renderer
        let canvas = window
            .into_canvas()
            .build()
            .map_err(|e| anyhow::anyhow!(e))?;

        // Create shared image buffer so we can save screenshots from the event loop
        let image_render = ImageRender::default_dimension();
        let sdl_render = SdlRender::with_image(image_render, canvas);

        // Create NES machine with SDL-backed renderer
        let mut machine = NesMachine::new(f, EmptyPlugin::new(), sdl_render, audio_driver);

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
                        machine.reset();
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::F1),
                        repeat: false,
                        ..
                    } => match machine.dump_ppu_state_to_file() {
                        Ok(path) => eprintln!(">> Dumped PPU state to {}", path),
                        Err(err) => eprintln!(">> Failed to dump PPU state: {}", err),
                    },
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
                            machine.press_button(button);
                        }
                    }
                    Event::KeyUp {
                        keycode: Some(keycode),
                        repeat: false,
                        ..
                    } => {
                        if let Some(button) = map_keycode_to_button(keycode) {
                            machine.release_button(button);
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
                    machine.reset();
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
