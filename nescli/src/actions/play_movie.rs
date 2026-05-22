use anyhow::Result;
use nes_core::{
    EmptyPlugin, Plugin,
    ines::INesFile,
    movie::{self, GamepadInput},
    nes::{NesMcu, apu::AudioDriver, controller::Button},
    nes_machine::NesMachine,
    render::{ImageRender, Render},
};
use nes_sdl::{
    self, AUDIO_CHUNK_SAMPLES, AUDIO_SAMPLE_RATE, SdlAudioDriver, SdlRender,
    sdl2::{Sdl, audio::AudioSpecDesired, event::Event, keyboard::Keycode},
};
use std::{
    fs,
    path::PathBuf,
    time::{Duration, Instant},
};

fn create_sdl_drivers(sdl_context: &Sdl) -> Result<(impl Render, impl AudioDriver)> {
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

    let window = video_subsystem
        .window("NES Movie Playback", 512, 480)
        .position_centered()
        .build()
        .map_err(|e| anyhow::anyhow!(e))?;

    let canvas = window
        .into_canvas()
        .build()
        .map_err(|e| anyhow::anyhow!(e))?;

    let image_render = ImageRender::default_dimension();
    let sdl_render = SdlRender::with_image(image_render, canvas);
    Ok((sdl_render, audio_driver))
}

fn set_buttons_from_gamepad<P, R, D>(machine: &mut NesMachine<P, R, D>, input: &GamepadInput)
where
    P: Plugin<NesMcu<R, D>>,
    R: Render,
    D: AudioDriver,
{
    for (button, pressed) in [
        (Button::A, input.a()),
        (Button::B, input.b()),
        (Button::Select, input.select()),
        (Button::Start, input.start()),
        (Button::Up, input.up()),
        (Button::Down, input.down()),
        (Button::Left, input.left()),
        (Button::Right, input.right()),
    ] {
        if pressed {
            if button == Button::Start {
                eprintln!("press {:?}", button);
            }
            machine.press_button(button);
        } else {
            machine.release_button(button);
        }
    }
}

#[derive(clap::Args)]
pub struct PlayMovieAction {
    fm2_file: PathBuf,
}

impl PlayMovieAction {
    pub fn run(&self, f: &INesFile) -> Result<()> {
        let fm2_data = fs::read(&self.fm2_file)?;
        let fm2 = movie::parse(&fm2_data)?;
        let input_logs = &fm2.input_logs;

        if input_logs.is_empty() {
            anyhow::bail!("FM2 file has no input logs");
        }
        eprintln!(">> Loaded movie with {} frames", input_logs.len());

        let sdl_context = nes_sdl::sdl2::init().map_err(|e| anyhow::anyhow!(e))?;
        let (render, audio_driver) = create_sdl_drivers(&sdl_context)?;

        let mut machine = NesMachine::new(f, EmptyPlugin::new(), render, audio_driver);

        let target_frame_duration = Duration::new(0, 1_000_000_000u32 / 60);

        let mut frame_index = 0;

        'running: loop {
            let frame_start = Instant::now();

            if frame_index < input_logs.len() {
                set_buttons_from_gamepad(&mut machine, &input_logs[frame_index].port0);
            }

            match machine.process_frame() {
                nes_core::ExecuteResult::Continue => {}
                nes_core::ExecuteResult::ShouldReset => {
                    eprintln!("CPU reset requested");
                    machine.reset();
                    frame_index = 0;
                }
                nes_core::ExecuteResult::Stop(code) => {
                    eprintln!("Execution stopped with code {}", code);
                    break 'running;
                }
                nes_core::ExecuteResult::Halt => {
                    eprintln!("CPU halt, possible because of an invalid instruction");
                    break 'running;
                }
            }

            frame_index += 1;

            if frame_index >= input_logs.len() {
                eprintln!(">> End of movie reached");
                break 'running;
            }

            let elapsed = frame_start.elapsed();
            if elapsed < target_frame_duration {
                std::thread::sleep(target_frame_duration - elapsed);
            }
        }

        Ok(())
    }
}
