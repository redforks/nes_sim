use crate::sdl::{
    INTERACTIVE_WINDOW, InputSource, LoopOptions, NesRender, SdlAudioDriver, create_sdl_drivers,
    run_event_loop,
};
use anyhow::Result;
use nes_core::{
    EmptyPlugin, ines::INesFile, nes::ppu::palette::ColorTheme, nes_machine::NesMachine,
};

fn create_interactive_drivers(
    sdl_context: &crate::sdl::sdl2::Sdl,
) -> Result<(NesRender, SdlAudioDriver)> {
    let (canvas, audio_driver) = create_sdl_drivers(sdl_context, &INTERACTIVE_WINDOW, false)?;
    // Interactive mode is always windowed, so the factory always returns a canvas.
    let canvas = canvas.expect("windowed SDL setup must return a canvas");

    // One render type serves both modes: with no recording sender and the
    // overlay disabled, `NesRender` presents exactly what `SdlRender` presented.
    let render = NesRender::new(Some(canvas), None, 0);
    Ok((render, audio_driver))
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

        // One loop body serves both modes: with no movie, the recording and
        // overlay slots of `NesRender` stay inactive and the interactive
        // input source keeps today's keys, window, throttle and exit path.
        let mut source = InputSource::Interactive;
        let opts = LoopOptions::interactive();
        run_event_loop(
            &mut machine,
            Some(&mut event_pump),
            &mut source,
            &opts,
            None,
        );

        Ok(())
    }
}
