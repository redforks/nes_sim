//! Unified SDL event loop (ticket #123).
//!
//! One loop body serves both the interactive `run` action and movie playback:
//! a single [`InputSource`] enum feeds per-frame input, with one
//! `ExecuteResult` match, one reset path and one mode-gated throttle.
//!
//! Input authority is mode-dependent: interactive keeps today's keys
//! (Quit/Esc exit, F2 soft-resets, F4 quits, controller keys drive port A)
//! while playback only honors the OS `Event::Quit` (no Esc/F2/F4, controller
//! keys ignored). Reset precedence is movie-driven reset first
//! (`hard_reset` > `soft_reset`) with `ShouldReset -> soft_reset` shared by
//! both modes; the sources are mutually exclusive per mode. End of movie
//! keeps today's `--extra-frames` countdown verbatim, including the
//! `trim_after` early exit and the auto-fast-forward idle handling (which
//! also covers past-the-end frames).

use super::{map_keycode_to_button, render::NesRender};
use nes_core::{
    Plugin,
    movie::{GamepadInput, InputLog},
    nes::{NesMcu, apu::AudioDriver},
    nes_machine::NesMachine,
    render::Render,
};
use sdl2::{EventPump, event::Event, keyboard::Keycode};
use std::{
    cell::Cell,
    rc::Rc,
    time::{Duration, Instant},
};

/// Which input feeds the unified loop: live keyboard or a recorded movie.
pub enum InputSource {
    Interactive,
    Movie { logs: Box<[InputLog]>, index: usize },
}

impl InputSource {
    /// True for movie playback; interactive input is live keyboard.
    pub fn is_movie(&self) -> bool {
        matches!(self, InputSource::Movie { .. })
    }

    /// The current movie frame's reset flags and port inputs, or `None` for
    /// interactive mode and past-the-end playback. Returned by value so the
    /// reset path can advance the index without aliasing the logs.
    pub fn current_frame(&self) -> Option<(bool, bool, GamepadInput, GamepadInput)> {
        match self {
            InputSource::Interactive => None,
            InputSource::Movie { logs, index } => logs.get(*index).map(|log| {
                (
                    log.command.hard_reset(),
                    log.command.soft_reset(),
                    log.port0,
                    log.port1,
                )
            }),
        }
    }

    /// Advance past the consumed movie frame; a no-op for interactive mode.
    pub fn advance(&mut self) {
        if let InputSource::Movie { index, .. } = self {
            *index += 1;
        }
    }
}

/// Mode-gated pacing and end-of-movie knobs. Interactive uses
/// [`LoopOptions::interactive`] (windowed, always throttled, no countdown);
/// playback fills every field from its action flags.
pub struct LoopOptions {
    pub headless: bool,
    pub no_throttle: bool,
    pub has_output: bool,
    pub auto_fast_forward: bool,
    pub trim_after: u32,
    pub extra_frames: u32,
}

impl LoopOptions {
    /// Interactive `run`: always windowed, never fast-forwards, never trims,
    /// never counts down.
    pub fn interactive() -> Self {
        Self {
            headless: false,
            no_throttle: false,
            has_output: false,
            auto_fast_forward: false,
            trim_after: 0,
            extra_frames: 0,
        }
    }
}

/// Fast-forward state shared with the recording audio driver: the precomputed
/// idle ranges plus the skip flag the audio wrapper reads per flush.
pub struct FastForwardState {
    pub ranges: Vec<(usize, usize)>,
    pub skip: Rc<Cell<bool>>,
}

/// One pacing decision for both modes: interactive always sleeps the
/// remaining frame budget; playback only when windowed, throttled and not
/// recording.
pub fn should_throttle(source: &InputSource, opts: &LoopOptions) -> bool {
    matches!(source, InputSource::Interactive)
        || (!opts.headless && !opts.no_throttle && !opts.has_output)
}

fn set_controller_from_gamepad<P, R, D>(
    machine: &mut NesMachine<P, R, D>,
    input: &GamepadInput,
    port_b: bool,
) where
    P: Plugin<NesMcu<R, D>>,
    R: Render,
    D: AudioDriver,
{
    use nes_core::nes::controller::Button;
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
            if port_b {
                machine.press_controller_b(button);
            } else {
                machine.press_controller_a(button);
            }
        } else if port_b {
            machine.release_controller_b(button);
        } else {
            machine.release_controller_a(button);
        }
    }
}

fn set_controller_a_from_gamepad<P, R, D>(machine: &mut NesMachine<P, R, D>, input: &GamepadInput)
where
    P: Plugin<NesMcu<R, D>>,
    R: Render,
    D: AudioDriver,
{
    set_controller_from_gamepad(machine, input, false);
}

fn set_controller_b_from_gamepad<P, R, D>(machine: &mut NesMachine<P, R, D>, input: &GamepadInput)
where
    P: Plugin<NesMcu<R, D>>,
    R: Render,
    D: AudioDriver,
{
    set_controller_from_gamepad(machine, input, true);
}

/// Drive `machine` until an exit event, `Stop`/`Halt`, the `trim_after` limit
/// or the end-of-movie countdown runs out. `event_pump` is `None` for
/// headless playback; `fast_forward` is `Some` for playback (idle ranges may
/// still be empty when `--auto-fast-forward` is off).
pub fn run_event_loop<P, D>(
    machine: &mut NesMachine<P, NesRender, D>,
    mut event_pump: Option<&mut EventPump>,
    source: &mut InputSource,
    opts: &LoopOptions,
    fast_forward: Option<&FastForwardState>,
) where
    P: Plugin<NesMcu<NesRender, D>>,
    D: AudioDriver,
{
    let target_frame_duration = Duration::new(0, 1_000_000_000u32 / 60);
    let mut extra_frames_remaining = opts.extra_frames;

    'running: loop {
        let frame_start = Instant::now();
        // Event handling: input authority depends on the mode. Interactive
        // keeps today's keys; playback only honors the OS Quit event.
        if let Some(pump) = event_pump.as_deref_mut() {
            for event in pump.poll_iter() {
                if source.is_movie() {
                    if let Event::Quit { .. } = event {
                        break 'running;
                    }
                    continue;
                }
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        repeat: false,
                        ..
                    } => {
                        break 'running;
                    }
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
        }

        // Snapshot the current movie frame into owned copies so the reset
        // path below can advance the shared index without aliasing the logs.
        let movie_frame: Option<(bool, bool, GamepadInput, GamepadInput)> = source.current_frame();
        if let Some((hard_reset, soft_reset, port0, port1)) = movie_frame {
            // Movie-driven reset wins over any other reset source; the two
            // sources are mutually exclusive per mode. A reset frame advances
            // the index once here and once at the end of the loop, verbatim
            // from the playback loop this unifies.
            if hard_reset {
                source.advance();
                machine.hard_reset();
            } else if soft_reset {
                source.advance();
                machine.soft_reset();
            }
            set_controller_a_from_gamepad(machine, &port0);
            set_controller_b_from_gamepad(machine, &port1);
            machine.render_mut().set_input(&port0, &port1);
        }

        // Auto fast-forward: idle stretches (plus past-the-end frames) set
        // the shared skip flag and the render fast-forward flag.
        if opts.auto_fast_forward
            && let InputSource::Movie { logs, index } = source
        {
            let is_idle = if *index >= logs.len() {
                true
            } else {
                fast_forward.is_some_and(|ff| {
                    ff.ranges
                        .iter()
                        .any(|&(start, end)| *index >= start && *index < end)
                })
            };
            if let Some(ff) = fast_forward {
                ff.skip.set(is_idle);
            }
            machine.render_mut().set_fast_forward(is_idle);
        }

        // One ExecuteResult match for both modes.
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

        // Trim-after early exit (playback only: interactive passes 0).
        if opts.trim_after > 0 && machine.frame_no() >= opts.trim_after as usize {
            eprintln!(">> Trim after {} frames", opts.trim_after);
            break 'running;
        }

        // End-of-movie extra-frames countdown (playback only).
        if let InputSource::Movie { logs, index } = source
            && *index >= logs.len()
        {
            if extra_frames_remaining == 0 {
                eprintln!(
                    ">> End of movie reached (after {} extra frames)",
                    opts.extra_frames
                );
                break 'running;
            }
            extra_frames_remaining -= 1;
        }

        // One throttle, gated by mode.
        if should_throttle(source, opts) {
            let elapsed = frame_start.elapsed();
            if elapsed < target_frame_duration {
                std::thread::sleep(target_frame_duration - elapsed);
            }
        }

        // Advance past the consumed movie frame.
        source.advance();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nes_core::movie::{Command, InputLog};

    fn movie_source() -> InputSource {
        InputSource::Movie {
            logs: vec![InputLog::new(
                Command::default(),
                GamepadInput::default(),
                GamepadInput::default(),
            )]
            .into_boxed_slice(),
            index: 0,
        }
    }

    fn movie_opts(headless: bool, no_throttle: bool, has_output: bool) -> LoopOptions {
        LoopOptions {
            headless,
            no_throttle,
            has_output,
            auto_fast_forward: false,
            trim_after: 0,
            extra_frames: 0,
        }
    }

    #[test]
    fn interactive_always_throttles_regardless_of_flags() {
        let source = InputSource::Interactive;
        for headless in [false, true] {
            for no_throttle in [false, true] {
                for has_output in [false, true] {
                    assert!(
                        should_throttle(&source, &movie_opts(headless, no_throttle, has_output)),
                        "interactive must throttle (headless={headless} no_throttle={no_throttle} has_output={has_output})",
                    );
                }
            }
        }
    }

    #[test]
    fn movie_throttles_only_when_windowed_throttled_and_not_recording() {
        let source = movie_source();
        assert!(should_throttle(&source, &movie_opts(false, false, false)));
        assert!(!should_throttle(&source, &movie_opts(true, false, false)));
        assert!(!should_throttle(&source, &movie_opts(false, true, false)));
        assert!(!should_throttle(&source, &movie_opts(false, false, true)));
        assert!(!should_throttle(&source, &movie_opts(true, true, true)));
    }

    #[test]
    fn movie_source_starts_at_frame_zero() {
        match movie_source() {
            InputSource::Movie { index, .. } => assert_eq!(index, 0),
            InputSource::Interactive => panic!("expected a movie source"),
        }
    }
}
