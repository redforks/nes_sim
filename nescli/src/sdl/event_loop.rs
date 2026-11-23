//! Unified SDL event loop (ticket #123).
//!
//! One loop body serves both the interactive `run` action and movie playback:
//! a single [`InputSource`] enum feeds per-frame input, with one
//! `ExecuteResult` match, one reset path and one mode-gated throttle.
//!
//! Input authority is mode-dependent: interactive keeps today's keys
//! (Quit/Esc exit, F2 soft-resets, F4 quits, F5 saves, F7 loads, controller
//! keys drive port A) while playback only honors the OS `Event::Quit`
//! (no Esc/F2/F4/F5/F7, controller keys ignored). Reset precedence is movie-driven reset first
//! (`hard_reset` > `soft_reset`) with `ShouldReset -> soft_reset` shared by
//! both modes; the sources are mutually exclusive per mode. One movie entry
//! plays one frame and consumes one index, reset entries included. End of movie
//! keeps today's `--extra-frames` countdown verbatim, including the
//! `trim_after` early exit and the auto-fast-forward idle handling (which
//! also covers past-the-end frames).

use super::{NesAudioDriver, map_keycode_to_button, render::NesRender};
use crate::save_state::{load_machine_from_path, save_machine_to_path};
use nes_core::{
    Plugin,
    movie::{GamepadInput, InputLog},
    nes::NesMcu,
    nes_machine::NesMachine,
};
use sdl2::{EventPump, event::Event, keyboard::Keycode};
use std::{
    cell::Cell,
    rc::Rc,
    time::{Duration, Instant},
};

/// Reset a movie entry asks for, applied before its frame is emulated.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MovieReset {
    Hard,
    Soft,
}

/// One movie entry decoded for the frame that plays it: the reset to apply
/// first, then both ports' inputs for that same frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MovieFrame {
    pub reset: Option<MovieReset>,
    pub port0: GamepadInput,
    pub port1: GamepadInput,
}

/// Decode the movie entry at `index` into the frame that plays it. Hard reset
/// wins over soft reset. `None` past the end of the movie. Pure: needs no SDL
/// and no machine.
pub fn movie_frame_at(logs: &[InputLog], index: usize) -> Option<MovieFrame> {
    logs.get(index).map(|log| MovieFrame {
        reset: if log.command.hard_reset() {
            Some(MovieReset::Hard)
        } else if log.command.soft_reset() {
            Some(MovieReset::Soft)
        } else {
            None
        },
        port0: log.port0,
        port1: log.port1,
    })
}

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

    /// The movie frame at the current index, or `None` for interactive mode
    /// and past-the-end playback.
    pub fn movie_frame(&self) -> Option<MovieFrame> {
        match self {
            InputSource::Interactive => None,
            InputSource::Movie { logs, index } => movie_frame_at(logs, *index),
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

/// Fast-forward state shared with the unified audio driver: the precomputed
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

fn set_controller_from_gamepad<P>(machine: &mut NesMachine<P>, input: &GamepadInput, port_b: bool)
where
    P: Plugin<NesMcu>,
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

fn apply_movie_reset<P>(machine: &mut NesMachine<P>, reset: MovieReset)
where
    P: Plugin<NesMcu>,
{
    match reset {
        MovieReset::Hard => machine.hard_reset(),
        MovieReset::Soft => machine.soft_reset(),
    }
}

fn set_controller_a_from_gamepad<P>(machine: &mut NesMachine<P>, input: &GamepadInput)
where
    P: Plugin<NesMcu>,
{
    set_controller_from_gamepad(machine, input, false);
}

fn set_controller_b_from_gamepad<P>(machine: &mut NesMachine<P>, input: &GamepadInput)
where
    P: Plugin<NesMcu>,
{
    set_controller_from_gamepad(machine, input, true);
}

/// F5 (save) and F7 (load) arm only in interactive mode with no key
/// repeat, so movie playback stays deterministic. (The loop already routes
/// every key but OS Quit away from the match arms in movie mode; this is
/// the second gate at the decision itself.)
fn state_hotkey_arms_action(source: &InputSource, repeat: bool) -> bool {
    !repeat && !source.is_movie()
}

/// Caller-owned inputs for one [`run_event_loop`] call: the event pump
/// (`None` for headless playback), the input source, pacing knobs, the
/// playback fast-forward state (`Some` for playback; idle ranges may still
/// be empty when `--auto-fast-forward` is off), and the save-file path
/// (`Some` for interactive runs, where F5 saves and F7 loads there;
/// `None` for movie playback, where F5/F7 are ignored).
pub struct LoopInputs<'a> {
    pub event_pump: Option<&'a mut EventPump>,
    pub source: &'a mut InputSource,
    pub opts: &'a LoopOptions,
    pub fast_forward: Option<&'a FastForwardState>,
    pub state_path: Option<&'a std::path::Path>,
}

/// Drive `machine` until an exit event, `Stop`/`Halt`, the `trim_after` limit
/// or the end-of-movie countdown runs out.
pub fn run_event_loop<P>(
    machine: &mut NesMachine<P>,
    render: &mut NesRender,
    audio: &mut NesAudioDriver,
    mut inputs: LoopInputs<'_>,
) where
    P: Plugin<NesMcu>,
{
    let LoopInputs {
        event_pump,
        source,
        opts,
        fast_forward,
        state_path,
    } = &mut inputs;
    let (event_pump, source, opts, fast_forward, state_path) =
        (event_pump, source, opts, fast_forward, state_path);
    let target_frame_duration = Duration::new(0, 1_000_000_000u32 / 60);
    let mut extra_frames_remaining = opts.extra_frames;
    let mut save_requested = false;
    // `spin_sleep` sleeps the bulk of the remaining budget with the OS
    // sleeper and spins the tail, landing the frame boundary more
    // precisely than `std::thread::sleep` alone.
    let sleeper = spin_sleep::SpinSleeper::default();

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
                        keycode: Some(Keycode::F5),
                        repeat,
                        ..
                    } => {
                        // Save the simulator state; the write runs right
                        // after the next `process_frame()` returns.
                        if state_hotkey_arms_action(source, repeat) {
                            save_requested = true;
                        }
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::F7),
                        repeat,
                        ..
                    } => {
                        // Load the state file in place at the top of the
                        // event loop: the restored state is emulated by the
                        // very next `process_frame()` below. A failed load
                        // leaves the machine untouched (validation runs
                        // before any mutation in nes_core) and the game
                        // keeps running.
                        if state_hotkey_arms_action(source, repeat) {
                            if let Some(path) = state_path {
                                let message = match load_machine_from_path(machine, path) {
                                    Ok(message) | Err(message) => message,
                                };
                                eprintln!("{message}");
                            }
                        }
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

        // Movie-driven reset wins over any other reset source; the two
        // sources are mutually exclusive per mode. The index advances once per
        // iteration at the end of the loop, reset entries included. A reset
        // entry consumes its index but emulates no frame (FCEUX movie
        // semantics): the next entry plays on the first emulated frame.
        let mut reset_only = false;
        if let Some(frame) = source.movie_frame() {
            if let Some(reset) = frame.reset {
                apply_movie_reset(machine, reset);
                reset_only = true;
            }
            set_controller_a_from_gamepad(machine, &frame.port0);
            set_controller_b_from_gamepad(machine, &frame.port1);
            render.set_input(&frame.port0, &frame.port1);
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
            render.set_fast_forward(is_idle);
        }

        // One frame: present owned PPU framebuffer, queue owned APU audio,
        // then match the frame status for both modes. Skipped for a movie
        // reset-only entry; trim, countdown and index advance still run.
        if !reset_only {
            let output = machine.process_frame();
            let status = output.status;
            render.present_frame_bytes(output.frame);
            audio.push_samples(output.audio);
            audio.flush_frame();
            // The save runs in the same iteration that emulated the
            // frame, before any further emulation: present/push/flush
            // never touch the machine, so the stored state is exactly
            // the post-`process_frame()` state. It must wait until the
            // output borrow ends (above) before reborrowing `machine`.
            if save_requested {
                save_requested = false;
                if let Some(path) = state_path {
                    let message = match save_machine_to_path(machine, path) {
                        Ok(message) | Err(message) => message,
                    };
                    eprintln!("{message}");
                }
            }
            match status {
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
            sleeper.sleep_until(frame_start + target_frame_duration);
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
    #[test]
    fn state_hotkeys_arm_only_for_interactive_without_repeat() {
        // Movie-mode no-op (#196): F5/F7 never fire during playback, so a
        // recorded run stays deterministic.
        let interactive = InputSource::Interactive;
        assert!(state_hotkey_arms_action(&interactive, false));
        assert!(!state_hotkey_arms_action(&interactive, true));
        assert!(!state_hotkey_arms_action(&movie_source(), false));
        assert!(!state_hotkey_arms_action(&movie_source(), true));
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

    fn entry(command: Command, port0: GamepadInput) -> InputLog {
        InputLog::new(command, port0, GamepadInput::default())
    }

    fn a_pressed() -> GamepadInput {
        GamepadInput::new().with_a(true)
    }

    fn b_pressed() -> GamepadInput {
        GamepadInput::new().with_b(true)
    }

    fn movie_of(logs: Vec<InputLog>) -> InputSource {
        InputSource::Movie {
            logs: logs.into_boxed_slice(),
            index: 0,
        }
    }

    fn frame(reset: Option<MovieReset>, port0: GamepadInput) -> Option<MovieFrame> {
        Some(MovieFrame {
            reset,
            port0,
            port1: GamepadInput::default(),
        })
    }

    fn index_of(source: &InputSource) -> usize {
        match source {
            InputSource::Movie { index, .. } => *index,
            InputSource::Interactive => panic!("expected a movie source"),
        }
    }

    /// Mirrors the loop's per-iteration bookkeeping without emulating a frame:
    /// read the frame the iteration plays, then consume exactly one index.
    fn play(source: &mut InputSource, frames: usize) -> Vec<Option<MovieFrame>> {
        (0..frames)
            .map(|_| {
                let frame = source.movie_frame();
                source.advance();
                frame
            })
            .collect()
    }

    #[test]
    fn soft_reset_entry_then_normal_entry_plays_both_in_order() {
        let mut source = movie_of(vec![
            entry(Command::new().with_soft_reset(true), a_pressed()),
            entry(Command::default(), b_pressed()),
        ]);
        assert_eq!(
            play(&mut source, 2),
            vec![
                frame(Some(MovieReset::Soft), a_pressed()),
                frame(None, b_pressed()),
            ],
        );
        assert_eq!(index_of(&source), 2);
    }

    #[test]
    fn hard_reset_entry_then_normal_entry_plays_both_in_order() {
        let mut source = movie_of(vec![
            entry(Command::new().with_hard_reset(true), a_pressed()),
            entry(Command::default(), b_pressed()),
        ]);
        assert_eq!(
            play(&mut source, 2),
            vec![
                frame(Some(MovieReset::Hard), a_pressed()),
                frame(None, b_pressed()),
            ],
        );
        assert_eq!(index_of(&source), 2);
    }

    #[test]
    fn consecutive_reset_entries_each_play_once_in_order() {
        let mut source = movie_of(vec![
            entry(Command::new().with_soft_reset(true), a_pressed()),
            entry(Command::new().with_hard_reset(true), b_pressed()),
            entry(Command::default(), a_pressed().with_b(true)),
        ]);
        assert_eq!(
            play(&mut source, 3),
            vec![
                frame(Some(MovieReset::Soft), a_pressed()),
                frame(Some(MovieReset::Hard), b_pressed()),
                frame(None, a_pressed().with_b(true)),
            ],
        );
        assert_eq!(index_of(&source), 3);
    }

    #[test]
    fn reset_entry_as_last_entry_ends_movie_after_one_step() {
        let mut source = movie_of(vec![entry(
            Command::new().with_hard_reset(true),
            a_pressed(),
        )]);
        assert_eq!(
            play(&mut source, 1),
            vec![frame(Some(MovieReset::Hard), a_pressed())],
        );
        assert_eq!(index_of(&source), 1);
        assert_eq!(source.movie_frame(), None);
    }

    #[test]
    fn movie_without_reset_entries_plays_each_entry_once() {
        let mut source = movie_of(vec![
            entry(Command::default(), a_pressed()),
            entry(Command::default(), b_pressed()),
            entry(Command::default(), GamepadInput::default()),
        ]);
        assert_eq!(
            play(&mut source, 3),
            vec![
                frame(None, a_pressed()),
                frame(None, b_pressed()),
                frame(None, GamepadInput::default()),
            ],
        );
        assert_eq!(index_of(&source), 3);
    }

    #[test]
    fn interactive_source_has_no_movie_frame_and_advance_is_noop() {
        let mut source = InputSource::Interactive;
        assert!(!source.is_movie());
        assert_eq!(source.movie_frame(), None);
        source.advance();
        assert_eq!(source.movie_frame(), None);
    }

    #[test]
    fn movie_source_starts_at_frame_zero() {
        match movie_source() {
            InputSource::Movie { index, .. } => assert_eq!(index, 0),
            InputSource::Interactive => panic!("expected a movie source"),
        }
    }
}
