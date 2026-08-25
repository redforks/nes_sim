use std::path::PathBuf;

use crate::image::MachineWrapper;
use ansi_term::Color;
use clap::Parser;
use nes_core::ExecuteResult;
use nes_core::nes::controller::Button;

mod image;
mod plugin;

#[cfg(feature = "tcp-server")]
mod tcp_server;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short)]
    f: PathBuf,

    #[arg(short, long)]
    quiet: bool,
    /// Exit if more than this many instructions executed (0 = disabled)
    #[arg(long = "max-instructions", default_value_t = 0)]
    max_instructions: u64,
    /// Set the CPU start PC after reset (hex with 0x prefix or decimal)
    #[arg(long = "start-pc")]
    start_pc: Option<String>,
    /// Start TCP server on port 28800 for MCP communication
    #[arg(long = "tcp-server")]
    tcp_server: bool,
    /// Press and hold a controller 1 button from a frame onward, as
    /// <button>@<frame> (e.g. --press start@11); repeatable. Frames count
    /// PPU frames from reset, matching --dump-frame.
    #[arg(long = "press", value_parser = parse_press)]
    presses: Vec<PressAction>,
    /// Dump the rendered frame at this frame number (first vblank observed at
    /// or after it) to --dump-out and exit; blessing tool for PngFrameMatch
    /// expected images
    #[arg(long = "dump-frame")]
    dump_frame: Option<usize>,
    /// Output PNG path for --dump-frame
    #[arg(long = "dump-out")]
    dump_out: Option<PathBuf>,
}

fn main() {
    let Args {
        f,
        quiet,
        max_instructions,
        start_pc,
        tcp_server,
        dump_frame,
        dump_out,
        presses,
    } = Args::parse();

    env_logger::builder().format_timestamp(None).init();

    // TCP server mode
    #[cfg(feature = "tcp-server")]
    if tcp_server {
        tcp_server::run_tcp_server(f, quiet, start_pc, max_instructions);
    }

    #[cfg(not(feature = "tcp-server"))]
    if tcp_server {
        eprintln!("Error: --tcp-server requires the tcp-server feature");
        std::process::exit(1);
    }

    let image = image::load_image(f).unwrap();

    let start_pc = match start_pc {
        Some(s) => {
            let parsed = if s.starts_with("0x") || s.starts_with("0X") {
                u16::from_str_radix(&s[2..], 16)
            } else {
                s.parse::<u16>()
            };
            Some(parsed.expect("invalid --start-pc value"))
        }
        None => None,
    };

    let mut machine = if let Some(dump_frame) = dump_frame {
        let dump_out = dump_out.expect("--dump-frame requires --dump-out");
        image.create_dump_machine(quiet, start_pc, max_instructions, dump_frame, dump_out)
    } else {
        image.create_machine(quiet, start_pc, max_instructions)
    };
    let mut presses = presses;
    exec(&mut machine, |m| {
        let result = m.tick();
        apply_presses(m, &mut presses);
        result
    });
}
/// A joypad button held from `frame` onward: the CLI form of tetanes-core's
/// tests.json `{"action": {"Joypad": ["One", "<Button>"]}}` frame action,
/// which presses the button at the named frame and never releases it.
#[derive(Debug, Clone, Copy)]
struct PressAction {
    button: Button,
    frame: usize,
    pressed: bool,
}

fn parse_press(spec: &str) -> Result<PressAction, String> {
    let (name, frame) = spec
        .split_once('@')
        .ok_or_else(|| format!("invalid --press `{spec}`: expected <button>@<frame>"))?;
    let button = match name {
        "a" => Button::A,
        "b" => Button::B,
        "select" => Button::Select,
        "start" => Button::Start,
        "up" => Button::Up,
        "down" => Button::Down,
        "left" => Button::Left,
        "right" => Button::Right,
        other => return Err(format!("unknown button `{other}` in --press `{spec}`")),
    };
    let frame = frame
        .parse()
        .map_err(|_| format!("invalid frame `{frame}` in --press `{spec}`"))?;
    Ok(PressAction {
        button,
        frame,
        pressed: false,
    })
}

impl PressAction {
    /// Returns the button on the first call once `frame_no` has reached the
    /// target frame; `None` before that and on every later call.
    fn due(&mut self, frame_no: usize) -> Option<Button> {
        if self.pressed || frame_no < self.frame {
            return None;
        }
        self.pressed = true;
        Some(self.button)
    }
}

fn apply_presses(m: &mut MachineWrapper, presses: &mut [PressAction]) {
    // Bin machines (raw .bin CPU test images) have no PPU frame counter, so
    // only touch `frame_no` when a press could actually be due.
    if presses.iter().all(|press| press.pressed) {
        return;
    }
    let frame_no = m.frame_no();
    for press in presses.iter_mut() {
        if let Some(button) = press.due(frame_no) {
            m.press_controller_a(button);
        }
    }
}

fn exec<F>(m: &mut MachineWrapper, mut f: F)
where
    F: FnMut(&mut MachineWrapper) -> ExecuteResult,
{
    loop {
        match f(m) {
            ExecuteResult::Continue => {}
            ExecuteResult::ShouldReset => {
                eprintln!("{}", Color::Red.paint("RESET"));
                m.reset();
            }
            ExecuteResult::Stop(result) => std::process::exit(result as i32),
            ExecuteResult::Halt => std::process::exit(128),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_press_rejects_malformed_specs() {
        assert!(parse_press("start").is_err());
        assert!(parse_press("start@").is_err());
        assert!(parse_press("start@x").is_err());
        assert!(parse_press("turbo@11").is_err());
        assert!(parse_press("@11").is_err());
    }

    #[test]
    fn parse_press_accepts_button_frame_pairs() {
        let press = parse_press("start@11").unwrap();
        assert_eq!(press.button, Button::Start);
        assert_eq!(press.frame, 11);
        assert!(!press.pressed);
        assert_eq!(parse_press("a@0").unwrap().button, Button::A);
        assert_eq!(parse_press("select@3").unwrap().button, Button::Select);
    }

    #[test]
    fn due_fires_once_at_or_after_target_frame() {
        let mut press = parse_press("start@11").unwrap();
        assert_eq!(press.due(10), None);
        assert!(!press.pressed);
        assert_eq!(press.due(11), Some(Button::Start));
        assert!(press.pressed);
        assert_eq!(press.due(12), None);
        assert_eq!(press.due(80), None);
    }
}
