use std::path::PathBuf;

use crate::image::MachineWrapper;
use crate::zapper_test::ZapperTest;
use ansi_term::Color;
use clap::Parser;
use nes_core::ExecuteResult;
use nes_core::nes::controller::Button;

mod image;
mod plugin;
mod zapper_test;

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
    /// Capture APU output for manual listening: run the ROM for --frames PPU
    /// frames, then write every mixed sample to this path as a 44.1 kHz mono
    /// 16-bit WAV and exit (e.g. --dump-audio noise_pitch.wav)
    #[arg(long = "dump-audio")]
    dump_audio: Option<PathBuf>,
    /// PPU frames to execute before the --dump-audio export; 600 is ~10 s NTSC
    #[arg(long, requires = "dump_audio", default_value_t = 600)]
    frames: usize,
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
        dump_audio,
        frames,
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

    let rom_name = f
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("")
        .to_owned();
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

    if let Some(wav_out) = dump_audio {
        let recorder = image::WavRecorder::new();
        let mut machine =
            image.create_audio_dump_machine(recorder.clone(), quiet, start_pc, max_instructions);
        let mut presses = presses;
        let code = run_capture(&mut machine, frames, &mut presses);
        match recorder.write_wav(&wav_out) {
            Ok(samples) => eprintln!(
                "captured {} samples ({:.2} s) to {}",
                samples,
                samples as f64 / 44_100.0,
                wav_out.display()
            ),
            Err(e) => {
                eprintln!("failed to write {}: {e}", wav_out.display());
                std::process::exit(1);
            }
        }
        std::process::exit(code);
    }

    // Zapper ROMs run a scripted light-gun session (zapper_test.rs).
    // Blessing dumps (--dump-frame) still apply the actions but skip
    // expectation checks, so frames can be re-blessed deterministically.
    let mut zapper = ZapperTest::for_rom(&rom_name);
    let verify = dump_frame.is_none();
    let mut machine = if let Some(dump_frame) = dump_frame {
        let dump_out = dump_out.expect("--dump-frame requires --dump-out");
        image.create_dump_machine(quiet, start_pc, max_instructions, dump_frame, dump_out)
    } else {
        image.create_machine(quiet, start_pc, max_instructions)
    };
    let mut presses = presses;
    exec(&mut machine, &mut presses, zapper.as_mut(), verify);
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

fn exec(
    m: &mut MachineWrapper,
    presses: &mut [PressAction],
    mut zapper: Option<&mut ZapperTest>,
    verify: bool,
) {
    loop {
        // Scripted Zapper events land at the start of their target frame,
        // before that frame's scanlines run.
        if let Some(z) = zapper.as_deref_mut() {
            z.apply_due_actions(m);
        }
        match m.tick() {
            ExecuteResult::Continue => {}
            ExecuteResult::ShouldReset => {
                eprintln!("{}", Color::Red.paint("RESET"));
                m.reset();
            }
            ExecuteResult::Stop(result) => std::process::exit(result as i32),
            ExecuteResult::Halt => std::process::exit(128),
        }
        apply_presses(m, presses);
        if let Some(code) = zapper.as_deref_mut().and_then(|z| z.after_tick(m, verify)) {
            std::process::exit(code);
        }
    }
}

/// Audio-capture run loop: ticks until `frames` PPU frames have elapsed (the
/// ROM's own termination, if any, ends the capture early and its status
/// becomes the exit code), applying --press actions along the way. The caller
/// exports the APU buffer afterwards.
fn run_capture(m: &mut MachineWrapper, frames: usize, presses: &mut [PressAction]) -> i32 {
    loop {
        match m.tick() {
            ExecuteResult::Continue => {}
            ExecuteResult::ShouldReset => {
                eprintln!("{}", Color::Red.paint("RESET"));
                m.reset();
            }
            ExecuteResult::Stop(result) => return result as i32,
            ExecuteResult::Halt => return 128,
        }
        apply_presses(m, presses);
        if m.frame_no() >= frames {
            return 0;
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
    #[test]
    fn wav_recorder_writes_mono_pcm16_header_and_samples() {
        use crate::image::WavRecorder;
        use nes_core::nes::apu::AudioDriver;

        let mut recorder = WavRecorder::new();
        for sample in [-1.0f32, 0.0, 0.5, 1.5] {
            recorder.push_sample(sample);
        }
        let dir = std::env::temp_dir().join(format!("nes_cpu_test-wav-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("out.wav");
        let written = recorder.write_wav(&path).unwrap();
        assert_eq!(written, 4);

        let bytes = std::fs::read(&path).unwrap();
        assert_eq!(&bytes[0..4], b"RIFF");
        assert_eq!(&bytes[8..12], b"WAVE");
        assert_eq!(&bytes[12..16], b"fmt ");
        // PCM, mono, 44.1 kHz, 16-bit
        assert_eq!(u16::from_le_bytes(bytes[20..22].try_into().unwrap()), 1);
        assert_eq!(u16::from_le_bytes(bytes[22..24].try_into().unwrap()), 1);
        assert_eq!(
            u32::from_le_bytes(bytes[24..28].try_into().unwrap()),
            44_100
        );
        assert_eq!(u16::from_le_bytes(bytes[34..36].try_into().unwrap()), 16);
        assert_eq!(&bytes[36..40], b"data");
        assert_eq!(u32::from_le_bytes(bytes[40..44].try_into().unwrap()), 8);
        assert_eq!(u32::from_le_bytes(bytes[4..8].try_into().unwrap()), 44);
        // -1.0 -> -32767, 0.0 -> 0, 0.5 -> ~16384, clamped 1.5 -> 32767
        assert_eq!(
            i16::from_le_bytes(bytes[44..46].try_into().unwrap()),
            -32767
        );
        assert_eq!(i16::from_le_bytes(bytes[46..48].try_into().unwrap()), 0);
        assert_eq!(i16::from_le_bytes(bytes[48..50].try_into().unwrap()), 16384);
        assert_eq!(i16::from_le_bytes(bytes[50..52].try_into().unwrap()), 32767);
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
