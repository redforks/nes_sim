use ansi_term::Color;
use is_terminal::IsTerminal;
use nes_core::nes::NesMcu;
use nes_core::render::Render;
use nes_core::{Cpu, ExecuteResult, Plugin};
use std::sync::LazyLock;

static IS_TERMINAL: LazyLock<bool> = LazyLock::new(|| std::io::stdout().is_terminal());

const NAMETABLE_START: u16 = 0x2000;
const NAMETABLE_LEN: usize = 0x03C0;

#[derive(Default)]
pub struct NametableConsole {
    stop: Option<ExecuteResult>,
    /// If console contains this string, consider the test passed.
    magic_success_word: Option<String>,
    last: String,
}

impl NametableConsole {
    pub fn with_magic_success_word(word: &str) -> Self {
        Self {
            magic_success_word: Some(word.to_owned()),
            ..Default::default()
        }
    }
}

impl<R: Render> Plugin<NesMcu<R, ()>> for NametableConsole {
    fn start(&mut self, _: &mut Cpu<NesMcu<R, ()>>) {}

    fn end(&mut self, cpu: &mut Cpu<NesMcu<R, ()>>) {
        if !cpu.mcu().ppu().in_vblank() || !cpu.mcu().ppu().rendering_enabled() {
            return;
        }

        let buf = read_console(cpu);
        if buf.is_empty() || buf == self.last {
            return;
        }

        if !self.last.is_empty() && buf.starts_with(&self.last[..]) {
            let s = &buf[self.last.len()..];
            if !s.is_empty() {
                output(s);
                output("\n");
            }
        } else {
            output(&buf);
            output("\n");
        }

        self.stop = if contains_passed(&buf) {
            Some(ExecuteResult::Stop(0))
        } else if contains_failed(&buf) {
            Some(ExecuteResult::Stop(1))
        } else if let Some(magic_success_word) = &self.magic_success_word {
            if buf.contains(magic_success_word) {
                Some(ExecuteResult::Stop(0))
            } else {
                None
            }
        } else {
            None
        };
        self.last = buf;
    }

    fn should_stop(&self) -> ExecuteResult {
        self.stop.unwrap_or(ExecuteResult::Continue)
    }
}

fn read_console<R: Render>(cpu: &Cpu<NesMcu<R, ()>>) -> String {
    let mut buf = Vec::with_capacity(NAMETABLE_LEN);
    for offset in 0..NAMETABLE_LEN as u16 {
        let value = cpu.mcu().read_nametable(NAMETABLE_START + offset);
        if value == 0 {
            break;
        }
        buf.push(value);
        if offset % 32 == 31 {
            let trimmed = buf.trim_ascii_end();
            buf.truncate(trimmed.len());
            buf.push(b'\n');
        }
    }

    let mut r = String::from_utf8_lossy(&buf).as_ref().to_owned();
    loop {
        let last_len = r.len();
        r = r.replace("\n\n", "\n");
        if r.len() == last_len {
            break;
        }
    }
    r
}

fn contains_passed(s: &str) -> bool {
    s.contains("PASSED") || s.contains("Passed\n")
}

fn contains_failed(s: &str) -> bool {
    s.contains("FAILED") || s.contains("Error ") || s.contains("Failed")
}

fn output<S: AsRef<str>>(s: S) {
    if s.as_ref().is_empty() {
        return;
    }

    if *IS_TERMINAL {
        print!("{}", Color::Green.paint(s.as_ref()));
    } else {
        print!("{}", s.as_ref());
    }
}
