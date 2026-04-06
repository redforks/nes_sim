use ansi_term::Color;
use is_terminal::IsTerminal;
use nes_core::mcu::Mcu;
use nes_core::{Cpu, Plugin};
use std::fmt::Write as _;
use std::sync::LazyLock;

static IS_TERMINAL: LazyLock<bool> = LazyLock::new(|| std::io::stdout().is_terminal());

#[derive(Default)]
pub struct Console {
    buf: Vec<u8>,
    line_buf: String,
}

impl Console {
    fn output<S: AsRef<str>>(&mut self, s: S) {
        if s.as_ref().is_empty() {
            return;
        }

        let _ = write!(&mut self.line_buf, "{}", s.as_ref());
        if self.line_buf.ends_with('\n') {
            if *IS_TERMINAL {
                let _ = print!("{}", Color::Green.paint(&self.line_buf));
            } else {
                print!("{}", &self.line_buf);
            }
            self.line_buf.clear();
        }
    }
}

impl<M: Mcu> Plugin<M> for Console {
    fn start(&mut self, _: &mut Cpu<M>) {}

    fn end(&mut self, cpu: &mut Cpu<M>) {
        if cpu.peek_byte(0x6001) != 0xDE
            || cpu.peek_byte(0x6002) != 0xB0
            || cpu.peek_byte(0x6003) != 0x61
        {
            return;
        }

        // write string at $6004 to console until end with '\0'
        let mut buf = Vec::with_capacity(32);
        let mut addr = 0x6004;
        loop {
            let c = cpu.peek_byte(addr);
            if c == 0 {
                break;
            }
            buf.push(c);
            addr += 1;
        }
        // if buf not start with self.buf, print it
        if buf.len() >= self.buf.len() {
            if buf[..self.buf.len()] != self.buf[..] {
                self.output(String::from_utf8_lossy(&buf));
            } else {
                self.output(String::from_utf8_lossy(&buf[self.buf.len()..]));
            }
        } else {
            self.output(String::from_utf8_lossy(&buf));
        }
        self.buf = buf;
    }
}
