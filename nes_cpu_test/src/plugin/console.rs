use ansi_term::Color;
use is_terminal::IsTerminal;
use nes_core::nes::NesMcu;
use nes_core::nes::ppu::Ppu;
use nes_core::render::Render;
use nes_core::{Cpu, Plugin, SystemClock};
use std::cell::RefCell;
use std::fmt::Write as _;
use std::sync::LazyLock;

static IS_TERMINAL: LazyLock<bool> = LazyLock::new(|| std::io::stdout().is_terminal());

#[derive(Default)]
struct NewFrameDetector {
    last_frame_no: usize,
}

impl NewFrameDetector {
    fn is_new_frame<R: Render>(&mut self, ppu: &Ppu<R>) -> bool {
        let frame_no = ppu.timing().frame_no();
        if frame_no != self.last_frame_no {
            self.last_frame_no = frame_no;
            true
        } else {
            false
        }
    }
}

#[derive(Default)]
pub struct Console {
    buf: Vec<u8>,
    line_buf: RefCell<String>,
    read_buf: Vec<u8>,
    new_frame_detector: NewFrameDetector,
}

impl Console {
    fn output<S: AsRef<str>>(&self, s: S) {
        if s.as_ref().is_empty() {
            return;
        }

        let _ = write!(self.line_buf.borrow_mut(), "{}", s.as_ref());
        if self.line_buf.borrow().ends_with('\n') {
            let line_buf = self.line_buf.take();
            if *IS_TERMINAL {
                print!("{}", Color::Green.paint(&line_buf));
            } else {
                print!("{}", &line_buf);
            }
        }
    }
}

impl<R: Render> Plugin<NesMcu<R, ()>> for Console {
    fn start(&mut self, _: &Cpu<NesMcu<R, ()>>, _: SystemClock) {}

    fn end(&mut self, cpu: &Cpu<NesMcu<R, ()>>, _: SystemClock) {
        if self.new_frame_detector.is_new_frame(cpu.mcu().ppu()) {
            if cpu.peek_byte(0x6001) != 0xDE
                || cpu.peek_byte(0x6002) != 0xB0
                || cpu.peek_byte(0x6003) != 0x61
            {
                return;
            }

            // write string at $6004 to console until end with '\0'
            self.read_buf.clear();
            let mut addr = 0x6004;
            loop {
                let c = cpu.peek_byte(addr);
                if c == 0 {
                    break;
                }
                self.read_buf.push(c);
                addr += 1;
            }
            // if buf not start with self.buf, print it
            if self.read_buf.len() >= self.buf.len() {
                if self.read_buf[..self.buf.len()] != self.buf[..] {
                    self.output(String::from_utf8_lossy(&self.read_buf));
                } else {
                    self.output(String::from_utf8_lossy(&self.read_buf[self.buf.len()..]));
                }
            } else {
                self.output(String::from_utf8_lossy(&self.read_buf));
            }
            std::mem::swap(&mut self.buf, &mut self.read_buf);
        }
    }
}
