use ansi_term::Color;
use nes_core::{Cpu, Plugin};

#[derive(Default)]
pub struct Console {
    buf: Vec<u8>,
}

impl Plugin for Console {
    fn start(&mut self, _: &Cpu) {}

    fn end(&mut self, cpu: &Cpu) {
        // if cpu.read_byte(0x6001) != 0xDE
        //     || cpu.read_byte(0x6002) != 0xB0
        //     || cpu.read_byte(0x6003) != 0x01
        // {
        //     return;
        // }

        // write string at $6004 to console until end with '\0'
        let mut buf = Vec::with_capacity(32);
        let mut addr = 0x6004;
        loop {
            let c = cpu.read_byte(addr);
            if c == 0 {
                break;
            }
            buf.push(c);
            addr += 1;
        }
        // if buf not start with self.buf, print it
        if buf.len() >= self.buf.len() {
            if buf[..self.buf.len()] != self.buf[..] {
                print!("{}", Color::Green.paint(String::from_utf8_lossy(&buf)));
            } else {
                print!(
                    "{}",
                    Color::Green.paint(String::from_utf8_lossy(&buf[self.buf.len()..]))
                );
            }
        } else {
            print!("{}", Color::Green.paint(String::from_utf8_lossy(&buf)));
        }
        self.buf = buf;
    }
}
