use nes_core::{Cpu, Plugin};

pub struct Console();

impl Plugin for Console {
    fn start(&mut self, _: &Cpu) {}

    fn end(&mut self, cpu: &Cpu) {
        // write string at $6004 to console until end with '\0'
        let mut addr = 0x6004;
        loop {
            let c = cpu.read_byte(addr);
            if c == 0 {
                break;
            }
            print!("{}", c as char);
            addr += 1;
        }
    }
}
