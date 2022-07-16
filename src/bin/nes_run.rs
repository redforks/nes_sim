use std::fs::File;
use nes_sim::{Cpu, Plugin, Instruction};
use std::io::Read;

fn main() {
    let mut f = File::open("6502_functional_test.bin").unwrap();
    let mut cpu = Cpu::new();
    assert_eq!(65536, f.read(&mut cpu.memory).unwrap());
    let mut sync_cycle = ReportPlugin::new();
    cpu.run(&mut sync_cycle);
}

struct ReportPlugin {
    last_pc: Option<u16>
}

impl ReportPlugin {
    fn new() -> ReportPlugin {
        ReportPlugin { last_pc: None }
    }
}

impl Plugin for ReportPlugin {
    fn start(&mut self, cpu: &Cpu) {
        println!("pc: ${:04x}", cpu.pc);
    }

    fn end(&mut self, cpu: &Cpu, inst: Instruction, _: u8) -> bool {
        println!("{:?}\na: ${:02x}, x: ${:02x}, y: ${:02x}, sp: ${:02x}, p: ${:02x}\n", inst, cpu.a, cpu.x, cpu.y, cpu.sp, cpu.status);

        if let Some(last) = self.last_pc {
            if last == cpu.pc {
                println!("pc repeated");
                return true;
            }
        }
        self.last_pc = Some(cpu.pc);
        false
    }
}
