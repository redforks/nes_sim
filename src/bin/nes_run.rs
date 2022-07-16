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
    count: u32,
    last_pc: Option<u16>,
}

impl ReportPlugin {
    fn new() -> ReportPlugin {
        ReportPlugin { count:0, last_pc: None }
    }
}

impl Plugin for ReportPlugin {
    fn start(&mut self, cpu: &Cpu) {
        self.count += 1;
        println!("[{}] pc: ${:04x}", self.count, cpu.pc);
    }

    fn end(&mut self, cpu: &Cpu, inst: Instruction, _: u8) -> bool {
        println!(
            "{:?}\na: ${:02x}, x: ${:02x}, y: ${:02x}, sp: ${:02x}, p: ${:02x} {} top: ${:02x}\n",
            inst, cpu.a, cpu.x, cpu.y, cpu.sp, cpu.status, format_flags(cpu), cpu.peek_stack());

        if let Some(last) = self.last_pc {
            if last == cpu.pc {
                println!("test failed: pc repeated");
                return true;
            }
        }
        self.last_pc = Some(cpu.pc);
        false
    }
}

fn format_flags(cpu: &Cpu) -> String {
    let mut r = String::new();

    r.push(if cpu.flag(nes_sim::NegativeFlag) {'N'} else {'n'});
    r.push(if cpu.flag(nes_sim::OverflowFlag) {'V'} else {'v'});
    r.push(if cpu.flag(nes_sim::BreakFlag) {'B'} else {'b'});
    r.push(if cpu.flag(nes_sim::DecimalModeFlag) {'D'} else {'d'});
    r.push(if cpu.flag(nes_sim::InterruptDisableFlag) {'I'} else {'i'});
    r.push(if cpu.flag(nes_sim::ZeroFlag) {'Z'} else {'z'});
    r.push(if cpu.flag(nes_sim::CarryFlag) {'C'} else {'c'});

    r
}
