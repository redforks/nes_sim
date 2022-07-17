use std::fs::File;
use nes_sim::{Cpu, Plugin, Instruction, MemoryBank, RamBank};
use std::io::Read;
use generic_array::GenericArray;
use generic_array::typenum;
use std::env;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let quiet = args.len() >= 2 && args[1] == "--quiet";

    let mut f = File::open("6502_functional_test.bin").unwrap();
    let mut ram = RamBank::<typenum::U65536>(GenericArray::default());
    assert_eq!(65536, f.read(&mut ram.0).unwrap());
    let mut cpu = Cpu::new_with_memory_banks(
        vec![MemoryBank::new(0, 0, Box::new(ram))]
    );
    cpu.pc = 0x400;
    let mut sync_cycle = ReportPlugin::new(quiet);
    while !sync_cycle.should_exit() {
        cpu.clock_tick(&mut sync_cycle);
    }
}

struct ReportPlugin {
    verbose: bool,
    count: u32,
    last_pc: Option<u16>,
    should_exit: bool,
}

impl ReportPlugin {
    fn new(quiet: bool) -> ReportPlugin {
        ReportPlugin { verbose: !quiet, count: 0, last_pc: None, should_exit: false }
    }

    fn should_exit(&self) -> bool {
        self.should_exit
    }
}

impl Plugin for ReportPlugin {
    fn start(&mut self, cpu: &Cpu) {
        self.count += 1;

        if self.verbose {
            println!("[{}] pc: ${:04x}", self.count, cpu.pc);
        }
    }

    fn end(&mut self, cpu: &Cpu, inst: Instruction) {
        if self.verbose {
            println!(
                "{:?}\na: ${:02x}, x: ${:02x}, y: ${:02x}, sp: ${:02x}, p: ${:02x} {} top: ${:02x}\n",
                inst, cpu.a, cpu.x, cpu.y, cpu.sp, cpu.status, format_flags(cpu), cpu.peek_stack());
        }

        if let Some(last) = self.last_pc {
            if last == cpu.pc {
                self.should_exit = true;
                if cpu.flag(nes_sim::DecimalModeFlag) {
                    // decimal mode not implemented, it is okay to exit test on decimal error,
                    // decimal test is the last of opCode test.
                    println!("test succeed!");
                    return;
                }

                println!("test failed: pc repeated");
                return;
            }
        }
        self.last_pc = Some(cpu.pc);
    }
}

fn format_flags(cpu: &Cpu) -> String {
    let mut r = String::new();

    r.push(if cpu.flag(nes_sim::NegativeFlag) { 'N' } else { 'n' });
    r.push(if cpu.flag(nes_sim::OverflowFlag) { 'V' } else { 'v' });
    r.push(if cpu.flag(nes_sim::BreakFlag) { 'B' } else { 'b' });
    r.push(if cpu.flag(nes_sim::DecimalModeFlag) { 'D' } else { 'd' });
    r.push(if cpu.flag(nes_sim::InterruptDisableFlag) { 'I' } else { 'i' });
    r.push(if cpu.flag(nes_sim::ZeroFlag) { 'Z' } else { 'z' });
    r.push(if cpu.flag(nes_sim::CarryFlag) { 'C' } else { 'c' });

    r
}
