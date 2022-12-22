use nes_sim::mcu_mem::RamMcu;
use nes_sim::{Cpu, Flag, Plugin};
use std::env;
use std::fs::File;
use std::io::{Read, Write};

fn main() {
    env_logger::Builder::new()
        .filter(None, log::LevelFilter::Debug)
        .format(|buf, record| writeln!(buf, "{}", record.args()))
        .init();

    let args = env::args().collect::<Vec<_>>();
    let quiet = args.len() >= 2 && args[1] == "--quiet";

    let mut f = File::open("6502_functional_test.bin").unwrap();
    let mut ram = [0u8; 0x10000];
    assert_eq!(65536, f.read(&mut ram).unwrap());
    let mut cpu = Cpu::new(Box::new(RamMcu::new(ram)));
    cpu.reset();
    // the test rom reset position, not the start position.
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
        ReportPlugin {
            verbose: !quiet,
            count: 0,
            last_pc: None,
            should_exit: false,
        }
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

    fn end(&mut self, cpu: &Cpu) {
        if self.verbose {
            let flags = format_flags(cpu);
            let top = cpu.peek_stack();
            println!(
                "a: ${:02x}, x: ${:02x}, y: ${:02x}, sp: ${:02x}, p: ${:02x} {} top: ${:02x}\n",
                cpu.a, cpu.x, cpu.y, cpu.sp, cpu.status, flags, top
            );
        }

        if let Some(last) = self.last_pc {
            if last == cpu.pc {
                self.should_exit = true;
                if cpu.flag(Flag::Decimal) {
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

    r.push(if cpu.flag(Flag::Negative) { 'N' } else { 'n' });
    r.push(if cpu.flag(Flag::Overflow) { 'V' } else { 'v' });
    r.push(if cpu.flag(Flag::Break) { 'B' } else { 'b' });
    r.push(if cpu.flag(Flag::Decimal) { 'D' } else { 'd' });
    r.push(if cpu.flag(Flag::Interrupt) { 'I' } else { 'i' });
    r.push(if cpu.flag(Flag::Zero) { 'Z' } else { 'z' });
    r.push(if cpu.flag(Flag::Carry) { 'C' } else { 'c' });

    r
}
