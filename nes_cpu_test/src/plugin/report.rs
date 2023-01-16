use nes_core::{Cpu, Flag, Plugin};

pub struct ReportPlugin {
    verbose: bool,
    count: u32,
}

impl ReportPlugin {
    pub fn new(quiet: bool) -> ReportPlugin {
        ReportPlugin {
            verbose: !quiet,
            count: 0,
        }
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
    }
}

fn format_flags(cpu: &Cpu) -> String {
    let mut r = String::new();

    r.push(if cpu.flag(Flag::Negative) { 'N' } else { 'n' });
    r.push(if cpu.flag(Flag::Overflow) { 'V' } else { 'v' });
    r.push(if cpu.flag(Flag::Break) { 'B' } else { 'b' });
    r.push(if cpu.flag(Flag::Decimal) { 'D' } else { 'd' });
    r.push(if cpu.flag(Flag::InterruptDisabled) {
        'I'
    } else {
        'i'
    });
    r.push(if cpu.flag(Flag::Zero) { 'Z' } else { 'z' });
    r.push(if cpu.flag(Flag::Carry) { 'C' } else { 'c' });

    r
}
