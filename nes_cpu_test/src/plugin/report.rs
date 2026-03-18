use nes_core::mcu::Mcu;
use nes_core::{Cpu, ExecuteResult, Flag, Plugin};

// Test ROM completion signature at $6001-$6003
const TEST_SIGNATURE: [u8; 3] = [0xDE, 0xB0, 0x61];
const TEST_RESULT_ADDR: u16 = 0x6000;
const TEST_SIGNATURE_ADDR: u16 = 0x6001;

pub struct ReportPlugin {
    verbose: bool,
    count: u32,
    last_result_check: u32,
    pending_result: Option<ExecuteResult>,
}

impl ReportPlugin {
    pub fn new(quiet: bool) -> ReportPlugin {
        ReportPlugin {
            verbose: !quiet,
            count: 0,
            last_result_check: 0,
            pending_result: None,
        }
    }
}

impl<M: Mcu> Plugin<M> for ReportPlugin {
    fn start(&mut self, cpu: &mut Cpu<M>) {
        self.count += 1;

        if self.verbose {
            // Read the opcode and next few bytes for context
            let op = cpu.read_byte(cpu.pc);
            let b1 = cpu.read_byte(cpu.pc.wrapping_add(1));
            let b2 = cpu.read_byte(cpu.pc.wrapping_add(2));
            println!(
                "[{}] pc: ${:04x}  [{:02x} {:02x} {:02x}]",
                self.count, cpu.pc, op, b1, b2
            );
        }
    }

    fn end(&mut self, cpu: &mut Cpu<M>, _cycles: u8) {
        if self.verbose {
            let flags = format_flags(cpu);
            let top = cpu.peek_stack();
            println!(
                "a: ${:02x}, x: ${:02x}, y: ${:02x}, sp: ${:02x}, p: ${:02x} {} top: ${:02x}\n",
                cpu.a, cpu.x, cpu.y, cpu.sp, cpu.status, flags, top
            );
        }

        // Check for CPU halt - if halted, the test is complete
        if cpu.is_halted() {
            let result = cpu.read_byte(TEST_RESULT_ADDR);

            // Read text output from $6004+
            let mut text_output = String::new();
            let mut addr = 0x6004u16;
            loop {
                let ch = cpu.read_byte(addr);
                if ch == 0 {
                    break;
                }
                if (0x20..0x7f).contains(&ch) {
                    text_output.push(ch as char);
                }
                addr += 1;
                if addr > 0x6100 {
                    break; // Safety limit
                }
            }

            if self.verbose {
                let msg = if result == 0 {
                    "PASSED".to_string()
                } else if result == 0x80 {
                    "FAILED (no result written)".to_string()
                } else {
                    format!("FAILED #{}", result)
                };
                println!("CPU halted with result: {}", msg);
                if !text_output.is_empty() {
                    println!("Test output: {}", text_output);
                }
            } else if !text_output.is_empty() {
                // In quiet mode, just print the test output
                println!("{}", text_output);
            }

            // If result is still 0x80, treat as failure
            self.pending_result =
                Some(ExecuteResult::Stop(if result == 0x80 { 1 } else { result }));
            return;
        }

        // Check for test completion every 1000 instructions
        if self.count >= self.last_result_check + 1000 {
            self.last_result_check = self.count;

            // Always check $6000 memory
            let sig = [
                cpu.read_byte(TEST_SIGNATURE_ADDR),
                cpu.read_byte(TEST_SIGNATURE_ADDR + 1),
                cpu.read_byte(TEST_SIGNATURE_ADDR + 2),
            ];
            let result = cpu.read_byte(TEST_RESULT_ADDR);

            // Print debug every 100000 instructions
            if self.verbose && self.count.is_multiple_of(100000) {
                // Read first 20 chars of text output
                let mut text = String::new();
                for i in 0..20 {
                    let ch = cpu.read_byte(0x6004 + i);
                    if ch == 0 {
                        break;
                    }
                    if (0x20..0x7f).contains(&ch) {
                        text.push(ch as char);
                    }
                }
                eprintln!(
                    "DEBUG [{}]: sig={:02x?} result=${:02x} text='{}'",
                    self.count, sig, result, text
                );
            }

            if sig == TEST_SIGNATURE && result != 0x80 {
                if self.verbose {
                    let msg = if result == 0 {
                        "PASSED".to_string()
                    } else {
                        format!("FAILED #{}", result)
                    };
                    println!("Test completed with result: {}", msg);
                }
                self.pending_result = Some(ExecuteResult::Stop(result));
            }
        }
    }

    fn should_stop(&self) -> ExecuteResult {
        self.pending_result.unwrap_or(ExecuteResult::Continue)
    }
}

fn format_flags<M: Mcu>(cpu: &Cpu<M>) -> String {
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
