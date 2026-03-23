use nes_core::mcu::Mcu;
use nes_core::{Cpu, ExecuteResult, Plugin};

mod simple_disassembly;

// Test ROM completion signature at $6001-$6003
const TEST_SIGNATURE: [u8; 3] = [0xDE, 0xB0, 0x61];
const TEST_RESULT_ADDR: u16 = 0x6000;
const TEST_SIGNATURE_ADDR: u16 = 0x6001;

pub struct ReportPlugin {
    verbose: bool,
    count: u32,
    last_result_check: u32,
    pending_result: Option<ExecuteResult>,

    cycles: usize,
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    p: u8,
}

impl ReportPlugin {
    pub fn new(quiet: bool) -> ReportPlugin {
        ReportPlugin {
            verbose: !quiet,
            count: 0,
            last_result_check: 0,
            pending_result: None,
            cycles: 0,
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0,
            p: 0,
        }
    }
}

impl<M: Mcu> Plugin<M> for ReportPlugin {
    fn start(&mut self, cpu: &mut Cpu<M>) {
        self.cycles += 1; // decode opcode takes one cycle
        self.a = cpu.a;
        self.x = cpu.x;
        self.y = cpu.y;
        self.pc = cpu.pc;
        self.sp = cpu.sp;
        self.p = cpu.status;
    }

    fn decoded(&mut self, cpu: &Cpu<M>) {
        self.cycles += cpu.microcodes_len()
    }

    fn end(&mut self, cpu: &mut Cpu<M>) {
        if self.verbose {
            let (op, low, high) = (
                cpu.peek_byte(self.pc),
                cpu.peek_byte(self.pc + 1),
                cpu.peek_byte(self.pc + 2),
            );
            let instruction = format!("{}", simple_disassembly::AsAsm(op, low, high));

            println!(
                "{:04X}  {:32}A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} CYC:{}",
                self.pc, instruction, self.a, self.x, self.y, self.p, self.sp, self.cycles
            );
        }

        // Check for CPU halt - if halted, the test is complete
        if cpu.is_halted() {
            let result = cpu.peek_byte(TEST_RESULT_ADDR);

            // Read text output from $6004+
            let mut text_output = String::new();
            let mut addr = 0x6004u16;
            loop {
                let ch = cpu.peek_byte(addr);
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
                cpu.peek_byte(TEST_SIGNATURE_ADDR),
                cpu.peek_byte(TEST_SIGNATURE_ADDR + 1),
                cpu.peek_byte(TEST_SIGNATURE_ADDR + 2),
            ];
            let result = cpu.peek_byte(TEST_RESULT_ADDR);

            // Print debug every 100000 instructions
            if self.verbose && self.count.is_multiple_of(100000) {
                // Read first 20 chars of text output
                let mut text = String::new();
                for i in 0..20 {
                    let ch = cpu.peek_byte(0x6004 + i);
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
