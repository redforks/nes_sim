use nes_core::mcu::Mcu;
use nes_core::nes::NesMcu;
use nes_core::nes::apu::AudioDriver;
use nes_core::render::Render;
use nes_core::{Cpu, ExecuteResult, Plugin};

mod simple_disassembly;

// Test ROM completion signature at $6001-$6003
const TEST_SIGNATURE: [u8; 3] = [0xDE, 0xB0, 0x61];
const TEST_RESULT_ADDR: u16 = 0x6000;
const TEST_SIGNATURE_ADDR: u16 = 0x6001;

/// if quiet, do not call inner Plugin,
struct QuietPlugin<P> {
    quiet: bool,
    inner: P,
}

impl<P> QuietPlugin<P> {
    pub fn new(quiet: bool, inner: P) -> Self {
        Self { quiet, inner }
    }
}

impl<M: Mcu, P: Plugin<M>> Plugin<M> for QuietPlugin<P> {
    fn start(&mut self, cpu: &mut Cpu<M>) {
        if !self.quiet {
            self.inner.start(cpu);
        }
    }

    fn end(&mut self, cpu: &mut Cpu<M>) {
        if !self.quiet {
            self.inner.end(cpu);
        }
    }

    fn should_stop(&self) -> ExecuteResult {
        if self.quiet {
            ExecuteResult::Continue
        } else {
            self.inner.should_stop()
        }
    }
}

pub struct ReportPlugin {
    start_cycles: usize,
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    p: u8,
}

impl ReportPlugin {
    fn inner_new() -> Self {
        Self {
            start_cycles: 0,
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0,
            p: 0,
        }
    }

    pub fn create<M: Mcu>(quiet: bool) -> impl Plugin<M> {
        QuietPlugin::new(quiet, ReportPlugin::inner_new())
    }

    fn output<M: Mcu>(&mut self, cpu: &mut Cpu<M>, ppu: &str) {
        let (op, low, high) = (
            cpu.peek_byte(self.pc),
            cpu.peek_byte(self.pc + 1),
            cpu.peek_byte(self.pc + 2),
        );
        let instruction = format!("{}", simple_disassembly::AsAsm(op, self.pc, low, high));

        println!(
            "{:04X}  {:32}A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} {}CYC:{}",
            self.pc, instruction, self.a, self.x, self.y, self.p, self.sp, ppu, self.start_cycles
        );
    }
}

impl<M: Mcu> Plugin<M> for ReportPlugin {
    fn start(&mut self, cpu: &mut Cpu<M>) {
        self.start_cycles = cpu.total_cycles();
        self.a = cpu.a;
        self.x = cpu.x;
        self.y = cpu.y;
        self.pc = cpu.pc;
        self.sp = cpu.sp;
        self.p = cpu.status;
    }

    fn end(&mut self, cpu: &mut Cpu<M>) {
        self.output(cpu, "")
    }
}

/// Check nestest.nes test result,
pub struct ReportNesTestResult {
    instruction_executed: usize,
    // saved nes memory 02h and 03h
    result: Option<u16>,
}

impl ReportNesTestResult {
    pub fn new() -> Self {
        Self {
            result: None,
            instruction_executed: 0,
        }
    }
}

impl<M: Mcu> Plugin<M> for ReportNesTestResult {
    fn start(&mut self, _cpu: &mut Cpu<M>) {}

    fn end(&mut self, cpu: &mut Cpu<M>) {
        self.instruction_executed += 1;
        if cpu.total_cycles() >= 26560 {
            // 26560 is the cycle count after the last instruction executed, 26554 is the cycle count before the last instruction
            let low = cpu.peek_byte(0x0002) as u16;
            let high = cpu.peek_byte(0x0003) as u16;
            self.result = Some((high << 8) | low);
        }
    }

    fn should_stop(&self) -> ExecuteResult {
        if let Some(result) = self.result {
            let low = result as u8;
            if self.instruction_executed != 8992 {
                // Actually 8991 instructions, but when start cpu, we treat RESET action as an instruction
                println!(
                    "Warning: nestest executed {} instructions, expected 8992",
                    self.instruction_executed
                );
            }
            if low == 0 {
                println!("nestest PASSED");
                ExecuteResult::Stop(0)
            } else {
                println!("nestest FAILED with code {:03X}", result);
                ExecuteResult::Stop(1)
            }
        } else {
            ExecuteResult::Continue
        }
    }
}

// Check for CPU halt - if halted, the test is complete
pub struct ExitTestPlugin {
    count: u32,
    last_result_check: u32,
    pending_result: Option<ExecuteResult>,
}

impl ExitTestPlugin {
    pub fn new() -> Self {
        Self {
            count: 0,
            last_result_check: 0,
            pending_result: None,
        }
    }
}

impl<M: Mcu> Plugin<M> for ExitTestPlugin {
    fn start(&mut self, _cpu: &mut Cpu<M>) {}

    fn end(&mut self, cpu: &mut Cpu<M>) {
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

            if sig == TEST_SIGNATURE && result != 0x80 {
                let msg = if result == 0 {
                    "PASSED".to_string()
                } else {
                    format!("FAILED #{}", result)
                };
                println!("Test completed with result: {}", msg);

                self.pending_result = Some(ExecuteResult::Stop(result));
            }
        }
    }
}

pub struct NesReportPlugin<R, A> {
    inner: ReportPlugin,
    timing: (u16, u16),
    _phantom: std::marker::PhantomData<(R, A)>,
}

impl<R: Render, A: AudioDriver> NesReportPlugin<R, A> {
    pub fn create(quiet: bool) -> impl Plugin<NesMcu<R, A>> {
        QuietPlugin::new(
            quiet,
            NesReportPlugin {
                inner: ReportPlugin::inner_new(),
                timing: (0, 0),
                _phantom: std::marker::PhantomData,
            },
        )
    }
}

impl<R: Render, A: AudioDriver> Plugin<NesMcu<R, A>> for NesReportPlugin<R, A> {
    fn start(&mut self, cpu: &mut Cpu<NesMcu<R, A>>) {
        self.inner.start(cpu);
        self.timing = cpu.mcu().ppu_timing();
    }

    fn end(&mut self, cpu: &mut Cpu<NesMcu<R, A>>) {
        let ppu = format!("PPU:{:3},{:3} ", self.timing.0, self.timing.1);
        self.inner.output(cpu, &ppu);
    }
}
