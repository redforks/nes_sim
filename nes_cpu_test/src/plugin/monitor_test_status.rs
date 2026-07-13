use ansi_term::Color;
use nes_core::mcu::Mcu;
use nes_core::{Cpu, ExecuteResult, Plugin, SystemClock, SYSTEM_CYCLES_PER_PPU_CYCLE};

const RESET_WAIT_SYSTEM_CYCLES: u64 = 536_000 * SYSTEM_CYCLES_PER_PPU_CYCLE;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Status {
    Running,     // 0x80
    ShouldReset, // 0x81, // should reset cpu after 100ms
    Succeed,     // 0x00
    Failed(u8),  // failed with result code
    Unknown,     // status is not valid or not available
}

impl Status {
    fn parse<M: Mcu>(cpu: &Cpu<M>) -> Status {
        if cpu.peek_byte(0x6001) != 0xDE
            || cpu.peek_byte(0x6002) != 0xB0
            || cpu.peek_byte(0x6003) != 0x61
        {
            return Status::Unknown;
        }

        let status = cpu.peek_byte(0x6000);
        match status {
            0x80 => Status::Running,
            0x81 => Status::ShouldReset,
            0x00 => Status::Succeed,
            0x01..=0x7f => Status::Failed(status),
            _ => Status::Unknown,
        }
    }
}

#[derive(Default)]
pub struct MonitorTestStatus {
    cycles_request_reset: Option<u64>,
    cycles_last_reset: Option<u64>,
    should_reset: bool,
    exit_code: Option<u8>,
}

impl<M: Mcu> Plugin<M> for MonitorTestStatus {
    fn start(&mut self, _: &Cpu<M>, _: SystemClock) {}

    fn end(&mut self, cpu: &Cpu<M>, system_clock: SystemClock) {
        let status = Status::parse(cpu);
        let now = system_clock.cycles();
        self.should_reset = if let Some(cycles) = self.cycles_request_reset {
            // PPU clock is 5.320342 MHz, so 100ms is about 532,000 PPU cycles.
            if (now > cycles) && (now - cycles >= RESET_WAIT_SYSTEM_CYCLES) {
                self.cycles_request_reset = None;
                self.cycles_last_reset = Some(now);
                true
            } else {
                false
            }
        } else {
            false
        };

        self.exit_code = match status {
            Status::Succeed => Some(0),
            Status::Failed(r) => {
                eprintln!("test failed: {}", r);
                Some(r)
            }
            Status::ShouldReset => {
                if self.cycles_request_reset.is_none()
                    && self.cycles_last_reset.is_none_or(|c| now - c >= RESET_WAIT_SYSTEM_CYCLES)
                {
                    self.cycles_request_reset = Some(now);
                }
                None
            }
            _ => None,
        };
    }

    fn should_stop(&self) -> ExecuteResult {
        if self.should_reset {
            eprintln!("{}", Color::Yellow.paint("100ms waited, request reset"));
            return ExecuteResult::ShouldReset;
        }

        if let Some(exit_code) = self.exit_code {
            ExecuteResult::Stop(exit_code)
        } else {
            ExecuteResult::Continue
        }
    }
}
