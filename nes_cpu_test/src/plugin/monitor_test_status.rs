use ansi_term::Color;
use nes_core::mcu::Mcu;
use nes_core::{Cpu, ExecuteResult, Plugin};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Status {
    Running,     // 0x80
    ShouldReset, // 0x81, // should reset cpu after 100ms
    Succeed,     // 0x00
    Failed(u8),  // failed with result code
    Unknown,     // status is not valid or not available
}

impl Status {
    fn parse<M: Mcu>(cpu: &mut Cpu<M>) -> Status {
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
    cycles_request_reset: Option<usize>,
    cycles_last_reset: Option<usize>,
    should_reset: bool,
    exit_code: Option<u8>,
}

impl<M: Mcu> Plugin<M> for MonitorTestStatus {
    fn start(&mut self, _: &mut Cpu<M>) {}

    fn end(&mut self, cpu: &mut Cpu<M>) {
        let status = Status::parse(cpu);
        self.should_reset = if let Some(cycles) = self.cycles_request_reset {
            // ppu clock is 5.320342 MHz, 100ms is about 532,000 cycles
            if (cpu.total_cycles() > cycles) && (cpu.total_cycles() - cycles >= 536_000) {
                self.cycles_request_reset = None;
                self.cycles_last_reset = Some(cpu.total_cycles());
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
                    && self
                        .cycles_last_reset
                        .map_or(true, |c| cpu.total_cycles() - c >= 536_000)
                {
                    self.cycles_request_reset = Some(cpu.total_cycles());
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
