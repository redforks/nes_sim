use ansi_term::Color;
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
    fn parse(cpu: &Cpu) -> Status {
        if cpu.read_byte(0x6001) != 0xDE
            || cpu.read_byte(0x6002) != 0xB0
            || cpu.read_byte(0x6003) != 0x61
        {
            return Status::Unknown;
        }

        let status = cpu.read_byte(0x6000);
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
    should_reset: u16,
    exit_code: Option<u8>,
    last_status: Option<Status>,
}

impl Plugin for MonitorTestStatus {
    fn start(&mut self, _: &Cpu) {}

    fn end(&mut self, cpu: &Cpu) {
        let status = Status::parse(cpu);
        if status == Status::ShouldReset && self.should_reset > 0 {
            self.should_reset -= 1;

            return;
        }

        if let Some(last_status) = &self.last_status {
            if *last_status == status {
                return;
            }
        }
        self.last_status = Some(status);

        self.exit_code = match status {
            Status::Succeed => Some(0),
            Status::Failed(r) => {
                eprintln!("test failed: {}", r);
                Some(r)
            }
            Status::ShouldReset => {
                self.should_reset = 7000;
                None
            }
            _ => None,
        };
    }

    fn should_stop(&self) -> ExecuteResult {
        if self.should_reset == 1 {
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
