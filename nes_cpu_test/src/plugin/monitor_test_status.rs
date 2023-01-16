use nes_core::{Cpu, ExecuteResult, Plugin};

#[derive(Debug)]
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
}

impl Plugin for MonitorTestStatus {
    fn start(&mut self, _: &Cpu) {}

    fn end(&mut self, cpu: &Cpu) {
        let status = Status::parse(cpu);
        self.exit_code = match status {
            Status::Succeed => Some(0),
            Status::Failed(r) => {
                eprintln!("test failed: {}", r);
                Some(r)
            }
            Status::ShouldReset => None,
            _ => None,
        };

        if self.should_reset > 0 {
            self.should_reset -= 1;
        } else {
            self.should_reset = match status {
                Status::ShouldReset => {
                    eprintln!("should reset");
                    100
                }
                _ => 0,
            };
        }
    }

    fn should_stop(&self) -> ExecuteResult {
        if self.should_reset == 1 {
            return ExecuteResult::ShouldReset;
        }

        if let Some(exit_code) = self.exit_code {
            ExecuteResult::Stop(exit_code)
        } else {
            ExecuteResult::Continue
        }
    }
}
