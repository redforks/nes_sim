use nes_core::{Cpu, Plugin};

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
            || cpu.read_byte(0x6003) != 0x01
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
    should_stop: bool,
}

impl Plugin for MonitorTestStatus {
    fn start(&mut self, _: &Cpu) {}

    fn end(&mut self, cpu: &Cpu) {
        let status = Status::parse(cpu);
        format!("test status: {:?}", status);
        self.should_stop = match status {
            Status::Succeed => true,
            Status::Failed(_) => true,
            Status::ShouldReset => true,
            _ => false,
        };
    }

    fn should_stop(&self) -> bool {
        self.should_stop
    }
}
