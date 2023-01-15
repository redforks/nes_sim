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
            0x01..=0xff => Status::Failed(status),
            _ => Status::Unknown,
        }
    }
}

pub struct MonitorTestStatus();

impl Plugin for MonitorTestStatus {
    fn start(&mut self, _: &Cpu) {}

    fn end(&mut self, cpu: &Cpu) {
        let status = Status::parse(cpu);
        format!("test status: {:?}", status);
    }

    fn should_stop(&self) -> bool {
        todo!()
    }
}
