use nes_core::mcu::Mcu;
use nes_core::{Cpu, ExecuteResult, Plugin};

/// Plugin that stops execution after a maximum number of instructions.
#[derive(Debug)]
pub struct MaxInstructions {
    max: u64,
    count: u64,
}

impl MaxInstructions {
    pub fn new(max: u64) -> Self {
        MaxInstructions { max, count: 0 }
    }
}

impl<M: Mcu> Plugin<M> for MaxInstructions {
    fn start(&mut self, _: &mut Cpu<M>) {}

    fn end(&mut self, _cpu: &mut Cpu<M>, _cycles: u8) {
        self.count = self.count.saturating_add(1);
        if self.count > self.max {
            // nothing to do here; should_stop will return Stop
        }
    }

    fn should_stop(&self) -> ExecuteResult {
        if self.count > self.max {
            ExecuteResult::Stop(1)
        } else {
            ExecuteResult::Continue
        }
    }
}

impl Default for MaxInstructions {
    fn default() -> Self {
        MaxInstructions { max: 0, count: 0 }
    }
}
