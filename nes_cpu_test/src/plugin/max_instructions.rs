use nes_core::mcu::Mcu;
use nes_core::{Cpu, ExecuteResult, Plugin, SystemClock};

/// Plugin that stops execution after a maximum number of instructions.
#[derive(Debug, Default)]
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
    fn start(&mut self, _: &Cpu<M>, _: SystemClock) {}

    fn end(&mut self, _cpu: &Cpu<M>, _: SystemClock) {
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
