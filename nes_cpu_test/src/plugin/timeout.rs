use std::time::Duration;

use nes_core::{ExecuteResult, Plugin, mcu::Mcu};

/// Cpu plugin that stops the test if it runs for too long.
pub struct Timeout {
    duration: Duration,
    start_time: Option<std::time::Instant>,
}

impl Timeout {
    pub fn new(duration: Duration) -> Self {
        Self {
            duration,
            start_time: None,
        }
    }
}

impl<M: Mcu> Plugin<M> for Timeout {
    fn start(&mut self, _cpu: &mut nes_core::Cpu<M>) {
        if self.start_time.is_none() {
            self.start_time = Some(std::time::Instant::now());
        }
    }

    fn end(&mut self, _cpu: &mut nes_core::Cpu<M>) {}

    fn should_stop(&self) -> ExecuteResult {
        if let Some(start_time) = self.start_time {
            if start_time.elapsed() > self.duration {
                eprintln!("Test timed out after {:?}", self.duration);
                return ExecuteResult::Stop(2);
            }
        }
        ExecuteResult::Continue
    }
}
