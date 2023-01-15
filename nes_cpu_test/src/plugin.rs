use nes_core::{Cpu, Plugin};

mod console;
mod monitor_test_status;
mod report;
pub use console::*;
pub use report::*;

pub struct CompositePlugin(Vec<Box<dyn Plugin>>);

impl Plugin for CompositePlugin {
    fn start(&mut self, cpu: &Cpu) {
        for p in self.0.iter_mut() {
            p.start(cpu);
        }
    }

    fn end(&mut self, cpu: &Cpu) {
        for p in self.0.iter_mut() {
            p.end(cpu);
        }
    }

    fn should_stop(&self) -> bool {
        self.0.iter().any(|p| p.should_stop())
    }
}
