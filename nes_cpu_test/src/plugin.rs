use nes_core::{Cpu, Plugin};

mod console;
mod detect_dead_loop;
mod monitor_test_status;
mod report;
pub use console::*;
pub use detect_dead_loop::*;
pub use monitor_test_status::*;
pub use report::*;

pub struct CompositePlugin(Vec<Box<dyn Plugin>>);

impl CompositePlugin {
    pub fn new(plugins: Vec<Box<dyn Plugin>>) -> CompositePlugin {
        CompositePlugin(plugins)
    }
}

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
