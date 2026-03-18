use nes_core::mcu::Mcu;
use nes_core::{Cpu, ExecuteResult, Plugin};

mod console;
mod detect_dead_loop;
mod img_exit;
mod monitor_test_status;
mod report;
pub use console::*;
pub use detect_dead_loop::*;
pub use img_exit::*;
pub use monitor_test_status::*;
pub use report::*;

pub struct CompositePlugin<M: Mcu>(Vec<Box<dyn Plugin<M>>>);

impl<M: Mcu> CompositePlugin<M> {
    pub fn new(plugins: Vec<Box<dyn Plugin<M>>>) -> CompositePlugin<M> {
        CompositePlugin(plugins)
    }
}

impl<M: Mcu> Plugin<M> for CompositePlugin<M> {
    fn start(&mut self, cpu: &Cpu<M>) {
        for p in self.0.iter_mut() {
            p.start(cpu);
        }
    }

    fn end(&mut self, cpu: &Cpu<M>, cycles: u8) {
        for p in self.0.iter_mut() {
            p.end(cpu, cycles);
        }
    }

    fn should_stop(&self) -> ExecuteResult {
        for p in self.0.iter() {
            let r = p.should_stop();
            if r != ExecuteResult::Continue {
                return r;
            }
        }
        ExecuteResult::Continue
    }
}
