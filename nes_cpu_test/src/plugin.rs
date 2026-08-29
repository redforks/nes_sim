use nes_core::mcu::Mcu;
use nes_core::view::MachineView;
use nes_core::{ExecuteResult, Plugin, SystemClock};
mod console;
mod detect_dead_loop;
mod frame_png_dump;
mod img_exit;
mod max_instructions;
mod monitor_test_status;
mod nametable_console;
pub(crate) mod png_frame_match;
mod report;
mod timeout;

pub use console::*;
pub use detect_dead_loop::*;
pub use frame_png_dump::*;
pub use img_exit::*;
pub use max_instructions::*;
pub use monitor_test_status::*;
pub use nametable_console::*;
pub use png_frame_match::*;
pub use report::*;
pub use timeout::*;

pub struct CompositePlugin<M: Mcu>(Vec<Box<dyn Plugin<M>>>);

impl<M: Mcu> CompositePlugin<M> {
    pub fn new(plugins: Vec<Box<dyn Plugin<M>>>) -> CompositePlugin<M> {
        CompositePlugin(plugins)
    }
}

impl<M: Mcu> Plugin<M> for CompositePlugin<M> {
    fn start(&mut self, view: &MachineView<M>, system_clock: SystemClock) {
        for p in self.0.iter_mut() {
            p.start(view, system_clock);
        }
    }

    fn end(&mut self, view: &MachineView<M>, system_clock: SystemClock) {
        for p in self.0.iter_mut() {
            p.end(view, system_clock);
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
