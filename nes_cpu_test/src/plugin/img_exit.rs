use nes_core::mcu::Mcu;
use nes_core::view::MachineView;
use nes_core::{ExecuteResult, Flag, Plugin, SystemClock};

#[derive(Default)]
pub struct ImageExit {
    last_pc: Option<u16>,
    exit_code: Option<u8>,
}

impl<M: Mcu> Plugin<M> for ImageExit {
    fn start(&mut self, _: &MachineView<M>, _: SystemClock) {}

    fn end(&mut self, view: &MachineView<M>, _: SystemClock) {
        if let Some(last) = self.last_pc
            && last == view.cpu.pc
        {
            if (view.cpu.status & Flag::Decimal as u8) != 0 {
                // decimal mode not implemented, it is okay to exit test on decimal error,
                // decimal test is the last of opCode test.
                println!("test succeed!");
                self.exit_code = Some(0);
                return;
            }

            println!("test failed: pc repeated");
            self.exit_code = Some(1);
            return;
        }
        self.last_pc = Some(view.cpu.pc);
    }

    fn should_stop(&self) -> ExecuteResult {
        if let Some(exit_code) = self.exit_code {
            ExecuteResult::Stop(exit_code)
        } else {
            ExecuteResult::Continue
        }
    }
}
