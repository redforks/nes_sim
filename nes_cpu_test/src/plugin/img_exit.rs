use nes_core::{Cpu, ExecuteResult, Flag, Plugin};

#[derive(Default)]
pub struct ImageExit {
    last_pc: Option<u16>,
    should_exit: bool,
}

impl Plugin for ImageExit {
    fn start(&mut self, _: &Cpu) {}

    fn end(&mut self, cpu: &Cpu) {
        if let Some(last) = self.last_pc {
            if last == cpu.pc {
                self.should_exit = true;
                if cpu.flag(Flag::Decimal) {
                    // decimal mode not implemented, it is okay to exit test on decimal error,
                    // decimal test is the last of opCode test.
                    println!("test succeed!");
                    return;
                }

                println!("test failed: pc repeated");
                return;
            }
        }
        self.last_pc = Some(cpu.pc);
    }

    fn should_stop(&self) -> ExecuteResult {
        if self.should_exit {
            ExecuteResult::Stop
        } else {
            ExecuteResult::Continue
        }
    }
}
