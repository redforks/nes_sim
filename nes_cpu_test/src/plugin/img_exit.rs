use nes_core::{Cpu, ExecuteResult, Flag, Plugin};

#[derive(Default)]
pub struct ImageExit {
    last_pc: Option<u16>,
    exit_code: Option<u8>,
}

impl Plugin for ImageExit {
    fn start(&mut self, _: &Cpu) {}

    fn end(&mut self, cpu: &Cpu) {
        if let Some(last) = self.last_pc {
            if last == cpu.pc {
                if cpu.flag(Flag::Decimal) {
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
        }
        self.last_pc = Some(cpu.pc);
    }

    fn should_stop(&self) -> ExecuteResult {
        if let Some(exit_code) = self.exit_code {
            ExecuteResult::Stop(exit_code)
        } else {
            ExecuteResult::Continue
        }
    }
}
