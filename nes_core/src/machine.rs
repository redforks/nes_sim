use crate::mcu::Mcu;
use crate::{Cpu, EmptyPlugin, ExecuteResult, Plugin};

pub struct Machine<P, M: Mcu> {
    cpu: Cpu<M>,
    p: P,
}

impl<M: Mcu> Machine<EmptyPlugin<M>, M> {
    pub fn new(mcu: M) -> Self {
        Self::with_plugin(EmptyPlugin::new(), mcu)
    }
}

impl<P: Plugin<M>, M: Mcu> Machine<P, M> {
    pub fn with_plugin(p: P, mcu: M) -> Self {
        let mut machine = Machine {
            cpu: Cpu::new(mcu),
            p,
        };
        machine.cpu.reset();
        machine
    }

    pub fn run_ticks(&mut self, ticks: u32) -> ExecuteResult {
        for _ in 0..ticks {
            match self.cpu.tick(&mut self.p) {
                ExecuteResult::Continue => continue,
                other => return other,
            }
        }
        ExecuteResult::Continue
    }

    pub fn reset(&mut self) {
        self.cpu.reset()
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.cpu.pc = pc;
    }

    pub fn mcu(&self) -> &M {
        self.cpu.mcu()
    }
}

#[cfg(test)]
mod tests;
