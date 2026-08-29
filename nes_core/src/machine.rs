use crate::mcu::Mcu;
use crate::{Cpu, EmptyPlugin, ExecuteResult, Plugin, SystemClock};

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
        Machine {
            cpu: Cpu::new(mcu),
            p,
        }
    }

    /// Execute one CPU instruction and return its result and cycle count.
    pub fn tick(&mut self, clock: SystemClock) -> ExecuteResult {
        self.cpu.tick(&mut self.p, clock).0
    }

    pub fn reset(&mut self) {
        self.cpu.reset()
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.cpu.set_pc(pc);
    }

    /// Drain to instruction boundary via the CPU-only seam.
    ///
    /// Advances `clock` by `queue.len()` dots (one per microcode) without
    /// ticking PPU/APU/DMA. For full interleaving on a `NesMachine`, use
    /// `NesMachine::run_to_instruction_boundary`.
    pub fn run_to_instruction_boundary(&mut self, clock: &mut SystemClock) {
        let (cpu, p) = (&mut self.cpu, &mut self.p);
        cpu.run_to_instruction_boundary(p, clock);
    }

    pub fn mcu(&self) -> &M {
        self.cpu.mcu()
    }

    pub fn mcu_mut(&mut self) -> &mut M {
        self.cpu.mcu_mut()
    }

    pub fn cpu(&self) -> &Cpu<M> {
        &self.cpu
    }

    pub fn cpu_mut(&mut self) -> &mut Cpu<M> {
        &mut self.cpu
    }

    pub fn microcodes_empty(&self) -> bool {
        self.cpu.microcodes_empty()
    }
}

#[cfg(test)]
mod tests;
