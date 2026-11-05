use crate::bus::Bus;
use crate::mcu::Mcu;
use crate::{Cpu, EmptyPlugin, ExecuteResult, Plugin};

pub struct Machine<P, M: Mcu> {
    cpu: Cpu,
    /// The external bus the CPU drives; it owns the address-space `Mcu`
    /// (ADR-0015).
    bus: Bus<M>,
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
            cpu: Cpu::new(),
            bus: Bus::new(mcu),
            p,
        }
    }

    /// Execute one CPU instruction and return its result and cycle count.
    /// The executed dot is read from — and advanced by — the bus's master
    /// clock (ADR-0018).
    pub fn tick(&mut self) -> ExecuteResult {
        let control = self.cpu.tick(&mut self.bus, &mut self.p).control;
        self.bus.advance();
        control
    }

    pub fn reset(&mut self) {
        self.cpu.reset()
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.cpu.set_pc(pc);
    }

    /// Drain to instruction boundary via the CPU-only seam.
    ///
    /// Advances the bus's master clock by `queue.len()` dots (one per
    /// microcode) without ticking PPU/APU/DMA. For full interleaving on a
    /// `NesMachine`, use `NesMachine::run_to_instruction_boundary`.
    pub fn run_to_instruction_boundary(&mut self) {
        self.cpu
            .run_to_instruction_boundary(&mut self.bus, &mut self.p);
    }

    /// [`Self::run_to_instruction_boundary`] with an explicit plugin —
    /// setup-time callers drain with a bare plugin so the machine's real
    /// plugin does not observe the reset/drain instructions.
    pub fn drain_to_instruction_boundary<DrainPlugin: Plugin<M>>(
        &mut self,
        plugin: &mut DrainPlugin,
    ) {
        self.cpu.run_to_instruction_boundary(&mut self.bus, plugin);
    }

    pub fn mcu(&self) -> &M {
        self.bus.mcu()
    }

    pub fn mcu_mut(&mut self) -> &mut M {
        self.bus.mcu_mut()
    }

    pub fn cpu(&self) -> &Cpu {
        &self.cpu
    }

    pub fn cpu_mut(&mut self) -> &mut Cpu {
        &mut self.cpu
    }

    pub fn microcodes_empty(&self) -> bool {
        self.cpu.microcodes_empty()
    }
}

#[cfg(test)]
mod tests;
