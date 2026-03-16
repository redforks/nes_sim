use crate::mcu::Mcu;
use crate::{Cpu, EmptyPlugin, ExecuteResult, Plugin};

/// CPU cycles per frame: 262 scanlines × 341 PPU dots / 3 (PPU:CPU ratio) ≈ 29780.67
pub const CYCLES_PER_FRAME: f64 = 29780.5;

pub struct Machine<P> {
    cpu: Cpu,
    p: P,
}

impl Machine<EmptyPlugin> {
    pub fn new(mcu: Box<dyn Mcu>) -> Self {
        Self::with_plugin(EmptyPlugin(), mcu)
    }
}

impl<P: Plugin> Machine<P> {
    pub fn with_plugin(p: P, mcu: Box<dyn Mcu>) -> Self {
        let mut machine = Machine {
            cpu: Cpu::new(mcu),
            p,
        };
        machine.cpu.reset();
        machine
    }

    /// Run the machine for a single frame.
    /// Executes CPU instructions until VBlank is detected (one full frame of PPU output).
    /// A frame is ~29780.5 CPU cycles (262 scanlines × 341 dots / 3 PPU:CPU ratio).
    pub fn process_frame(&mut self) -> ExecuteResult {
        // Run enough ticks to cover one full frame
        // 262 scanlines * 341 dots = 89342 PPU dots per frame
        // 89342 / 3 ≈ 29780.67 CPU cycles per frame
        // We use a slightly larger number to ensure we complete the frame
        const MAX_TICKS_PER_FRAME: u32 = 29781;

        self.run_ticks(MAX_TICKS_PER_FRAME)
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
}

#[cfg(test)]
mod tests;
