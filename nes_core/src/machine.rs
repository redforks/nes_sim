use crate::mcu::Mcu;
use crate::{Cpu, EmptyPlugin, ExecuteResult, Plugin};
use image::RgbaImage;

pub struct Machine<P> {
    cpu: Cpu,
    p: P,
}

impl Machine<EmptyPlugin> {
    pub fn new(mcu: Box<dyn Mcu>) -> Self {
        Self::with_plugin(EmptyPlugin(), mcu)
    }
}

pub const CYCLES_PER_FRAME: f64 = 29780.5;
pub const CYCLES_PER_MS: f64 = CYCLES_PER_FRAME / 100.0 * 60.0;
pub const V_BLANK_CYCLES: f64 = 2273.3333333;

impl<P: Plugin> Machine<P> {
    pub fn with_plugin(p: P, mcu: Box<dyn Mcu>) -> Self {
        let mut machine = Machine {
            // cpu: Cpu::new(Box::new(create_mcu(&ines))),
            cpu: Cpu::new(mcu),
            p,
        };
        machine.cpu.reset();
        machine
    }

    /// Run the machine for a single frame.
    /// `ms`: milliseconds elapsed since last frame.
    pub fn process_frame(&mut self, ms: f64) -> (&RgbaImage, ExecuteResult) {
        let cycles = (ms * CYCLES_PER_MS) as u32;

        let er = self.run_ticks(cycles);

        (self.cpu.mcu().get_machine_mcu().render(), er)
    }

    pub fn run_ticks(&mut self, ticks: u32) -> ExecuteResult {
        for _ in 0..ticks {
            match self.cpu.clock_tick(&mut self.p) {
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
