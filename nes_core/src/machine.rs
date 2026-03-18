use crate::mcu::Mcu;
use crate::{Cpu, EmptyPlugin, ExecuteResult, Plugin};

/// Safety limit: maximum CPU instruction ticks per `process_frame()` call.
/// Two full frames worth of ticks; prevents an infinite loop if VBlank never fires
/// (e.g. when the PPU is not connected or NES hardware is not present).
const MAX_TICKS_PER_FRAME: u32 = 60000;

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

    /// Run the machine for a single frame.
    ///
    /// Executes CPU instructions until the PPU enters VBlank (scanline 241, dot 1),
    /// which marks the natural end of a rendered frame. This is the correct approach
    /// because the PPU has finished rendering all 240 visible scanlines at that point.
    ///
    /// If VBlank does not occur within `MAX_TICKS_PER_FRAME` instruction ticks
    /// (safety guard for MCUs without a real PPU), the function returns early.
    ///
    /// A frame is ~29780.5 CPU cycles (262 scanlines × 341 dots / 3 PPU:CPU ratio).
    pub fn process_frame(&mut self) -> ExecuteResult {
        for _ in 0..MAX_TICKS_PER_FRAME {
            match self.cpu.tick(&mut self.p) {
                ExecuteResult::Continue => {}
                other => return other,
            }
            if self.cpu.mcu().take_vblank() {
                return ExecuteResult::Continue;
            }
        }
        ExecuteResult::Continue
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
