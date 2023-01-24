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

impl<P: Plugin> Machine<P> {
    pub fn with_plugin(p: P, mcu: Box<dyn Mcu>) -> Self {
        Machine {
            // cpu: Cpu::new(Box::new(create_mcu(&ines))),
            cpu: Cpu::new(mcu),
            p,
        }
    }

    /// Run the machine for a single frame.
    /// `ms`: milliseconds elapsed since last frame.
    pub fn process_frame(&mut self, ms: f64) -> (&RgbaImage, ExecuteResult) {
        const CYCLES_PER_MS: u32 = 1789;
        const V_BLANK_CYCLES: u32 = 2273;
        let cycles = (ms * CYCLES_PER_MS as f64) as u32;
        let er = if cycles > V_BLANK_CYCLES {
            // TODO: start ppu vblank, trigger nmi if ppu.should_nmi()
            let ppu = self.cpu.mcu().get_ppu();
            ppu.set_v_blank(true);
            if ppu.should_nmi() {
                self.cpu.nmi();
            }
            self.run_ticks(V_BLANK_CYCLES);
            // TODO: end ppu vblank
            self.run_ticks(cycles - V_BLANK_CYCLES)
        } else {
            self.run_ticks(cycles)
        };
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
