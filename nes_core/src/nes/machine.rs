use crate::ines::INesFile;
use crate::nes::create_mcu;
use crate::{Cpu, EmptyPlugin};

pub struct Machine {
    cpu: Cpu,
}

impl Machine {
    pub fn new(ines: INesFile) -> Self {
        Machine {
            cpu: Cpu::new(Box::new(create_mcu(&ines))),
        }
    }

    /// Run the machine for a single frame.
    /// `ms`: milliseconds elapsed since last frame.
    pub fn process_frame(&mut self, ms: f64) {
        const CYCLES_PER_MS: u32 = 1789;
        const V_BLANK_CYCLES: u32 = 2273;
        let cycles = (ms * CYCLES_PER_MS as f64) as u32;
        if cycles > V_BLANK_CYCLES {
            // TODO: start ppu vblank, trigger nmi if ppu.should_nmi()
            self.run_ticks(V_BLANK_CYCLES);
            // TODO: end ppu vblank
            self.run_ticks(cycles - V_BLANK_CYCLES);
        } else {
            self.run_ticks(cycles);
        }
    }

    fn run_ticks(&mut self, ticks: u32) {
        let mut plugin = EmptyPlugin {};
        for _ in 0..ticks {
            self.cpu.clock_tick(&mut plugin);
        }
    }
}
