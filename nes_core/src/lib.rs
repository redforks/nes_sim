mod cpu;
pub mod ines;
pub mod machine;
pub mod mcu;
pub mod nes;
pub mod nes_machine;
pub mod render;

// Test utilities (private, for tests only)
#[cfg(test)]
mod test_utils;

use std::cell::Cell;

pub use cpu::*;

#[derive(Debug, Default, Clone, Copy)]
pub struct SystemClock(u64);

impl SystemClock {
    pub fn is_apu_clock(self) -> bool {
        self.cpu_clock_phase() == CpuClockPhase::Last
    }

    pub fn is_cpu_clock(self) -> bool {
        self.0 % SYSTEM_CYCLES_PER_CPU_CYCLE == 0
    }

    pub fn is_even_cpu_cycle(self) -> bool {
        // self.0 / SYSTEM_CYCLES_PER_CPU_CYCLE % 2 == 0
        self.0 % (SYSTEM_CYCLES_PER_CPU_CYCLE * 2) < SYSTEM_CYCLES_PER_CPU_CYCLE
    }

    pub const fn is_ppu_clock(self) -> bool {
        // self.0 % SYSTEM_CYCLES_PER_PPU_CYCLE == 0
        true
    }

    pub fn cpu_clock_phase(self) -> CpuClockPhase {
        match self.0 % SYSTEM_CYCLES_PER_CPU_CYCLE {
            0 => CpuClockPhase::First,
            2 => CpuClockPhase::Last,
            _ => CpuClockPhase::Middle,
        }
    }
}

thread_local! {
    static SYSTEM_CLOCK: Cell<SystemClock> = Cell::new(Default::default());
}

pub const SYSTEM_CYCLES_PER_PPU_CYCLE: u64 = 1;
const PPU_CYCLES_PER_CPU_CYCLE: u64 = 3;
pub const SYSTEM_CYCLES_PER_CPU_CYCLE: u64 = SYSTEM_CYCLES_PER_PPU_CYCLE * PPU_CYCLES_PER_CPU_CYCLE;

#[cfg(test)]
fn set_system_cycles(cycles: u64) {
    SYSTEM_CLOCK.with(|sc| sc.set(SystemClock(cycles)));
}

pub fn inc_system_clock() -> SystemClock {
    SYSTEM_CLOCK.with(|sc| {
        let r = SystemClock(sc.get().0.wrapping_add(1));
        sc.set(r);
        r
    })
}

pub fn get_system_clock() -> SystemClock {
    SYSTEM_CLOCK.with(|sc| sc.get())
}

pub fn get_system_cycles() -> u64 {
    get_system_clock().0
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CpuClockPhase {
    First,
    // should ignored
    Middle,
    Last,
}
