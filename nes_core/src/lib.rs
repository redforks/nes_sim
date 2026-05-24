pub(crate) mod cpu;
pub mod ines;
pub mod machine;
pub mod mcu;
pub mod movie;
pub mod nes;
pub mod nes_machine;
pub mod render;

// Test utilities (private, for tests only)
#[cfg(test)]
mod test_utils;

use std::sync::atomic::{AtomicU64, Ordering};

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

    pub fn cpu_clock_phase(self) -> CpuClockPhase {
        match self.0 % SYSTEM_CYCLES_PER_CPU_CYCLE {
            0 => CpuClockPhase::First,
            2 => CpuClockPhase::Last,
            _ => CpuClockPhase::Middle,
        }
    }

    pub fn is_apu_get_clock(self) -> bool {
        self.is_even_cpu_cycle()
    }
}

static SYSTEM_CLOCK: AtomicU64 = AtomicU64::new(0);

pub const SYSTEM_CYCLES_PER_PPU_CYCLE: u64 = 1;
const PPU_CYCLES_PER_CPU_CYCLE: u64 = 3;
pub const SYSTEM_CYCLES_PER_CPU_CYCLE: u64 = SYSTEM_CYCLES_PER_PPU_CYCLE * PPU_CYCLES_PER_CPU_CYCLE;

#[cfg(test)]
fn set_system_cycles(cycles: u64) {
    SYSTEM_CLOCK.update(Ordering::SeqCst, Ordering::SeqCst, |_| cycles);
}

pub fn inc_system_clock() -> SystemClock {
    SystemClock(SYSTEM_CLOCK.fetch_add(1, Ordering::Relaxed).wrapping_add(1))
}

pub fn get_system_clock() -> SystemClock {
    SystemClock(SYSTEM_CLOCK.load(Ordering::Relaxed))
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
