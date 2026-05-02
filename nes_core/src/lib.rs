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

thread_local! {
    static SYSTEM_CYCLES: Cell<u64> = Cell::new(0);
}

pub const SYSTEM_CYCLES_PER_PPU_CYCLE: u64 = 1;
pub const PPU_CYCLES_PER_CPU_CYCLE: u64 = 3;
pub const SYSTEM_CYCLES_PER_CPU_CYCLE: u64 =
    SYSTEM_CYCLES_PER_PPU_CYCLE * PPU_CYCLES_PER_CPU_CYCLE;
pub const CPU_SECOND_PHASE_OFFSET: u64 = SYSTEM_CYCLES_PER_CPU_CYCLE - 1;

#[cfg(test)]
fn set_system_cycles(cycles: u64) {
    SYSTEM_CYCLES.with(|sc| sc.set(cycles));
}

pub fn inc_system_cycles() -> u64 {
    SYSTEM_CYCLES.with(|sc| {
        let r = sc.get().wrapping_add(1);
        sc.set(r);
        r
    })
}

pub fn get_system_cycles() -> u64 {
    SYSTEM_CYCLES.with(|sc| sc.get())
}
