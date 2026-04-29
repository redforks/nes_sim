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
