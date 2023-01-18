use log::{debug, warn};
use nes_core::nes::create_mcu;
use nes_core::{Cpu, EmptyPlugin};
use std::panic;
use wasm_bindgen::prelude::*;

mod drivers;
use crate::drivers::ppu::PpuDriver;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub struct Machine {
    cpu: Cpu,
}

#[wasm_bindgen]
pub fn new_machine(ines: Vec<u8>) -> Machine {
    debug!("new_machine");
    let ines = nes_core::ines::INesFile::new(ines).unwrap();
    Machine {
        cpu: Cpu::new(Box::new(create_mcu(&ines, PpuDriver()))),
    }
}

#[wasm_bindgen]
impl Machine {
    pub fn tick_for_milliseconds(&mut self, ms: f64) {
        const CYCLES_PER_MS: u32 = 18000;

        let mut p = EmptyPlugin();
        for _ in 0..(ms * CYCLES_PER_MS as f64) as u32 {
            self.cpu.clock_tick(&mut p);
        }
    }
}

#[wasm_bindgen]
pub fn init() {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    console_log::init_with_level(log::Level::Warn).unwrap();
    warn!("log inited");
}
