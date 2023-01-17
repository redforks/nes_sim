use nes_core::nes::create_mcu;
use nes_core::{Cpu, EmptyPlugin};
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
    let ines = nes_core::ines::INesFile::new(ines).unwrap();
    Machine {
        cpu: Cpu::new(Box::new(create_mcu(&ines, PpuDriver()))),
    }
}

#[wasm_bindgen]
impl Machine {
    pub fn tick(&mut self) {
        let mut p = EmptyPlugin();
        self.cpu.clock_tick(&mut p);
    }
}
