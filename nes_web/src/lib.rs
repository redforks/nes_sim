use nes_core::{Cpu, EmptyPlugin};
use wasm_bindgen::prelude::*;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub struct Machine {
    cpu: Cpu,
}

#[wasm_bindgen]
pub fn new_machine() -> Machine {
    todo!()
}

#[wasm_bindgen]
impl Machine {
    pub fn tick(&mut self) {
        let mut p = EmptyPlugin();
        self.cpu.clock_tick(&mut p);
    }
}
