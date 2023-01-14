use nes_core::mcu::{MappingMcu, RamMcu, Region};
use nes_core::nes::controller::Controller;
use nes_core::nes::ppu::{Ppu, PpuCtrl, PpuMask, PpuStatus};
use nes_core::nes::{apu, ppu, setup_mem_mirror};
use nes_core::{Cpu, EmptyPlugin};
use std::iter::once;
use wasm_bindgen::prelude::*;

mod drivers;
use drivers::apu::*;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub struct Machine {
    cpu: Cpu,
}

#[wasm_bindgen]
pub fn new_machine() -> Machine {
    let regions = [
        Region::with_defined(RamMcu::new([0; 0x2000])),
        Region::with_defined(Controller::new()),
    ];
    let regions = regions
        .into_iter()
        .chain(apu::new(
            PulseDriver(),
            PulseDriver(),
            TriangleDriver(),
            NoiseDriver(),
            DmcDriver(),
            ApuControlDriver(),
        ))
        .chain(once(Region::with_defined(Ppu::new(PpuDriver()))));
    let mcu = MappingMcu::new(regions.collect());
    let mcu = setup_mem_mirror(mcu);
    Machine {
        cpu: Cpu::new(Box::new(mcu)),
    }
}

#[wasm_bindgen]
impl Machine {
    pub fn tick(&mut self) {
        let mut p = EmptyPlugin();
        self.cpu.clock_tick(&mut p);
    }
}

// empty PpuDriver impl
pub struct PpuDriver();

impl ppu::PpuDriver for PpuDriver {
    fn set_ctrl(&mut self, _: PpuCtrl) {
        todo!()
    }

    fn set_mask(&mut self, _: PpuMask) {
        todo!()
    }

    fn get_status(&self) -> PpuStatus {
        todo!()
    }

    fn get_oma_address(&self) -> u8 {
        todo!()
    }

    fn set_oma_address(&mut self, _: u8) {
        todo!()
    }

    fn get_oma_data(&self) -> u8 {
        todo!()
    }

    fn set_oma_data(&mut self, _: u8) {
        todo!()
    }

    fn set_scroll_position(&mut self, _: u8, _: u8) {
        todo!()
    }

    fn set_address(&mut self, _: u16) {
        todo!()
    }

    fn get_data(&self) -> u8 {
        todo!()
    }

    fn set_data(&mut self, _: u8) {
        todo!()
    }

    fn set_dma(&mut self, _: u8) {
        todo!()
    }
}
