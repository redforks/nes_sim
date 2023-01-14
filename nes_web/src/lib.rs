use nes_core::mcu::{MappingMcu, RamMcu, Region};
use nes_core::nes::apu::{
    APUStatus, ControlFlags, DmcIRQLoopFreq, DutyCycle, FrameCounter, LengthCounterLoad,
    LinearCounterControl, NoiseEnvelop, NoiseLength, NoisePeriod, Sweep,
};
use nes_core::nes::controller::Controller;
use nes_core::nes::ppu::{Ppu, PpuCtrl, PpuMask, PpuStatus};
use nes_core::nes::{apu, ppu, setup_mem_mirror};
use nes_core::{Cpu, EmptyPlugin};
use std::iter::once;
use wasm_bindgen::prelude::*;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub struct Machine {
    cpu: Cpu,
}

#[wasm_bindgen]
pub fn new_machine() -> Machine {
    let regions = [
        Region::new(0x0000, 0x1fff, Box::new(RamMcu::new([0; 0x2000]))),
        Region::new(0x2000, 0x3fff, Box::new(Controller::new())),
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
        .chain(once(Region::new(
            0x4015,
            0x4015,
            Box::new(Ppu::new(PpuDriver())),
        )));
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

// empty PulseDriver impl
pub struct PulseDriver();

impl apu::PulseDriver for PulseDriver {
    fn set_duty_cycle(&mut self, _: DutyCycle) {
        todo!()
    }

    fn set_sweep(&mut self, _: Sweep) {
        todo!()
    }

    fn set_length_counter_load(&mut self, _: LengthCounterLoad) {
        todo!()
    }
}

// empty TriangleDriver impl
pub struct TriangleDriver();

impl apu::TriangleDriver for TriangleDriver {
    fn set_linear_counter_control(&mut self, _: LinearCounterControl) {
        todo!()
    }

    fn set_length_counter_load(&mut self, _: LengthCounterLoad) {
        todo!()
    }
}

// empty NoiseDriver impl
pub struct NoiseDriver();

impl apu::NoiseDriver for NoiseDriver {
    fn set_envelop(&mut self, _: NoiseEnvelop) {
        todo!()
    }

    fn set_period(&mut self, _: NoisePeriod) {
        todo!()
    }

    fn set_length(&mut self, _: NoiseLength) {
        todo!()
    }
}

// empty DmcDriver impl
pub struct DmcDriver();

impl apu::DmcDriver for DmcDriver {
    fn set_irq_loop_freq(&mut self, _: DmcIRQLoopFreq) {
        todo!()
    }

    fn set_load_counter(&mut self, _: u8) {
        todo!()
    }

    fn set_sample_address(&mut self, _: u8) {
        todo!()
    }

    fn set_sample_length(&mut self, _: u8) {
        todo!()
    }
}

// empty ApuControlDriver impl
pub struct ApuControlDriver();

impl apu::APUControllerDriver for ApuControlDriver {
    fn set_control_flags(&mut self, _: ControlFlags) {
        todo!()
    }

    fn set_frame_counter(&mut self, _: FrameCounter) {
        todo!()
    }

    fn read_status(&self) -> APUStatus {
        todo!()
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
