use image::DynamicImage;
use log::{debug, warn};
use nes_core::ines::INesFile;
use nes_core::nes::create_mcu;
use nes_core::nes::ppu::{draw_pattern, PatternBand};
use nes_core::{Cpu, EmptyPlugin};
use std::panic;
use wasm_bindgen::prelude::*;
use wasm_bindgen::{Clamped, JsCast};
use web_sys::{window, CanvasRenderingContext2d, HtmlCanvasElement, ImageData};

mod drivers;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub struct Machine {
    cpu: Cpu,
}

#[wasm_bindgen]
pub fn new_machine(ines: Vec<u8>) -> Machine {
    debug!("new_machine");
    let ines = INesFile::new(ines).unwrap();
    Machine {
        cpu: Cpu::new(Box::new(create_mcu(&ines))),
    }
}

fn read_chr(f: &INesFile) -> Vec<u8> {
    let band = PatternBand::new(f.read_chr_rom());
    let img: DynamicImage = draw_pattern(&band).into();
    img.into_bytes()
}

#[wasm_bindgen]
pub fn draw_chr(ines: Vec<u8>, canvas_id: &str) {
    let ines = INesFile::new(ines).unwrap();
    let img = read_chr(&ines);
    let image_data = ImageData::new_with_u8_clamped_array(Clamped(&img), 128).unwrap();

    let canvas = window()
        .unwrap()
        .document()
        .unwrap()
        .get_element_by_id(canvas_id)
        .unwrap();
    let canvas: HtmlCanvasElement = canvas.dyn_into().unwrap();
    let ctx: CanvasRenderingContext2d = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into()
        .unwrap();
    ctx.put_image_data(&image_data, 0.0, 0.0).unwrap();
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
