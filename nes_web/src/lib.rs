use image::DynamicImage;
use log::{debug, info};
use nes_core::ines::INesFile;
use nes_core::nes::ppu::{draw_pattern, PatternBand};
use nes_core::nes::Machine as NesMachine;
use nes_core::EmptyPlugin;
use std::panic;
use wasm_bindgen::prelude::*;
use wasm_bindgen::{Clamped, JsCast};
use web_sys::{window, CanvasRenderingContext2d, HtmlCanvasElement, ImageData};

mod drivers;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub struct Machine {
    inner: NesMachine<EmptyPlugin>,
    ctx: CanvasRenderingContext2d,
}

#[wasm_bindgen]
pub fn new_machine(canvas_id: &str, ines: Vec<u8>) -> Machine {
    debug!("new_machine");
    let ines = INesFile::new(ines).unwrap();
    Machine {
        inner: NesMachine::new(ines),
        ctx: get_canvas_context(canvas_id),
    }
}

#[wasm_bindgen]
impl Machine {
    pub fn process_frame(&mut self, ms: f64) {
        info!("process_frame: {}ms", ms);
        let img = self.inner.process_frame(ms);
        let img_data = ImageData::new_with_u8_clamped_array_and_sh(Clamped(img), 256, 240).unwrap();
        self.ctx.put_image_data(&img_data, 0.0, 0.0).unwrap();
    }
}

fn read_chr(f: &INesFile) -> Vec<u8> {
    let band = PatternBand::new(f.read_chr_rom());
    let img: DynamicImage = draw_pattern(&band).into();
    img.into_bytes()
}

fn get_canvas_context(canvas_id: &str) -> CanvasRenderingContext2d {
    let window = window().unwrap();
    let document = window.document().unwrap();
    let canvas: HtmlCanvasElement = document
        .get_element_by_id(canvas_id)
        .unwrap()
        .dyn_into()
        .unwrap();
    canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into()
        .unwrap()
}

#[wasm_bindgen]
pub fn draw_chr(ines: Vec<u8>, canvas_id: &str) {
    let ines = INesFile::new(ines).unwrap();
    let img = read_chr(&ines);
    let image_data = ImageData::new_with_u8_clamped_array(Clamped(&img), 128).unwrap();

    let ctx = get_canvas_context(canvas_id);
    ctx.put_image_data(&image_data, 0.0, 0.0).unwrap();
}

#[wasm_bindgen]
pub fn init() {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    console_log::init_with_level(log::Level::Info).unwrap();
    info!("log inited");
}
