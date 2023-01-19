use image::{DynamicImage, Rgba, RgbaImage};
use js_sys::Uint8ClampedArray;
use log::{debug, warn};
use nes_core::ines::INesFile;
use nes_core::nes::create_mcu;
use nes_core::nes::ppu::PatternBand;
use nes_core::{Cpu, EmptyPlugin};
use std::panic;
use wasm_bindgen::prelude::*;
use wasm_bindgen::{Clamped, JsCast};
use web_sys::{window, CanvasRenderingContext2d, HtmlCanvasElement, ImageData};

mod drivers;
use crate::drivers::ppu::PpuDriver;

const PALLET: [Rgba<u8>; 4] = [
    Rgba([0xff, 0xff, 0xff, 0xff]),
    Rgba([0x7f, 0x0, 0x0, 0xff]),
    Rgba([0x0, 0x00, 0x7f, 0xff]),
    Rgba([0x0, 0x7f, 0x0, 0xff]),
];

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
        cpu: Cpu::new(Box::new(create_mcu(&ines, PpuDriver()))),
    }
}

fn read_chr(f: &INesFile) -> Vec<u8> {
    let band = PatternBand::new(f.read_chr_rom());
    let total_bands = band.iter().count() as u32;
    if total_bands == 0 {
        debug!("No CHR ROM found");
        return Vec::default();
    }

    let mut img: RgbaImage = RgbaImage::new(128, 128 * total_bands + 32 * (total_bands - 1));
    for (band_idx, pattern) in band.iter().enumerate() {
        let start_y = band_idx * (128 + 32);
        for (tile_idx, tile) in pattern.iter().enumerate() {
            let tile_x = tile_idx % 16;
            let tile_y = tile_idx / 16;
            for (x, y, pixel) in tile.iter() {
                let image_x = (tile_x * 8 + x) as u32;
                let image_y = (start_y + tile_y * 8 + y) as u32;
                img.put_pixel(image_x, image_y, PALLET[pixel as usize]);
            }
        }
    }
    let img: DynamicImage = img.into();
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
