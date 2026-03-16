use image::DynamicImage;
use image::EncodableLayout;
use log::{debug, info};
use nes_core::EmptyPlugin;
use nes_core::ines::INesFile;
use nes_core::machine::Machine as NesMachine;
use nes_core::nes::ppu::{PatternBand, draw_pattern};
use nes_core::render::{ImageRender, Render};
use std::cell::RefCell;
use std::panic;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::{Clamped, JsCast};
use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement, ImageData, window};

mod drivers;

/// A Render implementation that delegates to a shared Rc<RefCell<ImageRender>>
#[derive(Debug)]
struct SharedRender {
    inner: Rc<RefCell<ImageRender>>,
}

impl SharedRender {
    fn new(inner: Rc<RefCell<ImageRender>>) -> Self {
        Self { inner }
    }
}

impl Render for SharedRender {
    fn clear(&mut self, color: [u8; 4]) {
        self.inner.borrow_mut().clear(color);
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        self.inner.borrow_mut().set_pixel(x, y, color);
    }

    fn dimensions(&self) -> (u32, u32) {
        self.inner.borrow().dimensions()
    }

    fn finish(&mut self) {
        self.inner.borrow_mut().finish();
    }
}

#[wasm_bindgen]
pub struct Machine {
    inner: NesMachine<EmptyPlugin>,
    ctx: CanvasRenderingContext2d,
    image_render: Rc<RefCell<ImageRender>>,
}

#[wasm_bindgen]
pub fn new_machine(canvas_id: &str, ines: Vec<u8>) -> Machine {
    debug!("new_machine");
    let ines = INesFile::new(ines).unwrap();

    // Create image renderer for display - keep a reference to access later
    let image_render = Rc::new(RefCell::new(ImageRender::new(256, 240)));

    // Create a shared render wrapper
    let shared_render = SharedRender::new(image_render.clone());

    // Create MCU with the shared renderer
    let mcu = nes_core::nes::create_mcu_with_renderer(&ines, Some(Box::new(shared_render)));

    Machine {
        inner: NesMachine::new(Box::new(mcu)),
        ctx: get_canvas_context(canvas_id),
        image_render,
    }
}

#[wasm_bindgen]
impl Machine {
    pub fn process_frame(&mut self, ms: f64) {
        self.inner.process_frame(ms);
        let borrowed = self.image_render.borrow();
        let img = borrowed.as_image();
        let bytes = img.as_bytes();
        let img_data =
            ImageData::new_with_u8_clamped_array_and_sh(Clamped(bytes), 256, 240).unwrap();
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
