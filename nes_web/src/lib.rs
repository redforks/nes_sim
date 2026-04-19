#![cfg(target_arch = "wasm32")]

use log::{debug, info};
use nes_core::EmptyPlugin;
use nes_core::ines::INesFile;
use nes_core::nes::controller::Button;
use nes_core::nes_machine::NesMachine;
use std::panic;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement, window};

mod canvas_render;
mod drivers;

use canvas_render::CanvasRender;
use drivers::apu::WebAudioDriver;

#[wasm_bindgen]
pub struct Machine {
    inner: NesMachine<
        nes_core::EmptyPlugin<nes_core::nes::NesMcu<CanvasRender, WebAudioDriver>>,
        CanvasRender,
        WebAudioDriver,
    >,
}

#[wasm_bindgen]
pub fn new_machine(canvas_id: &str, ines: Vec<u8>) -> Result<Machine, JsValue> {
    debug!("new_machine");
    let ines = INesFile::new(ines).unwrap();

    let ctx = get_canvas_context(canvas_id);
    let canvas_render = CanvasRender::new(ctx, 256, 240);
    let audio_driver = WebAudioDriver::new()?;

    Ok(Machine {
        inner: NesMachine::new(&ines, EmptyPlugin::new(), canvas_render, audio_driver),
    })
}

#[wasm_bindgen]
impl Machine {
    pub fn process_frame(&mut self) {
        self.inner.process_frame();
        self.inner.flush_audio();
    }

    pub fn reset(&mut self) {
        self.inner.reset();
    }

    pub fn key_down(&mut self, key: &str) {
        if let Some(button) = map_key_to_button(key) {
            self.inner.press_button(button);
        }
    }

    pub fn key_up(&mut self, key: &str) {
        if let Some(button) = map_key_to_button(key) {
            self.inner.release_button(button);
        }
    }
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

fn map_key_to_button(key: &str) -> Option<Button> {
    match key {
        "ArrowUp" => Some(Button::Up),
        "ArrowDown" => Some(Button::Down),
        "ArrowLeft" => Some(Button::Left),
        "ArrowRight" => Some(Button::Right),
        "KeyZ" => Some(Button::A),
        "KeyX" => Some(Button::B),
        "Space" => Some(Button::Select),
        "Enter" => Some(Button::Start),
        _ => None,
    }
}

#[wasm_bindgen]
pub fn init() {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    console_log::init_with_level(log::Level::Info).unwrap();
    info!("log inited");
}
