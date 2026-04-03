use nes_core::render::Render;
use std::fmt;
use wasm_bindgen::Clamped;
use web_sys::{CanvasRenderingContext2d, ImageData};

#[derive(Clone)]
pub struct CanvasRender {
    ctx: CanvasRenderingContext2d,
    buffer: Vec<u8>,
    width: u32,
    height: u32,
}

impl fmt::Debug for CanvasRender {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CanvasRender")
            .field("width", &self.width)
            .field("height", &self.height)
            .finish_non_exhaustive()
    }
}

impl CanvasRender {
    pub fn new(ctx: CanvasRenderingContext2d, width: u32, height: u32) -> Self {
        Self {
            ctx,
            buffer: vec![0; (width * height * 4) as usize],
            width,
            height,
        }
    }

    fn flush(&self) {
        let data = ImageData::new_with_u8_clamped_array_and_sh(
            Clamped(self.buffer.as_slice()),
            self.width,
            self.height,
        )
        .unwrap();
        self.ctx.put_image_data(&data, 0.0, 0.0).unwrap();
    }
}

impl Render for CanvasRender {
    fn clear(&mut self, color: [u8; 4]) {
        for px in self.buffer.chunks_exact_mut(4) {
            px.copy_from_slice(&color);
        }
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        if x >= self.width || y >= self.height {
            return;
        }

        let idx = ((y * self.width + x) * 4) as usize;
        self.buffer[idx..idx + 4].copy_from_slice(&color);
    }

    fn finish(&mut self) {
        self.flush();
    }
}
