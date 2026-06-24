#![allow(dead_code)]

use image::{Rgba, RgbaImage};
use nes_core::nes::NesMcu;
use nes_core::nes::apu::AudioDriver;
use nes_core::render::ImageRender;
use nes_core::{Cpu, ExecuteResult, Plugin};
use std::path::Path;
use std::path::PathBuf;

#[derive(Debug)]
pub struct PngFrameMatch {
    expected: Vec<(PathBuf, RgbaImage)>,
    matched: Vec<bool>,
}

impl PngFrameMatch {
    fn is_complete(&self) -> bool {
        self.matched.iter().all(|matched| *matched)
    }

    pub fn new<I: Into<PathBuf>>(paths: Vec<I>) -> image::ImageResult<Self> {
        let mut expected: Vec<(PathBuf, RgbaImage)> = Vec::new();
        for path in paths {
            let expected_path = path.into();
            let expected_img = image::open(&expected_path)?.to_rgba8();
            expected.push((expected_path, expected_img));
        }
        let n = expected.len();
        Ok(Self {
            expected,
            matched: vec![false; n],
        })
    }

    fn compare_frame(actual: &RgbaImage, expected: &RgbaImage) -> bool {
        assert_eq!(
            actual.dimensions(),
            expected.dimensions(),
            "Actual frame dimensions do not match expected image dimensions"
        );

        actual
            .pixels()
            .zip(expected.pixels())
            .all(|(actual, expected)| pixel_within_threshold(*actual, *expected))
    }
}

fn pixel_within_threshold(actual: Rgba<u8>, expected: Rgba<u8>) -> bool {
    let diff: u32 = actual
        .0
        .into_iter()
        .zip(expected.0)
        .map(|(a, e)| a.abs_diff(e) as u32)
        .sum();
    diff as f32 / (255.0 * 4.0) < 0.05
}

impl<A: AudioDriver> Plugin<NesMcu<ImageRender, A>> for PngFrameMatch {
    fn start(&mut self, _cpu: &mut Cpu<NesMcu<ImageRender, A>>) {}

    fn end(&mut self, cpu: &mut Cpu<NesMcu<ImageRender, A>>) {
        if self.is_complete() {
            return;
        }

        if !cpu.mcu().ppu().in_vblank() {
            return;
        }

        let frame_no = cpu.mcu().ppu().timing().frame_no();
        let actual = cpu.mcu().ppu().renderer().borrow_image();
        for idx in 0..self.expected.len() {
            if !self.matched[idx] && Self::compare_frame(actual, &self.expected[idx].1) {
                let _ = actual.save(Path::new("/tmp/png-frame-match.png"));
                eprintln!(
                    "Frame {} matches expected image {}",
                    frame_no,
                    self.expected[idx].0.display()
                );
                self.matched[idx] = true;
                break;
            }
        }

        if self.is_complete() {
            eprintln!("Frame {} matched both expected images", frame_no);
        } else if frame_no.is_multiple_of(50) {
            let _ = actual.save(Path::new("/tmp/png-frame-match.png"));
        }
    }

    fn should_stop(&self) -> ExecuteResult {
        if self.is_complete() {
            ExecuteResult::Stop(0)
        } else {
            ExecuteResult::Continue
        }
    }
}
