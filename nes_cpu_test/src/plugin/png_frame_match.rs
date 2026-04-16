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
    expected: RgbaImage,
    expected_path: PathBuf,
    passed: bool,
}

impl PngFrameMatch {
    pub fn new(path: impl Into<PathBuf>) -> image::ImageResult<Self> {
        let expected_path = path.into();
        let expected = image::open(&expected_path)?.to_rgba8();

        Ok(Self {
            expected,
            expected_path,
            passed: false,
        })
    }

    fn compare_frame(&self, actual: &RgbaImage) -> bool {
        assert_eq!(
            actual.dimensions(),
            self.expected.dimensions(),
            "Actual frame dimensions do not match expected image dimensions"
        );

        actual
            .pixels()
            .zip(self.expected.pixels())
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
        if self.passed {
            return;
        }

        if !cpu.mcu().ppu().in_vblank() {
            return;
        }

        let frame_no = cpu.mcu().ppu().frame_no();
        let actual = cpu.mcu().ppu().renderer().borrow_image();
        if self.compare_frame(actual) {
            let _ = actual.save(Path::new("/tmp/png-frame-match.png"));
            eprintln!("Frame {} matches expected image", frame_no);
            self.passed = true;
        } else if frame_no.is_multiple_of(50) {
            let _ = actual.save(Path::new("/tmp/png-frame-match.png"));
        }
    }

    fn should_stop(&self) -> ExecuteResult {
        if self.passed {
            ExecuteResult::Stop(0)
        } else {
            ExecuteResult::Continue
        }
    }
}
