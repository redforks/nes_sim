use nes_core::nes::NesMcu;
use nes_core::nes::apu::AudioDriver;
use nes_core::render::ImageRender;
use nes_core::view::MachineView;
use nes_core::{ExecuteResult, Plugin, SystemClock};
use std::path::PathBuf;

/// Development tool: renders until `target_frame` (observed at vblank, matching
/// [`crate::plugin::PngFrameMatch`] sampling), saves the frame to `out_path`,
/// and stops. Used to bless expected PNGs for PngFrameMatch-based tests.
///
/// Frame-snapshot assertion scheme ported from tetanes-core, which stores a
/// per-frame pixel hash per ROM in test_roms/<category>/tests.json and
/// re-blesses via UPDATE_SNAPSHOT; here the blessed artifact is a PNG under
/// src/png-exps/ compared by PngFrameMatch.
pub struct FramePngDump {
    target_frame: usize,
    out_path: PathBuf,
    dumped: bool,
}

impl FramePngDump {
    pub fn new(target_frame: usize, out_path: impl Into<PathBuf>) -> Self {
        Self {
            target_frame,
            out_path: out_path.into(),
            dumped: false,
        }
    }
}

impl<A: AudioDriver> Plugin<NesMcu<ImageRender, A>> for FramePngDump {
    fn start(&mut self, _view: &MachineView<NesMcu<ImageRender, A>>, _: SystemClock) {}

    fn end(&mut self, view: &MachineView<NesMcu<ImageRender, A>>, _: SystemClock) {
        if self.dumped || !view.ppu_in_vblank() {
            return;
        }

        let frame_no = view.ppu_frame_no();
        if frame_no < self.target_frame {
            return;
        }

        let image = view.borrow_image();
        if let Err(e) = image.save(&self.out_path) {
            eprintln!("failed to save {}: {e}", self.out_path.display());
            return;
        }
        eprintln!("dumped frame {} to {}", frame_no, self.out_path.display());
        self.dumped = true;
    }

    fn should_stop(&self) -> ExecuteResult {
        if self.dumped {
            ExecuteResult::Stop(0)
        } else {
            ExecuteResult::Continue
        }
    }
}
