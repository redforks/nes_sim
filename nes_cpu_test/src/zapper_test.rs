//! Scripted Zapper input for the tetanes input-suite ROMs.
//!
//! The four Zapper ROMs report only through the rendered screen and $4011
//! DAC-register "clicks" (no $6000 signature, no console text), so like
//! tetanes-core they are driven by per-frame scripted input plus blessed
//! frame snapshots. [`ZapperTest`] encodes, per ROM:
//!
//! - `actions`: Zapper events applied at the *start* of the named frame
//!   (analog of tetanes-core's tests.json frame actions),
//! - `expectations`: checks sampled at the *vblank edge* of the named frame
//!   (render complete), the same sampling point as `--dump-frame` /
//!   [`crate::plugin::PngFrameMatch`].
//!
//! `Clicks(n)` asserts the total number of $4011 writes so far; `Frame(png)`
//! compares the rendered framebuffer against a blessed snapshot under
//! `src/png-exps/`.

use std::path::PathBuf;

use crate::image::MachineWrapper;

#[derive(Debug, Clone, Copy)]
pub enum ZapperAction {
    Connect,
    Aim(u16, u16),
    Trigger,
}

#[derive(Debug, Clone, Copy)]
enum Expectation {
    /// Total $4011 DAC-register writes must equal this by the sample frame.
    Clicks(u64),
    /// The rendered frame must match the blessed `png-exps/<name>.png`.
    Frame(&'static str),
}

struct Timed<T> {
    frame: usize,
    item: T,
}

pub struct ZapperTest {
    actions: Vec<Timed<ZapperAction>>,
    next_action: usize,
    expectations: Vec<Timed<Expectation>>,
    next_expectation: usize,
    /// Edge detector: `in_vblank` stays high for a whole vblank period and
    /// expectations must sample each frame's vblank exactly once.
    prev_vblank: bool,
}

/// One ROM's scripted session: `(frame, action)` pairs and
/// `(frame, expectation)` pairs; the per-ROM tables in
/// [`ZapperTest::for_rom`].
type ScriptActions = [(usize, ZapperAction)];
type ScriptExpectations = [(usize, Expectation)];

impl ZapperTest {
    /// Action/expectation frames follow the ROM loops (see issue #14):
    /// each ROM polls $4017 in a spin loop and reacts once per NMI, so
    /// events land on frame boundaries.
    pub fn for_rom(file_name: &str) -> Option<ZapperTest> {
        let (actions, expectations): (&ScriptActions, &ScriptExpectations) = match file_name {
            // last polled trigger bit ($30 bright / $0F black). Trigger at
            // frame 5 reads held ~6 frames: black board frames 7-11,
            // bright again from 12.
            "zapper_flip.nes" => (
                &[
                    (0, ZapperAction::Connect),
                    (0, ZapperAction::Aim(10, 10)),
                    (5, ZapperAction::Trigger),
                ],
                &[
                    (4, Expectation::Clicks(0)),
                    (4, Expectation::Frame("zapper_flip-f4")),
                    (7, Expectation::Frame("zapper_flip-f7")),
                    (13, Expectation::Frame("zapper_flip-f4")),
                ],
            ),
            // Same checkerboard; one click per frame while the aperture
            // sees light (aimed at the bright board from frame 5), none
            // on black.
            "zapper_light.nes" => (
                &[
                    (0, ZapperAction::Connect),
                    (0, ZapperAction::Aim(10, 10)),
                    (5, ZapperAction::Aim(100, 100)),
                    (13, ZapperAction::Aim(10, 10)),
                ],
                &[
                    (4, Expectation::Clicks(0)),
                    (4, Expectation::Frame("zapper_light-f4")),
                    (8, Expectation::Clicks(4)),
                    (12, Expectation::Clicks(8)),
                    (16, Expectation::Clicks(8)),
                ],
            ),
            // 704 grid cells record the polled trigger bit over 11+ NMIs;
            // hold from frame 5 shows as tile-pattern differences.
            "zapper_stream.nes" => (
                &[(0, ZapperAction::Connect), (5, ZapperAction::Trigger)],
                &[
                    (4, Expectation::Frame("zapper_stream-f4")),
                    (6, Expectation::Frame("zapper_stream-f6")),
                    (11, Expectation::Frame("zapper_stream-f11")),
                    (13, Expectation::Frame("zapper_stream-f13")),
                ],
            ),
            // Static screen; one click per frame while the trigger reads
            // held (pull at frame 4, release delay ~6 frames).
            "zapper_trigger.nes" => (
                &[(0, ZapperAction::Connect), (4, ZapperAction::Trigger)],
                &[
                    (3, Expectation::Clicks(0)),
                    (4, Expectation::Frame("zapper_trigger-f4")),
                    (9, Expectation::Clicks(6)),
                    (13, Expectation::Clicks(7)),
                    (17, Expectation::Clicks(7)),
                ],
            ),
            _ => return None,
        };
        Some(ZapperTest {
            actions: actions
                .iter()
                .map(|&(frame, item)| Timed { frame, item })
                .collect(),
            next_action: 0,
            expectations: expectations
                .iter()
                .map(|&(frame, item)| Timed { frame, item })
                .collect(),
            next_expectation: 0,
            prev_vblank: false,
        })
    }

    /// Apply actions due at the start of the current frame. Called before
    /// every tick; the frame counter rolls to N during frame N-1's final
    /// ticks, so the first call observing `frame_no == N` sits in frame N's
    /// earliest scanlines.
    pub fn apply_due_actions(&mut self, m: &mut MachineWrapper) {
        while let Some(action) = self.actions.get(self.next_action) {
            if m.frame_no() < action.frame {
                break;
            }
            let Timed { item, .. } = self.actions[self.next_action];
            self.next_action += 1;
            m.apply_zapper_action(item);
        }
    }

    /// Sample expectations at vblank edges. Returns the process exit code
    /// once every expectation has passed (0), on the first failure (1), or
    /// `None` while the run must continue. With `verify` false (blessing
    /// runs) expectations are skipped entirely.
    pub fn after_tick(&mut self, m: &mut MachineWrapper, verify: bool) -> Option<i32> {
        let vblank = m.in_vblank();
        let edge = vblank && !self.prev_vblank;
        self.prev_vblank = vblank;
        if !edge || !verify {
            return None;
        }

        let frame_no = m.frame_no();
        while let Some(expected) = self.expectations.get(self.next_expectation) {
            if frame_no < expected.frame {
                break;
            }
            let Timed { frame, item } = self.expectations[self.next_expectation];
            if let Err(msg) = check_expectation(m, item) {
                eprintln!("zapper expectation failed at frame {frame}: {msg}");
                return Some(1);
            }
            self.next_expectation += 1;
        }

        if self.next_expectation == self.expectations.len() {
            eprintln!("all {} zapper expectations passed", self.expectations.len());
            return Some(0);
        }
        None
    }
}

fn check_expectation(m: &MachineWrapper, expected: Expectation) -> Result<(), String> {
    match expected {
        Expectation::Clicks(count) => {
            let actual = m.dmc_dac_writes();
            if actual == count {
                Ok(())
            } else {
                Err(format!("{count} $4011 clicks expected, counted {actual}"))
            }
        }
        Expectation::Frame(name) => {
            let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("src/png-exps/")
                .join(format!("{name}.png"));
            let expected = image::open(&path)
                .map_err(|e| format!("failed to load {}: {e}", path.display()))?
                .to_rgba8();
            let actual: &[[u8; 4]] = m.renderer_image();
            if crate::plugin::png_frame_match::PngFrameMatch::compare_frame(actual, &expected) {
                Ok(())
            } else {
                Err(format!(
                    "rendered frame differs from blessed snapshot {}",
                    path.display()
                ))
            }
        }
    }
}
