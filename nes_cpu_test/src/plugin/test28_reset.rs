//! test28 scripted-Reset watch (ticket #92): emits a single
//! [`ExecuteResult::ShouldReset`] at the genuine pre-reset WAIT state — the
//! zero-page signature (`$FC=$1E`, `$FD=$F0`, `$FE=$01`) is present AND the
//! wait screen (`Please press the Reset ...`) is displayed in nametable
//! tall text. The run loop's existing reset handling performs the reset;
//! no new command-line flag.
//!
//! Background (verified by binary sweep of the released ROM): the only
//! `$FE` store in all 512 KiB writes `$01`, so a `$FE=$00` predicate can
//! never fire; and the signature is stored before the wait screen is
//! drawn, so firing on the signature alone would reset too early (bank 0
//! reads `$FE!=0` as NOT SO FAST). The screen half of the predicate is the
//! `Please press the Reset` tall-text row, which no verdict screen
//! contains — the early-reset guard prints uppercase `DON'T PRESS RESET
//! UNTIL ASKED.`, and the final pass screen has no press line.
//!
//! Single-shot: `MachineWrapper::reset` preserves RAM and VRAM, so both
//! halves of the WAIT state are still present afterwards — the latch, not
//! the state's absence, prevents a second request.
//!
//! Status-memory ([`MonitorTestStatus`](super::MonitorTestStatus)) and
//! dead-loop ([`DetectDeadLoop`](super::DetectDeadLoop)) detection stay out
//! of the test28 branch: this ROM never writes `$6000` and halts its
//! verdict screens in self-jumps, so neither plugin may report for it. A
//! hang with no verdict fails via [`Timeout`](super::Timeout) instead.

use nes_core::mcu::Mcu;
use nes_core::nes::NesMcu;
use nes_core::render::Render;
use nes_core::view::MachineView;
use nes_core::{ExecuteResult, Plugin};

/// Single-shot latch for the scripted reset: `Armed` watches for the WAIT
/// state, `Requested` asks the run loop for one reset, `Spent` never fires
/// again. The reset lands between `end` and the next `end`, so a
/// `Requested` latch is consumed exactly once however long the WAIT state
/// persists afterwards.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Latch {
    Armed,
    Requested,
    Spent,
}

impl Latch {
    fn note(&mut self, wait: bool) {
        match (*self, wait) {
            (Latch::Armed, true) => *self = Latch::Requested,
            (Latch::Requested, _) => *self = Latch::Spent,
            _ => {}
        }
    }

    fn should_stop(&self) -> ExecuteResult {
        match self {
            Latch::Requested => ExecuteResult::ShouldReset,
            _ => ExecuteResult::Continue,
        }
    }
}

pub struct Test28ResetWatch {
    latch: Latch,
    /// Frame of the last wait-screen decode. The screen is decoded at most
    /// once per frame; the signature half stays three `peek`s per tick.
    last_frame: Option<usize>,
}

impl Test28ResetWatch {
    pub fn new() -> Self {
        Self {
            latch: Latch::Armed,
            last_frame: None,
        }
    }

    fn signature_present<M: Mcu>(view: &MachineView<M>) -> bool {
        view.peek(0xFC) == 0x1E && view.peek(0xFD) == 0xF0 && view.peek(0xFE) == 0x01
    }

    fn wait_screen_visible<R: Render>(view: &MachineView<NesMcu<R, ()>>) -> bool {
        super::nametable_console::contains_test28_reset_prompt(
            &super::nametable_console::read_tall_console(view),
        )
    }
}

impl Default for Test28ResetWatch {
    fn default() -> Self {
        Self::new()
    }
}

impl<R: Render> Plugin<NesMcu<R, ()>> for Test28ResetWatch {
    fn start(&mut self, _: &MachineView<NesMcu<R, ()>>) {}

    fn end(&mut self, view: &MachineView<NesMcu<R, ()>>) {
        // The reset preserves RAM and VRAM, so the WAIT state is still
        // present after it lands: the latch (not the state's absence) is
        // what keeps the request single-shot.
        let mut ready = false;
        if self.latch == Latch::Armed && Self::signature_present(view) {
            // The signature is stored before the wait screen is drawn;
            // decode the screen at most once per frame until it shows the
            // prompt.
            let frame = view.ppu_frame_no();
            if self.last_frame != Some(frame) {
                self.last_frame = Some(frame);
                ready = Self::wait_screen_visible(view);
            }
        }
        self.latch.note(ready);
    }

    fn should_stop(&self) -> ExecuteResult {
        self.latch.should_stop()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nes_core::SystemClock;
    use nes_core::ines::INesFile;
    use nes_core::nes_machine::NesMachine;

    /// Minimal NROM machine: the watch only needs zero-page `peek` and
    /// nametable VRAM, both mapper-independent, so the wiring tests don't
    /// need a mapper-28 build.
    fn watch_machine() -> NesMachine<Test28ResetWatch, (), ()> {
        let mut image = vec![
            b'N', b'E', b'S', 0x1A, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00,
        ];
        image.extend(vec![0u8; 0x4000 + 0x2000]);
        // Point the reset vector at $8000 so construction never sees a
        // zero-page vector, even though these tests never execute code.
        let base = 16;
        image[base + 0x3FFC] = 0x00;
        image[base + 0x3FFD] = 0x80;
        let file = INesFile::new(image).expect("fixture iNES");
        NesMachine::new(&file, Test28ResetWatch::new(), (), ())
    }

    fn poll(
        watch: &mut Test28ResetWatch,
        machine: &NesMachine<Test28ResetWatch, (), ()>,
    ) -> ExecuteResult {
        let view = MachineView::new(
            machine.cpu().snapshot(),
            machine.mcu(),
            SystemClock::default(),
        );
        watch.end(&view);
        <Test28ResetWatch as Plugin<NesMcu<(), ()>>>::should_stop(watch)
    }

    fn poke_signature(machine: &mut NesMachine<Test28ResetWatch, (), ()>) {
        let mcu = machine.mcu_mut();
        mcu.write(0xFC, 0x1E, SystemClock::default());
        mcu.write(0xFD, 0xF0, SystemClock::default());
        mcu.write(0xFE, 0x01, SystemClock::default());
    }

    /// Writes one tall-text row pair: top tiles `c*2` on `row_pair*2`,
    /// bottom tiles `c*2+1` on `row_pair*2+1`, the tall encoding
    /// `decode_tall_char` inverts.
    fn write_tall_text(
        machine: &mut NesMachine<Test28ResetWatch, (), ()>,
        row_pair: usize,
        text: &str,
    ) {
        let mcu = machine.mcu_mut();
        for pass in [0u16, 1u16] {
            mcu.read(0x2002); // reset the $2006 latch
            let addr = 0x2000 + ((row_pair as u16) * 2 + pass) * 32;
            mcu.write(0x2006, (addr >> 8) as u8, SystemClock::default());
            mcu.write(0x2006, (addr & 0xFF) as u8, SystemClock::default());
            for &c in text.as_bytes() {
                let tile = c.wrapping_mul(2).wrapping_add(pass as u8);
                mcu.write(0x2007, tile, SystemClock::default());
            }
        }
    }

    #[test]
    fn latch_requests_once_then_spends() {
        let mut latch = Latch::Armed;
        assert_eq!(latch.should_stop(), ExecuteResult::Continue);
        latch.note(false);
        assert_eq!(latch, Latch::Armed);
        latch.note(true);
        assert_eq!(latch, Latch::Requested);
        assert_eq!(latch.should_stop(), ExecuteResult::ShouldReset);
        // Consumed on the next observation however the WAIT state reads:
        // the reset preserves RAM and VRAM, so it is still present.
        latch.note(true);
        assert_eq!(latch, Latch::Spent);
        assert_eq!(latch.should_stop(), ExecuteResult::Continue);
        latch.note(true);
        assert_eq!(latch, Latch::Spent);
    }

    #[test]
    fn no_signature_means_continue() {
        let machine = watch_machine();
        let mut watch = Test28ResetWatch::new();
        assert_eq!(poll(&mut watch, &machine), ExecuteResult::Continue);
    }

    #[test]
    fn signature_without_wait_screen_means_continue() {
        // The ROM stores the signature before drawing the wait screen;
        // firing here would reset too early (bank 0 reads `$FE!=0` as
        // NOT SO FAST).
        let mut machine = watch_machine();
        poke_signature(&mut machine);
        let mut watch = Test28ResetWatch::new();
        assert_eq!(poll(&mut watch, &machine), ExecuteResult::Continue);
        assert_eq!(poll(&mut watch, &machine), ExecuteResult::Continue);
    }

    #[test]
    fn wait_screen_without_signature_means_continue() {
        let mut machine = watch_machine();
        write_tall_text(&mut machine, 0, "Please press the Reset");
        let mut watch = Test28ResetWatch::new();
        assert_eq!(poll(&mut watch, &machine), ExecuteResult::Continue);
    }

    #[test]
    fn unrelated_screen_with_signature_means_continue() {
        // A per-sub-test verdict row plus the signature must not fire;
        // only the wait-screen row completes the WAIT state.
        let mut machine = watch_machine();
        poke_signature(&mut machine);
        write_tall_text(&mut machine, 0, "PASS!");
        let mut watch = Test28ResetWatch::new();
        assert_eq!(poll(&mut watch, &machine), ExecuteResult::Continue);
    }

    #[test]
    fn wait_state_fires_single_reset_then_continues() {
        let mut machine = watch_machine();
        poke_signature(&mut machine);
        write_tall_text(&mut machine, 0, "Please press the Reset");
        let mut watch = Test28ResetWatch::new();
        assert_eq!(poll(&mut watch, &machine), ExecuteResult::ShouldReset);

        // The reset preserves RAM and VRAM, so the WAIT state is still
        // present: the latch (not the state's absence) holds it to one.
        machine.reset();
        assert_eq!(machine.mcu().peek(0xFC), 0x1E);
        assert_eq!(machine.mcu().peek(0xFD), 0xF0);
        assert_eq!(machine.mcu().peek(0xFE), 0x01);
        assert_eq!(poll(&mut watch, &machine), ExecuteResult::Continue);
        assert_eq!(poll(&mut watch, &machine), ExecuteResult::Continue);
    }
}
