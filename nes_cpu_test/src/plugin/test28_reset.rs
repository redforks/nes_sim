//! test28 scripted-Reset watch (ticket #92): emits a single
//! [`ExecuteResult::ShouldReset`] when the pre-reset zero-page signature
//! (`$FC=$1E`, `$FD=$F0`, `$FE=$00`, the reset-ready state the ROM presents
//! while waiting for the Reset button) is observed. The run loop's existing
//! reset handling performs the reset; no new command-line flag.

/// Watches the test28 pre-reset zero-page signature and requests the single
/// scripted Reset through the existing [`ExecuteResult::ShouldReset`]
/// execution seam (no new command-line flag).
///
/// The ROM presents the reset-ready state `$FC=$1E`, `$FD=$F0`, `$FE=$00`
/// while waiting for the Reset button; the mid-run `$FE=$01` state (tests
/// still running) and partial signatures never fire. The request is
/// single-shot: the signature persists across the reset (RAM is preserved
/// by [`MachineWrapper::reset`](crate::image::MachineWrapper::reset)), so a
/// latch — not the signature's absence — prevents a second request.
///
/// Status-memory ([`MonitorTestStatus`](super::MonitorTestStatus)) and
/// dead-loop ([`DetectDeadLoop`](super::DetectDeadLoop)) detection stay out
/// of the test28 branch: this ROM never writes `$6000` and halts its
/// verdict screens in self-jumps, so neither plugin may report for it. A
/// hang with no verdict fails via [`Timeout`](super::Timeout) instead.
pub struct Test28ResetWatch {
    fired: bool,
    pending: bool,
}

impl Test28ResetWatch {
    pub fn new() -> Self {
        Self {
            fired: false,
            pending: false,
        }
    }

    fn signature_present<M: nes_core::mcu::Mcu>(view: &nes_core::view::MachineView<M>) -> bool {
        view.peek(0xFC) == 0x1E && view.peek(0xFD) == 0xF0 && view.peek(0xFE) == 0x00
    }
}

impl Default for Test28ResetWatch {
    fn default() -> Self {
        Self::new()
    }
}

impl<M: nes_core::mcu::Mcu> nes_core::Plugin<M> for Test28ResetWatch {
    fn start(&mut self, _: &nes_core::view::MachineView<M>) {}

    fn end(&mut self, view: &nes_core::view::MachineView<M>) {
        // `end` always runs immediately before `should_stop` within a tick,
        // and runs again after the reset, so recomputing `pending` here
        // (rather than clearing it in `should_stop`, which cannot mutate)
        // delivers exactly one reset: the latch holds after the reset even
        // though the signature persists in preserved RAM.
        self.pending = !self.fired && Self::signature_present(view);
        self.fired |= self.pending;
    }

    fn should_stop(&self) -> nes_core::ExecuteResult {
        if self.pending {
            nes_core::ExecuteResult::ShouldReset
        } else {
            nes_core::ExecuteResult::Continue
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nes_core::Cpu;
    use nes_core::bus::Bus;
    use nes_core::mcu::{Mcu, RamMcu};
    use nes_core::view::MachineView;
    use nes_core::{ExecuteResult, Plugin};

    fn view_with_zp(fc: u8, fd: u8, fe: u8) -> (Cpu, Bus<RamMcu<65536>>) {
        let (mut cpu, mut bus) = (Cpu::new(), Bus::new(RamMcu::new([0; 65536])));
        bus.mcu_mut()
            .write(0xFC, fc, nes_core::SystemClock::default());
        bus.mcu_mut()
            .write(0xFD, fd, nes_core::SystemClock::default());
        bus.mcu_mut()
            .write(0xFE, fe, nes_core::SystemClock::default());
        let mut plugin = nes_core::EmptyPlugin::new();
        cpu.run_to_instruction_boundary(&mut bus, &mut plugin);
        (cpu, bus)
    }

    fn poll(p: &mut Test28ResetWatch, cpu: &Cpu, bus: &Bus<RamMcu<65536>>) -> ExecuteResult {
        let view = MachineView::new(cpu.snapshot(), bus.mcu(), nes_core::SystemClock::default());
        p.end(&view);
        <Test28ResetWatch as Plugin<RamMcu<65536>>>::should_stop(p)
    }

    #[test]
    fn no_signature_means_continue() {
        let (cpu, bus) = view_with_zp(0x00, 0x00, 0x00);
        let mut p = Test28ResetWatch::new();
        assert_eq!(poll(&mut p, &cpu, &bus), ExecuteResult::Continue);
    }

    #[test]
    fn stored_but_not_ready_signature_means_continue() {
        // `$FE=$01` is the ROM's "tests running, don't reset yet" state;
        // only the reset-ready `$FE=$00` state fires.
        let (cpu, bus) = view_with_zp(0x1E, 0xF0, 0x01);
        let mut p = Test28ResetWatch::new();
        assert_eq!(poll(&mut p, &cpu, &bus), ExecuteResult::Continue);
    }

    #[test]
    fn partial_signature_means_continue() {
        let (cpu, bus) = view_with_zp(0x1E, 0xF0, 0xFF);
        let mut p = Test28ResetWatch::new();
        assert_eq!(poll(&mut p, &cpu, &bus), ExecuteResult::Continue);
        let (cpu, bus) = view_with_zp(0x00, 0xF0, 0x00);
        assert_eq!(poll(&mut p, &cpu, &bus), ExecuteResult::Continue);
    }

    #[test]
    fn ready_signature_fires_single_reset_then_continues() {
        let (cpu, bus) = view_with_zp(0x1E, 0xF0, 0x00);
        let mut p = Test28ResetWatch::new();
        assert_eq!(poll(&mut p, &cpu, &bus), ExecuteResult::ShouldReset);
        // The signature persists across the reset (RAM is preserved); the
        // watch must not fire again.
        assert_eq!(poll(&mut p, &cpu, &bus), ExecuteResult::Continue);
        assert_eq!(poll(&mut p, &cpu, &bus), ExecuteResult::Continue);
    }
}
