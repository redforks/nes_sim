use crate::{
    Cpu, EmptyPlugin, ExecuteResult, Plugin, SystemClock,
    bus::Bus,
    dma_bus::{BusOwner, DmaBus},
    ines::INesFile,
    nes::{Mmc3IrqOverride, NesMcu, controller::Button, ppu::palette::ColorTheme},
};

/// Borrowed output of one emulated frame.
///
/// `frame` is the PPU RGBA bytes (`256*240*4`, row-major) and `audio` is
/// the mixed mono samples (`f32`, [`crate::nes::apu::Apu::SAMPLE_RATE`])
/// for the frame. This struct is the per-frame read: consumers that step
/// with [`NesMachine::process_frame`] take pixels and samples from here
/// and never re-borrow the machine. [`Self::audio_bytes`] is the
/// little-endian `f32` byte view of `audio` for encoders.
pub struct FrameOutput<'a> {
    pub status: ExecuteResult,
    pub frame: &'a [u8],
    pub audio: &'a [f32],
}

impl<'a> FrameOutput<'a> {
    /// Byte view of [`Self::audio`] (little-endian `f32` bytes).
    /// Zero-copy: the same frame buffer, so it always matches the
    /// sample slice in length (`4x`) and content.
    pub fn audio_bytes(&self) -> &[u8] {
        bytemuck::cast_slice(self.audio)
    }
}

pub struct NesMachine<P> {
    cpu: Cpu,
    p: P,
    /// The CPU's external bus; owns the memory map `NesMcu` (ADR-0015).
    bus: Bus<NesMcu>,
    /// DMA arbitration on the bus — renamed from the former `Bus` (ADR-0009).
    dma_bus: DmaBus,
    /// The loaded ROM, kept so [`Self::hard_reset`] can rebuild the
    /// cartridge (mapper state never survives a power cycle on hardware)
    /// without the caller re-supplying it.
    file: INesFile,
    /// MMC3 IRQ-revision override from construction, re-applied on hard reset.
    irq_override: Mmc3IrqOverride,
}

impl<P> NesMachine<P>
where
    P: Plugin<NesMcu>,
{
    pub fn new(file: &INesFile, plugin: P) -> Self {
        Self::new_with_mmc3_irq_override(file, plugin, Mmc3IrqOverride::Auto)
    }

    /// [`Self::new`] with an explicit MMC3 IRQ-revision override — the
    /// harness/test hook (step 1) of [`mmc3_irq_revision_is_alternate`]:
    /// [`Mmc3IrqOverride::ForceAlternate`] forces revision-A (Alternate)
    /// semantics, [`Mmc3IrqOverride::ForceStandard`] forces Standard, and
    /// [`Mmc3IrqOverride::Auto`] uses [`NesMcu::new`] auto-detection.
    pub fn new_with_mmc3_irq_override(
        file: &INesFile,
        plugin: P,
        irq_override: Mmc3IrqOverride,
    ) -> Self {
        let mcu = NesMcu::new_with_mmc3_irq_override(file, irq_override);
        Self {
            cpu: Cpu::new(),
            p: plugin,
            bus: Bus::new(mcu),
            dma_bus: DmaBus::new(),
            file: file.clone(),
            irq_override,
        }
    }

    pub fn frame_no(&self) -> usize {
        self.bus.mcu().ppu().timing().frame_no()
    }

    /// Run the machine for a single frame.
    ///
    /// Ticks until the PPU reports entering vblank (scanline 241, dot 1),
    /// which marks the natural end of a rendered frame: the PPU has finished
    /// rendering all 240 visible scanlines at that point.
    ///
    /// The stop condition consumes the PPU's dedicated vblank-entry event
    /// ([`crate::nes::ppu::Ppu::take_vblank_entered`]), not the `$2002` VBL
    /// status flag, so emulated-program reads of `$2002` (which clear the
    /// flag) cannot interfere with frame stepping. PPU timing always
    /// advances in `tick`, so the event fires within one frame (~89k dots)
    /// and no tick-count guard is needed.
    ///
    /// Returns the frame status plus borrows of the owned PPU framebuffer
    /// and APU audio buffer for the completed frame.
    ///
    /// Drain contract (per-frame drain is canonical): the APU buffer is
    /// cleared at the start of each call, so every call yields exactly one
    /// frame's samples and sinks drain the returned [`FrameOutput`] the
    /// same frame — SDL (`nescli` event loop) and web (`nes_web::Machine`)
    /// both `push_samples(output.audio)` + `flush_frame()` per frame.
    /// Tick-stepped capture (`nes_cpu_test --dump-audio`) is the deliberate
    /// exception: it never calls this, so the buffer accumulates the whole
    /// run for one end-of-run export. Stepping that loop via this without
    /// a per-frame drain truncates the dump to the last frame (pinned by
    /// `audio_dump_length_matches_under_both_stepping_styles`).
    pub fn process_frame(&mut self) -> FrameOutput<'_> {
        self.bus.mcu_mut().clear_audio_buffer();
        let mut status = ExecuteResult::Continue;
        loop {
            let control = self.tick();
            if !matches!(control, ExecuteResult::Continue) {
                status = control;
                break;
            }
            if self.bus.mcu_mut().ppu_mut().take_vblank_entered() {
                break;
            }
        }

        let mcu = self.bus.mcu();
        // Reborrow as shared: both slices derive from `&self`.
        let frame: &[u8] = mcu.ppu().frame_bytes();
        let audio: &[f32] = mcu.apu().audio_samples();
        // Extend the borrows to the `&mut self` lifetime: `frame`/`audio`
        // point into `self` and outlive this call.
        let frame: &[u8] = unsafe { &*(frame as *const [u8]) };
        let audio: &[f32] = unsafe { &*(audio as *const [f32]) };
        FrameOutput {
            status,
            frame,
            audio,
        }
    }

    /// Execute one master clock tick. Returns the `ExecuteResult`.
    ///
    /// Every device tick (PPU, APU, DMA, CPU) observes the same dot: the
    /// bus's master clock is captured at the top, advanced only after the
    /// dot completes (ADR-0018). Cartridge writes timestamp themselves via
    /// `Mcu::write` — see GLOSSARY "Per-dot contract".
    pub fn tick(&mut self) -> ExecuteResult {
        self.tick_dot(true); // dot 0 — the /NMI sampling phase
        self.bus.advance();
        self.tick_dot(false); // dot 1
        self.bus.advance();
        let control = self.tick_dot_2();
        self.bus.advance();
        control
    }

    /// Dots 0 and 1 of a CPU cycle: tick the PPU and publish its /NMI
    /// line. Only dot 0 is the CPU's sampling phase, so only it passes
    /// `sample = true` to the detector.
    fn tick_dot(&mut self, sample: bool) {
        self.bus.mcu_mut().tick_ppu();
        let nmi = self.bus.mcu_mut().ppu_mut().nmi_line();
        self.cpu.update_nmi_line(nmi, self.bus.clock(), sample);
    }

    fn tick_dot_2(&mut self) -> ExecuteResult {
        let clock = self.bus.clock();
        self.bus.mcu_mut().tick_ppu();
        let nmi = self.bus.mcu_mut().ppu_mut().nmi_line();
        self.cpu.update_nmi_line(nmi, clock, false);

        self.bus.mcu_mut().tick_zapper();

        // No reset-drain freeze gate is needed on this path. The only
        // states `reset()` drains in are already CPU-stalling: an in-flight
        // OAM DMA returns a non-`Idle` `BusOwner` (early return below) and
        // an in-flight DMC DMA freezes the CPU in `Cpu::tick`. The brief
        // pre-freeze window (DMC `DelayForLoad`/`TryHalt` retries) may run
        // a couple of microcodes, but `reset()` discards them wholesale via
        // `cpu.reset()` (queue + interrupt latches) after the drain, and
        // its `is_busy` check runs before each `tick`, so the CPU cannot
        // re-arm DMA after the bus quiesces.
        //
        // Cartridge IRQ is sampled only on this CPU dot (post-`tick_ppu`):
        // mapper counters are PPU-dot-clocked, so sampling here defers a
        // mid-cycle trip to this CPU-cycle boundary (CPU-cycle
        // quantization, pinned by `mmc3_irq_tests`).
        // APU IRQ is read BEFORE `tick_apu` — a transition the step
        // raises or clears becomes CPU-visible on the NEXT dot (one-dot
        // skew, pinned by blargg's `cpu_interrupts_v2` 3-nmi_and_irq).
        // The level is OR-ed for the CPU IRQ line.
        let irq_level = self.bus.mcu().apu_irq_pending() || self.bus.mcu().cartridge_irq_pending();
        self.cpu.set_irq(irq_level);

        self.bus.mcu_mut().tick_apu(clock);
        let owner = self.dma_bus.tick(&mut self.cpu, &mut self.bus);
        if !matches!(owner, BusOwner::Idle) {
            return ExecuteResult::Continue;
        }

        self.cpu.tick(&mut self.bus, &mut self.p)
    }

    /// Maximum dots spent draining in-flight DMA on a machine reset. One
    /// OAM DMA is ~1539 dots; a running DMC channel is suppressed for fresh
    /// requests, so anything beyond this bound means a stuck bus.
    const RESET_DRAIN_LIMIT_DOTS: u32 = 4000;

    /// Soft reset (front-panel RESET button): execution restarts at the
    /// reset vector while state persists — RAM, the cartridge, controllers,
    /// PPU timing, and the bus clock all survive.
    ///
    /// Drain first: any DMA work already accepted by the bus completes
    /// while PPU/APU keep interleaving, before the device resets apply.
    /// The CPU needs no explicit freeze gate for the drain (see
    /// `tick_dot_2`): in-flight DMA already stalls it, any pre-freeze
    /// microcodes are discarded by the resets below, and the `is_busy`
    /// check precedes each `tick` so the CPU cannot re-arm DMA once the
    /// bus is quiet. Fresh DMC fetch requests are suppressed during the
    /// drain so a playing sample channel cannot extend it indefinitely.
    pub fn soft_reset(&mut self) {
        let mut drained = 0_u32;
        while self.dma_bus.is_busy(self.bus.mcu()) && drained < Self::RESET_DRAIN_LIMIT_DOTS {
            // Kill freshly generated DMC fetch requests before each step so
            // the drain terminates even with the DMC channel still playing.
            self.dma_bus.suppress_new_dmc_requests(self.bus.mcu_mut());
            self.tick();
            drained += 1;
        }
        debug_assert!(
            !self.dma_bus.is_busy(self.bus.mcu()),
            "DMA bus did not quiesce"
        );

        self.bus.mcu_mut().reset();
        self.dma_bus.reset();
        self.cpu.reset();
    }

    /// Hard reset (power cycle): the full power-up contract — RAM cleared,
    /// cartridge rebuilt from the loaded ROM, CPU/PPU/APU back to power-up
    /// state, and the bus clock and frame count restarted at zero.
    ///
    /// Unlike [`Self::soft_reset`] this aborts DMA instead of draining it:
    /// power loss does not wait for in-flight transfers. Held controller
    /// buttons stay held — they are physical, not machine state.
    pub fn hard_reset(&mut self) {
        self.dma_bus.reset();
        self.bus.set_clock(SystemClock::default());
        let (file, irq_override) = (self.file.clone(), self.irq_override);
        self.bus.mcu_mut().reset_hard(&file, irq_override);
        self.cpu.reset_hard();
    }

    /// Mixed samples buffered so far (`f32` mono, [`crate::nes::apu::Apu::SAMPLE_RATE`]).
    ///
    /// Kept for tick-stepped capture (e.g. `--dump-audio`): that loop
    /// accumulates the live buffer across many frames without a per-frame
    /// [`FrameOutput`], then exports it once at the end. Per-frame
    /// consumers stepping with [`Self::process_frame`] must read
    /// [`FrameOutput::audio`] / [`FrameOutput::audio_bytes`] instead —
    /// reading this after per-frame stepping sees the last frame only
    /// (pinned by `end_of_run_drain_after_per_frame_stepping_sees_last_frame_only`).
    pub fn audio_samples(&self) -> &[f32] {
        self.bus.mcu().apu().audio_samples()
    }

    pub fn press_controller_a(&mut self, button: Button) {
        self.bus.mcu_mut().press_controller_a(button);
    }

    pub fn release_controller_a(&mut self, button: Button) {
        self.bus.mcu_mut().release_controller_a(button);
    }

    pub fn press_controller_b(&mut self, button: Button) {
        self.bus.mcu_mut().press_controller_b(button);
    }

    pub fn release_controller_b(&mut self, button: Button) {
        self.bus.mcu_mut().release_controller_b(button);
    }

    pub fn connect_zapper(&mut self, connected: bool) {
        self.bus.mcu_mut().connect_zapper(connected);
    }

    pub fn aim_zapper(&mut self, x: u16, y: u16) {
        self.bus.mcu_mut().aim_zapper(x, y);
    }

    pub fn trigger_zapper(&mut self) {
        self.bus.mcu_mut().trigger_zapper();
    }

    pub fn set_color_theme(&mut self, theme: ColorTheme) {
        self.bus.mcu_mut().ppu_mut().set_color_theme(theme);
    }

    /// Set the CPU program counter.
    /// Drains any pending microcodes (e.g. from reset) before setting PC.
    /// Uses the CPU-only drain so APU/DMA state is not advanced during
    /// setup; `clock` advances by `queue.len()` dots. For full
    /// interleaving, call `run_to_instruction_boundary` instead.
    pub fn set_pc(&mut self, pc: u16) {
        let mut empty = EmptyPlugin::new();
        self.cpu
            .run_to_instruction_boundary(&mut self.bus, &mut empty);
        self.cpu.set_pc(pc);
    }

    pub fn cpu(&self) -> &Cpu {
        &self.cpu
    }

    pub fn mcu(&self) -> &NesMcu {
        self.bus.mcu()
    }

    pub fn mcu_mut(&mut self) -> &mut NesMcu {
        self.bus.mcu_mut()
    }

    #[cfg(test)]
    fn system_cycles(&self) -> u64 {
        self.bus.clock().cycles()
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::mcu::Mcu;

    use crate::EmptyPlugin;

    use super::*;
    use crate::wait_rom;

    /// Create a minimal valid iNES file for testing
    fn test_nes_file() -> INesFile {
        INesFile::new(wait_rom::bare_spin_image()).expect("shared bare spin ROM must parse")
    }

    /// PRG-ROM with `BIT $2002 / BPL top` and NMI disabled, so the only
    /// exit is a real status poll observing VBL.
    fn vbl_status_wait_nes_file() -> INesFile {
        INesFile::new(wait_rom::vbl_status_wait_image()).expect("shared VBL wait ROM must parse")
    }

    /// PRG-ROM with `LDA $0010 / BNE top` at $8000 and an RTI NMI handler.
    /// The RAM flag starts asserted, so the loop waits for the NMI handler
    /// to clear it: frozen and unfrozen runs can be compared at frame
    /// boundaries.
    fn ram_flag_wait_nes_file() -> INesFile {
        INesFile::new(wait_rom::ram_flag_wait_image()).expect("shared RAM wait ROM must parse")
    }

    /// One dot of program-visible state: CPU registers (PC excluded: a
    /// frozen loop pins it while the control keeps executing), the polled
    /// status byte of the ROM's own wait register, interrupt line levels,
    /// and the framebuffer at the last VBL entry. Framebuffers are compared
    /// byte-for-byte.
    #[derive(Debug, PartialEq, Eq)]
    struct TraceState {
        registers: [u8; 6],
        status: u8,
        interrupts: [u8; 3],
        frame: Vec<u8>,
    }

    /// Collect A at the end of each `LDA $2007`, i.e. when PC is the BPL
    /// at $8005. The recorder is a plugin seam: no in-loop store, so an
    /// excluded poll is the only reason new values keep appearing.
    struct ARecorder(Rc<RefCell<Vec<u8>>>);

    impl Plugin<NesMcu> for ARecorder {
        fn start(&mut self, _: &crate::view::MachineView<'_, NesMcu>) {}

        fn end(&mut self, view: &crate::view::MachineView<'_, NesMcu>) {
            if view.cpu.pc == 0x8005 {
                self.0.borrow_mut().push(view.cpu.a);
            }
        }
    }

    /// PRG: `LDA #$FF; loop: LDA $2007; BPL loop`. The loop only reads
    /// PPUDATA; every read advances v and refills the read buffer.
    fn ppudata_poll_nes_file() -> INesFile {
        INesFile::new(wait_rom::ppudata_poll_image()).expect("shared PPUDATA wait ROM must parse")
    }

    /// PRG: controller poll loop (`LDA $4016 / BNE`), a device-register
    /// poll that freezes while replaying its read effects.
    fn controller_poll_nes_file() -> INesFile {
        INesFile::new(wait_rom::controller_poll_image())
            .expect("shared controller poll ROM must parse")
    }

    /// PRG: `LDA $4016 / BNE` poll of controller port B.
    fn controller_b_poll_nes_file() -> INesFile {
        INesFile::new(wait_rom::controller_b_poll_image())
            .expect("shared controller B poll ROM must parse")
    }

    /// PRG: `LDA $4017 / BNE` poll of the APU frame-IRQ/status register.
    fn apu_status_poll_nes_file() -> INesFile {
        INesFile::new(wait_rom::apu_status_poll_image())
            .expect("shared APU status poll ROM must parse")
    }

    /// Dots to settle after VBL entry before the trace window: the
    /// counterfactual VBL read and its back edge land a few dots after the
    /// entry dot, so sampling at entry catches one run mid-exit. A full
    /// three-cycle poll read must also finish, because the frozen run
    /// replays it without consuming the real bus read.
    const VBL_SETTLE: usize = 16;

    /// Dots of per-dot register/status/interrupt trace after the settle.
    const TRACE_DOTS: usize = 12;

    /// Advance three frames by dot stepping. For each VBL entry, record a
    /// per-dot trace of all visible register values (except PC), the polled
    /// status byte, and interrupt line levels for a window of dots, plus the
    /// framebuffer bytes at VBL entry. The window starts after a short
    /// settle so the counterfactual VBL read and its back edge have resolved
    /// in both runs. No internal freeze bit is observed.
    fn vbl_frame_trace(
        machine: &mut NesMachine<EmptyPlugin<NesMcu>>,
        status_addr: Option<u16>,
    ) -> Vec<TraceState> {
        let mut states = Vec::new();
        for _ in 0..3 {
            loop {
                assert_ne!(machine.tick(), ExecuteResult::Halt);
                if machine.mcu_mut().ppu_mut().take_vblank_entered() {
                    break;
                }
            }
            for _ in 0..VBL_SETTLE {
                assert_ne!(machine.tick(), ExecuteResult::Halt);
            }
            let frame = machine.mcu().ppu().frame_bytes().to_vec();
            for _ in 0..TRACE_DOTS {
                assert_ne!(machine.tick(), ExecuteResult::Halt);
                let s = machine.cpu().snapshot();
                states.push(TraceState {
                    registers: [s.a, s.x, s.y, s.sp, s.status, u8::from(s.halt)],
                    status: status_addr.map_or(0, |addr| machine.mcu().peek(addr)),
                    interrupts: [
                        u8::from(machine.mcu().ppu().nmi_line()),
                        u8::from(machine.mcu().apu_irq_pending()),
                        u8::from(machine.mcu().cartridge_irq_pending()),
                    ],
                    frame: frame.clone(),
                });
            }
        }
        states
    }

    /// The headline `$2002` VBL status wait must be invisible to the
    /// program: three frames of frozen and never-freeze control execution
    /// agree byte-for-byte on the framebuffer, and identically on the
    /// register/polled-status trace and interrupt timing. NMI is disabled
    /// in the shared ROM, so the only exit is the counterfactual poll that
    /// observes VBL (the Synthesized VBL exit).
    #[test]
    fn frozen_and_unfrozen_vbl_status_wait_frames_match() {
        assert_frames_match(
            &vbl_status_wait_nes_file(),
            Some(0x2002),
            |_| {},
            "VBL status wait framebuffer/register/interrupt trace diverged",
        );
    }

    /// Build two machines from one ROM, disable spin declaration on the
    /// control, apply the same setup to both, then compare three frames of
    /// visible state. The harness owns the common comparison so each wait
    /// test only supplies its ROM, its setup, and its failure message.
    fn assert_frames_match(
        rom: &INesFile,
        status_addr: Option<u16>,
        mut setup: impl FnMut(&mut NesMachine<EmptyPlugin<NesMcu>>),
        divergence: &str,
    ) {
        let mut frozen = NesMachine::new(rom, EmptyPlugin::new());
        let mut unfrozen = NesMachine::new(rom, EmptyPlugin::new());
        unfrozen.cpu.set_spin_declaration_disabled_for_test(true);
        setup(&mut frozen);
        setup(&mut unfrozen);

        let frozen_trace = vbl_frame_trace(&mut frozen, status_addr);
        let control_trace = vbl_frame_trace(&mut unfrozen, status_addr);
        assert_eq!(frozen_trace.len(), control_trace.len());
        // Vacuity is not asserted here: once the wait exits, both runs park
        // at the same self-loop, so their program-visible state converges and
        // cannot distinguish a frozen run from an executing one. The same
        // freeze bit can never prove it either. The declaring tests in
        // `cpu/spin_tests.rs` prove the freeze; this harness proves that a
        // declared spin changes nothing a program can observe.
        for (dot, (frozen_state, control_state)) in
            frozen_trace.iter().zip(&control_trace).enumerate()
        {
            assert_eq!(
                frozen_state.frame, control_state.frame,
                "dot {dot} framebuffer bytes diverged: {divergence}"
            );
            assert_eq!(
                frozen_state.registers, control_state.registers,
                "dot {dot} register trace diverged: {divergence}"
            );
            assert_eq!(
                frozen_state.status, control_state.status,
                "dot {dot} polled status diverged: {divergence}"
            );
            assert_eq!(
                frozen_state.interrupts, control_state.interrupts,
                "dot {dot} interrupt timing diverged: {divergence}"
            );
        }
    }

    /// A bare self-loop wait must produce the same framebytes and visible
    /// CPU/interrupt state with its spin declaration enabled and disabled.
    /// The control is test-only; dot stepping keeps the comparison honest.
    #[test]
    fn frozen_and_unfrozen_self_loop_frames_match() {
        assert_frames_match(
            &test_nes_file(),
            None,
            |machine| machine.set_pc(0x8000),
            "framebuffer/register/interrupt trace diverged",
        );
    }

    /// A RAM flag wait that exits through its NMI handler must also be
    /// frame-equivalent: freezing the poll must not change the visible
    /// framebuffer, register, or interrupt state compared with dot-stepping
    /// the same program with spin declaration disabled.
    #[test]
    fn frozen_and_unfrozen_nmi_flag_wait_frames_match() {
        assert_frames_match(
            &ram_flag_wait_nes_file(),
            Some(0x0010),
            |machine| machine.mcu_mut().write(0x0010, 1, SystemClock::default()),
            "NMI-flag wait framebuffer/register/interrupt trace diverged",
        );
    }

    /// A controller poll freezes, but its device action (the shift and its
    /// read-driven output-enable state) must be replayed at loop cadence,
    /// leaving framebytes, registers, and interrupt lines frame-equivalent.
    #[test]
    fn frozen_and_unfrozen_controller_poll_frames_match() {
        assert_frames_match(
            &controller_poll_nes_file(),
            Some(0x4016),
            |machine| machine.mcu_mut().press_controller_a(Button::A),
            "controller-poll framebuffer/register/interrupt trace diverged",
        );
    }

    /// Controller port B has its own shift chain and /OE bit. A held B
    /// button keeps the poll asserted, so only device replay can keep the
    /// frozen and never-freeze traces identical.
    #[test]
    fn frozen_and_unfrozen_controller_b_poll_frames_match() {
        assert_frames_match(
            &controller_b_poll_nes_file(),
            Some(0x4017),
            |machine| {
                machine.set_pc(0x8000);
                let clock = SystemClock::default();
                // Hold the strobe so B bit 0 stays asserted for every
                // real and replayed read, as in the controller A case.
                machine.mcu_mut().write(0x4016, 1, clock);
                machine.mcu_mut().press_controller_b(Button::A);
            },
            "controller B poll framebuffer/register/interrupt trace diverged",
        );
    }

    /// The APU status register is a device poll too. Holding DMC enabled
    /// and starting a sample keeps bit 4 asserted, so the loop stays frozen
    /// while its read effects are replayed; the frame comparison covers the
    /// newly replayed `$4015` path.
    #[test]
    fn frozen_and_unfrozen_apu_status_poll_frames_match() {
        assert_frames_match(
            &apu_status_poll_nes_file(),
            Some(0x4015),
            |machine| {
                let clock = SystemClock::default();
                machine.mcu_mut().write(0x4015, 0x10, clock);
                machine.mcu_mut().write(0x4011, 0x40, clock);
                machine.mcu_mut().write(0x4012, 0x00, clock);
                machine.mcu_mut().write(0x4013, 0x01, clock);
                machine.mcu_mut().write(0x4015, 0x10, clock);
            },
            "APU status poll framebuffer/register/interrupt trace diverged",
        );
    }

    /// A `$2007` poll is not a declaration candidate: its reads must keep
    /// refilling the buffer and advancing v. Seeding $2000-$2003 with
    /// 11/22/33/44 and rewinding v to $2000 makes those effects visible as
    /// the A sequence 00/11/22/33 at each loop-top read.
    #[test]
    fn ppudata_poll_never_declares_a_spin_and_keeps_reading() {
        let file = ppudata_poll_nes_file();
        let results = Rc::new(RefCell::new(Vec::new()));
        let mut machine = NesMachine::new(&file, ARecorder(results.clone()));
        for (addr, value) in [
            (0x2006, 0x20),
            (0x2006, 0x00),
            (0x2007, 0x11),
            (0x2007, 0x22),
            (0x2007, 0x33),
            (0x2007, 0x44),
            (0x2006, 0x20),
            (0x2006, 0x00),
        ] {
            machine.mcu_mut().write(addr, value, SystemClock::default());
        }

        let mut dot = 0u64;
        while results.borrow().len() < 4 {
            assert_ne!(machine.tick(), ExecuteResult::Halt);
            dot += 1;
            assert!(dot < 5000, "PPUDATA poll did not produce four reads");
        }
        // The first result is the old buffer; each later one is the byte
        // read from v on the preceding poll. A frozen poll would stop after
        // its second iteration, so this sequence also proves no declaration
        // without asserting the internal freeze bit.
        assert_eq!(results.borrow().as_slice(), &[0x00, 0x11, 0x22, 0x33]);
    }

    #[test]
    fn process_frame_output_carries_audio_byte_view() {
        use crate::nes::ppu::{NES_HEIGHT, NES_WIDTH};

        let file = test_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new());
        let output = machine.process_frame();

        // Per-frame read: frame bytes plus both audio views of one frame.
        assert_eq!(output.frame.len(), NES_WIDTH * NES_HEIGHT * 4);
        assert_eq!(output.audio_bytes().len(), output.audio.len() * 4);
        let expected: &[u8] = bytemuck::cast_slice(output.audio);
        assert_eq!(
            output.audio_bytes(),
            expected,
            "byte view must match the sample slice exactly"
        );
    }

    /// Tick until `frames` vblank entries have fired, mirroring
    /// `nes_cpu_test --dump-audio`'s capture loop: per-dot stepping never
    /// calls `process_frame`, so the APU buffer accumulates the whole run
    /// and a single end-of-run drain sees every frame.
    fn tick_frames(machine: &mut NesMachine<EmptyPlugin<NesMcu>>, frames: usize) {
        let mut seen = 0;
        while seen < frames {
            machine.tick();
            if machine.mcu_mut().ppu_mut().take_vblank_entered() {
                seen += 1;
            }
        }
    }

    /// Multi-frame dump length is identical under tick-stepping (single
    /// end-of-run drain, the `--dump-audio` cadence) and under
    /// `process_frame` stepping (per-frame drain of each `FrameOutput`).
    ///
    /// Regression guard for the drain trap (§8): if the capture loop ever
    /// steps via `process_frame` — whose start-of-frame clear drops every
    /// prior frame — while keeping its single end-of-run drain, the dump
    /// truncates to the last frame and this test fails on the length.
    #[test]
    fn audio_dump_length_matches_under_both_stepping_styles() {
        const FRAMES: usize = 3;
        let file = test_nes_file();

        let mut ticked = NesMachine::new(&file, EmptyPlugin::new());
        tick_frames(&mut ticked, FRAMES);
        let ticked_total = ticked.audio_samples().len();
        assert!(
            ticked_total > 0,
            "capture must accumulate samples over {FRAMES} frames"
        );

        let mut framed = NesMachine::new(&file, EmptyPlugin::new());
        let mut framed_total = 0;
        for _ in 0..FRAMES {
            let output = framed.process_frame();
            framed_total += output.audio.len();
        }

        assert_eq!(
            ticked_total, framed_total,
            "tick-stepped end-of-run drain must see the same samples as \
             per-frame drains ({FRAMES} frames)"
        );
    }

    /// `process_frame` clears the APU buffer at the start of each frame,
    /// so an end-of-run read after per-frame stepping sees only the last
    /// frame. This pins the clearing half of the drain contract: removing
    /// the clear (or adding one to the tick path) breaks this test.
    #[test]
    fn end_of_run_drain_after_per_frame_stepping_sees_last_frame_only() {
        const FRAMES: usize = 3;
        let file = test_nes_file();

        let mut machine = NesMachine::new(&file, EmptyPlugin::new());
        let mut last_len = 0;
        for _ in 0..FRAMES {
            let output = machine.process_frame();
            last_len = output.audio.len();
        }
        assert!(last_len > 0, "each frame must mix samples");
        assert_eq!(
            machine.audio_samples().len(),
            last_len,
            "buffer after per-frame stepping holds the last frame only"
        );

        let mut whole = NesMachine::new(&file, EmptyPlugin::new());
        tick_frames(&mut whole, FRAMES);
        assert!(
            machine.audio_samples().len() < whole.audio_samples().len(),
            "end-of-run drain after per-frame stepping truncates the run"
        );
    }

    #[test]
    fn test_process_frame_waits_for_vblank() {
        let file = test_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new());

        // process_frame should return as soon as the PPU reaches VBlank
        // (scanline 241, dot 1 = after 241*341 + 1 = 82,262 PPU dots = ~27,421 CPU cycles).
        // The call must complete within one frame (~89k dots).
        let result = machine.process_frame().status;
        assert_eq!(result, ExecuteResult::Continue);

        // After one frame the PPU should be at or past scanline 241.
        // We verify by running a second frame — it should also complete without hitting
        // the safety limit, proving VBlank fires consistently every frame.
        let result = machine.process_frame().status;
        assert_eq!(result, ExecuteResult::Continue);
    }

    /// PRG: `LDA #$02; STA $4014; JMP $8002` — arms a fresh OAM DMA every
    /// ~9 CPU cycles, so the bus is virtually never idle.
    fn dma_loop_nes_file() -> INesFile {
        let mut rom = Vec::new();
        rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a, 1, 1, 0x00, 0x00]);
        rom.extend_from_slice(&[0; 8]);

        let mut prg = vec![0_u8; 16 * 1024];
        let program: [u8; 8] = [0xA9, 0x02, 0x8D, 0x14, 0x40, 0x4C, 0x02, 0x80];
        prg[..program.len()].copy_from_slice(&program);
        // Reset vector $8000 (NROM-128 mirrors its single page at $C000 too).
        prg[0x3FFA..=0x3FFF].copy_from_slice(&[0x00, 0x80, 0x00, 0x80, 0x00, 0x80]);
        rom.extend_from_slice(&prg);
        rom.extend(std::iter::repeat_n(0, 8 * 1024));
        INesFile::new(rom).unwrap()
    }

    #[test]
    fn soft_reset_preserves_ram_hard_reset_clears_it() {
        use crate::mcu::Mcu;

        let file = test_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new());
        machine
            .mcu_mut()
            .write(0x0042, 0xAB, SystemClock::default());

        machine.soft_reset();
        assert_eq!(
            machine.mcu().peek(0x0042),
            0xAB,
            "soft reset (RESET button) must preserve RAM"
        );

        machine.hard_reset();
        assert_eq!(
            machine.mcu().peek(0x0042),
            0x00,
            "hard reset (power cycle) must clear RAM"
        );
    }

    /// UxROM with distinct 16 KB banks: bank 0 filled `0x11`, bank 1 filled
    /// `0x22`, so the switchable `$8000` window reports which bank is mapped.
    fn uxrom_nes_file() -> INesFile {
        let mut rom = Vec::new();
        // Mapper 2 (UxROM), 2x16 KB PRG, 1x8 KB CHR.
        rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a, 2, 1, 0x20, 0x00]);
        rom.extend_from_slice(&[0; 8]);

        let mut prg = vec![0x11_u8; 16 * 1024];
        prg.extend(std::iter::repeat_n(0x22_u8, 16 * 1024));
        // Reset vector $8000 (plus NMI/IRQ) at the end of the fixed bank.
        prg[0x7FFA..=0x7FFF].copy_from_slice(&[0x00, 0x80, 0x00, 0x80, 0x00, 0x80]);
        rom.extend_from_slice(&prg);
        rom.extend(std::iter::repeat_n(0, 8 * 1024));
        INesFile::new(rom).unwrap()
    }

    #[test]
    fn soft_reset_preserves_mapper_bank_hard_reset_rebuilds_it() {
        use crate::mcu::Mcu;

        let file = uxrom_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new());
        assert_eq!(machine.mcu().peek(0x8000), 0x11);

        // Map bank 1 into the switchable window.
        machine.mcu_mut().write(0x8000, 1, SystemClock::default());
        assert_eq!(machine.mcu().peek(0x8000), 0x22);

        machine.soft_reset();
        assert_eq!(
            machine.mcu().peek(0x8000),
            0x22,
            "soft reset must not touch mapper state"
        );

        machine.hard_reset();
        assert_eq!(
            machine.mcu().peek(0x8000),
            0x11,
            "hard reset rebuilds the cartridge from the ROM"
        );
    }

    /// PRG: `LDA #$42; LDX #$43; LDY #$44; JMP $8000` — dirties the
    /// register file, then loops.
    fn reg_loop_nes_file() -> INesFile {
        let mut rom = Vec::new();
        rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a, 1, 1, 0x00, 0x00]);
        rom.extend_from_slice(&[0; 8]);

        let mut prg = vec![0_u8; 16 * 1024];
        let program: [u8; 9] = [0xA9, 0x42, 0xA2, 0x43, 0xA0, 0x44, 0x4C, 0x00, 0x80];
        prg[..program.len()].copy_from_slice(&program);
        // Reset vector $8000 (NROM-128 mirrors its single page at $C000 too).
        prg[0x3FFA..=0x3FFF].copy_from_slice(&[0x00, 0x80, 0x00, 0x80, 0x00, 0x80]);
        rom.extend_from_slice(&prg);
        rom.extend(std::iter::repeat_n(0, 8 * 1024));
        INesFile::new(rom).unwrap()
    }

    #[test]
    fn soft_reset_preserves_registers_hard_reset_restores_power_up() {
        let file = reg_loop_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new());
        for _ in 0..1000 {
            machine.tick();
        }
        assert_eq!(machine.cpu().a(), 0x42);
        assert_eq!(machine.cpu().x(), 0x43);
        assert_eq!(machine.cpu().y(), 0x44);
        assert_eq!(machine.cpu().sp(), 0xFD);

        machine.soft_reset();
        assert_eq!(machine.cpu().a(), 0x42, "soft reset preserves A");
        assert_eq!(machine.cpu().x(), 0x43, "soft reset preserves X");
        assert_eq!(machine.cpu().y(), 0x44, "soft reset preserves Y");
        assert_eq!(
            machine.cpu().sp(),
            0xFA,
            "soft reset drops SP by three (discarded push)"
        );

        machine.hard_reset();
        assert_eq!(machine.cpu().a(), 0x00, "hard reset clears A");
        assert_eq!(machine.cpu().x(), 0x00, "hard reset clears X");
        assert_eq!(machine.cpu().y(), 0x00, "hard reset clears Y");
        assert_eq!(machine.cpu().sp(), 0xFD, "hard reset restores power-up SP");
        assert_eq!(
            machine.cpu().status(),
            0x24,
            "hard reset restores power-up status (I + unused)"
        );
    }

    #[test]
    fn soft_reset_preserves_clock_hard_reset_restarts_it() {
        let file = test_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new());
        assert_eq!(machine.process_frame().status, ExecuteResult::Continue);
        assert_eq!(machine.frame_no(), 1);
        let cycles = machine.system_cycles();
        assert!(cycles > 0);

        machine.soft_reset();
        assert_eq!(machine.frame_no(), 1, "soft reset keeps the PPU running");
        assert_eq!(
            machine.system_cycles(),
            cycles,
            "soft reset keeps the bus clock running"
        );

        machine.hard_reset();
        assert_eq!(machine.frame_no(), 0, "hard reset restarts frame count");
        assert_eq!(
            machine.system_cycles(),
            0,
            "hard reset restarts the bus clock"
        );
        // The rebuilt machine still runs: frames complete normally.
        assert_eq!(machine.process_frame().status, ExecuteResult::Continue);
        assert_eq!(machine.frame_no(), 1);
    }

    #[test]
    fn hard_reset_preserves_held_buttons_but_rewinds_shift() {
        use crate::mcu::Mcu;

        let file = test_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new());
        machine.press_controller_a(Button::A);

        machine.hard_reset();

        // The pad is physical: the held button still reports pressed on the
        // first shift-register read, with no strobe written since power-up.
        assert_eq!(machine.mcu().peek(0x4016) & 0x01, 0x01);
    }

    #[test]
    fn reset_drains_in_flight_oam_dma_before_applying() {
        let file = dma_loop_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new());

        // Warm up until the loop's first $4014 write has armed a DMA.
        for _ in 0..10_000 {
            machine.tick();
            if machine.dma_bus.is_busy(machine.bus.mcu()) {
                break;
            }
        }
        assert!(
            machine.dma_bus.is_busy(machine.bus.mcu()),
            "test ROM never armed an OAM DMA"
        );

        // Resetting mid-DMA must quiesce the bus first, bounded by the
        // internal drain limit (no hang), then apply cleanly.
        machine.soft_reset();

        assert!(!machine.dma_bus.is_busy(machine.bus.mcu()));

        // The post-reset machine still runs: frames complete normally.
        assert_eq!(machine.process_frame().status, ExecuteResult::Continue);
    }

    /// Soft reset (front-panel RESET button) preserves the visible screen:
    /// the PPU keeps running mid-frame and the owned framebuffer is
    /// untouched, so post-reset pixels equal pre-reset pixels.
    #[test]
    fn soft_reset_preserves_framebuffer_bytes() {
        use crate::nes::ppu::{NES_HEIGHT, NES_WIDTH};

        let file = test_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new());
        machine.process_frame();
        let before = machine.mcu().ppu().frame_bytes().to_vec();
        assert_eq!(before.len(), NES_WIDTH * NES_HEIGHT * 4);
        assert!(
            before.iter().any(|&b| b != 0),
            "test ROM must render a flushed (opaque) frame so the test is sensitive"
        );

        machine.soft_reset();

        assert_eq!(
            machine.mcu().ppu().frame_bytes(),
            &before[..],
            "soft reset must preserve the framebuffer"
        );
    }

    /// Hard reset (power cycle) returns the framebuffer to its power-up
    /// contents: opaque black. The frame is rendered under an all-white
    /// theme first so the fill is distinguishable from rendered output.
    #[test]
    fn hard_reset_clears_framebuffer_to_opaque_black() {
        use crate::nes::ppu::palette::ColorTheme;

        let file = test_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new());
        machine.set_color_theme(ColorTheme::load_from_pal_buf(&[0xFF; 192]));
        machine.process_frame();
        assert!(
            machine.mcu().ppu().frame_bytes().iter().any(|&b| b != 0),
            "test ROM must render a flushed frame so the test is sensitive"
        );

        machine.hard_reset();

        assert!(
            machine
                .mcu()
                .ppu()
                .frame_bytes()
                .as_chunks::<4>()
                .0
                .iter()
                .all(|px| *px == [0, 0, 0, 255]),
            "hard reset must fill the framebuffer with opaque black"
        );
    }

    /// Hard reset preserves the host color theme: the first frame rendered
    /// after a power cycle matches the pre-reset frame under the same
    /// theme. The blue-theme control proves the comparison is sensitive.
    #[test]
    fn hard_reset_preserves_color_theme() {
        use crate::nes::ppu::palette::ColorTheme;

        fn solid(r: u8, g: u8, b: u8) -> ColorTheme {
            let mut buf = [0_u8; 192];
            for chunk in buf.as_chunks_mut::<3>().0 {
                chunk.copy_from_slice(&[r, g, b]);
            }
            ColorTheme::load_from_pal_buf(&buf)
        }

        let file = test_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new());
        machine.set_color_theme(solid(0xFF, 0x00, 0x00));
        machine.process_frame();
        let before = machine.mcu().ppu().frame_bytes().to_vec();

        machine.hard_reset();
        machine.process_frame();
        assert_eq!(
            machine.mcu().ppu().frame_bytes(),
            &before[..],
            "hard reset must preserve the color theme"
        );

        let mut other = NesMachine::new(&file, EmptyPlugin::new());
        other.set_color_theme(solid(0x00, 0x00, 0xFF));
        other.process_frame();
        assert_ne!(
            other.mcu().ppu().frame_bytes(),
            &before[..],
            "theme control: a different theme must render different pixels"
        );
    }
}
