use crate::{
    Cpu, EmptyPlugin, ExecuteResult, Flag, Plugin, SystemClock,
    bus::Bus,
    dma_bus::{BusOwner, DmaBus},
    ines::INesFile,
    mcu::Mcu,
    nes::{Mmc3IrqOverride, NesMcu, controller::Button, ppu::palette::ColorTheme},
    spin_skip::{
        POLL_ADDR, QuietSkip, SpinBackedgeHit, SpinDeclaration, backedge_crosses_page,
        declaring_backedge_parks_through_vbl_boundary, next_parked_iteration_covers_vbl_set,
    },
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
    /// Test seam for the $2002 spin-wait skip (spec #167, ticket #168):
    /// whether this machine may fast-forward a recognized `BIT/LDA $2002`
    /// poll loop across the VBL flag set. No skip exists yet, so until
    /// tickets #169-#171 land this flag changes nothing.
    spin_skip_enabled: bool,
    /// Ticket #169: the declared poll loop, once the shape gate accepts a
    /// back-edge. `None` until then. Ticket #170 consumes this at the
    /// declaring back-edge to start (and later end) the quiet-span skip.
    spin_declaration: Option<SpinDeclaration>,
    /// Ticket #170: the armed quiet-span skip for the declared loop —
    /// `Some` from the declaring back-edge (switch on, `/NMI` quiet, grid
    /// shape sound) until a wake ends it. While armed and past its first
    /// parked dot the CPU stays parked and poll reads are replayed on the
    /// anchor grid; see [`Self::tick_quiet_span`].
    spin_skip: Option<QuietSkip>,
    /// Counterfactual polls replayed so far (ticket #170). Written only on
    /// the skip path; observed only through the `#[cfg(test)` probe.
    spin_skip_replays: u64,
    /// Ticket #171 test probe: the dot of every counterfactual poll
    /// replay, in order. Test-only storage — the release build keeps no
    /// per-replay history (the probe exists to pin that a replayed read
    /// lands exactly on the control's poll dot, never moved).
    #[cfg(test)]
    spin_skip_replay_clocks: Vec<u64>,
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
            spin_skip_enabled: false,
            spin_declaration: None,
            spin_skip: None,
            spin_skip_replays: 0,
            #[cfg(test)]
            spin_skip_replay_clocks: Vec::new(),
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
        let clock = self.bus.clock();
        self.cpu.update_nmi_line(nmi, clock, sample);
        // Ticket #171: while a quiet span is armed, watch the /NMI line on
        // EVERY dot, not just CPU dots. The wake rule keys on the
        // dot-exact rise dot (the control's branch-cycle poll reads a
        // sample taken two dots earlier), and a rise set by this dot's
        // `tick_ppu` is attributed to this dot — the same attribution
        // the machine's own `update_nmi_line` call just above made.
        if let Some(skip) = &mut self.spin_skip
            && nmi != skip.span_nmi_line()
        {
            skip.record_nmi_event(clock.cycles());
        }
    }

    fn tick_dot_2(&mut self) -> ExecuteResult {
        let clock = self.bus.clock();
        self.bus.mcu_mut().tick_ppu();
        let nmi = self.bus.mcu_mut().ppu_mut().nmi_line();
        self.cpu.update_nmi_line(nmi, clock, false);
        // Ticket #171: the CPU-dot half of the every-dot /NMI line watch
        // (see `tick_dot`) — a rise set by this dot's `tick_ppu` is
        // recorded on this dot.
        if let Some(skip) = &mut self.spin_skip
            && nmi != skip.span_nmi_line()
        {
            skip.record_nmi_event(clock.cycles());
        }

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
        // Ticket #171: the IRQ level is published every CPU dot, parked or
        // not. While a quiet span is armed, record the first CPU dot whose
        // level left the span snapshot — the wake rule keys on it.
        if let Some(skip) = &mut self.spin_skip
            && irq_level != skip.span_irq_level()
        {
            skip.record_irq_event(clock.cycles());
        }

        self.bus.mcu_mut().tick_apu(clock);
        // Ticket #171 OAM dot-precision: a mid-span `$4014` write arms a
        // pending request that the dma tick takes on the NEXT CPU dot (its
        // halt cycle). This pre-tick check sees the pending first, so end
        // the span at that very dot — the DMA owns the bus from its halt
        // cycle on and the parked CPU resumes from the last completed
        // real iteration (the fallback rule: no partial replay, the grid
        // is never stretched by an unmodelled stall).
        if self.spin_skip.is_some() && self.bus.mcu().has_oam_dma_pending() {
            self.end_quiet_span();
        }
        let owner = self.dma_bus.tick(&mut self.cpu, &mut self.bus);
        if !matches!(owner, BusOwner::Idle) {
            return ExecuteResult::Continue;
        }

        // Ticket #170: an armed quiet span may consume this dot — the
        // CPU stays parked and a counterfactual poll read may be applied
        // on its grid dot. `None` means real execution must run.
        if let Some(result) = self.tick_quiet_span(clock) {
            return result;
        }

        let control = self.cpu.tick(&mut self.bus, &mut self.p);
        // Ticket #169 shape gate: a taken back-edge this dot may have
        // latched a poll-loop shape hit. Drain it on the same dot it was
        // latched on — the CPU surface stays one transient field (ADR
        // 0010), and the clock is the declaring dot itself (ADR 0018).
        if let Some(hit) = self.cpu.take_spin_backedge_hit() {
            self.declare_spin_loop(hit, clock);
        }
        control
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
        // The declared loop belonged to the pre-reset program.
        self.spin_declaration = None;
        self.spin_skip = None;
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
        // Power cycle: the declared loop of the previous power-on is gone.
        self.spin_declaration = None;
        self.spin_skip = None;
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
        // A manual PC move leaves any armed span's loop behind: drop it
        // before the CPU runs anywhere else.
        self.end_quiet_span();
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

    /// Test seam (spec #167, ticket #168): enable or disable the $2002
    /// spin-wait skip on this machine, so differential tests can run a
    /// skip-on build against a skip-off control from the same binary.
    /// Default OFF. Currently a no-op: no skip is implemented yet (the
    /// skip itself lands with tickets #169-#171), so both settings run
    /// identical real execution.
    ///
    /// This is a *machine-level* seam. The reverted freeze's CPU-level
    /// `set_spin_declaration_disabled_for_test` hook (HISTORY.md) stays
    /// gone; do not reintroduce CPU test hooks.
    pub fn set_spin_skip_enabled(&mut self, enabled: bool) {
        self.spin_skip_enabled = enabled;
    }

    /// Current [`Self::set_spin_skip_enabled`] setting.
    pub fn spin_skip_enabled(&self) -> bool {
        self.spin_skip_enabled
    }

    /// Ticket #169 test seam: the declared spin-wait loop, if the shape
    /// gate has declared one yet. `None` until the CPU executes a
    /// declarable back-edge outside any DMA window.
    #[cfg(test)]
    pub(crate) fn spin_declaration(&self) -> Option<SpinDeclaration> {
        self.spin_declaration
    }

    pub(crate) fn system_cycles(&self) -> u64 {
        self.bus.clock().cycles()
    }

    /// Ticket #170 test probe: counterfactual polls replayed so far.
    /// Zero unless a quiet span engaged for this machine.
    #[cfg(test)]
    pub(crate) fn spin_skip_replayed_polls(&self) -> u64 {
        self.spin_skip_replays
    }

    /// Ticket #171 test probe: the dot of every replayed poll, oldest
    /// first. Each must land exactly on a poll dot of the skip-off
    /// control's real lattice — the read is never moved.
    #[cfg(test)]
    pub(crate) fn spin_skip_replay_clocks(&self) -> Vec<u64> {
        self.spin_skip_replay_clocks.clone()
    }

    /// Ticket #170 test probe: is a quiet span armed right now?
    #[cfg(test)]
    pub(crate) fn spin_skip_active(&self) -> bool {
        self.spin_skip.is_some()
    }

    /// Ticket #170 test probe: the PPU open-bus latch (a replayed
    /// `$2002` read must refresh bits 7-5 only — bits 4-0 survive).
    #[cfg(test)]
    pub(crate) fn ppu_open_bus_latch_for_test(&mut self) -> u8 {
        self.bus.mcu_mut().ppu_mut().open_bus_latch_for_test()
    }

    /// Ticket #170 test probe: the PPU decay clock — must advance by
    /// exactly the skipped ticks.
    #[cfg(test)]
    pub(crate) fn ppu_decay_cycle_for_test(&self) -> u64 {
        self.bus.mcu().ppu().decay_cycle_for_test()
    }

    /// Ticket #169 — record a shape-gate hit as the machine's declared
    /// spin-wait loop. Runs only when the CPU just executed a taken
    /// back-edge matching the declarable $2002 poll-loop shape (see
    /// [`crate::spin_skip`]), so the cost outside that shape is zero
    /// (#165 Q7).
    ///
    /// Gate rules (spec #167 story 8 / #165 Q8): a declaration is
    /// refused while any DMA is in flight or pending — the declaring
    /// back-edge then simply goes undeclared, and the loop's next
    /// back-edge re-tries.
    ///
    /// Anchor (spec #165 Q3): a declaration captures the dot of the
    /// declaring back-edge — the FIRST back-edge of that loop. Ticket
    /// #170 consumes this slot to start its quiet-span skip aligned on
    /// the anchor, and will own the re-arm lifecycle (declare → skip →
    /// wake → re-declare per loop instance).
    ///
    /// This runs regardless of [`Self::set_spin_skip_enabled`]: the
    /// switch gates only ticket #170's skip consumption, not detection
    /// or declaration. The #168 differential sweep runs both switch
    /// sides with detection active, proving the declaration itself is
    /// observationally invisible.
    fn declare_spin_loop(&mut self, hit: SpinBackedgeHit, clock: SystemClock) {
        // #165 Q8: never declare (nor keep) a loop while DMA owns the
        // bus — the declaration's anchor grid is only meaningful when
        // the CPU alone drives the loop. The refused back-edge is not
        // an anchor: a later clean back-edge of the same loop declares.
        if self.dma_bus.is_busy(self.bus.mcu()) {
            return;
        }
        // Keep-first (#165 Q3): every back-edge of an already-declared
        // loop belongs to that loop's first-back-edge grid — the anchor
        // never moves while the slot holds that loop, span armed or not.
        if let Some(decl) = self.spin_declaration
            && decl.loop_pc == hit.loop_pc
        {
            return;
        }
        // Ticket #171: transient arm refusals must not store. A stored
        // but unarmed declaration only ever happens for a permanent
        // reason (the page-crossing back-edge below); every transient
        // refusal — asserted `/NMI`, a dispatchable IRQ level, a
        // latched interrupt, the boundary window — declines the whole
        // declaration so the loop's next clean back-edge is free to
        // re-declare. Storing here would make the refusal immortal:
        // nothing but a span end clears the slot.
        let irq_level = self.bus.mcu().apu_irq_pending() || self.bus.mcu().cartridge_irq_pending();
        let irq_would_dispatch = irq_level && !self.cpu.flag(Flag::InterruptDisabled);
        let declaring_timing = self.bus.mcu().ppu().timing();
        let parks_through_vbl_boundary = declaring_backedge_parks_through_vbl_boundary(
            declaring_timing.scanline(),
            declaring_timing.dot(),
        );
        if self.bus.mcu().ppu().nmi_line()
            || irq_would_dispatch
            || self.cpu.has_latched_interrupt()
            || parks_through_vbl_boundary
        {
            return;
        }
        // A different loop replaces the slot: any span armed for the
        // old loop is stale — drop it before re-arming below.
        self.spin_skip = None;
        let declaration = SpinDeclaration::new(hit, clock);
        self.spin_declaration = Some(declaration);
        // Ticket #170: arm the quiet-span skip. The switch gates it;
        // a page-crossing taken branch takes 4 cycles and breaks the
        // 21-dot grid, so it stores the declaration but never arms —
        // the one permanent unarmability (#165 Q10 keeps declaring
        // regardless). Every transient refusal was handled before the
        // store above; the arm itself needs nothing further.
        if self.spin_skip_enabled && !backedge_crosses_page(hit.loop_pc) {
            self.spin_skip = Some(QuietSkip::new(declaration, false, irq_level));
        }
    }

    /// End the active quiet span. Every wake drops the declaration too:
    /// the loop's next taken back-edge re-declares with a fresh anchor, so
    /// each wait walks declare → skip → wake → re-declare (#165 Q3
    /// anchors, #167 story 9 lifecycle).
    fn end_quiet_span(&mut self) {
        self.spin_skip = None;
        self.spin_declaration = None;
    }

    /// Tickets #170/#171 quiet-span skip (spec #167 story 9): consume one
    /// CPU dot on behalf of the declared poll loop, or end the span.
    ///
    /// Returns `Some` when this dot was spent inside the skip — the CPU
    /// stayed parked, or a counterfactual `$2002` read was applied on its
    /// poll dot — and `None` when real execution must run this dot: the
    /// declaring iteration's tail, a wake dot, or no armed span at all.
    ///
    /// Ticket #171 edge rules. The span is quiet *until it isn't*, and
    /// every edge ends it at dot precision: a poll that reads the VBL
    /// flag (or whose poll-cycle interrupt poll latches) ends it at
    /// that poll dot with PC at the branch — the next dot decides the
    /// exit for real; a line event ends it at the next grid top, with
    /// the standard interrupt poll run at the wake when the timing
    /// table says the control's in-flight branch cycle already saw the
    /// edge (so the dispatch lands on the control's dot); a visible DMA
    /// ends it at the visibility dot. Every decision runs the published
    /// machinery on real state — no detector internals, no synthesized
    /// exit (ADR 0019's lesson), and every resume is an instruction
    /// boundary — the loop top or the branch PC — from the last
    /// completed real iteration, never a partial replay.
    ///
    /// Dot grid: the declaration anchors at the declaring back-edge
    /// (operand read, anchor+0); iteration 1's loop top is anchor+6 and
    /// each iteration is 21 dots (BIT/LDA 4 cycles + taken branch 3
    /// cycles), with the poll read at top+9. Counterfactual polls land
    /// exactly on the dots the real loop would have polled.
    fn tick_quiet_span(&mut self, clock: SystemClock) -> Option<ExecuteResult> {
        let mut skip = *self.spin_skip.as_ref()?;
        let dot = clock.cycles();
        if dot < skip.first_parked_dot() {
            // The declaring iteration's taken cycle (anchor+3) still runs
            // real; parking starts at iteration 1's loop top.
            return None;
        }
        // Ticket #171 DMA dot-precision: a DMC load request taken by the
        // dma tick on this very dot is visible here as busy only now
        // (the bus was `Idle` when the request was posted). End the span
        // at the first dot the DMA is visible — real execution owns the
        // stall and the resume (fallback rule: no partial replay, the
        // grid is never stretched by an unmodelled stall).
        if self.dma_bus.is_busy(self.bus.mcu()) {
            self.end_quiet_span();
            return None;
        }
        // A grid dot passed while DMA owned the bus — only when BOTH
        // the pending poll and the pending top are behind: the grid
        // advances at the top, so the poll position goes stale the dot
        // after it is applied while `next_top` still lies ahead. The
        // anchor grid is broken — resume real execution.
        if dot > skip.next_poll_dot() && dot > skip.next_top_dot() {
            self.end_quiet_span();
            return None;
        }
        if dot == skip.next_poll_dot() {
            // Replay one poll on its grid dot: the full published read
            // path (VBL clear, write-toggle reset, race logic, latch
            // refresh) via `Mcu::read`, then the CPU-visible effects
            // through the narrow `Cpu::apply_spin_poll_result` seam.
            let declaration = *skip.declaration();
            let value = self.bus.mcu_mut().read(POLL_ADDR);
            let taken = self.cpu.apply_spin_poll_result(
                declaration.poll_opcode,
                value,
                declaration.branch_test,
            );
            // Ticket #171: the poll read is the poll instruction's final
            // cycle, whose standard end-of-cycle interrupt poll the real
            // `Cpu::tick` would run right here — the replay owes it too.
            // Real detector state, live line levels: the control's
            // poll-cycle latch decision, dot for dot.
            self.cpu.detect_interrupt_at_wake(clock);
            self.spin_skip_replays += 1;
            #[cfg(test)]
            self.spin_skip_replay_clocks.push(dot);
            // Ticket #171: if the next parked iteration would cover the
            // frame boundary (the upcoming VBL set), end the span at
            // this poll — the resume runs the branch for real, so the
            // boundary dot and the flag-observing exit poll run real
            // on the control's own cycle grid, and the frame-boundary
            // snapshot compares real state on both switch sides. The
            // boundary case at distance 21 (the next poll IS the set
            // dot, the #168 race read) keeps parking: the next replay
            // is the control's own read on that exact dot.
            let timing = self.bus.mcu().ppu().timing();
            let vbl_boundary_end =
                next_parked_iteration_covers_vbl_set(timing.scanline(), timing.dot());
            if !taken || self.cpu.has_latched_interrupt() || vbl_boundary_end {
                // The exit poll: the branch would not be taken — or an
                // interrupt latched and real execution must dispatch at
                // the next boundary (preempting the branch fetch, exactly
                // the control's move). The read's side effects are
                // applied, so park the CPU at the branch and end the
                // span; the next CPU dot decides for real — no
                // synthesized exit (#165 Q5).
                self.cpu.set_pc(declaration.branch_pc);
                self.end_quiet_span();
            } else {
                self.spin_skip = Some(skip);
            }
            return Some(ExecuteResult::Continue);
        }
        if dot < skip.next_top_dot() {
            // Mid-iteration parked dot; a recorded line event waits for
            // the top decision below so the poll grid stays intact.
            self.spin_skip = Some(skip);
            return Some(ExecuteResult::Continue);
        }
        if dot > skip.next_top_dot() {
            // Oversoot (a grid dot consumed by DMA): the CPU is parked
            // at the loop top — an instruction boundary — so the real
            // fetch re-enters the loop exactly where the control does.
            self.end_quiet_span();
            return None;
        }
        // A grid loop-top dot: the ticket #171 wake decision for any
        // line event recorded since the span armed.
        match skip.wake_kind_at_top() {
            Some(with_poll) => {
                if with_poll {
                    // The control's in-flight branch cycle already saw
                    // the line move, so its dispatch begins at this top —
                    // deliver the standard interrupt poll here (real
                    // detector state, real lines) and the dispatch lands
                    // on the control's dot. When the poll latches
                    // nothing (a transient level, a short pulse the
                    // sampler missed), this degrades to the plain wake.
                    self.cpu.detect_interrupt_at_wake(clock);
                }
                // Plain wake: the CPU is parked at the loop top, so the
                // real fetch from this top re-enters the loop exactly
                // where the control is — a later line edge is first
                // seen by the next real poll, on the control's dot.
                self.end_quiet_span();
                None
            }
            None => {
                // The world stayed quiet: park the top and advance the
                // grid. A poll whose lattice dot falls inside the VBL
                // race window still replays there — a real read on its
                // exact dot, never moved (#168 pin).
                skip.advance_top();
                self.spin_skip = Some(skip);
                Some(ExecuteResult::Continue)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::EmptyPlugin;

    use super::*;

    /// Create a minimal valid iNES file for testing
    fn test_nes_file() -> INesFile {
        let mut rom = Vec::new();

        // NES signature
        rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a]);

        // PRG ROM pages (1 page = 16KB)
        rom.push(1);

        // CHR ROM pages (1 page = 8KB)
        rom.push(1);

        // Control byte 1: mapper 0, horizontal mirroring
        rom.push(0x00);

        // Control byte 2: mapper 0
        rom.push(0x00);

        // 8 bytes of padding
        rom.extend_from_slice(&[0; 8]);

        // PRG ROM data (16KB) - JMP $8000 infinite loop
        rom.extend_from_slice(&[0x4C, 0x00, 0x80]); // JMP $8000
        rom.extend(std::iter::repeat_n(0, 16 * 1024 - 3));

        // CHR ROM data (8KB)
        rom.extend(std::iter::repeat_n(0, 8 * 1024));

        INesFile::new(rom).unwrap()
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

    #[test]
    fn spin_skip_switch_defaults_off_and_round_trips() {
        // Spec #167, ticket #168: a machine-level test seam so differential
        // tests can run skip-on and skip-off builds from one binary.
        // Default OFF; until tickets #169-#171 land the skip itself, both
        // settings must run identical real execution.
        let file = test_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new());
        assert!(!machine.spin_skip_enabled());
        machine.set_spin_skip_enabled(true);
        assert!(machine.spin_skip_enabled());
        machine.set_spin_skip_enabled(false);
        assert!(!machine.spin_skip_enabled());
    }
}
