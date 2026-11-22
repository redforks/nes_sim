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
    /// Returns at the save point (ADR 0022): the end of the first CPU
    /// cycle at or after scanline 261, so the boundary PPU dot is 0, 1,
    /// or 2 (one CPU cycle is three PPU dots). The check reads the
    /// existing `Timing` scanline accessor in two phases — phase 1 ticks
    /// until the scanline leaves 261 (cold start begins there, as does a
    /// restored state), phase 2 ticks until it returns to 261 — with no
    /// new PPU flag. The vblank-entry event fired mid-frame (scanline 241)
    /// is cleared at the boundary instead of breaking the loop: a save
    /// taken here observes it unset. `take_vblank_entered` keeps the
    /// VBL-entry break for tick-stepped capture (`tick_frames`).
    ///
    /// Returns the frame status plus borrows of the owned PPU framebuffer
    /// and APU audio buffer for the completed frame. Frame bytes are
    /// unchanged by the boundary move: the palette flush still runs at
    /// the VBL set dot, while each call's audio covers about 20 more
    /// scanlines than the old VBL-entry break.
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
        // Phase 1: cold start (and a restored state) begins at scanline
        // 261, so tick until the scanline leaves it.
        while matches!(status, ExecuteResult::Continue)
            && self.bus.mcu().ppu().timing().scanline() == 261
        {
            status = self.tick();
        }
        // Phase 2: tick until the scanline returns to 261, then return.
        // The stop lands at the end of a CPU cycle, so the dot is 0-2.
        while matches!(status, ExecuteResult::Continue)
            && self.bus.mcu().ppu().timing().scanline() != 261
        {
            status = self.tick();
        }
        // The VBL-entry event fired mid-frame; clear it at the boundary.
        let _ = self.bus.mcu_mut().ppu_mut().take_vblank_entered();

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
    /// The tick-stepped loop is aligned to the same scanline-261 boundary
    /// `process_frame` returns at (ADR 0022), so both styles cover the
    /// identical emulated span; `tick_frames` itself keeps the VBL-entry
    /// break and is not used here.
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
        for _ in 0..FRAMES {
            while ticked.mcu().ppu().timing().scanline() == 261 {
                ticked.tick();
            }
            while ticked.mcu().ppu().timing().scanline() != 261 {
                ticked.tick();
            }
        }
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
    fn process_frame_stops_at_scanline_261_boundary() {
        let file = test_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new());

        // Save point (ADR 0022): each call returns at the end of the
        // first CPU cycle at or after scanline 261 (PPU dot 0-2).
        for _ in 0..3 {
            let result = machine.process_frame().status;
            assert_eq!(result, ExecuteResult::Continue);
            assert_eq!(
                machine.mcu().ppu().timing().scanline(),
                261,
                "process_frame must stop at scanline 261"
            );
            assert!(
                (0..=2).contains(&machine.mcu().ppu().timing().dot()),
                "boundary dot must be 0-2, got {}",
                machine.mcu().ppu().timing().dot()
            );
        }
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
