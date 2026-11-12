use crate::{
    Cpu, EmptyPlugin, ExecuteResult, Plugin, SystemClock,
    bus::Bus,
    dma_bus::{BusOwner, DmaBus},
    ines::INesFile,
    interrupt::InterruptLines,
    nes::{Mmc3IrqOverride, NesMcu, controller::Button, ppu::palette::ColorTheme},
    render::Render,
};
pub struct NesMachine<P, R: Render, D: crate::nes::apu::AudioDriver> {
    cpu: Cpu,
    p: P,
    /// The CPU's external bus; owns the memory map `NesMcu` (ADR-0015).
    bus: Bus<NesMcu<R, D>>,
    /// DMA arbitration on the bus — renamed from the former `Bus` (ADR-0009).
    dma_bus: DmaBus,
    /// The loaded ROM, kept so [`Self::hard_reset`] can rebuild the
    /// cartridge (mapper state never survives a power cycle on hardware)
    /// without the caller re-supplying it.
    file: INesFile,
    /// MMC3 IRQ-revision override from construction, re-applied on hard reset.
    irq_override: Mmc3IrqOverride,
}

impl<P, R, D> NesMachine<P, R, D>
where
    P: Plugin<NesMcu<R, D>>,
    R: Render,
    D: crate::nes::apu::AudioDriver,
{
    pub fn new(file: &INesFile, plugin: P, render: R, audio_driver: D) -> Self {
        Self::new_with_mmc3_irq_override(file, plugin, render, audio_driver, Mmc3IrqOverride::Auto)
    }

    /// [`Self::new`] with an explicit MMC3 IRQ-revision override — the
    /// harness/test hook (step 1) of [`mmc3_irq_revision_is_alternate`]:
    /// [`Mmc3IrqOverride::ForceAlternate`] forces revision-A (Alternate)
    /// semantics, [`Mmc3IrqOverride::ForceStandard`] forces Standard, and
    /// [`Mmc3IrqOverride::Auto`] uses [`NesMcu::new`] auto-detection.
    pub fn new_with_mmc3_irq_override(
        file: &INesFile,
        plugin: P,
        render: R,
        audio_driver: D,
        irq_override: Mmc3IrqOverride,
    ) -> Self {
        let mcu = NesMcu::new_with_mmc3_irq_override(file, render, audio_driver, irq_override);
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
    #[hotpath::measure(impl_type = "NesMachine")]
    pub fn process_frame(&mut self) -> ExecuteResult {
        crate::hotpath_cpu::hotpath_cpu_alias!(
            Self,
            concat!(module_path!(), "::NesMachine::process_frame"),
            "process_frame"
        );
        loop {
            self.tick();
            if self.bus.mcu_mut().ppu_mut().take_vblank_entered() {
                break;
            }
        }

        self.flush_audio();
        ExecuteResult::Continue
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
        self.bus.mcu_mut().tick_ppu();
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
        // The level is OR-ed before building the single `InterruptLines` bundle.
        let irq_level = self.bus.mcu().apu_irq_pending() || self.bus.mcu().cartridge_irq_pending();

        let clock = self.bus.clock();
        self.bus.mcu_mut().tick_apu(clock);
        let owner = self.dma_bus.tick(&mut self.cpu, &mut self.bus);
        if !matches!(owner, BusOwner::Idle) {
            return ExecuteResult::Continue;
        }

        let nmi = self.bus.mcu_mut().ppu_mut().nmi_line();
        let lines = InterruptLines { nmi, irq_level };
        self.cpu.update_interrupt_lines(lines, clock, false);
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

    fn flush_audio(&mut self) {
        self.bus.mcu_mut().flush_audio();
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

    pub fn render_mut(&mut self) -> &mut R {
        self.bus.mcu_mut().ppu_mut().renderer_mut()
    }

    pub fn set_color_theme(&mut self, theme: ColorTheme) {
        self.bus.mcu_mut().ppu_mut().set_color_theme(theme);
    }

    /// Drain to instruction boundary with full device interleaving.
    ///
    /// Advances the bus's master clock and calls `self.tick()` per dot (PPU →
    /// cartridge IRQ → APU → DMA → NMI → CPU), satisfying GLOSSARY.md
    /// *Microcode* invariant. Each CPU microcode still consumes one CPU
    /// cycle (3 dots) because `tick()` only drives the CPU on
    /// `is_cpu_clock()` dots — unlike the CPU-only drain
    /// (`Cpu::run_to_instruction_boundary`) which consumes one dot per
    /// microcode for setup-time speed.
    pub fn run_to_instruction_boundary(&mut self) {
        while !self.cpu.microcodes_empty() {
            self.tick();
        }
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

    pub fn cpu_mut(&mut self) -> &mut Cpu {
        &mut self.cpu
    }

    pub fn mcu(&self) -> &NesMcu<R, D> {
        self.bus.mcu()
    }

    pub fn mcu_mut(&mut self) -> &mut NesMcu<R, D> {
        self.bus.mcu_mut()
    }

    pub fn system_cycles(&self) -> u64 {
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
    fn test_process_frame_waits_for_vblank() {
        let file = test_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new(), (), ());

        // process_frame should return as soon as the PPU reaches VBlank
        // (scanline 241, dot 1 = after 241*341 + 1 = 82,262 PPU dots = ~27,421 CPU cycles).
        // The call must complete within one frame (~89k dots).
        let result = machine.process_frame();
        assert_eq!(result, ExecuteResult::Continue);

        // After one frame the PPU should be at or past scanline 241.
        // We verify by running a second frame — it should also complete without hitting
        // the safety limit, proving VBlank fires consistently every frame.
        let result = machine.process_frame();
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
        let mut machine = NesMachine::new(&file, EmptyPlugin::new(), (), ());
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
        let mut machine = NesMachine::new(&file, EmptyPlugin::new(), (), ());
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
        let mut machine = NesMachine::new(&file, EmptyPlugin::new(), (), ());
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
        let mut machine = NesMachine::new(&file, EmptyPlugin::new(), (), ());
        assert_eq!(machine.process_frame(), ExecuteResult::Continue);
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
        assert_eq!(machine.process_frame(), ExecuteResult::Continue);
        assert_eq!(machine.frame_no(), 1);
    }

    #[test]
    fn hard_reset_preserves_held_buttons_but_rewinds_shift() {
        use crate::mcu::Mcu;

        let file = test_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new(), (), ());
        machine.press_controller_a(Button::A);

        machine.hard_reset();

        // The pad is physical: the held button still reports pressed on the
        // first shift-register read, with no strobe written since power-up.
        assert_eq!(machine.mcu().peek(0x4016) & 0x01, 0x01);
    }

    #[test]
    fn reset_drains_in_flight_oam_dma_before_applying() {
        let file = dma_loop_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new(), (), ());

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
        assert_eq!(machine.process_frame(), ExecuteResult::Continue);
    }
}
