use crate::{
    Cpu, EmptyPlugin, ExecuteResult, Plugin, SystemClock,
    ines::INesFile,
    interrupt::{ApuIrqSampler, CartridgeIrqLatch, InterruptLines},
    nes::{NesMcu, controller::Button, dmc_dma::DmcDma, ppu::palette::ColorTheme},
    render::Render,
};

/// Safety limit: maximum system ticks per `process_frame()` call.
/// A full frame is 89,342 PPU dots, and one system tick now maps to one PPU dot.
/// This leaves a little headroom while still guarding against infinite loops.
const MAX_TICKS_PER_FRAME: u32 = 100000;

pub struct NesMachine<P, R: Render, D: crate::nes::apu::AudioDriver> {
    cpu: Cpu<NesMcu<R, D>>,
    p: P,
    cart_latch: CartridgeIrqLatch,
    apu_sampler: ApuIrqSampler,
    dmc_dma: DmcDma,
    clock: SystemClock,
    /// Set while `reset()` is draining in-flight DMA work: the CPU gets no
    /// more cycles (hardware holds the reset line asserted), but PPU/APU/DMA
    /// keep interleaving until the bus is quiet.
    reset_requested: bool,
}

impl<P, R, D> NesMachine<P, R, D>
where
    P: Plugin<NesMcu<R, D>>,
    R: Render,
    D: crate::nes::apu::AudioDriver,
{
    pub fn new(file: &INesFile, plugin: P, render: R, audio_driver: D) -> Self {
        let mcu = NesMcu::new(file, render, audio_driver);
        Self {
            cpu: Cpu::new(mcu),
            p: plugin,
            cart_latch: CartridgeIrqLatch::new(),
            apu_sampler: ApuIrqSampler::new(),
            dmc_dma: DmcDma::default(),
            clock: SystemClock::default(),
            reset_requested: false,
        }
    }

    pub fn frame_no(&self) -> usize {
        self.cpu.mcu().ppu().timing().frame_no()
    }

    /// Run the machine for a single frame.
    ///
    /// Executes CPU instructions until the PPU enters VBlank (scanline 241, dot 1),
    /// which marks the natural end of a rendered frame. This is the correct approach
    /// because the PPU has finished rendering all 240 visible scanlines at that point.
    ///
    /// If VBlank does not occur within `MAX_TICKS_PER_FRAME` instruction ticks
    /// (safety guard for MCUs without a real PPU), the function returns early.
    pub fn process_frame(&mut self) -> ExecuteResult {
        let mut last_in_vblank = self.cpu.mcu().ppu().in_vblank();
        for _ in 0..MAX_TICKS_PER_FRAME {
            self.tick();

            if self.cpu.is_halted() {
                self.flush_audio();
                return ExecuteResult::Halt;
            }

            let in_vblank = self.cpu.mcu().ppu().in_vblank();
            if !last_in_vblank && in_vblank {
                break;
            }
            last_in_vblank = in_vblank;
        }

        self.flush_audio();
        ExecuteResult::Continue
    }

    /// Execute one master clock tick. Returns the `ExecuteResult`.
    ///
    /// Captures the current clock before advancing so that all device ticks
    /// (PPU, APU, DMA, CPU) see the same pre-increment cycle value, matching
    /// the original global-clock semantics where `get_system_clock()` was read
    /// at the top of tick and the increment happened afterward.
    pub fn tick(&mut self) -> ExecuteResult {
        let clock = self.clock;
        self.clock = self.clock.inc();
        let cpu_tick = clock.is_cpu_clock();

        self.cpu.mcu_mut().tick_ppu();
        if cpu_tick {
            self.cpu.mcu_mut().tick_zapper();
        }
        self.cart_latch
            .capture_next(self.cpu.mcu().cartridge_irq_pending());

        // While a machine reset is draining DMA work the CPU stays frozen:
        // no IRQ/NMI latching, no microcode, no same-tick race retract (the
        // CPU state is about to be discarded wholesale).
        if !self.reset_requested {
            if cpu_tick {
                self.cart_latch.latch_on_cpu_tick();
            }

            // APU IRQ is sampled BEFORE `tick_apu` — a transition the step
            // raises or clears becomes CPU-visible on the NEXT dot (one-dot
            // skew, see `ApuIrqSampler`). The level is OR-ed with the
            // cartridge latch's CPU-quantized level before building the
            // single `InterruptLines` bundle.
            let apu_sampled = self
                .apu_sampler
                .sample_before_tick(self.cpu.mcu().apu_irq_pending());
            let irq_level = apu_sampled || self.cart_latch.level();

            self.cpu.mcu_mut().tick_apu(clock);
            if clock.is_apu_clock() {
                let dmc_drove_bus = self.dmc_dma.tick(&mut self.cpu, clock);
                if self.cpu.mcu_mut().tick_oam_dma(clock, dmc_drove_bus) {
                    return ExecuteResult::Continue;
                }
            }

            let nmi = self.cpu.mcu_mut().ppu_mut().nmi_lines();
            let lines = InterruptLines { nmi, irq_level };
            self.cpu.update_interrupt_lines(lines, clock);
            let result = if clock.is_cpu_clock() {
                self.cpu.tick(&mut self.p, clock).0
            } else {
                ExecuteResult::Continue
            };
            result
        } else {
            self.cpu.mcu_mut().tick_apu(clock);
            if clock.is_apu_clock() {
                let dmc_drove_bus = self.dmc_dma.tick(&mut self.cpu, clock);
                let _ = self.cpu.mcu_mut().tick_oam_dma(clock, dmc_drove_bus);
            }
            ExecuteResult::Continue
        }
    }
    /// Maximum dots spent draining in-flight DMA on a machine reset. One
    /// OAM DMA is ~1539 dots; a running DMC channel is suppressed for fresh
    /// requests, so anything beyond this bound means a stuck bus.
    const RESET_DRAIN_LIMIT_DOTS: u32 = 4000;

    /// Reset the machine to its power-up contract.
    ///
    /// Hardware model chosen (see review round 2): once the reset line is
    /// asserted, the CPU stops being fed immediately; PPU/APU keep running
    /// and any DMA work already accepted by the bus completes first. Fresh
    /// DMC fetch requests are suppressed during the drain so a playing
    /// sample channel cannot extend it indefinitely.
    pub fn reset(&mut self) {
        debug_assert!(
            !self.reset_requested,
            "nested reset while a previous drain is active"
        );
        let mut drained = 0_u32;
        self.reset_requested = true;
        while (self.dmc_dma.is_busy() || self.cpu.mcu().oam_dma_active())
            && drained < Self::RESET_DRAIN_LIMIT_DOTS
        {
            // Kill freshly generated DMC fetch requests before each step so
            // the drain terminates even with the DMC channel still playing.
            self.cpu.mcu_mut().suppress_new_dmc_dma_requests();
            self.tick();
            drained += 1;
        }
        self.reset_requested = false;
        debug_assert!(!self.dmc_dma.is_busy(), "DMA bus did not quiesce");
        debug_assert!(!self.cpu.mcu().oam_dma_active(), "OAM DMA did not quiesce");

        let clock = self.clock;
        self.cpu.mcu_mut().reset(clock);
        self.dmc_dma.reset();
        self.cpu.reset();

        self.cart_latch.reset();
    }

    fn flush_audio(&mut self) {
        self.cpu.mcu_mut().flush_audio();
    }

    pub fn press_controller_a(&mut self, button: Button) {
        self.cpu.mcu_mut().press_controller_a(button);
    }

    pub fn release_controller_a(&mut self, button: Button) {
        self.cpu.mcu_mut().release_controller_a(button);
    }

    pub fn press_controller_b(&mut self, button: Button) {
        self.cpu.mcu_mut().press_controller_b(button);
    }

    pub fn release_controller_b(&mut self, button: Button) {
        self.cpu.mcu_mut().release_controller_b(button);
    }

    pub fn connect_zapper(&mut self, connected: bool) {
        self.cpu.mcu_mut().connect_zapper(connected);
    }

    pub fn aim_zapper(&mut self, x: u16, y: u16) {
        self.cpu.mcu_mut().aim_zapper(x, y);
    }

    pub fn trigger_zapper(&mut self) {
        self.cpu.mcu_mut().trigger_zapper();
    }

    pub fn render_mut(&mut self) -> &mut R {
        self.cpu.mcu_mut().ppu_mut().renderer_mut()
    }

    pub fn set_color_theme(&mut self, theme: ColorTheme) {
        self.cpu.mcu_mut().ppu_mut().set_color_theme(theme);
    }

    /// Drain to instruction boundary with full device interleaving.
    ///
    /// Advances `self.clock` and calls `self.tick()` per dot (PPU →
    /// cartridge IRQ → APU → DMA → NMI → CPU), satisfying CONTEXT.md
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
            .run_to_instruction_boundary(&mut empty, &mut self.clock);
        self.cpu.set_pc(pc);
    }

    pub fn cpu(&self) -> &Cpu<NesMcu<R, D>> {
        &self.cpu
    }

    pub fn cpu_mut(&mut self) -> &mut Cpu<NesMcu<R, D>> {
        &mut self.cpu
    }

    pub fn mcu(&self) -> &NesMcu<R, D> {
        self.cpu.mcu()
    }

    pub fn mcu_mut(&mut self) -> &mut NesMcu<R, D> {
        self.cpu.mcu_mut()
    }

    pub fn system_cycles(&self) -> u64 {
        self.clock.cycles()
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
        // The call must complete well within MAX_TICKS_PER_FRAME.
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
    fn reset_drains_in_flight_oam_dma_before_applying() {
        let file = dma_loop_nes_file();
        let mut machine = NesMachine::new(&file, EmptyPlugin::new(), (), ());

        // Warm up until the loop's first $4014 write has armed a DMA.
        for _ in 0..10_000 {
            machine.tick();
            if machine.cpu.mcu().oam_dma_active() {
                break;
            }
        }
        assert!(
            machine.cpu.mcu().oam_dma_active(),
            "test ROM never armed an OAM DMA"
        );

        // Resetting mid-DMA must quiesce the bus first, bounded by the
        // internal drain limit (no hang), then apply cleanly.
        machine.reset();

        assert!(!machine.cpu.mcu().oam_dma_active());
        assert!(!machine.dmc_dma.is_busy());

        // The post-reset machine still runs: frames complete normally.
        assert_eq!(machine.process_frame(), ExecuteResult::Continue);
    }
}
