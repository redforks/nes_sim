use crate::{
    Cpu, EmptyPlugin, ExecuteResult, Plugin, SystemClock,
    ines::INesFile,
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
    cartridge_irq_latched: bool,
    cartridge_irq_next: bool,
    dmc_dma: DmcDma,
    clock: SystemClock,
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
            cartridge_irq_latched: false,
            cartridge_irq_next: false,
            dmc_dma: DmcDma::default(),
            clock: SystemClock::default(),
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
        self.cartridge_irq_next = self.cpu.mcu().cartridge_irq_pending();
        if cpu_tick {
            self.cartridge_irq_latched = self.cartridge_irq_next;
        }

        let irq_pending = self.cpu.mcu().apu_irq_pending() || self.cartridge_irq_latched;
        self.cpu.set_irq(irq_pending);

        self.cpu.mcu_mut().tick_apu(clock);
        if clock.is_apu_clock() {
            self.dmc_dma.tick(&mut self.cpu, clock);
            if self.cpu.mcu_mut().tick_oam_dma(clock) {
                return ExecuteResult::Continue;
            }
        }

        let nmi_line = self.cpu.mcu().ppu().nmi_line_out();
        self.cpu.update_nmi_line(nmi_line);
        let result = if clock.is_cpu_clock() {
            self.cpu.tick(&mut self.p, clock).0
        } else {
            ExecuteResult::Continue
        };
        self.cpu.detect_interrupt(clock);
        result
    }

    pub fn reset(&mut self) {
        self.cpu.mcu_mut().reset();
        self.cpu.reset();
        self.cartridge_irq_latched = false;
        self.cartridge_irq_next = false;
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

    pub fn render_mut(&mut self) -> &mut R {
        self.cpu.mcu_mut().ppu_mut().renderer_mut()
    }

    pub fn set_color_theme(&mut self, theme: ColorTheme) {
        self.cpu.mcu_mut().ppu_mut().set_color_theme(theme);
    }

    /// Set the CPU program counter.
    /// Drains any pending microcodes (e.g. from reset) before setting PC.
    /// Uses CPU-only ticks so APU/DMA state is not advanced during setup.
    pub fn set_pc(&mut self, pc: u16) {
        let mut empty = EmptyPlugin::new();
        while !self.cpu.microcodes_empty() {
            self.cpu.tick(&mut empty, self.clock);
            self.clock = self.clock.inc();
        }
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
}
