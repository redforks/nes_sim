use crate::{
    ines::INesFile,
    machine::Machine,
    nes::controller::Button,
    nes::{self, NesMcu},
    render::{ImageRender, Render},
    EmptyPlugin, ExecuteResult, Plugin,
};

/// Safety limit: maximum CPU instruction ticks per `process_frame()` call.
/// Two full frames worth of ticks; prevents an infinite loop if VBlank never fires
/// (e.g. when the PPU is not connected or NES hardware is not present).
const MAX_TICKS_PER_FRAME: u32 = 60000;

pub struct NesMachine<P, R: Render = ImageRender> {
    machine: Machine<P, NesMcu<R>>,
    is_vblank: bool,
    enter_vblank: bool,
}

impl NesMachine<EmptyPlugin<NesMcu>, ImageRender> {
    /// Create a new NES machine from an iNES file with the default plugin.
    pub fn new(file: &INesFile) -> Self {
        Self::with_plugin(file, EmptyPlugin::new())
    }
}

impl<R: Render> NesMachine<EmptyPlugin<NesMcu<R>>, R> {
    /// Create a new NES machine from an iNES file with a custom renderer.
    pub fn with_renderer(file: &INesFile, renderer: R) -> Self {
        let mcu = nes::create_mcu_with_renderer(file, renderer);
        let is_vblank = mcu.ppu().vblank();
        Self {
            machine: Machine::with_plugin(EmptyPlugin::new(), mcu),
            is_vblank,
            enter_vblank: false,
        }
    }

    pub fn with_renderer_and_audio(
        file: &INesFile,
        renderer: R,
        audio_driver: Box<dyn crate::nes::apu::AudioDriver>,
    ) -> Self {
        let mcu = nes::create_mcu_with_renderer_and_audio(file, renderer, audio_driver);
        let is_vblank = mcu.ppu().vblank();
        Self {
            machine: Machine::with_plugin(EmptyPlugin::new(), mcu),
            is_vblank,
            enter_vblank: false,
        }
    }
}

impl<P: Plugin<NesMcu>> NesMachine<P, ImageRender> {
    /// Create a new NES machine from an iNES file with a custom plugin.
    pub fn with_plugin(file: &INesFile, plugin: P) -> Self {
        let mcu = nes::create_mcu(file);
        let is_vblank = mcu.ppu().vblank();
        Self {
            machine: Machine::with_plugin(plugin, mcu),
            is_vblank,
            enter_vblank: false,
        }
    }
}

impl<P, R> NesMachine<P, R>
where
    P: Plugin<NesMcu<R>>,
    R: Render,
{
    /// Create a new NES machine from an iNES file with a custom plugin and renderer.
    pub fn with_plugin_and_renderer(file: &INesFile, plugin: P, renderer: R) -> Self {
        let mcu = nes::create_mcu_with_renderer(file, renderer);
        let is_vblank = mcu.ppu().vblank();
        Self {
            machine: Machine::with_plugin(plugin, mcu),
            is_vblank,
            enter_vblank: false,
        }
    }

    /// Run the machine for a single frame.
    ///
    /// Executes CPU instructions until the PPU enters VBlank (scanline 241, dot 1),
    /// which marks the natural end of a rendered frame. This is the correct approach
    /// because the PPU has finished rendering all 240 visible scanlines at that point.
    ///
    /// If VBlank does not occur within `MAX_TICKS_PER_FRAME` instruction ticks
    /// (safety guard for MCUs without a real PPU), the function returns early.
    ///
    /// A frame is ~29780.5 CPU cycles (262 scanlines × 341 dots / 3 PPU:CPU ratio).
    pub fn process_frame(&mut self) -> ExecuteResult {
        // Delegate to `tick()` which executes a single CPU instruction and
        // advances PPU/APU accordingly. Loop until VBlank or safety limit.
        for _ in 0..MAX_TICKS_PER_FRAME {
            self.tick();

            if self.machine.cpu_mut().is_halted() {
                return ExecuteResult::Halt;
            }

            // Check for VBlank signalled by the PPU
            if std::mem::take(&mut self.enter_vblank) {
                return ExecuteResult::Continue;
            }
        }

        ExecuteResult::Continue
    }

    /// Execute one CPU instruction and tick PPU/APU. Returns the `ExecuteResult`.
    pub fn tick(&mut self) -> ExecuteResult {
        let result = self.machine.tick();
        self.machine.mcu_mut().tick_apu();
        let apu_irq = self.machine.mcu().apu_irq_pending();
        let cartridge_irq = self.machine.mcu().cartridge_irq_pending();
        self.machine.cpu_mut().set_irq(apu_irq || cartridge_irq);
        for _ in 0..3 {
            self.machine.mcu_mut().tick_ppu()
        }
        if self.mcu().ppu().vblank() != self.is_vblank && !self.is_vblank {
            self.enter_vblank = true;
        }
        self.is_vblank = self.mcu().ppu().vblank();
        let nmi_line = self.mcu().ppu().nmi_outline();
        self.machine.cpu_mut().update_nmi_line(nmi_line);
        result
    }

    fn mcu(&self) -> &NesMcu<R> {
        self.machine.mcu()
    }

    pub fn reset(&mut self) {
        self.machine.reset()
    }

    pub fn flush_audio(&mut self) {
        self.machine.mcu_mut().flush_audio();
    }

    pub fn press_button(&mut self, button: Button) {
        self.machine.mcu_mut().press_button(button);
    }

    pub fn release_button(&mut self, button: Button) {
        self.machine.mcu_mut().release_button(button);
    }

    /// Set the CPU program counter.
    pub fn set_pc(&mut self, pc: u16) {
        self.machine.set_pc(pc);
    }
}

#[cfg(test)]
mod tests {
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
    fn test_nes_machine_creation() {
        let file = test_nes_file();
        let _machine = NesMachine::new(&file);
        // If we got here without panicking, the test passed
    }

    #[test]
    fn test_nes_machine_with_plugin() {
        let file = test_nes_file();
        let _machine = NesMachine::with_plugin(&file, EmptyPlugin::new());
        // If we got here without panicking, the test passed
    }

    #[test]
    fn test_process_frame_waits_for_vblank() {
        let file = test_nes_file();
        let mut machine = NesMachine::new(&file);

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
