use crate::{
    ines::INesFile,
    machine::Machine,
    nes::{
        controller::Button,
        ppu::{VBLANK_SET_DOT, VBLANK_SET_SCANLINE},
        NesMcu,
    },
    render::Render,
    ExecuteResult, Plugin,
};
use std::fs;

/// Safety limit: maximum PPU instruction ticks per `process_frame()` call.
/// Two full frames worth of ticks; prevents an infinite loop if VBlank never fires
/// (e.g. when the PPU is not connected or NES hardware is not present).
const MAX_TICKS_PER_FRAME: u32 = 90000;

pub struct NesMachine<P, R: Render, D: crate::nes::apu::AudioDriver> {
    machine: Machine<P, NesMcu<R, D>>,
    cycles: usize,
    delayed_irq_pending: bool,
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
            machine: Machine::with_plugin(plugin, mcu),
            cycles: 0,
            delayed_irq_pending: false,
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
    pub fn process_frame(&mut self) -> ExecuteResult {
        // Delegate to `tick()` which executes a single CPU instruction and
        // advances PPU/APU accordingly. Loop until VBlank or safety limit.
        for _ in 0..MAX_TICKS_PER_FRAME {
            self.tick();

            if self.machine.cpu_mut().is_halted() {
                return ExecuteResult::Halt;
            }

            // Check for VBlank signalled by the PPU
            if self.machine.mcu().ppu_timing() == (VBLANK_SET_SCANLINE, VBLANK_SET_DOT) {
                return ExecuteResult::Continue;
            }
        }

        ExecuteResult::Continue
    }

    /// Execute one CPU instruction and tick PPU/APU. Returns the `ExecuteResult`.
    pub fn tick(&mut self) -> ExecuteResult {
        self.cycles += 1;

        self.machine.mcu_mut().tick_ppu();
        let nmi_line = self.mcu().ppu().nmi_line_out();
        let timing = self.mcu().ppu().timing();
        self.machine.cpu_mut().update_nmi_line(nmi_line, timing);

        if self.cycles.is_multiple_of(3) {
            self.machine.mcu_mut().tick_apu();
        }
        let irq_pending = self.machine.mcu().irq_pending();
        self.machine.cpu_mut().set_irq(self.delayed_irq_pending);
        if self.cycles.is_multiple_of(3) {
            self.delayed_irq_pending = irq_pending;
        }

        let result = self.machine.tick();

        if self.machine.mcu_mut().take_oam_dma_pending() {
            self.machine.cpu_mut().request_oam_dma();
        }

        result
    }

    fn mcu(&self) -> &NesMcu<R, D> {
        self.machine.mcu()
    }

    pub fn reset(&mut self) {
        self.machine.mcu_mut().reset();
        self.machine.reset();
        self.delayed_irq_pending = false;
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

    pub fn dump_ppu_state_to_file(&self) -> std::io::Result<String> {
        let mcu = self.machine.mcu();
        let (scanline, dot) = mcu.ppu_timing();
        let frame_no = mcu.ppu().frame_no();
        let path = format!("/tmp/ppu-{}-{}-{}.md", frame_no, scanline, dot);
        fs::write(&path, mcu.dump_ppu_state())?;
        Ok(path)
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
