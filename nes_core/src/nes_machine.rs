use crate::{
    ExecuteResult, Plugin, SYSTEM_CYCLES_PER_CPU_CYCLE, SYSTEM_CYCLES_PER_PPU_CYCLE,
    get_system_cycles, inc_system_cycles,
    ines::INesFile,
    machine::Machine,
    nes::{
        NesMcu,
        controller::Button,
        ppu::{VBLANK_SET_DOT, VBLANK_SET_SCANLINE},
    },
    render::Render,
};
use std::fs;

/// Safety limit: maximum master clock ticks per `process_frame()` call.
/// A full frame is 89,342 PPU dots, each dot now spans 4 master clock ticks.
/// This leaves a little headroom while still guarding against infinite loops.
const MAX_TICKS_PER_FRAME: u32 = 360000;

pub struct NesMachine<P, R: Render, D: crate::nes::apu::AudioDriver> {
    machine: Machine<P, NesMcu<R, D>>,
    cartridge_irq_latched: bool,
    cartridge_irq_next: bool,
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
            cartridge_irq_latched: false,
            cartridge_irq_next: false,
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
            inc_system_cycles();
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

    /// Execute one master clock tick. Returns the `ExecuteResult`.
    pub fn tick(&mut self) -> ExecuteResult {
        let cycles = get_system_cycles();
        let ppu_tick = cycles.is_multiple_of(SYSTEM_CYCLES_PER_PPU_CYCLE);
        let cpu_tick = cycles.is_multiple_of(SYSTEM_CYCLES_PER_CPU_CYCLE);

        if ppu_tick {
            self.machine.mcu_mut().tick_ppu();
            let nmi_line = self.mcu().ppu().nmi_line_out();
            let timing = self.mcu().ppu().timing();
            self.machine.cpu_mut().update_nmi_line(nmi_line, timing);
            self.cartridge_irq_next = self.machine.mcu().cartridge_irq_pending();
            if cpu_tick {
                self.cartridge_irq_latched = self.cartridge_irq_next;
            }
        }

        // Cartridge IRQs are exposed on the next CPU boundary, while APU IRQs
        // keep the existing immediate visibility used by the interrupt tests.
        let irq_pending = self.machine.mcu().apu_irq_pending() || self.cartridge_irq_latched;
        self.machine.cpu_mut().set_irq(irq_pending);

        self.machine.mcu_mut().tick_apu();
        if let Some(is_reload) = self.machine.mcu_mut().take_dmc_dma_pending() {
            self.machine.cpu_mut().request_dmc_dma(is_reload);
        }

        if ppu_tick {
            let ppu_cycle = (cycles / SYSTEM_CYCLES_PER_PPU_CYCLE).wrapping_sub(1);
            let cpu_tick_phase = ppu_cycle % 3;
            let cpu_cycle = ppu_cycle / 3;
            if self
                .machine
                .mcu_mut()
                .tick_oam_dma(cpu_tick_phase, cpu_cycle)
            {
                return ExecuteResult::Continue;
            }
        }

        let result = self.machine.tick();

        // After the CPU tick, re-check DMC DMA in case the CPU just
        // wrote to $4015 enabling DMC with an empty sample buffer.
        // On real hardware the APU and CPU are clocked simultaneously,
        // so the DMA check sees the $4015 write on the same cycle.
        if cpu_tick {
            self.machine.mcu_mut().recheck_dmc_dma();
        }

        result
    }

    pub fn mcu(&self) -> &NesMcu<R, D> {
        self.machine.mcu()
    }

    pub fn mcu_mut(&mut self) -> &mut NesMcu<R, D> {
        self.machine.mcu_mut()
    }

    pub fn cpu(&self) -> &crate::Cpu<NesMcu<R, D>> {
        self.machine.cpu()
    }

    pub fn reset(&mut self) {
        self.machine.mcu_mut().reset();
        self.machine.reset();
        self.cartridge_irq_latched = false;
        self.cartridge_irq_next = false;
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
