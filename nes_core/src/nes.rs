use crate::SystemClock;
use crate::ines::INesFile;
use crate::mcu::Mcu;
use crate::nes::apu::{Apu, AudioDriver};
use crate::nes::controller::{Button, Controller};
use crate::nes::lower_ram::LowerRam;
use crate::nes::ppu::{Ppu, Timing};
use crate::render::Render;

pub mod apu;
pub mod controller;
pub(crate) mod dmc_dma;
mod lower_ram;
mod mapper;
pub mod ppu;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct OamDmaState {
    page: u8,
    startup_cycles: usize,
    transfer_cycle: usize,
    latch: u8,
}

pub struct NesMcu<R: Render, D: AudioDriver> {
    lower_ram: LowerRam,
    ppu: Ppu<R>,
    controller: Controller,
    apu: Apu<D>,
    oam_dma_pending: Option<u8>,
    oam_dma: Option<OamDmaState>,
    /// CPU data bus open bus value: the last value read by the CPU.
    /// Reading write-only or unmapped addresses returns this value.
    open_bus: u8,
}

impl<R: Render, D: AudioDriver> NesMcu<R, D> {
    pub fn new(file: &INesFile, renderer: R, audio_driver: D) -> Self {
        let (cartridge, mirroring) = mapper::create_cartridge(file);
        let ppu = Ppu::new(renderer, mirroring, cartridge);

        Self {
            lower_ram: LowerRam::new(),
            ppu,
            controller: Controller::new(),
            apu: Apu::new(audio_driver),
            oam_dma_pending: None,
            oam_dma: None,
            open_bus: 0,
        }
    }

    pub fn reset(&mut self) {
        self.ppu.reset();
        self.apu.reset();
    }

    fn ppu_dma(&mut self, address: u8) {
        self.oam_dma_pending = Some(address);
    }

    pub fn tick_ppu(&mut self) {
        self.ppu.tick();
    }

    pub fn tick_apu(&mut self, clock: SystemClock) {
        self.apu.tick(clock);
    }

    pub fn apu_irq_pending(&self) -> bool {
        self.apu.request_irq()
    }

    pub fn cartridge_irq_pending(&self) -> bool {
        self.ppu.cartridge_irq_pending()
    }

    pub fn flush_audio(&mut self) {
        self.apu.flush();
    }

    pub fn tick_oam_dma(&mut self, clock: SystemClock) -> bool {
        if let Some(mut dma) = self.oam_dma.take() {
            if dma.startup_cycles > 0 {
                dma.startup_cycles -= 1;
            } else {
                let byte_index = dma.transfer_cycle / 2;
                if dma.transfer_cycle.is_multiple_of(2) {
                    let addr = ((dma.page as u16) << 8) | byte_index as u16;
                    dma.latch = self.read(addr);
                } else {
                    self.ppu.write_oam_data(dma.latch);
                }
                dma.transfer_cycle += 1;
            }

            if dma.startup_cycles == 0 && dma.transfer_cycle == 512 {
                return true;
            }

            self.oam_dma = Some(dma);
            return true;
        }

        if let Some(page) = self.oam_dma_pending {
            self.oam_dma_pending = None;
            self.oam_dma = Some(OamDmaState {
                page,
                startup_cycles: if clock.is_even_cpu_cycle() { 1 } else { 2 },
                transfer_cycle: 0,
                latch: 0,
            });

            return true;
        }

        false
    }

    pub fn press_controller_a(&mut self, button: Button) {
        self.controller.a.press(button);
    }

    pub fn release_controller_a(&mut self, button: Button) {
        self.controller.a.release(button);
    }

    pub fn press_controller_b(&mut self, button: Button) {
        self.controller.b.press(button);
    }

    pub fn release_controller_b(&mut self, button: Button) {
        self.controller.b.release(button);
    }

    pub fn ppu_timing(&self) -> &Timing {
        self.ppu.timing()
    }

    pub fn ppu(&self) -> &Ppu<R> {
        &self.ppu
    }

    pub fn ppu_mut(&mut self) -> &mut Ppu<R> {
        &mut self.ppu
    }

    pub fn apu(&self) -> &Apu<D> {
        &self.apu
    }

    pub fn apu_mut(&mut self) -> &mut Apu<D> {
        &mut self.apu
    }

    pub fn read_vram(&self, addr: u16) -> u8 {
        self.ppu().read_vram(addr)
    }
}

impl<R: Render, D: AudioDriver> Mcu for NesMcu<R, D> {
    fn read(&mut self, address: u16) -> u8 {
        let value = match address {
            0x0000..=0x1fff => self.lower_ram.read(address),
            0x2000..=0x3fff => self.ppu.read(address),
            0x4015 => self.apu.read(address),
            0x4016 | 0x4017 => self.controller.read(address),
            // Write-only APU/IO registers and unused test registers: open bus
            0x4000..=0x401f => self.open_bus,
            // Unallocated I/O space: open bus
            0x4020..=0x40ff => self.open_bus,
            0x4100..=0xffff => self.ppu.read(address),
        };
        self.open_bus = value;
        value
    }

    fn peek(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.lower_ram.peek(address),
            0x2000..=0x3fff => self.ppu.peek(address),
            0x4015 => self.apu.peek(address),
            0x4016 | 0x4017 => self.controller.peek(address),
            0x4000..=0x401f => self.open_bus,
            0x4020..=0x40ff => self.open_bus,
            0x4100..=0xffff => self.ppu.peek(address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        self.open_bus = value;
        match address {
            0x0000..=0x1fff => self.lower_ram.write(address, value),
            0x2000..=0x3fff => self.ppu.write(address, value),
            0x4000..=0x401f => match address {
                0x4014 => self.ppu_dma(value),
                0x4016 => self.controller.write(address, value),
                _ => self.apu.write(address, value),
            },
            // Unallocated I/O space: writes are ignored
            0x4020..=0x40ff => {}
            0x4100..=0xffff => {
                self.ppu.write(address, value);
            }
        }
    }
}

#[cfg(test)]
mod tests;
