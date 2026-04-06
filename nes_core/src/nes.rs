use crate::ines::INesFile;
use crate::mcu::Mcu;
use crate::nes::apu::{Apu, AudioDriver};
use crate::nes::controller::{Button, Controller};
use crate::nes::lower_ram::LowerRam;
use crate::nes::mapper::Cartridge;
use crate::nes::ppu::Ppu;
use crate::render::Render;
use log::trace;

pub mod apu;
pub mod controller;
mod lower_ram;
mod mapper;
pub mod ppu;

pub struct NesMcu<R: Render, D: AudioDriver> {
    lower_ram: LowerRam,
    ppu: Ppu<R>,
    controller: Controller,
    cartridge: Cartridge,
    apu: Apu<D>,
    oam_dma_pending: bool,
}

impl<R: Render, D: AudioDriver> NesMcu<R, D> {
    pub fn new(file: &INesFile, renderer: R, audio_driver: D) -> Self {
        let cartridge = mapper::create_cartridge(file);
        let ppu = Ppu::new(renderer);

        Self {
            lower_ram: LowerRam::new(),
            ppu,
            controller: Controller::new(),
            cartridge,
            apu: Apu::new(audio_driver),
            oam_dma_pending: false,
        }
    }

    pub fn reset(&mut self) {
        self.ppu.reset();
        self.apu.reset();
    }

    fn ppu_dma(&mut self, address: u8) {
        trace!("ppu dma");
        let addr = (address as u16) << 8;
        let mut buf = [0x00u8; 0x100];
        for (i, item) in buf.iter_mut().enumerate() {
            *item = self.read(addr + i as u16);
        }
        self.ppu.oam_dma(&buf);
        self.cartridge.on_oam_dma();
        self.oam_dma_pending = true;
    }

    /// Tick PPU by one dot.
    /// Pattern data is passed through from the cartridge for rendering.
    pub fn tick_ppu(&mut self) {
        let (scanline, dot) = self.ppu.timing();
        let rendering_enabled = self.ppu.rendering_enabled();
        self.ppu.tick(&self.cartridge);
        self.cartridge.on_ppu_tick(scanline, dot, rendering_enabled);
    }

    /// Tick APU frame counter. Returns true if frame IRQ should be triggered.
    pub fn tick_apu(&mut self) -> bool {
        self.apu.tick()
    }

    pub fn irq_pending(&self) -> bool {
        self.apu.request_irq() || self.cartridge.irq_pending()
    }

    pub fn flush_audio(&mut self) {
        self.apu.flush();
    }

    pub fn take_oam_dma_pending(&mut self) -> bool {
        std::mem::take(&mut self.oam_dma_pending)
    }

    pub fn press_button(&mut self, button: Button) {
        self.controller.a.press(button);
    }

    pub fn release_button(&mut self, button: Button) {
        self.controller.a.release(button);
    }

    pub fn ppu_timing(&self) -> (u16, u16) {
        self.ppu.timing()
    }

    pub fn ppu(&self) -> &Ppu<R> {
        &self.ppu
    }

    pub fn read_nametable(&self, address: u16) -> u8 {
        self.cartridge.read_nametable(address)
    }

    pub fn dump_ppu_state(&self) -> String {
        self.ppu.dump_state(&self.cartridge)
    }
}

impl<R: Render, D: AudioDriver> Mcu for NesMcu<R, D> {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.lower_ram.read(address),
            0x2000..=0x3fff => self.ppu.read(address, &mut self.cartridge),
            0x4000..=0x401f => {
                if address == 0x4016 || address == 0x4017 {
                    self.controller.read(address)
                } else {
                    self.apu.read(address)
                }
            }
            0x4020..=0xffff => self.cartridge.read(address),
        }
    }

    fn peek(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.lower_ram.peek(address),
            0x2000..=0x3fff => self.ppu.peek(address, &self.cartridge),
            0x4000..=0x401f => {
                if address == 0x4016 || address == 0x4017 {
                    self.controller.peek(address)
                } else {
                    self.apu.peek(address)
                }
            }
            0x4020..=0xffff => self.cartridge.peek(address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1fff => self.lower_ram.write(address, value),
            0x2000..=0x3fff => self.ppu.write(address, value, &mut self.cartridge),
            0x4000..=0x401f => match address {
                0x4014 => self.ppu_dma(value),
                0x4016 => self.controller.write(address, value),
                _ => self.apu.write(address, value),
            },
            0x4020..=0xffff => self.cartridge.write(address, value),
        }
    }
}

#[cfg(test)]
mod tests;
