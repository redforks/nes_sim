use crate::ines::INesFile;
use crate::mcu::{Mcu, RamMcu};
use crate::nes::apu::{Apu, AudioDriver, NullAudioDriver};
use crate::nes::controller::{Button, Controller};
use crate::nes::lower_ram::LowerRam;
use crate::nes::mapper;
use crate::nes::mapper::Cartridge;
use crate::nes::ppu::{Mirroring, Ppu};
use crate::render::Render;
use log::trace;

pub struct NesMcu {
    lower_ram: LowerRam,
    ppu: Ppu,
    after_ppu: RamMcu<0x20>,
    controller: Controller,
    cartridge: Cartridge,
    apu: Apu,
}

pub fn build(file: &INesFile) -> NesMcu {
    build_with_renderer(file, None)
}

/// Build a NesMcu with a custom renderer
///
/// # Parameters
/// - `file`: The iNES file to load
/// - `renderer`: Optional custom renderer. If None, uses default ImageRender.
pub fn build_with_renderer(file: &INesFile, renderer: Option<Box<dyn Render>>) -> NesMcu {
    build_with_renderer_and_audio(file, renderer, Box::new(NullAudioDriver))
}

pub fn build_with_renderer_and_audio(
    file: &INesFile,
    renderer: Option<Box<dyn Render>>,
    audio_driver: Box<dyn AudioDriver>,
) -> NesMcu {
    let cartridge = mapper::create_cartridge(file);
    let mut ppu = Ppu::new();
    ppu.set_mirroring(if file.header().ignore_mirror_control {
        Mirroring::Four
    } else if file.header().ver_or_hor_arrangement {
        Mirroring::Vertical
    } else {
        Mirroring::Horizontal
    });

    // Set custom renderer if provided
    if let Some(r) = renderer {
        ppu.set_renderer(r);
    }

    NesMcu {
        lower_ram: LowerRam::new(),
        ppu,
        after_ppu: RamMcu::start_from(0x4000, [0; 0x20]),
        controller: Controller::new(),
        cartridge,
        apu: Apu::new(audio_driver),
    }
}

impl NesMcu {
    fn ppu_dma(&mut self, address: u8) {
        trace!("ppu dma");
        let addr = (address as u16) << 8;
        let mut buf = [0x00u8; 0x100];
        for (i, item) in buf.iter_mut().enumerate() {
            *item = self.read(addr + i as u16);
        }
        self.ppu.oam_dma(&buf);
    }

    /// Tick PPU by one dot.
    /// Pattern data is passed through from the cartridge for rendering.
    pub fn tick_ppu(&mut self) {
        let (scanline, dot) = self.ppu.timing();
        let rendering_enabled = self.ppu.rendering_enabled();
        {
            let pattern = self.cartridge.pattern_ref();
            self.ppu.tick(pattern);
        }
        self.cartridge.on_ppu_tick(scanline, dot, rendering_enabled);
    }

    /// Tick APU frame counter. Returns true if frame IRQ should be triggered.
    pub fn tick_apu(&mut self) -> bool {
        self.apu.tick()
    }

    pub fn apu_irq_pending(&self) -> bool {
        self.apu.request_irq()
    }

    pub fn cartridge_irq_pending(&self) -> bool {
        self.cartridge.irq_pending()
    }

    pub fn flush_audio(&mut self) {
        self.apu.flush();
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

    pub fn ppu(&self) -> &Ppu {
        &self.ppu
    }
}

impl Mcu for NesMcu {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.lower_ram.read(address),
            0x2000..=0x3fff => self.ppu.read(self.cartridge.pattern_ref(), address),
            0x4015 => self.apu.read(address),
            0x4016..=0x4017 => self.controller.read(address),
            0x4000..=0x401f => self.after_ppu.read(address),
            0x4020..=0xffff => self.cartridge.read(address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1fff => self.lower_ram.write(address, value),
            0x2000..=0x3fff => {
                let cartridge = &mut self.cartridge;
                self.ppu
                    .write_with_pattern_write(address, value, |pattern_addr, pattern_value| {
                        cartridge.write_pattern(pattern_addr, pattern_value)
                    });
            }
            0x4014 => self.ppu_dma(value),
            0x4016 => {
                self.after_ppu.write(address, value);
                self.controller.write(address, value);
            }
            0x4000..=0x4017 => {
                self.after_ppu.write(address, value);
                if matches!(
                    address,
                    0x4000..=0x400F | 0x4010..=0x4013 | 0x4015 | 0x4017
                ) {
                    self.apu.write(address, value);
                }
            }
            0x4018..=0x401f => self.after_ppu.write(address, value),
            0x4020..=0xffff => {
                // Cartridge now takes &mut self and &mut Ppu
                self.cartridge.write(&mut self.ppu, address, value)
            }
        }
    }
}

#[cfg(test)]
mod tests;
