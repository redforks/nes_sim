use crate::ines::INesFile;
use crate::mcu::{Mcu, RamMcu};
use crate::nes::apu::{
    ApuController, FakeApuControllerDriver, LengthCounterChannel, LengthCounterLoad,
};
use crate::nes::lower_ram::LowerRam;
use crate::nes::mapper;
use crate::nes::mapper::Cartridge;
use crate::nes::ppu::{Mirroring, Ppu};
use crate::render::Render;
use log::info;

pub struct NesMcu {
    lower_ram: LowerRam,
    ppu: Ppu,
    after_ppu: RamMcu<0x20>,
    cartridge: Box<dyn Cartridge>,
    apu: ApuController<FakeApuControllerDriver>,
    vblank_started: bool,
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
    let cartridge = mapper::create_cartridge(file);
    let mut ppu = Ppu::default();
    ppu.set_mirroring(if file.header().ver_or_hor_arrangement {
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
        cartridge,
        apu: ApuController::new(FakeApuControllerDriver::default()),
        vblank_started: false,
    }
}

impl NesMcu {
    fn ppu_dma(&mut self, address: u8) {
        info!("ppu dma");
        let addr = (address as u16) << 8;
        let mut buf = [0x00u8; 0x100];
        for (i, item) in buf.iter_mut().enumerate() {
            *item = self.read(addr + i as u16);
        }
        self.ppu.oam_dma(&buf);
    }

    pub fn take_vblank(&mut self) -> bool {
        std::mem::take(&mut self.vblank_started)
    }

    /// Tick PPU by one dot. Returns true if NMI should be triggered.
    /// Pattern data is passed through from the cartridge for rendering.
    pub fn tick_ppu(&mut self) -> bool {
        let rv = self.ppu.tick(self.cartridge.pattern_ref());
        self.vblank_started = rv.vblank_started;
        rv.nmi_requested
    }

    /// Tick APU frame counter. Returns true if frame IRQ should be triggered.
    pub fn tick_apu(&mut self) -> bool {
        self.apu.tick()
    }

    pub fn ppu_timing(&self) -> (u16, u16) {
        self.ppu.timing()
    }
}

impl Mcu for NesMcu {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.lower_ram.read(address),
            0x2000..=0x3fff => self.ppu.read(self.cartridge.pattern_ref(), address),
            0x4015 => self.apu.read(address),
            0x4000..=0x401f => self.after_ppu.read(address),
            0x4020..=0xffff => self.cartridge.read(address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1fff => self.lower_ram.write(address, value),
            0x2000..=0x3fff => {
                self.ppu.write(address, value);
            }
            0x4014 => self.ppu_dma(value),
            0x4015 | 0x4017 => self.apu.write(address, value),
            // Length counter load registers (high byte)
            0x4003 => {
                self.after_ppu.write(address, value);
                let low = self.after_ppu.read(0x4002);
                self.apu.set_length_counter_load(
                    LengthCounterChannel::Pulse1,
                    LengthCounterLoad::from_registers(low, value),
                );
            }
            0x4007 => {
                self.after_ppu.write(address, value);
                let low = self.after_ppu.read(0x4006);
                self.apu.set_length_counter_load(
                    LengthCounterChannel::Pulse2,
                    LengthCounterLoad::from_registers(low, value),
                );
            }
            0x400B => {
                self.after_ppu.write(address, value);
                let low = self.after_ppu.read(0x400A);
                self.apu.set_length_counter_load(
                    LengthCounterChannel::Triangle,
                    LengthCounterLoad::from_registers(low, value),
                );
            }
            0x400F => {
                self.after_ppu.write(address, value);
                self.apu.set_length_counter_load(
                    LengthCounterChannel::Noise,
                    LengthCounterLoad::from_registers(0, value),
                );
            }
            // Halt flag registers (low byte with length counter halt bit)
            0x4000 | 0x4004 | 0x4008 | 0x400C => {
                self.after_ppu.write(address, value);
                let channel = match address {
                    0x4000 => LengthCounterChannel::Pulse1,
                    0x4004 => LengthCounterChannel::Pulse2,
                    0x4008 => LengthCounterChannel::Triangle,
                    0x400C => LengthCounterChannel::Noise,
                    _ => unreachable!(),
                };
                self.apu.set_length_counter_halt(channel, value & 0x20 != 0);
            }
            0x4000..=0x401f => self.after_ppu.write(address, value),
            0x4020..=0xffff => {
                // Cartridge now takes &mut self and &mut Ppu
                self.cartridge.write(&mut self.ppu, address, value)
            }
        }
    }
}

#[cfg(test)]
mod tests;
