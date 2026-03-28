use log::info;

use crate::ines::INesFile;
use crate::nes::mapper::mapper0::Mapper0;
use crate::nes::mapper::mapper2::Mapper2;
use crate::nes::mapper::mmc1::MMC1;
use crate::nes::mapper::mmc3::MMC3;
use crate::nes::ppu::Ppu;
use crate::render::Render;

const CARTRIDGE_START_ADDR: u16 = 0x4020;

mod mapper0;
mod mapper2;
mod mmc1;
mod mmc3;

pub fn create_cartridge(f: &INesFile) -> Cartridge {
    let mapper_no = f.header().mapper_no;
    info!("Creating cartridge with mapper no: {}", mapper_no);
    match mapper_no {
        0 => Cartridge::Mapper0(Box::new(Mapper0::new(f.read_prg_rom(), f.read_chr_rom()))),
        1 => Cartridge::MMC1(Box::new(MMC1::new(f.read_prg_rom(), f.read_chr_rom()))),
        2 => Cartridge::Mapper2(Box::new(Mapper2::new(f.read_prg_rom(), f.read_chr_rom()))),
        4 => Cartridge::MMC3(Box::new(MMC3::new(
            f.read_prg_rom(),
            f.read_chr_rom(),
            f.header().ignore_mirror_control,
        ))),
        _ => panic!("Unsupported cartridge mapper no: {}", f.header().mapper_no),
    }
}

pub enum Cartridge {
    Mapper0(Box<Mapper0>),
    Mapper2(Box<Mapper2>),
    MMC1(Box<MMC1>),
    MMC3(Box<MMC3>),
    #[cfg(test)]
    Test(TestCartridge),
}

#[cfg(test)]
pub struct TestCartridge {
    pub(crate) prg_rom: [u8; 0x8000],
    pub(crate) chr_rom: [u8; 0x2000],
}

#[cfg(test)]
impl TestCartridge {
    pub fn new() -> Self {
        Self {
            prg_rom: [0; 0x8000],
            chr_rom: [0; 0x2000],
        }
    }

    pub fn pattern_ref(&self) -> &[u8] {
        &self.chr_rom
    }

    pub fn write_pattern(&mut self, address: u16, value: u8) {
        self.chr_rom[address as usize] = value;
    }

    pub fn read(&mut self, address: u16) -> u8 {
        if address >= 0x8000 {
            self.prg_rom[(address - 0x8000) as usize]
        } else {
            0
        }
    }

    pub fn write<R: Render>(&mut self, _ppu: &mut Ppu<R>, _address: u16, _value: u8) {}

    pub fn on_ppu_tick(&mut self, _scanline: u16, _dot: u16, _rendering_enabled: bool) {}

    pub fn irq_pending(&self) -> bool {
        false
    }
}

impl Cartridge {
    pub fn pattern_ref(&self) -> &[u8] {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.pattern_ref(),
            Cartridge::Mapper2(cartridge) => cartridge.pattern_ref(),
            Cartridge::MMC1(cartridge) => cartridge.pattern_ref(),
            Cartridge::MMC3(cartridge) => cartridge.pattern_ref(),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.pattern_ref(),
        }
    }

    pub fn write_pattern(&mut self, address: u16, value: u8) {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.write_pattern(address, value),
            Cartridge::Mapper2(cartridge) => cartridge.write_pattern(address, value),
            Cartridge::MMC1(cartridge) => cartridge.write_pattern(address, value),
            Cartridge::MMC3(cartridge) => cartridge.write_pattern(address, value),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.write_pattern(address, value),
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.read(address),
            Cartridge::Mapper2(cartridge) => cartridge.read(address),
            Cartridge::MMC1(cartridge) => cartridge.read(address),
            Cartridge::MMC3(cartridge) => cartridge.read(address),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.read(address),
        }
    }

    pub fn write<R: Render>(&mut self, ppu: &mut Ppu<R>, address: u16, value: u8) {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.write(ppu, address, value),
            Cartridge::Mapper2(cartridge) => cartridge.write(ppu, address, value),
            Cartridge::MMC1(cartridge) => cartridge.write(ppu, address, value),
            Cartridge::MMC3(cartridge) => cartridge.write(ppu, address, value),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.write(ppu, address, value),
        }
    }

    pub fn on_ppu_tick(&mut self, scanline: u16, dot: u16, rendering_enabled: bool) {
        match self {
            Cartridge::Mapper0(cartridge) => {
                cartridge.on_ppu_tick(scanline, dot, rendering_enabled)
            }
            Cartridge::Mapper2(cartridge) => {
                cartridge.on_ppu_tick(scanline, dot, rendering_enabled)
            }
            Cartridge::MMC1(cartridge) => cartridge.on_ppu_tick(scanline, dot, rendering_enabled),
            Cartridge::MMC3(cartridge) => cartridge.on_ppu_tick(scanline, dot, rendering_enabled),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.on_ppu_tick(scanline, dot, rendering_enabled),
        }
    }

    pub fn irq_pending(&self) -> bool {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.irq_pending(),
            Cartridge::Mapper2(cartridge) => cartridge.irq_pending(),
            Cartridge::MMC1(cartridge) => cartridge.irq_pending(),
            Cartridge::MMC3(cartridge) => cartridge.irq_pending(),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.irq_pending(),
        }
    }
}

#[cfg(test)]
mod tests;
