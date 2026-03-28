use log::info;

use crate::ines::INesFile;
use crate::nes::ppu::Ppu;

const CARTRIDGE_START_ADDR: u16 = 0x4020;

mod mapper0;
mod mapper2;
mod mmc1;
mod mmc3;

pub fn create_cartridge(f: &INesFile) -> Box<dyn Cartridge> {
    let mapper_no = f.header().mapper_no;
    info!("Creating cartridge with mapper no: {}", mapper_no);
    match mapper_no {
        0 => Box::new(mapper0::Mapper0::new(f.read_prg_rom(), f.read_chr_rom())),
        1 => Box::new(mmc1::MMC1::new(f.read_prg_rom(), f.read_chr_rom())),
        2 => Box::new(mapper2::Mapper2::new(f.read_prg_rom(), f.read_chr_rom())),
        4 => Box::new(mmc3::MMC3::new(
            f.read_prg_rom(),
            f.read_chr_rom(),
            f.header().ignore_mirror_control,
        )),
        _ => panic!("Unsupported cartridge mapper no: {}", f.header().mapper_no),
    }
}

pub trait Cartridge {
    fn pattern_ref(&self) -> &[u8];
    fn write_pattern(&mut self, _address: u16, _value: u8) {}
    fn read(&mut self, address: u16) -> u8;
    fn write(&mut self, ppu: &mut Ppu, address: u16, value: u8);
    fn on_ppu_tick(&mut self, _scanline: u16, _dot: u16, _rendering_enabled: bool) {}
    fn irq_pending(&self) -> bool {
        false
    }
}

#[cfg(test)]
mod tests;
