use crate::ines::INesFile;
use crate::mcu::Mcu;

const CARTRIDGE_START_ADDR: u16 = 0x4020;

mod mapper0;
mod mmc1;

pub fn create_cartridge(f: &INesFile) -> Box<dyn Cartridge> {
    match f.header().mapper_no {
        0 => Box::new(mapper0::Mapper0::new(f.read_prg_rom(), f.read_chr_rom())),
        1 => Box::new(mmc1::MMC1::new(f.read_prg_rom(), f.read_chr_rom())),
        _ => panic!("Unsupported cartridge mapper no: {}", f.header().mapper_no),
    }
}

pub trait Cartridge: Mcu {
    fn pattern_ref(&self) -> &[u8];
}
