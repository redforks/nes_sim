use crate::ines::INesFile;
use crate::mcu::Mcu;

mod mapper0;
mod mmc1;

pub fn create_cartridge(f: &INesFile) -> Box<dyn Cartridge> {
    match f.header().mapper_no {
        0 => Box::new(mapper0::Mapper0 {}),
        1 => Box::new(mmc1::MMC1::new(f.read_prg_rom())),
        _ => panic!("Unsupported cartridge mapper no: {}", f.header().mapper_no),
    }
}

pub trait Cartridge: Mcu {
    fn pattern_ref(&self) -> &[u8];
    fn pattern_mut(&mut self) -> &mut [u8];
}
