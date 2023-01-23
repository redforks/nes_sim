use crate::ines::INesFile;
use crate::mcu::{DefinedRegion, Mcu, RamMcu, Region};

mod mapper0;
mod mmc1;

pub fn create_cartridge(f: &INesFile) -> Box<dyn Cartridge> {
    match f.header().mapper_no {
        0 => Box::new(mapper0::Mapper0 {}),
        1 => Box::new(mmc1::MMC1::new(f.read_prg_rom())),
        _ => panic!("Unsupported cartridge mapper no: {}", f.header().mapper_no),
    }
}

pub fn create_ppu_pattern(f: &INesFile) -> impl Mcu + AsRef<[u8]> {
    let chr = f.read_chr_rom();
    let mut buf = [0; 0x2000];
    if !chr.is_empty() {
        // copy chr to buf
        assert!(chr.len() <= 0x2000);
        buf[..chr.len()].copy_from_slice(chr);
    }
    RamMcu::start_from(0x0, buf)
}

fn mapper000(f: &INesFile) -> impl Iterator<Item = Region> {
    let prg_rom = f.read_prg_rom();
    assert_eq!(prg_rom.len(), 32 * 1024);
    let mut arr = [0; 32 * 1024];
    arr[..prg_rom.len()].copy_from_slice(prg_rom);
    let prg_rom = RamMcu::start_from(0x8000, arr);

    let ram1 = RamMcu::start_from(0x4020, [0; 0x3fe0]);
    [Region::with_defined(ram1), Region::with_defined(prg_rom)].into_iter()
}

pub trait Cartridge: Mcu {
    fn pattern_ref(&self) -> &[u8];
    fn pattern_mut(&mut self) -> &mut [u8];
}

impl<C> DefinedRegion for C
where
    C: Cartridge,
{
    fn region(&self) -> (u16, u16) {
        (0x4020, 0xffff)
    }
}
