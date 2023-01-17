use crate::ines::INesFile;
use crate::mcu::{DefinedRegion, RamMcu, Region};

mod mmc1;

pub fn create_cartridge(f: &INesFile) -> Vec<Region> {
    match f.header().mapper_no {
        0 => mapper000(f).collect(),
        1 => vec![Region::with_defined(mmc1::MMC1::new(f.read_prg_rom()))],
        _ => panic!("Unsupported cartridge mapper no: {}", f.header().mapper_no),
    }
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

trait Cartridge {}

impl<C> DefinedRegion for C
where
    C: Cartridge,
{
    fn region(&self) -> (u16, u16) {
        (0x4020, 0xffff)
    }
}
