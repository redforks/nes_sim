use crate::ines::INesFile;
use crate::mcu::{RamMcu, Region};

pub fn create_cartridge(f: &INesFile) -> impl Iterator<Item = Region> {
    match f.header().mapper_no {
        0 => mapper000(f),
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
