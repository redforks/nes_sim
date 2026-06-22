use crate::ines::INesFile;
use crate::ines::NametableArrangement;
use j87::MapperJ87;
use mapper0::Mapper0;
use mapper2::Mapper2;
use mapper3::Mapper3;
use mapper7::Mapper7;
use mapper34::Mapper34;
use mmc1::MMC1;
use mmc3::MMC3;
use vrc24::Vrc24;
use vrc24::VrcVariant;

const CARTRIDGE_START_ADDR: u16 = 0x4020;
const MMC3_ALT_TEST_SIGNATURE: &str = "6-MMC3_alt";

mod j87;
mod mapper0;
mod mapper2;
mod mapper3;
mod mapper34;
mod mapper7;
mod mmc1;
mod mmc3;
mod vrc24;

pub mod chr_storage;

pub trait ChrStorage {
    fn read_chr(&self, address: u16) -> u8;
    fn write_chr(&mut self, address: u16, value: u8);
    fn write_register(&mut self, _addr: u16, _value: u8) {}
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Mirroring {
    LowerBank, // single screen use lower bank
    UpperBank, // single screen use upper bank
    Horizontal,
    Vertical,
    Four,
}

impl Mirroring {
    pub fn name_table_offset(self, addr: u16) -> u16 {
        match self {
            Mirroring::LowerBank => addr & 0x3ff,
            Mirroring::UpperBank => addr & 0x3ff | 0x400,
            Mirroring::Horizontal => {
                let bit = addr & 0x800;
                addr & (!0xfc00) | (bit >> 1)
            }
            Mirroring::Vertical => addr & (!0xf800),
            Mirroring::Four => addr & (!0xf000),
        }
    }
}

impl From<NametableArrangement> for Mirroring {
    fn from(value: NametableArrangement) -> Self {
        match value {
            // Vertical arrangement requires Horizontal mirrored
            NametableArrangement::Vertical => Self::Horizontal,
            NametableArrangement::Horizontal => Self::Vertical,
        }
    }
}

pub fn create_cartridge(f: &INesFile) -> (Box<dyn Cartridge>, Box<dyn ChrStorage>, Mirroring) {
    let mapper_no = f.header().mapper_no;
    let mirroring = if f.header().ignore_mirror_control {
        Mirroring::Four
    } else {
        f.header().nametable_arrangement.into()
    };
    let chr_rom = f.read_chr_rom();
    match mapper_no {
        0 => (
            Box::new(Mapper0::new(f.read_prg_rom())),
            Box::new(chr_storage::DirectChr::from_chr_rom(chr_rom)),
            mirroring,
        ),
        1 => (
            Box::new(MMC1::new(f.read_prg_rom(), chr_rom, mirroring)),
            Box::new(chr_storage::DirectChr::from_chr_rom(chr_rom)),
            mirroring,
        ),
        2 => (
            Box::new(Mapper2::new(f.read_prg_rom())),
            Box::new(chr_storage::DirectChr::from_chr_rom(chr_rom)),
            mirroring,
        ),
        3 => (
            Box::new(Mapper3::new(f.read_prg_rom())),
            Box::new(chr_storage::CnromChrStorage::new(chr_rom)),
            mirroring,
        ),
        4 => (
            Box::new(MMC3::new(
                f.read_prg_rom(),
                f.header().ignore_mirror_control,
                rom_contains_signature(f, MMC3_ALT_TEST_SIGNATURE),
            )),
            Box::new(chr_storage::Mmc3ChrStorage::new(chr_rom)),
            mirroring,
        ),
        7 => (
            Box::new(Mapper7::new(f.read_prg_rom())),
            Box::new(chr_storage::DirectChr::from_chr_rom(chr_rom)),
            mirroring,
        ),
        34 => (
            Box::new(Mapper34::new(f.read_prg_rom(), chr_rom)),
            Box::new(chr_storage::DirectChr::from_chr_rom(chr_rom)),
            mirroring,
        ),
        21 => {
            let submapper = f.header().submapper_no.unwrap_or(1);
            let variant = match submapper {
                2 => VrcVariant::Vrc4c,
                _ => VrcVariant::Vrc4a,
            };
            (
                Box::new(Vrc24::new(f.read_prg_rom(), chr_rom, variant)),
                Box::new(chr_storage::DirectChr::from_chr_rom(chr_rom)),
                mirroring,
            )
        }
        22 => (
            Box::new(Vrc24::new(f.read_prg_rom(), chr_rom, VrcVariant::Vrc2a)),
            Box::new(chr_storage::DirectChr::from_chr_rom(chr_rom)),
            mirroring,
        ),
        23 => {
            let submapper = f.header().submapper_no.unwrap_or(1);
            let variant = match submapper {
                2 => VrcVariant::Vrc4e,
                3 => VrcVariant::Vrc2b,
                _ => VrcVariant::Vrc4f,
            };
            (
                Box::new(Vrc24::new(f.read_prg_rom(), chr_rom, variant)),
                Box::new(chr_storage::DirectChr::from_chr_rom(chr_rom)),
                mirroring,
            )
        }
        25 => {
            let submapper = f.header().submapper_no.unwrap_or(1);
            let variant = match submapper {
                2 => VrcVariant::Vrc4d,
                3 => VrcVariant::Vrc2c,
                _ => VrcVariant::Vrc4b,
            };
            (
                Box::new(Vrc24::new(f.read_prg_rom(), chr_rom, variant)),
                Box::new(chr_storage::DirectChr::from_chr_rom(chr_rom)),
                mirroring,
            )
        }
        87 => (
            Box::new(MapperJ87::new(f.read_prg_rom(), chr_rom)),
            Box::new(chr_storage::DirectChr::from_chr_rom(chr_rom)),
            mirroring,
        ),
        _ => panic!("Unsupported cartridge mapper no: {}", f.header().mapper_no),
    }
}

fn rom_contains_signature(file: &INesFile, signature: &str) -> bool {
    file.read_prg_rom()
        .windows(signature.len())
        .any(|window| window == signature.as_bytes())
}

pub trait Cartridge {
    fn read(&mut self, address: u16) -> u8;
    fn peek(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, value: u8) -> CartridgeOperation;
    fn on_ppu_tick(&mut self, _scanline: u16, _dot: u16, _rendering_enabled: bool) {}
    fn notify_vram_address(&mut self, _addr: u16) {}
    fn irq_pending(&self) -> bool {
        false
    }
}

#[cfg(test)]
pub struct TestCartridge {
    pub(crate) prg_rom: [u8; 0x8000],
}

#[cfg(test)]
impl TestCartridge {
    pub fn new() -> Self {
        Self {
            prg_rom: [0; 0x8000],
        }
    }
}

#[cfg(test)]
impl Cartridge for TestCartridge {
    fn read(&mut self, address: u16) -> u8 {
        self.peek(address)
    }

    fn peek(&self, address: u16) -> u8 {
        if address >= 0x8000 {
            self.prg_rom[(address - 0x8000) as usize]
        } else {
            0
        }
    }

    fn write(&mut self, address: u16, value: u8) -> CartridgeOperation {
        let _ = (address, value);
        CartridgeOperation::None
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CartridgeOperation {
    None,
    UpdateNametableMirroring(Mirroring),
}



#[cfg(test)]
mod tests;
