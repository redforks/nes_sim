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

pub fn create_cartridge(f: &INesFile) -> (Cartridge, Mirroring) {
    let mapper_no = f.header().mapper_no;
    let mirroring = if f.header().ignore_mirror_control {
        Mirroring::Four
    } else {
        f.header().nametable_arrangement.into()
    };
    let cartridge = match mapper_no {
        0 => Cartridge::Mapper0(Box::new(Mapper0::new(f.read_prg_rom(), f.read_chr_rom()))),
        1 => Cartridge::MMC1(Box::new(MMC1::new(
            f.read_prg_rom(),
            f.read_chr_rom(),
            mirroring,
        ))),
        2 => Cartridge::Mapper2(Box::new(Mapper2::new(f.read_prg_rom(), f.read_chr_rom()))),
        3 => Cartridge::Mapper3(Box::new(Mapper3::new(f.read_prg_rom(), f.read_chr_rom()))),
        4 => Cartridge::MMC3(Box::new(MMC3::new(
            f.read_prg_rom(),
            f.read_chr_rom(),
            f.header().ignore_mirror_control,
            rom_contains_signature(f, MMC3_ALT_TEST_SIGNATURE),
        ))),
        7 => Cartridge::Mapper7(Box::new(Mapper7::new(f.read_prg_rom(), f.read_chr_rom()))),
        34 => Cartridge::Mapper34(Box::new(Mapper34::new(f.read_prg_rom(), f.read_chr_rom()))),
        21 => {
            let submapper = f.header().submapper_no.unwrap_or(1);
            let variant = match submapper {
                2 => VrcVariant::Vrc4c,
                _ => VrcVariant::Vrc4a, // default: submapper 1
            };
            Cartridge::Vrc24(Box::new(Vrc24::new(
                f.read_prg_rom(),
                f.read_chr_rom(),
                variant,
            )))
        }
        22 => Cartridge::Vrc24(Box::new(Vrc24::new(
            f.read_prg_rom(),
            f.read_chr_rom(),
            VrcVariant::Vrc2a,
        ))),
        23 => {
            let submapper = f.header().submapper_no.unwrap_or(1);
            let variant = match submapper {
                2 => VrcVariant::Vrc4e,
                3 => VrcVariant::Vrc2b,
                _ => VrcVariant::Vrc4f, // default: submapper 1
            };
            Cartridge::Vrc24(Box::new(Vrc24::new(
                f.read_prg_rom(),
                f.read_chr_rom(),
                variant,
            )))
        }
        25 => {
            let submapper = f.header().submapper_no.unwrap_or(1);
            let variant = match submapper {
                2 => VrcVariant::Vrc4d,
                3 => VrcVariant::Vrc2c,
                _ => VrcVariant::Vrc4b, // default: submapper 1
            };
            Cartridge::Vrc24(Box::new(Vrc24::new(
                f.read_prg_rom(),
                f.read_chr_rom(),
                variant,
            )))
        }
        87 => Cartridge::Mapper87(Box::new(MapperJ87::new(f.read_prg_rom(), f.read_chr_rom()))),
        _ => panic!("Unsupported cartridge mapper no: {}", f.header().mapper_no),
    };
    (cartridge, mirroring)
}

fn rom_contains_signature(file: &INesFile, signature: &str) -> bool {
    file.read_prg_rom()
        .windows(signature.len())
        .any(|window| window == signature.as_bytes())
}

pub enum Cartridge {
    Mapper0(Box<Mapper0>),
    Mapper2(Box<Mapper2>),
    Mapper3(Box<Mapper3>),
    Mapper7(Box<Mapper7>),
    Mapper34(Box<Mapper34>),
    Mapper87(Box<MapperJ87>),
    MMC1(Box<MMC1>),
    MMC3(Box<MMC3>),
    Vrc24(Box<Vrc24>),
    #[cfg(test)]
    Test(Box<TestCartridge>),
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

    pub fn read_pattern(&self, address: u16) -> u8 {
        self.chr_rom[address as usize % self.chr_rom.len()]
    }

    pub fn write_pattern(&mut self, address: u16, value: u8) {
        self.chr_rom[address as usize] = value;
    }

    pub fn write(&mut self, address: u16, value: u8) -> CartridgeOperation {
        let _ = (address, value);
        CartridgeOperation::None
    }

    pub fn read(&mut self, address: u16) -> u8 {
        if address >= 0x8000 {
            self.prg_rom[(address - 0x8000) as usize]
        } else {
            0
        }
    }

    pub fn peek(&self, address: u16) -> u8 {
        if address >= 0x8000 {
            self.prg_rom[(address - 0x8000) as usize]
        } else {
            0
        }
    }

    pub fn read_chr(&self, address: u16) -> u8 {
        self.chr_rom[address as usize % self.chr_rom.len()]
    }

    pub fn irq_pending(&self) -> bool {
        false
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CartridgeOperation {
    None,
    UpdateNametableMirroring(Mirroring),
}

impl Cartridge {
    pub fn read_pattern(&self, address: u16) -> u8 {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.read_pattern(address),
            Cartridge::Mapper2(cartridge) => cartridge.read_pattern(address),
            Cartridge::Mapper3(cartridge) => cartridge.read_pattern(address),
            Cartridge::Mapper7(cartridge) => cartridge.read_pattern(address),
            Cartridge::Mapper34(cartridge) => cartridge.read_pattern(address),
            Cartridge::MMC1(cartridge) => cartridge.read_pattern(address),
            Cartridge::MMC3(cartridge) => cartridge.read_pattern(address),
            Cartridge::Mapper87(cartridge) => cartridge.read_pattern(address),
            Cartridge::Vrc24(cartridge) => cartridge.read_pattern(address),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.read_pattern(address),
        }
    }

    pub fn write_pattern(&mut self, address: u16, value: u8) {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.write_pattern(address, value),
            Cartridge::Mapper2(cartridge) => cartridge.write_pattern(address, value),
            Cartridge::Mapper3(_) => {}
            Cartridge::MMC1(cartridge) => cartridge.write_pattern(address, value),
            Cartridge::Mapper7(cartridge) => cartridge.write_pattern(address, value),
            Cartridge::Mapper34(cartridge) => cartridge.write_pattern(address, value),
            Cartridge::MMC3(cartridge) => cartridge.write_pattern(address, value),
            Cartridge::Vrc24(cartridge) => cartridge.write_pattern(address, value),
            Cartridge::Mapper87(cartridge) => cartridge.write_pattern(address, value),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.write_pattern(address, value),
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.read(address),
            Cartridge::Mapper2(cartridge) => cartridge.read(address),
            Cartridge::Mapper3(cartridge) => cartridge.read(address),
            Cartridge::Mapper7(cartridge) => cartridge.read(address),
            Cartridge::Mapper34(cartridge) => cartridge.read(address),
            Cartridge::MMC1(cartridge) => cartridge.read(address),
            Cartridge::MMC3(cartridge) => cartridge.read(address),
            Cartridge::Mapper87(cartridge) => cartridge.read(address),
            Cartridge::Vrc24(cartridge) => cartridge.read(address),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.read(address),
        }
    }

    pub fn peek(&self, address: u16) -> u8 {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.peek(address),
            Cartridge::Mapper2(cartridge) => cartridge.peek(address),
            Cartridge::Mapper3(cartridge) => cartridge.peek(address),
            Cartridge::Mapper7(cartridge) => cartridge.peek(address),
            Cartridge::Mapper34(cartridge) => cartridge.peek(address),
            Cartridge::MMC1(cartridge) => cartridge.peek(address),
            Cartridge::MMC3(cartridge) => cartridge.peek(address),
            Cartridge::Mapper87(cartridge) => cartridge.peek(address),
            Cartridge::Vrc24(cartridge) => cartridge.peek(address),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.peek(address),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) -> CartridgeOperation {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.write(address, value),
            Cartridge::Mapper2(cartridge) => cartridge.write(address, value),
            Cartridge::Mapper3(cartridge) => cartridge.write(address, value),
            Cartridge::Mapper7(cartridge) => cartridge.write(address, value),
            Cartridge::Mapper34(cartridge) => cartridge.write(address, value),
            Cartridge::MMC1(cartridge) => cartridge.write(address, value),
            Cartridge::MMC3(cartridge) => cartridge.write(address, value),
            Cartridge::Mapper87(cartridge) => cartridge.write(address, value),
            Cartridge::Vrc24(cartridge) => cartridge.write(address, value),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.write(address, value),
        }
    }

    pub fn on_ppu_tick(&mut self, scanline: u16, dot: u16, rendering_enabled: bool) {
        match self {
            Cartridge::Mapper0(_)
            | Cartridge::Mapper2(_)
            | Cartridge::Mapper3(_)
            | Cartridge::Mapper7(_)
            | Cartridge::Mapper34(_)
            | Cartridge::MMC1(_) => {}
            Cartridge::MMC3(cartridge) => cartridge.on_ppu_tick(scanline, dot, rendering_enabled),
            Cartridge::Vrc24(cartridge) => cartridge.on_ppu_tick(scanline, dot, rendering_enabled),
            Cartridge::Mapper87(_) => {}
            #[cfg(test)]
            Cartridge::Test(_) => {}
        }
    }

    pub fn notify_vram_address(&mut self, addr: u16) {
        if let Cartridge::MMC3(cartridge) = self {
            cartridge.notify_vram_address(addr);
        }
    }

    pub fn irq_pending(&self) -> bool {
        match self {
            Cartridge::Mapper0(_)
            | Cartridge::Mapper2(_)
            | Cartridge::Mapper3(_)
            | Cartridge::Mapper7(_)
            | Cartridge::Mapper34(_)
            | Cartridge::MMC1(_) => false,
            Cartridge::MMC3(cartridge) => cartridge.irq_pending(),
            Cartridge::Vrc24(cartridge) => cartridge.irq_pending(),
            Cartridge::Mapper87(_) => false,
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.irq_pending(),
        }
    }
}

#[cfg(test)]
mod tests;
