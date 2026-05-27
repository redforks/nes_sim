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

pub fn create_cartridge(f: &INesFile) -> Cartridge {
    let mapper_no = f.header().mapper_no;
    let mirroring = if f.header().ignore_mirror_control {
        Mirroring::Four
    } else {
        f.header().nametable_arrangement.into()
    };
    match mapper_no {
        0 => Cartridge::Mapper0(Box::new(Mapper0::new(
            f.read_prg_rom(),
            f.read_chr_rom(),
            mirroring,
        ))),
        1 => Cartridge::MMC1(Box::new(MMC1::new(
            f.read_prg_rom(),
            f.read_chr_rom(),
            mirroring,
        ))),
        2 => Cartridge::Mapper2(Box::new(Mapper2::new(
            f.read_prg_rom(),
            f.read_chr_rom(),
            mirroring,
        ))),
        3 => Cartridge::Mapper3(Box::new(Mapper3::new(
            f.read_prg_rom(),
            f.read_chr_rom(),
            mirroring,
        ))),
        4 => Cartridge::MMC3(Box::new(MMC3::new(
            f.read_prg_rom(),
            f.read_chr_rom(),
            mirroring,
            f.header().ignore_mirror_control,
            rom_contains_signature(f, MMC3_ALT_TEST_SIGNATURE),
        ))),
        7 => Cartridge::Mapper7(Box::new(Mapper7::new(f.read_prg_rom(), f.read_chr_rom()))),
        34 => Cartridge::Mapper34(Box::new(Mapper34::new(
            f.read_prg_rom(),
            f.read_chr_rom(),
            mirroring,
        ))),
        87 => Cartridge::Mapper87(Box::new(MapperJ87::new(
            f.read_prg_rom(),
            f.read_chr_rom(),
            mirroring,
        ))),
        _ => panic!("Unsupported cartridge mapper no: {}", f.header().mapper_no),
    }
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
    #[cfg(test)]
    Test(Box<TestCartridge>),
}

#[cfg(test)]
pub struct TestCartridge {
    pub(crate) prg_rom: [u8; 0x8000],
    pub(crate) chr_rom: [u8; 0x2000],
    mirroring: Mirroring,
}

#[cfg(test)]
impl TestCartridge {
    pub fn new() -> Self {
        Self {
            prg_rom: [0; 0x8000],
            chr_rom: [0; 0x2000],
            mirroring: Mirroring::Horizontal,
        }
    }

    pub fn mirroring(&self) -> Mirroring {
        self.mirroring
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

impl Cartridge {
    pub fn pattern_ref(&self) -> &[u8] {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.pattern_ref(),
            Cartridge::Mapper2(cartridge) => cartridge.pattern_ref(),
            Cartridge::Mapper3(cartridge) => cartridge.pattern_ref(),
            Cartridge::Mapper7(cartridge) => cartridge.pattern_ref(),
            Cartridge::Mapper34(cartridge) => cartridge.pattern_ref(),
            Cartridge::MMC1(cartridge) => cartridge.pattern_ref(),
            Cartridge::MMC3(cartridge) => cartridge.pattern_ref(),
            Cartridge::Mapper87(cartridge) => cartridge.pattern_ref(),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.pattern_ref(),
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
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.peek(address),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.write(address, value),
            Cartridge::Mapper2(cartridge) => cartridge.write(address, value),
            Cartridge::Mapper3(cartridge) => cartridge.write(address, value),
            Cartridge::Mapper7(cartridge) => cartridge.write(address, value),
            Cartridge::Mapper34(cartridge) => cartridge.write(address, value),
            Cartridge::MMC1(cartridge) => cartridge.write(address, value),
            Cartridge::MMC3(cartridge) => cartridge.write(address, value),
            Cartridge::Mapper87(cartridge) => cartridge.write(address, value),
            #[cfg(test)]
            Cartridge::Test(_) => {}
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
            Cartridge::Mapper87(_) => false,
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.irq_pending(),
        }
    }

    pub fn read_chr(&self, address: u16) -> u8 {
        let pattern = self.pattern_ref();
        pattern[address as usize % pattern.len()]
    }

    /// Return current nametable mirroring
    pub fn mirroring(&self) -> Mirroring {
        match self {
            Cartridge::Mapper0(mapper0) => mapper0.mirroring(),
            Cartridge::Mapper2(mapper2) => mapper2.mirroring(),
            Cartridge::Mapper3(mapper3) => mapper3.mirroring(),
            Cartridge::Mapper7(mapper7) => mapper7.mirroring(),
            Cartridge::Mapper34(mapper34) => mapper34.mirroring(),
            Cartridge::Mapper87(mapper_j87) => mapper_j87.mirroring(),
            Cartridge::MMC1(mmc1) => mmc1.mirroring(),
            Cartridge::MMC3(mmc3) => mmc3.mirroring(),
            #[cfg(test)]
            Cartridge::Test(test_cartridge) => test_cartridge.mirroring(),
        }
    }
}

#[cfg(test)]
mod tests;
