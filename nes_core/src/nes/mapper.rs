use crate::ines::INesFile;
use crate::ines::NametableArrangement;
use crate::nes::mapper::mapper0::Mapper0;
use crate::nes::mapper::mapper2::Mapper2;
use crate::nes::mapper::mapper3::Mapper3;
use crate::nes::mapper::mmc1::MMC1;
use crate::nes::mapper::mmc3::MMC3;
use crate::nes::ppu::BackgroundTileOverride;
use crate::nes::ppu::PatternAccess;

const CARTRIDGE_START_ADDR: u16 = 0x4020;
const MMC3_ALT_TEST_SIGNATURE: &str = "6-MMC3_alt";

mod mapper0;
mod mapper2;
mod mapper3;
mod mmc1;
mod mmc3;
mod mmc5;

use crate::nes::mapper::mmc5::MMC5;

const NAME_TABLE_MEM_START: u16 = 0x2000;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Mirroring {
    LowerBank, // single screen use lower bank
    UpperBank, // single screen use upper bank
    Horizontal,
    Vertical,
    Four,
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

pub(crate) struct NameTableControl {
    mem: [u8; 4096],
    mirroring: Mirroring,
}

impl NameTableControl {
    pub(crate) fn new(mirroring: Mirroring) -> Self {
        Self {
            mem: [0; 4096],
            mirroring,
        }
    }

    pub(crate) fn set_mirroring(&mut self, mirroring: Mirroring) {
        self.mirroring = mirroring;
    }

    fn offset(&self, addr: u16) -> usize {
        let offset = addr - NAME_TABLE_MEM_START;
        let page = (offset / 1024) as usize;
        let offset_in_page = (offset % 1024) as usize;
        let page_start = match self.mirroring {
            Mirroring::LowerBank => [0, 0, 0, 0][page],
            Mirroring::UpperBank => [1024, 1024, 1024, 1024][page],
            Mirroring::Vertical => [0, 1024, 0, 1024][page],
            Mirroring::Horizontal => [0, 0, 1024, 1024][page],
            Mirroring::Four => page * 1024,
        };
        page_start + offset_in_page
    }

    pub(crate) fn read(&self, address: u16) -> u8 {
        self.mem[self.offset(address)]
    }

    pub(crate) fn write(&mut self, address: u16, value: u8) {
        self.mem[self.offset(address)] = value;
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
        5 => Cartridge::MMC5(Box::new(MMC5::new(
            f.read_prg_rom(),
            f.read_chr_rom(),
            f.header().prg_ram_size.or(f.header().prg_nvram_size),
            f.header().ignore_mirror_control,
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
    MMC1(Box<MMC1>),
    MMC3(Box<MMC3>),
    MMC5(Box<MMC5>),
    #[cfg(test)]
    Test(Box<TestCartridge>),
}

#[cfg(test)]
pub struct TestCartridge {
    pub(crate) prg_rom: [u8; 0x8000],
    pub(crate) chr_rom: [u8; 0x2000],
    name_table: NameTableControl,
}

#[cfg(test)]
impl TestCartridge {
    pub fn new() -> Self {
        Self {
            prg_rom: [0; 0x8000],
            chr_rom: [0; 0x2000],
            name_table: NameTableControl::new(Mirroring::Horizontal),
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

    pub fn write_nametable(&mut self, address: u16, value: u8) {
        self.name_table.write(address, value);
    }

    pub fn read_nametable(&self, address: u16) -> u8 {
        self.name_table.read(address)
    }
}

impl Cartridge {
    #[cfg(test)]
    pub fn pattern_ref(&self) -> &[u8] {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.pattern_ref(),
            Cartridge::Mapper2(cartridge) => cartridge.pattern_ref(),
            Cartridge::Mapper3(cartridge) => cartridge.pattern_ref(),
            Cartridge::MMC1(cartridge) => cartridge.pattern_ref(),
            Cartridge::MMC3(cartridge) => cartridge.pattern_ref(),
            Cartridge::MMC5(cartridge) => cartridge.pattern_ref(),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.pattern_ref(),
        }
    }

    pub fn write_pattern(&mut self, address: u16, value: u8) {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.write_pattern(address, value),
            Cartridge::Mapper2(cartridge) => cartridge.write_pattern(address, value),
            Cartridge::Mapper3(_) | Cartridge::MMC1(_) => {}
            Cartridge::MMC3(cartridge) => cartridge.write_pattern(address, value),
            Cartridge::MMC5(cartridge) => cartridge.write_pattern(address, value),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.write_pattern(address, value),
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.read(address),
            Cartridge::Mapper2(cartridge) => cartridge.read(address),
            Cartridge::Mapper3(cartridge) => cartridge.read(address),
            Cartridge::MMC1(cartridge) => cartridge.read(address),
            Cartridge::MMC3(cartridge) => cartridge.read(address),
            Cartridge::MMC5(cartridge) => cartridge.read(address),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.read(address),
        }
    }

    pub fn peek(&self, address: u16) -> u8 {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.peek(address),
            Cartridge::Mapper2(cartridge) => cartridge.peek(address),
            Cartridge::Mapper3(cartridge) => cartridge.peek(address),
            Cartridge::MMC1(cartridge) => cartridge.peek(address),
            Cartridge::MMC3(cartridge) => cartridge.peek(address),
            Cartridge::MMC5(cartridge) => cartridge.peek(address),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.peek(address),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.write(address, value),
            Cartridge::Mapper2(cartridge) => cartridge.write(address, value),
            Cartridge::Mapper3(cartridge) => cartridge.write(address, value),
            Cartridge::MMC1(cartridge) => cartridge.write(address, value),
            Cartridge::MMC3(cartridge) => cartridge.write(address, value),
            Cartridge::MMC5(cartridge) => cartridge.write(address, value),
            #[cfg(test)]
            Cartridge::Test(_) => {}
        }
    }

    pub fn on_ppu_tick(&mut self, scanline: u16, dot: u16, rendering_enabled: bool) {
        match self {
            Cartridge::Mapper0(_)
            | Cartridge::Mapper2(_)
            | Cartridge::Mapper3(_)
            | Cartridge::MMC1(_) => {}
            Cartridge::MMC3(cartridge) => cartridge.on_ppu_tick(scanline, dot, rendering_enabled),
            Cartridge::MMC5(cartridge) => cartridge.on_ppu_tick(scanline, dot, rendering_enabled),
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
            | Cartridge::MMC1(_) => false,
            Cartridge::MMC3(cartridge) => cartridge.irq_pending(),
            Cartridge::MMC5(cartridge) => cartridge.irq_pending(),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.irq_pending(),
        }
    }

    pub fn read_chr(&self, address: u16, access: PatternAccess) -> u8 {
        match self {
            Cartridge::Mapper0(cartridge) => {
                cartridge.pattern_ref()[address as usize % cartridge.pattern_ref().len()]
            }
            Cartridge::Mapper2(cartridge) => {
                cartridge.pattern_ref()[address as usize % cartridge.pattern_ref().len()]
            }
            Cartridge::Mapper3(cartridge) => {
                cartridge.pattern_ref()[address as usize % cartridge.pattern_ref().len()]
            }
            Cartridge::MMC1(cartridge) => {
                cartridge.pattern_ref()[address as usize % cartridge.pattern_ref().len()]
            }
            Cartridge::MMC3(cartridge) => {
                cartridge.pattern_ref()[address as usize % cartridge.pattern_ref().len()]
            }
            Cartridge::MMC5(cartridge) => cartridge.read_chr(address, access),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.read_chr(address),
        }
    }

    pub fn write_nametable(&mut self, address: u16, value: u8) {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.write_nametable(address, value),
            Cartridge::Mapper2(cartridge) => cartridge.write_nametable(address, value),
            Cartridge::Mapper3(cartridge) => cartridge.write_nametable(address, value),
            Cartridge::MMC1(cartridge) => cartridge.write_nametable(address, value),
            Cartridge::MMC3(cartridge) => cartridge.write_nametable(address, value),
            Cartridge::MMC5(cartridge) => cartridge.write_nametable(address, value),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.write_nametable(address, value),
        }
    }

    pub fn read_nametable(&self, address: u16) -> u8 {
        match self {
            Cartridge::Mapper0(cartridge) => cartridge.read_nametable(address),
            Cartridge::Mapper2(cartridge) => cartridge.read_nametable(address),
            Cartridge::Mapper3(cartridge) => cartridge.read_nametable(address),
            Cartridge::MMC1(cartridge) => cartridge.read_nametable(address),
            Cartridge::MMC3(cartridge) => cartridge.read_nametable(address),
            Cartridge::MMC5(cartridge) => cartridge.read_nametable(address),
            #[cfg(test)]
            Cartridge::Test(cartridge) => cartridge.read_nametable(address),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn background_override(
        &self,
        screen_x: u8,
        screen_y: u8,
        nametable_addr: u16,
        tile_idx: u8,
        palette_idx: u8,
        tile_fine_x: usize,
        tile_fine_y: usize,
    ) -> Option<BackgroundTileOverride> {
        match self {
            Cartridge::Mapper0(_)
            | Cartridge::Mapper2(_)
            | Cartridge::Mapper3(_)
            | Cartridge::MMC1(_)
            | Cartridge::MMC3(_) => None,
            Cartridge::MMC5(cartridge) => cartridge.background_override(
                screen_x,
                screen_y,
                nametable_addr,
                tile_idx,
                palette_idx,
                tile_fine_x,
                tile_fine_y,
            ),
            #[cfg(test)]
            Cartridge::Test(_) => None,
        }
    }

    pub fn on_ppu_ctrl_write(&mut self, value: u8) {
        if let Cartridge::MMC5(cartridge) = self {
            cartridge.on_ppu_ctrl_write(value);
        }
    }

    pub fn on_ppu_mask_write(&mut self, value: u8) {
        if let Cartridge::MMC5(cartridge) = self {
            cartridge.on_ppu_mask_write(value);
        }
    }

    pub fn on_ppu_scroll_write(&mut self, value: u8) {
        if let Cartridge::MMC5(cartridge) = self {
            cartridge.on_ppu_scroll_write(value);
        }
    }

    pub fn on_oam_dma(&mut self) {
        if let Cartridge::MMC5(cartridge) = self {
            cartridge.on_oam_dma();
        }
    }
}

#[cfg(test)]
mod tests;
