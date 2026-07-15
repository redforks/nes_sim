use super::chr_storage::DirectChr;
use super::{CARTRIDGE_START_ADDR, Cartridge, CartridgeOperation, Mirroring};

const PRG_ROM_BANK_SIZE: usize = 0x8000;
const CARTRIDGE_RAM_SIZE: usize = 0x4000 - 0x20;

pub struct AxRom {
    prg_rom: Vec<u8>,
    ram: [u8; CARTRIDGE_RAM_SIZE],
    selected_prg_bank: usize,
    prg_bank_count: usize,
    chr: DirectChr,
}

impl AxRom {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8]) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert_eq!(prg_rom.len() % PRG_ROM_BANK_SIZE, 0);

        Self {
            prg_rom: prg_rom.to_vec(),
            ram: [0; CARTRIDGE_RAM_SIZE],
            selected_prg_bank: 0,
            prg_bank_count: prg_rom.len() / PRG_ROM_BANK_SIZE,
            chr: DirectChr::from_chr_rom(chr_rom),
        }
    }

    fn selected_prg_bank(&self) -> usize {
        self.selected_prg_bank % self.prg_bank_count
    }

    fn read_prg_bank(&self, bank: usize, address: u16) -> u8 {
        let bank_start = bank * PRG_ROM_BANK_SIZE;
        let offset = (address as usize) % PRG_ROM_BANK_SIZE;
        self.prg_rom[bank_start + offset]
    }
}

impl Cartridge for AxRom {
    fn read(&self, address: u16) -> u8 {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => self.ram[(address - CARTRIDGE_START_ADDR) as usize],
            0x8000..=0xffff => self.read_prg_bank(self.selected_prg_bank(), address - 0x8000),
            _ => unreachable!(),
        }
    }

    fn write(&mut self, address: u16, value: u8) -> CartridgeOperation {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => {
                self.ram[(address - CARTRIDGE_START_ADDR) as usize] = value;
                CartridgeOperation::None
            }
            0x8000..=0xffff => {
                self.selected_prg_bank = (value & 0x0f) as usize;
                let mirroring = if value & 0x10 == 0 {
                    Mirroring::LowerBank
                } else {
                    Mirroring::UpperBank
                };
                CartridgeOperation::UpdateNametableMirroring(mirroring)
            }
            _ => unreachable!(),
        }
    }

    fn read_chr(&self, address: u16) -> u8 {
        self.chr.read_chr(address)
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        self.chr.write_chr(address, value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn switches_32k_prg_banks() {
        let mut prg_rom = [0u8; PRG_ROM_BANK_SIZE * 4];
        prg_rom[0] = 0x10;
        prg_rom[PRG_ROM_BANK_SIZE] = 0x20;
        prg_rom[PRG_ROM_BANK_SIZE * 2] = 0x30;
        prg_rom[PRG_ROM_BANK_SIZE * 3] = 0x40;

        let mut mapper = AxRom::new(&prg_rom, &[]);

        assert_eq!(mapper.read(0x8000), 0x10);

        mapper.write(0x8000, 0x02);
        assert_eq!(mapper.read(0x8000), 0x30);

        mapper.write(0xffff, 0x03);
        assert_eq!(mapper.read(0x8000), 0x40);
    }

    #[test]
    fn switches_single_screen_mirroring() {
        let mut mapper = AxRom::new(&[0; PRG_ROM_BANK_SIZE], &[]);

        assert_eq!(
            mapper.write(0x8000, 0x10),
            CartridgeOperation::UpdateNametableMirroring(Mirroring::UpperBank)
        );
    }
}
