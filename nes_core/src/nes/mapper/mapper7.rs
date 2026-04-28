use super::CARTRIDGE_START_ADDR;
use crate::nes::mapper::Mirroring;
use crate::nes::mapper::NameTableControl;

const PRG_ROM_BANK_SIZE: usize = 0x8000;
const CHR_SIZE: usize = 0x2000;
const CARTRIDGE_RAM_SIZE: usize = 0x4000 - 0x20;

pub struct Mapper7 {
    prg_rom: Vec<u8>,
    chr: [u8; CHR_SIZE],
    has_chr_ram: bool,
    ram: [u8; CARTRIDGE_RAM_SIZE],
    selected_prg_bank: usize,
    prg_bank_count: usize,
    name_table: NameTableControl,
}

impl Mapper7 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8]) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert_eq!(prg_rom.len() % PRG_ROM_BANK_SIZE, 0);
        debug_assert!(chr_rom.len() <= CHR_SIZE);

        let mut mapper = Self {
            prg_rom: prg_rom.to_vec(),
            chr: [0; CHR_SIZE],
            has_chr_ram: chr_rom.is_empty(),
            ram: [0; CARTRIDGE_RAM_SIZE],
            selected_prg_bank: 0,
            prg_bank_count: prg_rom.len() / PRG_ROM_BANK_SIZE,
            name_table: NameTableControl::new(Mirroring::LowerBank),
        };
        mapper.chr[..chr_rom.len()].copy_from_slice(chr_rom);
        mapper
    }

    fn selected_prg_bank(&self) -> usize {
        self.selected_prg_bank % self.prg_bank_count
    }

    fn read_prg_bank(&self, bank: usize, address: u16) -> u8 {
        let bank_start = bank * PRG_ROM_BANK_SIZE;
        let offset = (address as usize) % PRG_ROM_BANK_SIZE;
        self.prg_rom[bank_start + offset]
    }

    pub fn pattern_ref(&self) -> &[u8] {
        &self.chr
    }

    pub fn write_pattern(&mut self, address: u16, value: u8) {
        if self.has_chr_ram {
            self.chr[address as usize % CHR_SIZE] = value;
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
        self.peek(address)
    }

    pub fn peek(&self, address: u16) -> u8 {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => self.ram[(address - CARTRIDGE_START_ADDR) as usize],
            0x8000..=0xffff => self.read_prg_bank(self.selected_prg_bank(), address - 0x8000),
            _ => unreachable!(),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => {
                self.ram[(address - CARTRIDGE_START_ADDR) as usize] = value;
            }
            0x8000..=0xffff => {
                self.selected_prg_bank = (value & 0x0f) as usize;
                let mirroring = if value & 0x10 == 0 {
                    Mirroring::LowerBank
                } else {
                    Mirroring::UpperBank
                };
                self.name_table.set_mirroring(mirroring);
            }
            _ => unreachable!(),
        }
    }

    pub fn write_nametable(&mut self, address: u16, value: u8) {
        self.name_table.write(address, value);
    }

    pub fn read_nametable(&self, address: u16) -> u8 {
        self.name_table.read(address)
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

        let mut mapper = Mapper7::new(&prg_rom, &[]);

        assert_eq!(mapper.read(0x8000), 0x10);

        mapper.write(0x8000, 0x02);
        assert_eq!(mapper.read(0x8000), 0x30);

        mapper.write(0xffff, 0x03);
        assert_eq!(mapper.read(0x8000), 0x40);
    }

    #[test]
    fn switches_single_screen_mirroring() {
        let mut mapper = Mapper7::new(&[0; PRG_ROM_BANK_SIZE], &[]);

        mapper.write_nametable(0x2000, 0x11);

        assert_eq!(mapper.read_nametable(0x2000), 0x11);
        assert_eq!(mapper.read_nametable(0x2400), 0x11);
        assert_eq!(mapper.read_nametable(0x2800), 0x11);
        assert_eq!(mapper.read_nametable(0x2c00), 0x11);

        mapper.write(0x8000, 0x10);
        mapper.write_nametable(0x2400, 0x22);

        assert_eq!(mapper.read_nametable(0x2000), 0x22);
        assert_eq!(mapper.read_nametable(0x2800), 0x22);
        assert_eq!(mapper.read_nametable(0x2400), 0x22);
        assert_eq!(mapper.read_nametable(0x2c00), 0x22);
    }
}
