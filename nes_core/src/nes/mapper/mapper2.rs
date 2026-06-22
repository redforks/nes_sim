use super::{Cartridge, ChrStorage, CARTRIDGE_START_ADDR, CartridgeOperation};
use super::chr_storage::DirectChr;

const PRG_ROM_BANK_SIZE: usize = 0x4000;
const CARTRIDGE_RAM_SIZE: usize = 0x4000 - 0x20;

pub struct Mapper2 {
    prg_rom: Vec<u8>,
    has_chr_ram: bool,
    chr_storage: DirectChr,
    ram: [u8; CARTRIDGE_RAM_SIZE],
    selected_prg_bank: usize,
    prg_bank_count: usize,
}

impl Mapper2 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8]) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert_eq!(prg_rom.len() % PRG_ROM_BANK_SIZE, 0);
        debug_assert!(chr_rom.len() <= 0x2000);

        let mut mapper = Self {
            prg_rom: prg_rom.to_vec(),
            has_chr_ram: chr_rom.is_empty(),
            chr_storage: DirectChr::empty(),
            ram: [0; CARTRIDGE_RAM_SIZE],
            selected_prg_bank: 0,
            prg_bank_count: prg_rom.len() / PRG_ROM_BANK_SIZE,
        };
        for i in 0..chr_rom.len() {
            mapper.chr_storage.write_chr(i as u16, chr_rom[i]);
        }
        mapper
    }

    fn selected_prg_bank(&self) -> usize {
        self.selected_prg_bank % self.prg_bank_count
    }

    fn last_prg_bank(&self) -> usize {
        self.prg_bank_count - 1
    }

    fn read_prg_bank(&self, bank: usize, address: u16) -> u8 {
        let bank_start = bank * PRG_ROM_BANK_SIZE;
        let offset = (address as usize) % PRG_ROM_BANK_SIZE;
        self.prg_rom[bank_start + offset]
    }
}

impl Cartridge for Mapper2 {
    fn read_chr(&self, address: u16) -> u8 {
        self.chr_storage.read_chr(address)
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        if self.has_chr_ram {
            self.chr_storage.write_chr(address, value);
        }
    }

    fn read(&mut self, address: u16) -> u8 {
        self.peek(address)
    }

    fn peek(&self, address: u16) -> u8 {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => self.ram[(address - CARTRIDGE_START_ADDR) as usize],
            0x8000..=0xbfff => self.read_prg_bank(self.selected_prg_bank(), address - 0x8000),
            0xc000..=0xffff => self.read_prg_bank(self.last_prg_bank(), address - 0xc000),
            _ => unreachable!(),
        }
    }

    fn write(&mut self, address: u16, value: u8) -> CartridgeOperation {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => {
                self.ram[(address - CARTRIDGE_START_ADDR) as usize] = value;
            }
            0x8000..=0xffff => {
                self.selected_prg_bank = (value as usize) % self.prg_bank_count;
            }
            _ => unreachable!(),
        }
        CartridgeOperation::None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_and_writes_cartridge_ram() {
        let mut mapper = Mapper2::new(&[0; PRG_ROM_BANK_SIZE * 2], &[]);

        mapper.write(CARTRIDGE_START_ADDR, 0x12);
        mapper.write(0x7fff, 0x34);

        assert_eq!(mapper.read(CARTRIDGE_START_ADDR), 0x12);
        assert_eq!(mapper.read(0x7fff), 0x34);
    }

    #[test]
    fn switches_lower_prg_bank_and_keeps_last_bank_fixed() {
        let mut prg_rom = [0u8; PRG_ROM_BANK_SIZE * 4];
        prg_rom[0] = 0x10;
        prg_rom[PRG_ROM_BANK_SIZE] = 0x20;
        prg_rom[PRG_ROM_BANK_SIZE * 2] = 0x30;
        prg_rom[PRG_ROM_BANK_SIZE * 3] = 0x40;
        prg_rom[(PRG_ROM_BANK_SIZE * 4) - 1] = 0x4f;

        let mut mapper = Mapper2::new(&prg_rom, &[]);

        assert_eq!(mapper.read(0x8000), 0x10);
        assert_eq!(mapper.read(0xc000), 0x40);
        assert_eq!(mapper.read(0xffff), 0x4f);

        mapper.write(0x8000, 0x02);
        assert_eq!(mapper.read(0x8000), 0x30);
        assert_eq!(mapper.read(0xbfff), prg_rom[(PRG_ROM_BANK_SIZE * 3) - 1]);
        assert_eq!(mapper.read(0xc000), 0x40);
        assert_eq!(mapper.read(0xffff), 0x4f);
    }

    #[test]
    fn exposes_chr_rom_patterns() {
        let mapper = Mapper2::new(&[0; PRG_ROM_BANK_SIZE * 2], &[1, 2, 3]);

        assert_eq!(mapper.read_chr(0), 1);
        assert_eq!(mapper.read_chr(1), 2);
        assert_eq!(mapper.read_chr(2), 3);
    }

    #[test]
    fn writes_to_chr_ram_when_chr_rom_is_absent() {
        let mut mapper = Mapper2::new(&[0; PRG_ROM_BANK_SIZE * 2], &[]);

        mapper.write_chr(0x0000, 0x12);
        mapper.write_chr(0x1fff, 0x34);

        assert_eq!(mapper.read_chr(0), 0x12);
        assert_eq!(mapper.read_chr(0x1fff), 0x34);
    }

    #[test]
    fn ignores_pattern_writes_when_chr_rom_is_present() {
        let mut mapper = Mapper2::new(&[0; PRG_ROM_BANK_SIZE * 2], &[0x56]);

        mapper.write_chr(0x0000, 0x12);

        assert_eq!(mapper.read_chr(0), 0x56);
    }
}
