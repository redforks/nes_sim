use super::CARTRIDGE_START_ADDR;
use crate::nes::mapper::Mirroring;
use crate::nes::mapper::NameTableControl;

const PRG_ROM_BANK_SIZE: usize = 0x8000;
const CHR_SIZE: usize = 0x2000;
const CARTRIDGE_RAM_SIZE: usize = 0x4000 - 0x20;

pub struct Mapper34 {
    prg_rom: Vec<u8>,
    chr: [u8; CHR_SIZE],
    has_chr_ram: bool,
    ram: [u8; CARTRIDGE_RAM_SIZE],
    selected_prg_bank: usize,
    prg_bank_count: usize,
    name_table: NameTableControl,
}

impl Mapper34 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8], mirroring: Mirroring) -> Self {
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
            name_table: NameTableControl::new(mirroring),
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
                self.selected_prg_bank = value as usize;
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

        let mut mapper = Mapper34::new(&prg_rom, &[], Mirroring::Horizontal);

        assert_eq!(mapper.read(0x8000), 0x10);

        mapper.write(0x8000, 0x02);
        assert_eq!(mapper.read(0x8000), 0x30);

        mapper.write(0xffff, 0x03);
        assert_eq!(mapper.read(0x8000), 0x40);
    }

    #[test]
    fn writes_to_chr_ram_when_chr_rom_is_absent() {
        let mut mapper = Mapper34::new(&[0; PRG_ROM_BANK_SIZE], &[], Mirroring::Horizontal);

        mapper.write_pattern(0x0000, 0x12);
        mapper.write_pattern(0x1fff, 0x34);

        assert_eq!(mapper.pattern_ref()[0], 0x12);
        assert_eq!(mapper.pattern_ref()[0x1fff], 0x34);
    }

    #[test]
    fn keeps_hardwired_mirroring() {
        let mut mapper = Mapper34::new(&[0; PRG_ROM_BANK_SIZE], &[], Mirroring::Vertical);

        mapper.write_nametable(0x2000, 0x11);
        mapper.write_nametable(0x2400, 0x22);

        assert_eq!(mapper.read_nametable(0x2000), 0x11);
        assert_eq!(mapper.read_nametable(0x2800), 0x11);
        assert_eq!(mapper.read_nametable(0x2400), 0x22);
        assert_eq!(mapper.read_nametable(0x2c00), 0x22);
    }
}
