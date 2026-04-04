use super::CARTRIDGE_START_ADDR;
use crate::nes::mapper::Mirroring;
use crate::nes::mapper::NameTableControl;

const PRG_ROM_SIZE: usize = 0x8000;
const CHR_BANK_SIZE: usize = 0x2000;
const CHR_WINDOW_SIZE: usize = 0x2000;
const CARTRIDGE_RAM_SIZE: usize = 0x4000 - 0x20;

pub struct Mapper3 {
    prg_rom: [u8; PRG_ROM_SIZE],
    prg_rom_len: usize,
    chr_rom: Vec<u8>,
    chr_window: [u8; CHR_WINDOW_SIZE],
    selected_chr_bank: usize,
    chr_bank_count: usize,
    ram: [u8; CARTRIDGE_RAM_SIZE],
    name_table: NameTableControl,
}

impl Mapper3 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8], mirroring: Mirroring) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert!(prg_rom.len() <= PRG_ROM_SIZE);
        debug_assert!(!chr_rom.is_empty());
        debug_assert_eq!(chr_rom.len() % CHR_BANK_SIZE, 0);

        let mut mapper = Self {
            prg_rom: [0; PRG_ROM_SIZE],
            prg_rom_len: prg_rom.len(),
            chr_rom: chr_rom.to_vec(),
            chr_window: [0; CHR_WINDOW_SIZE],
            selected_chr_bank: 0,
            chr_bank_count: chr_rom.len() / CHR_BANK_SIZE,
            ram: [0; CARTRIDGE_RAM_SIZE],
            name_table: NameTableControl::new(mirroring),
        };
        mapper.prg_rom[..prg_rom.len()].copy_from_slice(prg_rom);
        mapper.refresh_chr_window();
        mapper
    }

    fn selected_chr_bank(&self) -> usize {
        self.selected_chr_bank % self.chr_bank_count
    }

    fn refresh_chr_window(&mut self) {
        let bank_start = self.selected_chr_bank() * CHR_BANK_SIZE;
        self.chr_window
            .copy_from_slice(&self.chr_rom[bank_start..bank_start + CHR_BANK_SIZE]);
    }
}

impl Mapper3 {
    pub fn pattern_ref(&self) -> &[u8] {
        &self.chr_window
    }

    pub fn write_pattern(&mut self, _address: u16, _value: u8) {}

    pub fn read(&mut self, address: u16) -> u8 {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => self.ram[(address - CARTRIDGE_START_ADDR) as usize],
            0x8000..=0xffff => {
                let offset = (address - 0x8000) as usize;
                let prg_offset = match self.prg_rom_len {
                    0x4000 => offset % 0x4000,
                    _ => offset,
                };
                self.prg_rom[prg_offset]
            }
            _ => unreachable!(),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => {
                self.ram[(address - CARTRIDGE_START_ADDR) as usize] = value;
            }
            0x8000..=0xffff => {
                self.selected_chr_bank = value as usize;
                self.refresh_chr_window();
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
    fn reads_and_writes_cartridge_ram() {
        let mut mapper = Mapper3::new(&[0; 0x8000], &[0; CHR_BANK_SIZE], Mirroring::Horizontal);

        mapper.write(CARTRIDGE_START_ADDR, 0x12);
        mapper.write(0x7fff, 0x34);

        assert_eq!(mapper.read(CARTRIDGE_START_ADDR), 0x12);
        assert_eq!(mapper.read(0x7fff), 0x34);
    }

    #[test]
    fn mirrors_16k_prg_into_upper_bank() {
        let mut prg = [0u8; 0x4000];
        prg[0x0000] = 0x11;
        prg[0x3fff] = 0x22;

        let mut mapper = Mapper3::new(&prg, &[0; CHR_BANK_SIZE], Mirroring::Horizontal);

        assert_eq!(mapper.read(0x8000), 0x11);
        assert_eq!(mapper.read(0xbfff), 0x22);
        assert_eq!(mapper.read(0xc000), 0x11);
        assert_eq!(mapper.read(0xffff), 0x22);
    }

    #[test]
    fn switches_8k_chr_banks() {
        let prg = [0u8; 0x8000];
        let mut chr = vec![0u8; CHR_BANK_SIZE * 4];
        chr[0] = 0x10;
        chr[CHR_BANK_SIZE] = 0x20;
        chr[CHR_BANK_SIZE * 2] = 0x30;
        chr[CHR_BANK_SIZE * 3] = 0x40;

        let mut mapper = Mapper3::new(&prg, &chr, Mirroring::Horizontal);

        assert_eq!(mapper.pattern_ref()[0], 0x10);

        mapper.write(0x8000, 0x01);
        assert_eq!(mapper.pattern_ref()[0], 0x20);

        mapper.write(0x9000, 0x02);
        assert_eq!(mapper.pattern_ref()[0], 0x30);

        mapper.write(0xffff, 0x03);
        assert_eq!(mapper.pattern_ref()[0], 0x40);
    }

    #[test]
    fn wraps_chr_bank_selection() {
        let prg = [0u8; 0x8000];
        let mut chr = vec![0u8; CHR_BANK_SIZE * 2];
        chr[0] = 0xaa;
        chr[CHR_BANK_SIZE] = 0xbb;

        let mut mapper = Mapper3::new(&prg, &chr, Mirroring::Horizontal);

        mapper.write(0x8000, 0x03);

        assert_eq!(mapper.pattern_ref()[0], 0xbb);
    }

    #[test]
    fn ignores_pattern_writes() {
        let mut mapper = Mapper3::new(&[0; 0x8000], &[0x56; CHR_BANK_SIZE], Mirroring::Horizontal);

        mapper.write_pattern(0x0000, 0x12);

        assert_eq!(mapper.pattern_ref()[0], 0x56);
    }
}
