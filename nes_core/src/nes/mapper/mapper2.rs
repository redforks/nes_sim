use super::CARTRIDGE_START_ADDR;

const PRG_ROM_BANK_SIZE: usize = 0x4000;
const CHR_ROM_SIZE: usize = 0x2000;
const CARTRIDGE_RAM_SIZE: usize = 0x4000 - 0x20;

pub struct Mapper2 {
    prg_rom: Vec<u8>,
    chr: [u8; CHR_ROM_SIZE],
    has_chr_ram: bool,
    ram: [u8; CARTRIDGE_RAM_SIZE],
    selected_prg_bank: usize,
    prg_bank_count: usize,
}

impl Mapper2 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8]) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert_eq!(prg_rom.len() % PRG_ROM_BANK_SIZE, 0);
        debug_assert!(chr_rom.len() <= CHR_ROM_SIZE);

        let mut mapper = Self {
            prg_rom: prg_rom.to_vec(),
            chr: [0; CHR_ROM_SIZE],
            has_chr_ram: chr_rom.is_empty(),
            ram: [0; CARTRIDGE_RAM_SIZE],
            selected_prg_bank: 0,
            prg_bank_count: prg_rom.len() / PRG_ROM_BANK_SIZE,
        };
        mapper.chr[..chr_rom.len()].copy_from_slice(chr_rom);
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

impl Mapper2 {
    pub fn pattern_ref(&self) -> &[u8] {
        &self.chr
    }

    pub fn write_pattern(&mut self, address: u16, value: u8) {
        if self.has_chr_ram {
            self.chr[address as usize % CHR_ROM_SIZE] = value;
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => self.ram[(address - CARTRIDGE_START_ADDR) as usize],
            0x8000..=0xbfff => self.read_prg_bank(self.selected_prg_bank(), address - 0x8000),
            0xc000..=0xffff => self.read_prg_bank(self.last_prg_bank(), address - 0xc000),
            _ => unreachable!(),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => {
                self.ram[(address - CARTRIDGE_START_ADDR) as usize] = value;
            }
            0x8000..=0xffff => {
                self.selected_prg_bank = (value as usize) % self.prg_bank_count;
            }
            _ => unreachable!(),
        }
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

        assert_eq!(mapper.pattern_ref()[0], 1);
        assert_eq!(mapper.pattern_ref()[1], 2);
        assert_eq!(mapper.pattern_ref()[2], 3);
    }

    #[test]
    fn writes_to_chr_ram_when_chr_rom_is_absent() {
        let mut mapper = Mapper2::new(&[0; PRG_ROM_BANK_SIZE * 2], &[]);

        mapper.write_pattern(0x0000, 0x12);
        mapper.write_pattern(0x1fff, 0x34);

        assert_eq!(mapper.pattern_ref()[0], 0x12);
        assert_eq!(mapper.pattern_ref()[0x1fff], 0x34);
    }

    #[test]
    fn ignores_pattern_writes_when_chr_rom_is_present() {
        let mut mapper = Mapper2::new(&[0; PRG_ROM_BANK_SIZE * 2], &[0x56]);

        mapper.write_pattern(0x0000, 0x12);

        assert_eq!(mapper.pattern_ref()[0], 0x56);
    }
}
