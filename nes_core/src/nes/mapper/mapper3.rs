use super::{Cartridge, CARTRIDGE_START_ADDR, CartridgeOperation};

const PRG_ROM_SIZE: usize = 0x8000;
const CARTRIDGE_RAM_SIZE: usize = 0x4000 - 0x20;

pub struct Mapper3 {
    prg_rom: [u8; PRG_ROM_SIZE],
    prg_rom_len: usize,
    ram: [u8; CARTRIDGE_RAM_SIZE],
    chr_cache: [u8; 0x2000],
    chr_source: Vec<u8>,
    chr_bank_count: usize,
    has_chr_ram: bool,
}

impl Mapper3 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8]) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert!(prg_rom.len() <= PRG_ROM_SIZE);

        let has_chr_ram = chr_rom.is_empty();
        let bank_count = if has_chr_ram {
            1
        } else {
            chr_rom.len() / 0x2000
        };
        let mut cache = [0; 0x2000];
        let len = chr_rom.len().min(0x2000);
        cache[..len].copy_from_slice(&chr_rom[..len]);

        let mut mapper = Self {
            prg_rom: [0; PRG_ROM_SIZE],
            prg_rom_len: prg_rom.len(),
            ram: [0; CARTRIDGE_RAM_SIZE],
            chr_cache: cache,
            chr_source: chr_rom.to_vec(),
            chr_bank_count: bank_count,
            has_chr_ram,
        };
        mapper.prg_rom[..prg_rom.len()].copy_from_slice(prg_rom);
        mapper
    }

    fn chr_refresh(&mut self, offset_into_source: usize) {
        let end = (offset_into_source + 0x2000).min(self.chr_source.len());
        let len = end - offset_into_source;
        self.chr_cache[..len].copy_from_slice(&self.chr_source[offset_into_source..end]);
    }
}

impl Cartridge for Mapper3 {
    fn read(&self, address: u16) -> u8 {
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

    fn write(&mut self, address: u16, value: u8) -> CartridgeOperation {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => {
                self.ram[(address - CARTRIDGE_START_ADDR) as usize] = value;
            }
            0x8000..=0xffff => {
                let bank = (value as usize) % self.chr_bank_count;
                self.chr_refresh(bank * 0x2000);
            }
            _ => unreachable!(),
        }
        CartridgeOperation::None
    }

    fn read_chr(&self, address: u16) -> u8 {
        self.chr_cache[address as usize % self.chr_cache.len()]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        if self.has_chr_ram {
            self.chr_cache[address as usize % self.chr_cache.len()] = value;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_and_writes_cartridge_ram() {
        let mut mapper = Mapper3::new(&[0; 0x8000], &[]);

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

        let mapper = Mapper3::new(&prg, &[]);

        assert_eq!(mapper.read(0x8000), 0x11);
        assert_eq!(mapper.read(0xbfff), 0x22);
        assert_eq!(mapper.read(0xc000), 0x11);
        assert_eq!(mapper.read(0xffff), 0x22);
    }

    fn make_chr() -> Vec<u8> {
        let mut data = vec![0u8; 0x4000];
        data[0] = 0x10;
        data[0x2000] = 0x20;
        data
    }

    #[test]
    fn chr_initial_state_reads_bank_0() {
        let mapper = Mapper3::new(&[0; 0x8000], &make_chr());
        assert_eq!(mapper.read_chr(0), 0x10);
    }

    #[test]
    fn chr_write_register_switches_bank() {
        let mut mapper = Mapper3::new(&[0; 0x8000], &make_chr());
        mapper.write(0x8000, 1);
        assert_eq!(mapper.read_chr(0), 0x20);
    }

    #[test]
    fn chr_bank_wraps_at_bank_count() {
        let mut mapper = Mapper3::new(&[0; 0x8000], &make_chr());
        mapper.write(0x8000, 2);
        assert_eq!(mapper.read_chr(0), 0x10);
    }

    #[test]
    fn chr_ram_writes_and_reads_cache() {
        let mut mapper = Mapper3::new(&[0; 0x8000], &[]);
        mapper.write_chr(0x100, 0xab);
        assert_eq!(mapper.read_chr(0x100), 0xab);
    }

    #[test]
    fn chr_refresh_copies_source_to_cache() {
        let mut source = vec![0; 0x4000];
        source[0x2100] = 0xcd;
        let mut mapper = Mapper3::new(&[0; 0x8000], &source);
        mapper.write(0x8000, 1);
        assert_eq!(mapper.read_chr(0x100), 0xcd);
    }
}
