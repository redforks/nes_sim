use super::{Cartridge, CARTRIDGE_START_ADDR, CartridgeOperation};

const PRG_ROM_SIZE: usize = 0x8000;
const CARTRIDGE_RAM_SIZE: usize = 0x4000 - 0x20;

pub struct Mapper3 {
    prg_rom: [u8; PRG_ROM_SIZE],
    prg_rom_len: usize,
    ram: [u8; CARTRIDGE_RAM_SIZE],
}

impl Mapper3 {
    pub fn new(prg_rom: &[u8]) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert!(prg_rom.len() <= PRG_ROM_SIZE);

        let mut mapper = Self {
            prg_rom: [0; PRG_ROM_SIZE],
            prg_rom_len: prg_rom.len(),
            ram: [0; CARTRIDGE_RAM_SIZE],
        };
        mapper.prg_rom[..prg_rom.len()].copy_from_slice(prg_rom);
        mapper
    }
}

impl Cartridge for Mapper3 {
    fn read(&mut self, address: u16) -> u8 {
        self.peek(address)
    }

    fn peek(&self, address: u16) -> u8 {
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
            0x8000..=0xffff => {}
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
        let mut mapper = Mapper3::new(&[0; 0x8000]);

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

        let mut mapper = Mapper3::new(&prg);

        assert_eq!(mapper.read(0x8000), 0x11);
        assert_eq!(mapper.read(0xbfff), 0x22);
        assert_eq!(mapper.read(0xc000), 0x11);
        assert_eq!(mapper.read(0xffff), 0x22);
    }
}
