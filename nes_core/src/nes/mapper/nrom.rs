use super::chr_storage::DirectChr;
use super::{CARTRIDGE_START_ADDR, Cartridge, CartridgeOperation};

pub struct NRom {
    prg_rom: [u8; 0x8000],
    prg_rom_len: usize,
    ram: [u8; 0x4000 - 0x20],
    chr: DirectChr,
}

impl NRom {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8]) -> Self {
        debug_assert!(prg_rom.len() <= 0x8000);

        let mut r = Self {
            prg_rom: [0; 0x8000],
            prg_rom_len: prg_rom.len(),
            ram: [0; 0x4000 - 0x20],
            chr: DirectChr::from_chr_rom(chr_rom),
        };
        r.prg_rom[0..prg_rom.len()].copy_from_slice(prg_rom);
        r
    }
}

impl Default for NRom {
    fn default() -> Self {
        Self {
            prg_rom: [0; 0x8000],
            prg_rom_len: 0,
            ram: [0; 0x4000 - 0x20],
            chr: DirectChr::empty(),
        }
    }
}

impl Cartridge for NRom {
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
            0x8000..=0xffff => {}
            _ => unreachable!(),
        }
        CartridgeOperation::None
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
    use crate::nes::mapper::CARTRIDGE_START_ADDR;

    #[test]
    fn mcu() {
        let mut mcu = NRom::default();

        // read-write ram
        mcu.write(CARTRIDGE_START_ADDR, 0x01);
        assert_eq!(mcu.read(CARTRIDGE_START_ADDR), 0x01);
        assert_eq!(mcu.ram[0], 0x01);
        mcu.write(0x7fff, 0x03);
        assert_eq!(mcu.read(0x7fff), 0x03);

        // read-write rom
        assert_eq!(mcu.read(0x8000), 0);
        // ignore write to rom
        mcu.write(0x8000, 0x03);
        assert_eq!(mcu.read(0x8000), 0);
    }

    #[test]
    fn mirrors_16k_prg_into_upper_bank() {
        let mut prg = [0u8; 0x4000];
        prg[0x3ffc] = 0x00;
        prg[0x3ffd] = 0x80;

        let mcu = NRom::new(&prg, &[]);

        assert_eq!(mcu.read(0xfffc), 0x00);
        assert_eq!(mcu.read(0xfffd), 0x80);
    }
}
