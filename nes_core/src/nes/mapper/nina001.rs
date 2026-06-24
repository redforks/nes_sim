use super::{Cartridge, CARTRIDGE_START_ADDR, CartridgeOperation};

const PRG_ROM_BANK_SIZE: usize = 0x8000;
const CARTRIDGE_RAM_SIZE: usize = 0x4000 - 0x20;

pub struct Nina001Rom {
    prg_rom: Vec<u8>,
    ram: [u8; CARTRIDGE_RAM_SIZE],
    selected_prg_bank: usize,
    prg_bank_count: usize,
    chr_data: Vec<u8>,
    has_chr_ram: bool,
    chr_bank0: usize,
    chr_bank1: usize,
}

impl Nina001Rom {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8]) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert_eq!(prg_rom.len() % PRG_ROM_BANK_SIZE, 0);

        let has_chr_ram = chr_rom.is_empty();
        let chr_data = if has_chr_ram {
            vec![0; 0x2000]
        } else {
            chr_rom.to_vec()
        };

        Self {
            prg_rom: prg_rom.to_vec(),
            ram: [0; CARTRIDGE_RAM_SIZE],
            selected_prg_bank: 0,
            prg_bank_count: prg_rom.len() / PRG_ROM_BANK_SIZE,
            chr_data,
            has_chr_ram,
            chr_bank0: 0,
            chr_bank1: 0,
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

impl Cartridge for Nina001Rom {
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
                match address {
                    0x7ffd => self.selected_prg_bank = (value & 0x01) as usize,
                    0x7ffe => self.chr_bank0 = (value & 0x0f) as usize,
                    0x7fff => self.chr_bank1 = (value & 0x0f) as usize,
                    _ => {}
                }
            }
            0x8000..=0xffff => {
                self.selected_prg_bank = value as usize;
            }
            _ => unreachable!("Invalid write address: {:#04x}", address),
        }
        CartridgeOperation::None
    }

    fn read_chr(&self, address: u16) -> u8 {
        let addr = address as usize % 0x2000;
        let bank = if addr < 0x1000 { self.chr_bank0 } else { self.chr_bank1 };
        let src = bank * 0x1000 + (addr % 0x1000);
        let len = self.chr_data.len();
        self.chr_data[src % len]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        if !self.has_chr_ram {
            return;
        }
        let addr = address as usize % 0x2000;
        let bank = if addr < 0x1000 { self.chr_bank0 } else { self.chr_bank1 };
        let src = bank * 0x1000 + (addr % 0x1000);
        let len = self.chr_data.len();
        self.chr_data[src % len] = value;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chr_banking() {
        let mut data = vec![0u8; 0x4000];
        data[0x0000] = 0xa1;
        data[0x1000] = 0xb1;
        let mut mapper = Nina001Rom::new(&[0; PRG_ROM_BANK_SIZE], &data);
        mapper.write(0x7ffe, 0x00);
        mapper.write(0x7fff, 0x01);
        assert_eq!(mapper.read_chr(0x0000), 0xa1);
        assert_eq!(mapper.read_chr(0x1000), 0xb1);
    }
}
