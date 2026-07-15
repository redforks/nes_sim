use super::{CARTRIDGE_START_ADDR, Cartridge, CartridgeOperation};

const PRG_ROM_BANK_SIZE: usize = 0x8000;
const CARTRIDGE_RAM_SIZE: usize = 0x4000 - 0x20;

pub struct BxRom {
    prg_rom: Vec<u8>,
    ram: [u8; CARTRIDGE_RAM_SIZE],
    selected_prg_bank: usize,
    prg_bank_count: usize,
    chr_data: Vec<u8>,
    has_chr_ram: bool,
}

impl BxRom {
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

impl Cartridge for BxRom {
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
        let len = self.chr_data.len();
        self.chr_data[addr % len]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        if !self.has_chr_ram {
            return;
        }
        let addr = address as usize % 0x2000;
        let len = self.chr_data.len();
        self.chr_data[addr % len] = value;
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

        let mut mapper = BxRom::new(&prg_rom, &[]);

        assert_eq!(mapper.read(0x8000), 0x10);

        mapper.write(0x8000, 0x02);
        assert_eq!(mapper.read(0x8000), 0x30);

        mapper.write(0xffff, 0x03);
        assert_eq!(mapper.read(0x8000), 0x40);
    }

    #[test]
    fn chr_flat_access() {
        let mut mapper = BxRom::new(&[0; PRG_ROM_BANK_SIZE], &[]);
        mapper.write_chr(0x0000, 0x12);
        mapper.write_chr(0x1fff, 0x34);
        assert_eq!(mapper.read_chr(0), 0x12);
        assert_eq!(mapper.read_chr(0x1fff), 0x34);
    }
}
