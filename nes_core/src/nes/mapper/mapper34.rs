use super::{Cartridge, CARTRIDGE_START_ADDR, CartridgeOperation, ChrStorage};

const PRG_ROM_BANK_SIZE: usize = 0x8000;
const CARTRIDGE_RAM_SIZE: usize = 0x4000 - 0x20;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Board {
    BxRom,
    Nina001,
}

pub struct Mapper34 {
    prg_rom: Vec<u8>,
    ram: [u8; CARTRIDGE_RAM_SIZE],
    selected_prg_bank: usize,
    prg_bank_count: usize,
    board: Board,
}

impl Mapper34 {
    pub fn new(prg_rom: &[u8], board: Board) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert_eq!(prg_rom.len() % PRG_ROM_BANK_SIZE, 0);

        Self {
            prg_rom: prg_rom.to_vec(),
            ram: [0; CARTRIDGE_RAM_SIZE],
            selected_prg_bank: 0,
            prg_bank_count: prg_rom.len() / PRG_ROM_BANK_SIZE,
            board,
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

impl Cartridge for Mapper34 {
    fn read(&mut self, address: u16) -> u8 {
        self.peek(address)
    }

    fn peek(&self, address: u16) -> u8 {
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
                if self.board == Board::Nina001 {
                    match address {
                        0x7ffd => self.selected_prg_bank = (value & 0x01) as usize,
                        _ => {}
                    }
                }
            }
            0x8000..=0xffff => {
                self.selected_prg_bank = value as usize;
            }
            _ => unreachable!("Invalid write address: {:#04x}", address),
        }
        CartridgeOperation::None
    }
}

pub struct Mapper34ChrStorage {
    data: Vec<u8>,
    has_chr_ram: bool,
    is_nina: bool,
    bank0: usize,
    bank1: usize,
}

impl Mapper34ChrStorage {
    pub fn new(chr_rom: &[u8], is_nina: bool) -> Self {
        let has_chr_ram = chr_rom.is_empty();
        let data = if has_chr_ram {
            vec![0; 0x2000]
        } else {
            chr_rom.to_vec()
        };
        Self {
            data,
            has_chr_ram,
            is_nina,
            bank0: 0,
            bank1: 0,
        }
    }
}

impl ChrStorage for Mapper34ChrStorage {
    fn read_chr(&self, address: u16) -> u8 {
        let addr = address as usize % 0x2000;
        if self.is_nina {
            let bank = if addr < 0x1000 {
                self.bank0
            } else {
                self.bank1
            };
            let src = bank * 0x1000 + (addr % 0x1000);
            self.data[src % self.data.len()]
        } else {
            self.data[addr % self.data.len()]
        }
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        if !self.has_chr_ram {
            return;
        }
        let addr = address as usize % 0x2000;
        if self.is_nina {
            let bank = if addr < 0x1000 {
                self.bank0
            } else {
                self.bank1
            };
            let src = bank * 0x1000 + (addr % 0x1000);
            let len = self.data.len();
            self.data[src % len] = value;
        } else {
            let len = self.data.len();
            self.data[addr % len] = value;
        }
    }

    fn write_register(&mut self, addr: u16, value: u8) {
        if self.is_nina && (0x7ffe..=0x7fff).contains(&addr) {
            match addr {
                0x7ffe => self.bank0 = (value & 0x0f) as usize,
                0x7fff => self.bank1 = (value & 0x0f) as usize,
                _ => {}
            }
        }
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

        let mut mapper = Mapper34::new(&prg_rom, Board::BxRom);

        assert_eq!(mapper.read(0x8000), 0x10);

        mapper.write(0x8000, 0x02);
        assert_eq!(mapper.read(0x8000), 0x30);

        mapper.write(0xffff, 0x03);
        assert_eq!(mapper.read(0x8000), 0x40);
    }

    #[test]
    fn chr_bxrom_flat_access() {
        let mut chr = Mapper34ChrStorage::new(&[], false);
        chr.write_chr(0x0000, 0x12);
        chr.write_chr(0x1fff, 0x34);
        assert_eq!(chr.read_chr(0), 0x12);
        assert_eq!(chr.read_chr(0x1fff), 0x34);
    }

    #[test]
    fn chr_nina001_banking() {
        let mut data = vec![0u8; 0x4000];
        data[0x0000] = 0xa1;
        data[0x1000] = 0xb1;
        let mut chr = Mapper34ChrStorage::new(&data, true);
        chr.write_register(0x7ffe, 0x00);
        chr.write_register(0x7fff, 0x01);
        assert_eq!(chr.read_chr(0x0000), 0xa1);
        assert_eq!(chr.read_chr(0x1000), 0xb1);
    }
}
