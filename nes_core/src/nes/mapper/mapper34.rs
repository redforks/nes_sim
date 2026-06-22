use super::{Cartridge, CARTRIDGE_START_ADDR, CartridgeOperation};

const PRG_ROM_BANK_SIZE: usize = 0x8000;
const CHR_SIZE: usize = 0x2000;
const CARTRIDGE_RAM_SIZE: usize = 0x4000 - 0x20;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Board {
    BxRom,
    Nina001,
}

pub struct Mapper34 {
    prg_rom: Vec<u8>,
    chr: Vec<u8>,
    has_chr_ram: bool,
    ram: [u8; CARTRIDGE_RAM_SIZE],
    selected_prg_bank: usize,
    selected_chr_bank_0: usize,
    selected_chr_bank_1: usize,
    prg_bank_count: usize,
    board: Board,
}

impl Mapper34 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8]) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert_eq!(prg_rom.len() % PRG_ROM_BANK_SIZE, 0);

        let board = if chr_rom.len() > CHR_SIZE {
            Board::Nina001
        } else {
            Board::BxRom
        };

        let chr = if chr_rom.is_empty() {
            vec![0; CHR_SIZE]
        } else {
            chr_rom.to_vec()
        };

        Self {
            prg_rom: prg_rom.to_vec(),
            chr,
            has_chr_ram: chr_rom.is_empty(),
            ram: [0; CARTRIDGE_RAM_SIZE],
            selected_prg_bank: 0,
            selected_chr_bank_0: 0,
            selected_chr_bank_1: 0,
            prg_bank_count: prg_rom.len() / PRG_ROM_BANK_SIZE,
            board,
        }
    }

    fn selected_prg_bank(&self) -> usize {
        self.selected_prg_bank % self.prg_bank_count
    }

    fn selected_chr_bank_0(&self) -> usize {
        self.selected_chr_bank_0 % (self.chr.len() / 0x1000)
    }

    fn selected_chr_bank_1(&self) -> usize {
        self.selected_chr_bank_1 % (self.chr.len() / 0x1000)
    }

    fn read_prg_bank(&self, bank: usize, address: u16) -> u8 {
        let bank_start = bank * PRG_ROM_BANK_SIZE;
        let offset = (address as usize) % PRG_ROM_BANK_SIZE;
        self.prg_rom[bank_start + offset]
    }

}

impl Cartridge for Mapper34 {
    fn write_chr(&mut self, address: u16, value: u8) {
        if self.board == Board::BxRom && self.has_chr_ram {
            let index = address as usize % self.chr.len();
            self.chr[index] = value;
        }
    }

    fn read_chr(&self, address: u16) -> u8 {
        match self.board {
            Board::BxRom => self.chr[address as usize % self.chr.len()],
            Board::Nina001 => match address {
                0x0000..=0x0fff => {
                    let bank_start = self.selected_chr_bank_0() * 0x1000;
                    self.chr[bank_start + address as usize]
                }
                0x1000..=0x1fff => {
                    let bank_start = self.selected_chr_bank_1() * 0x1000;
                    self.chr[bank_start + (address as usize & 0x0fff)]
                }
                _ => unreachable!(),
            },
        }
    }

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
                        0x7ffe => self.selected_chr_bank_0 = (value & 0x0f) as usize,
                        0x7fff => self.selected_chr_bank_1 = (value & 0x0f) as usize,
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

        let mut mapper = Mapper34::new(&prg_rom, &[]);

        assert_eq!(mapper.read(0x8000), 0x10);

        mapper.write(0x8000, 0x02);
        assert_eq!(mapper.read(0x8000), 0x30);

        mapper.write(0xffff, 0x03);
        assert_eq!(mapper.read(0x8000), 0x40);
    }

    #[test]
    fn writes_to_chr_ram_when_chr_rom_is_absent() {
        let mut mapper = Mapper34::new(&[0; PRG_ROM_BANK_SIZE], &[]);

        mapper.write_chr(0x0000, 0x12);
        mapper.write_chr(0x1fff, 0x34);

        assert_eq!(mapper.read_chr(0), 0x12);
        assert_eq!(mapper.read_chr(0x1fff), 0x34);
    }

    #[test]
    fn nina001_banks_chr_with_4k_registers() {
        let mut prg_rom = vec![0u8; PRG_ROM_BANK_SIZE * 2];
        prg_rom[0x0000] = 0x11;
        prg_rom[0x8000] = 0x22;

        let mut chr_rom = vec![0u8; CHR_SIZE * 4];
        chr_rom[0x0000] = 0xa1;
        chr_rom[0x1000] = 0xb1;
        chr_rom[0x2000] = 0xa2;
        chr_rom[0x3000] = 0xb2;
        chr_rom[0x6000] = 0xa4;
        chr_rom[0x7000] = 0xb4;

        let mut mapper = Mapper34::new(&prg_rom, &chr_rom);

        mapper.write(0x7ffd, 0x01);
        mapper.write(0x7ffe, 0x02);
        mapper.write(0x7fff, 0x03);

        assert_eq!(mapper.read_chr(0x0000), 0xa2);
        assert_eq!(mapper.read_chr(0x1000), 0xb2);
        assert_eq!(mapper.read(0x8000), 0x22);
        assert_eq!(mapper.read(0x7ffd), 0x01);
        assert_eq!(mapper.read(0x7ffe), 0x02);
        assert_eq!(mapper.read(0x7fff), 0x03);
    }
}
