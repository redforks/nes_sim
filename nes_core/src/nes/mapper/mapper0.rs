use super::CARTRIDGE_START_ADDR;
use crate::nes::mapper::Cartridge;
use crate::nes::ppu::Ppu;
use std::cell::RefCell;

pub struct Mapper0 {
    prg_rom: [u8; 0x8000],
    chr_rom: [u8; 0x2000],
    ram: RefCell<[u8; 0x4000 - 0x20]>,
}

impl Mapper0 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8]) -> Self {
        debug_assert!(prg_rom.len() <= 0x8000);
        debug_assert!(chr_rom.len() <= 0x2000);

        let mut r = Self::default();
        r.prg_rom[0..prg_rom.len()].copy_from_slice(prg_rom);
        r.chr_rom[0..chr_rom.len()].copy_from_slice(chr_rom);
        r
    }
}

impl Default for Mapper0 {
    fn default() -> Self {
        Self {
            prg_rom: [0; 0x8000],
            chr_rom: [0; 0x2000],
            ram: RefCell::new([0; 0x4000 - 0x20]),
        }
    }
}

impl Cartridge for Mapper0 {
    fn pattern_ref(&self) -> &[u8] {
        &self.chr_rom
    }

    fn read(&self, address: u16) -> u8 {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => {
                self.ram.borrow()[(address - CARTRIDGE_START_ADDR) as usize]
            }
            0x8000..=0xffff => self.prg_rom[(address - 0x8000) as usize],
            _ => unreachable!(),
        }
    }

    fn write(&self, _ppu: &Ppu, address: u16, value: u8) {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => {
                self.ram.borrow_mut()[(address - CARTRIDGE_START_ADDR) as usize] = value
            }
            0x8000..=0xffff => {
                // ignore write to rom
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nes::mapper::CARTRIDGE_START_ADDR;

    #[test]
    fn mcu() {
        let mcu = Mapper0::default();
        let ppu = Ppu::default();

        // read-write ram
        mcu.write(&ppu, CARTRIDGE_START_ADDR, 0x01);
        assert_eq!(mcu.read(CARTRIDGE_START_ADDR), 0x01);
        assert_eq!(mcu.ram.borrow()[0], 0x01);
        mcu.write(&ppu, 0x7fff, 0x03);
        assert_eq!(mcu.read(0x7fff), 0x03);

        // read-write rom
        assert_eq!(mcu.read(0x8000), 0);
        // ignore write to rom
        mcu.write(&ppu, 0x8000, 0x03);
        assert_eq!(mcu.read(0x8000), 0);
    }

    #[test]
    fn cartridge() {
        let mcu = Mapper0::new(&[0; 0], &[1, 2]);
        assert_eq!(1, mcu.pattern_ref()[0]);
        assert_eq!(2, mcu.pattern_ref()[1]);
    }
}
