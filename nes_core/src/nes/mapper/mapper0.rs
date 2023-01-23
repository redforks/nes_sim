use super::CARTRIRIDGE_START_ADDR;
use crate::mcu::Mcu;
use crate::nes::mapper::Cartridge;

pub struct Mapper0 {
    prg_rom: [u8; 0x8000],
    chr_rom: [u8; 0x2000],
    ram: [u8; 0x4000 - 0x20],
}

impl Default for Mapper0 {
    fn default() -> Self {
        Self {
            prg_rom: [0; 0x8000],
            chr_rom: [0; 0x2000],
            ram: [0; 0x4000 - 0x20],
        }
    }
}

impl Mcu for Mapper0 {
    fn read(&self, address: u16) -> u8 {
        match address {
            CARTRIRIDGE_START_ADDR..=0x7fff => {
                self.ram[(address - CARTRIRIDGE_START_ADDR) as usize]
            }
            0x8000..=0xffff => self.prg_rom[(address - 0x8000) as usize],
            _ => unreachable!(),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            CARTRIRIDGE_START_ADDR..=0x7fff => {
                self.ram[(address - CARTRIRIDGE_START_ADDR) as usize] = value
            }
            0x8000..=0xffff => {
                // ignore write to rom
            }
            _ => unreachable!(),
        }
    }
}

impl Cartridge for Mapper0 {
    fn pattern_ref(&self) -> &[u8] {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nes::mapper::CARTRIRIDGE_START_ADDR;

    #[test]
    fn mcu() {
        let mut mcu = Mapper0::default();

        // read-write ram
        mcu.write(CARTRIRIDGE_START_ADDR, 0x01);
        assert_eq!(mcu.read(CARTRIRIDGE_START_ADDR), 0x01);
        assert_eq!(mcu.ram[0], 0x01);
        mcu.write(0x7fff, 0x03);
        assert_eq!(mcu.read(0x7fff), 0x03);

        mcu.prg_rom[0] = 0x02;
        // read-write rom
        assert_eq!(mcu.read(0x8000), 0x02);
        // ignore write to rom
        mcu.write(0x8000, 0x03);
        assert_eq!(mcu.prg_rom[0], 0x02);
    }
}
