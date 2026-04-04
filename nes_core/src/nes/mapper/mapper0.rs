use super::CARTRIDGE_START_ADDR;
use crate::nes::mapper::Mirroring;
use crate::nes::mapper::NameTableControl;

pub struct Mapper0 {
    prg_rom: [u8; 0x8000],
    prg_rom_len: usize,
    chr_rom: [u8; 0x2000],
    has_chr_ram: bool,
    ram: [u8; 0x4000 - 0x20],
    name_table: NameTableControl,
}

impl Mapper0 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8], mirroring: Mirroring) -> Self {
        debug_assert!(prg_rom.len() <= 0x8000);
        debug_assert!(chr_rom.len() <= 0x2000);

        let mut r = Self {
            prg_rom_len: prg_rom.len(),
            has_chr_ram: chr_rom.is_empty(),
            name_table: NameTableControl::new(mirroring),
            ..Self::default()
        };
        r.prg_rom[0..prg_rom.len()].copy_from_slice(prg_rom);
        r.chr_rom[0..chr_rom.len()].copy_from_slice(chr_rom);
        r
    }
}

impl Default for Mapper0 {
    fn default() -> Self {
        Self {
            prg_rom: [0; 0x8000],
            prg_rom_len: 0,
            chr_rom: [0; 0x2000],
            has_chr_ram: false,
            ram: [0; 0x4000 - 0x20],
            name_table: NameTableControl::new(Mirroring::Horizontal),
        }
    }
}

impl Mapper0 {
    pub fn pattern_ref(&self) -> &[u8] {
        &self.chr_rom
    }

    pub fn write_pattern(&mut self, address: u16, value: u8) {
        if self.has_chr_ram {
            self.chr_rom[address as usize % self.chr_rom.len()] = value;
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
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

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            CARTRIDGE_START_ADDR..=0x7fff => {
                self.ram[(address - CARTRIDGE_START_ADDR) as usize] = value
            }
            0x8000..=0xffff => {}
            _ => unreachable!(),
        }
    }

    pub fn write_nametable(&mut self, address: u16, value: u8) {
        self.name_table.write(address, value);
    }

    pub fn read_nametable(&self, address: u16) -> u8 {
        self.name_table.read(address)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nes::mapper::CARTRIDGE_START_ADDR;

    #[test]
    fn mcu() {
        let mut mcu = Mapper0::default();

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
    fn cartridge() {
        let mcu = Mapper0::new(&[0; 0], &[1, 2], Mirroring::Horizontal);
        assert_eq!(1, mcu.pattern_ref()[0]);
        assert_eq!(2, mcu.pattern_ref()[1]);
    }

    #[test]
    fn writes_to_chr_ram_when_chr_rom_is_absent() {
        let mut mcu = Mapper0::new(&[0; 0], &[], Mirroring::Horizontal);

        mcu.write_pattern(0x0000, 0x12);
        mcu.write_pattern(0x1fff, 0x34);

        assert_eq!(mcu.pattern_ref()[0], 0x12);
        assert_eq!(mcu.pattern_ref()[0x1fff], 0x34);
    }

    #[test]
    fn mirrors_16k_prg_into_upper_bank() {
        let mut prg = [0u8; 0x4000];
        prg[0x3ffc] = 0x00;
        prg[0x3ffd] = 0x80;

        let mut mcu = Mapper0::new(&prg, &[], Mirroring::Horizontal);

        assert_eq!(mcu.read(0xfffc), 0x00);
        assert_eq!(mcu.read(0xfffd), 0x80);
    }
}
