use super::{Cartridge, CartridgeOperation, ChrStorage};

pub struct MapperJ87 {
    prg_rom: [u8; 32768],
    is_16k_prg_rom: bool,
}

impl MapperJ87 {
    pub fn new(prg_rom: &[u8], prg_len: usize) -> Self {
        assert!(prg_len == (32 * 1024) || prg_len == (16 * 1024));
        let mut r = Self {
            prg_rom: [0; 32768],
            is_16k_prg_rom: prg_len == 16 * 1024,
        };
        r.prg_rom[..prg_rom.len()].copy_from_slice(prg_rom);
        r
    }
}

impl Cartridge for MapperJ87 {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x8000..=0xffff => {
                let mut offset = (address - 0x8000) as usize;
                if self.is_16k_prg_rom {
                    offset = offset % 0x4000;
                };
                self.prg_rom[offset]
            }
            _ => unreachable!(),
        }
    }

    fn write(&mut self, address: u16, _value: u8) -> CartridgeOperation {
        match address {
            0x6000..=0xffff => {}
            _ => unreachable!(),
        }
        CartridgeOperation::None
    }
}

pub struct J87ChrStorage {
    bands: [[u8; 0x2000]; 4],
    active_band: u8,
}

impl J87ChrStorage {
    pub fn new(chr_rom: &[u8]) -> Self {
        let mut bands = [[0u8; 0x2000]; 4];
        match chr_rom.len() {
            0x4000 => {
                bands[0].copy_from_slice(&chr_rom[0..0x2000]);
                bands[1].copy_from_slice(&chr_rom[0x2000..0x4000]);
            }
            0x8000 => {
                bands[0].copy_from_slice(&chr_rom[0..0x2000]);
                bands[1].copy_from_slice(&chr_rom[0x2000..0x4000]);
                bands[2].copy_from_slice(&chr_rom[0x4000..0x6000]);
                bands[3].copy_from_slice(&chr_rom[0x6000..0x8000]);
            }
            _ => {}
        }
        Self {
            bands,
            active_band: 0,
        }
    }
}

impl ChrStorage for J87ChrStorage {
    fn read_chr(&self, address: u16) -> u8 {
        self.bands[self.active_band as usize][address as usize % 0x2000]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        self.bands[self.active_band as usize][address as usize % 0x2000] = value;
    }

    fn write_register(&mut self, _addr: u16, value: u8) {
        let mut r = (value & 0x01) << 1;
        r |= (value & 0x02) >> 1;
        self.active_band = r;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chr_band_switching() {
        let mut data = vec![0u8; 0x8000];
        data[0x0000] = 0x10;
        data[0x2000] = 0x20;
        data[0x4000] = 0x30;
        data[0x6000] = 0x40;
        let mut chr = J87ChrStorage::new(&data);
        assert_eq!(chr.read_chr(0), 0x10);
        chr.write_register(0x6000, 0x02);
        assert_eq!(chr.read_chr(0), 0x20);
        chr.write_register(0x6000, 0x01);
        assert_eq!(chr.read_chr(0), 0x30);
        chr.write_register(0x6000, 0x03);
        assert_eq!(chr.read_chr(0), 0x40);
    }
}
