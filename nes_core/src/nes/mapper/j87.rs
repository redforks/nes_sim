use super::{Cartridge, CartridgeOperation};

pub struct J87 {
    prg_rom: [u8; 32768],
    is_16k_prg_rom: bool,
    chr_bands: [[u8; 0x2000]; 4],
    chr_active_band: u8,
}

impl J87 {
    pub fn new(prg_rom: &[u8], prg_len: usize, chr_rom: &[u8]) -> Self {
        assert!(prg_len == (32 * 1024) || prg_len == (16 * 1024));
        let mut r = Self {
            prg_rom: [0; 32768],
            is_16k_prg_rom: prg_len == 16 * 1024,
            chr_bands: [[0u8; 0x2000]; 4],
            chr_active_band: 0,
        };
        r.prg_rom[..prg_rom.len()].copy_from_slice(prg_rom);
        match chr_rom.len() {
            0x4000 => {
                r.chr_bands[0].copy_from_slice(&chr_rom[0..0x2000]);
                r.chr_bands[1].copy_from_slice(&chr_rom[0x2000..0x4000]);
            }
            0x8000 => {
                r.chr_bands[0].copy_from_slice(&chr_rom[0..0x2000]);
                r.chr_bands[1].copy_from_slice(&chr_rom[0x2000..0x4000]);
                r.chr_bands[2].copy_from_slice(&chr_rom[0x4000..0x6000]);
                r.chr_bands[3].copy_from_slice(&chr_rom[0x6000..0x8000]);
            }
            _ => {}
        }
        r
    }
}

impl Cartridge for J87 {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x8000..=0xffff => {
                let mut offset = (address - 0x8000) as usize;
                if self.is_16k_prg_rom {
                    offset %= 0x4000;
                };
                self.prg_rom[offset]
            }
            _ => unreachable!(),
        }
    }

    fn write(&mut self, address: u16, value: u8) -> CartridgeOperation {
        match address {
            0x6000..=0xffff => {
                let mut r = (value & 0x01) << 1;
                r |= (value & 0x02) >> 1;
                self.chr_active_band = r;
            }
            _ => unreachable!(),
        }
        CartridgeOperation::None
    }

    fn read_chr(&self, address: u16) -> u8 {
        self.chr_bands[self.chr_active_band as usize][address as usize % 0x2000]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        self.chr_bands[self.chr_active_band as usize][address as usize % 0x2000] = value;
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
        let prg = vec![0u8; 32768];
        let mut mapper = J87::new(&prg, 32768, &data);
        assert_eq!(mapper.read_chr(0), 0x10);
        mapper.write(0x6000, 0x02);
        assert_eq!(mapper.read_chr(0), 0x20);
        mapper.write(0x6000, 0x01);
        assert_eq!(mapper.read_chr(0), 0x30);
        mapper.write(0x6000, 0x03);
        assert_eq!(mapper.read_chr(0), 0x40);
    }
}
