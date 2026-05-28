use super::CartridgeOperation;

pub struct MapperJ87 {
    prg_rom: [u8; 32768],
    is_16k_prg_rom: bool,
    chr_rom_bands: [[u8; 8192]; 4],
    cur_chr_band: u8,
}

impl MapperJ87 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8]) -> Self {
        assert!(prg_rom.len() == (32 * 1024) || prg_rom.len() == (16 * 1024));
        let mut r = Self {
            prg_rom: [0; 32768],
            is_16k_prg_rom: prg_rom.len() == 16 * 1024,
            chr_rom_bands: [[0; 8192]; 4],
            cur_chr_band: 0,
        };
        r.prg_rom[..prg_rom.len()].copy_from_slice(prg_rom);

        match chr_rom.len() {
            16384 => {
                r.chr_rom_bands[0].copy_from_slice(&chr_rom[0..8192]);
                r.chr_rom_bands[1].copy_from_slice(&chr_rom[8192..16384]);
            }
            32768 => {
                r.chr_rom_bands[0].copy_from_slice(&chr_rom[0..8192]);
                r.chr_rom_bands[1].copy_from_slice(&chr_rom[8192..16384]);
                r.chr_rom_bands[2].copy_from_slice(&chr_rom[16384..24576]);
                r.chr_rom_bands[3].copy_from_slice(&chr_rom[24576..32768]);
            }
            _ => {
                panic!("Unsupported chr_rom size for MapperJ87");
            }
        }

        r
    }

    pub fn pattern_ref(&self) -> &[u8] {
        &self.chr_rom_bands[self.cur_chr_band as usize]
    }

    pub fn read(&mut self, address: u16) -> u8 {
        self.peek(address)
    }

    pub fn peek(&self, address: u16) -> u8 {
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

    pub fn write(&mut self, address: u16, value: u8) -> CartridgeOperation {
        match address {
            0x6000..=0xffff => {
                self.cur_chr_band = extract_band_selector_value(value);
            }
            _ => unreachable!(),
        }
        CartridgeOperation::None
    }

    pub fn write_pattern(&mut self, address: u16, value: u8) {
        self.chr_rom_bands[self.cur_chr_band as usize][address as usize] = value;
    }
}

fn extract_band_selector_value(value: u8) -> u8 {
    // 7  bit  0
    // ---- ----
    // xxxx xxLH
    //        ||
    //        |+- High CHR bit
    //        +-- Low CHR bit

    let mut r = (value & 0x01) << 1;
    r |= (value & 0x02) >> 1;
    r
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_band_selector_value() {
        assert_eq!(extract_band_selector_value(0x00), 0);
        assert_eq!(extract_band_selector_value(0x02), 1);
        assert_eq!(extract_band_selector_value(0x01), 2);
        assert_eq!(extract_band_selector_value(0x03), 3);
    }
}
