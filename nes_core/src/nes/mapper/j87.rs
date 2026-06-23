use super::{Cartridge, CartridgeOperation};

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
    fn read(&mut self, address: u16) -> u8 {
        self.peek(address)
    }

    fn peek(&self, address: u16) -> u8 {
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
