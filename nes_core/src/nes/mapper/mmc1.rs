use super::Cartridge;
use crate::mcu::Mcu;
use log::debug;

#[derive(Eq, PartialEq, Copy, Clone)]
enum PrgRomMode {
    Switch32K,
    FixFirst16K,
    FixLast16K,
}

pub struct MMC1 {
    prg_rom: Vec<u8>,
    prg_ram: [u8; 0x2000],
    prg_rom_bank1_start: usize,
    prg_rom_bank2_start: usize,

    sr: u8, // shift register
    sr_write_count: u8,
    prg_rom_mode: PrgRomMode,
}

impl Cartridge for MMC1 {
    fn pattern_ref(&self) -> &[u8] {
        todo!()
    }
}

const PRG_ROM_BANK_SIZE: usize = 0x4000;

impl MMC1 {
    pub fn new(prg_rom: &[u8]) -> Self {
        let mut r = Self {
            prg_rom: prg_rom.to_vec(),
            prg_ram: [0; 0x2000],
            prg_rom_bank1_start: 0,
            prg_rom_bank2_start: 0,
            sr: 0,
            sr_write_count: 0,
            prg_rom_mode: PrgRomMode::Switch32K,
        };

        r.reset();
        r
    }

    fn reset(&mut self) {
        self.sr = 0;
        self.sr_write_count = 0;
        self.set_prg_rom_mode(PrgRomMode::FixLast16K);
    }

    fn set_prg_rom_mode(&mut self, mode: PrgRomMode) {
        // return if mode == last mode
        if mode == self.prg_rom_mode {
            return;
        }
        self.prg_rom_mode = mode;

        match mode {
            PrgRomMode::Switch32K | PrgRomMode::FixFirst16K => {
                self.prg_rom_bank1_start = 0;
                self.prg_rom_bank2_start = PRG_ROM_BANK_SIZE;
            }
            PrgRomMode::FixLast16K => {
                self.prg_rom_bank1_start = 0;
                self.prg_rom_bank2_start = self.prg_rom.len() - PRG_ROM_BANK_SIZE;
            }
        }
    }
}

impl Mcu for MMC1 {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x4020..=0x5fff => 0,
            0x6000..=0x7fff => self.prg_ram[address as usize - 0x6000],
            0x8000..=0xbfff => self.prg_rom[address as usize - 0x8000 + self.prg_rom_bank1_start],
            0xc000..=0xffff => self.prg_rom[address as usize - 0xc000 + self.prg_rom_bank2_start],
            _ => panic!("read address out of range: {:04x}", address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        debug!("write address: {:04x}, value: {:02x}", address, value);
        match address {
            0x4020..=0x5fff => {}
            0x6000..=0x7fff => self.prg_ram[address as usize - 0x6000] = value,
            _ => {
                // reset if value high bit is 1
                if value & 0x80 != 0 {
                    debug!("{:x} written to ${:x} reset MMC1", value, address);
                    self.reset();
                    return;
                }

                if self.sr_write_count < 5 {
                    self.sr_write_count += 1;
                    self.sr <<= 1;
                    self.sr |= value & 0x01;
                }

                if self.sr_write_count == 5 {
                    match address {
                        // 0x8000..=0x9fff => {
                        //     // // control register
                        //     // let prg_rom_mode = match self.sr & 0x03 {
                        //     //     0 => PrgRomMode::Switch32K,
                        //     //     1 => PrgRomMode::FixFirst16K,
                        //     //     2 => PrgRomMode::FixLast16K,
                        //     //     _ => panic!("invalid prg rom mode"),
                        //     // };
                        //     // self.set_prg_rom_mode(prg_rom_mode);
                        // }
                        // 0xa000..=0xbfff => {}
                        // 0xc000..=0xdfff => {
                        //     // chr rom bank register
                        // }
                        0xe000..=0xffff => {
                            // prg rom bank register
                            let bank = self.sr & 0x0f;
                            debug!("MMC1 switch prg rom 1st bank to ${:x}", bank);
                            self.prg_rom_bank1_start = bank as usize * PRG_ROM_BANK_SIZE;
                        }
                        _ => panic!(
                            "write address out of range or unimplemented: ${:04x} {:x}",
                            address, self.sr,
                        ),
                    }
                    self.sr_write_count = 0;
                    self.sr = 0;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use log::info;
    use test_log::test;

    #[test]
    fn test() {
        info!("test");
        let mut prg_rom = [0; 128 * 1024];
        prg_rom[128 * 1024 - 1] = 0xab;
        prg_rom[0] = 0xba;
        prg_rom[PRG_ROM_BANK_SIZE] = 0xcd;
        let mut mmc1 = MMC1::new(&prg_rom);

        // no effect read/write before 0x6000
        mmc1.write(0x4020, 1);
        assert_eq!(mmc1.read(0x4020), 0);

        mmc1.write(0x6000, 1);
        assert_eq!(mmc1.read(0x6000), 1);
        mmc1.write(0x7fff, 255);
        assert_eq!(mmc1.read(0x7fff), 255);

        // default is FixLast16k
        assert_eq!(mmc1.read(0x8000), 0xba);
        assert_eq!(mmc1.read(0xffff), 0xab);

        mmc1.write(0xE000, 1);
        mmc1.write(0xE000, 1);
        // reset load register, and select 2nd page to first band
        mmc1.write(0x8000, 0x80);
        mmc1.write(0xE000, 1);
        mmc1.write(0xE001, 0);
        mmc1.write(0xE002, 0);
        mmc1.write(0xE002, 0);
        mmc1.write(0xE002, 1);
        assert_eq!(mmc1.read(0x8000), 0xcd);
        // mmc1.write(0x8000, 1);
        // mmc1.write(0xc000, 1);
        // mmc1.write(0xffff, 1);
        // assert_eq!(mmc1.read(0x8000), 0);
        // assert_eq!(mmc1.read(0xc000), 0);
        // assert_eq!(mmc1.read(0xffff), 0);
    }
}
