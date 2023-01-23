use super::Cartridge;
use crate::nes::ppu::{Mirroring, Ppu};
use crate::to_from_u8;
use log::debug;
use modular_bitfield::prelude::*;

#[derive(Clone, Copy)]
#[bitfield]
struct ControlFlags {
    #[skip]
    _not_used: B3,
    pub chr_in_4k: bool,
    pub prg_mode: B2,
    pub mirroring: B2,
}
to_from_u8!(ControlFlags);

impl From<ControlFlags> for Mirroring {
    fn from(value: ControlFlags) -> Self {
        match value.mirroring() {
            0b00 => Mirroring::LowerBank,
            0b01 => Mirroring::UpperBank,
            0b10 => Mirroring::Vertical,
            0b11 => Mirroring::Horizontal,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum PrgRomMode {
    Switch32K,
    FixFirst16K,
    FixLast16K,
}

impl From<ControlFlags> for PrgRomMode {
    fn from(value: ControlFlags) -> Self {
        match value.prg_mode() {
            0b00 => PrgRomMode::Switch32K,
            0b01 => PrgRomMode::Switch32K,
            0b10 => PrgRomMode::FixFirst16K,
            0b11 => PrgRomMode::FixLast16K,
            _ => unreachable!(),
        }
    }
}

pub struct MMC1 {
    chr_rom: Vec<u8>,
    cur_chr_rom: Vec<u8>,

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
        // &self.cur_chr_rom
        todo!()
    }

    fn read(&self, address: u16) -> u8 {
        match address {
            0x4020..=0x5fff => 0,
            0x6000..=0x7fff => self.prg_ram[address as usize - 0x6000],
            0x8000..=0xbfff => self.prg_rom[address as usize - 0x8000 + self.prg_rom_bank1_start],
            0xc000..=0xffff => self.prg_rom[address as usize - 0xc000 + self.prg_rom_bank2_start],
            _ => panic!("read address out of range: {:04x}", address),
        }
    }

    fn write(&mut self, ppu: &mut Ppu, address: u16, value: u8) {
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
                        0x8000..=0x9fff => self.control(ppu, self.sr.into()),
                        0xa000..=0xbfff => self.chr_bank1(self.sr),
                        0xc000..=0xdfff => self.chr_bank1(self.sr),
                        0xe000..=0xffff => self.prg_bank(self.sr),
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

const PRG_ROM_BANK_SIZE: usize = 0x4000;

impl MMC1 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8]) -> Self {
        let mut r = Self {
            chr_rom: chr_rom.to_vec(),
            cur_chr_rom: vec![],
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

    fn control(&mut self, ppu: &mut Ppu, flags: ControlFlags) {
        ppu.set_mirroring(flags.into());
        self.set_prg_rom_mode(flags.into());
        // todo: impl for other flags
        // // control register
        // let prg_rom_mode = match self.sr & 0x03 {
        //     0 => PrgRomMode::Switch32K,
        //     1 => PrgRomMode::FixFirst16K,
        //     2 => PrgRomMode::FixLast16K,
        //     _ => panic!("invalid prg rom mode"),
        // };
        // self.set_prg_rom_mode(prg_rom_mode);
    }

    fn chr_bank0(&mut self, _byte: u8) {
        todo!()
    }

    fn chr_bank1(&mut self, _byte: u8) {
        todo!()
    }

    fn prg_bank(&mut self, byte: u8) {
        // prg rom bank register
        let bank = byte & 0x0f;
        debug!("MMC1 switch prg rom 1st bank to ${:x}", bank);
        self.prg_rom_bank1_start = bank as usize * PRG_ROM_BANK_SIZE;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nes::ppu::Mirroring;
    use test_log::test;

    fn create<F>(mut init_prg: F) -> (MMC1, Ppu)
    where
        F: FnMut(&mut [u8]),
    {
        let mut prg_rom = [0; 128 * 1024];
        init_prg(&mut prg_rom);
        (MMC1::new(&prg_rom, &[]), Ppu::new())
    }

    #[test]
    fn prg_rom() {
        fn init_prg(prg_rom: &mut [u8]) {
            prg_rom[128 * 1024 - 1] = 0xab;
            prg_rom[0] = 0xba;
            prg_rom[PRG_ROM_BANK_SIZE] = 0xcd;
        }
        let (mut mmc1, mut ppu) = create(init_prg);

        // no effect read/write before 0x6000
        mmc1.write(&mut ppu, 0x4020, 1);
        assert_eq!(mmc1.read(0x4020), 0);

        mmc1.write(&mut ppu, 0x6000, 1);
        assert_eq!(mmc1.read(0x6000), 1);
        mmc1.write(&mut ppu, 0x7fff, 255);
        assert_eq!(mmc1.read(0x7fff), 255);

        // default is FixLast16k
        assert_eq!(mmc1.read(0x8000), 0xba);
        assert_eq!(mmc1.read(0xffff), 0xab);

        mmc1.write(&mut ppu, 0xE000, 1);
        mmc1.write(&mut ppu, 0xE000, 1);
        // reset load register, and select 2nd page to first band
        mmc1.write(&mut ppu, 0x8000, 0x80);
        mmc1.write(&mut ppu, 0xE000, 1);
        mmc1.write(&mut ppu, 0xE001, 0);
        mmc1.write(&mut ppu, 0xE002, 0);
        mmc1.write(&mut ppu, 0xE002, 0);
        mmc1.write(&mut ppu, 0xE002, 1);
        assert_eq!(mmc1.read(0x8000), 0xcd);
    }

    #[test]
    fn control_ppu_mirroring() {
        let (mut mmc1, mut ppu) = create(|_| {});
        mmc1.control(&mut ppu, ControlFlags::new().with_mirroring(0));
        assert_eq!(ppu.mirroring(), Mirroring::LowerBank);
    }

    #[test]
    fn control_flags_to_mirroring() {
        fn to_mirroring(v: u8) -> Mirroring {
            ControlFlags::new().with_mirroring(v).into()
        }
        assert_eq!(Mirroring::LowerBank, to_mirroring(0));
        assert_eq!(Mirroring::UpperBank, to_mirroring(1));
        assert_eq!(Mirroring::Vertical, to_mirroring(2));
        assert_eq!(Mirroring::Horizontal, to_mirroring(3));
    }

    #[test]
    fn control_flags_to_prg_rom_mode() {
        fn to_mode(v: u8) -> PrgRomMode {
            ControlFlags::new().with_prg_mode(v).into()
        }
        assert_eq!(PrgRomMode::Switch32K, to_mode(0));
        assert_eq!(PrgRomMode::Switch32K, to_mode(1));
        assert_eq!(PrgRomMode::FixFirst16K, to_mode(2));
        assert_eq!(PrgRomMode::FixLast16K, to_mode(3));
    }

    #[test]
    fn control_prg_rom_mode() {
        let (mut mmc1, mut ppu) = create(|rom| {
            rom[0] = 1;
            rom[32 * 1024 - 1] = 2;
            rom[128 * 1024 - 16 * 1024] = 3;
        });
        mmc1.control(&mut ppu, ControlFlags::new().with_prg_mode(0));
        assert_eq!(mmc1.read(0x8000), 1);
        assert_eq!(mmc1.read(0xffff), 2);

        // Fix last 16k
        mmc1.control(&mut ppu, ControlFlags::new().with_prg_mode(3));
        assert_eq!(mmc1.read(0x8000), 1);
        assert_eq!(mmc1.read(0xC000), 3);

        // Fix first 16k
        mmc1.control(&mut ppu, ControlFlags::new().with_prg_mode(1));
        assert_eq!(mmc1.read(0x8000), 1);
        assert_eq!(mmc1.read(0xffff), 2);
    }
}
