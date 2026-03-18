#![allow(dead_code)]

use super::Cartridge;
use crate::nes::ppu::{Mirroring, Ppu};
use crate::to_from_u8;
use log::{debug, info};
use modular_bitfield::prelude::*;
use std::cell::{RefCell, UnsafeCell};

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
    cur_chr_rom: UnsafeCell<[u8; 8 * 1024]>,
    chr_bank_size: RefCell<u16>,

    prg_rom: Vec<u8>,
    prg_ram: RefCell<[u8; 0x2000]>,
    prg_rom_bank1_start: RefCell<usize>,
    prg_rom_bank2_start: RefCell<usize>,
    prg_bank_register: RefCell<u8>, // Store the PRG bank register value

    sr: RefCell<u8>, // shift register
    sr_write_count: RefCell<u8>,
    prg_rom_mode: RefCell<PrgRomMode>,
}

impl Cartridge for MMC1 {
    fn pattern_ref(&self) -> &[u8] {
        unsafe { &*self.cur_chr_rom.get() }
    }

    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x4020..=0x5fff => 0,
            0x6000..=0x7fff => self.prg_ram.borrow()[address as usize - 0x6000],
            0x8000..=0xbfff => {
                let bank1_start = *self.prg_rom_bank1_start.borrow();
                self.prg_rom[address as usize - 0x8000 + bank1_start]
            }
            0xc000..=0xffff => {
                let bank2_start = *self.prg_rom_bank2_start.borrow();
                self.prg_rom[address as usize - 0xc000 + bank2_start]
            }
            _ => panic!("read address out of range: {:04x}", address),
        }
    }

    fn write(&mut self, ppu: &mut Ppu, address: u16, value: u8) {
        match address {
            0x4020..=0x5fff => {}
            0x6000..=0x7fff => self.prg_ram.borrow_mut()[address as usize - 0x6000] = value,
            _ => {
                // reset if value high bit is 1
                if value & 0x80 != 0 {
                    debug!("{:x} written to ${:x} reset MMC1", value, address);
                    self.reset();
                    return;
                }

                let mut sr_write_count = self.sr_write_count.borrow_mut();
                let mut sr = self.sr.borrow_mut();

                if *sr_write_count < 5 {
                    *sr_write_count += 1;
                    *sr <<= 1;
                    *sr |= value & 0x01;
                }

                if *sr_write_count == 5 {
                    let sr_value = *sr;
                    drop(sr);
                    drop(sr_write_count);

                    match address {
                        0x8000..=0x9fff => self.control(ppu, sr_value.into()),
                        0xa000..=0xbfff => self.select_chr_bank1(sr_value),
                        0xc000..=0xdfff => self.select_chr_bank2(sr_value),
                        0xe000..=0xffff => self.select_prg_bank(sr_value),
                        _ => panic!(
                            "write address out of range or unimplemented: ${:04x} {:x}",
                            address, sr_value,
                        ),
                    }

                    *self.sr_write_count.borrow_mut() = 0;
                    *self.sr.borrow_mut() = 0;
                }
            }
        }
    }
}

const PRG_ROM_BANK_SIZE: usize = 0x4000;

impl MMC1 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8]) -> Self {
        let r = Self {
            chr_rom: chr_rom.to_vec(),
            cur_chr_rom: UnsafeCell::new([0; 1024 * 8]),
            chr_bank_size: RefCell::new(4096),
            prg_rom: prg_rom.to_vec(),
            prg_ram: RefCell::new([0; 0x2000]),
            prg_rom_bank1_start: RefCell::new(0),
            prg_rom_bank2_start: RefCell::new(0),
            prg_bank_register: RefCell::new(0),
            sr: RefCell::new(0),
            sr_write_count: RefCell::new(0),
            prg_rom_mode: RefCell::new(PrgRomMode::Switch32K),
        };

        r.reset();
        r
    }

    fn reset(&self) {
        *self.sr.borrow_mut() = 0;
        *self.sr_write_count.borrow_mut() = 0;
        *self.chr_bank_size.borrow_mut() = 4096;
        *self.prg_bank_register.borrow_mut() = 0;
        self.set_prg_rom_mode(PrgRomMode::FixLast16K);
        self.select_chr_bank1(0);
        self.select_chr_bank2(1);
    }

    fn set_prg_rom_mode(&self, mode: PrgRomMode) {
        // return if mode == last mode
        let prg_rom_mode = self.prg_rom_mode.borrow();
        if mode == *prg_rom_mode {
            return;
        }
        drop(prg_rom_mode);

        *self.prg_rom_mode.borrow_mut() = mode;
        // Update bank pointers based on new mode and current bank register
        self.update_prg_banks();
    }

    fn update_prg_banks(&self) {
        let bank = *self.prg_bank_register.borrow() as usize;
        let prg_rom_mode = *self.prg_rom_mode.borrow();
        match prg_rom_mode {
            PrgRomMode::Switch32K => {
                // In 32K mode, bank register selects a 32K bank (bit 3 is ignored)
                // $8000-$BFFF: bank * 16K
                // $C000-$FFFF: (bank + 1) * 16K
                let bank32k = bank & 0x07;
                *self.prg_rom_bank1_start.borrow_mut() = bank32k * PRG_ROM_BANK_SIZE;
                *self.prg_rom_bank2_start.borrow_mut() =
                    *self.prg_rom_bank1_start.borrow() + PRG_ROM_BANK_SIZE;
            }
            PrgRomMode::FixFirst16K => {
                // $8000-$BFFF: fixed to first 16K
                // $C000-$FFFF: bank * 16K
                *self.prg_rom_bank1_start.borrow_mut() = 0;
                *self.prg_rom_bank2_start.borrow_mut() = bank * PRG_ROM_BANK_SIZE;
            }
            PrgRomMode::FixLast16K => {
                // $8000-$BFFF: bank * 16K
                // $C000-$FFFF: fixed to last 16K
                *self.prg_rom_bank1_start.borrow_mut() = bank * PRG_ROM_BANK_SIZE;
                *self.prg_rom_bank2_start.borrow_mut() = self.prg_rom.len() - PRG_ROM_BANK_SIZE;
            }
        }
    }

    fn set_chr_bank_mode(&self, is_4k: bool) {
        *self.chr_bank_size.borrow_mut() = if is_4k { 4096 } else { 8192 };
    }

    fn control(&self, ppu: &mut Ppu, flags: ControlFlags) {
        ppu.set_mirroring(flags.into());
        self.set_prg_rom_mode(flags.into());
        self.set_chr_bank_mode(flags.chr_in_4k());
    }

    fn select_chr_bank1(&self, offset: u8) {
        let offset = offset as usize;
        let bank_size = *self.chr_bank_size.borrow() as usize;
        if (offset + 1) * bank_size >= self.chr_rom.len() {
            return;
        }
        unsafe {
            let cur_chr = &mut *self.cur_chr_rom.get();
            cur_chr[0..bank_size]
                .copy_from_slice(&self.chr_rom[offset * bank_size..(offset + 1) * bank_size])
        }
    }

    fn select_chr_bank2(&self, offset: u8) {
        if *self.chr_bank_size.borrow() != 4096 {
            return;
        }
        let offset = offset as usize;
        if (offset + 1) * 4096 >= self.chr_rom.len() {
            return;
        }

        unsafe {
            let cur_chr = &mut *self.cur_chr_rom.get();
            cur_chr[4096..].copy_from_slice(&self.chr_rom[offset * 4096..(offset + 1) * 4096])
        }
    }

    fn select_prg_bank(&self, byte: u8) {
        // PRG ROM bank register (bits 0-3)
        // bit 4 is ignored
        *self.prg_bank_register.borrow_mut() = byte & 0x0f;
        info!("MMC1 switch prg rom bank to ${:x}", byte & 0x0f);
        self.update_prg_banks();
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
        (MMC1::new(&prg_rom, &[]), Ppu::default())
    }

    fn create_with_chr<F>(mut init_chr: F) -> (MMC1, Ppu)
    where
        F: FnMut(&mut [u8]),
    {
        let mut chr_rom = [0; 128 * 1024];
        init_chr(&mut chr_rom);
        (MMC1::new(&[0; 32 * 1024], &chr_rom), Ppu::default())
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

    #[test]
    fn control_chr_bank_size() {
        let (mut mmc1, mut ppu) = create(|_| {});

        // default is 4k
        assert_eq!(*mmc1.chr_bank_size.borrow(), 4 * 1024);

        // set to 8k
        mmc1.control(&mut ppu, ControlFlags::new().with_chr_in_4k(false));
        assert_eq!(*mmc1.chr_bank_size.borrow(), 8 * 1024);

        // set to 4k
        mmc1.control(&mut ppu, ControlFlags::new().with_chr_in_4k(true));
        assert_eq!(*mmc1.chr_bank_size.borrow(), 4 * 1024);
    }

    #[test]
    fn select_chr_bank1() {
        let (mmc1, _ppu) = create_with_chr(|rom| {
            rom[0] = 1;
            rom[8 * 1024] = 2;
            rom[4 * 1024] = 3;
        });
        assert_eq!(*mmc1.chr_bank_size.borrow(), 4 * 1024);

        // default select first 4k
        assert_eq!(unsafe { &*mmc1.cur_chr_rom.get() }[0x0000], 1);

        // set bank 2
        mmc1.select_chr_bank1(2);
        assert_eq!(unsafe { &*mmc1.cur_chr_rom.get() }[0x0000], 2);

        // switch to 8k mode
        mmc1.set_chr_bank_mode(false);
        assert_eq!(*mmc1.chr_bank_size.borrow(), 8 * 1024);
        mmc1.select_chr_bank1(0);
        assert_eq!(unsafe { &*mmc1.cur_chr_rom.get() }[0x0000], 1);
        assert_eq!(unsafe { &*mmc1.cur_chr_rom.get() }[4096], 3);
    }

    #[test]
    fn select_chr_bank2() {
        let (mmc1, _ppu) = create_with_chr(|rom| {
            rom[0] = 1;
            rom[4096] = 2;
            rom[8 * 1024] = 3;
        });
        assert_eq!(*mmc1.chr_bank_size.borrow(), 4 * 1024);

        // default select 2nd 4k
        assert_eq!(unsafe { &*mmc1.cur_chr_rom.get() }[4096], 2);

        // set bank 2
        mmc1.select_chr_bank2(2);
        assert_eq!(unsafe { &*mmc1.cur_chr_rom.get() }[4096], 3);

        // ignored in 8k mode
        mmc1.set_chr_bank_mode(false);
        assert_eq!(*mmc1.chr_bank_size.borrow(), 8 * 1024);
        mmc1.select_chr_bank2(0);
        assert_eq!(unsafe { &*mmc1.cur_chr_rom.get() }[4096], 3);
    }
}
