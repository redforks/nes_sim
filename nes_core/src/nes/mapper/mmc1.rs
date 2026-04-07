#![allow(dead_code)]

use crate::nes::mapper::Mirroring;
use crate::nes::mapper::NameTableControl;
use bitfield_struct::bitfield;

const CHR_WINDOW_SIZE: usize = 0x2000;
const CHR_BANK_SIZE_4K: usize = 0x1000;
const PRG_ROM_BANK_SIZE: usize = 0x4000;
const PRG_RAM_SIZE: usize = 0x2000;
const INITIAL_SHIFT_REGISTER: u8 = 0x10;

#[bitfield(u8)]
struct ControlFlags {
    // bitfield_struct field order is LSB-first.
    #[bits(2)]
    pub mirroring: u8,
    #[bits(2)]
    pub prg_mode: u8,
    pub chr_in_4k: bool,
    #[bits(3)]
    _not_used: u8,
}

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
            0b00 | 0b01 => PrgRomMode::Switch32K,
            0b10 => PrgRomMode::FixFirst16K,
            0b11 => PrgRomMode::FixLast16K,
            _ => unreachable!(),
        }
    }
}

pub struct MMC1 {
    chr: Vec<u8>,
    chr_window: [u8; CHR_WINDOW_SIZE],
    has_chr_ram: bool,

    prg_rom: Vec<u8>,
    prg_ram: [u8; PRG_RAM_SIZE],

    control: ControlFlags,
    chr_bank0: u8,
    chr_bank1: u8,
    prg_bank: u8,
    shift_register: u8,
    name_table: NameTableControl,
}

impl MMC1 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8], mirroring: Mirroring) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert_eq!(prg_rom.len() % PRG_ROM_BANK_SIZE, 0);

        let mut mapper = Self {
            chr: if chr_rom.is_empty() {
                vec![0; CHR_WINDOW_SIZE]
            } else {
                chr_rom.to_vec()
            },
            chr_window: [0; CHR_WINDOW_SIZE],
            has_chr_ram: chr_rom.is_empty(),
            prg_rom: prg_rom.to_vec(),
            prg_ram: [0; PRG_RAM_SIZE],
            control: ControlFlags::new().with_mirroring(Self::mirroring_bits(mirroring)),
            chr_bank0: 0,
            chr_bank1: 0,
            prg_bank: 0,
            shift_register: INITIAL_SHIFT_REGISTER,
            name_table: NameTableControl::new(mirroring),
        };

        mapper.reset_banks();
        mapper
    }

    pub fn pattern_ref(&self) -> &[u8] {
        &self.chr_window
    }

    pub fn write_pattern(&mut self, address: u16, value: u8) {
        if !self.has_chr_ram {
            return;
        }

        let window_offset = address as usize % CHR_WINDOW_SIZE;
        let chr_index = self.chr_index(address);
        self.chr[chr_index] = value;
        self.chr_window[window_offset] = value;
    }

    pub fn read(&mut self, address: u16) -> u8 {
        self.peek(address)
    }

    pub fn peek(&self, address: u16) -> u8 {
        match address {
            0x8000..=0xbfff => self.read_prg_bank(self.lower_prg_bank(), address - 0x8000),
            0xc000..=0xffff => self.read_prg_bank(self.upper_prg_bank(), address - 0xc000),
            0x6000..=0x7fff => self.prg_ram[address as usize - 0x6000],
            0x4020..=0x5fff => 0,
            _ => panic!("read address out of range: {:04x}", address),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            0x4020..=0x5fff => {}
            0x6000..=0x7fff => self.prg_ram[address as usize - 0x6000] = value,
            0x8000..=0xffff => self.write_load_register(address, value),
            _ => panic!("write address out of range: {:04x}", address),
        }
    }

    fn write_load_register(&mut self, address: u16, value: u8) {
        if value & 0x80 != 0 {
            self.reset_banks();
            return;
        }

        let register_ready = self.shift_register & 0x01 != 0;
        self.shift_register >>= 1;
        self.shift_register |= (value & 0x01) << 4;

        if register_ready {
            let register_value = self.shift_register & 0x1f;
            match address {
                0x8000..=0x9fff => self.control(ControlFlags::from_bits(register_value)),
                0xa000..=0xbfff => self.select_chr_bank0(register_value),
                0xc000..=0xdfff => self.select_chr_bank1(register_value),
                0xe000..=0xffff => self.select_prg_bank(register_value),
                _ => unreachable!(),
            }
            self.shift_register = INITIAL_SHIFT_REGISTER;
        }
    }

    fn mirroring_bits(mirroring: Mirroring) -> u8 {
        match mirroring {
            Mirroring::LowerBank => 0b00,
            Mirroring::UpperBank => 0b01,
            Mirroring::Vertical => 0b10,
            Mirroring::Horizontal | Mirroring::Four => 0b11,
        }
    }

    fn apply_control(&mut self) {
        self.name_table.set_mirroring(self.control.into());
        self.refresh_chr_window();
    }

    fn reset_banks(&mut self) {
        self.shift_register = INITIAL_SHIFT_REGISTER;
        self.control = ControlFlags::new()
            .with_mirroring(self.control.mirroring())
            .with_prg_mode(0b11)
            .with_chr_in_4k(true);
        self.prg_bank = 0;
        self.chr_bank0 = 0;
        self.chr_bank1 = 1;
        self.apply_control();
    }

    fn control(&mut self, flags: ControlFlags) {
        self.control = flags;
        self.apply_control();
    }

    fn chr_bank_size(&self) -> usize {
        if self.control.chr_in_4k() {
            CHR_BANK_SIZE_4K
        } else {
            CHR_WINDOW_SIZE
        }
    }

    fn prg_rom_mode(&self) -> PrgRomMode {
        self.control.into()
    }

    fn prg_rom_bank_count(&self) -> usize {
        self.prg_rom.len() / PRG_ROM_BANK_SIZE
    }

    fn chr_bank_count_4k(&self) -> usize {
        self.chr.len() / CHR_BANK_SIZE_4K
    }

    fn uses_outer_prg_bank(&self) -> bool {
        self.prg_rom.len() > 0x40000 && self.chr.len() <= CHR_WINDOW_SIZE
    }

    fn outer_prg_bank_base(&self) -> usize {
        if self.uses_outer_prg_bank() {
            usize::from((self.chr_bank0 >> 4) & 0x01) * 16
        } else {
            0
        }
    }

    fn prg_region_bank_count(&self) -> usize {
        let total = self.prg_rom_bank_count();
        if self.uses_outer_prg_bank() {
            16.min(total.saturating_sub(self.outer_prg_bank_base()))
        } else {
            total
        }
    }

    fn lower_prg_bank(&self) -> usize {
        if self.prg_rom_bank_count() <= 2 {
            return 0;
        }

        let outer = self.outer_prg_bank_base();
        let region_count = self.prg_region_bank_count();
        let selected = usize::from(self.prg_bank & 0x0f) % region_count;

        match self.prg_rom_mode() {
            PrgRomMode::Switch32K => outer + ((usize::from(self.prg_bank & 0x0e)) % region_count),
            PrgRomMode::FixFirst16K => outer,
            PrgRomMode::FixLast16K => outer + selected,
        }
    }

    fn upper_prg_bank(&self) -> usize {
        let total = self.prg_rom_bank_count();
        if total <= 2 {
            return total.saturating_sub(1);
        }

        let outer = self.outer_prg_bank_base();
        let region_count = self.prg_region_bank_count();
        let selected = usize::from(self.prg_bank & 0x0f) % region_count;

        match self.prg_rom_mode() {
            PrgRomMode::Switch32K => {
                outer + ((usize::from(self.prg_bank & 0x0e) + 1) % region_count)
            }
            PrgRomMode::FixFirst16K => outer + selected,
            PrgRomMode::FixLast16K => outer + region_count - 1,
        }
    }

    fn read_prg_bank(&self, bank: usize, offset: u16) -> u8 {
        let bank = bank % self.prg_rom_bank_count();
        let start = bank * PRG_ROM_BANK_SIZE;
        self.prg_rom[start + offset as usize]
    }

    fn chr_bank_base(&self, upper_half: bool) -> usize {
        let bank_count = self.chr_bank_count_4k();

        if self.control.chr_in_4k() {
            let bank = if upper_half {
                self.chr_bank1
            } else {
                self.chr_bank0
            };
            usize::from(bank % bank_count as u8) * CHR_BANK_SIZE_4K
        } else {
            let bank = usize::from(self.chr_bank0 & 0x1e) % bank_count;
            let bank = if upper_half {
                (bank + 1) % bank_count
            } else {
                bank
            };
            bank * CHR_BANK_SIZE_4K
        }
    }

    fn chr_index(&self, address: u16) -> usize {
        let offset = address as usize % CHR_WINDOW_SIZE;
        let base = if offset < CHR_BANK_SIZE_4K {
            self.chr_bank_base(false)
        } else {
            self.chr_bank_base(true)
        };
        base + (offset % CHR_BANK_SIZE_4K)
    }

    fn refresh_chr_window(&mut self) {
        let lower = self.chr_bank_base(false);
        let upper = self.chr_bank_base(true);

        self.chr_window[..CHR_BANK_SIZE_4K]
            .copy_from_slice(&self.chr[lower..lower + CHR_BANK_SIZE_4K]);
        self.chr_window[CHR_BANK_SIZE_4K..]
            .copy_from_slice(&self.chr[upper..upper + CHR_BANK_SIZE_4K]);
    }

    fn select_chr_bank0(&mut self, value: u8) {
        self.chr_bank0 = value & 0x1f;
        self.refresh_chr_window();
    }

    fn select_chr_bank1(&mut self, value: u8) {
        self.chr_bank1 = value & 0x1f;
        self.refresh_chr_window();
    }

    fn select_prg_bank(&mut self, value: u8) {
        self.prg_bank = value & 0x1f;
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
    use crate::nes::ppu::Ppu;

    fn write_serial(mapper: &mut MMC1, address: u16, value: u8) {
        for bit in 0..5 {
            mapper.write(address, (value >> bit) & 0x01);
        }
    }

    fn create<F>(mut init_prg: F) -> (MMC1, Ppu)
    where
        F: FnMut(&mut [u8]),
    {
        let mut prg_rom = [0; 128 * 1024];
        init_prg(&mut prg_rom);
        (
            MMC1::new(&prg_rom, &[], Mirroring::Horizontal),
            Ppu::new(()),
        )
    }

    fn create_with_chr<F>(mut init_chr: F) -> (MMC1, Ppu)
    where
        F: FnMut(&mut [u8]),
    {
        let mut chr_rom = [0; 128 * 1024];
        init_chr(&mut chr_rom);
        (
            MMC1::new(&[0; 32 * 1024], &chr_rom, Mirroring::Horizontal),
            Ppu::new(()),
        )
    }

    #[test]
    fn prg_rom_switching_uses_serial_load_register() {
        fn init_prg(prg_rom: &mut [u8]) {
            prg_rom[0] = 0xba;
            prg_rom[PRG_ROM_BANK_SIZE] = 0xcd;
            prg_rom[prg_rom.len() - 1] = 0xab;
        }

        let (mut mmc1, _ppu) = create(init_prg);

        assert_eq!(mmc1.read(0x8000), 0xba);
        assert_eq!(mmc1.read(0xffff), 0xab);

        write_serial(&mut mmc1, 0xe000, 1);
        assert_eq!(mmc1.read(0x8000), 0xcd);
        assert_eq!(mmc1.read(0xffff), 0xab);
    }

    #[test]
    fn prg_ram_remains_accessible_across_bank_writes() {
        let (mut mmc1, _ppu) = create(|_| {});

        mmc1.write(0x6000, 0x12);
        assert_eq!(mmc1.read(0x6000), 0x12);

        write_serial(&mut mmc1, 0xe000, 0x10);
        mmc1.write(0x6000, 0x34);
        assert_eq!(mmc1.read(0x6000), 0x34);
    }

    #[test]
    fn reset_write_restores_initial_bank_layout() {
        let (mut mmc1, _ppu) = create(|_| {});

        mmc1.control(ControlFlags::new().with_mirroring(0).with_chr_in_4k(false));
        mmc1.select_prg_bank(2);
        mmc1.select_chr_bank0(3);
        mmc1.select_chr_bank1(4);
        mmc1.write(0x8000, 0x80);

        assert_eq!(Mirroring::LowerBank, mmc1.control.into());
        assert!(mmc1.control.chr_in_4k());
        assert_eq!(PrgRomMode::FixLast16K, mmc1.prg_rom_mode());
        assert_eq!(0, mmc1.prg_bank);
        assert_eq!(0, mmc1.chr_bank0);
        assert_eq!(1, mmc1.chr_bank1);
    }

    #[test]
    fn control_ppu_mirroring() {
        let (mut mmc1, _ppu) = create(|_| {});
        mmc1.control(ControlFlags::new().with_mirroring(0));
        mmc1.write_nametable(0x2400, 0x55);
        assert_eq!(mmc1.read_nametable(0x2000), 0x55);
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
        let (mut mmc1, _ppu) = create(|rom| {
            rom[0] = 1;
            rom[PRG_ROM_BANK_SIZE] = 2;
            rom[PRG_ROM_BANK_SIZE * 2] = 3;
            rom[PRG_ROM_BANK_SIZE * 3] = 4;
            rom[rom.len() - PRG_ROM_BANK_SIZE] = 5;
        });

        mmc1.select_prg_bank(2);
        mmc1.control(ControlFlags::new().with_prg_mode(0));
        assert_eq!(mmc1.read(0x8000), 3);
        assert_eq!(mmc1.read(0xc000), 4);

        mmc1.control(ControlFlags::new().with_prg_mode(2));
        assert_eq!(mmc1.read(0x8000), 1);
        assert_eq!(mmc1.read(0xc000), 3);

        mmc1.control(ControlFlags::new().with_prg_mode(3));
        assert_eq!(mmc1.read(0x8000), 3);
        assert_eq!(mmc1.read(0xc000), 5);
    }

    #[test]
    fn control_chr_bank_size() {
        let (mut mmc1, _ppu) = create(|_| {});

        assert_eq!(mmc1.chr_bank_size(), 4 * 1024);

        mmc1.control(ControlFlags::new().with_chr_in_4k(false));
        assert_eq!(mmc1.chr_bank_size(), 8 * 1024);

        mmc1.control(ControlFlags::new().with_chr_in_4k(true));
        assert_eq!(mmc1.chr_bank_size(), 4 * 1024);
    }

    #[test]
    fn select_chr_bank0() {
        let (mut mmc1, _ppu) = create_with_chr(|rom| {
            rom[0] = 1;
            rom[4 * 1024] = 2;
            rom[8 * 1024] = 3;
            rom[12 * 1024] = 4;
        });

        assert_eq!(mmc1.pattern_ref()[0x0000], 1);
        assert_eq!(mmc1.pattern_ref()[0x1000], 2);

        mmc1.control(ControlFlags::new().with_chr_in_4k(true));
        mmc1.select_chr_bank0(2);
        assert_eq!(mmc1.pattern_ref()[0x0000], 3);
    }

    #[test]
    fn select_chr_bank1() {
        let (mut mmc1, _ppu) = create_with_chr(|rom| {
            rom[0] = 1;
            rom[4 * 1024] = 2;
            rom[8 * 1024] = 3;
            rom[12 * 1024] = 4;
        });

        mmc1.control(ControlFlags::new().with_chr_in_4k(true));
        mmc1.select_chr_bank1(3);
        assert_eq!(mmc1.pattern_ref()[0x1000], 4);
    }

    #[test]
    fn writes_to_chr_ram_when_chr_rom_is_absent() {
        let mut mmc1 = MMC1::new(&[0; 32 * 1024], &[], Mirroring::Horizontal);

        mmc1.write_pattern(0x0000, 0x12);
        mmc1.write_pattern(0x1fff, 0x34);

        assert_eq!(mmc1.pattern_ref()[0x0000], 0x12);
        assert_eq!(mmc1.pattern_ref()[0x1fff], 0x34);
    }
}
