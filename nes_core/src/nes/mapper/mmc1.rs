use super::{Cartridge, CartridgeOperation, ChrStorage};
use crate::nes::mapper::Mirroring;
use bitfield_struct::bitfield;

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
    prg_rom: Vec<u8>,
    prg_ram: [u8; PRG_RAM_SIZE],

    control: ControlFlags,
    chr_bank0: u8,
    chr_bank1: u8,
    prg_bank: u8,
    shift_register: u8,
}

impl MMC1 {
    pub fn new(prg_rom: &[u8], mirroring: Mirroring) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert_eq!(prg_rom.len() % PRG_ROM_BANK_SIZE, 0);

        Self {
            prg_rom: prg_rom.to_vec(),
            prg_ram: [0; PRG_RAM_SIZE],
            control: ControlFlags::new()
                .with_mirroring(Self::mirroring_bits(mirroring))
                .with_prg_mode(0b11),
            chr_bank0: 0,
            chr_bank1: 0,
            prg_bank: 0,
            shift_register: INITIAL_SHIFT_REGISTER,
        }
    }
}

impl Cartridge for MMC1 {
    fn read(&mut self, address: u16) -> u8 {
        self.peek(address)
    }

    fn peek(&self, address: u16) -> u8 {
        match address {
            0x8000..=0xbfff => self.read_prg_bank(self.lower_prg_bank(), address - 0x8000),
            0xc000..=0xffff => self.read_prg_bank(self.upper_prg_bank(), address - 0xc000),
            0x6000..=0x7fff => {
                if self.prg_ram_enabled() {
                    self.prg_ram[address as usize - 0x6000]
                } else {
                    0
                }
            }
            0x4020..=0x5fff => 0,
            _ => panic!("read address out of range: {:04x}", address),
        }
    }

    fn write(&mut self, address: u16, value: u8) -> CartridgeOperation {
        match address {
            0x4020..=0x5fff => CartridgeOperation::None,
            0x6000..=0x7fff => {
                if self.prg_ram_enabled() {
                    self.prg_ram[address as usize - 0x6000] = value;
                }
                CartridgeOperation::None
            }
            0x8000..=0xffff => self.write_load_register(address, value),
            _ => panic!("write address out of range: {:04x}", address),
        }
    }
}

impl MMC1 {
    fn write_load_register(&mut self, address: u16, value: u8) -> CartridgeOperation {
        if value & 0x80 != 0 {
            self.shift_register = INITIAL_SHIFT_REGISTER;
            self.control = ControlFlags::from_bits(self.control.into_bits() | 0x0c);
            return self.apply_control();
        }

        let register_ready = self.shift_register & 0x01 != 0;
        self.shift_register >>= 1;
        self.shift_register |= (value & 0x01) << 4;

        if register_ready {
            let register_value = self.shift_register & 0x1f;
            let result = match address {
                0x8000..=0x9fff => {
                    self.control = ControlFlags::from_bits(register_value);
                    self.apply_control()
                }
                0xa000..=0xbfff => {
                    self.chr_bank0 = register_value & 0x1f;
                    CartridgeOperation::None
                }
                0xc000..=0xdfff => {
                    self.chr_bank1 = register_value & 0x1f;
                    CartridgeOperation::None
                }
                0xe000..=0xffff => {
                    self.select_prg_bank(register_value);
                    CartridgeOperation::None
                }
                _ => unreachable!(),
            };
            self.shift_register = INITIAL_SHIFT_REGISTER;
            result
        } else {
            CartridgeOperation::None
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

    fn apply_control(&mut self) -> CartridgeOperation {
        let mirroring: Mirroring = self.control.into();
        CartridgeOperation::UpdateNametableMirroring(mirroring)
    }

    fn prg_rom_mode(&self) -> PrgRomMode {
        self.control.into()
    }

    fn prg_rom_bank_count(&self) -> usize {
        self.prg_rom.len() / PRG_ROM_BANK_SIZE
    }

    fn prg_ram_enabled(&self) -> bool {
        self.prg_bank & 0x10 == 0
    }

    fn uses_outer_prg_bank(&self) -> bool {
        self.prg_rom.len() > 0x40000
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

    fn select_prg_bank(&mut self, value: u8) {
        self.prg_bank = value & 0x1f;
    }
}

pub struct Mmc1ChrStorage {
    source: Vec<u8>,
    has_chr_ram: bool,
    chr_bank0: u8,
    chr_bank1: u8,
    chr_in_4k: bool,
    shift_register: u8,
}

impl Mmc1ChrStorage {
    const INITIAL_SHIFT_REGISTER: u8 = 0x10;

    pub fn new(chr_rom: &[u8]) -> Self {
        let has_chr_ram = chr_rom.is_empty();
        let size = if has_chr_ram { 0x2000 } else { chr_rom.len() };
        let mut source = vec![0; size];
        source[..chr_rom.len()].copy_from_slice(chr_rom);
        Self {
            source,
            has_chr_ram,
            chr_bank0: 0,
            chr_bank1: 0,
            chr_in_4k: false,
            shift_register: Self::INITIAL_SHIFT_REGISTER,
        }
    }

    fn bank_count_4k(&self) -> usize {
        if self.source.is_empty() {
            1
        } else {
            self.source.len() / 0x1000
        }
    }

    fn chr_offset(&self, address: u16) -> usize {
        let offset = address as usize % 0x2000;
        let upper = offset >= 0x1000;
        if self.chr_in_4k {
            let bank = if upper {
                self.chr_bank1
            } else {
                self.chr_bank0
            };
            (bank as usize % self.bank_count_4k()) * 0x1000 + (offset % 0x1000)
        } else {
            let bank = usize::from(self.chr_bank0 & 0x1e) % self.bank_count_4k();
            let bank = if upper {
                (bank + 1) % self.bank_count_4k()
            } else {
                bank
            };
            bank * 0x1000 + (offset % 0x1000)
        }
    }
}

impl ChrStorage for Mmc1ChrStorage {
    fn read_chr(&self, address: u16) -> u8 {
        let src_offset = self.chr_offset(address);
        self.source[src_offset % self.source.len()]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        if !self.has_chr_ram {
            return;
        }
        let src_offset = self.chr_offset(address);
        let len = self.source.len();
        self.source[src_offset % len] = value;
    }

    fn write_register(&mut self, addr: u16, value: u8) {
        if !(0x8000..=0xffff).contains(&addr) {
            return;
        }

        if value & 0x80 != 0 {
            self.shift_register = Self::INITIAL_SHIFT_REGISTER;
            return;
        }

        let ready = self.shift_register & 0x01 != 0;
        self.shift_register >>= 1;
        self.shift_register |= (value & 0x01) << 4;

        if ready {
            let reg_value = self.shift_register & 0x1f;
            match addr {
                0x8000..=0x9fff => {
                    self.chr_in_4k = (reg_value & 0x10) != 0;
                }
                0xa000..=0xbfff => {
                    self.chr_bank0 = reg_value;
                }
                0xc000..=0xdfff => {
                    self.chr_bank1 = reg_value;
                }
                _ => {}
            }
            self.shift_register = Self::INITIAL_SHIFT_REGISTER;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_serial(mapper: &mut MMC1, address: u16, value: u8) {
        for bit in 0..5 {
            mapper.write(address, (value >> bit) & 0x01);
        }
    }

    fn create<F>(mut init_prg: F) -> MMC1
    where
        F: FnMut(&mut [u8]),
    {
        let mut prg_rom = [0; 128 * 1024];
        init_prg(&mut prg_rom);
        MMC1::new(&prg_rom, Mirroring::Horizontal)
    }

    #[test]
    fn prg_rom_switching_uses_serial_load_register() {
        fn init_prg(prg_rom: &mut [u8]) {
            prg_rom[0] = 0xba;
            prg_rom[PRG_ROM_BANK_SIZE] = 0xcd;
            prg_rom[prg_rom.len() - 1] = 0xab;
        }

        let mut mmc1 = create(init_prg);

        assert_eq!(mmc1.read(0x8000), 0xba);
        assert_eq!(mmc1.read(0xffff), 0xab);

        write_serial(&mut mmc1, 0xe000, 1);
        assert_eq!(mmc1.read(0x8000), 0xcd);
        assert_eq!(mmc1.read(0xffff), 0xab);
    }

    #[test]
    fn prg_ram_respects_enable_bit() {
        let mut mmc1 = create(|_| {});

        mmc1.write(0x6000, 0x12);
        assert_eq!(mmc1.read(0x6000), 0x12);

        write_serial(&mut mmc1, 0xe000, 0x10);
        mmc1.write(0x6000, 0x34);
        assert_eq!(mmc1.read(0x6000), 0x00);
    }

    #[test]
    fn reset_write_keeps_mirroring_and_chr_mode() {
        let mut mmc1 = create(|_| {});

        let control = ControlFlags::new().with_mirroring(0).with_chr_in_4k(true);
        mmc1.control = control;
        mmc1.write(0x8000, 0x80);

        assert_eq!(Mirroring::LowerBank, mmc1.control.into());
        assert!(mmc1.control.chr_in_4k());
        assert_eq!(PrgRomMode::FixLast16K, mmc1.prg_rom_mode());
    }

    #[test]
    fn control_ppu_mirroring() {
        let mut mmc1 = create(|_| {});
        let control = ControlFlags::new().with_mirroring(0);
        write_serial(&mut mmc1, 0x8000, control.into_bits());
        assert_eq!(Mirroring::from(mmc1.control), Mirroring::LowerBank);
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
        let mut mmc1 = create(|rom| {
            rom[0] = 1;
            rom[PRG_ROM_BANK_SIZE] = 2;
            rom[PRG_ROM_BANK_SIZE * 2] = 3;
            rom[PRG_ROM_BANK_SIZE * 3] = 4;
            rom[rom.len() - PRG_ROM_BANK_SIZE] = 5;
        });

        mmc1.select_prg_bank(2);
        write_serial(
            &mut mmc1,
            0x8000,
            ControlFlags::new().with_prg_mode(0).into_bits(),
        );
        assert_eq!(mmc1.read(0x8000), 3);
        assert_eq!(mmc1.read(0xc000), 4);

        write_serial(
            &mut mmc1,
            0x8000,
            ControlFlags::new().with_prg_mode(2).into_bits(),
        );
        assert_eq!(mmc1.read(0x8000), 1);
        assert_eq!(mmc1.read(0xc000), 3);

        write_serial(
            &mut mmc1,
            0x8000,
            ControlFlags::new().with_prg_mode(3).into_bits(),
        );
        assert_eq!(mmc1.read(0x8000), 3);
        assert_eq!(mmc1.read(0xc000), 5);
    }

    fn make_chr() -> Vec<u8> {
        let mut data = vec![0u8; 0x8000];
        data[0x0000] = 0x10;
        data[0x1000] = 0x20;
        data[0x2000] = 0x30;
        data[0x3000] = 0x40;
        data
    }

    fn write_serial_chr(chr: &mut Mmc1ChrStorage, address: u16, value: u8) {
        for bit in 0..5 {
            chr.write_register(address, (value >> bit) & 0x01);
        }
    }

    #[test]
    fn chr_initial_state_reads_bank_0() {
        let chr = Mmc1ChrStorage::new(&make_chr());
        assert_eq!(chr.read_chr(0x0000), 0x10);
    }

    #[test]
    fn chr_select_chr_bank0_in_4k_mode() {
        let mut chr = Mmc1ChrStorage::new(&make_chr());
        write_serial_chr(&mut chr, 0x8000, 0x10);
        write_serial_chr(&mut chr, 0xa000, 0x03);
        assert_eq!(chr.read_chr(0x0000), 0x40);
    }

    #[test]
    fn chr_select_chr_bank1_in_4k_mode() {
        let mut chr = Mmc1ChrStorage::new(&make_chr());
        write_serial_chr(&mut chr, 0x8000, 0x10);
        write_serial_chr(&mut chr, 0xc000, 0x01);
        assert_eq!(chr.read_chr(0x1000), 0x20);
    }

    #[test]
    fn chr_writes_to_chr_ram_persists() {
        let mut chr = Mmc1ChrStorage::new(&[]);
        chr.write_chr(0x0000, 0xab);
        assert_eq!(chr.read_chr(0x0000), 0xab);
    }
}
