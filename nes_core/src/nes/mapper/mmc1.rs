use super::{Mapper, memory::CartridgeMemory};
use crate::SystemClock;
use crate::nes::mapper::Mirroring;
use modular_bitfield::prelude::*;

#[cfg(test)]
const PRG_ROM_BANK_SIZE: usize = 0x4000;
const INITIAL_SHIFT_REGISTER: u8 = 0x10;

#[bitfield]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
struct ControlFlags {
    pub mirroring: B2,
    pub prg_mode: B2,
    pub chr_in_4k: bool,
    #[skip]
    _not_used: B3,
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
    control: ControlFlags,
    chr_bank0: u8,
    chr_bank1: u8,
    prg_bank: u8,
    shift_register: u8,
    prg_bank_count: usize,
    prg_len_bytes: usize,
    last_write_cycle: Option<SystemClock>,
}

impl MMC1 {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        let instance = Self {
            control: ControlFlags::new()
                .with_mirroring(Self::mirroring_bits(memory.mirroring()))
                .with_prg_mode(0b11),
            chr_bank0: 0,
            chr_bank1: 0,
            prg_bank: 0,
            shift_register: INITIAL_SHIFT_REGISTER,
            prg_bank_count: memory.prg_bank_count_16k(),
            prg_len_bytes: memory.prg_len_bytes(),
            last_write_cycle: None,
        };
        instance.sync_prg(memory);
        instance.sync_chr(memory);
        instance.sync_prg_ram(memory);
        instance
    }

    fn sync_chr(&self, memory: &mut CartridgeMemory) {
        let count = memory.chr_bank_count_4k();
        if self.control.chr_in_4k() {
            memory.map_chr_4k(0, (self.chr_bank0 as usize % count) as u16);
            memory.map_chr_4k(1, (self.chr_bank1 as usize % count) as u16);
        } else {
            let bank = usize::from(self.chr_bank0 & 0x1e) % count;
            memory.map_chr_4k(0, bank as u16);
            memory.map_chr_4k(1, ((bank + 1) % count) as u16);
        }
    }

    fn sync_prg(&self, memory: &mut CartridgeMemory) {
        memory.map_prg_16k(0, self.calc_lower_prg_bank() as u16);
        memory.map_prg_16k(1, self.calc_upper_prg_bank() as u16);
    }

    fn sync_prg_ram(&self, memory: &mut CartridgeMemory) {
        memory.set_prg_ram_enabled(self.prg_ram_enabled());
    }

    fn mirroring_bits(mirroring: Mirroring) -> u8 {
        match mirroring {
            Mirroring::LowerBank => 0b00,
            Mirroring::UpperBank => 0b01,
            Mirroring::Vertical => 0b10,
            Mirroring::Horizontal | Mirroring::Four => 0b11,
        }
    }

    fn prg_rom_mode(&self) -> PrgRomMode {
        self.control.into()
    }

    fn prg_rom_bank_count(&self) -> usize {
        self.prg_bank_count
    }

    fn prg_ram_enabled(&self) -> bool {
        self.prg_bank & 0x10 == 0
    }

    fn uses_outer_prg_bank(&self) -> bool {
        self.prg_len_bytes > 0x40000
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

    fn calc_lower_prg_bank(&self) -> usize {
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

    fn calc_upper_prg_bank(&self) -> usize {
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

    fn select_prg_bank(&mut self, value: u8) {
        self.prg_bank = value & 0x1f;
    }

    fn apply_control(&self, memory: &mut CartridgeMemory) {
        memory.apply_mirroring(self.control.into());
        self.sync_chr(memory);
        self.sync_prg(memory);
    }

    fn write_load_register(&mut self, address: u16, value: u8, memory: &mut CartridgeMemory) {
        if value & 0x80 != 0 {
            self.shift_register = INITIAL_SHIFT_REGISTER;
            self.control = ControlFlags::from_bytes([self.control.into_bytes()[0] | 0x0c]);
            self.apply_control(memory);
            self.sync_prg(memory);
            return;
        }

        let register_ready = self.shift_register & 0x01 != 0;
        self.shift_register >>= 1;
        self.shift_register |= (value & 0x01) << 4;

        if register_ready {
            let register_value = self.shift_register & 0x1f;
            match address {
                0x8000..=0x9fff => {
                    self.control = ControlFlags::from_bytes([register_value]);
                    self.apply_control(memory);
                    self.sync_prg(memory);
                }
                0xa000..=0xbfff => {
                    self.chr_bank0 = register_value & 0x1f;
                    self.sync_chr(memory);
                    self.sync_prg(memory);
                }
                0xc000..=0xdfff => {
                    self.chr_bank1 = register_value & 0x1f;
                    self.sync_chr(memory);
                }
                0xe000..=0xffff => {
                    self.select_prg_bank(register_value);
                    self.sync_prg(memory);
                    self.sync_prg_ram(memory);
                }
                _ => unreachable!(),
            }
            self.shift_register = INITIAL_SHIFT_REGISTER;
        }
    }
}

impl Mapper for MMC1 {
    fn cpu_write(
        &mut self,
        address: u16,
        value: u8,
        cycle: SystemClock,
        memory: &mut CartridgeMemory,
    ) {
        match address {
            0x6000..=0x7fff => {
                if memory.prg_ram_enabled() {
                    memory.write_prg_ram(address, value);
                }
            }
            0x8000..=0xffff => {
                // Consecutive-cycle filter: data writes (bit7 clear) within
                // 1 CPU cycle (3 PPU cycles) of the previous mapper write
                // are ignored. Reset writes (bit7 set) are never ignored
                // but do update the timestamp.
                if value & 0x80 != 0 {
                    self.last_write_cycle = Some(cycle);
                    self.write_load_register(address, value, memory);
                    return;
                }
                if let Some(last) = self.last_write_cycle
                    && cycle.cycles().wrapping_sub(last.cycles()) <= 3
                {
                    return;
                }
                self.last_write_cycle = Some(cycle);
                self.write_load_register(address, value, memory)
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    type Board = (MMC1, CartridgeMemory);

    fn advance(mut cycle: SystemClock) -> SystemClock {
        for _ in 0..6 {
            cycle = cycle.inc();
        }
        cycle
    }

    fn write_serial(mapper: &mut MMC1, memory: &mut CartridgeMemory, address: u16, value: u8) {
        // Start far enough from any previous write to avoid the
        // consecutive-cycle filter (data writes within 1 CPU cycle / 3 PPU
        // cycles are ignored). Use a fresh base and advance past the
        // window before the first real data write.
        let mut cycle = advance(SystemClock::default());
        for bit in 0..5 {
            mapper.cpu_write(address, (value >> bit) & 0x01, cycle, memory);
            cycle = advance(cycle);
        }
    }

    fn create<F>(mut init_prg: F) -> Board
    where
        F: FnMut(&mut [u8]),
    {
        let mut prg_rom = [0; 128 * 1024];
        init_prg(&mut prg_rom);
        let mut memory = CartridgeMemory::test(&prg_rom, &[]);
        let mapper = MMC1::new(&mut memory);
        (mapper, memory)
    }

    fn create_with_chr() -> Board {
        let mut data = vec![0u8; 0x8000];
        data[0x0000] = 0x10;
        data[0x1000] = 0x20;
        data[0x2000] = 0x30;
        data[0x3000] = 0x40;
        let prg = vec![0u8; 0x8000];
        let mut memory = CartridgeMemory::test(&prg, &data);
        let mapper = MMC1::new(&mut memory);
        (mapper, memory)
    }

    #[test]
    fn prg_rom_switching_uses_serial_load_register() {
        let (mut mmc1, mut memory) = create(|prg_rom| {
            prg_rom[0] = 0xba;
            prg_rom[PRG_ROM_BANK_SIZE] = 0xcd;
            prg_rom[prg_rom.len() - 1] = 0xab;
        });

        assert_eq!(memory.read_prg(0x8000), 0xba);
        assert_eq!(memory.read_prg(0xffff), 0xab);

        write_serial(&mut mmc1, &mut memory, 0xe000, 1);
        assert_eq!(memory.read_prg(0x8000), 0xcd);
        assert_eq!(memory.read_prg(0xffff), 0xab);
    }

    #[test]
    fn prg_ram_respects_enable_bit() {
        let (mut mmc1, mut memory) = create(|_| {});

        mmc1.cpu_write(0x6000, 0x12, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg_ram(0x6000), 0x12);

        write_serial(&mut mmc1, &mut memory, 0xe000, 0x10);
        assert!(!memory.prg_ram_enabled());
        mmc1.cpu_write(0x6000, 0x34, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg_ram(0x6000), 0x12);
    }

    #[test]
    fn reset_write_keeps_mirroring_and_chr_mode() {
        let (mut mmc1, mut memory) = create(|_| {});

        let control = ControlFlags::new().with_mirroring(0).with_chr_in_4k(true);
        mmc1.control = control;
        mmc1.cpu_write(0x8000, 0x80, SystemClock::default(), &mut memory);

        assert_eq!(Mirroring::LowerBank, mmc1.control.into());
        assert!(mmc1.control.chr_in_4k());
        assert_eq!(PrgRomMode::FixLast16K, mmc1.prg_rom_mode());
    }

    #[test]
    fn control_ppu_mirroring() {
        let (mut mmc1, mut memory) = create(|_| {});
        let control = ControlFlags::new().with_mirroring(0);
        write_serial(&mut mmc1, &mut memory, 0x8000, control.into_bytes()[0]);
        assert_eq!(Mirroring::from(mmc1.control), Mirroring::LowerBank);
        assert_eq!(memory.mirroring(), Mirroring::LowerBank);
        // Single-screen lower bank: all four nametable slots alias.
        memory.write_vram(0x2000, 0x5a);
        assert_eq!(memory.read_vram(0x2400), 0x5a);
        assert_eq!(memory.read_vram(0x2800), 0x5a);
        assert_eq!(memory.read_vram(0x2c00), 0x5a);
    }

    #[test]
    fn vertical_mirroring_pairs_nametables() {
        let (mut mmc1, mut memory) = create(|_| {});
        let control = ControlFlags::new().with_mirroring(0b10);
        write_serial(&mut mmc1, &mut memory, 0x8000, control.into_bytes()[0]);
        assert_eq!(memory.mirroring(), Mirroring::Vertical);
        memory.write_vram(0x2000, 0x11);
        assert_eq!(memory.read_vram(0x2800), 0x11);
        assert_ne!(memory.read_vram(0x2400), 0x11);
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
        let (mut mmc1, mut memory) = create(|rom| {
            rom[0] = 1;
            rom[PRG_ROM_BANK_SIZE] = 2;
            rom[PRG_ROM_BANK_SIZE * 2] = 3;
            rom[PRG_ROM_BANK_SIZE * 3] = 4;
            rom[rom.len() - PRG_ROM_BANK_SIZE] = 5;
        });

        mmc1.select_prg_bank(2);
        write_serial(
            &mut mmc1,
            &mut memory,
            0x8000,
            ControlFlags::new().with_prg_mode(0).into_bytes()[0],
        );
        assert_eq!(memory.read_prg(0x8000), 3);
        assert_eq!(memory.read_prg(0xc000), 4);

        write_serial(
            &mut mmc1,
            &mut memory,
            0x8000,
            ControlFlags::new().with_prg_mode(2).into_bytes()[0],
        );
        assert_eq!(memory.read_prg(0x8000), 1);
        assert_eq!(memory.read_prg(0xc000), 3);

        write_serial(
            &mut mmc1,
            &mut memory,
            0x8000,
            ControlFlags::new().with_prg_mode(3).into_bytes()[0],
        );
        assert_eq!(memory.read_prg(0x8000), 3);
        assert_eq!(memory.read_prg(0xc000), 5);
    }

    #[test]
    fn chr_initial_state_reads_bank_0() {
        let (_mapper, memory) = create_with_chr();
        assert_eq!(memory.read_vram(0x0000), 0x10);
    }

    #[test]
    fn chr_select_chr_bank0_in_4k_mode() {
        let (mut mapper, mut memory) = create_with_chr();
        write_serial(&mut mapper, &mut memory, 0x8000, 0x10);
        write_serial(&mut mapper, &mut memory, 0xa000, 0x03);
        assert_eq!(memory.read_vram(0x0000), 0x40);
    }

    #[test]
    fn chr_select_chr_bank1_in_4k_mode() {
        let (mut mapper, mut memory) = create_with_chr();
        write_serial(&mut mapper, &mut memory, 0x8000, 0x10);
        write_serial(&mut mapper, &mut memory, 0xc000, 0x01);
        assert_eq!(memory.read_vram(0x1000), 0x20);
    }

    #[test]
    fn chr_writes_to_chr_ram_persists() {
        let prg = vec![0u8; 0x8000];
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let _mapper = MMC1::new(&mut memory);
        memory.write_vram(0x0000, 0xab);
        assert_eq!(memory.read_vram(0x0000), 0xab);
    }

    #[test]
    fn consecutive_data_writes_are_ignored() {
        let (mut mapper, mut memory) = create(|_| {});
        let mut cycle = SystemClock::default();
        // First data write at cycle 0
        mapper.cpu_write(0x8000, 0x00, cycle, &mut memory);
        let shift_after_first = mapper.shift_register;
        // Second data write on next CPU cycle (3 PPU cycles later) should be ignored
        for _ in 0..3 {
            cycle = cycle.inc();
        }
        mapper.cpu_write(0x8000, 0x01, cycle, &mut memory);
        // Shift register should not have advanced for the ignored write
        assert_eq!(
            mapper.shift_register, shift_after_first,
            "consecutive data write within 1 CPU cycle must be ignored"
        );
    }

    #[test]
    fn reset_write_is_never_ignored_even_when_consecutive() {
        let (mut mapper, mut memory) = create(|_| {});
        let mut cycle = SystemClock::default();
        // First data write
        mapper.cpu_write(0x8000, 0x00, cycle, &mut memory);
        for _ in 0..3 {
            cycle = cycle.inc();
        }
        // Reset write (bit7 set) on next CPU cycle must NOT be ignored
        // and must reset shift register to initial state
        mapper.cpu_write(0x8000, 0x80, cycle, &mut memory);
        assert_eq!(
            mapper.shift_register, INITIAL_SHIFT_REGISTER,
            "reset write must not be ignored even when consecutive"
        );
    }
}
