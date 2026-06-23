use super::{Cartridge, CARTRIDGE_START_ADDR, CartridgeOperation, ChrStorage};
use crate::nes::mapper::Mirroring;

const PRG_RAM_SIZE: usize = 0x2000;
const PRG_ROM_BANK_SIZE: usize = 0x2000;
const MMC3_A12_LOW_FILTER_TICKS: u8 = 12;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IrqRevision {
    Standard,
    Alternate,
}

pub struct MMC3 {
    prg_rom: Vec<u8>,
    prg_ram: [u8; PRG_RAM_SIZE],
    mirroring_locked: bool,
    bank_select: u8,
    reg_6: u8,
    reg_7: u8,
    prg_ram_enabled: bool,
    prg_ram_write_protect: bool,
    prg_offsets: [usize; 4],
    irq_latch: u8,
    irq_counter: u8,
    irq_reload: bool,
    irq_enabled: bool,
    irq_pending: bool,
    irq_revision: IrqRevision,
    prev_a12: bool,
    a12_low_ticks: u8,
    a12_transition_this_scanline: bool,
}

impl MMC3 {
    pub fn new(
        prg_rom: &[u8],
        mirroring_locked: bool,
        alternate_irq_revision: bool,
    ) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert_eq!(prg_rom.len() % PRG_ROM_BANK_SIZE, 0);

        let mut mapper = Self {
            prg_rom: prg_rom.to_vec(),
            prg_ram: [0; PRG_RAM_SIZE],
            mirroring_locked,
            bank_select: 0,
            reg_6: 0,
            reg_7: 0,
            prg_ram_enabled: true,
            prg_ram_write_protect: false,
            prg_offsets: [0; 4],
            irq_latch: 0,
            irq_counter: 0,
            irq_reload: false,
            irq_enabled: false,
            irq_pending: false,
            irq_revision: if alternate_irq_revision {
                IrqRevision::Alternate
            } else {
                IrqRevision::Standard
            },
            prev_a12: false,
            a12_low_ticks: 0,
            a12_transition_this_scanline: false,
        };
        mapper.sync_prg_banks();
        mapper
    }

    fn prg_bank_count(&self) -> usize {
        self.prg_rom.len() / PRG_ROM_BANK_SIZE
    }

    fn normalize_prg_bank(&self, bank: u8) -> usize {
        (bank as usize) % self.prg_bank_count()
    }

    fn set_prg_slot(&mut self, slot: usize, bank: usize) {
        self.prg_offsets[slot] = bank * PRG_ROM_BANK_SIZE;
    }

    fn sync_prg_banks(&mut self) {
        let last_bank = self.prg_bank_count() - 1;
        let second_last_bank = last_bank - 1;
        let bank6 = self.normalize_prg_bank(self.reg_6);
        let bank7 = self.normalize_prg_bank(self.reg_7);
        let prg_mode = (self.bank_select & 0x40) != 0;

        if prg_mode {
            self.set_prg_slot(0, second_last_bank);
            self.set_prg_slot(1, bank7);
            self.set_prg_slot(2, bank6);
        } else {
            self.set_prg_slot(0, bank6);
            self.set_prg_slot(1, bank7);
            self.set_prg_slot(2, second_last_bank);
        }
        self.set_prg_slot(3, last_bank);
    }

    fn read_prg(&self, slot: usize, address: u16) -> u8 {
        let offset = self.prg_offsets[slot] + ((address as usize) % PRG_ROM_BANK_SIZE);
        self.prg_rom[offset]
    }

    fn select_bank(&mut self, value: u8) {
        self.bank_select = value;
        self.sync_prg_banks();
    }

    fn write_bank_data(&mut self, value: u8) {
        match self.bank_select & 0x07 {
            6 => self.reg_6 = value,
            7 => self.reg_7 = value,
            _ => {}
        }
        self.sync_prg_banks();
    }

    fn set_mirroring(&mut self, value: u8) -> CartridgeOperation {
        if self.mirroring_locked {
            return CartridgeOperation::None;
        }

        let mirroring = if value & 0x01 == 0 {
            Mirroring::Vertical
        } else {
            Mirroring::Horizontal
        };
        CartridgeOperation::UpdateNametableMirroring(mirroring)
    }

    fn set_prg_ram_protect(&mut self, value: u8) {
        self.prg_ram_write_protect = (value & 0x40) != 0;
        self.prg_ram_enabled = (value & 0x80) != 0;
    }

    fn clock_irq(&mut self) {
        let counter_was_zero = self.irq_counter == 0;
        let reload_requested = self.irq_reload;
        let reload = counter_was_zero || reload_requested;

        if reload {
            self.irq_counter = self.irq_latch;
            self.irq_reload = false;
        } else {
            self.irq_counter -= 1;
        }

        let should_set_irq = match self.irq_revision {
            IrqRevision::Standard => self.irq_enabled && self.irq_counter == 0,
            IrqRevision::Alternate => {
                self.irq_enabled && self.irq_counter == 0 && (!counter_was_zero || reload_requested)
            }
        };

        if should_set_irq {
            self.irq_pending = true;
        }
    }
}

impl Cartridge for MMC3 {
    fn read(&self, address: u16) -> u8 {
        match address {
            CARTRIDGE_START_ADDR..=0x5fff => 0,
            0x6000..=0x7fff => {
                if self.prg_ram_enabled {
                    self.prg_ram[(address - 0x6000) as usize]
                } else {
                    0
                }
            }
            0x8000..=0x9fff => self.read_prg(0, address - 0x8000),
            0xa000..=0xbfff => self.read_prg(1, address - 0xa000),
            0xc000..=0xdfff => self.read_prg(2, address - 0xc000),
            0xe000..=0xffff => self.read_prg(3, address - 0xe000),
            _ => unreachable!(),
        }
    }

    fn write(&mut self, address: u16, value: u8) -> CartridgeOperation {
        match address {
            CARTRIDGE_START_ADDR..=0x5fff => CartridgeOperation::None,
            0x6000..=0x7fff => {
                if self.prg_ram_enabled && !self.prg_ram_write_protect {
                    self.prg_ram[(address - 0x6000) as usize] = value;
                }
                CartridgeOperation::None
            }
            0x8000..=0x9fff => {
                if address & 0x01 == 0 {
                    self.select_bank(value);
                } else {
                    self.write_bank_data(value);
                }
                CartridgeOperation::None
            }
            0xa000..=0xbfff => {
                if address & 0x01 == 0 {
                    self.set_mirroring(value)
                } else {
                    self.set_prg_ram_protect(value);
                    CartridgeOperation::None
                }
            }
            0xc000..=0xdfff => {
                if address & 0x01 == 0 {
                    self.irq_latch = value;
                } else {
                    self.irq_reload = true;
                }
                CartridgeOperation::None
            }
            0xe000..=0xffff => {
                if address & 0x01 == 0 {
                    self.irq_enabled = false;
                    self.irq_pending = false;
                } else {
                    self.irq_enabled = true;
                }
                CartridgeOperation::None
            }
            _ => unreachable!(),
        }
    }

    fn on_ppu_tick(&mut self, scanline: u16, _dot: u16, _rendering_enabled: bool) {
        if self.prev_a12 {
            self.a12_low_ticks = 0;
        } else {
            self.a12_low_ticks = self.a12_low_ticks.saturating_add(1);
        }

        if scanline == 261 || scanline == 239 {
            self.a12_transition_this_scanline = false;
        }
    }

    fn notify_vram_address(&mut self, addr: u16) {
        let addr = addr & 0x3fff;
        let a12 = (addr & 0x1000) != 0;
        let should_monitor = addr < 0x2000 || (0x2000..0x3000).contains(&addr);

        if !should_monitor {
            self.prev_a12 = a12;
            if !a12 {
                self.a12_low_ticks = 0;
            }
            return;
        }

        if a12 && !self.prev_a12 && self.a12_low_ticks >= MMC3_A12_LOW_FILTER_TICKS {
            self.a12_transition_this_scanline = true;
            self.clock_irq();
        }

        if a12 {
            self.a12_low_ticks = 0;
        }

        self.prev_a12 = a12;
    }

    fn irq_pending(&self) -> bool {
        self.irq_pending
    }
}

pub struct Mmc3ChrStorage {
    source: Vec<u8>,
    has_chr_ram: bool,
    bank_select: u8,
    bank_registers: [u8; 8],
    bank_offsets: [usize; 8],
}

impl Mmc3ChrStorage {
    const BANK_SIZE: usize = 0x0400;
    const WINDOW_SIZE: usize = 0x2000;

    pub fn new(chr_rom: &[u8]) -> Self {
        let has_chr_ram = chr_rom.is_empty();
        let size = if has_chr_ram {
            Self::WINDOW_SIZE
        } else {
            chr_rom.len()
        };
        let mut source = vec![0; size];
        source[..chr_rom.len()].copy_from_slice(chr_rom);
        let mut storage = Self {
            source,
            has_chr_ram,
            bank_select: 0,
            bank_registers: [0; 8],
            bank_offsets: [0; 8],
        };
        storage.sync_banks();
        storage
    }

    fn bank_count(&self) -> usize {
        self.source.len() / Self::BANK_SIZE
    }

    fn normalize_bank(&self, bank: u8) -> usize {
        if self.bank_count() == 0 {
            0
        } else {
            (bank as usize) % self.bank_count()
        }
    }

    fn sync_banks(&mut self) {
        let chr_mode = (self.bank_select & 0x80) != 0;
        let r0_even = self.normalize_bank(self.bank_registers[0] & !1);
        let r0_odd = self.normalize_bank(self.bank_registers[0] | 1);
        let r1_even = self.normalize_bank(self.bank_registers[1] & !1);
        let r1_odd = self.normalize_bank(self.bank_registers[1] | 1);
        let r2 = self.normalize_bank(self.bank_registers[2]);
        let r3 = self.normalize_bank(self.bank_registers[3]);
        let r4 = self.normalize_bank(self.bank_registers[4]);
        let r5 = self.normalize_bank(self.bank_registers[5]);

        let banks = if chr_mode {
            [r2, r3, r4, r5, r0_even, r0_odd, r1_even, r1_odd]
        } else {
            [r0_even, r0_odd, r1_even, r1_odd, r2, r3, r4, r5]
        };

        for (slot, bank) in banks.into_iter().enumerate() {
            self.bank_offsets[slot] = bank * Self::BANK_SIZE;
        }
    }
}

impl ChrStorage for Mmc3ChrStorage {
    fn read_chr(&self, address: u16) -> u8 {
        let addr = address as usize % Self::WINDOW_SIZE;
        let slot = addr / Self::BANK_SIZE;
        let offset = addr % Self::BANK_SIZE;
        let source_addr = self.bank_offsets[slot] + offset;
        let len = self.source.len();
        self.source[source_addr % len]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        if !self.has_chr_ram {
            return;
        }
        let addr = address as usize % Self::WINDOW_SIZE;
        let slot = addr / Self::BANK_SIZE;
        let offset = addr % Self::BANK_SIZE;
        let source_addr = self.bank_offsets[slot] + offset;
        let len = self.source.len();
        self.source[source_addr % len] = value;
    }

    fn write_register(&mut self, addr: u16, value: u8) {
        match addr {
            0x8000..=0x9fff if addr & 0x01 == 0 => {
                self.bank_select = value;
            }
            0x8000..=0x9fff => {
                let target = (self.bank_select & 0x07) as usize;
                if target < 6 {
                    self.bank_registers[target] = value;
                    self.sync_banks();
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_chr() -> Vec<u8> {
        let mut chr = vec![0u8; Mmc3ChrStorage::BANK_SIZE * 16];
        for bank in 0..16 {
            chr[bank * Mmc3ChrStorage::BANK_SIZE] = bank as u8;
        }
        chr
    }

    #[test]
    fn switches_chr_layout_in_chr_mode_0() {
        let chr_data = create_chr();
        let mut chr = Mmc3ChrStorage::new(&chr_data);

        chr.write_register(0x8000, 0x00);
        chr.write_register(0x8001, 0x06);
        chr.write_register(0x8000, 0x01);
        chr.write_register(0x8001, 0x08);
        chr.write_register(0x8000, 0x02);
        chr.write_register(0x8001, 0x0a);
        chr.write_register(0x8000, 0x03);
        chr.write_register(0x8001, 0x0b);
        chr.write_register(0x8000, 0x04);
        chr.write_register(0x8001, 0x0c);
        chr.write_register(0x8000, 0x05);
        chr.write_register(0x8001, 0x0d);

        assert_eq!(chr.read_chr(0x0000), 0x06);
        assert_eq!(chr.read_chr(0x0400), 0x07);
        assert_eq!(chr.read_chr(0x0800), 0x08);
        assert_eq!(chr.read_chr(0x0c00), 0x09);
        assert_eq!(chr.read_chr(0x1000), 0x0a);
        assert_eq!(chr.read_chr(0x1400), 0x0b);
        assert_eq!(chr.read_chr(0x1800), 0x0c);
        assert_eq!(chr.read_chr(0x1c00), 0x0d);
    }

    #[test]
    fn switches_chr_layout_in_chr_mode_1() {
        let chr_data = create_chr();
        let mut chr = Mmc3ChrStorage::new(&chr_data);

        chr.write_register(0x8000, 0x80);
        chr.write_register(0x8001, 0x06);
        chr.write_register(0x8000, 0x81);
        chr.write_register(0x8001, 0x08);
        chr.write_register(0x8000, 0x82);
        chr.write_register(0x8001, 0x0a);
        chr.write_register(0x8000, 0x83);
        chr.write_register(0x8001, 0x0b);
        chr.write_register(0x8000, 0x84);
        chr.write_register(0x8001, 0x0c);
        chr.write_register(0x8000, 0x85);
        chr.write_register(0x8001, 0x0d);

        assert_eq!(chr.read_chr(0x0000), 0x0a);
        assert_eq!(chr.read_chr(0x0400), 0x0b);
        assert_eq!(chr.read_chr(0x0800), 0x0c);
        assert_eq!(chr.read_chr(0x0c00), 0x0d);
        assert_eq!(chr.read_chr(0x1000), 0x06);
        assert_eq!(chr.read_chr(0x1400), 0x07);
        assert_eq!(chr.read_chr(0x1800), 0x08);
        assert_eq!(chr.read_chr(0x1c00), 0x09);
    }

    #[test]
    fn writes_to_chr_ram_through_current_mapping() {
        let mut chr = Mmc3ChrStorage::new(&[]);

        chr.write_register(0x8000, 0x02);
        chr.write_register(0x8001, 0x03);
        chr.write_chr(0x1000, 0xaa);

        assert_eq!(chr.read_chr(0x1000), 0xaa);
    }
}
