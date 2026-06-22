use super::{Cartridge, CARTRIDGE_START_ADDR, CartridgeOperation};
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
    bank_registers: [u8; 8],
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
            bank_registers: [0; 8],
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
        let bank6 = self.normalize_prg_bank(self.bank_registers[6]);
        let bank7 = self.normalize_prg_bank(self.bank_registers[7]);
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
        let target = (self.bank_select & 0x07) as usize;
        self.bank_registers[target] = value;
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
    fn read(&mut self, address: u16) -> u8 {
        self.peek(address)
    }

    fn peek(&self, address: u16) -> u8 {
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

#[cfg(test)]
mod tests;
