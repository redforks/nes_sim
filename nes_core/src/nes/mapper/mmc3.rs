use super::{CartridgeCaps, Mapper, memory::CartridgeMemory};
use crate::SystemClock;
use crate::nes::mapper::Mirroring;

pub(crate) const MMC3_A12_LOW_FILTER_TICKS: u8 = 12;
#[cfg(test)]
const PRG_ROM_BANK_SIZE: usize = 0x2000;
#[cfg(test)]
const CHR_BANK_SIZE: usize = 0x0400;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IrqRevision {
    Standard,
    Alternate,
}

pub struct MMC3 {
    mirroring_locked: bool,
    bank_select: u8,
    reg_6: u8,
    reg_7: u8,
    prg_ram_enabled: bool,
    prg_ram_write_protect: bool,
    irq_latch: u8,
    irq_counter: u8,
    irq_reload: bool,
    irq_enabled: bool,
    irq_pending: bool,
    irq_revision: IrqRevision,
    prev_a12: bool,
    a12_low_ticks: u8,
    a12_transition_this_scanline: bool,
    chr_bank_registers: [u8; 8],
    prg_bank_count: usize,
    chr_bank_count: usize,
}

impl MMC3 {
    pub fn new(
        memory: &mut CartridgeMemory,
        mirroring_locked: bool,
        alternate_irq_revision: bool,
    ) -> Self {
        let mapper = Self {
            mirroring_locked,
            bank_select: 0,
            reg_6: 0,
            reg_7: 0,
            prg_ram_enabled: true,
            prg_ram_write_protect: false,
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
            chr_bank_registers: [0; 8],
            prg_bank_count: memory.prg_bank_count_8k(),
            chr_bank_count: memory.chr_bank_count(),
        };
        mapper.sync_prg_banks(memory);
        mapper.sync_chr_banks(memory);
        mapper
    }

    fn normalize_prg_bank(&self, bank: u8) -> u16 {
        (bank as usize % self.prg_bank_count) as u16
    }

    fn sync_prg_banks(&self, memory: &mut CartridgeMemory) {
        let last_bank = self.prg_bank_count - 1;
        let second_last_bank = last_bank.saturating_sub(1);
        let bank6 = self.normalize_prg_bank(self.reg_6);
        let bank7 = self.normalize_prg_bank(self.reg_7);
        let prg_mode = (self.bank_select & 0x40) != 0;

        let (slot0, slot2) = if prg_mode {
            (second_last_bank as u16, bank6)
        } else {
            (bank6, second_last_bank as u16)
        };
        memory.map_prg_8k(0, slot0);
        memory.map_prg_8k(1, bank7);
        memory.map_prg_8k(2, slot2);
        memory.map_prg_8k(3, last_bank as u16);
    }

    fn chr_normalize_bank(&self, bank: u8) -> u16 {
        (bank as usize % self.chr_bank_count) as u16
    }

    fn sync_chr_banks(&self, memory: &mut CartridgeMemory) {
        let chr_mode = (self.bank_select & 0x80) != 0;
        let r0_even = self.chr_normalize_bank(self.chr_bank_registers[0] & !1);
        let r0_odd = self.chr_normalize_bank(self.chr_bank_registers[0] | 1);
        let r1_even = self.chr_normalize_bank(self.chr_bank_registers[1] & !1);
        let r1_odd = self.chr_normalize_bank(self.chr_bank_registers[1] | 1);
        let r2 = self.chr_normalize_bank(self.chr_bank_registers[2]);
        let r3 = self.chr_normalize_bank(self.chr_bank_registers[3]);
        let r4 = self.chr_normalize_bank(self.chr_bank_registers[4]);
        let r5 = self.chr_normalize_bank(self.chr_bank_registers[5]);

        let banks = if chr_mode {
            [r2, r3, r4, r5, r0_even, r0_odd, r1_even, r1_odd]
        } else {
            [r0_even, r0_odd, r1_even, r1_odd, r2, r3, r4, r5]
        };

        for (slot, bank) in banks.into_iter().enumerate() {
            memory.map_chr_1k(slot as u16, bank);
        }
    }

    fn select_bank(&mut self, value: u8, memory: &mut CartridgeMemory) {
        self.bank_select = value;
        self.sync_prg_banks(memory);
        self.sync_chr_banks(memory);
    }

    fn write_bank_data(&mut self, value: u8, memory: &mut CartridgeMemory) {
        let target = self.bank_select & 0x07;
        match target {
            6 => self.reg_6 = value,
            7 => self.reg_7 = value,
            _ => {}
        }
        self.sync_prg_banks(memory);
        if target < 6 {
            self.chr_bank_registers[target as usize] = value;
            self.sync_chr_banks(memory);
        }
    }

    fn apply_mirroring(&self, value: u8, memory: &mut CartridgeMemory) {
        if self.mirroring_locked {
            return;
        }
        let mirroring = if value & 0x01 == 0 {
            Mirroring::Vertical
        } else {
            Mirroring::Horizontal
        };
        memory.apply_mirroring(mirroring);
    }

    fn set_prg_ram_protect(&mut self, value: u8, memory: &mut CartridgeMemory) {
        self.prg_ram_write_protect = (value & 0x40) != 0;
        self.prg_ram_enabled = (value & 0x80) != 0;
        memory.set_prg_ram_enabled(self.prg_ram_enabled);
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

impl Mapper for MMC3 {
    fn cpu_write(
        &mut self,
        address: u16,
        value: u8,
        _cycle: SystemClock,
        memory: &mut CartridgeMemory,
    ) {
        match address {
            0x6000..=0x7fff => {
                if self.prg_ram_enabled && !self.prg_ram_write_protect {
                    memory.write_prg_ram(address, value);
                }
            }
            0x8000..=0x9fff => {
                if address & 0x01 == 0 {
                    self.select_bank(value, memory);
                } else {
                    self.write_bank_data(value, memory);
                }
            }
            0xa000..=0xbfff => {
                if address & 0x01 == 0 {
                    self.apply_mirroring(value, memory);
                } else {
                    self.set_prg_ram_protect(value, memory);
                }
            }
            0xc000..=0xdfff => {
                if address & 0x01 == 0 {
                    self.irq_latch = value;
                } else {
                    self.irq_reload = true;
                }
            }
            0xe000..=0xffff => {
                if address & 0x01 == 0 {
                    self.irq_enabled = false;
                    self.irq_pending = false;
                } else {
                    self.irq_enabled = true;
                }
            }
            _ => {}
        }
    }

    fn on_ppu_tick(&mut self, scanline: u16) {
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

    fn caps(&self) -> CartridgeCaps {
        CartridgeCaps {
            on_ppu_tick: true,
            notify_vram_address: true,
            irq_pending: true,
            ppu_read: false,
            read_expansion: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn board(prg: &[u8], chr: &[u8], chr_ram_size: Option<usize>) -> (MMC3, CartridgeMemory) {
        let mut memory = CartridgeMemory::from_rom(prg, chr, chr_ram_size, Mirroring::Horizontal);
        let mapper = MMC3::new(&mut memory, false, false);
        (mapper, memory)
    }

    fn make_mmc3_with_chr() -> (MMC3, CartridgeMemory) {
        let prg = vec![0u8; PRG_ROM_BANK_SIZE * 2];
        let mut chr_data = vec![0u8; CHR_BANK_SIZE * 16];
        for bank in 0..16 {
            chr_data[bank * CHR_BANK_SIZE] = bank as u8;
        }
        board(&prg, &chr_data, None)
    }

    fn chr_ram_board() -> (MMC3, CartridgeMemory) {
        let prg = vec![0u8; PRG_ROM_BANK_SIZE * 2];
        board(&prg, &[], None)
    }

    fn setup_chr_banks(mapper: &mut MMC3, memory: &mut CartridgeMemory, banks: &[(u8, u8)]) {
        for &(bank_reg, value) in banks {
            mapper.cpu_write(0x8000, bank_reg, SystemClock::default(), memory);
            mapper.cpu_write(0x8001, value, SystemClock::default(), memory);
        }
    }

    #[test]
    fn switches_chr_layout_in_chr_mode_0() {
        let (mut mapper, mut memory) = make_mmc3_with_chr();
        setup_chr_banks(
            &mut mapper,
            &mut memory,
            &[
                (0x00, 0x06),
                (0x01, 0x08),
                (0x02, 0x0a),
                (0x03, 0x0b),
                (0x04, 0x0c),
                (0x05, 0x0d),
            ],
        );

        assert_eq!(memory.read_vram(0x0000), 0x06);
        assert_eq!(memory.read_vram(0x0400), 0x07);
        assert_eq!(memory.read_vram(0x0800), 0x08);
        assert_eq!(memory.read_vram(0x0c00), 0x09);
        assert_eq!(memory.read_vram(0x1000), 0x0a);
        assert_eq!(memory.read_vram(0x1400), 0x0b);
        assert_eq!(memory.read_vram(0x1800), 0x0c);
        assert_eq!(memory.read_vram(0x1c00), 0x0d);
    }

    #[test]
    fn switches_chr_layout_in_chr_mode_1() {
        let (mut mapper, mut memory) = make_mmc3_with_chr();
        setup_chr_banks(
            &mut mapper,
            &mut memory,
            &[
                (0x80, 0x06),
                (0x81, 0x08),
                (0x82, 0x0a),
                (0x83, 0x0b),
                (0x84, 0x0c),
                (0x85, 0x0d),
            ],
        );

        assert_eq!(memory.read_vram(0x0000), 0x0a);
        assert_eq!(memory.read_vram(0x0400), 0x0b);
        assert_eq!(memory.read_vram(0x0800), 0x0c);
        assert_eq!(memory.read_vram(0x0c00), 0x0d);
        assert_eq!(memory.read_vram(0x1000), 0x06);
        assert_eq!(memory.read_vram(0x1400), 0x07);
        assert_eq!(memory.read_vram(0x1800), 0x08);
        assert_eq!(memory.read_vram(0x1c00), 0x09);
    }

    #[test]
    fn writes_to_chr_ram_through_current_mapping() {
        let (mut mapper, mut memory) = chr_ram_board();
        mapper.cpu_write(0x8000, 0x02, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x8001, 0x03, SystemClock::default(), &mut memory);
        memory.write_vram(0x1000, 0xaa);
        assert_eq!(memory.read_vram(0x1000), 0xaa);
    }

    // big_chr_ram.nes (NES 2.0, header byte 11 = 0x09): the ROM writes tiles
    // through banks 0..31 and probes that bank 8 does not alias bank 0, so
    // the declared 32 KiB must back all 32 bank registers.
    #[test]
    fn chr_ram_spans_declared_banks_without_aliasing() {
        let prg = vec![0u8; PRG_ROM_BANK_SIZE * 2];
        let (mut mapper, mut memory) = board(&prg, &[], Some(32 * 1024));
        // R2 (1 KiB window) -> bank 0: marker; then bank 8: distinct value.
        mapper.cpu_write(0x8000, 0x02, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x8001, 0x00, SystemClock::default(), &mut memory);
        memory.write_vram(0x1000, 0x11);
        mapper.cpu_write(0x8000, 0x02, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x8001, 0x08, SystemClock::default(), &mut memory);
        memory.write_vram(0x1000, 0x22);
        assert_eq!(memory.read_vram(0x1000), 0x22);
        mapper.cpu_write(0x8000, 0x02, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x8001, 0x00, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0x1000), 0x11);
    }

    #[test]
    fn chr_ram_defaults_to_8kib_when_undeclared() {
        let (mut mapper, mut memory) = chr_ram_board();
        mapper.cpu_write(0x8000, 0x02, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x8001, 0x08, SystemClock::default(), &mut memory);
        memory.write_vram(0x1000, 0x22);
        // Bank 8 wraps onto bank 0 in an 8 KiB CHR-RAM cart.
        mapper.cpu_write(0x8000, 0x02, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x8001, 0x00, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0x1000), 0x22);
    }

    // CHR mode 1 moves the 1 KiB R2-R5 windows to $0000-$0FFF and makes R0/R1
    // 2 KiB windows at $1000-$1FFF, so CHR-RAM writes must follow whichever
    // layout the mode bit selects.
    #[test]
    fn chr_ram_write_follows_chr_mode_1_window_layout() {
        let (mut mapper, mut memory) = chr_ram_board();
        mapper.cpu_write(0x8000, 0x82, SystemClock::default(), &mut memory); // chr mode 1, R2
        mapper.cpu_write(0x8001, 0x01, SystemClock::default(), &mut memory);
        memory.write_vram(0x0000, 0x5a); // R2's 1 KiB window -> bank 1
        assert_eq!(memory.read_vram(0x0000), 0x5a);

        // The R0 pair stays a 2 KiB window at $1000-$17FF, untouched by the
        // bank 1 write.
        mapper.cpu_write(0x8000, 0x80, SystemClock::default(), &mut memory); // chr mode 1, R0
        mapper.cpu_write(0x8001, 0x00, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0x1000), 0);

        // Bank 1 and bank 0 are distinct: repointing R2 at bank 0 exposes the
        // original zeroes, not the 0x5a.
        mapper.cpu_write(0x8000, 0x82, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x8001, 0x00, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0x0000), 0);
    }

    #[test]
    fn prg_mode_swaps_fixed_and_switchable_8k_windows() {
        let mut prg = vec![0u8; PRG_ROM_BANK_SIZE * 4];
        for bank in 0..4 {
            prg[bank * PRG_ROM_BANK_SIZE] = bank as u8 + 1;
        }
        let (mut mapper, mut memory) = board(&prg, &[], None);

        // Mode 0: R6 at $8000, second-last at $C000, last at $E000.
        mapper.cpu_write(0x8000, 0x06, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x8001, 0x01, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0x02);
        assert_eq!(memory.read_prg(0xc000), 0x03);
        assert_eq!(memory.read_prg(0xe000), 0x04);

        // Mode 1: second-last at $8000, R6 at $C000.
        mapper.cpu_write(0x8000, 0x46, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 0x03);
        assert_eq!(memory.read_prg(0xc000), 0x02);
        assert_eq!(memory.read_prg(0xe000), 0x04);
    }

    fn make_mmc3(alternate_irq_revision: bool) -> (MMC3, CartridgeMemory) {
        let prg = vec![0u8; PRG_ROM_BANK_SIZE * 2];
        let mut memory = CartridgeMemory::from_rom(&prg, &[], None, Mirroring::Horizontal);
        let mapper = MMC3::new(&mut memory, false, alternate_irq_revision);
        (mapper, memory)
    }

    fn ack_irq(mapper: &mut MMC3, memory: &mut CartridgeMemory) {
        mapper.cpu_write(0xe000, 0, SystemClock::default(), memory);
        mapper.cpu_write(0xe001, 0, SystemClock::default(), memory);
    }

    // 6-MMC6.s set_test 3 / blargg's MMC3 revision A: after the counter
    // reaches 0 by decrementing, the forced reload on the next clock must
    // not re-assert the IRQ even when the latch is 0.
    #[test]
    fn alternate_revision_skips_irq_on_reload_after_natural_zero() {
        let (mut mapper, mut memory) = make_mmc3(true);
        mapper.cpu_write(0xc000, 1, SystemClock::default(), &mut memory);
        mapper.cpu_write(0xc001, 0, SystemClock::default(), &mut memory);
        ack_irq(&mut mapper, &mut memory);

        mapper.clock_irq(); // requested reload -> counter = 1
        mapper.clock_irq(); // decrement -> 0, IRQ asserted
        assert!(mapper.irq_pending);
        ack_irq(&mut mapper, &mut memory);

        mapper.cpu_write(0xc000, 0, SystemClock::default(), &mut memory);
        mapper.clock_irq(); // counter was 0 -> reload to latch 0: no IRQ
        assert!(!mapper.irq_pending);
    }

    // 6-MMC6.s set_test 2: a $C001-requested reload landing on 0 asserts
    // the IRQ on both chip revisions.
    #[test]
    fn requested_reload_to_zero_asserts_irq_on_both_revisions() {
        for alternate_irq_revision in [false, true] {
            let (mut mapper, mut memory) = make_mmc3(alternate_irq_revision);
            mapper.cpu_write(0xc000, 0, SystemClock::default(), &mut memory);
            mapper.cpu_write(0xc001, 0, SystemClock::default(), &mut memory);
            ack_irq(&mut mapper, &mut memory);

            mapper.clock_irq(); // reload request -> counter = 0: IRQ
            assert!(mapper.irq_pending);
        }
    }

    // 5-MMC3.s set_test 2 / blargg's MMC3 revision B (Standard): once the
    // latch is 0 and the counter has reached 0, every clock reloads to 0
    // and asserts the IRQ again.
    #[test]
    fn standard_revision_sets_irq_on_every_clock_with_zero_latch() {
        let (mut mapper, mut memory) = make_mmc3(false);
        mapper.cpu_write(0xc000, 0, SystemClock::default(), &mut memory);
        mapper.cpu_write(0xc001, 0, SystemClock::default(), &mut memory);
        ack_irq(&mut mapper, &mut memory);

        for _ in 0..3 {
            mapper.clock_irq();
            assert!(mapper.irq_pending);
            ack_irq(&mut mapper, &mut memory);
        }
    }
}
