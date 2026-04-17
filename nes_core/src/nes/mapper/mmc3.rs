use super::CARTRIDGE_START_ADDR;
use crate::nes::mapper::Mirroring;
use crate::nes::mapper::NameTableControl;

const PRG_RAM_SIZE: usize = 0x2000;
const PRG_ROM_BANK_SIZE: usize = 0x2000;
const CHR_BANK_SIZE: usize = 0x0400;
const CHR_WINDOW_SIZE: usize = 0x2000;

pub struct MMC3 {
    prg_rom: Vec<u8>,
    prg_ram: [u8; PRG_RAM_SIZE],
    chr_mem: Vec<u8>,
    current_chr: [u8; CHR_WINDOW_SIZE],
    has_chr_ram: bool,
    mirroring_locked: bool,
    name_table: NameTableControl,
    bank_select: u8,
    bank_registers: [u8; 8],
    prg_ram_enabled: bool,
    prg_ram_write_protect: bool,
    prg_offsets: [usize; 4],
    chr_offsets: [usize; 8],
    irq_latch: u8,
    irq_counter: u8,
    irq_reload: bool,
    irq_enabled: bool,
    irq_pending: bool,
    prev_a12: bool,
    a12_transition_this_scanline: bool,
}

impl MMC3 {
    pub fn new(
        prg_rom: &[u8],
        chr_rom: &[u8],
        mirroring: Mirroring,
        mirroring_locked: bool,
    ) -> Self {
        debug_assert!(!prg_rom.is_empty());
        debug_assert_eq!(prg_rom.len() % PRG_ROM_BANK_SIZE, 0);
        debug_assert_eq!(chr_rom.len() % CHR_BANK_SIZE, 0);

        let has_chr_ram = chr_rom.is_empty();
        let chr_size = if has_chr_ram {
            CHR_WINDOW_SIZE
        } else {
            chr_rom.len()
        };
        let mut chr_mem = vec![0; chr_size];
        chr_mem[..chr_rom.len()].copy_from_slice(chr_rom);

        let mut mapper = Self {
            prg_rom: prg_rom.to_vec(),
            prg_ram: [0; PRG_RAM_SIZE],
            chr_mem,
            current_chr: [0; CHR_WINDOW_SIZE],
            has_chr_ram,
            mirroring_locked,
            name_table: NameTableControl::new(mirroring),
            bank_select: 0,
            bank_registers: [0; 8],
            prg_ram_enabled: true,
            prg_ram_write_protect: false,
            prg_offsets: [0; 4],
            chr_offsets: [0; 8],
            irq_latch: 0,
            irq_counter: 0,
            irq_reload: false,
            irq_enabled: false,
            irq_pending: false,
            prev_a12: false,
            a12_transition_this_scanline: false,
        };
        mapper.sync_banks();
        mapper
    }

    fn prg_bank_count(&self) -> usize {
        self.prg_rom.len() / PRG_ROM_BANK_SIZE
    }

    fn chr_bank_count(&self) -> usize {
        self.chr_mem.len() / CHR_BANK_SIZE
    }

    fn normalize_prg_bank(&self, bank: u8) -> usize {
        (bank as usize) % self.prg_bank_count()
    }

    fn normalize_chr_bank(&self, bank: u8) -> usize {
        (bank as usize) % self.chr_bank_count()
    }

    fn set_prg_slot(&mut self, slot: usize, bank: usize) {
        self.prg_offsets[slot] = bank * PRG_ROM_BANK_SIZE;
    }

    fn set_chr_slot(&mut self, slot: usize, bank: usize) {
        self.chr_offsets[slot] = bank * CHR_BANK_SIZE;
    }

    fn sync_banks(&mut self) {
        self.sync_prg_banks();
        self.sync_chr_banks();
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

    fn sync_chr_banks(&mut self) {
        let chr_mode = (self.bank_select & 0x80) != 0;
        let r0_even = self.normalize_chr_bank(self.bank_registers[0] & !1);
        let r0_odd = self.normalize_chr_bank(self.bank_registers[0] | 1);
        let r1_even = self.normalize_chr_bank(self.bank_registers[1] & !1);
        let r1_odd = self.normalize_chr_bank(self.bank_registers[1] | 1);
        let r2 = self.normalize_chr_bank(self.bank_registers[2]);
        let r3 = self.normalize_chr_bank(self.bank_registers[3]);
        let r4 = self.normalize_chr_bank(self.bank_registers[4]);
        let r5 = self.normalize_chr_bank(self.bank_registers[5]);

        let banks = if chr_mode {
            [r2, r3, r4, r5, r0_even, r0_odd, r1_even, r1_odd]
        } else {
            [r0_even, r0_odd, r1_even, r1_odd, r2, r3, r4, r5]
        };

        for (slot, bank) in banks.into_iter().enumerate() {
            self.set_chr_slot(slot, bank);
        }

        for slot in 0..self.chr_offsets.len() {
            let source = self.chr_offsets[slot];
            let dest = slot * CHR_BANK_SIZE;
            self.current_chr[dest..dest + CHR_BANK_SIZE]
                .copy_from_slice(&self.chr_mem[source..source + CHR_BANK_SIZE]);
        }
    }

    fn read_prg(&self, slot: usize, address: u16) -> u8 {
        let offset = self.prg_offsets[slot] + ((address as usize) % PRG_ROM_BANK_SIZE);
        self.prg_rom[offset]
    }

    fn select_bank(&mut self, value: u8) {
        self.bank_select = value;
        self.sync_banks();
    }

    fn write_bank_data(&mut self, value: u8) {
        let target = (self.bank_select & 0x07) as usize;
        self.bank_registers[target] = value;
        self.sync_banks();
    }

    fn set_mirroring(&mut self, value: u8) {
        if self.mirroring_locked {
            return;
        }

        let mirroring = if value & 0x01 == 0 {
            Mirroring::Vertical
        } else {
            Mirroring::Horizontal
        };
        self.name_table.set_mirroring(mirroring);
    }

    fn set_prg_ram_protect(&mut self, value: u8) {
        self.prg_ram_write_protect = (value & 0x40) != 0;
        self.prg_ram_enabled = (value & 0x80) != 0;
    }

    fn clock_irq(&mut self) {
        // Save state before modifying
        let reload = self.irq_counter == 0 || self.irq_reload;
        let prev_counter = self.irq_counter;
        let prev_reload_flag = self.irq_reload;

        if reload {
            self.irq_counter = self.irq_latch;
            self.irq_reload = false;
        } else {
            self.irq_counter -= 1;
        }

        // Determine if we should set IRQ
        // MMC3: Always set IRQ when reloading to 0
        // MMC6: Don't set IRQ when reloading to 0 if reload flag was just set this clock
        let should_set_irq = if self.irq_enabled && self.irq_counter == 0 {
            if !reload {
                // Decrement case: only if we went from 1 to 0
                prev_counter == 1
            } else if !prev_reload_flag {
                // MMC6: reload flag was just set this clock, don't set IRQ when reloading to 0
                false
            } else {
                // MMC3/MMC6: normal reload to 0 (reload flag was already set)
                self.irq_latch == 0
            }
        } else {
            false
        };

        if should_set_irq {
            self.irq_pending = true;
        }
    }
}

impl MMC3 {
    pub fn pattern_ref(&self) -> &[u8] {
        &self.current_chr
    }

    pub fn write_pattern(&mut self, address: u16, value: u8) {
        if !self.has_chr_ram {
            return;
        }

        let addr = address as usize % CHR_WINDOW_SIZE;
        let slot = addr / CHR_BANK_SIZE;
        let offset_in_bank = addr % CHR_BANK_SIZE;
        let source = self.chr_offsets[slot] + offset_in_bank;
        self.chr_mem[source] = value;
        self.current_chr[addr] = value;
    }

    pub fn read(&mut self, address: u16) -> u8 {
        self.peek(address)
    }

    pub fn peek(&self, address: u16) -> u8 {
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

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            CARTRIDGE_START_ADDR..=0x5fff => {}
            0x6000..=0x7fff => {
                if self.prg_ram_enabled && !self.prg_ram_write_protect {
                    self.prg_ram[(address - 0x6000) as usize] = value;
                }
            }
            0x8000..=0x9fff => {
                if address & 0x01 == 0 {
                    self.select_bank(value);
                } else {
                    self.write_bank_data(value);
                }
            }
            0xa000..=0xbfff => {
                if address & 0x01 == 0 {
                    self.set_mirroring(value);
                } else {
                    self.set_prg_ram_protect(value);
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
            _ => unreachable!(),
        }
    }

    pub fn on_ppu_tick(&mut self, scanline: u16, _dot: u16, _rendering_enabled: bool) {
        // Reset A12 transition flag at the end of each scanline
        if scanline == 261 || scanline == 239 {
            self.a12_transition_this_scanline = false;
        }
    }

    pub fn notify_vram_address(&mut self, addr: u16) {
        let a12 = (addr & 0x1000) != 0;
        // Clock only on rising edges (0→1 transitions) of A12
        if a12 && !self.prev_a12 {
            self.a12_transition_this_scanline = true;
            self.clock_irq();
        }
        self.prev_a12 = a12;
    }

    pub fn irq_pending(&self) -> bool {
        self.irq_pending
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

    fn create_prg() -> [u8; PRG_ROM_BANK_SIZE * 8] {
        let mut prg = [0u8; PRG_ROM_BANK_SIZE * 8];
        for bank in 0..8 {
            prg[bank * PRG_ROM_BANK_SIZE] = bank as u8;
        }
        prg[(8 * PRG_ROM_BANK_SIZE) - 1] = 0xfe;
        prg
    }

    fn create_chr() -> [u8; CHR_BANK_SIZE * 16] {
        let mut chr = [0u8; CHR_BANK_SIZE * 16];
        for bank in 0..16 {
            chr[bank * CHR_BANK_SIZE] = bank as u8;
        }
        chr
    }

    #[test]
    fn switches_prg_bank_in_normal_mode() {
        let prg = create_prg();
        let mut mapper = MMC3::new(&prg, &create_chr(), Mirroring::Horizontal, false);

        mapper.write(0x8000, 0x06);
        mapper.write(0x8001, 0x03);
        mapper.write(0x8000, 0x07);
        mapper.write(0x8001, 0x04);

        assert_eq!(mapper.read(0x8000), 0x03);
        assert_eq!(mapper.read(0xa000), 0x04);
        assert_eq!(mapper.read(0xc000), 0x06);
        assert_eq!(mapper.read(0xe000), 0x07);
        assert_eq!(mapper.read(0xffff), 0xfe);
    }

    #[test]
    fn swaps_fixed_and_switchable_prg_regions_in_prg_mode_1() {
        let prg = create_prg();
        let mut mapper = MMC3::new(&prg, &create_chr(), Mirroring::Horizontal, false);

        mapper.write(0x8000, 0x46);
        mapper.write(0x8001, 0x03);
        mapper.write(0x8000, 0x47);
        mapper.write(0x8001, 0x04);

        assert_eq!(mapper.read(0x8000), 0x06);
        assert_eq!(mapper.read(0xa000), 0x04);
        assert_eq!(mapper.read(0xc000), 0x03);
        assert_eq!(mapper.read(0xe000), 0x07);
    }

    #[test]
    fn switches_chr_layout_in_chr_mode_0() {
        let chr = create_chr();
        let mut mapper = MMC3::new(&create_prg(), &chr, Mirroring::Horizontal, false);

        mapper.write(0x8000, 0x00);
        mapper.write(0x8001, 0x06);
        mapper.write(0x8000, 0x01);
        mapper.write(0x8001, 0x08);
        mapper.write(0x8000, 0x02);
        mapper.write(0x8001, 0x0a);
        mapper.write(0x8000, 0x03);
        mapper.write(0x8001, 0x0b);
        mapper.write(0x8000, 0x04);
        mapper.write(0x8001, 0x0c);
        mapper.write(0x8000, 0x05);
        mapper.write(0x8001, 0x0d);

        assert_eq!(mapper.pattern_ref()[0x0000], 0x06);
        assert_eq!(mapper.pattern_ref()[0x0400], 0x07);
        assert_eq!(mapper.pattern_ref()[0x0800], 0x08);
        assert_eq!(mapper.pattern_ref()[0x0c00], 0x09);
        assert_eq!(mapper.pattern_ref()[0x1000], 0x0a);
        assert_eq!(mapper.pattern_ref()[0x1400], 0x0b);
        assert_eq!(mapper.pattern_ref()[0x1800], 0x0c);
        assert_eq!(mapper.pattern_ref()[0x1c00], 0x0d);
    }

    #[test]
    fn switches_chr_layout_in_chr_mode_1() {
        let chr = create_chr();
        let mut mapper = MMC3::new(&create_prg(), &chr, Mirroring::Horizontal, false);

        mapper.write(0x8000, 0x80);
        mapper.write(0x8001, 0x06);
        mapper.write(0x8000, 0x81);
        mapper.write(0x8001, 0x08);
        mapper.write(0x8000, 0x82);
        mapper.write(0x8001, 0x0a);
        mapper.write(0x8000, 0x83);
        mapper.write(0x8001, 0x0b);
        mapper.write(0x8000, 0x84);
        mapper.write(0x8001, 0x0c);
        mapper.write(0x8000, 0x85);
        mapper.write(0x8001, 0x0d);

        assert_eq!(mapper.pattern_ref()[0x0000], 0x0a);
        assert_eq!(mapper.pattern_ref()[0x0400], 0x0b);
        assert_eq!(mapper.pattern_ref()[0x0800], 0x0c);
        assert_eq!(mapper.pattern_ref()[0x0c00], 0x0d);
        assert_eq!(mapper.pattern_ref()[0x1000], 0x06);
        assert_eq!(mapper.pattern_ref()[0x1400], 0x07);
        assert_eq!(mapper.pattern_ref()[0x1800], 0x08);
        assert_eq!(mapper.pattern_ref()[0x1c00], 0x09);
    }

    #[test]
    fn writes_to_chr_ram_through_current_mapping() {
        let mut mapper = MMC3::new(&create_prg(), &[], Mirroring::Horizontal, false);

        mapper.write(0x8000, 0x02);
        mapper.write(0x8001, 0x03);
        mapper.write_pattern(0x1000, 0xaa);

        assert_eq!(mapper.pattern_ref()[0x1000], 0xaa);
    }

    #[test]
    fn changes_mirroring_unless_four_screen_is_forced() {
        let mut mapper = MMC3::new(&create_prg(), &create_chr(), Mirroring::Horizontal, false);

        mapper.write(0xa000, 0x00);
        mapper.write_nametable(0x2400, 0x12);
        assert_eq!(mapper.read_nametable(0x2c00), 0x12);

        mapper.write(0xa000, 0x01);
        mapper.write_nametable(0x2800, 0x34);
        assert_eq!(mapper.read_nametable(0x2c00), 0x34);

        let mut locked_mapper = MMC3::new(&create_prg(), &create_chr(), Mirroring::Four, true);
        locked_mapper.write(0xa000, 0x01);
        locked_mapper.write_nametable(0x2c00, 0x56);
        assert_eq!(locked_mapper.read_nametable(0x2c00), 0x56);
        assert_eq!(locked_mapper.read_nametable(0x2000), 0x00);
    }

    #[test]
    fn triggers_irq_after_scanline_clocks() {
        let mut mapper = MMC3::new(&create_prg(), &create_chr(), Mirroring::Horizontal, false);

        mapper.write(0xc000, 0x01); // Set IRQ latch to 1
        mapper.write(0xc001, 0x00); // Set irq_reload flag
        mapper.write(0xe001, 0x00); // Enable IRQ

        // Simulate A12 transitions to clock the IRQ
        // The MMC3 clocks IRQ on rising edges of A12 (when it goes from 0 to 1)
        // With latch=1, we need 2 clocks: first reloads counter to 1, second decrements to 0
        mapper.notify_vram_address(0x0fff); // A12 = 0
        mapper.notify_vram_address(0x1000); // A12 = 1 (rising edge, reloads counter to 1)
        mapper.notify_vram_address(0x0fff); // A12 = 0
        mapper.notify_vram_address(0x1000); // A12 = 1 (rising edge, decrements counter to 0, IRQ pending)

        assert!(mapper.irq_pending()); // Should be pending because counter reached 0

        mapper.write(0xe000, 0x00); // Disable IRQ and clear pending
        assert!(!mapper.irq_pending());
    }
}
