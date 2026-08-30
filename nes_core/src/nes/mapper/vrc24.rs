use super::{CARTRIDGE_START_ADDR, Cartridge, CartridgeCaps, CartridgeOperation};
use crate::SystemClock;
use crate::nes::mapper::Mirroring;

const PRG_BANK_SIZE: usize = 0x2000;
const PRG_RAM_SIZE: usize = 0x2000;
const PPU_DOTS_PER_SCANLINE: u16 = 341;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VrcVariant {
    Vrc4a,
    Vrc4c,
    Vrc2a,
    Vrc4f,
    Vrc4e,
    Vrc2b,
    Vrc4b,
    Vrc4d,
    Vrc2c,
}

impl VrcVariant {
    pub fn is_vrc4(self) -> bool {
        matches!(
            self,
            Self::Vrc4a | Self::Vrc4c | Self::Vrc4f | Self::Vrc4e | Self::Vrc4b | Self::Vrc4d
        )
    }

    pub fn chr_shift_low_bit(self) -> bool {
        matches!(self, Self::Vrc2a)
    }

    pub fn bit_positions(self) -> (u8, u8) {
        match self {
            Self::Vrc4a => (1, 2),
            Self::Vrc4c => (6, 7),
            Self::Vrc2a => (1, 0),
            Self::Vrc4f => (0, 1),
            Self::Vrc4e => (2, 3),
            Self::Vrc2b => (0, 1),
            Self::Vrc4b => (1, 0),
            Self::Vrc4d => (3, 2),
            Self::Vrc2c => (1, 0),
        }
    }
}

pub struct Vrc24 {
    prg_rom: Vec<u8>,
    prg_ram: [u8; PRG_RAM_SIZE],
    prg_ram_enabled: bool,
    variant: VrcVariant,
    s0: u8,
    s1: u8,
    prg_select_0: u8,
    prg_select_1: u8,
    mirroring_bits: u8,
    prg_swap_mode: bool,
    irq_latch: u8,
    irq_counter: u8,
    irq_prescaler: u16,
    irq_mode: bool,
    irq_enable: bool,
    irq_enable_after_ack: bool,
    irq_pending: bool,
    microwire_latch: u8,
    chr_source: Vec<u8>,
    has_chr_ram: bool,
    chr_nibbles: [u8; 16],
    chr_bank_offsets: [usize; 8],
}

impl Vrc24 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8], variant: VrcVariant) -> Self {
        debug_assert!(!prg_rom.is_empty());

        let (s0, s1) = variant.bit_positions();
        let has_chr_ram = chr_rom.is_empty();
        let size = if has_chr_ram { 0x2000 } else { chr_rom.len() };
        let mut chr_source = vec![0; size];
        chr_source[..chr_rom.len()].copy_from_slice(chr_rom);

        let mut mapper = Self {
            prg_rom: prg_rom.to_vec(),
            prg_ram: [0; PRG_RAM_SIZE],
            prg_ram_enabled: variant.is_vrc4(),
            variant,
            s0,
            s1,
            prg_select_0: 0,
            prg_select_1: 0,
            mirroring_bits: 0,
            prg_swap_mode: false,
            irq_latch: 0,
            irq_counter: 0,
            irq_prescaler: 0,
            irq_mode: false,
            irq_enable: false,
            irq_enable_after_ack: false,
            irq_pending: false,
            microwire_latch: 0,
            chr_source,
            has_chr_ram,
            chr_nibbles: [0; 16],
            chr_bank_offsets: [0; 8],
        };
        mapper.chr_refresh_banks();
        mapper
    }

    fn prg_bank_count(&self) -> usize {
        self.prg_rom.len() / PRG_BANK_SIZE
    }

    fn compute_register_index(&self, address: u16) -> usize {
        let bit0 = ((address >> self.s0) & 1) as usize;
        let bit1 = ((address >> self.s1) & 1) as usize;
        bit1 << 1 | bit0
    }

    fn normalize_prg_bank(&self, bank: u8) -> usize {
        (bank as usize) % self.prg_bank_count().max(1)
    }

    fn decrement_irq_counter(&mut self) {
        self.irq_counter = self.irq_counter.wrapping_sub(1);
        if self.irq_counter == 0 {
            self.irq_pending = true;
            self.reload_irq_counter();
        }
    }

    fn reload_irq_counter(&mut self) {
        self.irq_counter = self.irq_latch.wrapping_neg();
    }

    fn chr_bank_count(&self) -> usize {
        if self.chr_source.is_empty() {
            1
        } else {
            self.chr_source.len() / 0x0400
        }
    }

    fn chr_refresh_banks(&mut self) {
        for slot in 0..8 {
            let lo = self.chr_nibbles[slot * 2] as u16;
            let hi = self.chr_nibbles[slot * 2 + 1] as u16;
            let mut bank = lo | (hi << 4);
            if self.variant.is_vrc4() {
                bank &= 0x1ff;
            } else {
                bank &= 0x0ff;
            }
            if self.variant.chr_shift_low_bit() {
                bank >>= 1;
            }
            self.chr_bank_offsets[slot] = (bank as usize % self.chr_bank_count()) * 0x0400;
        }
    }
}

impl Cartridge for Vrc24 {
    fn read(&self, address: u16) -> u8 {
        match address {
            CARTRIDGE_START_ADDR..=0x5fff => 0,
            0x6000..=0x6fff => {
                if self.variant.is_vrc4() {
                    if self.prg_ram_enabled {
                        self.prg_ram[(address - 0x6000) as usize]
                    } else {
                        0
                    }
                } else {
                    (address as u8) | self.microwire_latch
                }
            }
            0x7000..=0x7fff => {
                let idx = (address as usize - 0x7000) % PRG_RAM_SIZE;
                if self.variant.is_vrc4() && self.prg_ram_enabled {
                    self.prg_ram[idx]
                } else {
                    0
                }
            }
            0x8000..=0x9fff => {
                if self.prg_swap_mode {
                    let bank = self.prg_bank_count().saturating_sub(2);
                    let offset = address as usize - 0x8000;
                    self.prg_rom[bank * PRG_BANK_SIZE + offset]
                } else {
                    let bank = self.normalize_prg_bank(self.prg_select_0);
                    let offset = address as usize - 0x8000;
                    self.prg_rom[bank * PRG_BANK_SIZE + offset]
                }
            }
            0xa000..=0xbfff => {
                let bank = self.normalize_prg_bank(self.prg_select_1);
                let offset = address as usize - 0xa000;
                self.prg_rom[bank * PRG_BANK_SIZE + offset]
            }
            0xc000..=0xdfff => {
                if self.prg_swap_mode {
                    let bank = self.normalize_prg_bank(self.prg_select_0);
                    let offset = address as usize - 0xc000;
                    self.prg_rom[bank * PRG_BANK_SIZE + offset % PRG_BANK_SIZE]
                } else {
                    let bank = self.prg_bank_count().saturating_sub(2);
                    let offset = address as usize - 0xc000;
                    self.prg_rom[bank * PRG_BANK_SIZE + offset]
                }
            }
            0xe000..=0xffff => {
                if self.variant.is_vrc4() {
                    let bank = self.prg_bank_count().saturating_sub(1);
                    let offset = address as usize - 0xe000;
                    self.prg_rom[bank * PRG_BANK_SIZE + offset]
                } else {
                    let last_16k = self.prg_bank_count().saturating_sub(2);
                    let offset = address as usize - 0xc000;
                    self.prg_rom[last_16k * PRG_BANK_SIZE + offset]
                }
            }
            _ => 0,
        }
    }

    fn write(&mut self, address: u16, value: u8, _cycle: SystemClock) -> CartridgeOperation {
        match address {
            CARTRIDGE_START_ADDR..=0x5fff => CartridgeOperation::None,
            0x6000..=0x7fff => {
                if self.variant.is_vrc4() {
                    if self.prg_ram_enabled {
                        self.prg_ram[(address - 0x6000) as usize % PRG_RAM_SIZE] = value;
                    }
                } else {
                    self.microwire_latch = value & 0x01;
                }
                CartridgeOperation::None
            }
            0x8000..=0x8fff => {
                self.prg_select_0 = value & 0x1f;
                CartridgeOperation::None
            }
            0x9000..=0x9fff => {
                let idx = self.compute_register_index(address);
                if self.variant.is_vrc4() && idx == 2 {
                    self.prg_ram_enabled = (value & 0x01) != 0;
                    self.prg_swap_mode = (value & 0x02) != 0;
                    CartridgeOperation::None
                } else {
                    self.mirroring_bits = value & 0x03;
                    let mirroring = if self.variant.is_vrc4() {
                        match value & 0x03 {
                            0 => Mirroring::Vertical,
                            1 => Mirroring::Horizontal,
                            2 => Mirroring::LowerBank,
                            3 => Mirroring::UpperBank,
                            _ => unreachable!(),
                        }
                    } else {
                        if value & 0x01 == 0 {
                            Mirroring::Vertical
                        } else {
                            Mirroring::Horizontal
                        }
                    };
                    CartridgeOperation::UpdateNametableMirroring(mirroring)
                }
            }
            0xa000..=0xafff => {
                self.prg_select_1 = value & 0x1f;
                CartridgeOperation::None
            }
            0xb000..=0xefff => {
                let group = ((address >> 12) & 0x0f) as usize;
                let idx = self.compute_register_index(address);
                let slot = (group - 0xb) * 2 + (idx >> 1);
                if idx & 1 == 0 {
                    self.chr_nibbles[slot * 2] = value & 0x0f;
                } else {
                    self.chr_nibbles[slot * 2 + 1] = value & 0x1f;
                }
                self.chr_refresh_banks();
                CartridgeOperation::None
            }
            0xf000..=0xffff => {
                if self.variant.is_vrc4() {
                    let idx = self.compute_register_index(address);
                    match idx {
                        0 => {
                            self.irq_latch = (self.irq_latch & 0xf0) | (value & 0x0f);
                        }
                        1 => {
                            self.irq_latch = (self.irq_latch & 0x0f) | ((value & 0x0f) << 4);
                        }
                        2 => {
                            // nesdev wiki, VRC_IRQ: "Any write to this register
                            // will acknowledge the pending IRQ and reset the
                            // prescaler. If this register is written to with 'E'
                            // set, the IRQ counter is reloaded with the latch
                            // value. If 'E' is clear, the IRQ counter remains
                            // unchanged."
                            self.irq_mode = (value & 0x04) != 0;
                            self.irq_enable_after_ack = (value & 0x01) != 0;
                            self.irq_enable = (value & 0x02) != 0;
                            self.irq_pending = false;
                            self.irq_prescaler = 0;
                            if self.irq_enable {
                                self.reload_irq_counter();
                            }
                        }
                        3 => {
                            // nesdev wiki, VRC_IRQ: "Any write to this register
                            // will acknowledge the pending IRQ. In addition, the
                            // 'A' control bit moves to the 'E' control bit...
                            // Writes to this register do not affect the current
                            // state of the IRQ counter or prescaler." 'A' itself
                            // persists until the next Control write (copy, not
                            // move), so back-to-back acknowledges are idempotent.
                            self.irq_pending = false;
                            self.irq_enable = self.irq_enable_after_ack;
                        }
                        _ => {}
                    }
                }
                CartridgeOperation::None
            }
            _ => CartridgeOperation::None,
        }
    }

    fn on_ppu_tick(&mut self, _scanline: u16) {
        // nesdev wiki, VRC_IRQ: "If IRQs are disabled ('E' bit clear),
        // neither the prescaler nor IRQ counter gets clocked." Freezing
        // mid-interval matters: an acknowledge must leave a partially
        // elapsed scanline phase intact for the next enable.
        if !self.variant.is_vrc4() || !self.irq_enable {
            return;
        }

        if self.irq_mode {
            self.irq_prescaler += 1;
            if self.irq_prescaler >= 3 {
                self.irq_prescaler -= 3;
                self.decrement_irq_counter();
            }
        } else {
            self.irq_prescaler += 1;
            if self.irq_prescaler >= PPU_DOTS_PER_SCANLINE {
                self.irq_prescaler -= PPU_DOTS_PER_SCANLINE;
                self.decrement_irq_counter();
            }
        }
    }

    fn irq_pending(&self) -> bool {
        self.irq_pending
    }

    fn prg_ram_enabled(&self) -> bool {
        // VRC2 has no PRG-RAM, VRC4's enable lives in $F002
        !self.variant.is_vrc4() || self.prg_ram_enabled
    }

    fn ppu_capabilities(&self) -> CartridgeCaps {
        CartridgeCaps {
            on_ppu_tick: true,
            notify_vram_address: false,
            irq_pending: true,
        }
    }

    fn read_chr(&self, address: u16) -> u8 {
        let addr = address as usize % 0x2000;
        let slot = addr / 0x0400;
        let offset = addr % 0x0400;
        let src = self.chr_bank_offsets[slot] + offset;
        let len = self.chr_source.len();
        self.chr_source[src % len]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        if !self.has_chr_ram {
            return;
        }
        let addr = address as usize % 0x2000;
        let slot = addr / 0x0400;
        let offset = addr % 0x0400;
        let src = self.chr_bank_offsets[slot] + offset;
        let len = self.chr_source.len();
        self.chr_source[src % len] = value;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_vrc24_with_chr() -> Vrc24 {
        let bank_count = 32;
        let mut chr_data = vec![0u8; bank_count * 0x0400];
        for bank in 0..bank_count {
            chr_data[bank * 0x0400] = bank as u8;
        }
        let prg = vec![0u8; PRG_BANK_SIZE * 2];
        Vrc24::new(&prg, &chr_data, VrcVariant::Vrc4f)
    }

    #[test]
    fn vrc4f_banking() {
        let mut mapper = make_vrc24_with_chr();
        mapper.write(0xb000, 0x02, SystemClock::default());
        assert_eq!(mapper.read_chr(0x0000), 0x02);
    }

    #[test]
    fn vrc4f_high_nibble() {
        let mut mapper = make_vrc24_with_chr();
        mapper.write(0xb001, 0x01, SystemClock::default());
        assert_eq!(mapper.read_chr(0x0000), 0x10);
    }

    #[test]
    fn writes_to_chr_ram() {
        let prg = vec![0u8; PRG_BANK_SIZE * 2];
        let mut mapper = Vrc24::new(&prg, &[], VrcVariant::Vrc4f);
        mapper.write_chr(0x0000, 0xab);
        assert_eq!(mapper.read_chr(0x0000), 0xab);
    }

    // --- IRQ pins: black-box, driving only the Cartridge surface ---

    const VARIANT: VrcVariant = VrcVariant::Vrc4f;

    fn irq_addr(idx: usize) -> u16 {
        let (s0, s1) = VARIANT.bit_positions();
        0xf000u16 | (((idx & 1) as u16) << s0) | (((idx >> 1) as u16) << s1)
    }

    fn write_irq_latch(mapper: &mut Vrc24, latch: u8) {
        mapper.write(irq_addr(0), latch & 0x0f, SystemClock::default());
        mapper.write(irq_addr(1), latch >> 4, SystemClock::default());
    }

    fn write_irq_control(mapper: &mut Vrc24, control: u8) {
        mapper.write(irq_addr(2), control, SystemClock::default());
    }

    /// First window-relative tick at which the IRQ level asserts, if it does.
    fn first_pending_tick(mapper: &mut Vrc24, ticks: usize) -> Option<usize> {
        for tick in 1..=ticks {
            mapper.on_ppu_tick(0);
            if mapper.irq_pending() {
                return Some(tick);
            }
        }
        None
    }

    fn acknowledge_irq(mapper: &mut Vrc24) {
        mapper.write(irq_addr(3), 0, SystemClock::default());
    }

    /// Drives `ticks` PPU dots, returning the 1-based tick indices at which
    /// the IRQ line was observed asserted. Does not acknowledge.
    fn drive(mapper: &mut Vrc24, ticks: usize) -> Vec<usize> {
        let mut edges = Vec::new();
        for tick in 1..=ticks {
            mapper.on_ppu_tick(0);
            if mapper.irq_pending() {
                edges.push(tick);
            }
        }
        edges
    }

    #[test]
    fn disabled_irq_never_clocks() {
        let mut mapper = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, 0xfe);
        write_irq_control(&mut mapper, 0x04); // cycle mode, 'E' clear
        assert!(drive(&mut mapper, 100_000).is_empty());
    }

    #[test]
    fn reenabling_control_write_rearms_full_period() {
        let mut mapper = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, 0xfe); // cycle-mode period: 3*(256-254) = 6 ticks
        write_irq_control(&mut mapper, 0x07); // M|E|A
        drive(&mut mapper, 3); // mid-count
        write_irq_control(&mut mapper, 0x07); // re-sync: reload + phase reset
        assert_eq!(drive(&mut mapper, 5), Vec::<usize>::new());
        assert_eq!(
            first_pending_tick(&mut mapper, 8),
            Some(1) // full period after the write (global tick 9)
        );
    }

    #[test]
    fn acknowledge_preserves_schedule() {
        let mut mapper = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, 0xfa); // scanline period: 341*(256-250) = 2046 dots
        write_irq_control(&mut mapper, 0x03); // scanline mode, E|A
        drive(&mut mapper, 1000); // mid-interval
        acknowledge_irq(&mut mapper);
        assert_eq!(drive(&mut mapper, 1046), vec![1046]); // edge at total tick 2046
    }

    #[test]
    fn control_write_resyncs_phase_and_counter() {
        let mut mapper = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, 0xfe); // scanline period: 341*2 = 682 dots
        write_irq_control(&mut mapper, 0x03);
        drive(&mut mapper, 340); // one dot short of the first decrement
        write_irq_control(&mut mapper, 0x03); // re-sync resets phase
        assert_eq!(drive(&mut mapper, 681), Vec::<usize>::new());
        assert_eq!(drive(&mut mapper, 1), vec![1]); // global tick 1022
    }

    #[test]
    fn repeated_irq_spacing_follows_latch() {
        let mut mapper = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, 0xfe); // interval 3*2 = 6 ticks
        write_irq_control(&mut mapper, 0x07); // M|E|A: repeats via copy-on-acknowledge
        let mut edges = Vec::new();
        for tick in 1..=20 {
            mapper.on_ppu_tick(0);
            if mapper.irq_pending() {
                edges.push(tick);
                acknowledge_irq(&mut mapper); // 'A'=1 keeps 'E' set: re-trips
            }
        }
        assert_eq!(edges, vec![6, 12, 18]); // uniform interval: 3*(256-latch)
    }

    #[test]
    fn acknowledge_with_a_clear_disables() {
        let mut mapper = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, 0xfe);
        write_irq_control(&mut mapper, 0x06); // M|E, 'A' clear: one-shot
        assert_eq!(drive(&mut mapper, 6), vec![6]);
        acknowledge_irq(&mut mapper); // 'A'=0 moves to 'E': disabled
        assert!(drive(&mut mapper, 10_000).is_empty());
    }

    #[test]
    fn scanline_mode_uniform_period() {
        let mut mapper = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, 0xfe); // 2 decrements of 341 dots
        write_irq_control(&mut mapper, 0x03);
        assert_eq!(drive(&mut mapper, 681), Vec::<usize>::new());
        assert_eq!(drive(&mut mapper, 1), vec![1]); // global tick 682
    }
}
