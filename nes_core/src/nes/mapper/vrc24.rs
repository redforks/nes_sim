use super::{Cartridge, CartridgeCaps, CARTRIDGE_START_ADDR, CartridgeOperation};
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
            self.irq_counter = 0xff;
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

    fn write(&mut self, address: u16, value: u8) -> CartridgeOperation {
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
                            self.irq_mode = (value & 0x04) != 0;
                            self.reload_irq_counter();
                            self.irq_prescaler = 0;
                            self.irq_pending = false;
                        }
                        3 => {
                            self.irq_pending = false;
                            self.reload_irq_counter();
                            self.irq_prescaler = 0;
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
        if !self.variant.is_vrc4() {
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
        mapper.write(0xb000, 0x02);
        assert_eq!(mapper.read_chr(0x0000), 0x02);
    }

    #[test]
    fn vrc4f_high_nibble() {
        let mut mapper = make_vrc24_with_chr();
        mapper.write(0xb001, 0x01);
        assert_eq!(mapper.read_chr(0x0000), 0x10);
    }

    #[test]
    fn writes_to_chr_ram() {
        let prg = vec![0u8; PRG_BANK_SIZE * 2];
        let mut mapper = Vrc24::new(&prg, &[], VrcVariant::Vrc4f);
        mapper.write_chr(0x0000, 0xab);
        assert_eq!(mapper.read_chr(0x0000), 0xab);
    }
}
