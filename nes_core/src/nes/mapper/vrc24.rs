use super::{CARTRIDGE_START_ADDR, CartridgeOperation};
use crate::nes::mapper::Mirroring;

/// PRG ROM bank size: 8 KiB
const PRG_BANK_SIZE: usize = 0x2000;
/// CHR bank size: 1 KiB
const CHR_BANK_SIZE: usize = 0x0400;
/// CHR window size: 8 KiB
const CHR_WINDOW_SIZE: usize = 0x2000;
/// PRG RAM size: 8 KiB
const PRG_RAM_SIZE: usize = 0x2000;
/// Number of PPU dots per scanline
const PPU_DOTS_PER_SCANLINE: u16 = 341;

/// Pin connection configuration for different VRC2/VRC4 board variants.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VrcVariant {
    /// VRC4a: A1 pin3, A2 pin4 => reg offsets $x000, $x002, $x004, $x006
    Vrc4a,
    /// VRC4c: A6 pin3, A7 pin4 => reg offsets $x000, $x040, $x080, $x0C0
    Vrc4c,
    /// VRC2a: A1 pin3, A0 pin4 => reg offsets $x000, $x002, $x001, $x003, CHR low bit ignored
    Vrc2a,
    /// VRC4f: A0 pin3, A1 pin4 => reg offsets $x000, $x001, $x002, $x003
    Vrc4f,
    /// VRC4e: A2 pin3, A3 pin4 => reg offsets $x000, $x004, $x008, $x00C
    Vrc4e,
    /// VRC2b: A0 pin3, A1 pin4 => reg offsets $x000, $x001, $x002, $x003
    Vrc2b,
    /// VRC4b: A1 pin3, A0 pin4 => reg offsets $x000, $x002, $x001, $x003
    Vrc4b,
    /// VRC4d: A3 pin3, A2 pin4 => reg offsets $x000, $x008, $x004, $x00C
    Vrc4d,
    /// VRC2c: A1 pin3, A0 pin4 => reg offsets $x000, $x002, $x001, $x003
    Vrc2c,
}

impl VrcVariant {
    fn is_vrc4(self) -> bool {
        matches!(
            self,
            Self::Vrc4a | Self::Vrc4c | Self::Vrc4f | Self::Vrc4e | Self::Vrc4b | Self::Vrc4d
        )
    }

    fn chr_shift_low_bit(self) -> bool {
        matches!(self, Self::Vrc2a)
    }

    fn bit_positions(self) -> (u8, u8) {
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

/// VRC2/VRC4 mapper state.
///
/// Handles all VRC2 (mapper 22) and VRC4 (mappers 21, 23, 25) variants
/// with configurable pin connections.
pub struct Vrc24 {
    prg_rom: Vec<u8>,
    chr_mem: Vec<u8>,
    current_chr: [u8; CHR_WINDOW_SIZE],
    has_chr_ram: bool,
    prg_ram: [u8; PRG_RAM_SIZE],
    prg_ram_enabled: bool,

    /// Configured board variant
    variant: VrcVariant,

    /// Bit positions of the two register-select address lines
    s0: u8,
    s1: u8,

    /// PRG Select 0: 8KiB bank at $8000-$9FFF (or $C000-$DFFF in swap mode)
    prg_select_0: u8,
    /// PRG Select 1: 8KiB bank at $A000-$BFFF
    prg_select_1: u8,

    /// Mirroring control (bits written to $9000)
    mirroring_bits: u8,
    /// PRG swap mode (VRC4 only): 0 = normal, 1 = swap $8000 and $C000
    prg_swap_mode: bool,

    /// CHR bank registers (9-bit values stored split across low/high regs)
    /// Each pair: [low, high] forms a 9-bit bank select
    chr_bank: [u8; 16],

    /// IRQ state (VRC4 only)
    irq_latch: u8,
    irq_counter: u8,
    irq_prescaler: u16,
    irq_mode: bool, // false=pseudo-scanline, true=cycle
    irq_pending: bool,

    /// VRC2 Microwire latch
    microwire_latch: u8,
}

impl Vrc24 {
    pub fn new(prg_rom: &[u8], chr_rom: &[u8], variant: VrcVariant) -> Self {
        debug_assert!(!prg_rom.is_empty());

        let has_chr_ram = chr_rom.is_empty();
        let chr_size = if has_chr_ram {
            CHR_WINDOW_SIZE
        } else {
            chr_rom.len()
        };
        let mut chr_mem = vec![0; chr_size];
        chr_mem[..chr_rom.len()].copy_from_slice(chr_rom);

        let (s0, s1) = variant.bit_positions();

        let mut mapper = Self {
            prg_rom: prg_rom.to_vec(),
            chr_mem,
            current_chr: [0; CHR_WINDOW_SIZE],
            has_chr_ram,
            prg_ram: [0; PRG_RAM_SIZE],
            prg_ram_enabled: variant.is_vrc4(), // VRC4 enables PRG RAM via $9002
            variant,
            s0,
            s1,
            prg_select_0: 0,
            prg_select_1: 0,
            mirroring_bits: 0,
            prg_swap_mode: false,
            chr_bank: [0; 16],
            irq_latch: 0,
            irq_counter: 0,
            irq_prescaler: 0,
            irq_mode: false,
            irq_pending: false,
            microwire_latch: 0,
        };
        mapper.refresh_chr_banks();
        mapper
    }

    fn prg_bank_count(&self) -> usize {
        self.prg_rom.len() / PRG_BANK_SIZE
    }

    fn chr_bank_count(&self) -> usize {
        self.chr_mem.len() / CHR_BANK_SIZE
    }

    fn compute_register_index(&self, address: u16) -> usize {
        let bit0 = ((address >> self.s0) & 1) as usize;
        let bit1 = ((address >> self.s1) & 1) as usize;
        bit1 << 1 | bit0
    }

    fn refresh_chr_banks(&mut self) {
        for slot in 0..8 {
            let lo = self.chr_bank[slot * 2] as u16;
            let hi = self.chr_bank[slot * 2 + 1] as u16;

            let mut bank = lo | (hi << 4);

            if self.variant.is_vrc4() {
                // VRC4: 9-bit bank select (0-511)
                bank &= 0x1ff;
            } else {
                // VRC2: 8-bit bank select (0-255), bit 4 of high reg is ignored
                bank &= 0x0ff;
            }

            if self.variant.chr_shift_low_bit() {
                // VRC2a: ignore low bit, right-shift by 1
                bank >>= 1;
            }

            let bank = (bank as usize) % self.chr_bank_count();
            let source = bank * CHR_BANK_SIZE;
            let dest = slot * CHR_BANK_SIZE;
            self.current_chr[dest..dest + CHR_BANK_SIZE]
                .copy_from_slice(&self.chr_mem[source..source + CHR_BANK_SIZE]);
        }
    }
}

// ── Cartridge interface methods ──

impl Vrc24 {
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

        // Compute the actual CHR bank for this slot
        let lo = self.chr_bank[slot * 2] as u16;
        let hi = self.chr_bank[slot * 2 + 1] as u16;
        let mut bank = lo | (hi << 4);
        if self.variant.is_vrc4() {
            bank &= 0x1ff;
        } else {
            bank &= 0x0ff;
        }
        if self.variant.chr_shift_low_bit() {
            bank >>= 1;
        }
        let bank = (bank as usize) % self.chr_bank_count();
        let source = bank * CHR_BANK_SIZE + offset_in_bank;

        self.chr_mem[source] = value;
        self.current_chr[addr] = value;
    }

    pub fn read(&mut self, address: u16) -> u8 {
        self.peek(address)
    }

    pub fn peek(&self, address: u16) -> u8 {
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
                    // VRC2 Microwire - return open bus top 7 bits, latch in bit 0
                    (address as u8) | self.microwire_latch
                }
            }
            0x7000..=0x7fff => {
                if self.variant.is_vrc4() {
                    if self.prg_ram_enabled {
                        self.prg_ram[(address - 0x6000) as usize]
                    } else {
                        0
                    }
                } else {
                    // VRC2: $7000-$7FFF is always open bus
                    0
                }
            }
            0x8000..=0x9fff => {
                if self.variant.is_vrc4() && self.prg_swap_mode {
                    // Fixed to second-last bank
                    let bank = self.prg_bank_count().saturating_sub(2);
                    let offset = address as usize - 0x8000;
                    self.prg_rom[bank * PRG_BANK_SIZE + offset]
                } else {
                    let bank = self.normalize_prg_bank(self.prg_select_0);
                    let offset = address as usize - 0x8000;
                    self.prg_rom[bank * PRG_BANK_SIZE + offset % PRG_BANK_SIZE]
                }
            }
            0xa000..=0xbfff => {
                let bank = self.normalize_prg_bank(self.prg_select_1);
                let offset = address as usize - 0xa000;
                self.prg_rom[bank * PRG_BANK_SIZE + offset % PRG_BANK_SIZE]
            }
            0xc000..=0xdfff => {
                if self.variant.is_vrc4() && self.prg_swap_mode {
                    let bank = self.normalize_prg_bank(self.prg_select_0);
                    let offset = address as usize - 0xc000;
                    self.prg_rom[bank * PRG_BANK_SIZE + offset % PRG_BANK_SIZE]
                } else if self.variant.is_vrc4() {
                    // Fixed to second-last bank
                    let bank = self.prg_bank_count().saturating_sub(2);
                    let offset = address as usize - 0xc000;
                    self.prg_rom[bank * PRG_BANK_SIZE + offset]
                } else {
                    // VRC2: $C000-$FFFF fixed to last 16 KiB
                    let last_16k = self.prg_bank_count().saturating_sub(2);
                    let offset = address as usize - 0xc000;
                    self.prg_rom[last_16k * PRG_BANK_SIZE + offset]
                }
            }
            0xe000..=0xffff => {
                if self.variant.is_vrc4() {
                    // Fixed to last bank
                    let bank = self.prg_bank_count().saturating_sub(1);
                    let offset = address as usize - 0xe000;
                    self.prg_rom[bank * PRG_BANK_SIZE + offset]
                } else {
                    // VRC2: part of the fixed 16 KiB
                    let last_16k = self.prg_bank_count().saturating_sub(2);
                    let offset = address as usize - 0xc000;
                    self.prg_rom[last_16k * PRG_BANK_SIZE + offset]
                }
            }
            _ => 0,
        }
    }

    pub fn write(&mut self, address: u16, value: u8) -> CartridgeOperation {
        match address {
            CARTRIDGE_START_ADDR..=0x5fff => CartridgeOperation::None,
            0x6000..=0x7fff => {
                if self.variant.is_vrc4() {
                    if self.prg_ram_enabled {
                        self.prg_ram[(address - 0x6000) as usize % PRG_RAM_SIZE] = value;
                    }
                } else {
                    // VRC2 Microwire latch
                    self.microwire_latch = value & 0x01;
                }
                CartridgeOperation::None
            }
            0x8000..=0x8fff => {
                // PRG Select 0
                self.prg_select_0 = value & 0x1f;
                CartridgeOperation::None
            }
            0x9000..=0x9fff => {
                let idx = self.compute_register_index(address);
                if self.variant.is_vrc4() && idx == 2 {
                    // VRC4: $9002 = PRG Swap Mode / WRAM control
                    // Bit 0: WRAM enable
                    // Bit 1: Swap Mode
                    self.prg_ram_enabled = (value & 0x01) != 0;
                    self.prg_swap_mode = (value & 0x02) != 0;
                    CartridgeOperation::None
                } else {
                    // Mirroring control
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
                        // VRC2: only horizontal or vertical
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
                // PRG Select 1
                self.prg_select_1 = value & 0x1f;
                CartridgeOperation::None
            }
            0xb000..=0xefff => {
                // CHR bank selects
                // Group = high nibble (B..E)
                // Register index within group selects low/high byte and bank pair
                let group = ((address >> 12) & 0x0f) as usize;
                let idx = self.compute_register_index(address);
                let slot = (group - 0xb) * 2 + (idx >> 1);

                if idx & 1 == 0 {
                    // Low byte
                    self.chr_bank[slot * 2] = value & 0x0f;
                } else {
                    // High byte
                    self.chr_bank[slot * 2 + 1] = value & 0x1f;
                }
                self.refresh_chr_banks();
                CartridgeOperation::None
            }
            0xf000..=0xffff => {
                if self.variant.is_vrc4() {
                    let idx = self.compute_register_index(address);
                    match idx {
                        0 => {
                            // IRQ Latch low 4 bits
                            self.irq_latch = (self.irq_latch & 0xf0) | (value & 0x0f);
                        }
                        1 => {
                            // IRQ Latch high 4 bits
                            self.irq_latch = (self.irq_latch & 0x0f) | ((value & 0x0f) << 4);
                        }
                        2 => {
                            // IRQ Control
                            // Bit 2: Mode (0=scanline prescaler, 1=cycle prescaler)
                            // Counter always runs after this write
                            self.irq_mode = (value & 0x04) != 0;
                            self.reload_irq_counter();
                            self.irq_prescaler = 0;
                            self.irq_pending = false;
                            log::debug!(
                                "VRC4 IRQ ctrl: val={:02x} mode={} latch={:02x} counter={:02x}",
                                value,
                                self.irq_mode,
                                self.irq_latch,
                                self.irq_counter
                            );
                        }
                        3 => {
                            // IRQ Acknowledge
                            self.irq_pending = false;
                            self.reload_irq_counter();
                            self.irq_prescaler = 0;
                            log::debug!(
                                "VRC4 IRQ ack: latch={:02x} counter={:02x}",
                                self.irq_latch,
                                self.irq_counter
                            );
                        }
                        _ => {}
                    }
                }
                CartridgeOperation::None
            }
            _ => CartridgeOperation::None,
        }
    }

    pub fn on_ppu_tick(&mut self, _scanline: u16, _dot: u16, _rendering_enabled: bool) {
        if !self.variant.is_vrc4() {
            return;
        }

        if self.irq_mode {
            // Cycle mode: decrement counter every CPU cycle (3 PPU ticks)
            self.irq_prescaler += 1;
            if self.irq_prescaler >= 3 {
                self.irq_prescaler -= 3;
                self.decrement_irq_counter();
            }
        } else {
            // Scanline mode: decrement counter every scanline (341 PPU ticks)
            self.irq_prescaler += 1;
            if self.irq_prescaler >= PPU_DOTS_PER_SCANLINE {
                self.irq_prescaler -= PPU_DOTS_PER_SCANLINE;
                self.decrement_irq_counter();
            }
        }
    }

    fn decrement_irq_counter(&mut self) {
        self.irq_counter = self.irq_counter.wrapping_sub(1);
        if self.irq_counter == 0 {
            self.irq_pending = true;
            self.irq_counter = 0xff;
            log::debug!("VRC4 IRQ fired! latch={:02x}", self.irq_latch);
        }
    }

    fn reload_irq_counter(&mut self) {
        self.irq_counter = self.irq_latch.wrapping_neg();
    }

    pub fn irq_pending(&self) -> bool {
        self.irq_pending
    }

    fn normalize_prg_bank(&self, bank: u8) -> usize {
        (bank as usize) % self.prg_bank_count().max(1)
    }
}

#[cfg(test)]
mod tests;
