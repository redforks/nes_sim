use modular_bitfield::prelude::*;

use super::oam::{PatternBank, TilePosition};
use super::{PPU_OPEN_BUS_DECAY_TICKS, Pixel};

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct PpuCtrl {
    pub name_table_select: B2,
    pub increment_mode: bool,
    pub sprite_pattern_table: bool,
    pub background_pattern_table: bool,
    pub sprite_size_16: bool,
    pub ppu_master: bool,
    pub nmi_enable: bool,
}

impl PpuCtrl {
    pub fn inc_ppu_addr(self, ppu_addr: &mut u16) {
        *ppu_addr = ppu_addr.wrapping_add(if self.increment_mode() { 32 } else { 1 });
    }

    pub fn background_tile_position(self, tile_idx: u8) -> TilePosition {
        let bank = if self.background_pattern_table() {
            PatternBank::Second
        } else {
            PatternBank::First
        };
        TilePosition::Size8(bank, tile_idx)
    }

    pub fn sprite_height(self) -> u8 {
        if self.sprite_size_16() { 16 } else { 8 }
    }
}

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct PpuMask {
    pub grayscale: bool,
    pub background_left_enabled: bool,
    pub sprite_left_enabled: bool,
    pub background_enabled: bool,
    pub sprite_enabled: bool,
    pub red_tint: bool,
    pub green_tint: bool,
    pub blue_tint: bool,
}

/// Color-only projection of [`PpuMask`]: the grayscale and emphasis
/// (red/green/blue tint) bits, without the per-dot enable bits.
///
/// Ticket #63 defers only these bits to the end-of-frame flush, recording
/// each mid-frame change as a split point. Enable bits stay live per-dot
/// and are never part of a [`ColorConfig`], so a stored configuration
/// cannot accidentally freeze rendering enablement for a segment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ColorConfig {
    grayscale: bool,
    red_tint: bool,
    green_tint: bool,
    blue_tint: bool,
}

impl ColorConfig {
    pub fn from_mask(mask: PpuMask) -> Self {
        Self {
            grayscale: mask.grayscale(),
            red_tint: mask.red_tint(),
            green_tint: mask.green_tint(),
            blue_tint: mask.blue_tint(),
        }
    }

    /// Mask a palette entry for grayscale rendering: hardware ANDs the
    /// 6-bit entry with $30 before the color lookup, so dark entries like
    /// $3F (black) render as their $3x row ($30, white). Single
    /// implementation shared by the mask, the segment flush, and the
    /// live brightness path (ticket #67).
    #[inline]
    pub fn grayscale_index(&self, index: u8) -> u8 {
        if self.grayscale { index & 0x30 } else { index }
    }

    /// Apply emphasis tints to a looked-up pixel color. Grayscale is not
    /// handled here: it applies to the palette index before the lookup
    /// (see [`Self::grayscale_index`]).
    #[inline]
    pub fn apply_effects(&self, pixel: Pixel) -> Pixel {
        if !self.red_tint && !self.green_tint && !self.blue_tint {
            pixel
        } else {
            self.do_apply_effects(pixel)
        }
    }

    fn do_apply_effects(&self, pixel: Pixel) -> Pixel {
        let (mut r, mut g, mut b) = pixel.to_rgb();

        if !self.red_tint && (self.green_tint || self.blue_tint) {
            r = (r as u16 * 192 / 256) as u8;
        }
        if !self.green_tint && (self.red_tint || self.blue_tint) {
            g = (g as u16 * 192 / 256) as u8;
        }
        if !self.blue_tint && (self.red_tint || self.green_tint) {
            b = (b as u16 * 192 / 256) as u8;
        }

        Pixel::new(r, g, b)
    }
}

impl PpuMask {
    /// Mask a palette entry for grayscale rendering. Delegates to the
    /// color-only projection so the mask, the segment flush, and the
    /// live brightness path share one implementation (ticket #67).
    pub fn grayscale_index(&self, index: u8) -> u8 {
        ColorConfig::from_mask(*self).grayscale_index(index)
    }

    /// Apply emphasis tints to a looked-up pixel color. Grayscale is not
    /// handled here: it applies to the palette index before the lookup
    /// (see [`PpuMask::grayscale_index`]).
    #[inline]
    pub fn apply_effects(&self, pixel: Pixel) -> Pixel {
        ColorConfig::from_mask(*self).apply_effects(pixel)
    }
}

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct PpuStatus {
    #[skip]
    pub __: B5,
    pub sprite_overflow: bool,
    pub sprite_zero_hit: bool,
    pub v_blank: bool,
}

impl PpuStatus {
    pub const EMPTY: Self = Self::new();
}

pub struct Registers {
    pub ctrl: PpuCtrl,
    pub mask: PpuMask,
    pub status: PpuStatus,

    // VRAM address registers: v (current), t (temporary), x (fine X), w (write toggle)
    pub vram_addr: u16,
    pub temp_vram_addr: u16,
    pub fine_x: u8,
    pub write_toggle: bool,

    // PPUDATA read buffer
    pub ppudata_buffer: u8,

    // PPU open bus / data latch
    pub bus_latch: u8,
    pub bus_latch_decay_deadlines: [u64; 8],
}

impl Registers {
    pub fn new() -> Self {
        Registers {
            ctrl: PpuCtrl::new(),
            mask: PpuMask::new(),
            status: PpuStatus::new(),
            vram_addr: 0,
            temp_vram_addr: 0,
            fine_x: 0,
            write_toggle: false,
            ppudata_buffer: 0,
            bus_latch: 0,
            bus_latch_decay_deadlines: [0; 8],
        }
    }

    pub fn reset(&mut self) {
        *self = Self::new();
    }

    pub fn set_control_flags(&mut self, flags: PpuCtrl) {
        self.ctrl = flags;
        self.temp_vram_addr =
            (self.temp_vram_addr & !0x0C00) | ((self.ctrl.name_table_select() as u16) << 10);
    }

    pub fn write_scroll(&mut self, value: u8) {
        if !self.write_toggle {
            self.fine_x = value & 0x07;
            self.temp_vram_addr = (self.temp_vram_addr & 0xFFE0) | (value as u16 >> 3);
            self.write_toggle = true;
        } else {
            self.temp_vram_addr = (self.temp_vram_addr & !0x73E0)
                | (((value as u16 & 0x07) << 12) & 0x7000)
                | ((value as u16 >> 3) << 5);
            self.write_toggle = false;
        }
    }

    /// Write to PPUADDR register. Returns `true` when the second write completes
    /// (i.e., vram_addr has been updated from temp_vram_addr).
    pub fn write_vram_addr(&mut self, value: u8) -> bool {
        if !self.write_toggle {
            self.temp_vram_addr = (self.temp_vram_addr & 0x00FF) | ((value as u16 & 0x3F) << 8);
            self.write_toggle = true;
            false
        } else {
            self.temp_vram_addr = (self.temp_vram_addr & 0x7F00) | (value as u16 & 0x00FF);
            self.vram_addr = self.temp_vram_addr;
            self.write_toggle = false;
            true
        }
    }

    pub fn current_bus_latch(&mut self, cycle: u64) -> u8 {
        self.apply_bus_decay(cycle);
        self.bus_latch
    }

    fn apply_bus_decay(&mut self, now: u64) {
        for bit in 0..8 {
            let mask = 1 << bit;
            let deadline = self.bus_latch_decay_deadlines[bit];
            if (self.bus_latch & mask) != 0 && deadline != 0 && now >= deadline {
                self.bus_latch &= !mask;
                self.bus_latch_decay_deadlines[bit] = 0;
            }
        }
    }

    pub fn refresh_bus_latch(&mut self, value: u8, cycle: u64) {
        self.apply_bus_decay(cycle);
        self.bus_latch = value;
        for (bit, deadline) in self.bus_latch_decay_deadlines.iter_mut().enumerate() {
            let mask = 1u8 << bit;
            *deadline = if (value & mask) != 0 {
                cycle.wrapping_add(PPU_OPEN_BUS_DECAY_TICKS)
            } else {
                0
            };
        }
    }

    pub fn refresh_bus_latch_bits(&mut self, mask: u8, value: u8, cycle: u64) {
        self.apply_bus_decay(cycle);
        for bit in 0..8 {
            let bit_mask = 1 << bit;
            if (mask & bit_mask) == 0 {
                continue;
            }

            if (value & bit_mask) != 0 {
                self.bus_latch |= bit_mask;
                self.bus_latch_decay_deadlines[bit] = cycle.wrapping_add(PPU_OPEN_BUS_DECAY_TICKS);
            } else {
                self.bus_latch &= !bit_mask;
                self.bus_latch_decay_deadlines[bit] = 0;
            }
        }
    }
}

#[cfg(test)]
mod tests;
