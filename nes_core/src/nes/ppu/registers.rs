use modular_bitfield::prelude::*;

use super::oam::{PatternBank, TilePosition};
use super::{Pixel, open_bus_decay_ticks};

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

impl PpuMask {
    /// Mask a palette entry for grayscale rendering: hardware ANDs the
    /// 6-bit entry with $30 before the color lookup, so dark entries like
    /// $3F (black) render as their $3x row ($30, white). Single
    /// implementation shared by the mask, the segment flush, and the
    /// live brightness path (ticket #67).
    #[inline]
    pub fn grayscale_index(&self, index: u8) -> u8 {
        if self.grayscale() {
            index & 0x30
        } else {
            index
        }
    }

    /// Apply emphasis tints to a looked-up pixel color. Grayscale is not
    /// handled here: it applies to the palette index before the lookup
    /// (see [`Self::grayscale_index`]).
    #[inline]
    pub fn apply_effects(&self, pixel: Pixel) -> Pixel {
        if !self.red_tint() && !self.green_tint() && !self.blue_tint() {
            pixel
        } else {
            self.do_apply_effects(pixel)
        }
    }

    fn do_apply_effects(&self, pixel: Pixel) -> Pixel {
        let (mut r, mut g, mut b) = pixel.to_rgb();

        if !self.red_tint() && (self.green_tint() || self.blue_tint()) {
            r = (r as u16 * 192 / 256) as u8;
        }
        if !self.green_tint() && (self.red_tint() || self.blue_tint()) {
            g = (g as u16 * 192 / 256) as u8;
        }
        if !self.blue_tint() && (self.red_tint() || self.green_tint()) {
            b = (b as u16 * 192 / 256) as u8;
        }

        Pixel::new(r, g, b)
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

    // PPU open bus / data latch. The latch holds the last value driven
    // onto the PPU data bus; each 1-bit decays to 0 on its own deadline,
    // 3-30 ms after the last refresh depending on the bit (~1 frame or
    // less; see `open_bus_decay_ticks`, bit 7 fastest).
    // Deadlines stay per-bit: partial refreshes (`refresh_bus_latch_bits`,
    // masks $E0/$3F) must NOT re-arm untouched bits (ppu_open_bus tests 7
    // and 9). `bus_latch_earliest_decay` caches the minimum live deadline
    // (`u64::MAX` when no 1-bits are pending) so the read fast path is a
    // single compare -- the 8-bit scan only runs once decay is actually due.
    pub bus_latch: u8,
    pub bus_latch_decay_deadlines: [u64; 8],
    pub bus_latch_earliest_decay: u64,
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
            bus_latch_earliest_decay: u64::MAX,
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

    #[inline]
    pub fn current_bus_latch(&mut self, cycle: u64) -> u8 {
        self.apply_bus_decay(cycle);
        self.bus_latch
    }

    #[inline]
    fn apply_bus_decay(&mut self, now: u64) {
        // Fast path: nothing expires before the earliest live deadline, so
        // the common case (decay a frame or so away, or no 1-bits pending)
        // is a single compare with no loop. The 8-bit scan only runs once
        // decay is actually due.
        if now < self.bus_latch_earliest_decay {
            return;
        }
        let mut earliest = u64::MAX;
        for (bit, deadline) in self.bus_latch_decay_deadlines.iter_mut().enumerate() {
            if *deadline != 0 && now >= *deadline {
                self.bus_latch &= !(1 << bit);
                *deadline = 0;
            }
            if *deadline != 0 && *deadline < earliest {
                earliest = *deadline;
            }
        }
        self.bus_latch_earliest_decay = earliest;
    }

    #[inline]
    fn recompute_earliest_decay(&mut self) {
        let mut earliest = u64::MAX;
        for &deadline in &self.bus_latch_decay_deadlines {
            if deadline != 0 && deadline < earliest {
                earliest = deadline;
            }
        }
        self.bus_latch_earliest_decay = earliest;
    }

    #[inline]
    pub fn refresh_bus_latch(&mut self, value: u8, cycle: u64) {
        // No pre-decay: the old latch is overwritten unconditionally, so
        // decaying it first would be dead work. Deadlines differ per bit
        // (bit 7 lives 3 ms, bit 0 lives 30 ms), so the earliest must be
        // tracked while the slots are stamped.
        self.bus_latch = value;
        let mut earliest = u64::MAX;
        for (bit, slot) in self.bus_latch_decay_deadlines.iter_mut().enumerate() {
            *slot = if (value >> bit) & 1 != 0 {
                let deadline = cycle.wrapping_add(open_bus_decay_ticks(bit));
                earliest = earliest.min(deadline);
                deadline
            } else {
                0
            };
        }
        self.bus_latch_earliest_decay = earliest;
    }

    #[inline]
    pub fn refresh_bus_latch_bits(&mut self, mask: u8, value: u8, cycle: u64) {
        self.apply_bus_decay(cycle);
        self.bus_latch = (self.bus_latch & !mask) | (value & mask);
        // Per-bit semantics: only masked bits get new deadlines; untouched
        // bits keep theirs (ppu_open_bus tests 7 and 9). Iterating the mask
        // touches at most popcount(mask) entries ($E0 -> 3, $3F -> 6).
        let mut pending = mask;
        while pending != 0 {
            let bit = pending.trailing_zeros() as usize;
            self.bus_latch_decay_deadlines[bit] = if (value >> bit) & 1 != 0 {
                cycle.wrapping_add(open_bus_decay_ticks(bit))
            } else {
                0
            };
            pending &= pending - 1;
        }
        self.recompute_earliest_decay();
    }
}

#[cfg(test)]
mod tests;
