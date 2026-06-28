use bitfield_struct::bitfield;

use super::oam::{PatternBank, TilePosition};
use super::{PPU_OPEN_BUS_DECAY_TICKS, Pixel};

#[bitfield(u8)]
pub struct PpuCtrl {
    #[bits(2)]
    pub name_table_select: u8,
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

    pub const fn sprite_height(self) -> u8 {
        if self.sprite_size_16() { 16 } else { 8 }
    }
}

#[bitfield(u8)]
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
    /// Apply grayscale and emphasis effects to a pixel color using this mask's flags.
    pub fn apply_effects(&self, pixel: Pixel) -> Pixel {
        if !self.grayscale() && !self.red_tint() && !self.green_tint() && !self.blue_tint() {
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

        if self.grayscale() {
            let gray = (r as u16 * 77 + g as u16 * 150 + b as u16 * 29) / 256;
            r = gray as u8;
            g = gray as u8;
            b = gray as u8;
        };

        Pixel::new(r, g, b)
    }
}

#[bitfield(u8)]
pub struct PpuStatus {
    #[bits(5)]
    pub __: u8,
    pub sprite_overflow: bool,
    pub sprite_zero_hit: bool,
    pub v_blank: bool,
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
        self.ctrl = PpuCtrl::new();
        self.status = PpuStatus::new();
        self.mask = PpuMask::new();
        self.vram_addr = 0;
        self.temp_vram_addr = 0;
        self.fine_x = 0;
        self.write_toggle = false;
        self.ppudata_buffer = 0;
        self.bus_latch = 0;
        self.bus_latch_decay_deadlines = [0; 8];
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
        for bit in 0..8 {
            let mask = 1 << bit;
            self.bus_latch_decay_deadlines[bit] = if (value & mask) != 0 {
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
