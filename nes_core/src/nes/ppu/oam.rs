use crate::nes::ppu::registers::PpuCtrl;
use bitfield_struct::bitfield;

#[derive(Debug, Copy, Clone, Default, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct Sprite {
    pub y: u8,
    pub tile_idx: u8,
    pub attributes: Attribute,
    pub x: u8,
}

impl Sprite {
    pub fn tile_position(self, ppu_ctrl: PpuCtrl) -> TilePosition {
        if ppu_ctrl.sprite_size_16() {
            let bank = if self.tile_idx & 1 == 0 {
                PatternBank::First
            } else {
                PatternBank::Second
            };
            TilePosition::Size16(bank, self.tile_idx & 0xFE)
        } else {
            let bank = if ppu_ctrl.sprite_pattern_table() {
                PatternBank::Second
            } else {
                PatternBank::First
            };
            TilePosition::Size8(bank, self.tile_idx)
        }
    }
}

/// Sprite attribute
#[bitfield(u8)]
#[derive(bytemuck::Pod, bytemuck::Zeroable)]
pub struct Attribute {
    #[bits(2)]
    pub palette: u8,
    #[bits(3)]
    __: u8,
    pub behind_background: bool,
    pub flip_horizontally: bool,
    pub flip_vertically: bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PatternBank {
    First,
    Second,
}

impl PatternBank {
    /// Returns the start address of vram of this pattern bank
    pub fn start_addr(self) -> u16 {
        match self {
            PatternBank::First => 0,
            PatternBank::Second => 0x1000,
        }
    }
}

/// Sprite tile information: size, bank, and index to the bank
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TilePosition {
    Size8(PatternBank, u8),
    Size16(PatternBank, u8),
}

impl TilePosition {
    /// Returns the (plane0_addr, plane1_addr) CHR addresses for the pixel at tile_y.
    pub fn resolve_pixel_addr(self, tile_y: u8) -> TileVRamAddr {
        match self {
            TilePosition::Size8(bank, tile_idx) => {
                let base = bank.start_addr() + tile_idx as u16 * 16 + tile_y as u16;
                TileVRamAddr(base)
            }
            TilePosition::Size16(bank, tile_idx) => {
                let tile_offset = (tile_y / 8) as u16;
                let tile_row = (tile_y % 8) as u16;
                let base = bank.start_addr() + (tile_idx as u16 + tile_offset) * 16 + tile_row;
                TileVRamAddr(base)
            }
        }
    }
}

/// Wrap tile pixel vram/pattern/chr first plane address
#[derive(Debug, Copy, Clone)]
pub struct TileVRamAddr(pub u16);

impl TileVRamAddr {
    /// Return vram/pattern/chr address for second plane
    pub fn second_plane_addr(self) -> u16 {
        self.0 + 8
    }
}

pub struct Oam {
    pub sprites: [Sprite; 64],
}

impl Default for Oam {
    fn default() -> Self {
        Oam {
            sprites: [bytemuck::Zeroable::zeroed(); 64],
        }
    }
}

impl Oam {
    pub fn as_bytes(&self) -> &[u8] {
        bytemuck::cast_slice(&self.sprites)
    }

    pub fn as_bytes_mut(&mut self) -> &mut [u8] {
        bytemuck::cast_slice_mut(&mut self.sprites)
    }
}

#[cfg(test)]
mod tests;
