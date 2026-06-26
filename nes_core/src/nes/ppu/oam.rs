use crate::nes::ppu::registers::PpuCtrl;
use bitfield_struct::bitfield;

#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
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
mod tests {
    use super::*;
    use test_case::test_case;

    #[test]
    fn oam_size_is_256_bytes() {
        assert_eq!(size_of::<Oam>(), 256);
    }

    #[test]
    fn sprite_size_is_4_bytes() {
        assert_eq!(size_of::<Sprite>(), 4);
    }

    #[test]
    fn oam_as_bytes_round_trip() {
        let mut oam = Oam::default();
        let bytes = oam.as_bytes_mut();
        for i in 0..256 {
            bytes[i] = i as u8;
        }
        let read_back = oam.as_bytes();
        for i in 0..256 {
            assert_eq!(read_back[i], i as u8, "byte {i} mismatch");
        }
    }

    #[test]
    fn oam_as_bytes_mut_modifies_sprites() {
        let mut oam = Oam::default();
        oam.as_bytes_mut()[0] = 42;
        assert_eq!(oam.sprites[0].y, 42);
    }

    #[test]
    fn attribute_palette_bits() {
        let attr = Attribute::new().with_palette(3);
        assert_eq!(attr.into_bits() & 0x03, 3);
        assert_eq!(attr.palette(), 3);
    }

    #[test]
    fn attribute_behind_background_bit() {
        let attr = Attribute::new().with_behind_background(true);
        assert_eq!(attr.into_bits() & 0x20, 0x20);
    }

    #[test]
    fn attribute_flip_horizontally_bit() {
        let attr = Attribute::new().with_flip_horizontally(true);
        assert_eq!(attr.into_bits() & 0x40, 0x40);
    }

    #[test]
    fn attribute_flip_vertically_bit() {
        let attr = Attribute::new().with_flip_vertically(true);
        assert_eq!(attr.into_bits() & 0x80, 0x80);
    }

    #[test]
    fn attribute_bit_positions_combined() {
        let attr = Attribute::new()
            .with_palette(2)
            .with_behind_background(true)
            .with_flip_horizontally(false)
            .with_flip_vertically(true);
        assert_eq!(attr.into_bits(), 0xA2);
    }

    #[test]
    fn oam_default_is_zeroed() {
        let oam = Oam::default();
        for byte in oam.as_bytes() {
            assert_eq!(*byte, 0);
        }
    }

    #[test_case(false, PatternBank::First, 5 ; "first bank")]
    #[test_case(true, PatternBank::Second, 5 ; "second bank")]
    fn test_8x8_tile_position(
        sprite_pattern_table: bool,
        expected_bank: PatternBank,
        expected_tile: u8,
    ) {
        let sprite = Sprite {
            y: 0,
            tile_idx: 5,
            attributes: 0.into(),
            x: 0,
        };
        let ctrl = PpuCtrl::new()
            .with_sprite_size_16(false)
            .with_sprite_pattern_table(sprite_pattern_table);
        assert_eq!(
            sprite.tile_position(ctrl),
            TilePosition::Size8(expected_bank, expected_tile)
        );
    }

    #[test_case(4, PatternBank::First, 4 ; "even tile idx")]
    #[test_case(5, PatternBank::Second, 4 ; "odd tile idx")]
    fn test_8x16_tile_position(tile_idx: u8, expected_bank: PatternBank, expected_tile: u8) {
        let sprite = Sprite {
            y: 0,
            tile_idx,
            attributes: 0.into(),
            x: 0,
        };
        let ctrl = PpuCtrl::new().with_sprite_size_16(true);
        assert_eq!(
            sprite.tile_position(ctrl),
            TilePosition::Size16(expected_bank, expected_tile)
        );
    }
}
