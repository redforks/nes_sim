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

impl TilePosition {
    /// Returns the (plane0_addr, plane1_addr) CHR addresses for the pixel at tile_y.
    pub fn resolve_pixel_addr(self, tile_y: u8) -> (u16, u16) {
        match self {
            TilePosition::Size8(bank, tile_idx) => {
                let base = bank.start_addr() + tile_idx as u16 * 16 + tile_y as u16;
                (base, base + 8)
            }
            TilePosition::Size16(bank, tile_idx) => {
                let tile_offset = (tile_y / 8) as u16;
                let tile_row = (tile_y % 8) as u16;
                let base = bank.start_addr() + (tile_idx as u16 + tile_offset) * 16 + tile_row;
                (base, base + 8)
            }
        }
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

    #[test_case(TilePosition::Size8(PatternBank::First, 0), 0, 0, 8 ; "size8 first bank tile0 y0")]
    #[test_case(TilePosition::Size8(PatternBank::First, 0), 7, 7, 15 ; "size8 first bank tile0 y7")]
    #[test_case(TilePosition::Size8(PatternBank::First, 1), 3, 19, 27 ; "size8 first bank tile1 y3")]
    #[test_case(TilePosition::Size8(PatternBank::Second, 0), 0, 0x1000, 0x1008 ; "size8 second bank tile0 y0")]
    #[test_case(TilePosition::Size8(PatternBank::Second, 3), 5, 0x1035, 0x103D ; "size8 second bank tile3 y5")]
    #[test_case(TilePosition::Size16(PatternBank::First, 0), 0, 0, 8 ; "size16 first bank tile0 y0")]
    #[test_case(TilePosition::Size16(PatternBank::First, 0), 7, 7, 15 ; "size16 first bank tile0 y7")]
    #[test_case(TilePosition::Size16(PatternBank::First, 0), 8, 16, 24 ; "size16 first bank tile0 y8 wraps to tile1")]
    #[test_case(TilePosition::Size16(PatternBank::First, 0), 15, 23, 31 ; "size16 first bank tile0 y15 wraps to tile1")]
    #[test_case(TilePosition::Size16(PatternBank::First, 2), 9, 49, 57 ; "size16 first bank tile2 y9 uses tile3")]
    #[test_case(TilePosition::Size16(PatternBank::Second, 4), 10, 0x1052, 0x105A ; "size16 second bank tile4 y10 uses tile5")]
    #[test_case(TilePosition::Size16(PatternBank::Second, 1), 0, 0x1010, 0x1018 ; "size16 second bank tile1 y0")]
    fn test_resolve_pixel_addr(
        tile_position: TilePosition,
        tile_y: u8,
        expected_plane0: u16,
        expected_plane1: u16,
    ) {
        assert_eq!(
            tile_position.resolve_pixel_addr(tile_y),
            (expected_plane0, expected_plane1)
        );
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
