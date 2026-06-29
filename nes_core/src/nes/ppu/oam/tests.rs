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
    for i in 0..255 {
        oam.set_byte(i, i);
    }
    for i in 0..255 {
        let read_back = oam.get_byte(i);
        assert_eq!(read_back, i, "byte {i} mismatch");
    }
}

#[test]
fn oam_as_bytes_mut_modifies_sprites() {
    let mut oam = Oam::default();
    oam.set_byte(0, 42);
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
    for i in 0..255 {
        assert_eq!(oam.get_byte(i), 0);
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
    let addr = tile_position.resolve_pixel_addr(tile_y);
    assert_eq!(
        (addr.0, addr.second_plane_addr()),
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
