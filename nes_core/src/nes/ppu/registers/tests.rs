use super::*;
use test_case::test_case;

#[test_case(false, 5, PatternBank::First ; "background at first bank")]
#[test_case(true, 5, PatternBank::Second ; "background at second bank")]
fn test_background_tile_position(
    background_pattern_table: bool,
    tile_idx: u8,
    expected_bank: PatternBank,
) {
    let ctrl = PpuCtrl::new().with_background_pattern_table(background_pattern_table);
    assert_eq!(
        ctrl.background_tile_position(tile_idx),
        TilePosition::Size8(expected_bank, tile_idx)
    );
}

#[test_case(0b00000000, 180, 120, 60 ; "no effects")]
#[test_case(0b00100000, 180, 90, 45 ; "red_tint only")]
#[test_case(0b01000000, 135, 120, 45 ; "green_tint only")]
#[test_case(0b10000000, 135, 90, 60 ; "blue_tint only")]
#[test_case(0b01100000, 180, 120, 45 ; "red+green tint")]
#[test_case(0b10100000, 180, 90, 60 ; "red+blue tint")]
#[test_case(0b11000000, 135, 120, 60 ; "green+blue tint")]
#[test_case(0b11100000, 180, 120, 60 ; "all three tints")]
#[test_case(0b00000001, 131, 131, 131 ; "grayscale only")]
#[test_case(0b00100001, 111, 111, 111 ; "grayscale + red_tint")]
#[test_case(0b11100001, 131, 131, 131 ; "grayscale + all tints")]
fn test_apply_effects(mask_bits: u8, r: u8, g: u8, b: u8) {
    const INPUT: Pixel = Pixel::new(180, 120, 60);

    let mask = PpuMask::from(mask_bits);
    assert_eq!(mask.apply_effects(INPUT), Pixel::new(r, g, b));
}
