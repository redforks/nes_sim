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
fn test_apply_effects(mask_bits: u8, r: u8, g: u8, b: u8) {
    const INPUT: Pixel = Pixel::new(180, 120, 60);

    let mask = PpuMask::from_bytes([mask_bits]);
    assert_eq!(mask.apply_effects(INPUT), Pixel::new(r, g, b));
}
#[test]
fn partial_refresh_preserves_unmasked_bit_deadlines() {
    // Mirrors ppu_open_bus test 7 ("Reading $2002 shouldn't refresh low 5
    // bits"): refreshing the high bits must not re-arm the low bits' decay.
    let mut regs = Registers::new();
    regs.refresh_bus_latch(0xFF, 0);
    regs.refresh_bus_latch_bits(0xE0, 0x80, 1000);
    // Low bits still decay on the original deadline ...
    assert_eq!(regs.current_bus_latch(PPU_OPEN_BUS_DECAY_TICKS), 0x80);
    // ... while the refreshed high bit survives until its own deadline.
    assert_eq!(
        regs.current_bus_latch(1000 + PPU_OPEN_BUS_DECAY_TICKS - 1),
        0x80
    );
    assert_eq!(
        regs.current_bus_latch(1000 + PPU_OPEN_BUS_DECAY_TICKS),
        0x00
    );
}

#[test]
fn palette_style_partial_refresh_preserves_high_bits() {
    // Mirrors ppu_open_bus test 9 ("Reading palette from $2007 shouldn't
    // refresh high 2 bits"): refreshing the low 6 bits must not extend the
    // high bits' decay.
    let mut regs = Registers::new();
    regs.refresh_bus_latch(0xFF, 0);
    regs.refresh_bus_latch_bits(0x3F, 0x3F, 1000);
    assert_eq!(regs.current_bus_latch(PPU_OPEN_BUS_DECAY_TICKS), 0x3F);
    assert_eq!(
        regs.current_bus_latch(1000 + PPU_OPEN_BUS_DECAY_TICKS),
        0x00
    );
}
