use super::*;
use crate::nes::ppu::{PPU_OPEN_BUS_DECAY_TICKS, open_bus_decay_ticks};
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
    // The re-armed bit 7 still expires 3 ms after its own refresh (1000),
    // while the low bits kept the deadlines they got at cycle 0.
    assert_eq!(
        regs.current_bus_latch(1000 + PPU_OPEN_BUS_DECAY_TICKS - 1) & 0xE0,
        0x80
    );
    assert_eq!(
        regs.current_bus_latch(1000 + PPU_OPEN_BUS_DECAY_TICKS) & 0xE0,
        0x00
    );
}

#[test]
fn bits_of_one_latch_decay_on_different_deadlines() {
    // One latch byte, eight deadlines: bit 7 (fastest) is gone at 3 ms while
    // bits 0-3 still hold 1s, and `$3F` reproduces lidnariq's warm-PPU
    // `$3F` -> `$1F` -> `$0F` progression (bit 5 decays before bit 4).
    let mut regs = Registers::new();
    regs.refresh_bus_latch(0xFF, 0);
    assert_eq!(regs.current_bus_latch(PPU_OPEN_BUS_DECAY_TICKS), 0x7F);

    let mut regs = Registers::new();
    regs.refresh_bus_latch(0x3F, 0);
    assert_eq!(regs.current_bus_latch(open_bus_decay_ticks(5)), 0x1F);
    assert_eq!(regs.current_bus_latch(open_bus_decay_ticks(4)), 0x0F);
    // Bits 3-0 outlive one frame (89,342 dots).
    assert_eq!(regs.current_bus_latch(89_342), 0x0F);
    assert_eq!(regs.current_bus_latch(open_bus_decay_ticks(0)), 0x00);
}

#[test]
fn palette_style_partial_refresh_preserves_high_bits() {
    // Mirrors ppu_open_bus test 9 ("Reading palette from $2007 shouldn't
    // refresh high 2 bits"): refreshing the low 6 bits must not extend the
    // high bits' decay.
    let mut regs = Registers::new();
    regs.refresh_bus_latch(0xFF, 0);
    regs.refresh_bus_latch_bits(0x3F, 0x3F, 1000);
    // Bits 6-7 keep cycle-0 deadlines (bit 6 is the slower of the two).
    assert_eq!(regs.current_bus_latch(open_bus_decay_ticks(6)), 0x3F);
    assert_eq!(regs.current_bus_latch(1000 + open_bus_decay_ticks(0)), 0x00);
}
