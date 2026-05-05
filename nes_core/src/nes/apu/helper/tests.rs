use super::*;
use crate::nes::apu::ControlGate;

fn make_sweep_bits(shift: u8, negate: bool, period: u8, enabled: bool) -> SweepBits {
    let mut bits = SweepBits::new();
    bits.set_shift(shift);
    bits.set_negate(negate);
    bits.set_period(period);
    bits.set_enabled(enabled);
    bits
}

fn new_sweep(period: u8, shift: u8, negate: bool, enabled: bool) -> Sweep {
    let bits = make_sweep_bits(shift, negate, period, enabled);
    Sweep::new(false, bits)
}

#[test]
fn sweep_new_creates_sweep_enabled_from_bits() {
    let mut sweep = new_sweep(0, 3, false, true);
    let mut period = 0x100u16;

    sweep.tick(&mut period);
    sweep.tick(&mut period);

    assert_eq!(period, 0x100 + (0x100 >> 3));
}

#[test]
fn shifter_update_period_add_mode() {
    let bits = make_sweep_bits(3, false, 0, true);
    let shifter = Shifter::new(false, bits);

    assert_eq!(shifter.update_period(0x100), 0x100 + 0x20);
    assert_eq!(shifter.update_period(0x080), 0x080 + 0x10);
    assert_eq!(shifter.update_period(0x001), 0x001 + 0x000);
}

#[test]
fn shifter_update_period_negate_first_channel() {
    let bits = make_sweep_bits(3, true, 0, true);
    let shifter = Shifter::new(false, bits);

    assert_eq!(
        shifter.update_period(0x100),
        0x100u16.wrapping_add(!(0x100 >> 3))
    );
    assert_eq!(
        shifter.update_period(0x080),
        0x080u16.wrapping_add(!(0x080 >> 3))
    );
    assert_eq!(
        shifter.update_period(0x001),
        0x001u16.wrapping_add(!(0x001 >> 3))
    );
}

#[test]
fn shifter_update_period_negate_clears_when_out_of_range() {
    let bits = make_sweep_bits(7, true, 0, true);
    let shifter = Shifter::new(true, bits);

    let exp = 0x001u16.wrapping_add((!(0x01u16 >> 7)).wrapping_add(1));

    assert_eq!(shifter.update_period(0x001), exp);
}

#[test]
fn sweep_tick_updates_period_when_divider_ticks() {
    let mut sweep = new_sweep(0, 1, false, true);
    let mut period = 0x100u16;

    sweep.tick(&mut period);
    assert_eq!(period, 0x100);
    sweep.tick(&mut period);
    assert_eq!(period, 0x100 + (0x100 >> 1));
}

#[test]
fn sweep_new_creates_sweep_disabled_when_enabled_false() {
    let bits = make_sweep_bits(3, true, 5, false);
    let mut sweep = Sweep::new(false, bits);
    let mut period = 0x100u16;

    for _ in 0..16 {
        sweep.tick(&mut period);
    }

    assert_eq!(period, 0x100);
}

#[test]
fn sweep_tick_zero_output_when_period_less_than_8() {
    let bits = make_sweep_bits(0, false, 0, true);
    let mut sweep = Sweep::new(false, bits);
    let mut period = 7u16;

    sweep.tick(&mut period);

    assert_eq!((&sweep).control(), 0);
}

#[test]
fn sweep_tick_zero_output_when_new_period_exceeds_0x7ff() {
    let bits = make_sweep_bits(1, false, 0, true);
    let mut sweep = Sweep::new(false, bits);
    let mut period = 0x600u16;

    sweep.tick(&mut period);
    assert_eq!((&sweep).control(), 0);
}

#[test]
fn sweep_tick_does_not_update_period_when_disabled() {
    let bits = make_sweep_bits(1, false, 0, false);
    let mut sweep = Sweep::new(false, bits);
    let mut period = 0x100u16;

    for _ in 0..8 {
        sweep.tick(&mut period);
    }

    assert_eq!(period, 0x100);
}

#[test]
fn sweep_tick_does_not_update_period_when_shifter_disabled() {
    let bits = make_sweep_bits(0, false, 0, true);
    let mut sweep = Sweep::new(false, bits);
    let mut period = 0x100u16;

    for _ in 0..8 {
        sweep.tick(&mut period);
    }

    assert_eq!(period, 0x100);
}

#[test]
fn sweep_tick_does_not_update_period_when_zero_output() {
    let bits = make_sweep_bits(1, true, 0, true);
    let mut sweep = Sweep::new(false, bits);
    let mut period = 1u16;

    for _ in 0..8 {
        sweep.tick(&mut period);
    }

    assert_eq!(period, 1);
}

#[test]
fn sweep_zero_output_returns_false_for_valid_period() {
    let bits = make_sweep_bits(3, false, 0, true);
    let mut sweep = Sweep::new(false, bits);
    let mut period = 0x100u16;

    sweep.tick(&mut period);

    assert_eq!((&sweep).control(), 1);
}

#[test]
fn sweep_config_updates_enabled_and_shifter() {
    let mut sweep = new_sweep(0, 0, false, false);
    let mut period = 0x100u16;

    sweep.tick(&mut period);
    sweep.tick(&mut period);
    assert_eq!(period, 0x100);

    sweep.config(make_sweep_bits(3, false, 0, true));
    sweep.tick(&mut period);
    sweep.tick(&mut period);

    assert_eq!(period, 0x100 + (0x100 >> 3));
}

#[test]
fn sweep_second_channel_affects_negate_subtraction() {
    let bits = make_sweep_bits(3, true, 0, true);

    let mut sweep1 = Sweep::new(false, bits);
    let mut sweep2 = Sweep::new(true, bits);
    let mut period1 = 0x100u16;
    let mut period2 = 0x100u16;

    sweep1.tick(&mut period1);
    sweep1.tick(&mut period1);
    sweep2.tick(&mut period2);
    sweep2.tick(&mut period2);

    let expected1 = 0x100u16.wrapping_add(!(0x100 >> 3));
    let expected2 = 0x100u16.wrapping_add(!(0x100 >> 3) + 1);

    assert_eq!(period1, expected1);
    assert_eq!(period2, expected2);
}
