use super::*;

#[test]
fn test_sweep_bitfield() {
    let mut sweep = Sweep::new();
    sweep.set_enabled(true);
    sweep.set_period(0b111);
    sweep.set_negate(true);
    sweep.set_shift(0b101);

    assert!(sweep.enabled());
    assert_eq!(sweep.period(), 0b111);
    assert!(sweep.negate());
    assert_eq!(sweep.shift(), 0b101);
}

#[test]
fn test_duty_cycle_bitfield() {
    let mut duty = DutyCycle::new();
    duty.set_duty(0b11);
    duty.set_length_counter_halt(true);
    duty.set_constant_volume(false);
    duty.set_volume(0b1010);

    assert_eq!(duty.duty(), 0b11);
    assert!(duty.length_counter_halt());
    assert!(!duty.constant_volume());
    assert_eq!(duty.volume(), 0b1010);
}

#[test]
fn test_duty_cycle_to_from_u8() {
    let mut duty = DutyCycle::new();
    duty.set_duty(0b10);
    duty.set_length_counter_halt(false);
    duty.set_constant_volume(true);
    duty.set_volume(0b0101);

    let byte: u8 = duty.into();
    let duty2: DutyCycle = byte.into();

    assert_eq!(duty2.duty(), 0b10);
    assert!(!duty2.length_counter_halt());
    assert!(duty2.constant_volume());
    assert_eq!(duty2.volume(), 0b0101);
}

#[test]
fn test_linear_counter_control() {
    let mut lcc = LinearCounterControl::new();
    lcc.set_reload_flag(true);
    lcc.set_counter(0x55);

    assert!(lcc.reload_flag());
    assert_eq!(lcc.counter(), 0x55);
}

#[test]
fn test_linear_counter_control_to_from_u8() {
    let mut lcc = LinearCounterControl::new();
    lcc.set_reload_flag(false);
    lcc.set_counter(0x7F);

    let byte: u8 = lcc.into();
    let lcc2: LinearCounterControl = byte.into();

    assert!(!lcc2.reload_flag());
    assert_eq!(lcc2.counter(), 0x7F);
}

#[test]
fn test_duty_cycle_various_values() {
    for duty in 0u8..4 {
        for volume in 0u8..16 {
            let mut d = DutyCycle::new();
            d.set_duty(duty);
            d.set_volume(volume);

            let byte: u8 = d.into();
            let d2: DutyCycle = byte.into();

            assert_eq!(d2.duty(), duty);
            assert_eq!(d2.volume(), volume);
        }
    }
}

#[test]
fn test_length_counter_load_default() {
    let lcl = LengthCounterLoad::default();
    assert_eq!(lcl.low_byte, 0);
    assert_eq!(lcl.high_byte, 0);
    assert_eq!(lcl.timer(), 0);
}

#[test]
fn test_noise_envelop_bitfield() {
    let mut envelop = NoiseEnvelop::new();
    envelop.set_loop_flag(true);
    envelop.set_constant_volume(false);
    envelop.set_volume(0b1010);

    assert!(envelop.loop_flag());
    assert!(!envelop.constant_volume());
    assert_eq!(envelop.volume(), 0b1010);
}

#[test]
fn test_noise_envelop_to_from_u8() {
    let mut envelop = NoiseEnvelop::new();
    envelop.set_loop_flag(false);
    envelop.set_constant_volume(true);
    envelop.set_volume(0b0101);

    let byte: u8 = envelop.into();
    let envelop2: NoiseEnvelop = byte.into();

    assert!(!envelop2.loop_flag());
    assert!(envelop2.constant_volume());
    assert_eq!(envelop2.volume(), 0b0101);
}

#[test]
fn test_noise_period_bitfield() {
    let mut period = NoisePeriod::new();
    period.set_enabled(true);
    period.set_period(0b1010);

    assert!(period.enabled());
    assert_eq!(period.period(), 0b1010);
}

#[test]
fn test_noise_period_to_from_u8() {
    let mut period = NoisePeriod::new();
    period.set_enabled(false);
    period.set_period(0b0101);

    let byte: u8 = period.into();
    let period2: NoisePeriod = byte.into();

    assert!(!period2.enabled());
    assert_eq!(period2.period(), 0b0101);
}

#[test]
fn test_noise_length_bitfield() {
    let mut length = NoiseLength::new();
    length.set_length(0b10101);

    assert_eq!(length.length(), 0b10101);
}

#[test]
fn test_noise_length_to_from_u8() {
    let mut length = NoiseLength::new();
    length.set_length(0b01010);

    let byte: u8 = length.into();
    let length2: NoiseLength = byte.into();

    assert_eq!(length2.length(), 0b01010);
}

#[test]
fn test_dmc_irq_loop_freq_bitfield() {
    let mut freq = DmcIRQLoopFreq::new();
    freq.set_irq_enabled(true);
    freq.set_loop_flag(false);
    freq.set_freq(0b1010);

    assert!(freq.irq_enabled());
    assert!(!freq.loop_flag());
    assert_eq!(freq.freq(), 0b1010);
}

#[test]
fn test_dmc_irq_loop_freq_to_from_u8() {
    let mut freq = DmcIRQLoopFreq::new();
    freq.set_irq_enabled(false);
    freq.set_loop_flag(true);
    freq.set_freq(0b0101);

    let byte: u8 = freq.into();
    let freq2: DmcIRQLoopFreq = byte.into();

    assert!(!freq2.irq_enabled());
    assert!(freq2.loop_flag());
    assert_eq!(freq2.freq(), 0b0101);
}

#[test]
fn test_control_flags_bitfield() {
    let mut flags = ControlFlags::new();
    flags.set_dmc_enabled(true);
    flags.set_noise_enabled(false);
    flags.set_triangle_enabled(true);
    flags.set_pulse1_enabled(false);
    flags.set_pulse2_enabled(true);

    assert!(flags.dmc_enabled());
    assert!(!flags.noise_enabled());
    assert!(flags.triangle_enabled());
    assert!(!flags.pulse1_enabled());
    assert!(flags.pulse2_enabled());
}

#[test]
fn test_control_flags_to_from_u8() {
    let mut flags = ControlFlags::new();
    flags.set_dmc_enabled(false);
    flags.set_noise_enabled(true);
    flags.set_triangle_enabled(false);
    flags.set_pulse1_enabled(true);
    flags.set_pulse2_enabled(false);

    let byte: u8 = flags.into();
    let flags2: ControlFlags = byte.into();

    assert!(!flags2.dmc_enabled());
    assert!(flags2.noise_enabled());
    assert!(!flags2.triangle_enabled());
    assert!(flags2.pulse1_enabled());
    assert!(!flags2.pulse2_enabled());
}

#[test]
fn test_apu_status_bitfield() {
    let mut status = APUStatus::new();
    status.set_dmc_interrupt(true);
    status.set_frame_interrupt(false);
    status.set_dmc_enabled(true);
    status.set_noise_enabled(false);
    status.set_triangle_enabled(true);
    status.set_pulse1_enabled(false);
    status.set_pulse2_enabled(true);

    assert!(status.dmc_interrupt());
    assert!(!status.frame_interrupt());
    assert!(status.dmc_enabled());
    assert!(!status.noise_enabled());
    assert!(status.triangle_enabled());
    assert!(!status.pulse1_enabled());
    assert!(status.pulse2_enabled());
}

#[test]
fn test_apu_status_to_from_u8() {
    let mut status = APUStatus::new();
    status.set_dmc_interrupt(false);
    status.set_frame_interrupt(true);
    status.set_dmc_enabled(false);
    status.set_noise_enabled(true);
    status.set_triangle_enabled(false);
    status.set_pulse1_enabled(true);
    status.set_pulse2_enabled(false);

    let byte: u8 = status.into();
    let status2: APUStatus = byte.into();

    assert!(!status2.dmc_interrupt());
    assert!(status2.frame_interrupt());
    assert!(!status2.dmc_enabled());
    assert!(status2.noise_enabled());
    assert!(!status2.triangle_enabled());
    assert!(status2.pulse1_enabled());
    assert!(!status2.pulse2_enabled());
}

#[test]
fn test_frame_counter_bitfield() {
    let mut counter = FrameCounter::new();
    counter.set_mode(true);
    counter.set_interrupt_flag(false);

    assert!(counter.mode());
    assert!(!counter.interrupt_flag());
}

#[test]
fn test_frame_counter_to_from_u8() {
    let mut counter = FrameCounter::new();
    counter.set_mode(false);
    counter.set_interrupt_flag(true);

    let byte: u8 = counter.into();
    let counter2: FrameCounter = byte.into();

    assert!(!counter2.mode());
    assert!(counter2.interrupt_flag());
}

// Test APU control logic
#[test]
fn test_apu_controller_read_status() {
    let mut channel = Apu::new(());
    let val = channel.read(0x4015);
    assert_eq!(val, 0); // Default APUStatus
}

#[test]
fn test_apu_controller_peek_status_does_not_clear_interrupt() {
    let mut channel = Apu::new(());
    channel.frame_interrupt = true;

    let val = channel.peek(0x4015);
    assert_eq!(val & 0x40, 0x40);
    assert!(channel.frame_interrupt);
}

#[test]
fn test_apu_controller_write_control_flags() {
    let mut channel = Apu::new(());
    channel.write(0x4015, 0x1F); // Should call set_control_flags
}

#[test]
fn test_apu_controller_write_frame_counter() {
    let mut channel = Apu::new(());
    channel.write(0x4017, 0xC0); // Should call set_frame_counter
}

#[test]
fn test_apu_controller_driver_control_flags_status() {
    let mut driver = Apu::new(());

    driver.write(0x4003, 0xF8);
    driver.write(0x4015, 0x01);

    let status = driver.read(0x4015);
    assert_eq!(status & 0x01, 0x01);
    assert_eq!(status & 0x40, 0x00);
}
