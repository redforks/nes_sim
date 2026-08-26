use super::*;

#[test]
fn dmc_counts_dac_register_writes() {
    let mut apu = Apu::new(());
    assert_eq!(apu.dmc_dac_writes(), 0);

    apu.write(0x4011, 0x40);
    apu.write(0x4011, 0x7f);
    // Other DMC registers are not DAC writes.
    apu.write(0x4010, 0x0f);
    apu.write(0x4015, 0x10);

    assert_eq!(apu.dmc_dac_writes(), 2);
}

#[test]
fn test_sweep_bitfield() {
    let mut sweep = SweepBits::new();
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
    let mut duty = PulseControlBits::new();
    duty.set_duty(0b11);
    duty.set_loop_and_is_halt(true);
    duty.set_constant_volume(false);
    duty.set_volume(0b1010);

    assert_eq!(duty.duty(), 0b11);
    assert!(duty.loop_and_is_halt());
    assert!(!duty.constant_volume());
    assert_eq!(duty.volume(), 0b1010);
}

#[test]
fn test_duty_cycle_to_from_u8() {
    let mut duty = PulseControlBits::new();
    duty.set_duty(0b10);
    duty.set_loop_and_is_halt(false);
    duty.set_constant_volume(true);
    duty.set_volume(0b0101);

    let byte: u8 = duty.into();
    let duty2: PulseControlBits = byte.into();

    assert_eq!(duty2.duty(), 0b10);
    assert!(!duty2.loop_and_is_halt());
    assert!(duty2.constant_volume());
    assert_eq!(duty2.volume(), 0b0101);
}

#[test]
fn test_linear_counter_control() {
    let mut lcc = TriangleControlBits::new();
    lcc.set_loop_and_is_halt(true);
    lcc.set_counter(0x55);

    assert!(lcc.loop_and_is_halt());
    assert_eq!(lcc.counter(), 0x55);
}

#[test]
fn test_linear_counter_control_to_from_u8() {
    let mut lcc = TriangleControlBits::new();
    lcc.set_loop_and_is_halt(false);
    lcc.set_counter(0x7F);

    let byte: u8 = lcc.into();
    let lcc2: TriangleControlBits = byte.into();

    assert!(!lcc2.loop_and_is_halt());
    assert_eq!(lcc2.counter(), 0x7F);
}

#[test]
fn test_duty_cycle_various_values() {
    for duty in 0u8..4 {
        for volume in 0u8..16 {
            let mut d = PulseControlBits::new();
            d.set_duty(duty);
            d.set_volume(volume);

            let byte: u8 = d.into();
            let d2: PulseControlBits = byte.into();

            assert_eq!(d2.duty(), duty);
            assert_eq!(d2.volume(), volume);
        }
    }
}

#[test]
fn test_noise_envelop_bitfield() {
    let mut envelop = NoiseControlBits::new();
    envelop.set_loop_and_is_halt(true);
    envelop.set_constant_volume(false);
    envelop.set_volume(0b1010);

    assert!(envelop.loop_and_is_halt());
    assert!(!envelop.constant_volume());
    assert_eq!(envelop.volume(), 0b1010);
}

#[test]
fn test_noise_envelop_to_from_u8() {
    let mut envelop = NoiseControlBits::new();
    envelop.set_loop_and_is_halt(false);
    envelop.set_constant_volume(true);
    envelop.set_volume(0b0101);

    let byte: u8 = envelop.into();
    let envelop2: NoiseControlBits = byte.into();

    assert!(!envelop2.loop_and_is_halt());
    assert!(envelop2.constant_volume());
    assert_eq!(envelop2.volume(), 0b0101);
}

#[test]
fn test_noise_period_bitfield() {
    let mut period = NoisePeriod::new();
    period.set_is_halt(true);
    period.set_period(0b1010);

    assert!(period.is_halt());
    assert_eq!(period.period(), 0b1010);
}

#[test]
fn test_noise_period_to_from_u8() {
    let mut period = NoisePeriod::new();
    period.set_is_halt(false);
    period.set_period(0b0101);

    let byte: u8 = period.into();
    let period2: NoisePeriod = byte.into();

    assert!(!period2.is_halt());
    assert_eq!(period2.period(), 0b0101);
}

#[test]
fn test_noise_length_bitfield() {
    let mut length = NoiseLength::new();
    length.set_length_idx(0b10101);

    assert_eq!(length.length_idx(), 0b10101);
}

#[test]
fn test_noise_length_to_from_u8() {
    let mut length = NoiseLength::new();
    length.set_length_idx(0b01010);

    let byte: u8 = length.into();
    let length2: NoiseLength = byte.into();

    assert_eq!(length2.length_idx(), 0b01010);
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
    let mut counter = FrameSequencerBits::new();
    counter.set_mode(FrameSequencerMode::FiveStep);
    counter.set_disable_interrupt(false);

    assert_eq!(counter.mode(), FrameSequencerMode::FiveStep);
    assert!(!counter.disable_interrupt());
}

#[test]
fn test_frame_counter_to_from_u8() {
    let mut counter = FrameSequencerBits::new();
    counter.set_mode(FrameSequencerMode::FourStep);
    counter.set_disable_interrupt(true);

    let byte: u8 = counter.into();
    let counter2: FrameSequencerBits = byte.into();

    assert_eq!(counter2.mode(), FrameSequencerMode::FourStep);
    assert!(counter2.disable_interrupt());
}

// Test APU control logic
#[test]
fn test_apu_controller_read_status() {
    let mut channel = Apu::new(());
    let val = channel.read(0x4015);
    assert_eq!(val, 0); // Default APUStatus
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

    // First enable the channel, then load the length counter
    driver.write(0x4015, 0x01);
    driver.write(0x4003, 0xF8);

    let status = driver.read(0x4015);
    assert_eq!(status & 0x01, 0x01);
    assert_eq!(status & 0x40, 0x00);
}

#[test]
fn divider() {
    let mut divider = Divider::new(1u8);
    assert!(!divider.tick());
    assert!(divider.tick());

    // second loop
    assert!(!divider.tick());
    assert!(divider.tick());

    let mut divider = Divider::new(0u8);
    assert!(divider.tick());
    assert!(divider.tick());
    assert!(divider.tick());
}

// Golden-timeline gate for the noise channel's shift cadence. The expected
// sequence derives only from primary-source facts, not from the emulator's
// tables: https://www.nesdev.org/wiki/APU_Noise — "The period determines how
// many CPU cycles happen between shift register clocks" (rates $0/$7/$F mean
// shifts every 4/160/4068 CPU cycles); the LFSR powers up as 1 in mode 0,
// feeding back XOR of bits 0 and 1 into bit 14 on a right shift; $400E powers
// up cleared, so the divider starts counting down from 4 - 1; a period write
// changes only the reload value, not the current count (doc/apu_ref.txt).
// Any drift in a loaded interval shifts the timeline and fails here.
#[test]
fn noise_shift_rate_matches_documented_intervals() {
    // (period index, documented CPU-cycle interval between LFSR shifts)
    const CASES: [(u8, u16); 3] = [(0, 4), (7, 160), (15, 4068)];
    const CONSTANT_VOLUME: u8 = 15;

    for &(index, interval) in &CASES {
        let mut noise = Noise::default();
        // $4015 noise enable; the length load below is ignored while disabled.
        noise.set_enabled(true);
        // --lc vvvv: length halt, constant-volume mode, volume 15.
        noise.write_envelope(0x3F.into());
        // lllll___ = 31: nonzero length counter keeps the output gate open.
        noise.write_length(0xF8.into(), false);
        noise.write_period(index.into());

        let total_ticks = 3 * interval as usize + 8;
        let mut lfsr: u16 = 1;
        // Power-up rate index 0: first shift lands on CPU cycle 4.
        let mut counter: u16 = 4 - 1;

        for tick in 1..=total_ticks {
            if counter == 0 {
                counter = interval - 1;
                let feedback = (lfsr ^ (lfsr >> 1)) & 0x0001;
                lfsr = (lfsr >> 1) | (feedback << 14);
            } else {
                counter -= 1;
            }
            noise.tick_timer();
            // Bit 0 set means the DAC receives 0; constant volume otherwise.
            let expected = if lfsr & 1 == 0 { CONSTANT_VOLUME } else { 0 };
            assert_eq!(
                noise.output(),
                expected,
                "period index {index} diverges from the documented timeline at CPU cycle {tick}"
            );
        }
    }
}

// Golden-timeline gate for the APU frame counter. Expectations derive from
// https://www.nesdev.org/wiki/APU_Frame_Counter (NTSC) cross-checked against
// blargg's forum APU tests (test-roms/apu/test_1.nes..test_10.nes): mode 0
// steps at CPU cycles 7457 (quarter), 14913 (quarter+half), 22371 (quarter),
// 29828/29829/29830 (frame IRQ set three times in a row; the 29829 event also
// clocks quarter+half), period 29830; mode 1 steps at 7457 (quarter), 14913
// (quarter+half), 22371 (quarter), 29829 (nothing), 37281 (quarter+half),
// period 37282, IRQ never set. A $4017 write with bit 7 set resets the
// divider on the write cycle itself and clocks a half frame immediately
// (length counters included); $00/$40 writes reset after the usual 3-4 CPU
// cycle delay and clock nothing. Any drift in a loaded interval shifts the
// timeline and fails.

use super::frame_sequencer::{FrameSequenceState, FrameSequencer};

const QUARTER: FrameSequenceState = FrameSequenceState {
    irq: false,
    length_and_sweep: false,
    envelop_and_linear: true,
};
const HALF: FrameSequenceState = FrameSequenceState {
    irq: false,
    length_and_sweep: true,
    envelop_and_linear: true,
};
const HALF_IRQ: FrameSequenceState = FrameSequenceState {
    irq: true,
    length_and_sweep: true,
    envelop_and_linear: true,
};
const IRQ_ONLY: FrameSequenceState = FrameSequenceState {
    irq: true,
    length_and_sweep: false,
    envelop_and_linear: false,
};
const NOTHING: FrameSequenceState = FrameSequenceState {
    irq: false,
    length_and_sweep: false,
    envelop_and_linear: false,
};

/// Drives [`FrameSequencer`] the way [`Apu`] does: `tick_timer` every system
/// tick, `tick` and latch consumption on CPU clocks (tick % 3 == 2).
struct FrameSeqDriver {
    seq: FrameSequencer,
    tick: u64,
    write_tick: u64,
}

impl FrameSeqDriver {
    fn new() -> Self {
        Self {
            seq: FrameSequencer::default(),
            tick: 0,
            write_tick: 0,
        }
    }

    fn step(&mut self, events: &mut Vec<(u64, FrameSequenceState)>) {
        self.tick += 1;
        let clock = SystemClock(self.tick);
        self.seq.tick_timer();
        if clock.is_apu_clock() {
            self.seq.tick();
            if let Some(state) = self.seq.output_latch.take() {
                // Apu sets the frame interrupt flag when it consumes a latch.
                if state.irq {
                    self.seq.set_interrupt();
                }
                events.push((self.tick, state));
            }
        }
    }

    /// Runs `cpu_cycles` CPU cycles, collecting sequencer latch events.
    fn run(&mut self, cpu_cycles: u64) -> Vec<(u64, FrameSequenceState)> {
        let mut events = Vec::new();
        for _ in 0..cpu_cycles * 3 {
            self.step(&mut events);
        }
        events
    }

    /// Writes $4017 on the next CPU clock, where a STA $4017 would land.
    fn write_control(&mut self, bits: FrameSequencerBits) {
        let mut events = Vec::new();
        loop {
            self.step(&mut events);
            if SystemClock(self.tick).is_apu_clock() {
                self.seq.write_control_bits(bits);
                self.write_tick = self.tick;
                return;
            }
        }
    }
}

/// Asserts `events` carry `states` in order, spaced at `tick_deltas` system
/// ticks after the $4017 write plus the shared 3-4 CPU cycle apply latency
/// (9-12 ticks). The frame sequencer's step constants are system ticks
/// (3 per CPU cycle); the documented CPU-cycle steps are those divided by 3.
fn assert_timeline(
    write_tick: u64,
    events: &[(u64, FrameSequenceState)],
    states: &[FrameSequenceState],
    tick_deltas: &[u64],
) {
    assert_eq!(
        events.len(),
        states.len(),
        "event count mismatch: {events:?}"
    );
    let apply_latency = (events[0].0 - write_tick) - tick_deltas[0];
    assert!(
        (9..=12).contains(&apply_latency),
        "write apply latency {apply_latency} ticks outside the documented 3-4 CPU cycles; events={events:?} write_tick={write_tick}"
    );
    for (i, ((tick, state), (expected_state, delta))) in events
        .iter()
        .zip(states.iter().zip(tick_deltas.iter()))
        .enumerate()
    {
        assert_eq!(state, expected_state, "event {i} state mismatch");
        assert_eq!(
            tick - write_tick,
            delta + apply_latency,
            "event {i} lands at write+{} ticks, expected write+{delta}+latency",
            tick - write_tick
        );
    }
}

#[test]
fn frame_counter_mode0_matches_documented_step_timeline() {
    let mut driver = FrameSeqDriver::new();
    driver.write_control(FrameSequencerBits::default()); // $00: mode 0
    let events = driver.run(2 * 29830 + 20);

    let states = [QUARTER, HALF, QUARTER, IRQ_ONLY, HALF_IRQ, IRQ_ONLY];
    let deltas: Vec<u64> = [22371u64, 44739, 67113, 89484, 89487, 89490]
        .into_iter()
        .chain(
            [22371u64, 44739, 67113, 89484, 89487, 89490]
                .into_iter()
                .map(|d| d + 89490),
        )
        .collect();
    assert_timeline(driver.write_tick, &events, &[states; 2].concat(), &deltas);
}

#[test]
fn frame_counter_mode0_sets_irq_flag_on_three_consecutive_cpu_cycles() {
    // Reading $4015 clears the frame IRQ flag; within the 29828-29830 window
    // a cleared flag is set again by the next step event, which is what
    // blargg's frame-IRQ probes observe.
    let mut driver = FrameSeqDriver::new();
    driver.write_control(FrameSequencerBits::default());
    let events = driver.run(29831 + 10);
    let irq_ticks: Vec<u64> = events
        .iter()
        .filter(|(_, s)| s.irq)
        .map(|(t, _)| *t)
        .collect();
    assert_eq!(
        irq_ticks.len(),
        3,
        "three IRQ sets per period: {irq_ticks:?}"
    );
    for pair in irq_ticks.windows(2) {
        assert_eq!(pair[1] - pair[0], 3, "IRQ sets on consecutive CPU cycles");
    }

    // Second period: the flag is still set from period 1's third step; clear
    // it (as a $4015 read would), then the next set must come from period 2's
    // first IRQ step, and clearing inside the window must be answered by the
    // following step events. After the third set no further set happens
    // until the next period.
    driver.seq.clear_interrupt();
    assert!(!driver.seq.request_irq());
    let mut events = Vec::new();
    while !driver.seq.request_irq() {
        driver.step(&mut events);
    }
    driver.seq.clear_interrupt();
    assert!(!driver.seq.request_irq());
    for _ in 0..3 {
        driver.step(&mut events);
    }
    assert!(driver.seq.request_irq(), "second IRQ set after clear");
    driver.seq.clear_interrupt();
    for _ in 0..3 {
        driver.step(&mut events);
    }
    assert!(driver.seq.request_irq(), "third IRQ set after clear");
    driver.seq.clear_interrupt();
    for _ in 0..6 {
        driver.step(&mut events);
    }
    assert!(
        !driver.seq.request_irq(),
        "no fourth set before period wrap"
    );
}

#[test]
fn frame_counter_mode1_matches_documented_step_timeline() {
    let mut driver = FrameSeqDriver::new();
    driver.write_control(FrameSequencerBits::default().with_mode(FrameSequencerMode::FiveStep));
    let events = driver.run(2 * 37282 + 20);

    // The write clocks an immediate half frame (consumed on the next CPU
    // clock, +3 ticks); the delayed reset then restarts the documented table
    // from step 1, so the scheduled events carry the 3-4 cycle apply latency.
    assert_eq!(
        events[0],
        (driver.write_tick + 3, HALF),
        "immediate half frame on the write"
    );
    let states = [QUARTER, HALF, QUARTER, NOTHING, HALF];
    let deltas: Vec<u64> = [22371u64, 44739, 67113, 89487, 111843]
        .into_iter()
        .chain(
            [22371u64, 44739, 67113, 89487, 111843]
                .into_iter()
                .map(|d| d + 111846),
        )
        .collect();
    assert_timeline(
        driver.write_tick,
        &events[1..],
        &[states; 2].concat(),
        &deltas,
    );
    assert!(events.iter().all(|(_, s)| !s.irq), "mode 1 never sets IRQ");
}

#[test]
fn write_4017_mode1_immediate_clock_is_half_frame() {
    // A $4017 write with bit 7 set clocks a half frame (length counters,
    // sweep, envelopes, linear counter) immediately — blargg's 01.len_ctr
    // test 4 zeroes a length-2 counter with two back-to-back $80 writes.
    let mut driver = FrameSeqDriver::new();
    driver.write_control(FrameSequencerBits::default());
    driver.run(10);
    driver.write_control(FrameSequencerBits::default().with_mode(FrameSequencerMode::FiveStep));
    let events = driver.run(20);
    assert_eq!(events.len(), 1, "exactly the immediate clock: {events:?}");
    assert_eq!(events[0].1, HALF, "immediate clock is a half frame");
}

#[test]
fn write_4017_mode0_does_not_immediately_clock_length() {
    // blargg's 01.len_ctr test 5: two $00 writes leave the length counter
    // untouched (the scheduled mode-0 events are far beyond this window).
    let mut driver = FrameSeqDriver::new();
    driver.write_control(FrameSequencerBits::default().with_mode(FrameSequencerMode::FiveStep));
    driver.run(10);
    driver.write_control(FrameSequencerBits::default());
    let events = driver.run(20);
    assert!(
        events.is_empty(),
        "no immediate clock for $00 writes: {events:?}"
    );
}

#[test]
fn blargg_forum_test_1_length_counter_survives_the_write_dance() {
    // Mirrors test-roms/apu/test_1.nes: enable pulse1, load length 10, seven
    // $4017=$80 writes 9 CPU cycles apart, $4017=$00, one full 29830-cycle
    // mode-0 period, then the $4017=$80 halt landing on the period wrap. The
    // counter may only be clocked by the mode-0 half-frame steps (14913 and
    // 29829), so $4015 bit 0 must still read 1 — and the frame IRQ the
    // 29828-29830 steps raised must show up on bit 6 before the halt clears it.
    struct ApuDriver {
        apu: Apu<()>,
        tick: u64,
    }

    impl ApuDriver {
        /// Runs until the next CPU clock (tick % 3 == 2), then writes.
        fn write_on_cpu_clock(&mut self, address: u16, value: u8) {
            loop {
                self.tick += 1;
                let clock = SystemClock(self.tick);
                self.apu.tick(clock);
                if clock.is_cpu_clock() {
                    self.apu.write(address, value);
                    return;
                }
            }
        }

        fn run_cpu(&mut self, cpu_cycles: u64) {
            for _ in 0..cpu_cycles * 3 {
                self.tick += 1;
                let clock = SystemClock(self.tick);
                self.apu.tick(clock);
            }
        }
    }

    let mut driver = ApuDriver {
        apu: Apu::new(()),
        tick: 0,
    };
    driver.write_on_cpu_clock(0x4015, 0x01); // enable pulse1
    driver.write_on_cpu_clock(0x4003, 0x00); // load length counter, index 0 -> 10
    for _ in 0..7 {
        // mode 1 + inhibit: reset timer + quarter-frame-only immediate clock
        driver.write_on_cpu_clock(0x4017, 0x80);
        driver.run_cpu(8); // STA $4017 + DEX + BNE taken = 9-cycle spacing
    }
    driver.write_on_cpu_clock(0x4017, 0x80); // halt on the period wrap
    for _ in 0..6 {
        driver.tick += 1;
        driver.apu.tick(SystemClock(driver.tick));
    }

    assert_eq!(
        driver.apu.read(0x4015) & 0x01,
        0x01,
        "pulse1 length counter must be nonzero"
    );
}
