use super::*;

const NOISE_PERIOD_TABLE: [u16; 16] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
];

#[derive(Debug)]
struct Shifter {
    short_mode: bool,
    val: u16,
}

impl Shifter {
    fn tick(&mut self) {
        let tap = if self.short_mode { 6 } else { 1 };
        let feedback = (self.val ^ (self.val >> tap)) & 0x0001;
        self.val = (self.val >> 1) | (feedback << 14);
    }
}

impl Default for Shifter {
    fn default() -> Self {
        Self {
            short_mode: false,
            val: 1,
        }
    }
}

#[derive(Debug)]
pub struct Noise {
    timer: DeadlineTimer,
    length: LengthControl,
    envelope: Envelope,
    /// Cached [`Envelope::output`], refreshed on tick/configure/restore
    /// (same contract as [`super::pulse::Pulse`]'s cache).
    envelope_output: u8,
    shifter: Shifter,
}

impl Default for Noise {
    fn default() -> Self {
        Self {
            // $400E powers up cleared: rate index 0, so load entry 0 - 1 to
            // match what writing that index produces (see write_period).
            timer: DeadlineTimer::new(NOISE_PERIOD_TABLE[0] - 1),
            length: LengthControl::default(),
            envelope: Envelope::default(),
            envelope_output: Envelope::default().output(),
            shifter: Shifter::default(),
        }
    }
}

/// Full [`Noise`] channel state for save-states (ticket #184).
#[derive(Debug, Clone)]
pub(crate) struct NoiseState {
    pub timer_period: u16,
    pub timer_counter: u16,
    pub length_counter: u8,
    pub length_halt: bool,
    pub length_enabled: bool,
    pub envelope_period: u8,
    pub envelope_divider_counter: u8,
    pub envelope_counter: u8,
    pub envelope_loop: bool,
    pub envelope_disabled: bool,
    pub envelope_reset_pending: bool,
    pub shifter_short_mode: bool,
    pub shifter_val: u16,
}

impl Noise {
    /// Capture the full channel state.
    pub(crate) fn capture_noise_state(&self, now: u64) -> NoiseState {
        NoiseState {
            timer_period: self.timer.period(),
            timer_counter: self.timer.counter(now),
            length_counter: self.length.counter_value(),
            length_halt: self.length.is_halt(),
            length_enabled: self.length.is_enabled(),
            envelope_period: self.envelope.divider_period(),
            envelope_divider_counter: self.envelope.divider_counter(),
            envelope_counter: self.envelope.counter_value(),
            envelope_loop: self.envelope.loop_enabled(),
            envelope_disabled: self.envelope.is_disabled(),
            envelope_reset_pending: self.envelope.reset_pending(),
            shifter_short_mode: self.shifter.short_mode,
            shifter_val: self.shifter.val,
        }
    }

    /// Restore a state previously produced by [`Self::capture_noise_state`].
    pub(crate) fn restore_noise_state(&mut self, s: &NoiseState) {
        self.timer.restore(s.timer_period, s.timer_counter);
        self.length
            .restore(s.length_counter, s.length_halt, s.length_enabled);
        self.envelope.restore(
            s.envelope_period,
            s.envelope_divider_counter,
            s.envelope_counter,
            s.envelope_loop,
            s.envelope_disabled,
            s.envelope_reset_pending,
        );
        self.envelope_output = self.envelope.output();
        self.shifter.short_mode = s.shifter_short_mode;
        self.shifter.val = s.shifter_val;
    }
}

impl Noise {
    pub fn set_enabled(&mut self, enabled: bool) {
        self.length.set_enabled(enabled);
    }

    pub fn write_envelope(&mut self, value: NoiseControlBits) {
        self.envelope.config(value.into_bytes()[0]);
        self.envelope_output = self.envelope.output();
        self.length.set_halt(value.loop_and_is_halt());
    }

    pub fn write_period(&mut self, value: NoisePeriod) {
        // https://www.nesdev.org/wiki/APU_Noise — "The period determines how
        // many CPU cycles happen between shift register clocks": table entries
        // already are CPU-cycle intervals. The divider fires once per
        // period + 1 ticks, so loading entry - 1 makes the shift interval
        // equal the documented entry for all sixteen rates.
        self.timer
            .set_period(NOISE_PERIOD_TABLE[value.period() as usize] - 1);
        self.shifter.short_mode = value.is_halt();
    }

    pub fn write_length(&mut self, value: NoiseLength, suppress_length: bool) {
        if !suppress_length {
            self.length.load(value.length_idx());
        }
        self.envelope.request_reset();
    }

    /// Next firing tick of the rate timer (see `DeadlineTimer::deadline`).
    pub(crate) fn timer_deadline(&self) -> u64 {
        self.timer.deadline()
    }

    #[inline(always)]
    pub fn tick_timer(&mut self, now: u64) {
        if self.timer.tick(now) {
            self.shifter.tick();
        }
    }

    #[inline(always)]
    pub fn tick_envelope(&mut self) {
        self.envelope.tick();
        self.envelope_output = self.envelope.output();
    }

    pub fn tick_length(&mut self) -> bool {
        self.length.tick()
    }

    pub fn output(&self) -> u8 {
        // Bit 0 of the shift register drives the DAC to zero; a zero length
        // counter closes the channel gate. Keep this branchless: the LFSR bit
        // changes often enough that a branch here mispredicts constantly.
        let gate = ((self.shifter.val & 1 == 0) & !self.length.is_zero()) as u8;
        self.envelope_output * gate
    }

    pub fn status_bit(&self) -> bool {
        !self.length.is_zero()
    }
}
