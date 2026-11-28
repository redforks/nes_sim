use crate::nes::apu::helper::SequencerN;

use super::*;
#[derive(Debug)]
struct Shifter {
    negate: bool,
    is_second_pulse_channel: bool,
    shift_bits: u8,
}

impl Shifter {
    fn new(is_second_pulse_channel: bool, bits: SweepBits) -> Self {
        Self {
            negate: bits.negate(),
            is_second_pulse_channel,
            shift_bits: bits.shift(),
        }
    }

    fn update_period(&self, period: u16) -> u16 {
        let change = period >> self.shift_bits;
        if self.negate {
            if self.is_second_pulse_channel {
                period.saturating_sub(change)
            } else {
                period.saturating_sub(change).saturating_sub(1)
            }
        } else {
            period.saturating_add(change)
        }
    }

    fn disabled(&self) -> bool {
        self.shift_bits == 0
    }

    fn config(&mut self, bits: SweepBits) {
        self.negate = bits.negate();
        self.shift_bits = bits.shift();
    }
}

#[derive(Debug)]
struct Sweep {
    divider: Divider<u8>,
    shifter: Shifter,
    enabled: bool,
    reload: bool,
    /// Cached [`Self::is_muted`]: the mute flag only changes when the timer
    /// period or the sweep configuration changes (register writes, sweep
    /// ticks, construction, snapshot restore), while `Pulse::output` runs
    /// per emitted sample — so the sample path reads the cache and the
    /// mutation points refresh it. Never serialized: restore recomputes it.
    muted: bool,
}

impl Sweep {
    fn new(is_second_pulse_channel: bool, bits: SweepBits) -> Self {
        let divider = Divider::new(bits.period());
        Self {
            divider,
            shifter: Shifter::new(is_second_pulse_channel, bits),
            enabled: bits.enabled(),
            reload: false,
            // The owning `Pulse` refreshes this against its timer period
            // right after construction (the sweep never observes the period
            // except through `tick`/refresh).
            muted: false,
        }
    }

    fn config(&mut self, bits: SweepBits) {
        self.divider.set_period(bits.period());
        self.reload = true;
        self.shifter.config(bits);
        self.enabled = bits.enabled();
    }

    #[inline(always)]
    fn tick(&mut self, period: &mut u16) {
        if self.reload {
            self.divider.reset();
            self.reload = false;
        }

        let target = self.shifter.update_period(*period);
        let muted = *period < 8 || target > 0x7ff;

        if self.divider.tick() && self.enabled && !self.shifter.disabled() && !muted {
            *period = target;
        }
        self.muted = Self::muted_for(&self.shifter, *period);
    }

    fn is_muted(&self) -> bool {
        self.muted
    }

    /// Recompute the cached mute flag after the timer period or sweep
    /// configuration changed outside [`Self::tick`].
    fn refresh_muted(&mut self, period: u16) {
        self.muted = Self::muted_for(&self.shifter, period);
    }

    fn muted_for(shifter: &Shifter, period: u16) -> bool {
        let target = shifter.update_period(period);
        period < 8 || target > 0x7ff
    }
}

const PULSE_DUTY_TABLE: [[u8; 8]; 4] = [
    [0, 1, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 1, 1, 0, 0, 0],
    [1, 0, 0, 1, 1, 1, 1, 1],
];

pub struct Pulse {
    timer: DeadlineTimer,
    divider_2: Divider<u8>,
    length: LengthControl,
    envelope: Envelope,
    /// Cached [`Envelope::output`]: refreshed whenever the envelope ticks
    /// or is (re)configured, so per-sample mixing pays one load instead of
    /// the disabled/counter branch. Derived, never serialized (recomputed
    /// on restore like [`Sweep::muted`]).
    envelope_output: u8,
    sweep: Sweep,
    sequencer: SequencerN<u8, 8>,
}

/// Full [`Pulse`] channel state for save-states (ticket #184) in core types.
#[derive(Debug, Clone)]
pub(crate) struct PulseState {
    pub timer_period: u16,
    pub timer_counter: u16,
    pub divider2_period: u8,
    pub divider2_counter: u8,
    pub length_counter: u8,
    pub length_halt: bool,
    pub length_enabled: bool,
    pub envelope_period: u8,
    pub envelope_divider_counter: u8,
    pub envelope_counter: u8,
    pub envelope_loop: bool,
    pub envelope_disabled: bool,
    pub envelope_reset_pending: bool,
    pub sweep_divider_period: u8,
    pub sweep_divider_counter: u8,
    pub sweep_reload: bool,
    pub sweep_enabled: bool,
    pub sweep_negate: bool,
    pub sweep_is_second: bool,
    pub sweep_shift: u8,
    /// Index into [`PULSE_DUTY_TABLE`] (0-3).
    pub duty: u8,
    pub sequencer_position: usize,
}

impl Pulse {
    /// Capture the full channel state. The duty index resolves by pointer
    /// equality against the `PULSE_DUTY_TABLE` rows.
    pub(crate) fn capture_pulse_state(&self, now: u64) -> PulseState {
        let duty = PULSE_DUTY_TABLE
            .iter()
            .position(|row| std::ptr::eq(self.sequencer.items().as_ptr(), row.as_ptr()))
            .unwrap_or(0) as u8;
        PulseState {
            timer_period: self.timer.period(),
            timer_counter: self.timer.counter(now),
            divider2_period: self.divider_2.period(),
            divider2_counter: self.divider_2.counter(),
            length_counter: self.length.counter_value(),
            length_halt: self.length.is_halt(),
            length_enabled: self.length.is_enabled(),
            envelope_period: self.envelope.divider_period(),
            envelope_divider_counter: self.envelope.divider_counter(),
            envelope_counter: self.envelope.counter_value(),
            envelope_loop: self.envelope.loop_enabled(),
            envelope_disabled: self.envelope.is_disabled(),
            envelope_reset_pending: self.envelope.reset_pending(),
            sweep_divider_period: self.sweep.divider.period(),
            sweep_divider_counter: self.sweep.divider.counter(),
            sweep_reload: self.sweep.reload,
            sweep_enabled: self.sweep.enabled,
            sweep_negate: self.sweep.shifter.negate,
            sweep_is_second: self.sweep.shifter.is_second_pulse_channel,
            sweep_shift: self.sweep.shifter.shift_bits,
            duty,
            sequencer_position: self.sequencer.position(),
        }
    }

    /// Restore a state previously produced by [`Self::capture_pulse_state`].
    pub(crate) fn restore_pulse_state(&mut self, s: &PulseState) {
        self.timer.restore(s.timer_period, s.timer_counter);
        self.divider_2
            .restore(s.divider2_period, s.divider2_counter);
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
        self.sweep
            .divider
            .restore(s.sweep_divider_period, s.sweep_divider_counter);
        self.sweep.reload = s.sweep_reload;
        self.sweep.enabled = s.sweep_enabled;
        self.sweep.shifter.negate = s.sweep_negate;
        self.sweep.shifter.is_second_pulse_channel = s.sweep_is_second;
        self.sweep.shifter.shift_bits = s.sweep_shift;
        // Recomputed, never serialized: must run after every sweep field
        // above lands or the cache reflects the pre-restore configuration.
        self.sweep.refresh_muted(self.timer.period());
        let duty = (s.duty as usize).min(PULSE_DUTY_TABLE.len() - 1);
        self.sequencer
            .restore(&PULSE_DUTY_TABLE[duty], s.sequencer_position);
    }
}

impl Pulse {
    pub fn new(is_second_pulse_channel: bool) -> Self {
        let mut channel = Self {
            timer: DeadlineTimer::new(u16::MAX),
            divider_2: Divider::new(1),
            length: LengthControl::default(),
            envelope: Envelope::default(),
            envelope_output: Envelope::default().output(),
            sweep: Sweep::new(is_second_pulse_channel, SweepBits::from_bytes([0])),
            sequencer: SequencerN::new(&PULSE_DUTY_TABLE[0]),
        };
        channel.sweep.refresh_muted(channel.timer.period());
        channel
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.length.set_enabled(enabled);
    }

    pub fn write_control(&mut self, value: PulseControlBits) {
        self.envelope.config(value.into_bytes()[0]);
        self.envelope_output = self.envelope.output();
        self.length.set_halt(value.loop_and_is_halt());
        self.sequencer
            .replace_items(&PULSE_DUTY_TABLE[value.duty() as usize]);
    }

    pub fn write_sweep(&mut self, value: SweepBits) {
        self.sweep.config(value);
        self.sweep.refresh_muted(self.timer.period());
    }

    pub fn write_timer_low(&mut self, value: u8) {
        self.timer.set_period_low(value);
        self.sweep.refresh_muted(self.timer.period());
    }

    pub fn write_timer_high(&mut self, load: LengthTimerHigh3Bits, suppress_length: bool) {
        self.timer.set_period_high(load.high3());
        self.sweep.refresh_muted(self.timer.period());
        if !suppress_length {
            self.length.load(load.length_idx());
        }
        self.envelope.request_reset();
        // When the fourth register is written to, the sequencer is restarted.
        self.sequencer.reset();
    }

    /// Next firing tick of the rate timer (see `DeadlineTimer::deadline`).
    pub(crate) fn timer_deadline(&self) -> u64 {
        self.timer.deadline()
    }

    #[inline(always)]
    pub fn tick_timer(&mut self, now: u64) {
        if self.timer.tick(now) && self.divider_2.tick() {
            self.sequencer.tick();
        }
    }

    #[inline(always)]
    pub fn tick_envelope(&mut self) {
        self.envelope.tick();
        self.envelope_output = self.envelope.output();
    }

    #[inline(always)]
    pub fn tick_length_and_sweep(&mut self) -> bool {
        let decremented = self.length.tick();
        self.sweep.tick(self.timer.period_mut());
        decremented
    }

    pub fn output(&self) -> u8 {
        // Branchless gate: the duty sequencer and length counter both change
        // often enough that short-circuit branches mispredict.
        let gate = (self.sequencer.output() != 0) & !self.length.is_zero() & !self.sweep.is_muted();
        self.envelope_output * gate as u8
    }

    pub fn status_bit(&self) -> bool {
        !self.length.is_zero()
    }
}

#[cfg(test)]
mod tests;
