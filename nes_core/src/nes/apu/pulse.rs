use super::*;
use crate::nes::apu::helper::AudioSequencer;

#[derive(Debug)]
struct Shifter {
    negate: bool,
    is_second_pulse_channel: bool,
    shift_bits: u8,
}

impl Shifter {
    fn new(is_second_pulse_channel: bool, bits: SweepBits) -> Self {
        let mut r = Self {
            negate: Default::default(),
            is_second_pulse_channel,
            shift_bits: Default::default(),
        };
        r.config(bits);
        r
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
}

impl Sweep {
    fn new(is_second_pulse_channel: bool, bits: SweepBits) -> Self {
        let divider = Divider::new(bits.period());
        Self {
            divider,
            shifter: Shifter::new(is_second_pulse_channel, bits),
            enabled: bits.enabled(),
            reload: false,
        }
    }

    fn config(&mut self, bits: SweepBits) {
        self.divider.set_period(bits.period());
        self.reload = true;
        self.shifter.config(bits);
        self.enabled = bits.enabled();
    }

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
    }

    fn is_muted(&self, period: u16) -> bool {
        let target = self.shifter.update_period(period);
        period < 8 || target > 0x7ff
    }
}

impl ControlGate for &Sweep {
    fn control(&self) -> u8 {
        // Muting is now a continuous predicate checked in Pulse::output via
        // is_muted(period), not a latched zero_output. For backwards
        // compatibility with tests that check control after tick, return 1
        // here; Pulse::output handles the actual muting.
        1
    }
}
const PULSE_DUTY_TABLE: [[u8; 8]; 4] = [
    [0, 1, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 1, 1, 0, 0, 0],
    [1, 0, 0, 1, 1, 1, 1, 1],
];

pub struct Pulse {
    timer: Divider<u16>,
    divider_2: Divider<u8>,
    length: LengthControl,
    envelope: Envelope,
    sweep: Sweep,
    sequencer: AudioSequencer<8>,
}

impl Pulse {
    pub fn new(is_second_pulse_channel: bool) -> Self {
        Self {
            timer: Divider::new(u16::MAX),
            divider_2: Divider::new(1),
            length: LengthControl::default(),
            envelope: Envelope::new(0),
            sweep: Sweep::new(is_second_pulse_channel, 0.into()),
            sequencer: AudioSequencer::new(&PULSE_DUTY_TABLE[0]),
        }
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.length.set_enabled(enabled);
    }

    pub fn write_control(&mut self, value: PulseControlBits) {
        self.envelope.config(value.into());
        self.length.set_halt(value);
        self.sequencer
            .replace_items(&PULSE_DUTY_TABLE[value.duty() as usize]);
    }

    pub fn write_sweep(&mut self, value: SweepBits) {
        self.sweep.config(value);
    }

    pub fn write_timer_low(&mut self, value: u8) {
        self.timer.set_period_low(value);
    }

    pub fn write_timer_high(&mut self, load: LengthTimerHigh3Bits, suppress_length: bool) {
        self.timer.set_period_high(load.high3());
        if !suppress_length {
            self.length.load(load);
        }
        self.envelope.request_reset();
        // When the fourth register is written to, the sequencer is restarted.
        self.sequencer.reset();
    }

    pub fn tick_timer(&mut self) {
        if self.timer.tick() && self.divider_2.tick() {
            self.sequencer.tick();
        }
    }

    pub fn tick_envelope(&mut self) {
        self.envelope.tick();
    }

    pub fn tick_length_and_sweep(&mut self) {
        self.length.tick();
        self.sweep.tick(self.timer.period_mut());
    }

    pub fn output(&self) -> u8 {
        if self.sweep.is_muted(*self.timer.period()) {
            return 0;
        }
        let control_gate = (&self.sequencer, &self.length);
        control_gate.filter(self.envelope.output())
    }

    pub fn status_bit(&self) -> bool {
        !self.length.is_zero()
    }

    pub fn length_will_decrement(&self) -> bool {
        self.length.will_decrement()
    }
}

#[cfg(test)]
mod tests;
