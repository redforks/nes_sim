use super::*;
use crate::nes::apu::helper::{AudioSequencer, Sweep};

const PULSE_DUTY_TABLE: [[u8; 8]; 4] = [
    [0, 1, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 1, 1, 0, 0, 0],
    [1, 0, 0, 1, 1, 1, 1, 1],
];

pub(super) struct Pulse {
    timer: Divider<u16>,
    divider_2: Divider<u8>,
    length: LengthControl,
    envelope: Envelope,
    sweep: Sweep,
    sequencer: AudioSequencer<8>,
}

impl Pulse {
    pub fn new(ones_complement_negate: bool) -> Self {
        Self {
            timer: Divider::new(u16::MAX),
            divider_2: Divider::new(1),
            length: LengthControl::default(),
            envelope: Envelope::new(0),
            sweep: Sweep::new(ones_complement_negate, 0.into()),
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

    pub fn write_timer_high(&mut self, load: LengthTimerHigh3Bits) {
        self.timer.set_period_high(load.high3());
        self.length.load(load);
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
        self.sweep.tick(&mut self.timer.period);
    }

    pub fn output(&self) -> u8 {
        let control_gate = (&self.sweep, &self.sequencer, &self.length);
        control_gate.filter(self.envelope.output())
    }

    pub fn status_bit(&self) -> bool {
        !self.length.is_zero()
    }
}
