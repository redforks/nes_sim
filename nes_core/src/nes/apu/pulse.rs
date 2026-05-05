use crate::nes::apu::helper::Sweep;

use super::*;

const PULSE_DUTY_TABLE: [[u8; 8]; 4] = [
    [0, 1, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 1, 1, 0, 0, 0],
    [1, 0, 0, 1, 1, 1, 1, 1],
];

pub(super) struct Pulse {
    control: PulseControlBits,
    timer: Divider<u16>,
    length_control: LengthControl,
    enabled: bool,
    sequence_step: usize,
    envelope: Envelope,
    sweep: Sweep,
}

impl Pulse {
    pub fn new(ones_complement_negate: bool) -> Self {
        Self {
            control: PulseControlBits::default(),
            timer: Divider::new(u16::MAX),
            length_control: LengthControl::default(),
            enabled: false,
            sequence_step: 0,
            envelope: Envelope::new(0),
            sweep: Sweep::new(ones_complement_negate, 0.into()),
        }
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.length_control.clear();
        }
    }

    pub fn write_control(&mut self, value: PulseControlBits) {
        self.envelope.config(value.into());
        self.control = value;
        self.length_control.set_halt(value);
    }

    pub fn write_sweep(&mut self, value: SweepBits) {
        self.sweep.config(value);
    }

    pub fn write_timer_low(&mut self, value: u8) {
        self.timer.set_period_low(value);
    }

    pub fn write_timer_high(&mut self, load: LengthTimerHigh3Bits) {
        self.timer.set_period_high(load.high3());
        if self.enabled {
            self.length_control.load(load);
        }
        self.sequence_step = 0;
        self.envelope.reset();
    }

    pub fn step_timer(&mut self) {
        if self.timer.tick() {
            self.sequence_step = (self.sequence_step + 1) % 8;
        }
    }

    pub fn step_envelope(&mut self) {
        self.envelope.tick();
    }

    pub fn step_length_counter(&mut self) {
        self.length_control.tick();
    }

    pub fn step_sweep(&mut self) {
        self.sweep.tick(&mut self.timer.period);
    }

    fn current_volume(&self) -> u8 {
        if self.control.constant_volume() {
            self.control.volume()
        } else {
            self.envelope.output()
        }
    }

    pub fn output(&self) -> u8 {
        if !self.enabled || self.length_control.is_zero() || self.sweep.zero_output() {
            return 0;
        }

        PULSE_DUTY_TABLE[self.control.duty() as usize][self.sequence_step] * self.current_volume()
    }

    pub fn status_bit(&self) -> bool {
        !self.length_control.is_zero()
    }
}
