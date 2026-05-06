use super::*;
use crate::nes::apu::helper::Sweep;

const PULSE_DUTY_TABLE: [[u8; 8]; 4] = [
    [0, 1, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 1, 1, 0, 0, 0],
    [1, 0, 0, 1, 1, 1, 1, 1],
];

pub(super) struct Pulse {
    timer: Divider<u16>,
    length: LengthControl,
    enabled: bool,
    envelope: Envelope,
    sweep: Sweep,
    sequencer: Sequencer<u8>,
}

impl Pulse {
    pub fn new(ones_complement_negate: bool) -> Self {
        Self {
            timer: Divider::new(u16::MAX),
            length: LengthControl::default(),
            enabled: false,
            envelope: Envelope::new(0),
            sweep: Sweep::new(ones_complement_negate, 0.into()),
            sequencer: Sequencer::new(&PULSE_DUTY_TABLE[0]),
        }
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.length.clear();
        }
    }

    pub fn write_control(&mut self, value: PulseControlBits) {
        self.envelope.config(value.into());
        self.length.set_halt(value);
        // Changes duty without resetting the position of the sequencer, not use `reset_items()`
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
        if self.enabled {
            self.length.load(load);
        }
        self.envelope.reset();
        // When the fourth register is written to, the sequencer is restarted.
        self.sequencer.reset();
    }

    pub fn tick_timer(&mut self) {
        if self.timer.tick() {
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
        if self.enabled {
            let control_gate = (&self.sweep, &self.sequencer, &self.length);
            control_gate.filter(self.envelope.output())
        } else {
            0
        }
    }

    pub fn status_bit(&self) -> bool {
        !self.length.is_zero()
    }
}
