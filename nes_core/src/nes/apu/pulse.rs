use super::*;

#[derive(Default)]
pub(super) struct Pulse {
    control: DutyCycle,
    sweep: Sweep,
    timer: Timer<u16>,
    length_counter: u8,
    enabled: bool,
    sequence_step: usize,
    envelope: EnvelopeState,
    sweep_divider: u8,
    sweep_reload: bool,
    ones_complement_negate: bool,
}

impl Pulse {
    pub fn new(ones_complement_negate: bool) -> Self {
        Self {
            ones_complement_negate,
            ..Default::default()
        }
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.length_counter = 0;
        }
    }

    pub fn write_control(&mut self, value: DutyCycle) {
        self.control = value;
    }

    pub fn write_sweep(&mut self, value: Sweep) {
        self.sweep = value;
        self.sweep_reload = true;
    }

    pub fn write_timer_low(&mut self, value: u8) {
        self.timer.set_period_low(value);
    }

    pub fn write_timer_high(&mut self, load: LengthTimerHigh3Bits) {
        self.timer.set_period_high(load.high3());
        if self.enabled {
            self.length_counter = LENGTH_TABLE[load.length() as usize];
        }
        self.sequence_step = 0;
        self.envelope.restart();
    }

    pub fn step_timer(&mut self) {
        if self.timer.tick() {
            self.sequence_step = (self.sequence_step + 1) % 8;
        }
    }

    pub fn step_envelope(&mut self) {
        self.envelope
            .tick(self.control.volume(), self.control.length_counter_halt());
    }

    pub fn step_length_counter(&mut self) {
        if !self.control.length_counter_halt() && self.length_counter > 0 {
            self.length_counter -= 1;
        }
    }

    pub fn step_sweep(&mut self) {
        let apply = self.sweep_divider == 0
            && self.sweep.enabled()
            && self.sweep.shift() > 0
            && !self.sweep_mutes_channel();
        if apply {
            self.timer.period = self.sweep_target_period();
        }

        if self.sweep_divider == 0 || self.sweep_reload {
            self.sweep_divider = self.sweep.period();
            self.sweep_reload = false;
        } else {
            self.sweep_divider -= 1;
        }
    }

    fn current_volume(&self) -> u8 {
        if self.control.constant_volume() {
            self.control.volume()
        } else {
            self.envelope.decay
        }
    }

    fn sweep_target_period(&self) -> u16 {
        let change = self.timer.period >> self.sweep.shift();
        if self.sweep.negate() {
            self.timer
                .period
                .saturating_sub(change + u16::from(self.ones_complement_negate))
        } else {
            self.timer.period.saturating_add(change)
        }
    }

    fn sweep_mutes_channel(&self) -> bool {
        self.timer.period < 8 || self.sweep_target_period() > 0x07FF
    }

    pub fn output(&self) -> u8 {
        if !self.enabled || self.length_counter == 0 || self.sweep_mutes_channel() {
            return 0;
        }

        PULSE_DUTY_TABLE[self.control.duty() as usize][self.sequence_step] * self.current_volume()
    }

    pub fn status_enabled(&self) -> bool {
        self.enabled && self.length_counter > 0
    }
}
