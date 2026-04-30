use super::*;

const NOISE_PERIOD_TABLE: [u16; 16] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
];

#[derive(Debug)]
pub struct Noise {
    envelope: NoiseControlBits,
    period: NoisePeriod,
    timer_counter: u16,
    length_control: LengthControl,
    enabled: bool,
    shift_register: u16,
    envelope_state: EnvelopeState,
}

impl Default for Noise {
    fn default() -> Self {
        Self {
            envelope: NoiseControlBits::default(),
            period: NoisePeriod::default(),
            timer_counter: 0,
            length_control: LengthControl::default(),
            enabled: false,
            shift_register: 1,
            envelope_state: EnvelopeState::default(),
        }
    }
}

impl Noise {
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.length_control.clear();
        }
    }

    pub fn write_envelope(&mut self, value: NoiseControlBits) {
        self.envelope = value;
    }

    pub fn write_period(&mut self, value: NoisePeriod) {
        self.period = value;
    }

    pub fn write_length(&mut self, value: NoiseLength) {
        if self.enabled {
            self.length_control.load(value.length());
        }
        self.envelope_state.restart();
    }

    pub fn step_timer(&mut self) {
        if self.timer_counter == 0 {
            self.timer_counter = NOISE_PERIOD_TABLE[self.period.period() as usize];
            let tap = if self.period.is_halt() { 6 } else { 1 };
            let feedback = (self.shift_register ^ (self.shift_register >> tap)) & 0x0001;
            self.shift_register = (self.shift_register >> 1) | (feedback << 14);
        } else {
            self.timer_counter -= 1;
        }
    }

    pub fn step_envelope(&mut self) {
        self.envelope_state
            .tick(self.envelope.volume(), self.envelope.loop_flag());
    }

    pub fn step_length_counter(&mut self) {
        if !self.envelope.loop_flag() {
            self.length_control.tick();
        }
    }

    fn current_volume(&self) -> u8 {
        if self.envelope.constant_volume() {
            self.envelope.volume()
        } else {
            self.envelope_state.decay
        }
    }

    pub fn output(&self) -> u8 {
        if !self.enabled || self.length_control.disabled() || (self.shift_register & 0x0001) != 0 {
            0
        } else {
            self.current_volume()
        }
    }

    pub fn status_enabled(&self) -> bool {
        self.enabled && !self.length_control.disabled()
    }

    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
}
