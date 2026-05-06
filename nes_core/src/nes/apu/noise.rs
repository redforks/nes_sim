use super::*;

const NOISE_PERIOD_TABLE: [u16; 16] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
];

#[derive(Debug)]
pub struct Noise {
    control_bits: NoiseControlBits,
    period: NoisePeriod,
    timer_counter: u16,
    length: LengthControl,
    enabled: bool,
    shift_register: u16,
    envelope: Envelope,
}

impl Default for Noise {
    fn default() -> Self {
        Self {
            control_bits: NoiseControlBits::default(),
            period: NoisePeriod::default(),
            timer_counter: 0,
            length: LengthControl::default(),
            enabled: false,
            shift_register: 1,
            envelope: Envelope::new(0),
        }
    }
}

impl Noise {
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.length.clear();
        }
    }

    pub fn write_envelope(&mut self, value: NoiseControlBits) {
        self.control_bits = value;
        self.envelope.config(value.into());
        self.length.set_halt(value);
    }

    pub fn write_period(&mut self, value: NoisePeriod) {
        self.period = value;
    }

    pub fn write_length(&mut self, value: NoiseLength) {
        if self.enabled {
            self.length.load(value);
        }
        self.envelope.reset();
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
        self.envelope.tick();
    }

    pub fn step_length(&mut self) {
        self.length.tick();
    }

    pub fn output(&self) -> u8 {
        if !self.enabled || self.length.is_zero() || (self.shift_register & 0x0001) != 0 {
            0
        } else {
            self.envelope.output()
        }
    }

    pub fn status_bit(&self) -> bool {
        !self.length.is_zero()
    }

    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
}
