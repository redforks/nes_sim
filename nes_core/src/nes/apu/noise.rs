use super::*;

const NOISE_PERIOD_TABLE: [u16; 16] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
];

#[derive(Debug)]
pub struct Noise {
    period: NoisePeriod,
    timer_counter: u16,
    length: LengthControl,
    shift_register: u16,
    envelope: Envelope,
}

impl Default for Noise {
    fn default() -> Self {
        Self {
            period: NoisePeriod::default(),
            timer_counter: 0,
            length: LengthControl::default(),
            shift_register: 1,
            envelope: Envelope::new(0),
        }
    }
}

impl Noise {
    pub fn set_enabled(&mut self, enabled: bool) {
        self.length.set_enabled(enabled);
    }

    pub fn write_envelope(&mut self, value: NoiseControlBits) {
        self.envelope.config(value.into());
        self.length.set_halt(value);
    }

    pub fn write_period(&mut self, value: NoisePeriod) {
        self.period = value;
    }

    pub fn write_length(&mut self, value: NoiseLength) {
        self.length.load(value);
        self.envelope.request_reset();
    }

    pub fn tick_timer(&mut self) {
        if self.timer_counter == 0 {
            self.timer_counter = NOISE_PERIOD_TABLE[self.period.period() as usize];
            let tap = if self.period.is_halt() { 6 } else { 1 };
            let feedback = (self.shift_register ^ (self.shift_register >> tap)) & 0x0001;
            self.shift_register = (self.shift_register >> 1) | (feedback << 14);
        } else {
            self.timer_counter -= 1;
        }
    }

    pub fn tick_envelope(&mut self) {
        self.envelope.tick();
    }

    pub fn tick_length(&mut self) {
        self.length.tick();
    }

    pub fn output(&self) -> u8 {
        if self.length.is_zero() || (self.shift_register & 0x0001) != 0 {
            0
        } else {
            self.envelope.output()
        }
    }

    pub fn status_bit(&self) -> bool {
        !self.length.is_zero()
    }
}
