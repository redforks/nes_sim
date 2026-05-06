use super::*;

const NOISE_PERIOD_TABLE: [u16; 16] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
];

#[derive(Debug)]
struct Shifter {
    mode: u8,
    val: u16,
}

impl Shifter {
    fn tick(&mut self) {
        let tap = if self.mode == 1 { 6 } else { 1 };
        let feedback = (self.val ^ (self.val >> tap)) & 0x0001;
        self.val = (self.val >> 1) | (feedback << 14);
    }
}

impl<'a> ControlGate for &'a Shifter {
    fn control(&self) -> u8 {
        //  When bit 0 of the shift register is set, the DAC receives 0.
        if self.val & 0x1 != 0 { 0 } else { 1 }
    }
}

impl Default for Shifter {
    fn default() -> Self {
        Self { val: 1, mode: 0 }
    }
}

#[derive(Debug)]
pub struct Noise {
    period: NoisePeriod,
    timer: Divider<u16>,
    length: LengthControl,
    envelope: Envelope,
    shifter: Shifter,
}

impl Default for Noise {
    fn default() -> Self {
        Self {
            period: NoisePeriod::default(),
            timer: Divider::new(NOISE_PERIOD_TABLE[0]),
            length: LengthControl::default(),
            envelope: Envelope::new(0),
            shifter: Shifter::default(),
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
        self.timer
            .set_period(NOISE_PERIOD_TABLE[self.period.period() as usize]);
        self.shifter.mode = if value.is_halt() { 1 } else { 0 };
    }

    pub fn write_length(&mut self, value: NoiseLength) {
        self.length.load(value);
        self.envelope.request_reset();
    }

    pub fn tick_timer(&mut self) {
        if self.timer.tick() {
            self.shifter.tick();
        }
    }

    pub fn tick_envelope(&mut self) {
        self.envelope.tick();
    }

    pub fn tick_length(&mut self) {
        self.length.tick();
    }

    pub fn output(&self) -> u8 {
        let control_gate = (&self.shifter, &self.length);
        control_gate.filter(self.envelope.output())
    }

    pub fn status_bit(&self) -> bool {
        !self.length.is_zero()
    }
}
