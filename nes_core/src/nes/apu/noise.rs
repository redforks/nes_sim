use super::*;

const NOISE_PERIOD_TABLE: [u16; 16] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
];

#[derive(Debug)]
struct Shifter {
    short_mode: bool,
    val: u16,
}

impl Shifter {
    fn tick(&mut self) {
        let tap = if self.short_mode { 6 } else { 1 };
        let feedback = (self.val ^ (self.val >> tap)) & 0x0001;
        self.val = (self.val >> 1) | (feedback << 14);
    }
}

impl Default for Shifter {
    fn default() -> Self {
        Self {
            short_mode: false,
            val: 1,
        }
    }
}

#[derive(Debug)]
pub struct Noise {
    timer: Divider<u16>,
    length: LengthControl,
    envelope: Envelope,
    shifter: Shifter,
}

impl Default for Noise {
    fn default() -> Self {
        Self {
            // $400E powers up cleared: rate index 0, so load entry 0 - 1 to
            // match what writing that index produces (see write_period).
            timer: Divider::new(NOISE_PERIOD_TABLE[0] - 1),
            length: LengthControl::default(),
            envelope: Envelope::default(),
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
        // https://www.nesdev.org/wiki/APU_Noise — "The period determines how
        // many CPU cycles happen between shift register clocks": table entries
        // already are CPU-cycle intervals. The divider fires once per
        // period + 1 ticks, so loading entry - 1 makes the shift interval
        // equal the documented entry for all sixteen rates.
        self.timer
            .set_period(NOISE_PERIOD_TABLE[value.period() as usize] - 1);
        self.shifter.short_mode = value.is_halt();
    }

    pub fn write_length(&mut self, value: NoiseLength, suppress_length: bool) {
        if !suppress_length {
            self.length.load(value);
        }
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

    pub fn tick_length(&mut self) -> bool {
        self.length.tick()
    }

    pub fn output(&self) -> u8 {
        // Bit 0 of the shift register drives the DAC to zero; a zero length
        // counter closes the channel gate. Keep this branchless: the LFSR bit
        // changes often enough that a branch here mispredicts constantly.
        let gate = ((self.shifter.val & 1 == 0) & !self.length.is_zero()) as u8;
        self.envelope.output() * gate
    }

    pub fn status_bit(&self) -> bool {
        !self.length.is_zero()
    }
}
