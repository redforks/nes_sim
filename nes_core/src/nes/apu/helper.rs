use bitfield_struct::bitfield;

use crate::nes::apu::{
    ControlGate, Divider, LengthTimerHigh3Bits, NoiseControlBits, NoiseLength, PulseControlBits,
    SweepBits, TriangleControlBits,
};

/// Abstract register that can be used to read length halt bit for noise and pulse channels.
pub trait GetLengthHalt {
    fn length_halt(&self) -> bool;
}

impl GetLengthHalt for PulseControlBits {
    fn length_halt(&self) -> bool {
        self.loop_and_is_halt()
    }
}

impl GetLengthHalt for TriangleControlBits {
    fn length_halt(&self) -> bool {
        self.loop_and_is_halt()
    }
}

impl GetLengthHalt for NoiseControlBits {
    fn length_halt(&self) -> bool {
        self.loop_and_is_halt()
    }
}

/// Abstract register that can be used to read length index for noise and pulse channels.
pub trait GetLengthIndex {
    fn length_idx(&self) -> u8;
}

impl GetLengthIndex for LengthTimerHigh3Bits {
    fn length_idx(&self) -> u8 {
        self.length_idx()
    }
}

impl GetLengthIndex for NoiseLength {
    fn length_idx(&self) -> u8 {
        self.length_idx()
    }
}

#[rustfmt::skip]
const LENGTH_TABLE: [u8; 32] = [
    0x0a, 0xfe, 0x14, 0x02, 0x28, 0x04, 0x50, 0x06, 0xa0, 0x08, 0x3c, 0x0a, 0x0e, 0x0c, 0x1a, 0x0e,
    0x0c, 0x10, 0x18, 0x12, 0x30, 0x14, 0x60, 0x16, 0xc0, 0x18, 0x48, 0x1a, 0x10, 0x1c, 0x20, 0x1e,
];

#[derive(Debug, Default)]
pub struct LengthControl {
    counter: u8,
    is_halt: bool,
    enabled: bool,
}

impl LengthControl {
    pub fn set_halt(&mut self, bits: impl GetLengthHalt) {
        self.is_halt = bits.length_halt();
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.clear();
        }
    }

    /// Should zero output if counter reaches zero
    pub fn is_zero(&self) -> bool {
        self.counter == 0
    }

    /// Clear the length counter.
    pub fn clear(&mut self) {
        self.counter = 0;
    }

    /// Load counter from length timer high 5 bits. Length timer high also reloads the counter.
    pub fn load(&mut self, bits: impl GetLengthIndex) {
        if self.enabled {
            self.counter = LENGTH_TABLE[bits.length_idx() as usize];
        }
    }

    /// Decrement counter if it's > 0.
    pub fn tick(&mut self) {
        if !self.is_halt {
            self.counter = self.counter.saturating_sub(1);
        }
    }
}

impl<'a> ControlGate for &'a LengthControl {
    fn control(&self) -> u8 {
        debug_assert!(self.enabled || self.counter == 0);
        self.counter
    }
}

#[bitfield(u8)]
struct EnvelopeBits {
    #[bits(4)]
    period: u8,
    disabled: bool,
    enable_loop: bool,
    #[bits(2)]
    __: u8,
}

#[derive(Debug)]
pub struct Envelope {
    divider: Divider<u8>,
    counter: u8,
    enable_loop: bool,
    disabled: bool,
    request_reset: bool,
}

impl Envelope {
    pub fn new(bits: u8) -> Self {
        let mut r = Self {
            divider: Divider::new(0),
            counter: 0,
            enable_loop: false,
            disabled: false,
            request_reset: false,
        };
        r.config(bits);
        r
    }

    pub fn request_reset(&mut self) {
        self.request_reset = true;
    }

    pub fn config(&mut self, bits: u8) {
        let bits = EnvelopeBits::from(bits);
        self.divider.set_period(bits.period());
        self.enable_loop = bits.enable_loop();
        self.disabled = bits.disabled();
    }

    fn reset(&mut self) {
        self.counter = 15;
        self.divider.reset();
    }

    pub fn tick(&mut self) {
        // When clocked by the frame sequencer, one of two actions occurs: if
        // there was a write to the fourth channel register since the last
        // clock, the counter is set to 15 and the divider is reset, otherwise
        // the divider is clocked.
        if std::mem::take(&mut self.request_reset) {
            self.reset();
        } else {
            if self.divider.tick() {
                if self.counter == 0 {
                    if self.enable_loop {
                        self.counter = 15;
                    }
                } else {
                    self.counter -= 1;
                }
            }
        }
    }

    pub fn output(&self) -> u8 {
        if self.disabled {
            self.divider.period
        } else {
            self.counter
        }
    }
}

#[derive(Debug)]
struct Shifter {
    negate: bool,
    /// The inverted value will inc 1 if is second pulse channel
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

    /// Shifter is disabled if shift bits is 0
    fn disabled(&self) -> bool {
        self.shift_bits == 0
    }

    fn config(&mut self, bits: SweepBits) {
        self.negate = bits.negate();
        self.shift_bits = bits.shift();
    }
}

#[derive(Debug)]
pub struct Sweep {
    divider: Divider<u8>,
    shifter: Shifter,
    // When the channel's period is less than 8 or the result of the shifter is
    // greater than $7FF, the channel's DAC receives 0
    zero_output: bool,
    enabled: bool,
}

impl Sweep {
    pub fn new(is_second_pulse_channel: bool, bits: SweepBits) -> Self {
        let divider = Divider::new(bits.period());
        Self {
            divider,
            shifter: Shifter::new(is_second_pulse_channel, bits),
            zero_output: false,
            enabled: bits.enabled(),
        }
    }

    pub fn config(&mut self, bits: SweepBits) {
        self.divider.set_period(bits.period());
        self.shifter.config(bits);
        self.enabled = bits.enabled();
    }

    pub fn tick(&mut self, period: &mut u16) {
        let new_period = self.shifter.update_period(*period);
        self.zero_output = *period < 8 || new_period > 0x7ff;

        if self.divider.tick() {
            if self.enabled && !self.shifter.disabled() && !self.zero_output {
                *period = new_period;
            }
        }
    }
}

impl<'a> ControlGate for &'a Sweep {
    fn control(&self) -> u8 {
        if self.zero_output { 0 } else { 1 }
    }
}

#[derive(Debug)]
pub struct AudioSequencer<const N: usize> {
    items: &'static [u8; N],
    cur_idx: usize,
}

impl<const N: usize> AudioSequencer<N> {
    pub fn new(items: &'static [u8; N]) -> Self {
        Self { items, cur_idx: 0 }
    }

    pub fn replace_items(&mut self, items: &'static [u8; N]) {
        self.items = items;
    }

    pub fn reset(&mut self) {
        self.cur_idx = 0;
    }

    pub fn tick(&mut self) {
        self.cur_idx = (self.cur_idx + 1) % N;
    }

    pub fn output(&self) -> u8 {
        self.items[self.cur_idx]
    }
}

impl<'a> ControlGate for &'a AudioSequencer<8> {
    fn control(&self) -> u8 {
        self.output()
    }
}

#[cfg(test)]
mod tests;
