use crate::nes::apu::registers::{
    LengthTimerHigh3Bits, NoiseControlBits, NoiseLength, PulseControlBits, TriangleControlBits,
};
use bitfield_struct::bitfield;

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
            self.counter = 0;
        }
    }

    /// Should zero output if counter reaches zero
    pub fn is_zero(&self) -> bool {
        self.counter == 0
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
        let bits = EnvelopeBits::from_bits(bits);
        self.divider.set_period(bits.period());
        self.enable_loop = bits.enable_loop();
        self.disabled = bits.disabled();
    }

    pub fn tick(&mut self) {
        if std::mem::take(&mut self.request_reset) {
            self.counter = 15;
            self.divider.reset();
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
            *self.divider.period()
        } else {
            self.counter
        }
    }
}

pub(crate) trait CounterPub: PartialOrd + Copy + Sized {
    const ZERO: Self;

    fn dec(self) -> Self;
}

impl CounterPub for u16 {
    const ZERO: Self = 0;

    fn dec(self) -> Self {
        self.wrapping_sub(1)
    }
}

impl CounterPub for u8 {
    const ZERO: Self = 0;

    fn dec(self) -> Self {
        self.wrapping_sub(1)
    }
}

impl CounterPub for u32 {
    const ZERO: Self = 0;

    fn dec(self) -> Self {
        self.wrapping_sub(1)
    }
}

impl CounterPub for u64 {
    const ZERO: Self = 0;

    fn dec(self) -> Self {
        self.wrapping_sub(1)
    }
}

#[derive(Debug)]
pub struct Divider<C> {
    period: C,
    counter: C,
}

impl<C: CounterPub> Divider<C> {
    pub fn new(period: C) -> Self {
        Self {
            period,
            counter: period,
        }
    }

    pub fn tick(&mut self) -> bool {
        if self.counter == C::ZERO {
            self.reset();
            true
        } else {
            self.counter = self.counter.dec();
            false
        }
    }

    pub fn reset(&mut self) {
        self.counter = self.period;
    }

    pub fn set_period(&mut self, period: C) {
        self.period = period;
    }

    pub fn period(&self) -> &C {
        &self.period
    }
}

impl Divider<u16> {
    pub fn set_period_high(&mut self, high: u8) {
        self.period = (self.period & 0x00FF) | ((high as u16) << 8);
    }

    pub fn set_period_low(&mut self, low: u8) {
        self.period = (self.period & 0xFF00) | (low as u16);
    }

    pub fn period_mut(&mut self) -> &mut u16 {
        &mut self.period
    }
}

#[derive(Debug)]
pub struct Sequencer<I: 'static> {
    items: &'static [I],
    cur_idx: usize,
}

impl<I: Copy> Sequencer<I> {
    pub fn new(items: &'static [I]) -> Self {
        Self { items, cur_idx: 0 }
    }

    pub fn reset_items(&mut self, items: &'static [I]) {
        self.items = items;
        self.reset();
    }

    pub fn reset(&mut self) {
        self.cur_idx = 0;
    }

    pub fn tick(&mut self) -> I {
        let item = self.items[self.cur_idx];
        self.cur_idx = (self.cur_idx + 1) % self.items.len();
        item
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

pub trait ControlGate {
    fn control(&self) -> u8;

    fn filter(&self, val: u8) -> u8 {
        if self.control() == 0 { 0 } else { val }
    }
}

impl<U, V> ControlGate for (U, V)
where
    U: ControlGate,
    V: ControlGate,
{
    fn control(&self) -> u8 {
        let a = if self.0.control() != 0 { 1 } else { 0 };
        let b = if self.1.control() != 0 { 1 } else { 0 };
        a & b
    }
}

impl<U, V, W> ControlGate for (U, V, W)
where
    U: ControlGate,
    V: ControlGate,
    W: ControlGate,
{
    fn control(&self) -> u8 {
        let a = if self.0.control() != 0 { 1 } else { 0 };
        let b = if self.1.control() != 0 { 1 } else { 0 };
        let c = if self.2.control() != 0 { 1 } else { 0 };
        a & b & c
    }
}

impl ControlGate for &LengthControl {
    fn control(&self) -> u8 {
        debug_assert!(self.enabled || self.counter == 0);
        self.counter
    }
}

impl ControlGate for &AudioSequencer<8> {
    fn control(&self) -> u8 {
        self.output()
    }
}

pub trait AudioDriver {
    fn sample_rate(&self) -> u32;

    fn push_sample(&mut self, sample: f32);

    fn flush(&mut self) {}
}

impl<T: AudioDriver + ?Sized> AudioDriver for Box<T> {
    fn sample_rate(&self) -> u32 {
        (**self).sample_rate()
    }

    fn push_sample(&mut self, sample: f32) {
        (**self).push_sample(sample)
    }

    fn flush(&mut self) {
        (**self).flush()
    }
}

impl AudioDriver for () {
    fn sample_rate(&self) -> u32 {
        44_100
    }

    fn push_sample(&mut self, _sample: f32) {}
}
