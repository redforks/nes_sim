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
    pub fn set_halt(&mut self, is_halt: bool) {
        self.is_halt = is_halt;
    }

    pub fn is_halt(&self) -> bool {
        self.is_halt
    }

    pub fn set_halt_flag(&mut self, is_halt: bool) {
        self.is_halt = is_halt;
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

    /// Raw counter value for save-state capture (ticket #190).
    pub fn counter_value(&self) -> u8 {
        self.counter
    }

    /// Whether the channel is enabled (length loads gate on this).
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Restores a captured counter/halt/enabled triple without tripping
    /// the enable-clear side effect of [`Self::set_enabled`].
    pub fn restore(&mut self, counter: u8, halt: bool, enabled: bool) {
        self.counter = counter;
        self.is_halt = halt;
        self.enabled = enabled;
    }

    /// Load counter from length timer high 5 bits. Length timer high also reloads the counter.
    pub fn load(&mut self, length_idx: u8) {
        if self.enabled {
            self.counter = LENGTH_TABLE[length_idx as usize];
        }
    }

    /// Decrement counter if it's > 0, returning whether it did. A same-tick
    /// reload write must be suppressed when this returns true.
    pub fn tick(&mut self) -> bool {
        if self.is_halt {
            return false;
        }

        let r = self.counter != 0;
        self.counter = self.counter.saturating_sub(1);
        r
    }
}

/// Register view for [`Envelope::config`]. Wrapped in a module so the
/// `#[allow(dead_code)]` silences the generated `new()` constructor
/// (construction goes through `from_bytes`): a struct-level allow does not
/// propagate into modular-bitfield's generated `impl` (upstream issue #56).
#[allow(dead_code)]
mod envelope_bits {
    use modular_bitfield::prelude::*;

    #[bitfield]
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
    pub(super) struct EnvelopeBits {
        pub(super) period: B4,
        pub(super) disabled: bool,
        pub(super) enable_loop: bool,
        #[skip]
        __: B2,
    }
}
use envelope_bits::EnvelopeBits;

#[derive(Debug)]
pub struct Envelope {
    divider: Divider<u8>,
    counter: u8,
    enable_loop: bool,
    disabled: bool,
    request_reset: bool,
}

impl Default for Envelope {
    fn default() -> Self {
        Self {
            divider: Divider::new(0),
            counter: 0,
            enable_loop: false,
            disabled: false,
            request_reset: false,
        }
    }
}

impl Envelope {
    pub fn request_reset(&mut self) {
        self.request_reset = true;
    }

    pub fn config(&mut self, bits: u8) {
        let bits = EnvelopeBits::from_bytes([bits]);
        self.divider.set_period(bits.period());
        self.enable_loop = bits.enable_loop();
        self.disabled = bits.disabled();
    }

    #[inline(always)]
    pub fn tick(&mut self) {
        if self.request_reset {
            self.request_reset = false;
            self.counter = 15;
            self.divider.reset();
            return;
        }

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

    pub fn output(&self) -> u8 {
        if self.disabled {
            self.divider.period()
        } else {
            self.counter
        }
    }

    /// Divider period (`$x000` volume/rate nibble) for save-state capture.
    pub fn divider_period(&self) -> u8 {
        self.divider.period()
    }

    /// Divider countdown for save-state capture (ticket #190).
    pub fn divider_counter(&self) -> u8 {
        self.divider.counter()
    }

    /// Decay counter for save-state capture (ticket #190).
    pub fn counter_value(&self) -> u8 {
        self.counter
    }

    /// Loop flag for save-state capture (ticket #190).
    pub fn loop_enabled(&self) -> bool {
        self.enable_loop
    }

    /// Constant-volume flag for save-state capture (ticket #190).
    pub fn is_disabled(&self) -> bool {
        self.disabled
    }

    /// Pending start-flag reset for save-state capture (ticket #190).
    pub fn reset_pending(&self) -> bool {
        self.request_reset
    }

    /// Restores a captured envelope without running a frame tick.
    pub fn restore(
        &mut self,
        period: u8,
        divider_counter: u8,
        counter: u8,
        enable_loop: bool,
        disabled: bool,
        request_reset: bool,
    ) {
        self.divider.restore(period, divider_counter);
        self.counter = counter;
        self.enable_loop = enable_loop;
        self.disabled = disabled;
        self.request_reset = request_reset;
    }
}

pub(crate) trait CounterPub: PartialOrd + Copy + Sized {
    const ZERO: Self;

    fn dec(self) -> Self;
}

impl CounterPub for u16 {
    const ZERO: Self = 0;

    fn dec(self) -> Self {
        self - 1
    }
}

impl CounterPub for u8 {
    const ZERO: Self = 0;

    fn dec(self) -> Self {
        self - 1
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Divider<C: Copy> {
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
        let r = self.counter == C::ZERO;
        if r {
            self.reset();
        } else {
            self.counter = self.counter.dec();
        }
        r
    }

    pub fn reset(&mut self) {
        self.counter = self.period;
    }

    pub fn set_period(&mut self, period: C) {
        self.period = period;
    }

    pub fn period(self) -> C {
        self.period
    }

    /// Countdown for save-state capture (ticket #190).
    pub fn counter(&self) -> C {
        self.counter
    }

    /// Restores a captured period/countdown pair verbatim.
    pub fn restore(&mut self, period: C, counter: C) {
        self.period = period;
        self.counter = counter;
    }
}

/// Audio-rate countdown as a firing deadline over the APU's monotonic tick
/// counter. A [`Divider`] pays a counter write-back every tick; the five
/// audio timers (both pulse timers, triangle, noise, DMC) tick once per
/// CPU cycle (~30k/frame), so eliding the write-back on the common
/// non-firing tick removes ~150k counter stores per frame — `tick` only
/// stores on the rare firing tick.
///
/// Fire cadence is identical to [`Divider`]: from a `new(period)` state the
/// first fire lands `period + 1` ticks out, then every `period + 1` ticks.
/// `now` is the owning [`super::Apu`]'s tick counter (see
/// [`super::Apu::restore_apu_state`], which re-anchors the epoch on load),
/// so the persisted countdown values are unchanged and the snapshot format
/// is untouched.
#[derive(Debug, Clone, Copy)]
pub struct DeadlineTimer {
    period: u16,
    deadline: u64,
}

impl DeadlineTimer {
    pub fn new(period: u16) -> Self {
        // Matches Divider::new(period): the countdown starts at `period`,
        // i.e. the first fire lands period + 1 ticks after the epoch.
        Self {
            period,
            deadline: period as u64 + 1,
        }
    }

    /// Advance one tick against the APU tick counter. Non-firing ticks are
    /// a load-compare-branch with no store; only the firing tick stores
    /// the next deadline.
    pub fn tick(&mut self, now: u64) -> bool {
        if now == self.deadline {
            self.deadline = now + self.period as u64 + 1;
            true
        } else {
            false
        }
    }

    pub fn set_period(&mut self, period: u16) {
        self.period = period;
    }

    pub fn set_period_high(&mut self, high: u8) {
        self.period = (self.period & 0x00FF) | ((high as u16) << 8);
    }

    pub fn set_period_low(&mut self, low: u8) {
        self.period = (self.period & 0xFF00) | (low as u16);
    }

    pub fn period_mut(&mut self) -> &mut u16 {
        &mut self.period
    }

    pub fn period(self) -> u16 {
        self.period
    }

    /// Ticks remaining until the firing tick, mirroring
    /// [`Divider::counter`]: the firing tick itself reports 0. The caller
    /// passes the same `now` the ticks advance against, so the result is
    /// always in `0..=period` (the deadline is at least one tick out).
    pub fn counter(&self, now: u64) -> u16 {
        debug_assert!(self.deadline > now);
        (self.deadline - now - 1) as u16
    }

    /// Restore a captured `(period, counter)` pair against a zeroed epoch
    /// (the owner zeroes its tick counter first): the next fire lands
    /// `counter + 1` ticks out, exactly like [`Divider::restore`].
    pub fn restore(&mut self, period: u16, counter: u16) {
        self.period = period;
        self.deadline = counter as u64 + 1;
    }
}

impl Divider<u16> {
    pub fn set_period_high(&mut self, high: u8) {
        self.period = (self.period & 0x00FF) | ((high as u16) << 8);
    }

    pub fn set_period_low(&mut self, low: u8) {
        self.period = (self.period & 0xFF00) | (low as u16);
    }
}

#[derive(Debug)]
pub struct SequencerN<I: 'static, const N: usize> {
    items: &'static [I; N],
    cur_idx: usize,
}

impl<I: Copy, const N: usize> SequencerN<I, N> {
    pub fn new(items: &'static [I; N]) -> Self {
        Self { items, cur_idx: 0 }
    }

    pub fn replace_items(&mut self, items: &'static [I; N]) {
        self.items = items;
        self.cur_idx = self.cur_idx.min(N - 1);
    }

    pub fn reset(&mut self) {
        self.cur_idx = 0;
    }

    pub fn tick(&mut self) -> I {
        let item = unsafe { *self.items.get_unchecked(self.cur_idx) };
        self.cur_idx = (self.cur_idx + 1) % N;
        item
    }

    pub fn output(&self) -> I {
        unsafe { *self.items.get_unchecked(self.cur_idx) }
    }

    /// Step position for save-state capture (ticket #190).
    pub fn position(&self) -> usize {
        self.cur_idx
    }

    /// Backing table for save-state capture (ticket #190): the caller
    /// maps the pointer to a table selector it can persist.
    pub fn items(&self) -> &'static [I] {
        self.items
    }

    /// Restores a captured table + step position verbatim.
    pub fn restore(&mut self, items: &'static [I; N], position: usize) {
        self.items = items;
        self.cur_idx = position.min(N.saturating_sub(1));
    }
}
