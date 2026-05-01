use crate::{SYSTEM_CYCLES_PER_CPU_CYCLE, get_system_cycles};
use bitfield_struct::{bitenum, bitfield};
use std::ops::SubAssign;

mod dmc;
mod frame_sequencer;
mod helper;
mod noise;
mod pulse;
mod triangle;

use dmc::Dmc;
use frame_sequencer::FrameSequencer;
use helper::LengthControl;
use noise::Noise;
use pulse::Pulse;
use triangle::Triangle;

trait Counter: Eq + SubAssign + Copy + Sized {
    const ZERO: Self;
    const ONE: Self;
}

impl Counter for u16 {
    const ZERO: Self = 0;
    const ONE: Self = 1;
}

impl Counter for u8 {
    const ZERO: Self = 0;
    const ONE: Self = 1;
}

impl Counter for u32 {
    const ZERO: Self = 0;
    const ONE: Self = 1;
}

impl Counter for u64 {
    const ZERO: Self = 0;
    const ONE: Self = 1;
}

/// A divider outputs a clock every n input clocks, where n is the divider's
/// period. It contains a counter which is decremented on the arrival of each
/// clock. When it reaches 0, it is reloaded with the period and an output clock
/// is generated. Resetting a divider reloads its counter without generating an
/// output clock. Changing a divider's period doesn't affect its current count.
#[derive(Debug, Default)]
struct Timer<C> {
    period: C,
    counter: C,
    /// True if last `tick()` method returns true.
    signal: bool,
}

impl<C: Counter> Timer<C> {
    /// Tick the timer. Returns true if an output clock is generated on this tick.
    fn tick(&mut self) -> bool {
        self.signal = if self.counter == C::ZERO {
            self.reset();
            true
        } else {
            self.counter -= C::ONE;
            false
        };
        self.signal
    }

    /// Reset the timer (reload counter with period, without generating an output clock).
    fn reset(&mut self) {
        self.counter = self.period;
    }

    /// Set the timer period. Doesn't affect the current counter value.
    fn set_period(&mut self, period: C) {
        self.period = period;
    }

    /// True if last `tick()` method returns true.
    fn signal(&self) -> bool {
        self.signal
    }
}

impl Timer<u16> {
    const fn set_period_high(&mut self, high: u8) {
        self.period = (self.period & 0x00FF) | ((high as u16) << 8);
    }

    const fn set_period_low(&mut self, low: u8) {
        self.period = (self.period & 0xFF00) | (low as u16);
    }
}

#[derive(Debug)]
struct Sequence<I: 'static> {
    items: &'static [I],
    cur_idx: usize,
}

impl<I: Copy> Sequence<I> {
    fn new(items: &'static [I]) -> Self {
        Self { items, cur_idx: 0 }
    }

    fn reset_items(&mut self, items: &'static [I]) {
        self.items = items;
        self.reset();
    }

    fn reset(&mut self) {
        self.cur_idx = 0;
    }

    fn tick(&mut self) -> I {
        let item = self.items[self.cur_idx];
        self.cur_idx = (self.cur_idx + 1) % self.items.len();
        item
    }
}

/// Control Gate, if control is non-zero, the input is passed unchanged to the
/// output, otherwise the output is 0.
#[derive(Debug)]
struct ControlGate<V> {
    control: V,
}

#[bitfield(u8)]
struct Sweep {
    #[bits(3)]
    shift: u8,
    negate: bool,
    #[bits(3)]
    period: u8,
    enabled: bool,
}

#[bitfield(u8)]
struct PulseControlBits {
    #[bits(4)]
    volume: u8,
    constant_volume: bool,
    is_halt: bool,
    #[bits(2)]
    duty: u8,
}

/// Length timer high 3 bit register
#[bitfield(u8)]
struct LengthTimerHigh3Bits {
    #[bits(3)]
    high3: u8,
    #[bits(5)]
    length: u8,
}

#[bitfield(u8)]
struct TriangleControlBits {
    #[bits(7)]
    counter: u8,
    is_halt: bool,
}

#[bitfield(u8)]
struct NoiseControlBits {
    #[bits(4)]
    volume: u8,
    constant_volume: bool,
    loop_flag: bool,
    #[bits(2)]
    __: u8,
}

#[bitfield(u8)]
struct NoisePeriod {
    #[bits(4)]
    period: u8,
    #[bits(3)]
    __: u8,
    is_halt: bool,
}

#[bitfield(u8)]
struct NoiseLength {
    #[bits(3)]
    __: u8,
    #[bits(5)]
    length: u8,
}

#[bitfield(u8)]
struct DmcIRQLoopFreq {
    #[bits(4)]
    freq: u8,
    #[bits(2)]
    __: u8,
    loop_flag: bool,
    irq_enabled: bool,
}

#[bitfield(u8)]
struct ControlFlags {
    pulse1_enabled: bool,
    pulse2_enabled: bool,
    triangle_enabled: bool,
    noise_enabled: bool,
    dmc_enabled: bool,
    #[bits(3)]
    __: u8,
}

#[bitfield(u8)]
struct APUStatus {
    pulse1_enabled: bool,
    pulse2_enabled: bool,
    triangle_enabled: bool,
    noise_enabled: bool,
    dmc_enabled: bool,
    #[bits(1)]
    __: u8,
    frame_interrupt: bool,
    dmc_interrupt: bool,
}

#[repr(u8)]
#[bitenum]
#[derive(Debug, Default, PartialEq, Eq)]
enum FrameSequencerMode {
    #[fallback]
    #[default]
    FourStep, // 0
    FiveStep, // 1
}

#[bitfield(u8)]
struct FrameSequencerBits {
    #[bits(6)]
    __: u8,
    interrupt_flag: bool,
    #[bits(1)]
    mode: FrameSequencerMode,
}

#[bitfield(u8)]
struct DmcLoadCounter {
    #[bits(7)]
    load_counter: u8,
    #[bits(1)]
    __: u8,
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

const CPU_CLOCK_HZ: u64 = 1_789_773;

/// APU status information for debugging/inspection
#[derive(Debug, Clone, Copy)]
pub struct ApuStatusInfo {
    pub pulse1_enabled: bool,
    pub pulse2_enabled: bool,
    pub triangle_enabled: bool,
    pub noise_enabled: bool,
    pub dmc_enabled: bool,
    pub frame_irq_pending: bool,
    pub dmc_irq_pending: bool,
}

#[derive(Default, Debug)]
struct EnvelopeState {
    start: bool,
    divider: u8,
    decay: u8,
}

impl EnvelopeState {
    fn restart(&mut self) {
        self.start = true;
    }

    fn tick(&mut self, period: u8, looping: bool) {
        if self.start {
            self.start = false;
            self.decay = 15;
            self.divider = period;
            return;
        }

        if self.divider == 0 {
            self.divider = period;
            if self.decay == 0 {
                if looping {
                    self.decay = 15;
                }
            } else {
                self.decay -= 1;
            }
        } else {
            self.divider -= 1;
        }
    }
}

pub struct Apu<D: AudioDriver = ()> {
    pulse1: Pulse,
    pulse2: Pulse,
    triangle: Triangle,
    noise: Noise,
    dmc: Dmc,
    driver: D,
    sample_rate: u32,
    sample_accumulator: u64,
    frame_sequencer: FrameSequencer,
    apu_even_cycle: bool,
    dmc_interrupt: bool,
    /// Pending DMC DMA request: (address, is_reload).
    /// is_reload = true for reload DMAs (output unit emptied buffer, 4 cycles),
    /// false for load DMAs ($4015 write with empty buffer, 3 cycles).
    dmc_dma_request: Option<(u16, bool)>,
    dc_last_input: f32,
    dc_last_output: f32,
}

impl Default for Apu<()> {
    fn default() -> Self {
        Self::new(())
    }
}

impl<D: AudioDriver> Apu<D> {
    pub fn new(driver: D) -> Self {
        let sample_rate = driver.sample_rate().max(1);
        Self {
            pulse1: Pulse::new(true),
            pulse2: Pulse::new(false),
            triangle: Triangle::default(),
            noise: Noise::default(),
            dmc: Dmc::default(),
            driver,
            sample_rate,
            sample_accumulator: 0,
            frame_sequencer: FrameSequencer::default(),
            apu_even_cycle: false,
            dmc_interrupt: false,
            dmc_dma_request: None,
            dc_last_input: 0.0,
            dc_last_output: 0.0,
        }
    }

    pub fn reset(&mut self) {
        // https://www.nesdev.org/wiki/CPU_power_up_state
        //
        // At reset, the APU behaves as if:
        // 1. $4015 is written with $00 (silences all channels)
        // 2. $4017 is written again with current mode but IRQ inhibit set
        // 3. Frame IRQ flag is cleared
        // 4. DMC interrupt flag is cleared

        // Clear all interrupt flags
        self.frame_sequencer.clear_interrupt();
        self.dmc_interrupt = false;

        // $4015 write with $00 silences all channels and clears DMC interrupt
        self.set_control_flags(ControlFlags::new());

        // Reset triangle channel state, but preserve the length counter halt flag
        // (control.reload_flag) which should persist across reset
        let length_counter_halt = self.triangle.is_halt();
        self.triangle = Triangle::default();
        self.triangle.restore_is_halt_flag(length_counter_halt);

        // Re-apply the last value written to $4017 after the reset delay.
        // The test ROMs rely on reset preserving the previous frame counter mode
        // and IRQ inhibit bit rather than synthesizing a new value here.
        self.frame_sequencer.reset();
    }

    pub fn tick(&mut self) {
        self.frame_sequencer.tick_timer();
        if get_system_cycles().is_multiple_of(SYSTEM_CYCLES_PER_CPU_CYCLE) {
            self.frame_sequencer.tick();
            self.triangle.step_timer();

            self.apu_even_cycle = !self.apu_even_cycle;
            if self.apu_even_cycle {
                self.pulse1.step_timer();
                self.pulse2.step_timer();
                self.noise.step_timer();
            }

            // Tick DMC timer every CPU cycle
            if self.dmc.tick() {
                // DMC requests a reload DMA read (output unit emptied buffer)
                self.dmc_dma_request = Some((self.dmc.current_address(), true));
            }

            if std::mem::take(&mut self.frame_sequencer.output_latch.irq) {
                self.frame_sequencer.set_interrupt();
            }
            if std::mem::take(&mut self.frame_sequencer.output_latch.envelop_and_linear) {
                self.tick_envelop_and_linear();
            }
            if std::mem::take(&mut self.frame_sequencer.output_latch.length_and_sweep) {
                self.tick_length_and_sweep();
            }
            self.emit_samples();
        }
    }

    pub fn request_irq(&self) -> bool {
        self.frame_sequencer.request_irq() || self.dmc_interrupt
    }

    /// Take the pending DMC DMA request, if any.
    /// Returns (address, is_reload).
    pub fn take_dmc_dma_request(&mut self) -> Option<(u16, bool)> {
        self.dmc_dma_request.take()
    }

    /// Supply a byte fetched by DMC DMA. May set the DMC interrupt flag.
    pub fn supply_dmc_byte(&mut self, byte: u8) {
        if self.dmc.supply_dma_byte(byte) {
            self.dmc_interrupt = true;
        }
    }

    /// Re-check if DMC DMA is needed after a CPU write to $4015.
    /// On real hardware the write and DMA check happen simultaneously,
    /// but in our sequential model the APU tick (with DMA check) runs
    /// before the CPU write. This extra check catches the case where
    /// `sta $4015` enables DMC with an empty buffer, which should
    /// trigger an immediate DMA on the same cycle.
    pub fn recheck_dmc_dma(&mut self) {
        if self.dmc_dma_request.is_none() && self.dmc.check_dma_needed() {
            // Load DMA (triggered by $4015 write, not output unit): is_reload = false
            self.dmc_dma_request = Some((self.dmc.current_address(), false));
        }
    }

    pub fn flush(&mut self) {
        self.driver.flush();
    }

    /// Get current APU status for debugging/inspection
    pub fn get_status(&self) -> ApuStatusInfo {
        ApuStatusInfo {
            pulse1_enabled: self.pulse1.status_bit(),
            pulse2_enabled: self.pulse2.status_bit(),
            triangle_enabled: self.triangle.is_enabled(),
            noise_enabled: self.noise.is_enabled(),
            dmc_enabled: self.dmc.status_bit(),
            frame_irq_pending: self.frame_sequencer.frame_interrupt,
            dmc_irq_pending: self.dmc_interrupt,
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match address {
            0x4015 => {
                let status = self.status_byte();
                self.frame_sequencer.clear_interrupt();
                status
            }
            _ => 0,
        }
    }

    pub fn peek(&self, address: u16) -> u8 {
        match address {
            0x4015 => self.status_byte(),
            _ => 0,
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            0x4000 => self.pulse1.write_control(value.into()),
            0x4001 => self.pulse1.write_sweep(value.into()),
            0x4002 => self.pulse1.write_timer_low(value),
            0x4003 => self.pulse1.write_timer_high(value.into()),
            0x4004 => self.pulse2.write_control(value.into()),
            0x4005 => self.pulse2.write_sweep(value.into()),
            0x4006 => self.pulse2.write_timer_low(value),
            0x4007 => self.pulse2.write_timer_high(value.into()),
            0x4008 => self.triangle.write_control(value.into()),
            0x400A => self.triangle.write_timer_low(value),
            0x400B => self.triangle.write_timer_high(value.into()),
            0x400C => self.noise.write_envelope(value.into()),
            0x400E => self.noise.write_period(value.into()),
            0x400F => self.noise.write_length(value.into()),
            0x4010 => self.write_dmc_irq_loop_freq(value.into()),
            0x4011 => self.dmc.write_load_counter(value.into()),
            0x4012 => self.dmc.write_sample_address(value),
            0x4013 => self.dmc.write_sample_length(value),
            0x4015 => self.set_control_flags(value.into()),
            0x4017 => self
                .frame_sequencer
                .write_mode(value.into(), self.apu_even_cycle),
            _ => {}
        }
    }

    fn status_byte(&self) -> u8 {
        let mut status = APUStatus::new();
        status.set_pulse1_enabled(self.pulse1.status_bit());
        status.set_pulse2_enabled(self.pulse2.status_bit());
        status.set_triangle_enabled(self.triangle.status_bit());
        status.set_noise_enabled(self.noise.status_bit());
        status.set_dmc_enabled(self.dmc.status_bit());
        status.set_frame_interrupt(self.frame_sequencer.frame_interrupt());
        status.set_dmc_interrupt(self.dmc_interrupt);
        status.into_bits()
    }

    fn set_control_flags(&mut self, flags: ControlFlags) {
        self.pulse1.set_enabled(flags.pulse1_enabled());
        self.pulse2.set_enabled(flags.pulse2_enabled());
        self.triangle.set_enabled(flags.triangle_enabled());
        self.noise.set_enabled(flags.noise_enabled());
        self.dmc.set_enabled(flags.dmc_enabled());

        // Always clear DMC interrupt on $4015 write
        self.dmc_interrupt = false;
    }

    fn tick_envelop_and_linear(&mut self) {
        self.pulse1.step_envelope();
        self.pulse2.step_envelope();
        self.triangle.step_linear_counter();
        self.noise.step_envelope();
    }

    fn tick_length_and_sweep(&mut self) {
        self.pulse1.step_length_counter();
        self.pulse2.step_length_counter();
        self.triangle.step_length_counter();
        self.noise.step_length_counter();
        self.pulse1.step_sweep();
        self.pulse2.step_sweep();
    }

    fn emit_samples(&mut self) {
        self.sample_accumulator = self
            .sample_accumulator
            .wrapping_add(self.sample_rate as u64);
        while self.sample_accumulator >= CPU_CLOCK_HZ {
            self.sample_accumulator -= CPU_CLOCK_HZ;
            let sample = self.mix_sample();
            self.driver.push_sample(sample);
        }
    }

    fn mix_sample(&mut self) -> f32 {
        let pulse_1 = self.pulse1.output() as f32;
        let pulse_2 = self.pulse2.output() as f32;
        let triangle = self.triangle.output() as f32;
        let noise = self.noise.output() as f32;
        let dmc = self.dmc.output() as f32;

        let pulse_mix = if pulse_1 + pulse_2 == 0.0 {
            0.0
        } else {
            95.88 / ((8128.0 / (pulse_1 + pulse_2)) + 100.0)
        };

        let tnd_input = triangle / 8227.0 + noise / 12241.0 + dmc / 22638.0;
        let tnd_mix = if tnd_input == 0.0 {
            0.0
        } else {
            159.79 / ((1.0 / tnd_input) + 100.0)
        };

        let input = pulse_mix + tnd_mix;
        let output = input - self.dc_last_input + (0.995 * self.dc_last_output);
        self.dc_last_input = input;
        self.dc_last_output = output;
        output.clamp(-1.0, 1.0)
    }

    fn write_dmc_irq_loop_freq(&mut self, value: DmcIRQLoopFreq) {
        self.dmc.write_irq_loop_freq(value);
        // If IRQ flag cleared, clear the DMC interrupt
        if !self.dmc.irq_enabled() {
            self.dmc_interrupt = false;
        }
    }
}

#[cfg(test)]
mod tests;
