mod dmc;
mod frame_sequencer;
/// Shared with the MMC5 dedicated audio unit (`mapper::mmc5_audio`): its
/// pulse channels reuse the APU envelope/length/timer blocks so the two
/// can never drift apart.
pub(crate) mod helper;
mod mixer;
mod noise;
mod pulse;
pub(crate) mod registers;
mod triangle;

pub(crate) use dmc::{DmcDmaRequestState, DmcState};
pub(crate) use frame_sequencer::{
    FrameCounterModeTag, FrameCounterState, LatchedFrameStepState, PendingFrameModeState,
};
pub(crate) use mixer::MixerState;
pub(crate) use noise::NoiseState;
pub(crate) use pulse::PulseState;
pub(crate) use triangle::TriangleState;

use crate::{SystemClock, nes::dmc_dma::DmcDmaType};
use dmc::Dmc;
use frame_sequencer::FrameSequencer;
use helper::{Divider, Envelope, LengthControl};
use mixer::Mixer;
use noise::Noise;
use pulse::Pulse;
use triangle::Triangle;

/// Expansion-audio channel levels mixed with the APU output on each tick.
///
/// Zeros when the cartridge provides no expansion audio (every non-MMC5
/// board), so the mix stays bit-identical to the APU alone. Produced by
/// [`crate::nes::mapper::Cartridge::tick_audio`] once per CPU cycle.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ExpansionMix {
    /// First MMC5 pulse channel output (0-15).
    pub pulse1: u8,
    /// Second MMC5 pulse channel output (0-15).
    pub pulse2: u8,
    /// MMC5 PCM DAC latch (0-255, 8-bit unlike the 7-bit APU DMC DAC).
    pub pcm: u8,
}
use registers::{
    APUStatus, ControlFlags, DmcDacBits, DmcIRQLoopFreq, FrameSequencerBits, FrameSequencerMode,
    LengthTimerHigh3Bits, NoiseControlBits, NoiseLength, NoisePeriod, PulseControlBits, SweepBits,
    TriangleControlBits,
};

pub struct Apu {
    pulse1: Pulse,
    pulse2: Pulse,
    triangle: Triangle,
    noise: Noise,
    dmc: Dmc,
    mixer: Mixer,
    /// Mixed output samples for the current frame (`f32` mono, 44.1 kHz).
    /// Filled by `tick`, cleared at each `process_frame` start, served
    /// zero-copy via `audio_samples` / `audio_bytes`.
    ///
    /// Retired (intentional, ticket #144): the pre-owned-buffer `AudioDriver`
    /// per-sample `push_sample`/`flush` callbacks into host drivers. Sinks
    /// now borrow each frame's buffer instead; see the drain contract on
    /// [`crate::NesMachine::process_frame`].
    audio_buffer: Vec<f32>,
    /// Latest expansion-audio levels, latched by `set_expansion_mix` ahead
    /// of `tick` so the mixer sees them without changing `tick`'s shape.
    expansion: ExpansionMix,
    frame_sequencer: FrameSequencer,
    /// Per-channel latch recording length clocks that decremented a nonzero
    /// counter (pulse1, pulse2, triangle, noise) on this very APU tick. A
    /// reload-register write landing on the same tick loses the
    /// read-modify-write race on hardware and must not reload — blargg's
    /// 11.len_reload_timing test 5. Cleared at the start of every APU tick.
    length_race_clobber: [bool; 4],
}

impl Default for Apu {
    fn default() -> Self {
        Self::new()
    }
}

/// Full [`Apu`] execution state for save-states (ticket #184) in core
/// types. The audio buffer is excluded (rewritten each frame); the mixer
/// sample rate is a fixed construction constant, never saved.
#[derive(Debug, Clone)]
pub(crate) struct ApuState {
    pub pulse1: PulseState,
    pub pulse2: PulseState,
    pub triangle: TriangleState,
    pub noise: NoiseState,
    pub dmc: DmcState,
    pub frame_counter: FrameCounterState,
    pub mixer: MixerState,
    pub expansion: ExpansionMix,
    pub length_race_clobber: [bool; 4],
}

impl Apu {
    /// Capture the full APU state.
    pub(crate) fn capture_apu_state(&self) -> ApuState {
        ApuState {
            pulse1: self.pulse1.capture_pulse_state(),
            pulse2: self.pulse2.capture_pulse_state(),
            triangle: self.triangle.capture_triangle_state(),
            noise: self.noise.capture_noise_state(),
            dmc: self.dmc.capture_dmc_state(),
            frame_counter: self.frame_sequencer.capture_frame_counter_state(),
            mixer: self.mixer.capture_mixer_state(),
            expansion: self.expansion,
            length_race_clobber: self.length_race_clobber,
        }
    }

    /// Restore a state previously produced by [`Self::capture_apu_state`].
    /// The audio buffer is left untouched (excluded from the save format).
    pub(crate) fn restore_apu_state(&mut self, s: &ApuState) {
        self.pulse1.restore_pulse_state(&s.pulse1);
        self.pulse2.restore_pulse_state(&s.pulse2);
        self.triangle.restore_triangle_state(&s.triangle);
        self.noise.restore_noise_state(&s.noise);
        self.dmc.restore_dmc_state(&s.dmc);
        self.frame_sequencer
            .restore_frame_counter_state(&s.frame_counter);
        self.mixer.restore_mixer_state(&s.mixer);
        self.expansion = s.expansion;
        self.length_race_clobber = s.length_race_clobber;
    }
}

impl Apu {
    /// Fixed output sample rate (Hz). The core owns the audio buffer;
    /// hosts resample if needed. A plain const: there is no setter and
    /// the value never changes.
    pub const SAMPLE_RATE: u32 = 44_100;

    pub fn new() -> Self {
        Self {
            pulse1: Pulse::new(false),
            pulse2: Pulse::new(true),
            triangle: Triangle::default(),
            noise: Noise::default(),
            dmc: Dmc::default(),
            mixer: Mixer::new(Self::SAMPLE_RATE),
            audio_buffer: Vec::new(),
            expansion: ExpansionMix::default(),
            frame_sequencer: FrameSequencer::default(),
            length_race_clobber: [false; 4],
        }
    }

    /// Soft reset (front-panel RESET button): channels fall silent while
    /// the frame-counter mode survives — the behavior blargg's test ROMs
    /// pin. RAM-less channel state (period, envelope) is preserved with it.
    pub fn reset(&mut self) {
        // https://www.nesdev.org/wiki/CPU_power_up_state
        //
        // At reset, the APU behaves as if:
        // 1. $4015 is written with $00 (silences all channels)
        // 2. $4017 is written again with current mode but IRQ inhibit set
        // 3. Frame IRQ flag is cleared
        // 4. DMC interrupt flag is cleared

        // Clear the frame-sequencer interrupt; the $4015 write below
        // silences all channels and clears the DMC interrupt.
        self.frame_sequencer.clear_interrupt();
        self.set_control_flags(ControlFlags::new());

        // Reset triangle channel state, but preserve the length counter halt
        // flag which should persist across reset.
        self.triangle.reset();

        // Re-apply the last value written to $4017 after the reset delay.
        // The test ROMs rely on reset preserving the previous frame counter mode
        // and IRQ inhibit bit rather than synthesizing a new value here.
        self.frame_sequencer.reset();
    }

    /// Hard reset (power cycle): every channel returns to its power-up
    /// state — the same contents [`Self::new`] builds — instead of the
    /// soft-reset silence-and-preserve. The buffered samples are dropped.
    pub fn reset_hard(&mut self) {
        // https://www.nesdev.org/wiki/APU_power_up_state
        self.pulse1 = Pulse::new(false);
        self.pulse2 = Pulse::new(true);
        self.triangle = Triangle::default();
        self.noise = Noise::default();
        self.dmc = Dmc::default();
        self.mixer = Mixer::new(Self::SAMPLE_RATE);
        self.audio_buffer.clear();
        self.expansion = ExpansionMix::default();
        self.frame_sequencer = FrameSequencer::default();
        self.length_race_clobber = [false; 4];
    }

    pub fn tick(&mut self) {
        self.length_race_clobber = [false; 4];
        self.frame_sequencer.tick_timer();
        self.frame_sequencer.tick();
        self.triangle.tick_timer();
        self.pulse1.tick_timer();
        self.pulse2.tick_timer();
        self.noise.tick_timer();
        self.dmc.tick_timer();

        if let Some(frame_sequencer) = self.frame_sequencer.output_latch {
            self.frame_sequencer.output_latch = None;
            if frame_sequencer.irq {
                self.frame_sequencer.set_interrupt();
            }
            if frame_sequencer.envelop_and_linear {
                self.tick_envelop_and_linear();
            }
            if frame_sequencer.length_and_sweep {
                self.tick_length_and_sweep();
            }
        }
        self.emit_samples();
    }

    pub fn request_irq(&self) -> bool {
        self.frame_sequencer.request_irq() || self.dmc.interrupt_flag()
    }

    /// Total $4011 DAC-register writes (the Zapper ROMs' click counter).
    pub fn dmc_dac_writes(&self) -> u64 {
        self.dmc.dac_writes()
    }

    /// Take the pending DMC DMA request, if any.
    pub fn take_dmc_dma_request(&mut self) -> Option<(DmcDmaType, u16)> {
        self.dmc.take_dma_request()
    }

    /// Supply a byte fetched by DMC DMA. May set the DMC interrupt flag.
    pub fn supply_dmc_byte(&mut self, byte: u8) {
        self.dmc.supply_dma_byte(byte);
    }

    /// Mixed samples for the current frame (`f32` mono, 44.1 kHz).
    pub fn audio_samples(&self) -> &[f32] {
        &self.audio_buffer
    }

    /// Byte view of [`Self::audio_samples`] (little-endian `f32` bytes).
    pub fn audio_bytes(&self) -> &[u8] {
        bytemuck::cast_slice(&self.audio_buffer)
    }

    /// Drop buffered samples at the start of the next frame.
    pub fn clear_audio_buffer(&mut self) {
        self.audio_buffer.clear();
    }

    pub fn read(&mut self, address: u16) -> u8 {
        let value = self.peek(address);
        if address == 0x4015 {
            self.frame_sequencer.clear_interrupt();
        }
        value
    }

    pub fn peek(&self, address: u16) -> u8 {
        match address {
            0x4015 => self.status_byte(),
            _ => 0,
        }
    }

    /// Register write at machine time `clock`: the $4017 frame-counter
    /// reset delay keys off the CPU-cycle parity of the write.
    pub fn write(&mut self, address: u16, value: u8, clock: SystemClock) {
        match address {
            0x4000 => self
                .pulse1
                .write_control(PulseControlBits::from_bytes([value])),
            0x4001 => self.pulse1.write_sweep(SweepBits::from_bytes([value])),
            0x4002 => self.pulse1.write_timer_low(value),
            0x4003 => {
                let suppress = self.length_race_clobber[0];
                self.pulse1
                    .write_timer_high(LengthTimerHigh3Bits::from_bytes([value]), suppress);
            }
            0x4004 => self
                .pulse2
                .write_control(PulseControlBits::from_bytes([value])),
            0x4005 => self.pulse2.write_sweep(SweepBits::from_bytes([value])),
            0x4006 => self.pulse2.write_timer_low(value),
            0x4007 => {
                let suppress = self.length_race_clobber[1];
                self.pulse2
                    .write_timer_high(LengthTimerHigh3Bits::from_bytes([value]), suppress);
            }
            0x4008 => self
                .triangle
                .write_control(TriangleControlBits::from_bytes([value])),
            0x400A => self.triangle.write_timer_low(value),
            0x400B => {
                let suppress = self.length_race_clobber[2];
                self.triangle
                    .write_timer_high(LengthTimerHigh3Bits::from_bytes([value]), suppress);
            }
            0x400C => self
                .noise
                .write_envelope(NoiseControlBits::from_bytes([value])),
            0x400E => self.noise.write_period(NoisePeriod::from_bytes([value])),
            0x400F => {
                let suppress = self.length_race_clobber[3];
                self.noise
                    .write_length(NoiseLength::from_bytes([value]), suppress);
            }
            0x4010 => self
                .dmc
                .write_dmc_irq_loop_freq(DmcIRQLoopFreq::from_bytes([value])),
            0x4011 => self.dmc.write_dac(DmcDacBits::from_bytes([value])),
            0x4012 => self.dmc.write_sample_address(value),
            0x4013 => self.dmc.write_sample_length(value),
            0x4015 => self.set_control_flags(ControlFlags::from_bytes([value])),
            0x4017 => {
                let cpu_odd = (clock.cycles() / 3) % 2 == 1;
                self.frame_sequencer
                    .write_control_bits(FrameSequencerBits::from_bytes([value]), cpu_odd)
            }
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
        status.set_frame_interrupt(self.frame_sequencer.interrupt_flag());
        status.set_dmc_interrupt(self.dmc.interrupt_flag());
        status.into_bytes()[0]
    }

    fn set_control_flags(&mut self, flags: ControlFlags) {
        self.pulse1.set_enabled(flags.pulse1_enabled());
        self.pulse2.set_enabled(flags.pulse2_enabled());
        self.triangle.set_enabled(flags.triangle_enabled());
        self.noise.set_enabled(flags.noise_enabled());
        self.dmc.set_enabled(flags.dmc_enabled());
    }

    fn tick_envelop_and_linear(&mut self) {
        self.pulse1.tick_envelope();
        self.pulse2.tick_envelope();
        self.triangle.tick_linear();
        self.noise.tick_envelope();
    }

    fn tick_length_and_sweep(&mut self) {
        self.length_race_clobber = [
            self.pulse1.tick_length_and_sweep(),
            self.pulse2.tick_length_and_sweep(),
            self.triangle.tick_length(),
            self.noise.tick_length(),
        ];
    }

    /// Latch expansion-audio levels for the next `tick`. Called once per
    /// CPU cycle by the MCU ahead of `tick`; defaults to silence.
    pub fn set_expansion_mix(&mut self, mix: ExpansionMix) {
        self.expansion = mix;
    }

    #[inline]
    fn emit_samples(&mut self) {
        self.mixer.emit_samples(
            || {
                (
                    self.pulse1.output(),
                    self.pulse2.output(),
                    self.triangle.output(),
                    self.noise.output(),
                    self.dmc.output(),
                    self.expansion,
                )
            },
            |sample| self.audio_buffer.push(sample),
        );
    }
}

#[cfg(test)]
mod tests;
