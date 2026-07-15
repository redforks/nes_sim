mod dmc;
mod frame_sequencer;
mod helper;
mod mixer;
mod noise;
mod pulse;
mod registers;
mod triangle;

use crate::{SystemClock, nes::dmc_dma::DmcDmaType};
use dmc::Dmc;
use frame_sequencer::FrameSequencer;
use helper::{ControlGate, Divider, Envelope, LengthControl, Sequencer};
use mixer::Mixer;
use noise::Noise;
use pulse::Pulse;
use triangle::Triangle;

pub use helper::AudioDriver;
use registers::{
    APUStatus, ControlFlags, DmcDacBits, DmcIRQLoopFreq, FrameSequencerBits, FrameSequencerMode,
    LengthTimerHigh3Bits, NoiseControlBits, NoiseLength, NoisePeriod, PulseControlBits, SweepBits,
    TriangleControlBits,
};

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

pub struct Apu<D: AudioDriver = ()> {
    pulse1: Pulse,
    pulse2: Pulse,
    triangle: Triangle,
    noise: Noise,
    dmc: Dmc,
    driver: D,
    mixer: Mixer,
    frame_sequencer: FrameSequencer,
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
            pulse1: Pulse::new(false),
            pulse2: Pulse::new(true),
            triangle: Triangle::default(),
            noise: Noise::default(),
            dmc: Dmc::default(),
            driver,
            mixer: Mixer::new(sample_rate),
            frame_sequencer: FrameSequencer::default(),
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
        self.dmc.clear_interrupt_flag();

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

    pub fn tick(&mut self, clock: SystemClock) {
        self.frame_sequencer.tick_timer();
        if clock.is_apu_clock() {
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
    }

    pub fn request_irq(&self) -> bool {
        self.frame_sequencer.request_irq() || self.dmc.interrupt_flag()
    }

    /// Take the pending DMC DMA request, if any.
    pub fn take_dmc_dma_request(&mut self) -> Option<(DmcDmaType, u16)> {
        self.dmc.take_dma_request()
    }

    /// Supply a byte fetched by DMC DMA. May set the DMC interrupt flag.
    pub fn supply_dmc_byte(&mut self, byte: u8) {
        self.dmc.supply_dma_byte(byte);
    }

    pub fn flush(&mut self) {
        self.driver.flush();
    }

    /// Get current APU status for debugging/inspection
    pub fn get_status(&self) -> ApuStatusInfo {
        ApuStatusInfo {
            pulse1_enabled: self.pulse1.status_bit(),
            pulse2_enabled: self.pulse2.status_bit(),
            triangle_enabled: self.triangle.status_bit(),
            noise_enabled: self.noise.status_bit(),
            dmc_enabled: self.dmc.status_bit(),
            frame_irq_pending: self.frame_sequencer.request_irq(),
            dmc_irq_pending: self.dmc.interrupt_flag(),
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
            0x4010 => self.dmc.write_dmc_irq_loop_freq(value.into()),
            0x4011 => self.dmc.write_dac(value.into()),
            0x4012 => self.dmc.write_sample_address(value),
            0x4013 => self.dmc.write_sample_length(value),
            0x4015 => self.set_control_flags(value.into()),
            0x4017 => self.frame_sequencer.write_control_bits(value.into()),
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
        status.set_frame_interrupt(self.frame_sequencer.request_irq());
        status.set_dmc_interrupt(self.dmc.interrupt_flag());
        status.into_bits()
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
        self.pulse1.tick_length_and_sweep();
        self.pulse2.tick_length_and_sweep();
        self.triangle.tick_length();
        self.noise.tick_length();
    }

    fn emit_samples(&mut self) {
        self.mixer.emit_samples(
            self.pulse1.output(),
            self.pulse2.output(),
            self.triangle.output(),
            self.noise.output(),
            self.dmc.output(),
            |sample| self.driver.push_sample(sample),
        );
    }
}

#[cfg(test)]
mod tests;
