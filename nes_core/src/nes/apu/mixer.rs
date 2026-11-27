const CPU_CLOCK_HZ: u32 = 1_789_773;

use super::ExpansionMix;

/// Sampled nonlinear-mix curves: the per-entry scalar expressions below are
/// exactly the `mix()` formulas, evaluated once at build time, so each table
/// load is bit-identical to the division it replaces (pinned by
/// `mix_tables_match_formula`). This trades five `f32` divisions per emitted
/// sample (~735 per frame) for table loads plus the two divisions the closed
/// form still needs.
///
/// Sizes cover every legal channel level: pulse sums four 0-15 levels
/// (max 60), triangle/noise are 4-bit, DMC is 7-bit, MMC5 PCM is 8-bit.
const PULSE_MIX_TABLE: [f32; 61] = {
    let mut table = [0.0f32; 61];
    let mut i = 1usize;
    while i < 61 {
        table[i] = 95.88 / ((8128.0 / (i as f32)) + 100.0);
        i += 1;
    }
    table
};

const TRIANGLE_PART_TABLE: [f32; 16] = {
    let mut table = [0.0f32; 16];
    let mut i = 1usize;
    while i < 16 {
        table[i] = (i as f32) / 8227.0;
        i += 1;
    }
    table
};

const NOISE_PART_TABLE: [f32; 16] = {
    let mut table = [0.0f32; 16];
    let mut i = 1usize;
    while i < 16 {
        table[i] = (i as f32) / 12241.0;
        i += 1;
    }
    table
};

const DMC_PART_TABLE: [f32; 128] = {
    let mut table = [0.0f32; 128];
    let mut i = 1usize;
    while i < 128 {
        table[i] = (i as f32) / 22638.0;
        i += 1;
    }
    table
};

const PCM_PART_TABLE: [f32; 256] = {
    let mut table = [0.0f32; 256];
    let mut i = 1usize;
    while i < 256 {
        table[i] = (i as f32) / 22638.0;
        i += 1;
    }
    table
};

pub struct Mixer {
    sample_accumulator: u32,
    sample_rate: u32,
    dc_last_input: f32,
    dc_last_output: f32,
}

/// Full [`Mixer`] state for save-states (ticket #184): the resample
/// accumulator plus the DC-blocking filter memory as raw `f32` bits.
/// `sample_rate` is a fixed construction constant, never saved.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MixerState {
    pub sample_accumulator: u32,
    pub dc_last_input_bits: u32,
    pub dc_last_output_bits: u32,
}

impl Mixer {
    /// Capture the mixer state.
    pub(crate) fn capture_mixer_state(&self) -> MixerState {
        MixerState {
            sample_accumulator: self.sample_accumulator,
            dc_last_input_bits: self.dc_last_input.to_bits(),
            dc_last_output_bits: self.dc_last_output.to_bits(),
        }
    }

    /// Restore a state previously produced by [`Self::capture_mixer_state`].
    pub(crate) fn restore_mixer_state(&mut self, s: &MixerState) {
        self.sample_accumulator = s.sample_accumulator;
        self.dc_last_input = f32::from_bits(s.dc_last_input_bits);
        self.dc_last_output = f32::from_bits(s.dc_last_output_bits);
    }
}

impl Mixer {
    pub fn new(sample_rate: u32) -> Self {
        Self {
            sample_accumulator: 0,
            sample_rate,
            dc_last_input: 0.0,
            dc_last_output: 0.0,
        }
    }

    /// Mix one APU tick, adding `expansion` (MMC5 pulse1/pulse2/PCM) levels.
    /// All-zero expansion — every non-MMC5 board — is bit-identical to the
    /// APU-alone mix (`x + 0.0 == x`, and the zero-sum branch is unchanged).
    #[inline]
    pub fn emit_samples(
        &mut self,
        get_channel_samples: impl Fn() -> (u8, u8, u8, u8, u8, ExpansionMix),
        mut receive_output: impl FnMut(f32),
    ) {
        self.sample_accumulator = self.sample_accumulator.wrapping_add(self.sample_rate);
        while self.sample_accumulator >= CPU_CLOCK_HZ {
            self.sample_accumulator -= CPU_CLOCK_HZ;
            let (p1, p2, t, n, d, expansion) = get_channel_samples();
            let sample = self.mix_sample(
                p1 as u16 + p2 as u16 + expansion.pulse1 as u16 + expansion.pulse2 as u16,
                t,
                n,
                d,
                expansion.pcm,
            );
            receive_output(sample);
        }
    }

    fn mix_sample(
        &mut self,
        pulse: u16,
        triangle: u8,
        noise: u8,
        dmc: u8,
        expansion_pcm: u8,
    ) -> f32 {
        let input = mix(pulse, triangle, noise, dmc, expansion_pcm);
        let output = input - self.dc_last_input + (0.995 * self.dc_last_output);
        self.dc_last_input = input;
        self.dc_last_output = output;
        output.clamp(-1.0, 1.0)
    }
}

fn mix(pulse: u16, triangle: u8, noise: u8, dmc: u8, expansion_pcm: u8) -> f32 {
    // MMC5 pulse channels join the APU pulses under the same nonlinear
    // curve (one shared analog node on hardware); the 8-bit MMC5 PCM DAC
    // joins the TND side with the DMC weight, so full scale is twice as
    // loud as the 7-bit APU DMC (NESdev MMC5 audio page).
    //
    // The curves are table-driven (see above): the loads replay the exact
    // scalar expressions in the same left-associative order, so the mix
    // stays bit-identical while the per-sample divisions drop from five
    // to two.
    debug_assert!((pulse as usize) < PULSE_MIX_TABLE.len());
    // SAFETY: `pulse` sums four 0-15 channel levels (max 60); the table has
    // 61 entries, and the `u8` part-table indices always land in range.
    let (pulse_mix, tnd_input) = unsafe {
        (
            *PULSE_MIX_TABLE.get_unchecked(pulse as usize),
            *TRIANGLE_PART_TABLE.get_unchecked(triangle as usize)
                + *NOISE_PART_TABLE.get_unchecked(noise as usize)
                + *DMC_PART_TABLE.get_unchecked(dmc as usize)
                + *PCM_PART_TABLE.get_unchecked(expansion_pcm as usize),
        )
    };

    let tnd_mix = if tnd_input == 0.0 {
        0.0
    } else {
        159.79 / ((1.0 / tnd_input) + 100.0)
    };

    pulse_mix + tnd_mix
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The lookup tables must be bit-identical to the scalar expressions
    /// they replace: entry-wise equality plus the same left-associative
    /// summation order in `mix()` makes the table path exact by
    /// construction, and the grid below checks the assembled mix.
    #[test]
    fn mix_tables_match_formula() {
        for (i, entry) in PULSE_MIX_TABLE.iter().enumerate() {
            let expected = if i == 0 {
                0.0f32
            } else {
                95.88 / ((8128.0 / (i as f32)) + 100.0)
            };
            assert_eq!(entry.to_bits(), expected.to_bits(), "pulse entry {i}");
        }
        for (i, entry) in TRIANGLE_PART_TABLE.iter().enumerate() {
            assert_eq!(
                entry.to_bits(),
                ((i as f32) / 8227.0).to_bits(),
                "triangle entry {i}"
            );
        }
        for (i, entry) in NOISE_PART_TABLE.iter().enumerate() {
            assert_eq!(
                entry.to_bits(),
                ((i as f32) / 12241.0).to_bits(),
                "noise entry {i}"
            );
        }
        for (i, entry) in DMC_PART_TABLE.iter().enumerate() {
            assert_eq!(
                entry.to_bits(),
                ((i as f32) / 22638.0).to_bits(),
                "dmc entry {i}"
            );
        }
        for (i, entry) in PCM_PART_TABLE.iter().enumerate() {
            assert_eq!(
                entry.to_bits(),
                ((i as f32) / 22638.0).to_bits(),
                "pcm entry {i}"
            );
        }
        for pulse in [0u16, 1, 7, 15, 30, 45, 60] {
            for triangle in [0u8, 1, 5, 15] {
                for noise in [0u8, 3, 15] {
                    for dmc in [0u8, 1, 64, 127] {
                        for pcm in [0u8, 1, 128, 255] {
                            let pulse_sum = pulse as f32;
                            let pulse_mix = if pulse_sum == 0.0 {
                                0.0
                            } else {
                                95.88 / ((8128.0 / pulse_sum) + 100.0)
                            };
                            let tnd_input = triangle as f32 / 8227.0
                                + noise as f32 / 12241.0
                                + dmc as f32 / 22638.0
                                + pcm as f32 / 22638.0;
                            let tnd_mix = if tnd_input == 0.0 {
                                0.0
                            } else {
                                159.79 / ((1.0 / tnd_input) + 100.0)
                            };
                            let expected = pulse_mix + tnd_mix;
                            assert_eq!(
                                mix(pulse, triangle, noise, dmc, pcm).to_bits(),
                                expected.to_bits(),
                                "mix({pulse}, {triangle}, {noise}, {dmc}, {pcm})",
                            );
                        }
                    }
                }
            }
        }
    }
}
