const CPU_CLOCK_HZ: u32 = 1_789_773;

pub struct Mixer {
    sample_accumulator: u32,
    sample_rate: u32,
    dc_last_input: f32,
    dc_last_output: f32,
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
    pub fn emit_samples(
        &mut self,
        get_channel_samples: impl Fn() -> (u8, u8, u8, u8, u8),
        expansion: (u8, u8, u8),
        mut receive_output: impl FnMut(f32),
    ) {
        self.sample_accumulator = self.sample_accumulator.wrapping_add(self.sample_rate);
        while self.sample_accumulator >= CPU_CLOCK_HZ {
            self.sample_accumulator -= CPU_CLOCK_HZ;
            let (p1, p2, t, n, d) = get_channel_samples();
            let sample = self.mix_sample(p1, p2, t, n, d, expansion);
            receive_output(sample);
        }
    }

    fn mix_sample(
        &mut self,
        pulse_1: u8,
        pulse_2: u8,
        triangle: u8,
        noise: u8,
        dmc: u8,
        expansion: (u8, u8, u8),
    ) -> f32 {
        let input = mix(pulse_1, pulse_2, triangle, noise, dmc, expansion);
        let output = input - self.dc_last_input + (0.995 * self.dc_last_output);
        self.dc_last_input = input;
        self.dc_last_output = output;
        output.clamp(-1.0, 1.0)
    }
}

fn mix(pulse_1: u8, pulse_2: u8, triangle: u8, noise: u8, dmc: u8, expansion: (u8, u8, u8)) -> f32 {
    // MMC5 pulse channels join the APU pulses under the same nonlinear
    // curve (one shared analog node on hardware); the 8-bit MMC5 PCM DAC
    // joins the TND side with the DMC weight, so full scale is twice as
    // loud as the 7-bit APU DMC (NESdev MMC5 audio page).
    let pulse_sum = pulse_1 as f32 + pulse_2 as f32 + expansion.0 as f32 + expansion.1 as f32;
    let triangle = triangle as f32;
    let noise = noise as f32;
    let dmc = dmc as f32;

    let pulse_mix = if pulse_sum == 0.0 {
        0.0
    } else {
        95.88 / ((8128.0 / pulse_sum) + 100.0)
    };

    let tnd_input =
        triangle / 8227.0 + noise / 12241.0 + dmc / 22638.0 + expansion.2 as f32 / 22638.0;
    let tnd_mix = if tnd_input == 0.0 {
        0.0
    } else {
        159.79 / ((1.0 / tnd_input) + 100.0)
    };

    pulse_mix + tnd_mix
}
