const CPU_CLOCK_HZ: u64 = 1_789_773;

pub struct Mixer {
    sample_accumulator: u64,
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

    pub fn emit_samples(
        &mut self,
        p1: u8,
        p2: u8,
        t: u8,
        n: u8,
        d: u8,
        mut recenive_output: impl FnMut(f32),
    ) {
        self.sample_accumulator = self
            .sample_accumulator
            .wrapping_add(self.sample_rate as u64);
        while self.sample_accumulator >= CPU_CLOCK_HZ {
            self.sample_accumulator -= CPU_CLOCK_HZ;
            let sample = self.mix_sample(p1, p2, t, n, d);
            recenive_output(sample);
        }
    }

    fn mix_sample(&mut self, pulse_1: u8, pulse_2: u8, triangle: u8, noise: u8, dmc: u8) -> f32 {
        let input = mix(pulse_1, pulse_2, triangle, noise, dmc);
        let output = input - self.dc_last_input + (0.995 * self.dc_last_output);
        self.dc_last_input = input;
        self.dc_last_output = output;
        (output * 2.0).clamp(-1.0, 1.0)
    }
}

fn mix(pulse_1: u8, pulse_2: u8, triangle: u8, noise: u8, dmc: u8) -> f32 {
    let pulse_1 = pulse_1 as f32;
    let pulse_2 = pulse_2 as f32;
    let triangle = triangle as f32;
    let noise = noise as f32;
    let dmc = dmc as f32;

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

    pulse_mix + tnd_mix
}
