use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::sync::{Arc, Mutex};

use nes_core::nes::apu::AudioDriver;

/// [`AudioDriver`] that buffers every mixed APU sample so a run can be
/// exported as a listenable WAV artifact (`--dump-audio`).
///
/// Clones share one buffer: a clone lives inside the machine's APU while the
/// original stays with the CLI to write the file once the run stops.
///
/// Declares the same 44.1 kHz rate as the no-op `()` driver so capture runs
/// resample identically to plain test runs.
#[derive(Clone)]
pub struct WavRecorder {
    buffer: Arc<Mutex<Vec<f32>>>,
}

impl WavRecorder {
    pub fn new() -> Self {
        Self {
            buffer: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Writes the buffered samples as a 44.1 kHz mono 16-bit PCM WAV file,
    /// returning the number of samples written.
    pub fn write_wav(&self, path: &Path) -> std::io::Result<usize> {
        let samples = self
            .buffer
            .lock()
            .expect("WavRecorder buffer poisoned")
            .clone();
        let data_len = (samples.len() * 2) as u32;
        let mut out = Vec::with_capacity(44 + samples.len() * 2);
        out.extend_from_slice(b"RIFF");
        out.extend_from_slice(&(36u32 + data_len).to_le_bytes());
        out.extend_from_slice(b"WAVE");
        out.extend_from_slice(b"fmt ");
        out.extend_from_slice(&16u32.to_le_bytes()); // fmt chunk size
        out.extend_from_slice(&1u16.to_le_bytes()); // PCM
        out.extend_from_slice(&1u16.to_le_bytes()); // mono
        out.extend_from_slice(&SAMPLE_RATE.to_le_bytes());
        out.extend_from_slice(&(SAMPLE_RATE * 2).to_le_bytes()); // byte rate
        out.extend_from_slice(&2u16.to_le_bytes()); // block align
        out.extend_from_slice(&16u16.to_le_bytes()); // bits per sample
        out.extend_from_slice(b"data");
        out.extend_from_slice(&data_len.to_le_bytes());
        for sample in &samples {
            out.extend_from_slice(&pcm16(*sample).to_le_bytes());
        }
        File::create(path)?.write_all(&out)?;
        Ok(samples.len())
    }
}

impl Default for WavRecorder {
    fn default() -> Self {
        Self::new()
    }
}

const SAMPLE_RATE: u32 = 44_100;

fn pcm16(sample: f32) -> i16 {
    (sample.clamp(-1.0, 1.0) * 32767.0).round() as i16
}

impl AudioDriver for WavRecorder {
    fn sample_rate(&self) -> u32 {
        SAMPLE_RATE
    }

    fn push_sample(&mut self, sample: f32) {
        self.buffer
            .lock()
            .expect("WavRecorder buffer poisoned")
            .push(sample);
    }
}
