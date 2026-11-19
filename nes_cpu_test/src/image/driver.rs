use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use nes_core::audio_chunk::ChunkedAudioPush;

/// Buffers mixed APU samples so a run can be exported as a listenable WAV
/// artifact (`--dump-audio`).
///
/// The APU owns its per-frame sample buffer; the CLI drains it after the run
/// into this recorder and writes the file once the run stops.
#[derive(Clone)]
pub struct WavRecorder {
    buffer: Arc<Mutex<RecorderBuffer>>,
}

/// Staging plus committed samples. Full chunks move from the shared chunker
/// to `committed` on `push`; a partial tail moves on `write_wav`, so the
/// file holds every sample in push order under the shared chunk cadence.
#[derive(Clone, Default)]
struct RecorderBuffer {
    chunker: ChunkedAudioPush,
    committed: Vec<f32>,
}

impl WavRecorder {
    pub fn new() -> Self {
        Self {
            buffer: Arc::new(Mutex::new(RecorderBuffer::default())),
        }
    }

    /// Writes the buffered samples as a 44.1 kHz mono 16-bit PCM WAV file,
    /// returning the number of samples written.
    pub fn write_wav(&self, path: &Path) -> std::io::Result<usize> {
        let samples = {
            let mut buffer = self.buffer.lock().expect("WavRecorder buffer poisoned");
            let RecorderBuffer { chunker, committed } = &mut *buffer;
            chunker.flush(|chunk| committed.extend_from_slice(chunk));
            committed.clone()
        };
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

const SAMPLE_RATE: u32 = nes_core::nes::apu::Apu::SAMPLE_RATE;

fn pcm16(sample: f32) -> i16 {
    (sample.clamp(-1.0, 1.0) * 32767.0).round() as i16
}

impl WavRecorder {
    pub fn sample_rate(&self) -> u32 {
        SAMPLE_RATE
    }

    pub fn push_sample(&mut self, sample: f32) {
        let mut buffer = self.buffer.lock().expect("WavRecorder buffer poisoned");
        let RecorderBuffer { chunker, committed } = &mut *buffer;
        chunker.push(&[sample], |chunk| committed.extend_from_slice(chunk));
    }

    pub fn push_samples(&mut self, samples: &[f32]) {
        let mut buffer = self.buffer.lock().expect("WavRecorder buffer poisoned");
        let RecorderBuffer { chunker, committed } = &mut *buffer;
        chunker.push(samples, |chunk| committed.extend_from_slice(chunk));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nes_core::audio_chunk::AUDIO_CHUNK_SAMPLES;

    fn samples(n: usize) -> Vec<f32> {
        (0..n).map(|i| (i % 997) as f32 / 997.0 - 0.5).collect()
    }

    fn wav_samples(path: &Path) -> Vec<i16> {
        let bytes = std::fs::read(path).unwrap();
        assert_eq!(&bytes[0..4], b"RIFF");
        assert_eq!(&bytes[36..40], b"data");
        let data_len = u32::from_le_bytes(bytes[40..44].try_into().unwrap()) as usize;
        assert_eq!(data_len, bytes.len() - 44);
        bytes[44..]
            .chunks_exact(2)
            .map(|pair| i16::from_le_bytes([pair[0], pair[1]]))
            .collect()
    }

    fn temp_path(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "nes_cpu_test-recorder-{}-{}",
            std::process::id(),
            name
        ));
        std::fs::create_dir_all(&dir).unwrap();
        dir.join("out.wav")
    }

    fn expect_pcm16(sample: f32) -> i16 {
        (sample.clamp(-1.0, 1.0) * 32767.0).round() as i16
    }

    /// Exact-multiple input chunks with no remainder: the WAV holds every
    /// sample in push order.
    #[test]
    fn exact_multiple_chunks_round_trip_in_order() {
        let input = samples(2 * AUDIO_CHUNK_SAMPLES);
        let mut recorder = WavRecorder::new();
        recorder.push_samples(&input);
        let path = temp_path("exact");
        let written = recorder.write_wav(&path).unwrap();
        assert_eq!(written, input.len());
        assert_eq!(
            wav_samples(&path),
            input.iter().map(|&s| expect_pcm16(s)).collect::<Vec<_>>()
        );
        std::fs::remove_dir_all(path.parent().unwrap()).unwrap();
    }

    /// Remainder input plus empty pushes: partial tails flush at write, and
    /// empty input contributes nothing.
    #[test]
    fn remainder_and_empty_inputs_round_trip_in_order() {
        let input = samples(AUDIO_CHUNK_SAMPLES + 452);
        let mut recorder = WavRecorder::new();
        recorder.push_samples(&input[..AUDIO_CHUNK_SAMPLES - 100]);
        recorder.push_samples(&[]);
        recorder.push_samples(&input[AUDIO_CHUNK_SAMPLES - 100..]);
        recorder.push_samples(&[]);
        recorder.push_sample(0.25);
        let path = temp_path("remainder");
        let written = recorder.write_wav(&path).unwrap();
        assert_eq!(written, input.len() + 1);
        let mut expected: Vec<i16> = input.iter().map(|&s| expect_pcm16(s)).collect();
        expected.push(expect_pcm16(0.25));
        assert_eq!(wav_samples(&path), expected);
        std::fs::remove_dir_all(path.parent().unwrap()).unwrap();
    }

    /// An untouched recorder writes a valid empty WAV.
    #[test]
    fn empty_recorder_writes_empty_wav() {
        let recorder = WavRecorder::new();
        let path = temp_path("empty");
        let written = recorder.write_wav(&path).unwrap();
        assert_eq!(written, 0);
        assert!(wav_samples(&path).is_empty());
        std::fs::remove_dir_all(path.parent().unwrap()).unwrap();
    }

    /// Capture parity: the WAV samples equal the shared chunker's emitted
    /// chunk sequence concatenated — capture chunks identically to the
    /// playback sinks for the same input.
    #[test]
    fn capture_chunks_match_the_shared_cadence() {
        use nes_core::audio_chunk::ChunkedAudioPush;

        let input = samples(2500);
        let mut chunker = ChunkedAudioPush::new();
        let mut chunks = Vec::new();
        chunker.push(&input, |chunk| chunks.push(chunk.to_vec()));
        chunker.flush(|chunk| chunks.push(chunk.to_vec()));
        assert_eq!(chunks.len(), 3);
        assert_eq!(chunks[0].len(), AUDIO_CHUNK_SAMPLES);
        assert_eq!(chunks[1].len(), AUDIO_CHUNK_SAMPLES);
        assert_eq!(chunks[2].len(), 2500 - 2 * AUDIO_CHUNK_SAMPLES);

        let mut recorder = WavRecorder::new();
        recorder.push_samples(&input);
        let path = temp_path("parity");
        recorder.write_wav(&path).unwrap();
        let flat: Vec<f32> = chunks.concat();
        assert_eq!(
            wav_samples(&path),
            flat.iter().map(|&s| expect_pcm16(s)).collect::<Vec<_>>()
        );
        std::fs::remove_dir_all(path.parent().unwrap()).unwrap();
    }
}
