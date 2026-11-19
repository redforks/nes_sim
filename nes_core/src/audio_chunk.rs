//! Shared chunked audio-push routine for all three audio sinks
//! (SDL playback, web worklet, test-runner capture).
//!
//! Every sink accumulates pushed `f32` mono samples and releases them in
//! fixed-size chunks of [`AUDIO_CHUNK_SAMPLES`], so queue thresholds and
//! browser buffering change in one place and all sinks share one cadence.
//! The per-sink emit policy (SDL queue-pressure drop, worklet post, WAV
//! append) stays in each sink; only the chunk-boundary loop lives here.

/// Samples per emitted chunk: the SDL playback cadence all sinks share.
/// (SDL's `AudioQueue` consumes `f32` frames; the web worklet and the WAV
/// capture re-chunk the same boundaries.)
pub const AUDIO_CHUNK_SAMPLES: usize = 1024;

/// Accumulates pushed samples and releases full chunks in order.
/// A partial tail is held until the next [`push`](Self::push) completes it
/// or [`flush`](Self::flush) releases it (frame end).
#[derive(Debug, Default, Clone)]
pub struct ChunkedAudioPush {
    staging: Vec<f32>,
}

impl ChunkedAudioPush {
    /// Empty accumulator with room for one chunk, so the first frame's
    /// pushes never reallocate.
    pub fn new() -> Self {
        Self {
            staging: Vec::with_capacity(AUDIO_CHUNK_SAMPLES),
        }
    }

    /// Append `samples`, calling `emit` once per full chunk, in order.
    /// A trailing partial chunk stays staged.
    pub fn push(&mut self, samples: &[f32], mut emit: impl FnMut(&[f32])) {
        for &sample in samples {
            self.staging.push(sample);
            if self.staging.len() >= AUDIO_CHUNK_SAMPLES {
                emit(&self.staging);
                self.staging.clear();
            }
        }
    }

    /// Release the staged partial tail, if any. No-op when empty, so an
    /// exact-multiple push followed by `flush` emits nothing extra.
    pub fn flush(&mut self, mut emit: impl FnMut(&[f32])) {
        if !self.staging.is_empty() {
            emit(&self.staging);
            self.staging.clear();
        }
    }

    /// Staged-but-unemitted sample count (for tests and debugging).
    pub fn pending(&self) -> usize {
        self.staging.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Record emitted chunks for boundary assertions.
    fn collect(pushes: &[&[f32]], flush: bool) -> Vec<Vec<f32>> {
        let mut chunker = ChunkedAudioPush::new();
        let mut emitted = Vec::new();
        for samples in pushes {
            chunker.push(samples, |chunk| emitted.push(chunk.to_vec()));
        }
        if flush {
            chunker.flush(|chunk| emitted.push(chunk.to_vec()));
        }
        emitted
    }

    fn samples(n: usize) -> Vec<f32> {
        (0..n).map(|i| i as f32).collect()
    }

    #[test]
    fn exact_multiple_emits_full_chunks_with_no_remainder() {
        let input = samples(2 * AUDIO_CHUNK_SAMPLES);
        let emitted = collect(&[&input], true);
        assert_eq!(emitted.len(), 2);
        assert_eq!(emitted[0], samples(AUDIO_CHUNK_SAMPLES));
        assert_eq!(
            emitted[1],
            (AUDIO_CHUNK_SAMPLES..2 * AUDIO_CHUNK_SAMPLES)
                .map(|i| i as f32)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn remainder_waits_for_flush() {
        let input = samples(AUDIO_CHUNK_SAMPLES + 452);
        let mut chunker = ChunkedAudioPush::new();
        let mut emitted = Vec::new();
        chunker.push(&input, |chunk| emitted.push(chunk.to_vec()));
        assert_eq!(emitted.len(), 1, "only the full chunk emits on push");
        assert_eq!(emitted[0].len(), AUDIO_CHUNK_SAMPLES);
        chunker.flush(|chunk| emitted.push(chunk.to_vec()));
        assert_eq!(emitted.len(), 2);
        assert_eq!(
            emitted[1],
            (AUDIO_CHUNK_SAMPLES..)
                .take(452)
                .map(|i| i as f32)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn empty_input_emits_nothing() {
        assert!(collect(&[&[]], true).is_empty());
        assert!(collect(&[], true).is_empty());
    }

    #[test]
    fn partial_chunk_completes_on_the_next_push() {
        let first = samples(AUDIO_CHUNK_SAMPLES - 100);
        let second = samples(200);
        let emitted = collect(&[&first, &second], true);
        assert_eq!(emitted.len(), 2);
        assert_eq!(emitted[0].len(), AUDIO_CHUNK_SAMPLES);
        assert_eq!(emitted[1].len(), 100);
        // Order is preserved across the push boundary.
        assert_eq!(&emitted[0][..AUDIO_CHUNK_SAMPLES - 100], &first[..]);
        assert_eq!(&emitted[0][AUDIO_CHUNK_SAMPLES - 100..], &second[..100]);
        assert_eq!(&emitted[1][..], &second[100..]);
    }

    /// Cross-sink parity: 1024 samples through the shared routine emit as
    /// one desktop-cadence chunk — the web sink's old 256-sample cadence
    /// would have emitted four.
    #[test]
    fn one_frame_of_samples_emits_one_desktop_cadence_chunk() {
        let input = samples(AUDIO_CHUNK_SAMPLES);
        let emitted = collect(&[&input], true);
        assert_eq!(emitted.len(), 1);
        assert_eq!(emitted[0].len(), AUDIO_CHUNK_SAMPLES);
    }

    #[test]
    fn capture_cadence_matches_playback_for_a_typical_dump() {
        // 2500 samples: SDL's legacy sequence was two full chunks plus a
        // 452-sample remainder on flush. Capture must chunk identically.
        let input = samples(2500);
        let emitted = collect(&[&input], true);
        assert_eq!(emitted.len(), 3);
        assert_eq!(emitted[0].len(), AUDIO_CHUNK_SAMPLES);
        assert_eq!(emitted[1].len(), AUDIO_CHUNK_SAMPLES);
        assert_eq!(emitted[2].len(), 2500 - 2 * AUDIO_CHUNK_SAMPLES);
        let flat: Vec<f32> = emitted.concat();
        assert_eq!(flat, input);
    }
}
