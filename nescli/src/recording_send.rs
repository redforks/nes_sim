/// Owns the trim/fast-forward send decision for movie recording.
///
/// Both `NesRender` and `RecordAudioDriver` consult this module instead of
/// inlining their own counters, so trim and fast-forward rules live in one
/// place and video/audio cannot drift on the reset rule.
#[derive(Debug, Clone, Copy)]
pub struct RecordingSendPolicy {
    trim_frames: u32,
    trim_counter: u32,
    ff_counter: u32,
}

impl RecordingSendPolicy {
    pub fn new(trim_frames: u32) -> Self {
        Self {
            trim_frames,
            trim_counter: 0,
            ff_counter: 0,
        }
    }

    /// Returns whether the current frame's data should be sent to the encoder.
    ///
    /// Applies the trim-then-fast-forward rule and advances internal counters.
    /// The fast-forward counter resets to zero on any non-fast-forward frame,
    /// unifying the behaviour of the render and audio paths.
    pub fn should_send(&mut self, fast_forward: bool) -> bool {
        if self.trim_counter < self.trim_frames {
            self.trim_counter += 1;
            false
        } else if fast_forward {
            let send = self.ff_counter.is_multiple_of(4);
            self.ff_counter += 1;
            send
        } else {
            self.ff_counter = 0;
            true
        }
    }
}

#[cfg(test)]
mod tests {
    use super::RecordingSendPolicy;

    #[test]
    fn zero_trim_sends_every_frame_when_not_fast_forwarding() {
        let mut p = RecordingSendPolicy::new(0);
        for _ in 0..5 {
            assert!(p.should_send(false));
        }
    }

    #[test]
    fn trim_skips_first_n_frames() {
        let mut p = RecordingSendPolicy::new(3);
        assert!(!p.should_send(false));
        assert!(!p.should_send(false));
        assert!(!p.should_send(false));
        assert!(p.should_send(false));
        assert!(p.should_send(false));
    }

    #[test]
    fn fast_forward_sends_every_fourth_frame() {
        let mut p = RecordingSendPolicy::new(0);
        // First frame of an idle range: counter 0 -> send, then 1,2,3 skip.
        assert!(p.should_send(true));
        assert!(!p.should_send(true));
        assert!(!p.should_send(true));
        assert!(!p.should_send(true));
        assert!(p.should_send(true));
    }

    #[test]
    fn counter_resets_on_non_fast_forward_frame() {
        let mut p = RecordingSendPolicy::new(0);
        // Advance into an idle range without sending.
        assert!(p.should_send(true));
        assert!(!p.should_send(true));
        // An active frame resets, so the next idle frame sends again.
        assert!(p.should_send(false));
        assert!(p.should_send(true));
    }

    #[test]
    fn interleaved_idle_and_active_ranges() {
        let mut p = RecordingSendPolicy::new(2);
        // Trim two frames.
        assert!(!p.should_send(false));
        assert!(!p.should_send(false));
        // Idle range: every 4th.
        assert!(p.should_send(true));
        assert!(!p.should_send(true));
        assert!(!p.should_send(true));
        assert!(!p.should_send(true));
        assert!(p.should_send(true));
        // Active frame resets and sends.
        assert!(p.should_send(false));
        // Next idle range starts fresh.
        assert!(p.should_send(true));
    }
}
