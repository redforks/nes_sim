use super::{FrameSequencerBits, FrameSequencerMode};
use crate::nes::apu::Sequencer;

// Step events per https://www.nesdev.org/wiki/APU_Frame_Counter (NTSC), in
// CPU cycles after the timer reset; the sequencer is clocked at the system
// clock rate (3 ticks per CPU cycle). "e" clocks envelopes and the triangle
// linear counter (quarter frame), "l" clocks length counters and sweep units
// (half frame), "f" sets the frame interrupt flag:
//
// mode 0:  7457 e | 14913 el | 22371 e | 29828 f | 29829 elf | 29830 f | reset
// mode 1:  7457 e | 14913 el | 22371 e | 29829 - | 37281 el  | reset (never f)
//
// The frame IRQ flag is therefore set on three consecutive CPU cycles
// (29828-29830) at the end of every mode-0 period; reading $4015 inside that
// window is answered by the next set. A $4017 write with bit 7 set resets the
// divider on the write cycle itself and clocks a half frame immediately
// (length counters included), the behavior blargg's forum APU tests
// (test-roms/apu/test_*.nes) pin down; $00/$40 writes reset after the usual
// 3-4 CPU cycle delay and clock nothing.

/// IRQ-flag-only pulse between scheduled steps (mode 0: CPU 29828 and 29830).
const IRQ_PULSE: FrameSequenceState = FrameSequenceState {
    irq: true,
    length_and_sweep: false,
    envelop_and_linear: false,
};

/// The half-frame clock generated immediately by a $4017 write with bit 7
/// set (length counters, sweep, envelopes, linear counter). It does not
/// consume a scheduled slot: the timer reset restarts the table from step 1
/// regardless.
const HALF_FRAME: FrameSequenceState = FrameSequenceState {
    irq: false,
    length_and_sweep: true,
    envelop_and_linear: true,
};

const FOUR_STEP_TRIGGERS: [FrameSequenceState; 4] = [
    /*  7457 */
    FrameSequenceState {
        irq: false,
        length_and_sweep: false,
        envelop_and_linear: true,
    },
    /* 14913 */
    FrameSequenceState {
        irq: false,
        length_and_sweep: true,
        envelop_and_linear: true,
    },
    /* 22371 */
    FrameSequenceState {
        irq: false,
        length_and_sweep: false,
        envelop_and_linear: true,
    },
    /* 29829 */
    FrameSequenceState {
        irq: true,
        length_and_sweep: true,
        envelop_and_linear: true,
    },
];

const FIVE_STEP_TRIGGERS: [FrameSequenceState; 5] = [
    /*  7457 */
    FrameSequenceState {
        irq: false,
        length_and_sweep: false,
        envelop_and_linear: true,
    },
    /* 14913 */
    FrameSequenceState {
        irq: false,
        length_and_sweep: true,
        envelop_and_linear: true,
    },
    /* 22371 */
    FrameSequenceState {
        irq: false,
        length_and_sweep: false,
        envelop_and_linear: true,
    },
    /* 29829 */
    FrameSequenceState {
        irq: false,
        length_and_sweep: false,
        envelop_and_linear: false,
    },
    /* 37281 */
    FrameSequenceState {
        irq: false,
        length_and_sweep: true,
        envelop_and_linear: true,
    },
];

#[derive(Debug)]
pub(super) struct FrameSequencer {
    pub output_latch: Option<FrameSequenceState>,

    frame_interrupt: bool,
    cycles: u32,
    sequences: Sequencer<FrameSequenceState>,
    frame_interrupt_inhibit: bool,
    request_timer_delay: u8,
    pending_mode: Option<FrameSequencerBits>,
    mode: FrameSequencerMode,
    /// use it to restore after reset
    saved_bits: FrameSequencerBits,
}

impl Default for FrameSequencer {
    fn default() -> Self {
        Self {
            frame_interrupt: false,
            cycles: 0,
            output_latch: Default::default(),
            sequences: Sequencer::new(&FOUR_STEP_TRIGGERS),
            frame_interrupt_inhibit: false,
            pending_mode: None,
            request_timer_delay: 0,
            mode: FrameSequencerMode::FourStep,
            saved_bits: FrameSequencerBits::default(),
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct FrameSequenceState {
    pub irq: bool,
    pub length_and_sweep: bool,
    pub envelop_and_linear: bool,
}

impl FrameSequencer {
    pub fn reset(&mut self) {
        self.frame_interrupt = false;
        self.output_latch = Default::default();
        self.pending_mode = Some(self.saved_bits);
        self.frame_interrupt_inhibit = self.saved_bits.disable_interrupt();
        self.request_timer_delay = 0;
    }

    pub fn tick_timer(&mut self) {
        self.cycles += 1;
        match self.mode {
            FrameSequencerMode::FourStep => match self.cycles {
                22371 | 44739 | 67113 | 89487 => self.output_latch = Some(self.sequences.tick()),
                89484 => self.output_latch = Some(IRQ_PULSE),
                // Period wrap: last IRQ set of the period lands on the reset.
                89490 => {
                    self.output_latch = Some(IRQ_PULSE);
                    self.cycles = 0;
                }
                _ => {}
            },
            FrameSequencerMode::FiveStep => match self.cycles {
                22371 | 44739 | 67113 | 89487 | 111843 => {
                    self.output_latch = Some(self.sequences.tick())
                }
                111846 => {
                    self.cycles = 0;
                }
                _ => {}
            },
        }
    }

    /// Applies a buffered $4017 write once its 3-4 CPU cycle delay elapses.
    /// The mode switch and timer reset land here for $00/$40 writes; $80/$C0
    /// writes already reset and clocked at the write itself (see
    /// [`Self::write_control_bits`]), so only the inhibit bookkeeping and the
    /// (re)reset happen here.
    pub fn tick(&mut self) {
        if self.request_timer_delay > 0 {
            self.request_timer_delay -= 1;
        } else if let Some(counter) = self.pending_mode.take() {
            self.cycles = 0;
            self.sequences.reset_items(match counter.mode() {
                FrameSequencerMode::FourStep => &FOUR_STEP_TRIGGERS,
                FrameSequencerMode::FiveStep => &FIVE_STEP_TRIGGERS,
            });
            self.mode = counter.mode();

            if self.frame_interrupt_inhibit {
                self.frame_interrupt = false;
            }
        }
    }

    pub fn request_irq(&self) -> bool {
        self.frame_interrupt && !self.frame_interrupt_inhibit
    }

    pub fn set_interrupt(&mut self) {
        self.frame_interrupt = true;
    }

    pub fn clear_interrupt(&mut self) {
        self.frame_interrupt = false;
    }

    pub fn write_control_bits(&mut self, bits: FrameSequencerBits) {
        debug_assert!(self.pending_mode.is_none());
        self.frame_interrupt_inhibit = bits.disable_interrupt();
        self.request_timer_delay = if self.cycles % 2 == 1 { 2 } else { 3 };
        if bits.mode() == FrameSequencerMode::FiveStep {
            // A $4017 write with the mode flag set resets the divider on the
            // write cycle itself and clocks a half frame (length counters,
            // sweep, envelopes, linear counter) immediately — the behavior
            // blargg's forum APU tests pin down. Only the mode switch runs
            // through the usual delayed path below.
            self.cycles = 0;
            self.mode = FrameSequencerMode::FiveStep;
            self.sequences.reset_items(&FIVE_STEP_TRIGGERS);
            self.output_latch = Some(HALF_FRAME);
        }
        self.pending_mode = Some(bits);
        self.saved_bits = bits;
    }
}
