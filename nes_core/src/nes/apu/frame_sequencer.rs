use crate::nes::apu::{Sequence, Timer};

use super::{FrameSequencerBits, FrameSequencerMode};

const FOUR_STEP_TRIGGERS: [FrameSequenceState; 4] = [
    FrameSequenceState {
        irq: false,
        length_and_sweep: false,
        envelop_and_linear: true,
    },
    FrameSequenceState {
        irq: false,
        length_and_sweep: true,
        envelop_and_linear: true,
    },
    FrameSequenceState {
        irq: false,
        length_and_sweep: false,
        envelop_and_linear: true,
    },
    FrameSequenceState {
        irq: true,
        length_and_sweep: true,
        envelop_and_linear: true,
    },
];

const FIVE_STEP_TRIGGERS: [FrameSequenceState; 5] = [
    FrameSequenceState {
        irq: false,
        length_and_sweep: true,
        envelop_and_linear: true,
    },
    FrameSequenceState {
        irq: false,
        length_and_sweep: false,
        envelop_and_linear: true,
    },
    FrameSequenceState {
        irq: false,
        length_and_sweep: true,
        envelop_and_linear: true,
    },
    FrameSequenceState {
        irq: false,
        length_and_sweep: false,
        envelop_and_linear: true,
    },
    FrameSequenceState {
        irq: false,
        length_and_sweep: false,
        envelop_and_linear: false,
    },
];

const DIVIDER: u32 = 89490;

#[derive(Debug)]
pub(super) struct FrameSequencer {
    pub frame_interrupt: bool,
    pub output_latch: FrameSequenceState,
    timer: Timer<u32>,
    sequences: Sequence<FrameSequenceState>,
    frame_interrupt_inhibit: bool,

    pending_mode: Option<FrameSequencerBits>,
}

impl Default for FrameSequencer {
    fn default() -> Self {
        Self {
            frame_interrupt: false,
            timer: Timer {
                period: DIVIDER,
                ..Default::default()
            },
            output_latch: Default::default(),
            sequences: Sequence::new(&FOUR_STEP_TRIGGERS),
            frame_interrupt_inhibit: false,
            pending_mode: None,
        }
    }
}

#[derive(Default, Debug, Clone, Copy)]
pub struct FrameSequenceState {
    pub irq: bool,
    pub length_and_sweep: bool,
    pub envelop_and_linear: bool,
}

impl FrameSequencer {
    pub fn reset(&mut self) {
        self.frame_interrupt = false;
        self.output_latch = Default::default();
    }

    pub fn tick_timer(&mut self) {
        let r = if self.timer.tick() {
            Some(self.sequences.tick())
        } else {
            None
        };
        if let Some(r) = r {
            self.output_latch = r;
        }
    }

    pub fn tick(&mut self) {
        if let Some(counter) = self.pending_mode.take() {
            self.timer.reset();
            self.sequences.reset_items(match counter.mode() {
                FrameSequencerMode::FourStep => &FOUR_STEP_TRIGGERS,
                FrameSequencerMode::FiveStep => &FIVE_STEP_TRIGGERS,
            });
            if counter.mode() == FrameSequencerMode::FiveStep {
                self.output_latch = self.sequences.tick();
            }

            self.frame_interrupt_inhibit = counter.interrupt_flag();
            if self.frame_interrupt_inhibit {
                self.frame_interrupt = false;
            }
        }
    }

    pub fn request_irq(&self) -> bool {
        self.frame_interrupt && !self.frame_interrupt_inhibit
    }

    pub fn frame_interrupt(&self) -> bool {
        self.frame_interrupt
    }

    pub fn set_interrupt(&mut self) {
        if !self.frame_interrupt_inhibit {
            self.frame_interrupt = true;
        }
    }

    pub fn clear_interrupt(&mut self) {
        self.frame_interrupt = false;
    }

    pub fn write_mode(&mut self, counter: FrameSequencerBits, _apu_even_cycle: bool) {
        debug_assert!(self.pending_mode.is_none());
        self.pending_mode = Some(counter);
    }
}
