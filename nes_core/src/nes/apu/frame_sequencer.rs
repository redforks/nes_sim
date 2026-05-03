use super::{FrameSequencerBits, FrameSequencerMode};
use crate::nes::apu::Sequence;

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

// The frame sequencer is clocked at the system clock rate (~1.79 MHz).
// In 4-step mode, a full frame is 89490 cycles (the PPU's 341 dots × 262 scanlines).
// Quarter-frame triggers occur at 1/4, 2/4, 3/4, and just before the end of the frame.
// 89484/89488 are IRQ-only pulses between the last quarter-frame and the reset.
// In 5-step mode, the frame is 111844 cycles (89490 × 5/4).

#[derive(Debug)]
pub(super) struct FrameSequencer {
    pub output_latch: Option<FrameSequenceState>,

    frame_interrupt: bool,
    cycles: u32,
    sequences: Sequence<FrameSequenceState>,
    frame_interrupt_inhibit: bool,
    request_timer_delay: u8,
    pending_mode: Option<FrameSequencerBits>,
    mode: FrameSequencerMode,
}

impl Default for FrameSequencer {
    fn default() -> Self {
        Self {
            frame_interrupt: false,
            cycles: 0,
            output_latch: Default::default(),
            sequences: Sequence::new(&FOUR_STEP_TRIGGERS),
            frame_interrupt_inhibit: false,
            pending_mode: None,
            request_timer_delay: 0,
            mode: FrameSequencerMode::FourStep,
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
        self.pending_mode = None;
    }

    pub fn tick_timer(&mut self) {
        self.cycles += 1;
        match self.mode {
            FrameSequencerMode::FourStep => match self.cycles {
                22641 | 44739 | 67113 | 89487 => self.output_latch = Some(self.sequences.tick()),
                89484 | 89488 => {
                    self.output_latch = Some(FrameSequenceState {
                        irq: true,
                        length_and_sweep: false,
                        envelop_and_linear: false,
                    })
                }
                89490 => {
                    self.cycles = 0;
                }
                _ => {}
            },
            FrameSequencerMode::FiveStep => match self.cycles {
                22641 | 44739 | 67113 | 89487 | 111843 => {
                    self.output_latch = Some(self.sequences.tick())
                }
                111844 => {
                    self.cycles = 0;
                }
                _ => {}
            },
        }
    }

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
            if counter.mode() == FrameSequencerMode::FiveStep {
                self.output_latch = Some(self.sequences.tick());
            }

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

    pub fn write_mode(&mut self, counter: FrameSequencerBits, apu_even_cycle: bool) {
        debug_assert!(self.pending_mode.is_none());
        self.pending_mode = Some(counter);
        self.frame_interrupt_inhibit = counter.disable_interrupt();
        self.request_timer_delay = if apu_even_cycle { 2 } else { 3 };
    }
}
