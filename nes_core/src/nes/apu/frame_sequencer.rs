use super::{FrameSequencerBits, FrameSequencerMode};

#[derive(Debug, Default)]
pub(super) struct FrameSequencer {
    pub frame_interrupt: bool,
    pub pending_frame_counter: Option<FrameSequencerBits>,

    frame_counter_cycle: u64,
    last_frame_counter_write: FrameSequencerBits,
    frame_counter_mode: FrameSequencerMode,
    frame_counter_write_delay: Option<u8>,
    frame_counter_short_delay: bool,
    frame_interrupt_inhibit: bool,
    frame_irq_shadow: bool,
}

#[derive(Default)]
pub(super) struct FrameSequencerClock {
    pub(super) quarter_frame: bool,
    pub(super) half_frame: bool,
}

impl FrameSequencer {
    pub fn reset(&mut self) {
        self.frame_interrupt = false;
        self.pending_frame_counter = Some(self.last_frame_counter_write);
        self.frame_counter_write_delay = Some(4);
    }

    pub fn tick(&mut self) -> FrameSequencerClock {
        self.frame_counter_cycle = self.frame_counter_cycle.wrapping_add(1);

        if let Some(delay) = &mut self.frame_counter_write_delay {
            *delay -= 1;
            if *delay == 0 {
                self.frame_counter_write_delay = None;
                if self.pending_frame_counter.take().is_some() {
                    return self.apply_frame_counter();
                }
            }
        }

        let prev_frame_interrupt = self.frame_interrupt;

        let clocks = if self.frame_counter_write_delay.is_none() {
            self.tick_frame_counter()
        } else {
            FrameSequencerClock::default()
        };

        self.frame_irq_shadow = prev_frame_interrupt;
        clocks
    }

    pub fn request_irq(&self) -> bool {
        if self.frame_counter_short_delay {
            self.frame_irq_shadow
        } else {
            self.frame_interrupt
        }
    }

    pub fn frame_interrupt(&self) -> bool {
        self.frame_interrupt
    }

    pub fn clear_interrupt(&mut self) {
        self.frame_interrupt = false;
    }

    pub fn set_counter(&mut self, counter: FrameSequencerBits, apu_even_cycle: bool) {
        self.last_frame_counter_write = counter;
        self.pending_frame_counter = Some(counter);
        let delay = if apu_even_cycle { 3 } else { 4 };
        self.frame_counter_write_delay = Some(delay);
        self.frame_counter_short_delay = delay == 3;

        self.frame_counter_mode = counter.mode();
        self.frame_interrupt_inhibit = counter.interrupt_flag();
        if self.frame_interrupt_inhibit {
            self.frame_interrupt = false;
        }
    }

    fn apply_frame_counter(&mut self) -> FrameSequencerClock {
        self.frame_counter_cycle = 0;

        if self.frame_counter_mode == FrameSequencerMode::FiveStep {
            FrameSequencerClock {
                quarter_frame: true,
                half_frame: true,
            }
        } else {
            FrameSequencerClock::default()
        }
    }

    fn tick_frame_counter(&mut self) -> FrameSequencerClock {
        match self.frame_counter_mode {
            FrameSequencerMode::FiveStep => match self.frame_counter_cycle {
                7_457 | 22_371 => FrameSequencerClock {
                    quarter_frame: true,
                    half_frame: false,
                },
                14_913 | 37_281 => FrameSequencerClock {
                    quarter_frame: true,
                    half_frame: true,
                },
                37_282 => {
                    self.frame_counter_cycle = 0;
                    FrameSequencerClock::default()
                }
                _ => FrameSequencerClock::default(),
            },
            FrameSequencerMode::FourStep => match self.frame_counter_cycle {
                7_457 | 22_371 => FrameSequencerClock {
                    quarter_frame: true,
                    half_frame: false,
                },
                14_913 => FrameSequencerClock {
                    quarter_frame: true,
                    half_frame: true,
                },
                29_828 => {
                    if !self.frame_interrupt_inhibit {
                        self.frame_interrupt = true;
                    }
                    FrameSequencerClock::default()
                }
                29_829 => {
                    if !self.frame_interrupt_inhibit {
                        self.frame_interrupt = true;
                    }
                    FrameSequencerClock {
                        quarter_frame: true,
                        half_frame: true,
                    }
                }
                29_830 => {
                    self.frame_counter_cycle = 0;
                    if !self.frame_interrupt_inhibit {
                        self.frame_interrupt = true;
                    }
                    FrameSequencerClock::default()
                }
                _ => FrameSequencerClock::default(),
            },
        }
    }
}
