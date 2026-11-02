use super::{FrameSequencerBits, FrameSequencerMode};
use crate::nes::apu::Sequencer;

// Step events per https://www.nesdev.org/wiki/APU_Frame_Counter (NTSC), in
// CPU cycles after the timer reset. The divider advances once per APU clock
// (one CPU cycle). "e" clocks envelopes and the triangle
// linear counter (quarter frame), "l" clocks length counters and sweep units
// (half frame), "f" sets the frame interrupt flag:
//
// mode 0:  7457 e | 14913 el | 22371 e | 29828 f | 29829 elf | 29830 f | reset
// mode 1:  7457 e | 14913 el | 22371 e | 29829 - | 37281 el  | reset (never f)
//
// The frame IRQ flag is raised on three consecutive CPU cycles
// (29828-29830) at the end of every mode-0 period; it stays visible in
// `$4015` bit 6 even while the interrupt inhibit bit is set (steps 3-4), and
// step 5 clears it again under inhibit. A $4017 write takes effect 3 or 4 CPU
// cycles later (Mesen2 `ApuFrameCounter`): even-cycle writes 3 cycles, odd
// ones 4, so effects always land on an odd CPU cycle. A write with bit 7 set
// then resets the divider and clocks a half frame (length counters included)
// together with the reset, unless a unit-clocking scheduled step fired on
// that CPU cycle: that step already clocked the units. A second unit clock is
// also suppressed on the cycle after any clock (`block`). $00/$40 writes
// reset and clock nothing, and a write that raises inhibit clears the flag
// immediately. Follows blargg's forum APU tests (test-roms/apu/test_*.nes,
// test-roms/test_apu_2/*.nes).

const fn step(irq: bool, length_and_sweep: bool, envelop_and_linear: bool) -> FrameSequenceState {
    FrameSequenceState {
        irq,
        length_and_sweep,
        envelop_and_linear,
    }
}

/// The half-frame clock generated immediately by a $4017 write with bit 7
/// set (length counters, sweep, envelopes, linear counter). It does not
/// consume a scheduled slot: the timer reset restarts the table from step 1
/// regardless.
const HALF_FRAME: FrameSequenceState = step(false, true, true);

const FOUR_STEP_TRIGGERS: [FrameSequenceState; 4] = [
    /*  7457 */ step(false, false, true),
    /* 14913 */ step(false, true, true),
    /* 22371 */ step(false, false, true),
    /* 29829 */ step(true, true, true),
];

const FIVE_STEP_TRIGGERS: [FrameSequenceState; 5] = [
    /*  7457 */ step(false, false, true),
    /* 14913 */ step(false, true, true),
    /* 22371 */ step(false, false, true),
    /* 29829 */ step(false, false, false),
    /* 37281 */ step(false, true, true),
];

#[derive(Debug)]
pub(super) struct FrameSequencer {
    pub output_latch: Option<FrameSequenceState>,

    /// Raw frame-IRQ flag as visible in `$4015` bit 6. Unlike the CPU IRQ
    /// line it stays set while `frame_interrupt_inhibit` is raised, for the
    /// two cycles between steps 3 and 5 (Mesen `GetIrqFlag`).
    frame_interrupt: bool,
    /// CPU cycles for which a unit-clocking step (quarter or half frame)
    /// must not fire again. Set to 2 whenever a scheduled step or a mode-set
    /// landing clocks a unit, decremented once per CPU cycle; a scheduled
    /// step inside the window is dropped, a mode-set landing does not clock
    /// its extra half frame. Mesen's `_blockFrameCounterTick`.
    block: u8,
    /// Frame-counter divider, in CPU cycles. Advanced once per APU clock by
    /// [`Self::tick_timer`].
    cpu_cycles: u32,
    sequences: Sequencer<FrameSequenceState>,
    frame_interrupt_inhibit: bool,
    /// A buffered `$4017` write and the CPU cycles left before it lands.
    pending_mode: Option<(FrameSequencerBits, u8)>,
    mode: FrameSequencerMode,
    /// Mode restored after reset (the inhibit bit is always cleared then).
    saved_mode: FrameSequencerMode,
}

impl Default for FrameSequencer {
    fn default() -> Self {
        Self {
            frame_interrupt: false,
            block: 0,
            cpu_cycles: 0,
            output_latch: Default::default(),
            sequences: Sequencer::new(&FOUR_STEP_TRIGGERS),
            frame_interrupt_inhibit: false,
            pending_mode: None,
            mode: FrameSequencerMode::FourStep,
            saved_mode: FrameSequencerMode::default(),
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
        self.block = 0;
        self.output_latch = Default::default();
        // After reset the APU acts as if the last mode were written again
        // with the IRQ inhibit bit clear (Mesen2
        // `ApuFrameCounter::Reset`).
        self.pending_mode = Some((FrameSequencerBits::default().with_mode(self.saved_mode), 0));
        self.frame_interrupt_inhibit = false;
    }

    /// Advance the frame-counter divider by one CPU cycle. Called once per
    /// APU clock ([`crate::SystemClock::is_apu_clock`]); the thresholds below
    /// are CPU-cycle counts.
    pub fn tick_timer(&mut self) {
        self.cpu_cycles += 1;
        match self.mode {
            FrameSequencerMode::FourStep => match self.cpu_cycles {
                7457 | 14913 | 22371 => {
                    let state = self.sequences.tick();
                    self.apply_step(state);
                }
                // Steps 3 and 5 of the four-step sequence only raise the IRQ
                // flag; step 5 clears it again when interrupts are inhibited.
                29828 => self.set_interrupt(),
                29829 => {
                    let state = self.sequences.tick();
                    self.apply_step(state);
                }
                // Period wrap: last IRQ set of the period lands on the reset.
                29830 => {
                    self.set_interrupt();
                    if self.frame_interrupt_inhibit {
                        self.frame_interrupt = false;
                    }
                    self.cpu_cycles = 0;
                }
                _ => {}
            },
            FrameSequencerMode::FiveStep => match self.cpu_cycles {
                7457 | 14913 | 22371 | 29829 | 37281 => {
                    let state = self.sequences.tick();
                    self.apply_step(state);
                }
                37282 => {
                    self.cpu_cycles = 0;
                }
                _ => {}
            },
        }
    }

    /// Applies a scheduled step: raises the IRQ flag, then clocks the units
    /// unless a blocking tick (scheduled or mode-set landing) covers this
    /// CPU cycle. A blocked step is dropped, never deferred.
    fn apply_step(&mut self, state: FrameSequenceState) {
        if state.irq {
            self.set_interrupt();
        }
        if (state.length_and_sweep || state.envelop_and_linear) && self.block == 0 {
            self.output_latch = Some(state);
            self.block = 2;
        }
    }

    /// Applies a buffered $4017 write once its 3-4 CPU cycle delay elapses.
    /// The divider reset and sequence restart land here. A mode-set
    /// ($80/$C0) landing also clocks a half frame together with the reset,
    /// unless a unit-clocking step was latched on this CPU cycle or the
    /// previous one (that step already clocked the units).
    pub fn tick(&mut self) {
        if let Some((counter, delay)) = self.pending_mode {
            if delay > 0 {
                self.pending_mode = Some((counter, delay - 1));
            } else {
                self.pending_mode = None;
                self.cpu_cycles = 0;
                self.sequences.reset_items(match counter.mode() {
                    FrameSequencerMode::FourStep => &FOUR_STEP_TRIGGERS,
                    FrameSequencerMode::FiveStep => &FIVE_STEP_TRIGGERS,
                });
                self.mode = counter.mode();
                if counter.mode() == FrameSequencerMode::FiveStep && self.block == 0 {
                    // OR-merged: a same-dot IRQ pulse keeps its flag.
                    let mut latch = self.output_latch.unwrap_or_default();
                    latch.length_and_sweep |= HALF_FRAME.length_and_sweep;
                    latch.envelop_and_linear |= HALF_FRAME.envelop_and_linear;
                    self.output_latch = Some(latch);
                    self.block = 2;
                }

                if self.frame_interrupt_inhibit {
                    self.frame_interrupt = false;
                }
            }
        }

        if self.block > 0 {
            self.block -= 1;
        }
    }

    pub fn request_irq(&self) -> bool {
        self.frame_interrupt && !self.frame_interrupt_inhibit
    }

    /// Raw IRQ flag as `$4015` bit 6 sees it (unmasked by inhibit).
    pub fn interrupt_flag(&self) -> bool {
        self.frame_interrupt
    }

    pub fn set_interrupt(&mut self) {
        self.frame_interrupt = true;
    }

    pub fn clear_interrupt(&mut self) {
        self.frame_interrupt = false;
    }

    /// Applies a $4017 write issued on CPU cycle parity `cpu_odd_cycle`.
    /// Nothing happens at the write itself: the divider reset, the mode
    /// switch and (for $80/$C0) the half-frame clock all land together after
    /// the buffered delay plus one CPU cycle. An even-cycle write lands
    /// 3 CPU cycles later, an odd-cycle write 4 (Mesen2 `ApuFrameCounter`).
    /// A write that raises inhibit clears the IRQ flag immediately.
    pub fn write_control_bits(&mut self, bits: FrameSequencerBits, cpu_odd_cycle: bool) {
        debug_assert!(self.pending_mode.is_none());
        self.frame_interrupt_inhibit = bits.disable_interrupt();
        if self.frame_interrupt_inhibit {
            self.frame_interrupt = false;
        }
        self.pending_mode = Some((bits, if cpu_odd_cycle { 3 } else { 2 }));
        self.saved_mode = bits.mode();
    }
}
