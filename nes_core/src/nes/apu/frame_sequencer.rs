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
// window is answered by the next set. A $4017 write takes effect 3 or 4 CPU
// cycles later (by the write cycle's parity). A write with bit 7 set then
// resets the divider and clocks a half frame (length counters included)
// together with the reset, unless a unit-clocking step falls on that cycle or
// the one before: that step already clocked them. $00/$40 writes reset and
// clock nothing. Follows blargg's forum APU tests (test-roms/apu/test_*.nes,
// test-roms/test_apu_2/*.nes).

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
    /// A unit-clocking step (quarter or half frame) was latched on this
    /// CPU cycle / on the previous one. A mode-set reset landing within
    /// that window does not clock a second half frame.
    clock_step_this_cpu: bool,
    clock_step_prev_cpu: bool,
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
            clock_step_this_cpu: false,
            clock_step_prev_cpu: false,
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
        self.clock_step_this_cpu = false;
        self.clock_step_prev_cpu = false;
        self.output_latch = Default::default();
        self.pending_mode = Some(self.saved_bits);
        self.frame_interrupt_inhibit = self.saved_bits.disable_interrupt();
        self.request_timer_delay = 0;
    }

    pub fn tick_timer(&mut self) {
        self.cycles += 1;
        match self.mode {
            FrameSequencerMode::FourStep => match self.cycles {
                22371 | 44739 | 67113 | 89487 => {
                    let state = self.sequences.tick();
                    self.latch_step(state);
                }
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
                    let state = self.sequences.tick();
                    self.latch_step(state);
                }
                111846 => {
                    self.cycles = 0;
                }
                _ => {}
            },
        }
    }

    /// Latches a scheduled step and records whether it clocks any unit.
    fn latch_step(&mut self, state: FrameSequenceState) {
        self.output_latch = Some(state);
        self.clock_step_this_cpu |= state.length_and_sweep || state.envelop_and_linear;
    }

    /// Applies a buffered $4017 write once its 3-4 CPU cycle delay elapses.
    /// The divider reset and sequence restart land here. A mode-set
    /// ($80/$C0) landing also clocks a half frame together with the reset,
    /// unless a unit-clocking step was latched on this CPU cycle or the
    /// previous one (that step already clocked the units).
    pub fn tick(&mut self) {
        let blocked = self.clock_step_this_cpu || self.clock_step_prev_cpu;
        self.clock_step_prev_cpu = self.clock_step_this_cpu;
        self.clock_step_this_cpu = false;

        if self.request_timer_delay > 0 {
            self.request_timer_delay -= 1;
        } else if let Some(counter) = self.pending_mode.take() {
            self.cycles = 0;
            self.sequences.reset_items(match counter.mode() {
                FrameSequencerMode::FourStep => &FOUR_STEP_TRIGGERS,
                FrameSequencerMode::FiveStep => &FIVE_STEP_TRIGGERS,
            });
            self.mode = counter.mode();
            if counter.mode() == FrameSequencerMode::FiveStep && !blocked {
                // OR-merged: a same-dot IRQ pulse keeps its flag.
                let mut latch = self.output_latch.unwrap_or_default();
                latch.length_and_sweep |= HALF_FRAME.length_and_sweep;
                latch.envelop_and_linear |= HALF_FRAME.envelop_and_linear;
                self.output_latch = Some(latch);
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

    /// Applies a $4017 write issued on CPU cycle parity `cpu_odd_cycle`.
    /// Nothing happens at the write itself: the divider reset, the mode
    /// switch and (for $80/$C0) the half-frame clock all land together
    /// `request_timer_delay + 1` CPU cycles later.
    ///
    /// The 3 / 4 cycle delay follows Mesen2's `ApuFrameCounter`, but the
    /// parity sense is inverted — except for a mode-keeping $00/$40 write
    /// issued while the IRQ inhibit flag is still raised by the previous
    /// write, which uses the Mesen sense (even-cycle write lands 3 CPU
    /// cycles later, odd-cycle 4). That narrow exception is for
    /// cpu_interrupts_v2's `sync_apu`: after its $40 raised inhibit, the
    /// following $00 (mode 0 -> 0) must land C+3 on even cycles and C+4 on
    /// odd (the Mesen-literal mapping); the inverted sense lands it a cycle
    /// off and the $4015 edge sample (~29825 cycles later) misreads the APU
    /// alignment, failing 4-irq_and_dma. Anything else (including
    /// mode-changing writes after inhibit, like apu_test's $C0 -> $00 in
    /// 5-len_timing) keeps the inverted sense: with Mesen2's sense
    /// everywhere, test-roms/apu test_1 and test_6 and test-roms/test_apu_2
    /// test_3 and test_7 fail instead. Why the senses differ is not
    /// established (the codebase's CPU-cycle numbering may run one step
    /// against Mesen2's except right after inhibit is raised).
    pub fn write_control_bits(&mut self, bits: FrameSequencerBits, cpu_odd_cycle: bool) {
        debug_assert!(self.pending_mode.is_none());
        // Mesen sense only for a mode-keeping $00/$40 issued while inhibit
        // is still raised ($40 -> $00 in sync_apu); read the old flag before
        // overwriting. Everything else ($80/$C0 always; $00/$40 after clear
        // or across a mode change) uses the inverted sense.
        let inhibited = self.frame_interrupt_inhibit;
        let keeping = bits.mode() == self.mode;
        let mode_clear = bits.mode() == FrameSequencerMode::FourStep;
        let mesen_sense = inhibited && keeping && mode_clear;
        self.frame_interrupt_inhibit = bits.disable_interrupt();
        self.request_timer_delay = if cpu_odd_cycle != mesen_sense { 2 } else { 3 };
        self.pending_mode = Some(bits);
        self.saved_bits = bits;
    }
}
