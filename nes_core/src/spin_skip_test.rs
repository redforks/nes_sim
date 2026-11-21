//! Test scaffold for the `$2002` spin-wait skip (spec #167, ticket #168).
//!
//! Three pieces, all test-only:
//!
//! 1. [`poll_loop_rom`] — builds a synthetic iNES image (in code, no
//!    committed ROM) whose CPU runs an exact-cycle delay tail, then
//!    spins in the canonical poll loop `BIT $2002 / BPL loop`, whose
//!    VBL fall-through enables NMI (`STA $2000`, A held at `$80` by
//!    setup) and re-arms into the next wait. The *first* `$2002`
//!    read lands on a requested absolute dot clock; because NMI is
//!    enabled only by the loop itself, the delay tail is never
//!    interrupted and the placement is exact for every start dot.
//! 2. [`run_observed`] — drives a machine tick-by-tick through the
//!    public API and records, at every frame boundary (the PPU
//!    `take_vblank_entered` event, scanline 241 dot 1): CPU registers,
//!    the bus clock, and the NMI dispatches observed since the previous
//!    boundary. A [`PollObserverPlugin`] (the CPU `Plugin` seam) also
//!    logs every poll-loop iteration and NMI handler entry.
//! 3. The `differential_sweep_*` tests — for every achievable start
//!    dot from scanline 240 dot 0 through scanline 242 dot 0 plus one
//!    at the frame wrap, run the machine twice through the
//!    [`crate::NesMachine::set_spin_skip_enabled`] seam — skip-off
//!    control vs skip-on replay — and require identical frame
//!    boundaries. Start dots whose span would cross the VBL window
//!    must fall back to real execution (ticket #171 extends the skip
//!    into that window), so the sweep also pins which side each
//!    target lands on.
//!
//! No emulator behaviour is changed by anything in this module, and no
//! per-instruction bookkeeping is added to the executing path: the
//! observer is a [`crate::cpu::Plugin`], which is a test seam by
//! design (ADR-0010's `MachineView`).

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use crate::cpu::microcode::BranchTest;
use crate::cpu::{CpuSnapshot, EmptyPlugin, ExecuteResult, Plugin};
use crate::ines::INesFile;
use crate::mcu::Mcu;
use crate::nes::NesMcu;
use crate::nes_machine::NesMachine;
use crate::spin_skip::{LOOP_PERIOD_DOTS, SpinDeclaration};
use crate::{SystemClock, view::MachineView};

/// NMI vector target: a bare `RTI` handler (entry logs an NMI dispatch).
pub(crate) const NMI_HANDLER: u16 = 0x8010;
/// IRQ/BRK vector target (masked by `SEI`-style inhibit via `$4017`;
/// never entered, same bare `RTI` if it ever is).
const IRQ_HANDLER: u16 = 0x8011;
/// Reset entry: `JMP $8020` over the two one-byte handlers.
const RESET_ENTRY: u16 = 0x8000;
/// First byte of the setup + delay-tail code (`$8020`).
const SETUP_ENTRY: u16 = 0x8020;
/// Address where the exact-cycle delay tail begins.
const TAIL_START: u16 = 0x802A;

/// Cycles from power-on to the first `$2002` read of a [`poll_loop_rom`]
/// built with a zero-cycle delay tail. Every extra tail cycle moves the
/// read exactly 3 dot clocks later (1 CPU cycle = 3 dots, ADR-0018's
/// bus-owned clock), so a requested read clock `t` needs
/// `(t - READ0) / 3` tail cycles. Exact for every start dot because the
/// loop enables NMI itself — nothing can interrupt the tail.
///
/// Pinned by `poll_loop_first_read_lands_at_requested_start_dot`.
pub(crate) const FIRST_POLL_READ_CLOCK_ZERO_TAIL: u64 = 86;

/// Dots per frame with rendering disabled (no odd-frame dot skip).
const FRAME_LEN_DOTS: u64 = 89_342;
/// Dots per scanline.
const DOTS_PER_LINE: u64 = 341;
/// Power-on starts the PPU on the pre-render line, so the first visible
/// frame begins 341 dots in.
const POWERON_PRELRENDER_DOTS: u64 = 341;

/// Absolute dot clock of `(scanline, dot)` in visible frame `frame_no`
/// (frames count from power-on; frame 1 is the first frame after the
/// power-on pre-render line, with rendering off).
pub(crate) fn frame_dot(frame_no: u64, scanline: u16, dot: u16) -> u64 {
    assert!(frame_no >= 1, "visible frames count from 1");
    POWERON_PRELRENDER_DOTS
        + (frame_no - 1) * FRAME_LEN_DOTS
        + scanline as u64 * DOTS_PER_LINE
        + dot as u64
}

/// A built poll-loop ROM plus the layout facts the observer needs.
pub(crate) struct PollLoopRom {
    pub file: INesFile,
    /// Address of the `BIT $2002` at the top of the poll loop.
    pub loop_top: u16,
}

/// Program bytes for a poll-loop ROM whose first `$2002` read lands at
/// `FIRST_POLL_READ_CLOCK_ZERO_TAIL + 3 * tail_cycles` dots after
/// power-on. Returns the image bytes and the loop-top address.
fn poll_rom_bytes(tail_cycles: u64) -> (Vec<u8>, u16) {
    poll_rom_bytes_with_loop(
        tail_cycles,
        &[
            0x2C, 0x02, 0x20, // BIT $2002  (4)
            0x10, 0xFB, // BPL loop   (3 taken / 2 not)
            0x8D, 0x00, 0x20, // STA $2000  (4) NMI enable
        ],
    )
}

/// [`poll_rom_bytes`] with a custom loop body: `loop_body` is placed at
/// the computed loop top (its first instruction must be a 4-cycle read
/// so the requested read clock stays exact) and the builder appends the
/// `JMP loop` re-entry behind it.
/// [`poll_rom_bytes`] with one custom loop body (see
/// [`poll_rom_bytes_with_loops`]).
fn poll_rom_bytes_with_loop(tail_cycles: u64, loop_body: &[u8]) -> (Vec<u8>, u16) {
    let (image, tops) = poll_rom_bytes_with_loops(tail_cycles, &[loop_body]);
    (image, tops[0])
}

/// [`poll_rom_bytes`] with any number of loop bodies: the first is the
/// power-on loop (the delay tail falls into it); every loop gets a
/// trailing `JMP loop` re-entry, and the next loop is placed right
/// behind it — `machine.set_pc` can spin any of them directly.
fn poll_rom_bytes_with_loops(tail_cycles: u64, loop_bodies: &[&[u8]]) -> (Vec<u8>, Vec<u16>) {
    const PRG_LEN: usize = 16 * 1024;
    const CHR_LEN: usize = 8 * 1024;

    // NROM-128 PRG: unused space filled with NOPs so a runaway program
    // counter harmlessly re-enters the delay code.
    let mut prg = vec![0xEA; PRG_LEN];

    let mut put = |addr: u16, bytes: &[u8]| {
        // NROM-128 maps its single 16 KiB PRG page at $8000 and mirrors it
        // at $C000, so byte offsets wrap within the page.
        let off = ((addr as usize) - 0x8000) & 0x3FFF;
        prg[off..off + bytes.len()].copy_from_slice(bytes);
    };

    // Vectors: NMI -> $8010, RESET -> $8000, IRQ/BRK -> $8011.
    put(0xFFFA, &[NMI_HANDLER as u8, (NMI_HANDLER >> 8) as u8]);
    put(0xFFFC, &[RESET_ENTRY as u8, (RESET_ENTRY >> 8) as u8]);
    put(0xFFFE, &[IRQ_HANDLER as u8, (IRQ_HANDLER >> 8) as u8]);

    put(RESET_ENTRY, &[0x4C, 0x20, 0x80]); // JMP $8020 (3 cycles)
    put(NMI_HANDLER, &[0x40]); // RTI
    put(IRQ_HANDLER, &[0x40]); // RTI

    // Load order matters: A must hold $80 when the poll loop's own
    // fall-through `STA $2000` first runs (the loop enables NMI itself),
    // so #$80 is loaded last.
    put(SETUP_ENTRY, &[0xA9, 0x40]); // LDA #$40      (2)
    put(0x8022, &[0xA9, 0x80]); // LDA #$80      (2) A=$80 for the NMI-enable write
    put(0x8024, &[0x8D, 0x17, 0x40]); // STA $4017    (4) APU frame-IRQ inhibit

    // Exact-cycle delay tail: `tail_cycles` cycles of straight-line code
    // before the poll loop. NOP = 2 cycles, so an odd count needs one
    // 3-cycle JMP over itself to fix parity (tail_cycles >= 3 then).
    let mut pc = TAIL_START;
    let mut remaining = tail_cycles;
    if remaining % 2 == 1 {
        assert!(
            remaining >= 3,
            "odd tail needs >= 3 cycles for the parity JMP"
        );
        let jmp_target = pc + 3;
        put(pc, &[0x4C, jmp_target as u8, (jmp_target >> 8) as u8]);
        pc += 3;
        remaining -= 3;
    }
    for _ in 0..remaining / 2 {
        put(pc, &[0xEA]); // NOP
        pc += 1;
    }

    // The poll loops. Each body's first instruction is the 4-cycle read
    // so the requested read clock stays exact; whatever follows
    // (typically the back-edge branch and a fall-through) is the
    // caller's; the trailing `JMP loop` re-entry is appended here, and
    // the next loop sits right behind it. Enabling NMI in setup would
    // let the frame-1 VBL dispatch an NMI that interrupts the delay
    // tail and shifts the first read, so the canonical loop enables
    // NMI itself on the first VBL fall-through.
    let mut pc = pc;
    let mut tops = Vec::new();
    for loop_body in loop_bodies {
        let loop_top = pc;
        put(loop_top, loop_body);
        let reentry = loop_top + loop_body.len() as u16;
        put(
            reentry,
            &[0x4C, loop_top as u8, (loop_top >> 8) as u8], // JMP loop (3)
        );
        assert!(
            reentry + 3 <= 0xBFFA,
            "delay tail overflows the PRG page before the vectors"
        );
        pc = reentry + 3;
        tops.push(loop_top);
    }

    // iNES header: NROM-128, one PRG page, one CHR page, horizontal
    // mirroring; CHR content is irrelevant (rendering is never enabled).
    let mut image = Vec::with_capacity(16 + PRG_LEN + CHR_LEN);
    image.extend_from_slice(b"NES\x1a");
    image.extend_from_slice(&[1, 1, 0, 0]);
    image.extend_from_slice(&[0; 8]);
    image.extend_from_slice(&prg);
    image.extend_from_slice(&vec![0; CHR_LEN]);
    (image, tops)
}

/// Build a poll-loop ROM with a custom loop body whose first read
/// lands at `first_poll_read_clock` — same phase rules as
/// [`poll_loop_rom`]. The body's first instruction must be a 4-cycle
/// read so the placement stays exact.
fn poll_loop_variant(first_poll_read_clock: u64, loop_body: &[u8]) -> PollLoopRom {
    assert!(
        first_poll_read_clock >= FIRST_POLL_READ_CLOCK_ZERO_TAIL,
        "start dot {first_poll_read_clock} precedes the zero-tail first read"
    );
    let spread = first_poll_read_clock - FIRST_POLL_READ_CLOCK_ZERO_TAIL;
    assert!(
        spread.is_multiple_of(3),
        "start dot must sit on the CPU read phase (2 mod 3)"
    );
    let (image, loop_top) = poll_rom_bytes_with_loop(spread / 3, loop_body);
    let file = INesFile::new(image).expect("synthetic poll ROM must parse");
    PollLoopRom { file, loop_top }
}

/// iNES header length: 16 bytes precede the PRG bank in the image.
const INES_HEADER_LEN: usize = 16;
/// PRG mapping base of the synthetic NROM-128 image (`$8000`).
const PRG_BASE: u16 = 0x8000;

/// A poll-loop ROM whose delay tail preloads the open-bus latch: the
/// first five tail NOPs are replaced by `LDA #$1F / STA $2004` (6
/// cycles over 5 bytes, so the read lands 3 dots later than the
/// builder's clock — the request compensates). Any `$2xxx` write
/// refreshes the full latch, so bits 4-0 hold `$1F` and stay live
/// (the decay deadline is millions of dots away): a counterfactual
/// `$2002` read that wrongly refreshed the full latch would zero them.
fn latch_preload_poll_loop_rom(first_read_dot: u64) -> PollLoopRom {
    let requested = first_read_dot - 3;
    let spread = requested - FIRST_POLL_READ_CLOCK_ZERO_TAIL;
    assert!(
        spread.is_multiple_of(3),
        "start dot must sit on the CPU read phase (2 mod 3)"
    );
    let (mut image, loop_top) = poll_rom_bytes(spread / 3);
    let prg_offset = (TAIL_START - PRG_BASE) as usize;
    image[INES_HEADER_LEN + prg_offset..INES_HEADER_LEN + prg_offset + 5]
        .copy_from_slice(&[0xA9, 0x1F, 0x8D, 0x04, 0x20]);
    let file = INesFile::new(image).expect("synthetic latch ROM must parse");
    PollLoopRom { file, loop_top }
}

/// Spin a poll-loop ROM for `dots` past power-on and prove the loop
/// really is executing (poll starts observed at the loop top), then
/// hand the machine back for declaration assertions.
fn spun_machine(
    rom: &PollLoopRom,
    dots: u64,
) -> (NesMachine<PollObserverPlugin>, Rc<RefCell<ObserverLog>>) {
    spun_machine_with(rom, dots, false)
}

/// [`spun_machine`] with the quiet-span skip switch explicitly set.
fn spun_machine_with(
    rom: &PollLoopRom,
    dots: u64,
    spin_skip_enabled: bool,
) -> (NesMachine<PollObserverPlugin>, Rc<RefCell<ObserverLog>>) {
    let log = Rc::new(RefCell::new(ObserverLog::default()));
    let plugin = PollObserverPlugin {
        log: Rc::clone(&log),
        loop_top: rom.loop_top,
    };
    let mut machine = NesMachine::new(&rom.file, plugin);
    machine.set_spin_skip_enabled(spin_skip_enabled);
    run_to_clock(&mut machine, dots);
    (machine, log)
}

/// Assert that `rom` spins as a poll loop for `dots` dots but never
/// declares — the observer proves the loop is live (a dead loop would
/// trivially never declare), so the refusal is a real shape-gate
/// decision and not a silent stall.
fn assert_never_declares(rom: &PollLoopRom, dots: u64) {
    let (machine, log) = spun_machine(rom, dots);
    let spins = log.borrow().poll_starts.len();
    assert!(
        spins >= 5,
        "loop must actually spin (only {spins} poll starts)"
    );
    assert_eq!(
        machine.spin_declaration(),
        None,
        "a refused loop shape must never declare"
    );
}

/// Build a poll-loop ROM whose first `$2002` read lands exactly at
/// `first_poll_read_clock` dot clocks after power-on.
///
/// CPU reads execute on one fixed phase of the 3-dot machine tick, so
/// only clocks congruent to 2 (mod 3) are reachable — the sweep tests
/// step by 3 for exactly this reason.
pub(crate) fn poll_loop_rom(first_poll_read_clock: u64) -> PollLoopRom {
    assert!(
        first_poll_read_clock >= FIRST_POLL_READ_CLOCK_ZERO_TAIL,
        "start dot {first_poll_read_clock} precedes the zero-tail first read"
    );
    let spread = first_poll_read_clock - FIRST_POLL_READ_CLOCK_ZERO_TAIL;
    assert!(
        spread.is_multiple_of(3),
        "start dot must sit on the CPU read phase (2 mod 3)"
    );
    let tail_cycles = spread / 3;
    let (image, loop_top) = poll_rom_bytes(tail_cycles);
    let file = INesFile::new(image).expect("synthetic poll ROM must parse");
    PollLoopRom { file, loop_top }
}

/// One frame-boundary observation, recorded when the PPU reports
/// entering vblank (scanline 241 dot 1).
///
/// `cycles` is the bus clock when the boundary tick completed — one dot
/// past the vblank event dot, a constant offset identical on every run.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FrameBoundaryRecord {
    /// PPU frame counter at the boundary.
    pub frame_no: usize,
    /// CPU registers, status byte, and program counter at the boundary.
    pub cpu: CpuSnapshot,
    /// Absolute dot clock at the boundary tick's end.
    pub cycles: u64,
    /// Clocks of every NMI handler entry since the previous boundary.
    pub nmi_dots: Vec<u64>,
}

/// Everything the observer plugin logged for one run, split per frame.
#[derive(Debug, Default)]
struct ObserverLog {
    /// Clocks of every poll-loop `BIT $2002` instruction start.
    poll_starts: Vec<u64>,
    /// Clocks of every NMI handler entry (first instruction fetch at
    /// [`NMI_HANDLER`], i.e. right after the 7-cycle dispatch).
    nmi_dots: Vec<u64>,
}

/// A [`Plugin`] that watches the public instruction stream and logs
/// poll-loop iterations and NMI entries into shared storage, so the
/// runner can slice the log per frame boundary. `end` needs no
/// bookkeeping — `start` sees the same view the CPU publishes to real
/// plugins, which is exactly the surface ticket #167 observes.
struct PollObserverPlugin {
    log: Rc<RefCell<ObserverLog>>,
    loop_top: u16,
}

impl Plugin<NesMcu> for PollObserverPlugin {
    fn start(&mut self, view: &MachineView<NesMcu>) {
        let pc = view.cpu().pc;
        let clock = view.clock().cycles();
        if pc == self.loop_top {
            self.log.borrow_mut().poll_starts.push(clock);
        } else if pc == NMI_HANDLER {
            self.log.borrow_mut().nmi_dots.push(clock);
        }
    }

    fn end(&mut self, _view: &MachineView<NesMcu>) {}
}

/// One observed run of a poll-loop ROM through the machine-level
/// [`crate::NesMachine::set_spin_skip_enabled`] seam.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ObservedRun {
    /// One record per frame boundary, oldest first.
    pub boundaries: Vec<FrameBoundaryRecord>,
    /// Clocks of every `$2002` read (poll-loop `BIT` instruction start +9:
    /// fetch, abs-lo, abs-hi, then the read microcode).
    pub poll_read_clocks: Vec<u64>,
    /// Counterfactual polls replayed by the quiet-span skip (ticket
    /// #170): zero for the skip-off control, positive once a span
    /// engages, zero when every span fell back to real execution.
    pub replayed_polls: u64,
    /// Replay-counter snapshot at each frame boundary, aligned with
    /// `boundaries`: engagement is measured per span, so a fallback in
    /// the VBL window does not read as disengagement when a later,
    /// quiet span re-engages (and vice versa).
    pub replay_polls_at_boundaries: Vec<u64>,
}

/// Run `rom` tick-by-tick through the public machine API until `frames`
/// frame boundaries (PPU vblank-entered events) have fired, recording
/// the CPU state, bus clock, and NMI dispatches at each boundary.
///
/// `spin_skip_enabled` is routed through the machine seam so the
/// differential sweep exercises it on both sides even while the switch
/// is still a no-op.
pub(crate) fn run_observed(
    rom: &PollLoopRom,
    spin_skip_enabled: bool,
    frames: usize,
) -> ObservedRun {
    let log = Rc::new(RefCell::new(ObserverLog::default()));
    let plugin = PollObserverPlugin {
        log: Rc::clone(&log),
        loop_top: rom.loop_top,
    };
    let mut machine = NesMachine::new(&rom.file, plugin);
    machine.set_spin_skip_enabled(spin_skip_enabled);

    let mut boundaries = Vec::with_capacity(frames);
    let mut replay_polls_at_boundaries: Vec<u64> = Vec::with_capacity(frames);
    let mut nmi_seen = 0;
    // Guard against a harness regression that never sees vblank-entered.
    let mut tick_budget = 5 * FRAME_LEN_DOTS;
    while boundaries.len() < frames {
        assert!(
            tick_budget > 0,
            "no frame boundary within 5 frames: harness broken"
        );
        tick_budget -= 1;
        let result = machine.tick();
        assert!(
            matches!(result, ExecuteResult::Continue),
            "machine stopped mid-run: {result:?}"
        );
        if machine.mcu_mut().ppu_mut().take_vblank_entered() {
            let log_ref = log.borrow();
            let nmi_dots = log_ref.nmi_dots[nmi_seen..].to_vec();
            nmi_seen = log_ref.nmi_dots.len();
            drop(log_ref);
            boundaries.push(FrameBoundaryRecord {
                frame_no: machine.frame_no(),
                cpu: machine.cpu().snapshot(),
                cycles: machine.system_cycles(),
                nmi_dots,
            });
            replay_polls_at_boundaries.push(machine.spin_skip_replayed_polls());
        }
    }

    let poll_starts = log.borrow().poll_starts.clone();
    ObservedRun {
        boundaries,
        poll_read_clocks: poll_starts.iter().map(|&c| c + 9).collect(),
        replayed_polls: machine.spin_skip_replayed_polls(),
        replay_polls_at_boundaries,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

/// Start dots for the differential sweep: every achievable first-read
/// clock from scanline 240 dot 0 through scanline 242 dot 0, plus one
/// at the frame wrap.
fn sweep_targets() -> Vec<u64> {
    let first = frame_dot(1, 240, 0);
    let last_line = frame_dot(1, 242, 0);
    let mut targets: Vec<u64> = (first..=last_line).step_by(3).collect();
    // The sweep window is one CPU cycle short of (242,0); step_by keeps
    // the last target at most 2 dots before it. Add the frame wrap:
    // the first achievable read dot of the frame after the swept one.
    assert_eq!(*targets.last().unwrap(), frame_dot(1, 241, 340));
    targets.push(FRAME_LEN_DOTS + POWERON_PRELRENDER_DOTS + 1);
    targets
}

/// The core differential of ticket #170: a skip-on run must observe
/// exactly the skip-off control's state at every frame boundary. The
/// quiet span replays polls on the anchor grid and always ends before
/// the VBL window; start dots whose span would cross that window fall
/// back to real execution (ticket #171 extends the skip into the race
/// window), so the expectation for each side comes from frame
/// geometry, not from the skip machinery.
fn assert_differential_matches(start_dot: u64) {
    let rom = poll_loop_rom(start_dot);
    let control = run_observed(&rom, false, 2);
    assert_eq!(
        control.poll_read_clocks.first(),
        Some(&start_dot),
        "first $2002 read must land on the requested start dot {start_dot}"
    );
    assert_eq!(control.replayed_polls, 0, "the control must never replay");
    let replay = run_observed(&rom, true, 2);

    // CPU state, cycle count, and NMI dispatch dots match the control
    // at every frame boundary.
    assert_eq!(
        control.boundaries, replay.boundaries,
        "skip-on vs skip-off frame boundaries diverged at start dot {start_dot}"
    );

    // The first read still lands on the requested dot — the declaring
    // iteration always runs real — and every real poll that remains
    // sits on the control's poll lattice: real iterations re-enter the
    // loop exactly where the control does.
    assert_eq!(
        replay.poll_read_clocks.first(),
        Some(&start_dot),
        "skip-on first read moved off the requested start dot {start_dot}"
    );
    let control_polls: HashSet<u64> = control.poll_read_clocks.iter().copied().collect();
    for &clock in &replay.poll_read_clocks {
        assert!(
            control_polls.contains(&clock),
            "skip-on real poll {clock} left the control poll lattice              (start dot {start_dot})"
        );
    }

    // Engagement, measured per span: the first span that could cover
    // a VBL boundary must fall back (the window runs real); every
    // other start dot must engage. The measurement boundary is the one
    // that follows the span, so a later quiet re-engage after a
    // fallback does not mask it (and a fallback never hides behind a
    // re-engage). Ticket #171 relaxes the fallback expectation.
    let first_top = first_span_top(start_dot, &control);
    let boundary_index = if first_top <= frame_dot(1, 241, 1) {
        0
    } else {
        1
    };
    let replays_at_span = replay
        .replay_polls_at_boundaries
        .get(boundary_index)
        .copied()
        .unwrap_or(0);
    if first_span_falls_back(start_dot, &control) {
        assert_eq!(
            replays_at_span, 0,
            "start dot {start_dot} must fall back to real execution: its              span would cover the VBL window"
        );
    } else {
        assert!(
            replays_at_span > 0,
            "start dot {start_dot} must engage the quiet-span skip"
        );
    }
}

/// The loop top that follows the declaring back-edge: the branch
/// microcode runs 6 dots after the poll read, the next top 6 dots
/// after that.
const TOP_AFTER_DECLARING_POLL: u64 = 12;

/// The smallest VBL-boundary dot (the `(241,0)` race read or the
/// `(241,1)` VBL set) at or after `clock`, per the frame geometry.
fn next_vbl_boundary_dot(clock: u64) -> u64 {
    let frame_origin = frame_dot(1, 0, 0);
    let rel = clock - frame_origin;
    let frame = rel / FRAME_LEN_DOTS;
    let rem = rel % FRAME_LEN_DOTS;
    let base = frame_origin + frame * FRAME_LEN_DOTS;
    let vbl_line = frame_dot(1, 241, 0) - frame_origin;
    if rem <= vbl_line {
        base + vbl_line
    } else if rem == vbl_line + 1 {
        base + vbl_line + 1
    } else {
        base + FRAME_LEN_DOTS + vbl_line
    }
}

/// Would the first armed span of a run starting at `start_dot` cover
/// the VBL window? The declaring read comes from the control's
/// observed polls (empirical, independent of the skip machinery):
/// tops 21 dots apart mean the first pass was taken and declared;
/// any other gap means the first read saw the flag set, fell through,
/// and the second pass declares.
fn first_span_top(start_dot: u64, control: &ObservedRun) -> u64 {
    assert_eq!(control.poll_read_clocks.first(), Some(&start_dot));
    let declaring_poll = if control.poll_read_clocks.len() > 1
        && control.poll_read_clocks[1] == start_dot + LOOP_PERIOD_DOTS
    {
        start_dot
    } else {
        *control
            .poll_read_clocks
            .get(1)
            .expect("the poll loop must re-enter within the run")
    };
    declaring_poll + TOP_AFTER_DECLARING_POLL
}

fn first_span_falls_back(start_dot: u64, control: &ObservedRun) -> bool {
    let first_top = first_span_top(start_dot, control);
    // The span's first iteration would cover the VBL boundary dot —
    // or the loop's taken branch crosses a page boundary, making each
    // iteration 8 cycles: the 21-dot anchor grid would be wrong, so
    // the machinery never arms (ticket #171 owns modeling the extra
    // dummy-read cycle). The steady spacing is read from the control
    // run, well before any NMI dispatch can interrupt the grid.
    let crosses_window = next_vbl_boundary_dot(first_top) < first_top + LOOP_PERIOD_DOTS;
    let polls = &control.poll_read_clocks;
    // The steady iteration spacing: the first gap that is an iteration
    // grid (21 dots, or 24 for a page-crossing taken branch). Exit
    // detours (the fall-through out of the loop) and one-off NMI
    // dispatch gaps are skipped by the membership test.
    let steady = polls
        .windows(2)
        .take(12)
        .map(|w| w[1] - w[0])
        .find(|gap| *gap == LOOP_PERIOD_DOTS || *gap == LOOP_PERIOD_DOTS + 3)
        .expect("the poll loop must settle into its iteration grid");
    crosses_window || steady != LOOP_PERIOD_DOTS
}

#[test]
fn poll_loop_first_read_lands_at_requested_start_dot() {
    // (240,0) of frame 1 is the sweep's first start dot; the read must
    // land exactly there, on the CPU read phase.
    let start_dot = frame_dot(1, 240, 0);
    assert_eq!(start_dot % 3, 2, "start dot must sit on the CPU read phase");
    let rom = poll_loop_rom(start_dot);
    let run = run_observed(&rom, false, 2);

    assert_eq!(run.poll_read_clocks.first(), Some(&start_dot));

    assert_eq!(run.boundaries.len(), 2);
    assert_eq!(run.boundaries[0].frame_no, 1);
    assert_eq!(run.boundaries[1].frame_no, 2);

    // Both frame boundaries are structural dots: vblank-entered fires at
    // (241,1), and the record clock is the end of the machine tick that
    // contains the event. The frame length (89342) is not divisible by
    // the 3-dot tick, so the event dot lands on a different tick phase
    // each frame: +1 for frame 1 (82,523 = tick end - 1), +2 for frame 2.
    let tick_end = |clock: u64| clock + 3 - clock % 3;
    assert_eq!(run.boundaries[0].cycles, tick_end(frame_dot(1, 241, 1)));
    assert_eq!(run.boundaries[1].cycles, tick_end(frame_dot(2, 241, 1)));

    // The loop keeps polling: every read on the read phase, quantized
    // to whole CPU cycles (3 dots) — 7 tight (BIT+BPL taken), 9 across
    // a VBL fall-through, more when an NMI dispatch lands in between.
    assert!(
        run.poll_read_clocks.len() > 100,
        "loop must keep polling across both boundaries"
    );
    for &c in &run.poll_read_clocks {
        assert_eq!(c % 3, 2, "read clock {c} off the CPU read phase");
    }
    for w in run.poll_read_clocks.windows(2) {
        assert_eq!((w[1] - w[0]) % 3, 0, "poll cadence off the CPU cycle grid");
        assert!(
            w[1] - w[0] >= 21,
            "poll gap {} dots faster than the 7-cycle loop",
            w[1] - w[0]
        );
    }

    // At the first boundary the CPU is inside the poll loop.
    let loop_top = rom.loop_top;
    let pc = run.boundaries[0].cpu.pc;
    assert!(
        (loop_top..loop_top + 11).contains(&pc),
        "boundary pc {pc:#06x} escaped the poll loop at {loop_top:#06x}"
    );
}

#[test]
fn observer_records_differ_when_start_dot_shifts() {
    // Sensitivity canary: the differential harness must be able to
    // fail. Shifting the loop by one CPU cycle (3 dots) changes the
    // boundary observations, so `assert_differential_matches` compares
    // live state, not vacuous constants.
    let a = run_observed(&poll_loop_rom(frame_dot(1, 240, 0)), false, 2);
    let b = run_observed(&poll_loop_rom(frame_dot(1, 240, 3)), false, 2);
    assert_ne!(a.boundaries, b.boundaries);
    assert_ne!(a.poll_read_clocks, b.poll_read_clocks);
}

#[test]
fn sweep_covers_the_vbl_race_window_and_frame_wrap() {
    let targets = sweep_targets();
    // ~682-dot window stepped by the 3-dot CPU tick, plus the wrap.
    assert_eq!(targets.len(), 229);
    assert_eq!(targets[0], frame_dot(1, 240, 0));
    assert_eq!(*targets.last().unwrap(), frame_dot(2, 0, 1));
    // The VBL set dot itself (241,1) is a reachable read clock — the
    // classic dot-0 race must be inside the sweep.
    assert!(targets.contains(&frame_dot(1, 241, 1)));
}

fn sweep_chunk(range: std::ops::Range<usize>) {
    for &t in &sweep_targets()[range] {
        assert_differential_matches(t);
    }
}

// Ticket #169 — shape-gate declaration tests. These are the LOW-LEVEL
// tier of the spec's testing decisions: they observe the machine's
// internal declaration slot through the #[cfg(test)] seam, because the
// declaration itself has no public behavior yet (no skipping — real
// execution is unchanged). The PUBLIC tier is the differential sweep
// above: both its switch sides run with detection+declaration active,
// proving the declaration is observationally invisible.

/// Step `machine` (one [`NesMachine::tick`] = 3 dots) until the bus
/// clock reaches `until` (absolute dot clock). Panics if the machine
/// stops: the synthetic poll-loop ROMs run forever.
fn run_to_clock<P: Plugin<NesMcu>>(machine: &mut NesMachine<P>, until: u64) {
    while machine.system_cycles() < until {
        assert!(
            matches!(machine.tick(), ExecuteResult::Continue),
            "machine stopped before clock {until}"
        );
    }
}

/// The declaring back-edge dot for a loop whose first $2002 read
/// lands at `read_dot`: the branch microcode (operand fetch + PC
/// redirect) runs 2 CPU cycles after the read microcode — the taken
/// 3rd cycle that follows is a separate interrupt-detect microcode.
const FIRST_BACKEDGE_DOTS_AFTER_READ: u64 = 6;

#[test]
fn shape_gate_declares_bit_poll_loop_at_first_backedge() {
    let start = frame_dot(1, 240, 0);
    let rom = poll_loop_rom(start);
    let mut machine = NesMachine::new(&rom.file, EmptyPlugin::new());
    run_to_clock(&mut machine, start + 10);
    assert_eq!(
        machine.spin_declaration(),
        Some(SpinDeclaration {
            loop_pc: rom.loop_top,
            back_edge_target: rom.loop_top,
            poll_opcode: 0x2C,
            poll_operands: [0x02, 0x20],
            branch_pc: rom.loop_top + 3,
            branch_test: BranchTest::IfNegativeClear,
            anchor_dot: start + FIRST_BACKEDGE_DOTS_AFTER_READ,
        })
    );
}

#[test]
fn shape_gate_anchor_keeps_first_backedge_across_reentries() {
    // #165 Q3: the declaration anchors at the FIRST back-edge of the
    // declared loop. The loop falls through at VBL, re-enters via its
    // JMP, and spins again every frame — all later back-edges belong to
    // the same declared loop and must not re-anchor.
    let start = frame_dot(1, 240, 0);
    let rom = poll_loop_rom(start);
    let mut machine = NesMachine::new(&rom.file, EmptyPlugin::new());
    // Two full frames of spinning: past the first fall-through + JMP
    // re-entry, and past the frame-2 VBL re-entry.
    run_to_clock(&mut machine, start + 2 * FRAME_LEN_DOTS);
    let decl = machine
        .spin_declaration()
        .expect("loop must stay declared across re-entries");
    assert_eq!(decl.loop_pc, rom.loop_top);
    assert_eq!(
        decl.anchor_dot,
        start + FIRST_BACKEDGE_DOTS_AFTER_READ,
        "anchor must stay at the first back-edge of the declared loop"
    );
}

#[test]
fn shape_gate_refuses_backedge_that_lands_in_dmc_dma_window() {
    let start = frame_dot(1, 240, 0);
    let backedge = start + FIRST_BACKEDGE_DOTS_AFTER_READ;
    let rom = poll_loop_rom(start);

    // Control: with no DMA pending the first back-edge declares at its dot.
    let mut control = NesMachine::new(&rom.file, EmptyPlugin::new());
    run_to_clock(&mut control, backedge + 1);
    assert_eq!(
        control.spin_declaration().map(|d| d.anchor_dot),
        Some(backedge)
    );

    // Arm a DMC load DMA so its busy window covers the first back-edge:
    // the $4015 write is not deferred, the request is taken by the dma
    // tick ON the back-edge dot (DelayForLoad), and the CPU still
    // executes that dot — DMC halts the CPU only after the halt grant,
    // so the shape gate must refuse to declare under it (#165 Q8).
    let mut machine = NesMachine::new(&rom.file, EmptyPlugin::new());
    run_to_clock(&mut machine, backedge - 2);
    machine
        .mcu_mut()
        .write(0x4015, 0x1F, SystemClock::from_cycles(backedge - 2));
    // This tick executes the first back-edge inside the DMC busy window.
    assert!(matches!(machine.tick(), ExecuteResult::Continue));
    assert_eq!(
        machine.spin_declaration(),
        None,
        "a back-edge inside a DMC busy window must not declare"
    );

    // The loop keeps spinning past the short DMC stall; its next clean
    // back-edge declares, anchored past the refused one and on the same
    // CPU dot phase as the refused back-edge.
    run_to_clock(&mut machine, backedge + 90);
    let decl = machine
        .spin_declaration()
        .expect("the next clean back-edge must declare");
    assert!(
        decl.anchor_dot > backedge && decl.anchor_dot <= backedge + 90,
        "anchor {anchor} must sit between the refused back-edge and the next clean one",
        anchor = decl.anchor_dot
    );
    assert_eq!(decl.anchor_dot % 3, backedge % 3);
}

#[test]
fn shape_gate_declares_only_after_an_oam_dma_window_passes() {
    let start = frame_dot(1, 240, 0);
    let backedge = start + FIRST_BACKEDGE_DOTS_AFTER_READ;
    let rom = poll_loop_rom(start);

    // OAM DMA armed two dots before the tick that would execute the
    // first back-edge: the dma tick consumes the pending request first,
    // so the CPU is skipped for ~513 cycles and no back-edge (hence no
    // declaration) can land inside the window.
    let mut machine = NesMachine::new(&rom.file, EmptyPlugin::new());
    run_to_clock(&mut machine, backedge - 2);
    machine
        .mcu_mut()
        .write(0x4014, 0x02, SystemClock::from_cycles(backedge - 2));
    run_to_clock(&mut machine, backedge + 1000);
    assert_eq!(
        machine.spin_declaration(),
        None,
        "no back-edge may execute inside the OAM DMA window"
    );
    // Past the window the loop resumes; its first clean back-edge declares.
    run_to_clock(&mut machine, backedge + 1650);
    let decl = machine
        .spin_declaration()
        .expect("declaration must land after the OAM DMA");
    assert!(
        decl.anchor_dot > backedge + 1500,
        "anchor {anchor} must sit past the ~1539-dot window",
        anchor = decl.anchor_dot
    );
    assert_eq!(decl.loop_pc, rom.loop_top);
}

#[test]
fn shape_gate_declares_lda_poll_loop() {
    // LDA abs $2002 + BEQ: the read leaves A = $00 while VBL is clear
    // (Z set, branch taken). The other canonical opcode (#165 Q1)
    // must declare just like the BIT form.
    let start = frame_dot(1, 240, 0);
    let rom = poll_loop_variant(
        start,
        &[
            0xAD, 0x02, 0x20, // LDA $2002  (4)
            0xF0, 0xFB, // BEQ loop   (3 taken)
            0x8D, 0x00, 0x20, // STA $2000  (4) NMI enable
        ],
    );
    let mut machine = NesMachine::new(&rom.file, EmptyPlugin::new());
    run_to_clock(&mut machine, start + 10);
    let decl = machine
        .spin_declaration()
        .expect("LDA $2002 + BEQ must declare");
    assert_eq!(decl.poll_opcode, 0xAD);
    assert_eq!(decl.loop_pc, rom.loop_top);
    assert_eq!(decl.anchor_dot, start + FIRST_BACKEDGE_DOTS_AFTER_READ);
}

#[test]
fn shape_gate_refuses_lda_abs_indexed_loop() {
    // LDA $2002,X (X = 0 at power-on) reads the same register, but the
    // declarable shape (#165 Q1) is abs addressing only.
    let start = frame_dot(1, 240, 0);
    let rom = poll_loop_variant(
        start,
        &[
            0xBD, 0x02, 0x20, // LDA $2002,X (4, X=0)
            0xF0, 0xFB, // BEQ loop
            0x8D, 0x00, 0x20,
        ],
    );
    assert_never_declares(&rom, start + 200);
}

#[test]
fn shape_gate_refuses_non_2002_poll_loop() {
    // BIT $2001 + BPL spins on a write-only PPU register read (open
    // bus, N = 0), but only $2002 is a declarable poll target.
    let start = frame_dot(1, 240, 0);
    let rom = poll_loop_variant(
        start,
        &[
            0x2C, 0x01, 0x20, // BIT $2001  (4)
            0x10, 0xFB, // BPL loop
            0x8D, 0x00, 0x20,
        ],
    );
    assert_never_declares(&rom, start + 200);
}

#[test]
fn shape_gate_refuses_read_not_adjacent_to_backedge() {
    // A NOP between the read and the branch: the branch still targets
    // the loop top, but the read no longer directly precedes it and
    // the offset is $FA, not $FB.
    let start = frame_dot(1, 240, 0);
    let rom = poll_loop_variant(
        start,
        &[
            0x2C, 0x02, 0x20, // BIT $2002  (4)
            0xEA, // NOP        (2)
            0x10, 0xFA, // BPL loop
            0x8D, 0x00, 0x20,
        ],
    );
    assert_never_declares(&rom, start + 200);
}

#[test]
fn shape_gate_refuses_forward_branch_jmp_backedge() {
    // The only back-edge here is the JMP's; the conditional branch is
    // forward (offset 0), so no BCC..BVS back-edge exists at all.
    let start = frame_dot(1, 240, 0);
    let rom = poll_loop_variant(
        start,
        &[
            0x2C, 0x02, 0x20, // BIT $2002  (4)
            0x10, 0x00, // BPL +0      (3 taken, N=0)
            0x8D, 0x00, 0x20, // STA $2000  (4)
        ],
    );
    assert_never_declares(&rom, start + 200);
}

#[test]
fn shape_gate_replaces_declaration_for_a_different_loop() {
    // Two distinct poll loops in one ROM. Keep-first holds per loop
    // (pinned above); a DIFFERENT loop's back-edge replaces the slot
    // so the machine always describes the loop it is actually in.
    let start = frame_dot(1, 240, 0);
    let spread = start - FIRST_POLL_READ_CLOCK_ZERO_TAIL;
    let (image, tops) = poll_rom_bytes_with_loops(
        spread / 3,
        &[
            &[0x2C, 0x02, 0x20, 0x10, 0xFB, 0x8D, 0x00, 0x20], // A: BIT/BPL
            &[0xAD, 0x02, 0x20, 0xF0, 0xFB, 0x8D, 0x00, 0x20], // B: LDA/BEQ
        ],
    );
    let file = INesFile::new(image).expect("synthetic poll ROM must parse");
    let (a_top, b_top) = (tops[0], tops[1]);
    let mut machine = NesMachine::new(&file, EmptyPlugin::new());
    run_to_clock(&mut machine, start + 10);
    assert_eq!(
        machine.spin_declaration().map(|d| d.loop_pc),
        Some(a_top),
        "the power-on loop A declares first"
    );
    // Jump into loop B (set_pc drains to an instruction boundary first)
    // and spin it; B's back-edge must replace A's declaration.
    machine.set_pc(b_top);
    run_to_clock(&mut machine, start + 100);
    let decl = machine
        .spin_declaration()
        .expect("loop B must declare after the jump");
    assert_eq!(decl.loop_pc, b_top);
    assert_eq!(decl.poll_opcode, 0xAD);
}

#[test]
fn differential_sweep_scanline_240_early() {
    sweep_chunk(0..57);
}

#[test]
fn differential_sweep_scanline_240_late() {
    sweep_chunk(57..114);
}

#[test]
fn differential_sweep_scanline_241_to_vbl_exit() {
    sweep_chunk(114..171);
}

#[test]
fn differential_sweep_scanline_242_to_frame_wrap() {
    sweep_chunk(171..229);
}

#[test]
fn vbl_set_dot_always_runs_real() {
    // Ticket #170: the quiet span must end a full iteration before the
    // VBL window, so the VBL set — and the poll iteration that first
    // observes the fresh flag — run real. Observable through the
    // public seam: the wake iteration's poll read exists in the
    // 21-dot window ending at the set dot (a span parked over the
    // window would leave no real poll there), and the frame boundary
    // still matches the control.
    let start = frame_dot(1, 240, 0);
    let rom = poll_loop_rom(start);
    let control = run_observed(&rom, false, 2);
    let replay = run_observed(&rom, true, 2);
    assert!(replay.replayed_polls > 0, "the quiet span must engage");
    let vbl_set = frame_dot(1, 241, 1);
    assert!(
        replay
            .poll_read_clocks
            .iter()
            .any(|&c| (vbl_set - LOOP_PERIOD_DOTS..vbl_set).contains(&c)),
        "no real poll read in the iteration ending at the VBL set dot:          the skip consumed the wake iteration"
    );
    assert_eq!(
        control.boundaries, replay.boundaries,
        "boundaries diverged across the VBL-set wake"
    );
}

#[test]
fn nmi_dispatch_pins_hold_with_the_skip_on() {
    // The #168 NMI pins are load-bearing regression guards for the
    // skip: the span ends before the VBL set, the wake iteration and
    // the NMI edge run real, so the dispatch must land on the same
    // pinned dot as the skip-off control.
    let start = frame_dot(1, 240, 0);
    let run = run_observed(&poll_loop_rom(start), true, 3);
    assert_eq!(run.boundaries.len(), 3);
    assert!(run.replayed_polls > 0, "the quiet span must engage");
    let f2_event = frame_dot(2, 241, 1);
    assert!(run.boundaries[0].nmi_dots.is_empty());
    assert!(run.boundaries[1].nmi_dots.is_empty());
    assert_eq!(run.boundaries[2].nmi_dots, vec![f2_event + 28]);
}

#[test]
fn quiet_span_re_engages_after_each_wake() {
    // Lifecycle (spec #167 story 9): declare → quiet skip → wake →
    // re-declare. The two-boundary run's spans replay some polls; the
    // third frame adds the next wait's span — a parked quiet span
    // covers most of a frame of poll iterations, far more than the
    // short spans around the first VBL window.
    let start = frame_dot(1, 240, 0);
    let rom = poll_loop_rom(start);
    let two_frames = run_observed(&rom, true, 2);
    let three_frames = run_observed(&rom, true, 3);
    assert!(two_frames.replayed_polls > 0, "the first wait must engage");
    let next_frame_replays = three_frames.replayed_polls - two_frames.replayed_polls;
    assert!(
        next_frame_replays > FRAME_LEN_DOTS / LOOP_PERIOD_DOTS / 2,
        "the next wait must re-engage the skip for most of a frame          (replayed {next_frame_replays})"
    );
}

#[test]
fn counterfactual_poll_preserves_latch_bits_and_decay_clock() {
    // Low-level tier (ticket #170): a replayed $2002 read refreshes
    // latch bits 7-5 only — bits 4-0 must stay exactly what the bus
    // holds (preloaded $1F here; a wrong full refresh would zero
    // them) — and the decay clock (Ppu.cycle) must advance by exactly
    // the skipped ticks. Probed mid-span (skip parked) and deep inside
    // the next wait's span, both at identical machine clocks.
    let start = frame_dot(1, 240, 0);
    let rom = latch_preload_poll_loop_rom(start);

    // Mid-span, inside the first wait's quiet span.
    let mid_span = frame_dot(1, 240, 200);
    let (mut control, _) = spun_machine(&rom, mid_span);
    let (mut replay, _) = spun_machine_with(&rom, mid_span, true);
    assert!(
        replay.spin_skip_active(),
        "mid-span probe dot must sit inside an armed quiet span"
    );
    assert!(replay.spin_skip_replayed_polls() > 0);
    assert_eq!(
        control.ppu_open_bus_latch_for_test(),
        0x1F,
        "latch preload must be live at the probe dot"
    );
    assert_eq!(
        control.ppu_open_bus_latch_for_test(),
        replay.ppu_open_bus_latch_for_test(),
        "a replayed poll must keep latch bits 4-0"
    );
    assert_eq!(
        control.ppu_decay_cycle_for_test(),
        replay.ppu_decay_cycle_for_test(),
        "the decay clock must advance through the skipped span"
    );

    // Deep inside the next wait's span, past the first frame boundary:
    // the skip must have re-engaged and the probes must still agree.
    let deep = frame_dot(2, 100, 0);
    let (mut control, _) = spun_machine(&rom, deep);
    let (mut replay, _) = spun_machine_with(&rom, deep, true);
    assert!(
        replay.spin_skip_active(),
        "the span must be re-armed inside the second wait"
    );
    assert_eq!(
        control.ppu_open_bus_latch_for_test(),
        replay.ppu_open_bus_latch_for_test()
    );
    assert_eq!(
        control.ppu_decay_cycle_for_test(),
        replay.ppu_decay_cycle_for_test()
    );
}

#[test]
fn nmi_observation_pins_dispatch_dot_and_responds_to_start_phase() {
    // The loop enables NMI itself on the first VBL fall-through, so the
    // frame-2 VBL event (with NMI enabled since frame 1) latches an NMI:
    // the handler fetch (`RTI` at NMI_HANDLER) must appear in the frame-3
    // boundary's window at a deterministic dot.
    let start = frame_dot(1, 240, 0);
    let run = run_observed(&poll_loop_rom(start), false, 3);
    assert_eq!(run.boundaries.len(), 3);
    let f2_event = frame_dot(2, 241, 1);
    // Frame 1 and 2 windows are NMI-free: the line first rises at f2_event.
    assert!(run.boundaries[0].nmi_dots.is_empty());
    assert!(run.boundaries[1].nmi_dots.is_empty());
    // One dispatch in the frame-3 window, after the frame-2 event.
    assert_eq!(run.boundaries[2].nmi_dots, vec![f2_event + 28]);
}

#[test]
fn nmi_observation_pins_the_vbl_race_start_dispatch_dot() {
    // The start whose first poll read lands exactly on the VBL set dot: the
    // read observes the freshly set flag (fall-through runs immediately --
    // the read cadence gains the one-time STA+JMP detour), NMI is enabled
    // within frame 1, and the frame-2 VBL event dispatches at a pinned dot.
    let start = frame_dot(1, 241, 1);
    let run = run_observed(&poll_loop_rom(start), false, 3);
    assert_eq!(run.boundaries.len(), 3);
    assert_eq!(run.poll_read_clocks.first(), Some(&start));
    // The +39-dot cadence gap after the first read marks the one-time
    // fall-through detour (STA $2000 + JMP loop = 13 cycles).
    assert_eq!(run.poll_read_clocks[1] - run.poll_read_clocks[0], 39);
    let f2_event = frame_dot(2, 241, 1);
    assert!(run.boundaries[0].nmi_dots.is_empty());
    assert!(run.boundaries[1].nmi_dots.is_empty());
    assert_eq!(run.boundaries[2].nmi_dots, vec![f2_event + 34]);
}
