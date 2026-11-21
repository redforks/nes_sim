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
//!    [`crate::NesMachine::set_spin_skip_enabled`] seam (skip does not
//!    exist yet, so both sides are the skip-off control) and require
//!    identical observations.
//!
//! No emulator behaviour is changed by anything in this module, and no
//! per-instruction bookkeeping is added to the executing path: the
//! observer is a [`crate::cpu::Plugin`], which is a test seam by
//! design (ADR-0010's `MachineView`).

use std::cell::RefCell;
use std::rc::Rc;

use crate::cpu::{CpuSnapshot, ExecuteResult, Plugin};
use crate::ines::INesFile;
use crate::nes::NesMcu;
use crate::nes_machine::NesMachine;
use crate::view::MachineView;

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

    // The poll loop. `BIT $2002` / `BPL loop` is the canonical VBL
    // wait. Enabling NMI in setup would let the frame-1 VBL dispatch
    // an NMI that interrupts the delay tail and shifts the first read,
    // so the loop enables NMI itself: the first VBL fall-through does
    // `STA $2000` (A = $80 from setup), which arms NMI observation for
    // every later frame, and the trailing `JMP loop` re-arms the wait.
    // The bare-`RTI` handler preserves A, so the enable write keeps
    // firing on every fall-through — same value, no side effect.
    let loop_top = pc;
    put(loop_top, &[0x2C, 0x02, 0x20]); // BIT $2002  (4)
    put(loop_top + 3, &[0x10, 0xFB]); // BPL loop   (3 taken / 2 not)
    put(loop_top + 5, &[0x8D, 0x00, 0x20]); // STA $2000  (4) NMI enable
    put(
        loop_top + 8,
        &[0x4C, loop_top as u8, (loop_top >> 8) as u8], // JMP loop (3)
    );
    assert!(
        loop_top + 11 <= 0xBFFA,
        "delay tail overflows the PRG page before the vectors"
    );

    // iNES header: NROM-128, one PRG page, one CHR page, horizontal
    // mirroring; CHR content is irrelevant (rendering is never enabled).
    let mut image = Vec::with_capacity(16 + PRG_LEN + CHR_LEN);
    image.extend_from_slice(b"NES\x1a");
    image.extend_from_slice(&[1, 1, 0, 0]);
    image.extend_from_slice(&[0; 8]);
    image.extend_from_slice(&prg);
    image.extend_from_slice(&vec![0; CHR_LEN]);
    (image, loop_top)
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
        }
    }

    let poll_starts = log.borrow().poll_starts.clone();
    ObservedRun {
        boundaries,
        poll_read_clocks: poll_starts.iter().map(|&c| c + 9).collect(),
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

/// The core differential of ticket #168, ready for tickets #169-#171:
/// two runs through the spin-skip seam must observe identical state.
/// Until the skip exists, both sides are the skip-off control.
fn assert_differential_matches(start_dot: u64) {
    let rom = poll_loop_rom(start_dot);
    let control = run_observed(&rom, false, 2);
    assert_eq!(
        control.poll_read_clocks.first(),
        Some(&start_dot),
        "first $2002 read must land on the requested start dot {start_dot}"
    );
    let replay = run_observed(&rom, false, 2);
    assert_eq!(
        control, replay,
        "skip-off vs skip-off observations diverged at start dot {start_dot}"
    );
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
