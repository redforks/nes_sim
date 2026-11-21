//! Spin-wait loop shape detection and declaration (spec #167, ticket #169).
//!
//! A NES program waiting for the vblank flag spins in a poll loop shaped
//! like `BIT $2002` / `BPL loop` (or `LDA $2002` / `BEQ loop`). Ticket
//! #169 only *detects and declares* that shape — real execution is
//! unchanged; the skip itself lands with ticket #170.
//!
//! Cost contract (spec #165 Q7): detection runs only when the CPU
//! executes a taken conditional branch whose offset is exactly $FB — the
//! only back-edge whose target is the 3-byte read directly preceding the
//! 2-byte branch. Every other instruction pays nothing: one integer
//! compare inside the already-taken branch path, no per-dot or
//! per-instruction bookkeeping, and no state anywhere outside a
//! declared candidate.
//!
//! Shape rules (spec #165 Q1):
//! - accept: `BIT abs` ($2C) or `LDA abs` ($AD) reading $2002, directly
//!   followed by a taken conditional branch (BCC..BVS — structural: only
//!   those opcodes dispatch to the branch microcode) jumping back to the
//!   read instruction's PC;
//! - refuse: `LDA abs,X` and every other opcode, non-$2002 addresses,
//!   reads not directly followed by the back-edge branch, and forward
//!   branches.
//!
//! Declaration gating (spec #165 Q8): a hit is refused while DMC or OAM
//! DMA is pending or in flight. The DMA check runs in the machine's
//! drain (see `NesMachine::declare_spin_loop`), not here.
//!
//! Anchor (spec #165 Q3): the first back-edge of a declared loop is the
//! grid anchor ticket #170's quiet-span skip aligns to. A declared loop
//! keeps its anchor: the declaration slot is only overwritten by a
//! *different* loop, and ticket #170 will own re-arming per loop
//! instance (declare → skip → wake → re-declare).
//!
//! `NesMachine::set_spin_skip_enabled` gates only ticket #170's skip
//! consumption, not detection: the #168 differential sweep runs the
//! switch on both sides with detection active, which is exactly what
//! proves the declaration itself is observationally invisible.
//!
//! Quiet-span skip (tickets #170 + #171): once a loop is declared and
//! the switch is on, the machine replaces real poll+branch execution
//! with the counterfactual model — each replayed `$2002` read runs the
//! real read semantics on its poll dot (VBL clear, write-toggle reset,
//! the dot-0 race logic, partial bus-latch refresh) through the
//! published `Mcu::read` path, and the CPU-visible flag effects are
//! applied via the narrow `Cpu::apply_spin_poll_result` seam. The grid
//! is anchored at the declaration's first back-edge (#165 Q3).
//!
//! Ticket #171 edge handling. The span is no longer scoped away from
//! the VBL window — replays land exactly on the control's poll dots,
//! so a poll inside the race window is a real read on its exact dot
//! (#168's pinned read is never moved) and the VBL flag it observes
//! ends the span at that poll. The frame boundary itself never runs
//! parked: at each poll the span checks whether the upcoming VBL set
//! sits inside the next parked iteration
//! ([`next_parked_iteration_covers_vbl_set`]) and ends at that poll
//! when it does, resuming at the branch PC so the branch, the boundary
//! dot, and the flag-observing exit poll all run real, on the control's
//! own cycle grid. Every other edge ends the span at dot precision: a
//! `/NMI` line move or IRQ level move is recorded at the dot it happens
//! (the machine watches the published lines every dot) and the span
//! wakes at the next grid top — running the standard end-of-cycle
//! interrupt poll first when the control's in-flight branch cycle
//! already saw the edge — and a DMA that becomes visible ends the span
//! at that very dot. No synthesized exit: the interrupt decision is
//! always the real published detection machinery on real detector state
//! (spec #165 Q2, ADR 0019's lesson — no detector internals on the skip
//! path), and every wake resumes at an instruction boundary (loop top
//! or branch PC) from the last completed real iteration — never a
//! partially replayed one.

use crate::SystemClock;
use crate::bus::Bus;
use crate::cpu::microcode::BranchTest;
use crate::mcu::Mcu;

/// The PPU status register the declarable poll loop spins on.
pub(crate) const POLL_ADDR: u16 = 0x2002;
/// `BIT abs` — poll opcode whose N/Z flags feed a `BPL`/`BMI` back-edge.
pub(crate) const BIT_ABS: u8 = 0x2C;
/// `LDA abs` — poll opcode whose loaded value feeds a `BEQ`/`BNE`
/// back-edge.
pub(crate) const LDA_ABS: u8 = 0xAD;

/// The only taken-branch offset that forms the declarable back-edge:
/// target = branch_pc + 2 + sext(offset), and the shape requires that
/// target to be the 3-byte read directly preceding the 2-byte branch,
/// i.e. branch_pc - 3 — which forces offset = -5 ($FB).
const BACKEDGE_TO_LOOP_TOP: u8 = 0xFB;

/// A back-edge branch that matched the declarable poll-loop shape.
///
/// Transient, same-dot CPU→machine event: the branch microcode latches
/// it (a single [`Cpu`] field), the machine drains it in the same
/// `tick_dot_2`, so it never survives a dot and nothing else on the CPU
/// surface can observe it (ADR 0010).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SpinBackedgeHit {
    /// PC of the poll instruction (the branch's target, i.e. the loop
    /// top).
    pub(crate) loop_pc: u16,
    /// The poll opcode ($2C `BIT abs` or $AD `LDA abs`).
    pub(crate) poll_opcode: u8,
    /// The branch condition the back-edge tests — `apply_spin_poll_result`
    /// evaluates it on the counterfactual polls to decide taken/not.
    pub(crate) branch_test: BranchTest,
}

/// A declared spin-wait poll loop — the hand-off state ticket #170's
/// quiet-span skip consumes. Captured once per loop (keep-first), at
/// the loop's first back-edge.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SpinDeclaration {
    /// PC of the poll instruction at the loop top.
    pub(crate) loop_pc: u16,
    /// The poll opcode ($2C or $AD).
    pub(crate) poll_opcode: u8,
    /// PC of the back-edge branch instruction (`loop_pc + 3` by shape).
    pub(crate) branch_pc: u16,
    /// Absolute dot clock of the declaring back-edge — the first
    /// back-edge of this loop (#165 Q3). #170's quiet-span grid anchors
    /// here.
    pub(crate) anchor_dot: u64,
    /// The back-edge branch condition, latched from the hit.
    pub(crate) branch_test: BranchTest,
}

impl SpinDeclaration {
    /// Capture a declared loop from a shape hit at `clock` (the dot the
    /// declaring back-edge executed on — bus-owned, ADR 0018).
    pub(crate) fn new(hit: SpinBackedgeHit, clock: SystemClock) -> Self {
        SpinDeclaration {
            loop_pc: hit.loop_pc,
            poll_opcode: hit.poll_opcode,
            branch_pc: hit.loop_pc.wrapping_add(3),
            anchor_dot: clock.cycles(),
            branch_test: hit.branch_test,
        }
    }
}

/// Does the taken branch at `branch_pc` with `offset` form the
/// declarable poll-loop back-edge? `peek` reads program bytes
/// side-effect-free (it must not consume `$2002` reads — only
/// [`Mcu::peek`] qualifies).
///
/// Called only from the taken-branch microcode path (BCC..BVS), so
/// "conditional branch" is structural, not re-checked here.
pub(crate) fn backedge_shape_hit(
    peek: impl Fn(u16) -> u8,
    branch_pc: u16,
    offset: u8,
    branch_test: BranchTest,
) -> Option<SpinBackedgeHit> {
    // A taken branch with offset $FB is the only back-edge that both
    // targets the PC directly before the 3-byte read and is itself
    // directly preceded by that read (target = branch_pc + 2 + sext($FB)
    // = branch_pc - 3). Anything else — forward branches, branches
    // with bytes between the read and the branch, branches that
    // overshoot the read — is not the declarable shape.
    if offset != BACKEDGE_TO_LOOP_TOP {
        return None;
    }
    let loop_pc = branch_pc
        .wrapping_add(2)
        .wrapping_add((offset as i8) as u16);
    debug_assert_eq!(loop_pc, branch_pc.wrapping_sub(3));

    // Match by opcode and operand bytes at the loop PC — no emulator
    // state participates (spec #165 Q1).
    let poll_opcode = peek(loop_pc);
    if !matches!(poll_opcode, BIT_ABS | LDA_ABS) {
        return None;
    }
    if peek(loop_pc.wrapping_add(1)) != POLL_ADDR as u8
        || peek(loop_pc.wrapping_add(2)) != (POLL_ADDR >> 8) as u8
    {
        return None;
    }
    Some(SpinBackedgeHit {
        loop_pc,
        poll_opcode,
        branch_test,
    })
}

/// [`backedge_shape_hit`] over the CPU's bus: peeks program bytes
/// through the memory map without disturbing it.
pub(crate) fn backedge_shape_hit_at_bus<M: Mcu>(
    bus: &Bus<M>,
    branch_pc: u16,
    offset: u8,
    branch_test: BranchTest,
) -> Option<SpinBackedgeHit> {
    backedge_shape_hit(|addr| bus.mcu().peek(addr), branch_pc, offset, branch_test)
}

/// One poll-loop iteration on the CPU read phase: `BIT abs` (4 cycles)
/// plus a taken back-edge branch (3 cycles) = 7 CPU cycles = 21 dots.
/// The declarable shape fixes this period — `#171` owns any exception.
pub(crate) const LOOP_PERIOD_DOTS: u64 = 21;

/// Dots from a grid loop top to the poll read microcode of that
/// iteration (fetch 2 + address 2 + read 1 = 5 cycles; the read is the
/// 4th cycle, landing on `top + 9` dots).
const POLL_AFTER_LOOP_TOP_DOTS: u64 = 9;

/// Dots from the declaring back-edge (the branch operand read, the
/// anchor) to iteration 1's loop top: the anchor is the branch's
/// second cycle; the taken-cycle is one cycle later, then iteration 1
/// fetches.
const TOP_AFTER_ANCHOR_DOTS: u64 = 6;

/// Dots before a grid top at which the control's in-flight back-edge
/// branch read the `/NMI` sample that its operand-cycle interrupt
/// poll uses: the branch's second cycle sits at `top - 6` and reads
/// the sample taken two dots earlier (`top - 8`). Ticket #171's wake
/// rule: a line rise at or before `top - 8` was already visible to
/// that poll, so the control's dispatch begins at the top and the
/// wake must run the standard interrupt poll there; a later rise is
/// first seen by the *next* iteration's poll, so a plain wake (real
/// fetch from the top) reproduces it exactly.
const NMI_BRANCH_SAMPLE_BEFORE_TOP: u64 = 8;

/// Dots before a grid top at which the control's in-flight back-edge
/// branch operand-cycle poll read the IRQ level (`top - 6`, level
/// semantics — read directly, no sample lag). A level rise at or
/// before `top - 6` was seen by that poll and dispatches at the top;
/// a later rise dispatches after the next poll, reproduced by a
/// plain wake.
const IRQ_BRANCH_POLL_BEFORE_TOP: u64 = 6;

/// Does the declared loop's taken back-edge cross a page? A crossing
/// branch takes 8 cycles instead of 7, breaking the fixed 21-dot grid,
/// so such loops are declared but never skipped (the machine refuses
/// to arm). `#171` may model the extra dummy-read cycle.
///
/// The branch's post-operand PC is `loop_pc + 5`; the taken jump back
/// to `loop_pc` crosses a page iff the high bytes differ.
pub(crate) fn backedge_crosses_page(loop_pc: u16) -> bool {
    loop_pc.wrapping_add(5) & 0xFF00 != loop_pc & 0xFF00
}

/// Ticket #171: would the parked iteration *after* this poll dot cover
/// the frame boundary — the VBL set at `(241,1)`?
///
/// `(scanline, dot)` is the PPU position *after* this poll dot's PPU
/// tick (the position the boundary-aware `$2002` read logic uses on
/// this same dot). The frame-boundary tick's CPU dot must never be
/// spent parked mid-iteration: the boundary snapshot would compare
/// the parked PC against the control's mid-iteration PC. After this
/// poll, the parked CPU dots run from `poll + 3` up to the next grid
/// poll at `poll + 21` (which replays — snapshot-exact). So the span
/// ends at *this* poll iff the boundary tick's CPU dot lands strictly
/// between the two, which for every frame parity reduces to the set
/// being at most 18 dots past the poll dot (`dist ∈ [0, 17]` after the
/// one-dot timing offset). The resume (`PC := branch_pc`) runs the
/// branch fetch at `poll + 3`, which covers every tight phase exactly —
/// the boundary dot becomes the branch fetch, the branch operand, or a
/// later real cycle, cycle-for-cycle identical to the control — and
/// the next iteration's real poll reads the fresh flag and exits the
/// loop through the fall-through.
///
/// Distances 18–20 leave the boundary tick's CPU dot exactly on the
/// next grid poll: parking one more iteration is exact — the replayed
/// read on that dot is the control's own read (the #168 race read at
/// distance 20 included), and the replay applies exactly the
/// control's cycle effects, so the boundary snapshot matches.
/// Positions at `(241,0)` and `(241,1)` are the two tightest
/// upcoming-set phases (the poll dot sits at `(241,339)`-relative and
/// `(241,0)`-relative clock positions; the frame parity decides which
/// one a replay can land on) — both must END the span. Positions
/// strictly past `(241,1)` mean the set already fired this frame;
/// the next one is a frame away.
pub(crate) fn next_parked_iteration_covers_vbl_set(scanline: u16, dot: u16) -> bool {
    if scanline > 241 || (scanline == 241 && dot > 1) {
        return false;
    }
    // Dots from just past this poll dot to the upcoming `(241,1)` set,
    // as one subtraction so a poll deep in scanline 240 borrows across
    // the line boundary instead of underflowing on `1 - dot`.
    let dist = 241 * 341 + 1 - (scanline as i32 * 341 + dot as i32);
    (0..=17).contains(&dist)
}

/// Ticket #171: would the *first parked iteration* of a freshly
/// declared span cover the frame boundary — the VBL set at `(241,1)`?
///
/// `(scanline, dot)` is the PPU position *after* the declaring
/// back-edge dot's PPU tick (the same offset convention as
/// [`next_parked_iteration_covers_vbl_set`]). A span declared now
/// parks its CPU from `anchor + 6` up to its first grid poll at
/// `anchor + 15`; the frame-boundary tick's CPU dot landing anywhere
/// in that pre-poll range would be spent parked mid-iteration and the
/// boundary snapshot would diverge from the control. This matters
/// most for the re-declare after a boundary-rule span end: the resume
/// branch's own taken back-edge fires a few dots before the set, and
/// without this gate the fresh span would park straight back through
/// the boundary tick.
///
/// The refusal window `dist ∈ [3, 13]` is the per-frame-parity union:
/// the boundary tick's CPU dot is `set`, `set + 1`, or `set + 2`
/// (whichever sits on the CPU's 3-dot phase), so the exact window
/// shifts by frame; the union over-covers a phase or two at each
/// edge, and an over-refusal is exact — the loop simply runs one
/// iteration for real and the next clean back-edge declares. Positions
/// at or past `(241,1)` mean the set already fired this frame; a span
/// parked now waits a whole frame for the next one.
pub(crate) fn declaring_backedge_parks_through_vbl_boundary(scanline: u16, dot: u16) -> bool {
    if scanline > 241 || (scanline == 241 && dot >= 1) {
        return false;
    }
    let dist = 241 * 341 + 1 - (scanline as i32 * 341 + dot as i32);
    (3..=13).contains(&dist)
}

/// The active quiet-span skip (ticket #170): the machine parks the CPU
/// at the loop top (a clean instruction boundary — PC set, empty
/// microcode queue) and replaces each poll iteration with the
/// counterfactual model until an edge event or the VBL boundary window
/// hands control back.
///
/// While parked, the machine keeps running its published per-dot
/// machinery (PPU, NMI line, IRQ level, APU, DMA) — only the CPU's
/// instruction stream is replaced. The grid is anchored at the
/// declaration's first back-edge (#165 Q3).
#[derive(Debug, Clone, Copy)]
pub(crate) struct QuietSkip {
    /// Facts of the declared loop, kept by value: the machine clears the
    /// declaration slot together with the skip.
    declaration: SpinDeclaration,
    /// First dot where the CPU is parked: iteration 1's loop top. The
    /// declaring iteration (0) runs real to the end, including its
    /// taken-cycle detection microcode.
    first_parked_dot: u64,
    /// Next grid loop-top dot awaiting an engage-or-wake decision.
    next_top_dot: u64,
    /// Next grid poll-read dot to replay counterfactually.
    next_poll_dot: u64,
    /// NMI line level when the span started (edge watchdog snapshot).
    span_nmi_line: bool,
    /// IRQ line level when the span started (level watchdog snapshot).
    span_irq_level: bool,
    /// Ticket #171: the dot the `/NMI` line first left its snapshot
    /// level, if it has. Recorded dot-exact (the machine watches the
    /// line every dot while a span is armed) because the wake rule
    /// turns on the two-dot sample lag of the branch-cycle poll.
    nmi_event_dot: Option<u64>,
    /// Ticket #171: the first CPU dot the IRQ level was observed to
    /// leave its snapshot level, if any. CPU-dot granularity suffices:
    /// the wake rule turns on the branch operand-cycle poll dot itself,
    /// and the level is read live at the decision.
    irq_event_dot: Option<u64>,
}

impl QuietSkip {
    /// Arm the skip for `declaration`. `nmi_line`/`irq_level` are the
    /// machine's current line levels — the span is quiet as long as
    /// they hold.
    pub(crate) fn new(declaration: SpinDeclaration, nmi_line: bool, irq_level: bool) -> Self {
        let first_parked_dot = declaration.anchor_dot + TOP_AFTER_ANCHOR_DOTS;
        Self {
            declaration,
            first_parked_dot,
            next_top_dot: first_parked_dot,
            next_poll_dot: first_parked_dot + POLL_AFTER_LOOP_TOP_DOTS,
            span_nmi_line: nmi_line,
            span_irq_level: irq_level,
            nmi_event_dot: None,
            irq_event_dot: None,
        }
    }

    pub(crate) fn declaration(&self) -> &SpinDeclaration {
        &self.declaration
    }

    /// The iteration whose loop top sits at `next_top_dot` was decided
    /// to stay counterfactual: schedule its poll and the next top
    /// decision.
    pub(crate) fn advance_top(&mut self) {
        self.next_poll_dot = self.next_top_dot + POLL_AFTER_LOOP_TOP_DOTS;
        self.next_top_dot += LOOP_PERIOD_DOTS;
    }

    pub(crate) fn first_parked_dot(&self) -> u64 {
        self.first_parked_dot
    }

    pub(crate) fn next_top_dot(&self) -> u64 {
        self.next_top_dot
    }

    pub(crate) fn next_poll_dot(&self) -> u64 {
        self.next_poll_dot
    }

    pub(crate) fn span_nmi_line(&self) -> bool {
        self.span_nmi_line
    }

    pub(crate) fn span_irq_level(&self) -> bool {
        self.span_irq_level
    }

    /// Ticket #171: record the dot the `/NMI` line first left its
    /// snapshot level. First observation wins — the wake rule keys on
    /// the first move only.
    pub(crate) fn record_nmi_event(&mut self, dot: u64) {
        if self.nmi_event_dot.is_none() {
            self.nmi_event_dot = Some(dot);
        }
    }

    /// Ticket #171: record the first CPU dot the IRQ level was observed
    /// to leave its snapshot level.
    pub(crate) fn record_irq_event(&mut self, dot: u64) {
        if self.irq_event_dot.is_none() {
            self.irq_event_dot = Some(dot);
        }
    }

    /// Ticket #171: which wake does the grid top at `next_top_dot` owe?
    /// `None` — the world stayed quiet, park the top and advance the
    /// grid. `Some(true)` — a line event already visible to the
    /// control's in-flight branch-cycle poll: wake and run the standard
    /// interrupt poll at the top first (the control's dispatch begins
    /// at the top; the late poll delivers the same dispatch dot).
    /// `Some(false)` — a later line event: plain wake, the real fetch
    /// from the top reproduces the control's next-poll dispatch.
    pub(crate) fn wake_kind_at_top(&self) -> Option<bool> {
        if self.nmi_event_dot.is_none() && self.irq_event_dot.is_none() {
            return None;
        }
        let top = self.next_top_dot;
        let nmi_seen_by_branch = self
            .nmi_event_dot
            .is_some_and(|dot| dot <= top - NMI_BRANCH_SAMPLE_BEFORE_TOP);
        let irq_seen_by_branch = self
            .irq_event_dot
            .is_some_and(|dot| dot <= top - IRQ_BRANCH_POLL_BEFORE_TOP);
        Some(nmi_seen_by_branch || irq_seen_by_branch)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A flat 64-KiB byte image with patches applied — the independent
    /// source of truth for the peek closure.
    fn mem_patched(patches: &[(u16, u8)]) -> Vec<u8> {
        let mut mem = vec![0u8; 0x10000];
        for &(addr, value) in patches {
            mem[addr as usize] = value;
        }
        mem
    }

    fn canonical_bit_loop(mem: &mut Vec<u8>, loop_pc: u16) {
        mem[loop_pc as usize] = BIT_ABS;
        mem[loop_pc.wrapping_add(1) as usize] = POLL_ADDR as u8;
        mem[loop_pc.wrapping_add(2) as usize] = (POLL_ADDR >> 8) as u8;
    }

    #[test]
    fn accepts_bit_abs_poll_loop_shape() {
        let loop_pc = 0x8020u16;
        let mut mem = mem_patched(&[]);
        canonical_bit_loop(&mut mem, loop_pc);
        let hit = backedge_shape_hit(
            |a| mem[a as usize],
            loop_pc + 3,
            BACKEDGE_TO_LOOP_TOP,
            BranchTest::IfNegativeClear,
        )
        .expect("BIT $2002 + back-edge branch must declare");
        assert_eq!(
            hit,
            SpinBackedgeHit {
                loop_pc,
                poll_opcode: BIT_ABS,
                branch_test: BranchTest::IfNegativeClear,
            }
        );
    }

    #[test]
    fn accepts_lda_abs_poll_loop_shape() {
        let loop_pc = 0xC000u16;
        let mut mem = mem_patched(&[]);
        mem[loop_pc as usize] = LDA_ABS;
        mem[loop_pc.wrapping_add(1) as usize] = POLL_ADDR as u8;
        mem[loop_pc.wrapping_add(2) as usize] = (POLL_ADDR >> 8) as u8;
        let hit = backedge_shape_hit(
            |a| mem[a as usize],
            loop_pc + 3,
            BACKEDGE_TO_LOOP_TOP,
            BranchTest::IfNegativeClear,
        )
        .expect("LDA $2002 + back-edge branch must declare");
        assert_eq!(
            hit,
            SpinBackedgeHit {
                loop_pc,
                poll_opcode: LDA_ABS,
                branch_test: BranchTest::IfNegativeClear,
            }
        );
    }

    #[test]
    fn refuses_lda_abs_indexed() {
        let loop_pc = 0x8020u16;
        let mut mem = mem_patched(&[]);
        mem[loop_pc as usize] = 0xBD; // LDA abs,X
        mem[loop_pc.wrapping_add(1) as usize] = POLL_ADDR as u8;
        mem[loop_pc.wrapping_add(2) as usize] = (POLL_ADDR >> 8) as u8;
        assert!(
            backedge_shape_hit(
                |a| mem[a as usize],
                loop_pc + 3,
                BACKEDGE_TO_LOOP_TOP,
                BranchTest::IfNegativeClear
            )
            .is_none()
        );
    }

    #[test]
    fn refuses_other_opcodes_at_the_loop_top() {
        for &opcode in &[0x2Du8, 0xA5, 0x4C, 0xAD ^ 0x10] {
            let loop_pc = 0x9000u16;
            let mem = mem_patched(&[
                (loop_pc, opcode),
                (loop_pc + 1, POLL_ADDR as u8),
                (loop_pc + 2, (POLL_ADDR >> 8) as u8),
            ]);
            assert!(
                backedge_shape_hit(
                    |a| mem[a as usize],
                    loop_pc + 3,
                    BACKEDGE_TO_LOOP_TOP,
                    BranchTest::IfNegativeClear
                )
                .is_none(),
                "opcode {opcode:#04x} must not declare"
            );
        }
    }

    #[test]
    fn refuses_non_2002_addresses() {
        for &(opcode, lo, hi) in &[
            (BIT_ABS, 0x01u8, 0x20u8),
            (BIT_ABS, 0x02, 0x21),
            (LDA_ABS, 0x12, 0x40),
        ] {
            let loop_pc = 0x8020u16;
            let mem = mem_patched(&[(loop_pc, opcode), (loop_pc + 1, lo), (loop_pc + 2, hi)]);
            assert!(
                backedge_shape_hit(
                    |a| mem[a as usize],
                    loop_pc + 3,
                    BACKEDGE_TO_LOOP_TOP,
                    BranchTest::IfNegativeClear
                )
                .is_none(),
                "{opcode:#04x} ${lo:02X}{hi:02X} must not declare"
            );
        }
    }

    #[test]
    fn refuses_forward_and_non_adjacent_offsets() {
        let loop_pc = 0x8020u16;
        let mem = mem_patched(&[]);
        // 0x00 (branch-to-next), 0x05 (forward), 0xFA (one byte between
        // read and branch), 0xFC (overshoots the loop top).
        for &offset in &[0x00u8, 0x05, 0xFA, 0xFC] {
            let mut mem = mem.clone();
            canonical_bit_loop(&mut mem, loop_pc);
            assert!(
                backedge_shape_hit(
                    |a| mem[a as usize],
                    loop_pc + 3,
                    offset,
                    BranchTest::IfNegativeClear
                )
                .is_none(),
                "offset {offset:#04X} must not declare"
            );
        }
    }

    #[test]
    fn accepts_only_the_fb_backedge() {
        let loop_pc = 0x8020u16;
        let mut mem = mem_patched(&[]);
        canonical_bit_loop(&mut mem, loop_pc);
        // Branch PC is fixed one instruction after the read; only the
        // $FB offset makes that branch jump back to the read itself.
        let branch_pc = loop_pc + 3;
        for offset in 0x00u8..=0xFF {
            let declared = backedge_shape_hit(
                |a| mem[a as usize],
                branch_pc,
                offset,
                BranchTest::IfNegativeClear,
            )
            .is_some();
            assert_eq!(
                declared,
                offset == BACKEDGE_TO_LOOP_TOP,
                "offset {offset:#04X} declared={declared}"
            );
        }
    }

    #[test]
    fn wraps_the_branch_arithmetic_at_the_address_bottom() {
        // branch_pc = $0000 with offset $FB targets $FFFD (6502 wrapping).
        let mem = mem_patched(&[
            (0xFFFD, BIT_ABS),
            (0xFFFE, POLL_ADDR as u8),
            (0xFFFF, (POLL_ADDR >> 8) as u8),
        ]);
        let hit = backedge_shape_hit(
            |a| mem[a as usize],
            0x0000,
            BACKEDGE_TO_LOOP_TOP,
            BranchTest::IfNegativeClear,
        )
        .expect("wrapped back-edge is still a back-edge");
        assert_eq!(
            hit,
            SpinBackedgeHit {
                loop_pc: 0xFFFD,
                poll_opcode: BIT_ABS,
                branch_test: BranchTest::IfNegativeClear,
            }
        );
    }

    #[test]
    fn declaration_captures_the_shape_facts() {
        let hit = SpinBackedgeHit {
            loop_pc: 0x8020,
            poll_opcode: BIT_ABS,
            branch_test: BranchTest::IfNegativeClear,
        };
        let decl = SpinDeclaration::new(hit, SystemClock(82190));
        assert_eq!(decl.loop_pc, 0x8020);
        assert_eq!(decl.poll_opcode, BIT_ABS);
        assert_eq!(decl.branch_pc, 0x8023);
        assert_eq!(decl.anchor_dot, 82190);
    }
}
