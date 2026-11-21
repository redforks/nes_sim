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

use crate::SystemClock;
use crate::bus::Bus;
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
}

/// A declared spin-wait poll loop — the hand-off state ticket #170's
/// quiet-span skip consumes. Captured once per loop (keep-first), at
/// the loop's first back-edge.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SpinDeclaration {
    /// PC of the poll instruction at the loop top.
    pub(crate) loop_pc: u16,
    /// Where the back-edge branch jumps to. Equal to `loop_pc` by the
    /// accepted shape; kept explicit because #170's wake path keys on
    /// the jump target.
    pub(crate) back_edge_target: u16,
    /// The poll opcode ($2C or $AD).
    pub(crate) poll_opcode: u8,
    /// The poll instruction's operand bytes (the $2002 address).
    pub(crate) poll_operands: [u8; 2],
    /// PC of the back-edge branch instruction (`loop_pc + 3` by shape).
    pub(crate) branch_pc: u16,
    /// Absolute dot clock of the declaring back-edge — the first
    /// back-edge of this loop (#165 Q3). #170's quiet-span grid anchors
    /// here.
    pub(crate) anchor_dot: u64,
}

impl SpinDeclaration {
    /// Capture a declared loop from a shape hit at `clock` (the dot the
    /// declaring back-edge executed on — bus-owned, ADR 0018).
    pub(crate) fn new(hit: SpinBackedgeHit, clock: SystemClock) -> Self {
        SpinDeclaration {
            loop_pc: hit.loop_pc,
            // The branch jumps back to the read instruction, and the read
            // instruction sits at the loop top — the accepted shape
            // makes these the same PC (asserted by the shape matcher).
            back_edge_target: hit.loop_pc,
            poll_opcode: hit.poll_opcode,
            poll_operands: [POLL_ADDR as u8, (POLL_ADDR >> 8) as u8],
            branch_pc: hit.loop_pc.wrapping_add(3),
            anchor_dot: clock.cycles(),
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
    })
}

/// [`backedge_shape_hit`] over the CPU's bus: peeks program bytes
/// through the memory map without disturbing it.
pub(crate) fn backedge_shape_hit_at_bus<M: Mcu>(
    bus: &Bus<M>,
    branch_pc: u16,
    offset: u8,
) -> Option<SpinBackedgeHit> {
    backedge_shape_hit(|addr| bus.mcu().peek(addr), branch_pc, offset)
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
        let hit = backedge_shape_hit(|a| mem[a as usize], loop_pc + 3, BACKEDGE_TO_LOOP_TOP)
            .expect("BIT $2002 + back-edge branch must declare");
        assert_eq!(
            hit,
            SpinBackedgeHit {
                loop_pc,
                poll_opcode: BIT_ABS
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
        let hit = backedge_shape_hit(|a| mem[a as usize], loop_pc + 3, BACKEDGE_TO_LOOP_TOP)
            .expect("LDA $2002 + back-edge branch must declare");
        assert_eq!(
            hit,
            SpinBackedgeHit {
                loop_pc,
                poll_opcode: LDA_ABS
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
            backedge_shape_hit(|a| mem[a as usize], loop_pc + 3, BACKEDGE_TO_LOOP_TOP).is_none()
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
                backedge_shape_hit(|a| mem[a as usize], loop_pc + 3, BACKEDGE_TO_LOOP_TOP)
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
                backedge_shape_hit(|a| mem[a as usize], loop_pc + 3, BACKEDGE_TO_LOOP_TOP)
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
                backedge_shape_hit(|a| mem[a as usize], loop_pc + 3, offset).is_none(),
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
            let declared = backedge_shape_hit(|a| mem[a as usize], branch_pc, offset).is_some();
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
        let hit = backedge_shape_hit(|a| mem[a as usize], 0x0000, BACKEDGE_TO_LOOP_TOP)
            .expect("wrapped back-edge is still a back-edge");
        assert_eq!(
            hit,
            SpinBackedgeHit {
                loop_pc: 0xFFFD,
                poll_opcode: BIT_ABS
            }
        );
    }

    #[test]
    fn declaration_captures_the_shape_facts() {
        let hit = SpinBackedgeHit {
            loop_pc: 0x8020,
            poll_opcode: BIT_ABS,
        };
        let decl = SpinDeclaration::new(hit, SystemClock(82190));
        assert_eq!(decl.loop_pc, 0x8020);
        assert_eq!(decl.back_edge_target, 0x8020);
        assert_eq!(decl.poll_opcode, BIT_ABS);
        assert_eq!(decl.poll_operands, [0x02, 0x20]);
        assert_eq!(decl.branch_pc, 0x8023);
        assert_eq!(decl.anchor_dot, 82190);
    }
}
