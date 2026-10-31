//! Opcode-walk harness at the `Mcu` trait boundary (spec #46, ticket #47).
//!
//! Every opcode's microcode sequence — plus the four interrupt sequences
//! (RESET, NMI, IRQ, BRK) — is executed against a [`RecordingMcu`] that
//! records every bus call, generic and fast-path alike. Two properties are
//! asserted:
//!
//! 1. **No generic access in RAM.** No generic `Mcu::read`/`Mcu::write`
//!    receives an address in `$0000-$01FF`: zero-page and stack-page
//!    traffic must take `read_zero_page`/`write_zero_page`/
//!    `read_stack_page`/`write_stack_page`. Known holes from the spec #46
//!    audit are listed in [`ALLOWED_MISSES`], grouped by the follow-up
//!    ticket that closes them.
//! 2. **The fast path was used.** For each audited already-correct group
//!    the matching fast-path counter must be non-zero, so a sequence that
//!    silently does nothing cannot satisfy property 1 by omission.
//!
//! Both are properties over the walk, not goldens: nothing here pins a
//! microcode shape, a private-helper call count, or an exact fast-path
//! call count.
//!
//! ## Fixed walk state
//!
//! Every walk starts from the same state, chosen so the two properties are
//! meaningful:
//!
//! | Item | Value | Why |
//! |------|-------|-----|
//! | PC | `$8000` | above the stack page, so instruction-stream fetches are not mistaken for mis-routed RAM accesses |
//! | memory filler | `$10` | every operand and pointer byte reads `$10`, so absolute/indirect targets land at `$1010-$1013`, above the stack page |
//! | A / X / Y | `$11` / `$11` / `$12` | non-zero indices keep `$zp,X`/`$zp,Y` and `(zp,X)`/`(zp),Y` distinguishable from their un-indexed base; the `SHX`/`SHY`/`SHA`/`TAS` store-address quirk forms its target as `value & (AH + 1)`, and these values keep that target at `$11xx` rather than folding it into RAM |
//! | SP | `$F0` | stack traffic lands at `$01F0-$01FE`, inside the protected range |
//! | status | `$20` | `NotUsed` set, all condition flags clear, interrupts enabled; no interrupt line is asserted during the opcode walk |
//! | address latch | `$1010` | left by the power-on RESET vector fetch; above the stack page |
//!
//! The invariant is scoped to this fixed state. A program with its PC or
//! latch inside RAM would legitimately violate property 1 — the CPU cannot
//! know that an absolute address computed from data is really zero page.
//!
//! The RESET walk re-queues the reset sequence after that state is applied
//! (`Cpu::reset` keeps PC), because power-on PC is `$0000`: letting the
//! first reset run its dead cycles at `$0000` would violate property 1 for
//! a reason the walk is not trying to detect.

use super::microcode::opcode;
use super::*;
use crate::test_utils::{GenericAccessKind, McuCall, RecordingMcu};
use core::ops::RangeInclusive;

/// Program counter every walk starts at — above the stack page.
const WALK_PC: u16 = 0x8000;
/// Uniform memory contents: operands and pointers are all this byte.
const WALK_FILLER: u8 = 0x10;
const WALK_A: u8 = 0x11;
const WALK_X: u8 = 0x11;
const WALK_Y: u8 = 0x12;
const WALK_SP: u8 = 0xF0;
/// `Flag::NotUsed` only: condition flags clear, interrupts enabled.
const WALK_STATUS: u8 = Flag::NotUsed as u8;

/// The address range the CPU must never reach through generic `read`/`write`.
const RAM_PAGE_RANGE: RangeInclusive<u16> = 0x0000..=0x01FF;

/// One bus call class the fast-path accounting recognizes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FastPath {
    ZeroPageRead,
    ZeroPageWrite,
    StackPageRead,
    StackPageWrite,
}

/// A group of opcodes already routed through the fast path, as audited in
/// spec #46 ("Already correct and not to be touched"). Each listed opcode
/// must make at least `min` calls of `fast_path` during its walk.
struct FastPathGroup {
    group: &'static str,
    fast_path: FastPath,
    /// Minimum calls; `2` is used only where hardware performs a dead read
    /// plus a real pull, so it pins the dead read, not a microcode shape.
    min: usize,
    opcodes: &'static [u8],
}

use FastPath::*;

/// The already-correct groups property 2 pins. A missing/renamed group or
/// an opcode dropping off the fast path fails here instead of passing
/// property 1 trivially.
const FAST_PATH_GROUPS: &[FastPathGroup] = &[
    FastPathGroup {
        group: "zero-page loads",
        fast_path: ZeroPageRead,
        min: 1,
        opcodes: &[
            opcode::LDA_ZERO_PAGE,
            opcode::LDA_ZERO_PAGE_X,
            opcode::LDX_ZERO_PAGE,
            opcode::LDX_ZERO_PAGE_Y,
            opcode::LDY_ZERO_PAGE,
            opcode::LDY_ZERO_PAGE_X,
        ],
    },
    FastPathGroup {
        group: "zero-page stores",
        fast_path: ZeroPageWrite,
        min: 1,
        opcodes: &[
            opcode::STA_ZERO_PAGE,
            opcode::STA_ZERO_PAGE_X,
            opcode::STX_ZERO_PAGE,
            opcode::STX_ZERO_PAGE_Y,
            opcode::STY_ZERO_PAGE,
            opcode::STY_ZERO_PAGE_X,
        ],
    },
    FastPathGroup {
        group: "zero-page ALU",
        fast_path: ZeroPageRead,
        min: 1,
        opcodes: &[
            opcode::ADC_ZERO_PAGE,
            opcode::ADC_ZERO_PAGE_X,
            opcode::SBC_ZERO_PAGE,
            opcode::SBC_ZERO_PAGE_X,
            opcode::AND_ZERO_PAGE,
            opcode::AND_ZERO_PAGE_X,
            opcode::ORA_ZERO_PAGE,
            opcode::ORA_ZERO_PAGE_X,
            opcode::EOR_ZERO_PAGE,
            opcode::EOR_ZERO_PAGE_X,
            opcode::CMP_ZERO_PAGE,
            opcode::CMP_ZERO_PAGE_X,
            opcode::CPX_ZERO_PAGE,
            opcode::CPY_ZERO_PAGE,
        ],
    },
    FastPathGroup {
        group: "BIT $zp",
        fast_path: ZeroPageRead,
        min: 1,
        opcodes: &[opcode::BIT_ZERO_PAGE],
    },
    FastPathGroup {
        // Every `$zp,X`/`$zp,Y` mode and every `(zp,X)` mode re-reads the
        // un-indexed base address while adding the index (hardware's dummy
        // read); that read is already on the zero-page fast path.
        group: "$zp,X / $zp,Y / (zp,X) index-add dummy reads",
        fast_path: ZeroPageRead,
        min: 1,
        opcodes: &[
            // $zp,X and $zp,Y addressing modes.
            opcode::NOP_ZERO_PAGE_X1,
            opcode::NOP_ZERO_PAGE_X2,
            opcode::NOP_ZERO_PAGE_X3,
            opcode::NOP_ZERO_PAGE_X4,
            opcode::NOP_ZERO_PAGE_X5,
            opcode::NOP_ZERO_PAGE_X6,
            opcode::ORA_ZERO_PAGE_X,
            opcode::AND_ZERO_PAGE_X,
            opcode::EOR_ZERO_PAGE_X,
            opcode::ADC_ZERO_PAGE_X,
            opcode::SBC_ZERO_PAGE_X,
            opcode::CMP_ZERO_PAGE_X,
            opcode::ASL_ZERO_PAGE_X,
            opcode::LSR_ZERO_PAGE_X,
            opcode::ROL_ZERO_PAGE_X,
            opcode::ROR_ZERO_PAGE_X,
            opcode::INC_ZERO_PAGE_X,
            opcode::DEC_ZERO_PAGE_X,
            opcode::DCP_ZERO_PAGE_X,
            opcode::ISC_ZERO_PAGE_X,
            opcode::RRA_ZERO_PAGE_X,
            opcode::RLA_ZERO_PAGE_X,
            opcode::SLO_ZERO_PAGE_X,
            opcode::SRE_ZERO_PAGE_X,
            opcode::STA_ZERO_PAGE_X,
            opcode::STX_ZERO_PAGE_Y,
            opcode::STY_ZERO_PAGE_X,
            opcode::SAX_ZERO_PAGE_Y,
            opcode::LDA_ZERO_PAGE_X,
            opcode::LDX_ZERO_PAGE_Y,
            opcode::LDY_ZERO_PAGE_X,
            opcode::LAX_ZERO_PAGE_Y,
            // (zp,X) addressing mode.
            opcode::ORA_INDEXED_INDIRECT,
            opcode::AND_INDEXED_INDIRECT,
            opcode::EOR_INDEXED_INDIRECT,
            opcode::ADC_INDEXED_INDIRECT,
            opcode::SBC_INDEXED_INDIRECT,
            opcode::CMP_INDEXED_INDIRECT,
            opcode::STA_INDEXED_INDIRECT,
            opcode::LDA_INDIRECT_INDEXED,
            opcode::SAX_INDEXED_INDIRECT,
            opcode::LAX_INDEXED_INDIRECT,
            opcode::SLO_INDEXED_INDIRECT,
            opcode::RLA_INDEXED_INDIRECT,
            opcode::SRE_INDEXED_INDIRECT,
            opcode::RRA_INDEXED_INDIRECT,
            opcode::DCP_INDEXED_INDIRECT,
            opcode::ISC_INDEXED_INDIRECT,
        ],
    },
    FastPathGroup {
        group: "NOP $zp / NOP $zp,X",
        fast_path: ZeroPageRead,
        min: 1,
        opcodes: &[
            opcode::NOP_ZERO_PAGE1,
            opcode::NOP_ZERO_PAGE2,
            opcode::NOP_ZERO_PAGE3,
            opcode::NOP_ZERO_PAGE_X1,
            opcode::NOP_ZERO_PAGE_X2,
            opcode::NOP_ZERO_PAGE_X3,
            opcode::NOP_ZERO_PAGE_X4,
            opcode::NOP_ZERO_PAGE_X5,
            opcode::NOP_ZERO_PAGE_X6,
        ],
    },
    FastPathGroup {
        // An RMW writes twice: the old value back, then the modified value
        // (spec #46, ticket #48). `min: 2` therefore pins the second, real
        // write — with only the dummy write routed this counts 1.
        group: "zero-page RMW real write ($zp / $zp,X)",
        fast_path: ZeroPageWrite,
        min: 2,
        opcodes: &[
            opcode::ASL_ZERO_PAGE,
            opcode::ASL_ZERO_PAGE_X,
            opcode::LSR_ZERO_PAGE,
            opcode::LSR_ZERO_PAGE_X,
            opcode::ROL_ZERO_PAGE,
            opcode::ROL_ZERO_PAGE_X,
            opcode::ROR_ZERO_PAGE,
            opcode::ROR_ZERO_PAGE_X,
            opcode::INC_ZERO_PAGE,
            opcode::INC_ZERO_PAGE_X,
            opcode::DEC_ZERO_PAGE,
            opcode::DEC_ZERO_PAGE_X,
        ],
    },
    FastPathGroup {
        // Both pointer bytes land in the zero page by construction for
        // these modes, so each sequence reads two bytes at $00xx on the
        // fast path. `(zp,X)` additionally re-reads the index-add base
        // address, which the group above already pins.
        group: "(zp,X) / (zp),Y pointer fetches",
        fast_path: ZeroPageRead,
        min: 2,
        opcodes: &[
            // (zp,X)
            opcode::ORA_INDEXED_INDIRECT,
            opcode::AND_INDEXED_INDIRECT,
            opcode::EOR_INDEXED_INDIRECT,
            opcode::ADC_INDEXED_INDIRECT,
            opcode::SBC_INDEXED_INDIRECT,
            opcode::CMP_INDEXED_INDIRECT,
            opcode::STA_INDEXED_INDIRECT,
            opcode::LDA_INDIRECT_INDEXED,
            opcode::SAX_INDEXED_INDIRECT,
            opcode::LAX_INDEXED_INDIRECT,
            opcode::SLO_INDEXED_INDIRECT,
            opcode::RLA_INDEXED_INDIRECT,
            opcode::SRE_INDEXED_INDIRECT,
            opcode::RRA_INDEXED_INDIRECT,
            opcode::DCP_INDEXED_INDIRECT,
            opcode::ISC_INDEXED_INDIRECT,
            // (zp),Y
            opcode::ORA_INDIRECT_INDEXED,
            opcode::AND_INDIRECT_INDEXED,
            opcode::EOR_INDIRECT_INDEXED,
            opcode::ADC_INDIRECT_INDEXED,
            opcode::SBC_INDIRECT_INDEXED,
            opcode::CMP_INDIRECT_INDEXED,
            opcode::STA_INDIRECT_INDEXED_Y,
            opcode::LDA_INDIRECT_INDEXED_Y,
            opcode::SHA_INDIRECT_INDEXED_Y,
            opcode::LAX_INDIRECT_INDEXED,
            opcode::SLO_INDIRECT_INDEXED,
            opcode::RLA_INDIRECT_INDEXED,
            opcode::SRE_INDIRECT_INDEXED,
            opcode::RRA_INDIRECT_INDEXED,
            opcode::DCP_INDIRECT_INDEXED,
            opcode::ISC_INDIRECT_INDEXED,
        ],
    },
    FastPathGroup {
        group: "stack pushes (PHA / PHP / JSR)",
        fast_path: StackPageWrite,
        min: 1,
        opcodes: &[opcode::PHA, opcode::PHP, opcode::JSR],
    },
    FastPathGroup {
        group: "stack pulls (PLA / PLP / RTS / RTI)",
        fast_path: StackPageRead,
        min: 1,
        opcodes: &[opcode::PLA, opcode::PLP, opcode::RTS, opcode::RTI],
    },
    FastPathGroup {
        // RTS/RTI/PLP read $0100 | SP once before pulling (hardware
        // pre-increments SP on that cycle), so each makes at least two
        // stack-page fast reads: the dead read plus the real pull(s).
        group: "RTS / RTI / PLP dead stack read",
        fast_path: StackPageRead,
        min: 2,
        opcodes: &[opcode::RTS, opcode::RTI, opcode::PLP],
    },
];

/// One group of opcodes the spec #46 audit found still routing a zero-page
/// access through the generic `read`/`write` path.
///
/// Each entry names the follow-up ticket that deletes it. The walk fails if
/// a listed opcode no longer misses, so an entry cannot outlive its fix.
struct AllowedMiss {
    /// Follow-up ticket that routes this group through the fast path.
    ticket: u32,
    group: &'static str,
    /// The generic call class the group is known to make.
    kind: GenericAccessKind,
    opcodes: &'static [u8],
}

/// The documented allow-list: exactly the 60 opcodes the spec #46 audit
/// measured as mis-routed, grouped per follow-up ticket.
///
/// Tickets #48 (12 opcodes), #51 (32), #49 (12 + 2) and #50 (2) have all
/// deleted their entries, leaving only the #49 and #50 groups below.
const ALLOWED_MISSES: &[AllowedMiss] = &[
    AllowedMiss {
        ticket: 49,
        group: "undocumented zero-page RMW second write ($zp/$zp,X)",
        kind: GenericAccessKind::Write,
        opcodes: &[
            opcode::DCP_ZERO_PAGE,
            opcode::DCP_ZERO_PAGE_X,
            opcode::ISC_ZERO_PAGE,
            opcode::ISC_ZERO_PAGE_X,
            opcode::RRA_ZERO_PAGE,
            opcode::RRA_ZERO_PAGE_X,
            opcode::RLA_ZERO_PAGE,
            opcode::RLA_ZERO_PAGE_X,
            opcode::SLO_ZERO_PAGE,
            opcode::SLO_ZERO_PAGE_X,
            opcode::SRE_ZERO_PAGE,
            opcode::SRE_ZERO_PAGE_X,
        ],
    },
    AllowedMiss {
        ticket: 49,
        group: "SAX $zp / SAX $zp,Y",
        kind: GenericAccessKind::Write,
        opcodes: &[opcode::SAX_ZERO_PAGE, opcode::SAX_ZERO_PAGE_Y],
    },
    AllowedMiss {
        ticket: 50,
        group: "LAX $zp / LAX $zp,Y",
        kind: GenericAccessKind::Read,
        opcodes: &[opcode::LAX_ZERO_PAGE, opcode::LAX_ZERO_PAGE_Y],
    },
];

/// The recorded bus activity of one executed sequence.
struct Walk {
    /// Human-readable name used in failure messages.
    name: String,
    /// The opcode, or `None` for a sequence entered without an opcode fetch.
    opcode: Option<u8>,
    calls: Vec<McuCall>,
}

impl Walk {
    fn fast_count(&self, fast_path: FastPath) -> usize {
        self.calls
            .iter()
            .filter(|call| match fast_path {
                FastPath::ZeroPageRead => call.is_read_zero_page(),
                FastPath::ZeroPageWrite => call.is_write_zero_page(),
                FastPath::StackPageRead => call.is_read_stack_page(),
                FastPath::StackPageWrite => call.is_write_stack_page(),
            })
            .count()
    }

    /// Generic `read`/`write` calls that landed in the protected RAM range.
    fn generic_ram_misses(&self) -> Vec<(GenericAccessKind, u16)> {
        self.calls
            .iter()
            .filter_map(|call| {
                let kind = call.generic()?;
                let addr = call.address();
                RAM_PAGE_RANGE.contains(&addr).then_some((kind, addr))
            })
            .collect()
    }
}

/// Applies the documented fixed register state and leaves the queue empty.
fn new_walk_cpu(program: &[u8]) -> Cpu<RecordingMcu> {
    new_walk_cpu_with(RecordingMcu::filled(WALK_FILLER).with_program(WALK_PC, program))
}

/// As [`new_walk_cpu`], over a caller-provided `Mcu` whose memory the test
/// controls.
fn new_walk_cpu_with(mcu: RecordingMcu) -> Cpu<RecordingMcu> {
    let mut cpu = Cpu::new(mcu);
    let mut plugin = EmptyPlugin::new();
    let mut clock = SystemClock::default();
    // Drain the power-on RESET sequence; the walk starts at a boundary.
    cpu.run_to_instruction_boundary(&mut plugin, &mut clock);
    cpu.set_a(WALK_A);
    cpu.set_x(WALK_X);
    cpu.set_y(WALK_Y);
    cpu.set_sp(WALK_SP);
    cpu.set_status(WALK_STATUS);
    cpu.set_pc(WALK_PC);
    cpu
}

/// Fetches the opcode at `WALK_PC` and drains exactly its sequence.
fn run_fetched_sequence(cpu: &mut Cpu<RecordingMcu>) {
    let mut plugin = EmptyPlugin::new();
    let mut clock = SystemClock::default();
    // The queue is empty: this tick fetches the opcode and queues its
    // sequence (`Microcode::FetchAndDecode`).
    cpu.tick(&mut plugin, clock);
    while !cpu.microcodes_empty() {
        clock = clock.inc();
        cpu.tick(&mut plugin, clock);
    }
}

fn walk_opcode(op: u8) -> Walk {
    let mut cpu = new_walk_cpu(&[op]);
    cpu.mcu().clear_calls();
    run_fetched_sequence(&mut cpu);
    Walk {
        name: format!("opcode 0x{op:02X}"),
        opcode: Some(op),
        calls: cpu.mcu().calls(),
    }
}

fn walk_all_opcodes() -> Vec<Walk> {
    (0..=u8::MAX).map(walk_opcode).collect()
}

/// Re-runs the reset sequence from the documented state (power-on PC is
/// `$0000`, which would otherwise put the six dead cycles in RAM).
fn walk_reset() -> Walk {
    let mut cpu = new_walk_cpu(&[]);
    cpu.mcu().clear_calls();
    cpu.reset(); // keeps PC, re-queues the RESET sequence
    let mut plugin = EmptyPlugin::new();
    let mut clock = SystemClock::default();
    while !cpu.microcodes_empty() {
        cpu.tick(&mut plugin, clock);
        clock = clock.inc();
    }
    Walk {
        name: "RESET sequence".to_string(),
        opcode: None,
        calls: cpu.mcu().calls(),
    }
}

/// Runs a NOP and drives `/NMI` high until the CPU dispatches the NMI
/// sequence, then drains that sequence. The recording starts on the
/// dispatch tick (the sequence's first, dead-PC cycle).
fn walk_nmi() -> Walk {
    let mut cpu = new_walk_cpu(&[opcode::NOP]);
    let mut plugin = EmptyPlugin::new();
    let mut clock = SystemClock::default();
    let mut dispatched = false;
    for _ in 0..64 {
        cpu.update_nmi_line(true, clock);
        if clock.is_cpu_clock() {
            if cpu.microcodes_empty() && cpu.interrupt_detected.is_some() {
                cpu.mcu().clear_calls();
                cpu.tick(&mut plugin, clock);
                dispatched = true;
                break;
            }
            cpu.tick(&mut plugin, clock);
        }
        clock = clock.inc();
    }
    assert!(dispatched, "NMI was never dispatched");
    while !cpu.microcodes_empty() {
        clock = clock.inc();
        cpu.tick(&mut plugin, clock);
    }
    Walk {
        name: "NMI sequence".to_string(),
        opcode: None,
        calls: cpu.mcu().calls(),
    }
}

/// As [`walk_nmi`], with the IRQ line asserted instead.
fn walk_irq() -> Walk {
    let mut cpu = new_walk_cpu(&[opcode::NOP]);
    cpu.set_irq(true);
    let mut plugin = EmptyPlugin::new();
    let mut clock = SystemClock::default();
    let mut dispatched = false;
    for _ in 0..64 {
        if clock.is_cpu_clock() {
            if cpu.microcodes_empty() && cpu.interrupt_detected.is_some() {
                cpu.mcu().clear_calls();
                cpu.tick(&mut plugin, clock);
                dispatched = true;
                break;
            }
            cpu.tick(&mut plugin, clock);
        }
        clock = clock.inc();
    }
    assert!(dispatched, "IRQ was never dispatched");
    while !cpu.microcodes_empty() {
        clock = clock.inc();
        cpu.tick(&mut plugin, clock);
    }
    Walk {
        name: "IRQ sequence".to_string(),
        opcode: None,
        calls: cpu.mcu().calls(),
    }
}

fn walk_brk() -> Walk {
    let mut walk = walk_opcode(opcode::BRK);
    walk.name = "BRK sequence (opcode 0x00)".to_string();
    walk.opcode = None;
    walk
}

/// Property 1, applied to one walk.
fn assert_no_unlisted_generic_ram_access(walk: &Walk) {
    for (kind, addr) in walk.generic_ram_misses() {
        let listed = ALLOWED_MISSES.iter().any(|entry| {
            entry.kind == kind && walk.opcode.is_some_and(|op| entry.opcodes.contains(&op))
        });
        assert!(
            listed,
            "{}: generic {:?} at {addr:#06X} is inside $0000-$01FF. \
             Zero-page and stack-page accesses must take the Mcu fast path \
             (read_zero_page/write_zero_page/read_stack_page/write_stack_page); \
             if this is a known audit hole, add the opcode to ALLOWED_MISSES \
             under its follow-up ticket.",
            walk.name, kind
        );
    }
}

/// Keeps [`ALLOWED_MISSES`] honest: an entry whose opcodes no longer miss
/// is stale and must be deleted by the ticket that fixed the routing.
fn assert_allow_list_is_still_accurate(walks: &[Walk]) {
    for entry in ALLOWED_MISSES {
        for &op in entry.opcodes {
            let walk = walks
                .iter()
                .find(|walk| walk.opcode == Some(op))
                .unwrap_or_else(|| panic!("no walk for allow-listed opcode {op:#04X}"));
            let still_misses = walk
                .generic_ram_misses()
                .iter()
                .any(|&(kind, _)| kind == entry.kind);
            assert!(
                still_misses,
                "allow-list entry for ticket #{} (\"{}\") is stale for {}: \
                 it no longer sends a generic {:?} into $0000-$01FF. \
                 Delete the opcode from ALLOWED_MISSES (and the entry once empty).",
                entry.ticket, entry.group, walk.name, entry.kind
            );
        }
    }
}

#[test]
fn no_generic_access_in_zero_page_or_stack_page() {
    let walks = walk_all_opcodes();
    for walk in &walks {
        assert_no_unlisted_generic_ram_access(walk);
    }
    assert_allow_list_is_still_accurate(&walks);
}

#[test]
fn already_routed_groups_use_the_fast_path() {
    let walks = walk_all_opcodes();
    for group in FAST_PATH_GROUPS {
        for &op in group.opcodes {
            let walk = walks
                .iter()
                .find(|walk| walk.opcode == Some(op))
                .unwrap_or_else(|| panic!("no walk for opcode {op:#04X}"));
            let count = walk.fast_count(group.fast_path);
            assert!(
                count >= group.min,
                "{}: group \"{}\" expects at least {} {:?} call(s), saw {count}. \
                 The opcode is no longer on the fast path (or the sequence did not run).",
                walk.name,
                group.group,
                group.min,
                group.fast_path,
            );
        }
    }
}

#[test]
fn interrupt_sequences_route_their_stack_accesses() {
    // (walk, low vector address, high vector address, stacks an entry)
    let walks = [
        (walk_reset(), 0xFFFC, 0xFFFD, false),
        (walk_nmi(), 0xFFFA, 0xFFFB, true),
        (walk_irq(), 0xFFFE, 0xFFFF, true),
        (walk_brk(), 0xFFFE, 0xFFFF, true),
    ];
    for (walk, vector_low, vector_high, stacks) in walks {
        assert_no_unlisted_generic_ram_access(&walk);

        // Prove the sequence actually ran: an empty or aborted sequence
        // cannot satisfy property 1 by doing nothing.
        for vector in [vector_low, vector_high] {
            assert!(
                walk.calls
                    .iter()
                    .any(|call| matches!(call, McuCall::Read { addr, .. } if *addr == vector)),
                "{}: expected a bus read of the vector at {vector:#06X}",
                walk.name
            );
        }

        if stacks {
            // NMI, IRQ and BRK stack PCH, PCL and P before the vector
            // fetch; all three pushes are on the stack fast path.
            assert!(
                walk.fast_count(FastPath::StackPageWrite) >= 1,
                "{}: interrupt entry pushes must use write_stack_page; \
                 saw no stack-page fast write",
                walk.name
            );
        }
    }
}

/// Runs `program` from the documented walk state over a memory image the
/// test controls, and returns the drained walk.
fn walk_program(mcu: RecordingMcu) -> Walk {
    let mut cpu = new_walk_cpu_with(mcu);
    cpu.set_y(0); // keep `(zp),Y` targets a function of the pointer alone
    cpu.mcu().clear_calls();
    run_fetched_sequence(&mut cpu);
    Walk {
        name: "program".to_string(),
        opcode: None,
        calls: cpu.mcu().calls(),
    }
}

/// `(zp),Y` fetches its pointer high byte from the *un-carried* low-byte
/// increment: a pointer low byte of `$FF` wraps to `$00`, still inside the
/// zero page, and the fast-path call must address that wrapped offset.
#[test]
fn indirect_indexed_high_byte_wraps_without_carry_inside_zero_page() {
    let walk = walk_program(
        RecordingMcu::filled(WALK_FILLER)
            .with_program(WALK_PC, &[opcode::LDA_INDIRECT_INDEXED_Y, 0xFF])
            .with_program(0x00FF, &[0x34])
            .with_program(0x0000, &[0x12])
            .with_program(0x1234, &[0xA5]),
    );

    for offset in [0xFFu8, 0x00] {
        assert!(
            walk.calls
                .iter()
                .any(|call| matches!(call, McuCall::ReadZeroPage { addr, .. } if *addr == offset)),
            "expected a zero-page fast read of the pointer byte at ${offset:02X} (wrapped low \
             byte), saw {:?}",
            walk.calls
        );
    }
    // The wrap must not escape into the stack page: nothing reads $0100.
    assert!(
        !walk
            .calls
            .iter()
            .any(|call| call.address() == 0x0100 || call.address() == 0x0101),
        "pointer high-byte fetch with a $FF low byte escaped the zero page: {:?}",
        walk.calls
    );
}

/// `JMP ($xxxx)`'s pointer is a program-chosen address, not an
/// addressing-mode guarantee: a legal `JMP ($00FF)` keeps reading its
/// pointer through the generic path (and wraps its high byte to `$0000`).
#[test]
fn indirect_jmp_keeps_reading_its_pointer_through_the_generic_path() {
    let walk = walk_program(
        RecordingMcu::filled(WALK_FILLER)
            .with_program(WALK_PC, &[opcode::JMP_INDIRECT, 0xFF, 0x00])
            .with_program(0x00FF, &[0x34])
            .with_program(0x0000, &[0x12]),
    );

    for addr in [0x00FFu16, 0x0000] {
        assert!(
            walk.calls
                .iter()
                .any(|call| matches!(call, McuCall::Read { addr: a, .. } if *a == addr)),
            "JMP ($00FF) must read its pointer at {addr:#06X} through the generic path, \
             saw {:?}",
            walk.calls
        );
    }
    assert!(
        !walk.calls.iter().any(|call| call.is_read_zero_page()),
        "JMP ($00FF) must not take the zero-page fast path, saw {:?}",
        walk.calls
    );
}
