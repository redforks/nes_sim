use super::{Cpu, Flag, Register, ValueSource};
use crate::bus::Bus;
use crate::cpu::{Alu, Immediate, Mem, ValueSourceTrait, ValueTargetTrait, ZeroPage};
use crate::mcu::Mcu;
use tinyvec::ArrayVec;

/// Pad a micro-op list to the seven-slot sequence buffer.
const fn seq<const N: usize>(items: [Microcode; N]) -> ArrayVec<[Microcode; 7]> {
    let mut buf = [Microcode::Nop; 7];
    let mut i = 0;
    while i < N {
        buf[i] = items[i];
        i += 1;
    }
    match ArrayVec::try_from_array_len(buf, N) {
        Ok(v) => v,
        Err(_) => panic!(),
    }
}

macro_rules! microcode_arr {
    ($($item:expr),* $(,)?) => {
        seq([$($item),*])
    };
}

const fn zero_page_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(Microcode::ZeroPage, op)
}

const fn zero_page_x_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(Microcode::ZeroPage, Microcode::ZeroPageIndexedX, op)
}

const fn zero_page_y_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(Microcode::ZeroPage, Microcode::ZeroPageIndexedY, op)
}

const fn absolute_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(Microcode::AbsoluteL, Microcode::AbsoluteH, op)
}

const fn absolute_indexed_x_op(indexed: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(Microcode::AbsoluteL, Microcode::AbsoluteH, indexed)
}

const fn absolute_indexed_y_op(indexed: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(Microcode::AbsoluteL, Microcode::AbsoluteH, indexed)
}

const fn indexed_indirect_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::ZeroPageIndexedX,
        Microcode::IndexedLZeroPage,
        Microcode::IndexedHZeroPage,
        op
    )
}

const fn indirect_indexed_op(indexed: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::IndexedLZeroPage,
        Microcode::IndexedHZeroPage,
        indexed
    )
}

const fn zero_page_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::LoadIntoAluZeroPage,
        Microcode::StoreAluZeroPage,
        op
    )
}

const fn zero_page_x_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::ZeroPageIndexedX,
        Microcode::LoadIntoAluZeroPage,
        Microcode::StoreAluZeroPage,
        op
    )
}

const fn absolute_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH,
        Microcode::LoadIntoAluMem,
        Microcode::StoreAluMem,
        op
    )
}

const fn absolute_indexed_x_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH,
        Microcode::IndexedXLoadAluAlways,
        Microcode::LoadIntoAluMem,
        Microcode::StoreAluMem,
        op
    )
}

const fn absolute_indexed_y_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH,
        Microcode::IndexedYLoadAluAlways,
        Microcode::LoadIntoAluMem,
        Microcode::StoreAluMem,
        op
    )
}

const fn indexed_indirect_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::ZeroPageIndexedX,
        Microcode::IndexedLZeroPage,
        Microcode::IndexedHZeroPage,
        Microcode::LoadIntoAluMem,
        Microcode::StoreAluMem,
        op
    )
}

const fn indirect_indexed_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::IndexedLZeroPage,
        Microcode::IndexedHZeroPage,
        Microcode::IndexedYLoadAluAlways,
        Microcode::LoadIntoAluMem,
        Microcode::StoreAluMem,
        op
    )
}

const fn absolute_indexed_x_store_a() -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH,
        Microcode::IndexedXStoreAAlways,
        Microcode::StoreAMem
    )
}

const fn absolute_indexed_y_store_a() -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH,
        Microcode::IndexedYStoreAAlways,
        Microcode::StoreAMem
    )
}

const fn indirect_indexed_store_a() -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::IndexedLZeroPage,
        Microcode::IndexedHZeroPage,
        Microcode::IndexedYStoreAAlways,
        Microcode::StoreAMem
    )
}

/// One NMI-hijack site declared by an [`InterruptWindow`]: the micro-op
/// index that performs the check and the Hijack deadline — an /NMI rise at
/// or before `sequence_start + deadline_dots` is eligible there.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HijackSite {
    pub op_index: u8,
    pub deadline_dots: u8,
}

/// The interrupt recognition window of one interrupt sequence, declared
/// next to its micro-ops (ADR-0006): the sequence's cycle count, whether
/// its final cycle suppresses the standard end-of-instruction interrupt
/// poll, and its NMI-hijack sites with deadlines in dots from sequence
/// start (the cycle executing micro-op 0).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InterruptWindow {
    #[cfg(any(debug_assertions, test))]
    pub sequence_cycles: u8,
    pub hijack_sites: &'static [HijackSite],
}

pub struct InterruptSequences;

impl InterruptSequences {
    /// Reset entry, one cycle longer than the generic 6502's seven-cycle
    /// sequence: Mesen2 models the NES/2A03 as taking eight CPU cycles after
    /// reset/power-up before the ROM's code runs. That extra cycle is the
    /// boot-alignment phase the APU frame counter and the CPU/APU divide-by-2
    /// phase are measured from; without it `$4017` effects land on the wrong
    /// cycle and the non-`sync_apu` length-counter probes (imported forum
    /// tests, test_apu_2) disagree with cpu_interrupts_v2 `sync_apu`.
    pub const RESET: [Microcode; 8] = [
        Microcode::Nop,
        Microcode::Nop,
        Microcode::Nop,
        Microcode::Nop,
        Microcode::Nop,
        Microcode::Nop,
        Microcode::LoadResetPcL,
        Microcode::LoadResetPcH,
    ];

    /// Hardware interrupt entry, exactly seven CPU cycles: two dead reads
    /// of PC (T1/T2 — the 6502's internal interrupt-sequence setup; the
    /// bus read happens but the fetched byte is discarded), then three
    /// stack pushes and the two vector fetches. Both dead cycles belong
    /// to the sequence itself; the dispatcher only hands back the
    /// sequence's head (T1).
    pub const NMI: [Microcode; 7] = [
        Microcode::FetchOnly, // T1: dead read of PC
        Microcode::FetchOnly, // T2: dead read of PC
        Microcode::PushPch,
        Microcode::PushPcl,
        Microcode::PushStatusBreakClear,
        Microcode::LoadNmiPcL,
        Microcode::LoadNmiPcH,
    ];

    pub const IRQ: [Microcode; 7] = [
        Microcode::FetchOnly, // T1: dead read of PC
        Microcode::FetchOnly, // T2: dead read of PC
        Microcode::PushPch,
        Microcode::PushPcl,
        Microcode::PushStatusBreakClear,
        Microcode::LoadIrqPcL,
        Microcode::LoadIrqPcH,
    ];

    pub const BRK: ArrayVec<[Microcode; 7]> = microcode_arr![
        Microcode::SkipImmediate,
        Microcode::PushPch,
        Microcode::PushPcl,
        Microcode::PushStatusBreakSet,
        Microcode::LoadIrqPcL,
        Microcode::LoadIrqPcH
    ];

    /// Interrupt recognition windows (ADR-0006), declared next to the
    /// sequences they schedule. Deadlines are dots from sequence start —
    /// the cycle executing micro-op 0 — and values are captured from
    /// blargg-green behavior (cpu_interrupts_v2):
    /// - the vector-fetch site's cutoff is the hardware decision boundary
    ///   (`site_tick − lag`, lags 5/7 pinned by blargg tests 2 and 3);
    /// - the PushStatus site consumes sampler-latched edges only: a rise
    ///   is latched strictly before the take tick iff it rose by
    ///   `site_tick − 5` (2-dot sample lag plus one full cycle, see
    ///   `NmiDetector` sampling).
    pub const NMI_WINDOW: InterruptWindow = InterruptWindow {
        #[cfg(any(debug_assertions, test))]
        sequence_cycles: 7,
        hijack_sites: &[],
    };
    pub const IRQ_WINDOW: InterruptWindow = InterruptWindow {
        #[cfg(any(debug_assertions, test))]
        sequence_cycles: 7,
        hijack_sites: &[
            HijackSite {
                op_index: 4,
                deadline_dots: 7,
            },
            HijackSite {
                op_index: 5,
                deadline_dots: 10,
            },
        ],
    };
    pub const BRK_WINDOW: InterruptWindow = InterruptWindow {
        #[cfg(any(debug_assertions, test))]
        sequence_cycles: 6,
        hijack_sites: &[
            HijackSite {
                op_index: 3,
                deadline_dots: 4,
            },
            HijackSite {
                op_index: 4,
                deadline_dots: 5,
            },
        ],
    };
    pub const RESET_WINDOW: InterruptWindow = InterruptWindow {
        #[cfg(any(debug_assertions, test))]
        sequence_cycles: 8,
        hijack_sites: &[],
    };

    /// The recognition window declared for a fetched opcode's sequence —
    /// only BRK is an interrupt sequence in the opcode table.
    pub const fn window_for_opcode(op: u8) -> Option<&'static InterruptWindow> {
        if op == opcode::BRK {
            Some(&Self::BRK_WINDOW)
        } else {
            None
        }
    }
}

pub(crate) const fn build_opcode_table() -> [ArrayVec<[Microcode; 7]>; 256] {
    use Microcode::*;
    use opcode::*;

    let mut r = [ArrayVec::from_array_empty([Microcode::Kill; 7]); 256];
    r[AND_IMMEDIATE as usize] = microcode_arr!(ImmediateAnd);
    r[AND_ZERO_PAGE as usize] = zero_page_op(AndZeroPage);
    r[AND_ZERO_PAGE_X as usize] = zero_page_x_op(AndZeroPage);
    r[AND_ABSOLUTE as usize] = absolute_op(AndMem);
    r[AND_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_op(Microcode::IndexedXAnd);
    r[AND_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_op(Microcode::IndexedYAnd);
    r[AND_INDEXED_INDIRECT as usize] = indexed_indirect_op(AndMem);
    r[AND_INDIRECT_INDEXED as usize] = indirect_indexed_op(Microcode::IndexedYAnd);
    r[LDA_IMMEDIATE as usize] = microcode_arr!(LoadAImmediate);
    r[LDA_ZERO_PAGE as usize] = zero_page_op(LoadAZeroPage);
    r[LDA_ZERO_PAGE_X as usize] = zero_page_x_op(LoadAZeroPage);
    r[LDA_ABSOLUTE as usize] = absolute_op(LoadAMem);
    r[LDA_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_op(Microcode::IndexedXLoadA);
    r[LDA_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_op(Microcode::IndexedYLoadA);
    r[LDA_INDIRECT_INDEXED as usize] = indexed_indirect_op(LoadAMem);
    r[LDA_INDIRECT_INDEXED_Y as usize] = indirect_indexed_op(Microcode::IndexedYLoadA);
    r[LDX_IMMEDIATE as usize] = microcode_arr!(LoadXImmediate);
    r[LDX_ZERO_PAGE as usize] = zero_page_op(LoadXZeroPage);
    r[LDX_ZERO_PAGE_Y as usize] = zero_page_y_op(LoadXZeroPage);
    r[LDX_ABSOLUTE as usize] = absolute_op(LoadXMem);
    r[LDX_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_op(Microcode::IndexedYLoadX);
    r[LDY_IMMEDIATE as usize] = microcode_arr!(LoadYImmediate);
    r[LDY_ZERO_PAGE as usize] = zero_page_op(LoadYZeroPage);
    r[LDY_ZERO_PAGE_X as usize] = zero_page_x_op(LoadYZeroPage);
    r[LDY_ABSOLUTE as usize] = absolute_op(LoadYMem);
    r[LDY_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_op(Microcode::IndexedXLoadY);
    r[STA_ZERO_PAGE as usize] = zero_page_op(StoreAZeroPage);
    r[STA_ZERO_PAGE_X as usize] = zero_page_x_op(StoreAZeroPage);
    r[STA_ABSOLUTE as usize] = absolute_op(StoreAMem);
    r[STA_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_store_a();
    r[STA_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_store_a();
    r[STA_INDEXED_INDIRECT as usize] = indexed_indirect_op(StoreAMem);
    r[STA_INDIRECT_INDEXED_Y as usize] = indirect_indexed_store_a();
    r[STX_ZERO_PAGE as usize] = zero_page_op(StoreXZeroPage);
    r[STX_ZERO_PAGE_Y as usize] = zero_page_y_op(StoreXZeroPage);
    r[STX_ABSOLUTE as usize] = absolute_op(StoreXMem);
    r[STY_ZERO_PAGE as usize] = zero_page_op(StoreYZeroPage);
    r[STY_ZERO_PAGE_X as usize] = zero_page_x_op(StoreYZeroPage);
    r[STY_ABSOLUTE as usize] = absolute_op(StoreYMem);
    r[BIT_ZERO_PAGE as usize] = zero_page_op(BitZeroPage);
    r[BIT_ABSOLUTE as usize] = absolute_op(BitMem);
    r[ADC_IMMEDIATE as usize] = microcode_arr!(ImmediateAdc);
    r[ADC_ZERO_PAGE as usize] = zero_page_op(AdcZeroPage);
    r[ADC_ZERO_PAGE_X as usize] = zero_page_x_op(AdcZeroPage);
    r[ADC_ABSOLUTE as usize] = absolute_op(AdcMem);
    r[ADC_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_op(Microcode::IndexedXAdc);
    r[ADC_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_op(Microcode::IndexedYAdc);
    r[ADC_INDEXED_INDIRECT as usize] = indexed_indirect_op(AdcMem);
    r[ADC_INDIRECT_INDEXED as usize] = indirect_indexed_op(Microcode::IndexedYAdc);
    r[ASL_ACCUMULATOR as usize] = microcode_arr!(AslAccumulator);
    r[ASL_ZERO_PAGE as usize] = zero_page_rmw_op(AslZeroPage);
    r[ASL_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(AslZeroPage);
    r[ASL_ABSOLUTE as usize] = absolute_rmw_op(AslMem);
    r[ASL_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(AslMem);
    r[LSR_ACCUMULATOR as usize] = microcode_arr!(LsrAccumulator);
    r[LSR_ZERO_PAGE as usize] = zero_page_rmw_op(LsrZeroPage);
    r[LSR_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(LsrZeroPage);
    r[LSR_ABSOLUTE as usize] = absolute_rmw_op(LsrMem);
    r[LSR_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(LsrMem);
    r[ROL_ACCUMULATOR as usize] = microcode_arr!(RolAccumulator);
    r[ROL_ZERO_PAGE as usize] = zero_page_rmw_op(RolZeroPage);
    r[ROL_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(RolZeroPage);
    r[ROL_ABSOLUTE as usize] = absolute_rmw_op(RolMem);
    r[ROL_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(RolMem);
    r[ROR_ACCUMULATOR as usize] = microcode_arr!(RorAccumulator);
    r[ROR_ZERO_PAGE as usize] = zero_page_rmw_op(RorZeroPage);
    r[ROR_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(RorZeroPage);
    r[ROR_ABSOLUTE as usize] = absolute_rmw_op(RorMem);
    r[ROR_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(RorMem);
    r[BCC as usize] = microcode_arr!(BranchCarryClear);
    r[BCS as usize] = microcode_arr!(BranchCarrySet);
    r[BEQ as usize] = microcode_arr!(BranchZeroSet);
    r[BMI as usize] = microcode_arr!(BranchNegativeSet);
    r[BNE as usize] = microcode_arr!(BranchZeroClear);
    r[BPL as usize] = microcode_arr!(BranchNegativeClear);
    r[BVC as usize] = microcode_arr!(BranchOverflowClear);
    r[BVS as usize] = microcode_arr!(BranchOverflowSet);
    r[PHA as usize] = microcode_arr!(Nop, PushA);
    r[PLA as usize] = microcode_arr!(Nop, PopStack, UpdateAFromAlu);
    r[PHP as usize] = microcode_arr!(Nop, PushStatusBreakSet);
    r[PLP as usize] = microcode_arr!(Nop, DummyStackRead, Plp);
    r[JMP_ABSOLUTE as usize] = microcode_arr!(AbsoluteL, LoadPcAbsoluteH);
    // The indirect JMP pointer is a program-chosen address, not an
    // addressing-mode guarantee, so it keeps the generic pointer read.
    r[JMP_INDIRECT as usize] = microcode_arr!(AbsoluteL, AbsoluteH, IndexedLMem, IndexedHAndJump);
    r[JSR as usize] = microcode_arr!(AbsoluteL, Nop, PushPch, PushPcl, LoadPcAbsoluteH);
    r[RTS as usize] = microcode_arr!(SkipImmediate, DummyStackRead, PopPcL, PopPcH, IncPc);
    r[RTI as usize] = microcode_arr!(SkipImmediate, DummyStackRead, Plp, PopPcL, PopPcH);
    r[CLC as usize] = microcode_arr!(ClearCarry);
    r[SEC as usize] = microcode_arr!(SetCarry);
    r[CLD as usize] = microcode_arr!(ClearDecimal);
    r[SED as usize] = microcode_arr!(SetDecimal);
    r[CLI as usize] = microcode_arr!(ClearInterruptDisabled);
    r[SEI as usize] = microcode_arr!(SetInterruptDisabled);
    r[CLV as usize] = microcode_arr!(ClearOverflow);
    r[NOP as usize] = microcode_arr!(Nop);
    r[NOP1 as usize] = microcode_arr!(Nop);
    r[NOP2 as usize] = microcode_arr!(Nop);
    r[NOP3 as usize] = microcode_arr!(Nop);
    r[NOP4 as usize] = microcode_arr!(Nop);
    r[NOP5 as usize] = microcode_arr!(Nop);
    r[NOP6 as usize] = microcode_arr!(Nop);
    r[NOP_IMMEDIATE1 as usize] = microcode_arr!(SkipImmediate);
    r[NOP_IMMEDIATE2 as usize] = microcode_arr!(SkipImmediate);
    r[NOP_IMMEDIATE3 as usize] = microcode_arr!(SkipImmediate);
    r[NOP_IMMEDIATE4 as usize] = microcode_arr!(SkipImmediate);
    r[NOP_IMMEDIATE5 as usize] = microcode_arr!(SkipImmediate);
    r[NOP_ZERO_PAGE1 as usize] =
        microcode_arr!(Microcode::ZeroPage, Microcode::LoadIntoAluZeroPage);
    r[NOP_ZERO_PAGE2 as usize] =
        microcode_arr!(Microcode::ZeroPage, Microcode::LoadIntoAluZeroPage);
    r[NOP_ZERO_PAGE3 as usize] =
        microcode_arr!(Microcode::ZeroPage, Microcode::LoadIntoAluZeroPage);
    r[NOP_ZERO_PAGE_X1 as usize] = zero_page_x_op(Microcode::LoadIntoAluZeroPage);
    r[NOP_ZERO_PAGE_X2 as usize] = zero_page_x_op(Microcode::LoadIntoAluZeroPage);
    r[NOP_ZERO_PAGE_X3 as usize] = zero_page_x_op(Microcode::LoadIntoAluZeroPage);
    r[NOP_ZERO_PAGE_X4 as usize] = zero_page_x_op(Microcode::LoadIntoAluZeroPage);
    r[NOP_ZERO_PAGE_X5 as usize] = zero_page_x_op(Microcode::LoadIntoAluZeroPage);
    r[NOP_ZERO_PAGE_X6 as usize] = zero_page_x_op(Microcode::LoadIntoAluZeroPage);
    r[NOP_ABSOLUTE as usize] = absolute_op(Microcode::LoadIntoAluMem);
    r[NOP_ABSOLUTE_INDEXED_X1 as usize] = absolute_indexed_x_op(Microcode::IndexedXNop);
    r[NOP_ABSOLUTE_INDEXED_X2 as usize] = absolute_indexed_x_op(Microcode::IndexedXNop);
    r[NOP_ABSOLUTE_INDEXED_X3 as usize] = absolute_indexed_x_op(Microcode::IndexedXNop);
    r[NOP_ABSOLUTE_INDEXED_X4 as usize] = absolute_indexed_x_op(Microcode::IndexedXNop);
    r[NOP_ABSOLUTE_INDEXED_X5 as usize] = absolute_indexed_x_op(Microcode::IndexedXNop);
    r[NOP_ABSOLUTE_INDEXED_X6 as usize] = absolute_indexed_x_op(Microcode::IndexedXNop);
    r[KIL1 as usize] = microcode_arr!(Kill);
    r[KIL2 as usize] = microcode_arr!(Kill);
    r[KIL3 as usize] = microcode_arr!(Kill);
    r[KIL4 as usize] = microcode_arr!(Kill);
    r[KIL5 as usize] = microcode_arr!(Kill);
    r[KIL6 as usize] = microcode_arr!(Kill);
    r[KIL7 as usize] = microcode_arr!(Kill);
    r[KIL8 as usize] = microcode_arr!(Kill);
    r[KIL9 as usize] = microcode_arr!(Kill);
    r[KIL10 as usize] = microcode_arr!(Kill);
    r[KIL11 as usize] = microcode_arr!(Kill);
    r[KIL12 as usize] = microcode_arr!(Kill);
    r[BRK as usize] = InterruptSequences::BRK;
    r[SBC_IMMEDIATE as usize] = microcode_arr!(ImmediateSbc);
    r[USBC as usize] = microcode_arr!(ImmediateSbc);
    r[SBC_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, SbcZeroPage);
    r[SBC_ZERO_PAGE_X as usize] = zero_page_x_op(SbcZeroPage);
    r[SBC_ABSOLUTE as usize] = absolute_op(SbcMem);
    r[SBC_ABSOLUTE_INDEXED_X as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedXSbc);
    r[SBC_ABSOLUTE_INDEXED_Y as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedYSbc);
    r[SBC_INDEXED_INDIRECT as usize] = indexed_indirect_op(SbcMem);
    r[SBC_INDIRECT_INDEXED as usize] = indirect_indexed_op(Microcode::IndexedYSbc);
    r[CMP_IMMEDIATE as usize] = microcode_arr!(ImmediateCmp);
    r[CMP_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, CmpZeroPage);
    r[CMP_ZERO_PAGE_X as usize] = zero_page_x_op(CmpZeroPage);
    r[CMP_ABSOLUTE as usize] = absolute_op(CmpMem);
    r[CMP_ABSOLUTE_INDEXED_X as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedXCmp);
    r[CMP_ABSOLUTE_INDEXED_Y as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedYCmp);
    r[CMP_INDEXED_INDIRECT as usize] = microcode_arr!(
        ZeroPage,
        ZeroPageIndexedX,
        IndexedLZeroPage,
        IndexedHZeroPage,
        CmpMem
    );
    r[CMP_INDIRECT_INDEXED as usize] = indirect_indexed_op(Microcode::IndexedYCmp);
    r[CPX_IMMEDIATE as usize] = microcode_arr!(ImmediateCpx);
    r[CPX_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, CpxZeroPage);
    r[CPX_ABSOLUTE as usize] = absolute_op(CpxMem);
    r[CPY_IMMEDIATE as usize] = microcode_arr!(ImmediateCpy);
    r[CPY_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, CpyZeroPage);
    r[CPY_ABSOLUTE as usize] = absolute_op(CpyMem);
    r[TAX as usize] = microcode_arr!(TransferAtoX);
    r[TXA as usize] = microcode_arr!(TransferXtoA);
    r[TAY as usize] = microcode_arr!(TransferAtoY);
    r[TYA as usize] = microcode_arr!(TransferYtoA);
    r[TSX as usize] = microcode_arr!(TransferSPtoX);
    r[TXS as usize] = microcode_arr!(TransferXtoSP);
    r[INX as usize] = microcode_arr!(IncX);
    r[INY as usize] = microcode_arr!(IncY);
    r[DEX as usize] = microcode_arr!(DecX);
    r[DEY as usize] = microcode_arr!(DecY);
    r[ORA_IMMEDIATE as usize] = microcode_arr!(ImmediateOra);
    r[ORA_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, OraZeroPage);
    r[ORA_ZERO_PAGE_X as usize] = zero_page_x_op(OraZeroPage);
    r[ORA_ABSOLUTE as usize] = absolute_op(OraMem);
    r[ORA_ABSOLUTE_INDEXED_X as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedXOra);
    r[ORA_ABSOLUTE_INDEXED_Y as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedYOra);
    r[ORA_INDEXED_INDIRECT as usize] = indexed_indirect_op(OraMem);
    r[ORA_INDIRECT_INDEXED as usize] = indirect_indexed_op(Microcode::IndexedYOra);
    r[EOR_IMMEDIATE as usize] = microcode_arr!(ImmediateEor);
    r[EOR_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, EorZeroPage);
    r[EOR_ZERO_PAGE_X as usize] = zero_page_x_op(EorZeroPage);
    r[EOR_ABSOLUTE as usize] = absolute_op(EorMem);
    r[EOR_ABSOLUTE_INDEXED_X as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedXEor);
    r[EOR_ABSOLUTE_INDEXED_Y as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedYEor);
    r[EOR_INDEXED_INDIRECT as usize] = indexed_indirect_op(EorMem);
    r[EOR_INDIRECT_INDEXED as usize] = indirect_indexed_op(Microcode::IndexedYEor);
    r[ALR as usize] = microcode_arr!(AlrImmediate);
    r[ANE as usize] = microcode_arr!(AneImmediate);
    r[ANC as usize] = microcode_arr!(AncImmediate);
    r[ANC1 as usize] = microcode_arr!(AncImmediate);
    r[ANC2 as usize] = microcode_arr!(AncImmediate);
    r[ARR as usize] = microcode_arr!(ArrImmediate);
    r[AXS as usize] = microcode_arr!(AxsImmediate);
    // LAX immediate should be a two-cycle instruction: FetchAndDecode + LaxImmediate
    // Make the immediate microcode a single micro-op (LaxImmediate) so total cycles = 2
    r[LAX_IMMEDIATE as usize] = microcode_arr!(LaxImmediate);
    r[LAX_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, LaxZeroPage);
    r[LAX_ZERO_PAGE_Y as usize] = zero_page_y_op(LaxZeroPage);
    r[LAX_ABSOLUTE as usize] = absolute_op(LaxMem);
    r[LAX_ABSOLUTE_INDEXED_Y as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedYLax);
    r[LAS_ABSOLUTE_INDEXED_Y as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedYLas);
    r[LAX_INDEXED_INDIRECT as usize] = indexed_indirect_op(LaxMem);
    r[LAX_INDIRECT_INDEXED as usize] = indirect_indexed_op(Microcode::IndexedYLax);
    r[SAX_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, SaxZeroPage);
    r[SAX_ZERO_PAGE_Y as usize] = zero_page_y_op(SaxZeroPage);
    r[SAX_ABSOLUTE as usize] = absolute_op(SaxMem);
    r[SAX_INDEXED_INDIRECT as usize] = indexed_indirect_op(SaxMem);
    r[DCP_ZERO_PAGE as usize] = zero_page_rmw_op(DcpZeroPage);
    r[DCP_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(DcpZeroPage);
    r[DCP_ABSOLUTE as usize] = absolute_rmw_op(DcpMem);
    r[DCP_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(DcpMem);
    r[DCP_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_rmw_op(DcpMem);
    r[DCP_INDEXED_INDIRECT as usize] = indexed_indirect_rmw_op(DcpMem);
    r[DCP_INDIRECT_INDEXED as usize] = indirect_indexed_rmw_op(DcpMem);
    r[DEC_ZERO_PAGE as usize] = zero_page_rmw_op(DecAluZeroPage);
    r[DEC_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(DecAluZeroPage);
    r[DEC_ABSOLUTE as usize] = absolute_rmw_op(DecAluMem);
    r[DEC_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(DecAluMem);
    r[INC_ZERO_PAGE as usize] = zero_page_rmw_op(IncAluZeroPage);
    r[INC_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(IncAluZeroPage);
    r[INC_ABSOLUTE as usize] = absolute_rmw_op(IncAluMem);
    r[INC_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(IncAluMem);
    r[ISC_ZERO_PAGE as usize] = zero_page_rmw_op(IscZeroPage);
    r[ISC_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(IscZeroPage);
    r[ISC_ABSOLUTE as usize] = absolute_rmw_op(IscMem);
    r[ISC_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(IscMem);
    r[ISC_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_rmw_op(IscMem);
    r[ISC_INDEXED_INDIRECT as usize] = indexed_indirect_rmw_op(IscMem);
    r[ISC_INDIRECT_INDEXED as usize] = indirect_indexed_rmw_op(IscMem);
    r[RRA_ZERO_PAGE as usize] = zero_page_rmw_op(RraZeroPage);
    r[RRA_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(RraZeroPage);
    r[RRA_ABSOLUTE as usize] = absolute_rmw_op(RraMem);
    r[RRA_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(RraMem);
    r[RRA_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_rmw_op(RraMem);
    r[RRA_INDEXED_INDIRECT as usize] = indexed_indirect_rmw_op(RraMem);
    r[RRA_INDIRECT_INDEXED as usize] = indirect_indexed_rmw_op(RraMem);
    r[RLA_ZERO_PAGE as usize] = zero_page_rmw_op(RlaZeroPage);
    r[RLA_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(RlaZeroPage);
    r[RLA_ABSOLUTE as usize] = absolute_rmw_op(RlaMem);
    r[RLA_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(RlaMem);
    r[RLA_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_rmw_op(RlaMem);
    r[RLA_INDEXED_INDIRECT as usize] = indexed_indirect_rmw_op(RlaMem);
    r[RLA_INDIRECT_INDEXED as usize] = indirect_indexed_rmw_op(RlaMem);
    r[SLO_ZERO_PAGE as usize] = zero_page_rmw_op(SloZeroPage);
    r[SLO_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(SloZeroPage);
    r[SLO_ABSOLUTE as usize] = absolute_rmw_op(SloMem);
    r[SLO_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(SloMem);
    r[SLO_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_rmw_op(SloMem);
    r[SLO_INDEXED_INDIRECT as usize] = indexed_indirect_rmw_op(SloMem);
    r[SLO_INDIRECT_INDEXED as usize] = indirect_indexed_rmw_op(SloMem);
    r[SRE_ZERO_PAGE as usize] = zero_page_rmw_op(SreZeroPage);
    r[SRE_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(SreZeroPage);
    r[SRE_ABSOLUTE as usize] = absolute_rmw_op(SreMem);
    r[SRE_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(SreMem);
    r[SRE_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_rmw_op(SreMem);
    r[SRE_INDEXED_INDIRECT as usize] = indexed_indirect_rmw_op(SreMem);
    r[SRE_INDIRECT_INDEXED as usize] = indirect_indexed_rmw_op(SreMem);
    r[SHX_ABSOLUTE_INDEXED_Y as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedYShxAlways, Shx);
    r[SHA_ABSOLUTE_INDEXED_Y as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedYShaAlways, Sha);
    r[SHY_ABSOLUTE_INDEXED_X as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedXShyAlways, Shy);
    r[TAS_ABSOLUTE_INDEXED_Y as usize] =
        microcode_arr!(AbsoluteL, AbsoluteH, Microcode::IndexedYTasAlways, Tas);
    r[SHA_INDIRECT_INDEXED_Y as usize] = microcode_arr!(
        ZeroPage,
        IndexedLZeroPage,
        IndexedHZeroPage,
        Microcode::IndexedYShaAlways,
        Sha
    );
    r
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImmediateOp {
    Adc,
    And,
    Sbc,
    Cmp,
    Cpx,
    Cpy,
    Ora,
    Eor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransferDirection {
    AtoX,
    XtoA,
    AtoY,
    YtoA,
    SPtoX,
    XtoSP,
}

#[derive(Debug, Clone, Copy)]
pub enum ShiftRotateOp {
    Asl,
    Lsr,
    Rol,
    Ror,
}

impl ImmediateOp {
    pub fn exec<M: Mcu>(self, cpu: &mut Cpu, bus: &mut Bus<M>) {
        match self {
            ImmediateOp::Adc => cpu.adc::<Alu, _>(bus),
            ImmediateOp::And => cpu.and::<Alu, _>(bus),
            ImmediateOp::Sbc => cpu.sbc::<Alu, _>(bus),
            ImmediateOp::Cmp => cpu.cmp::<Alu, _>(bus),
            ImmediateOp::Cpx => cpu.cpx::<Alu, _>(bus),
            ImmediateOp::Cpy => cpu.cpy::<Alu, _>(bus),
            ImmediateOp::Ora => cpu.ora::<Alu, _>(bus),
            ImmediateOp::Eor => cpu.eor::<Alu, _>(bus),
        }
    }
}

/// Each Microcode instruction executed by the CPU in a single cycle.
///
/// Fieldless (`#[repr(u8)]`) so `exec()` / `bus_cycle()` match on a plain
/// discriminant (jump table) instead of discriminant + payload. Dynamic data
/// lives on `Cpu` (e.g. `dummy_read_addr` for `DummyReadAt`); every other
/// former payload was a static per-opcode value, now expanded into distinct
/// variants (e.g. `LoadAImmediate`, `AdcZeroPage`, `IndexedXLoadA`).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, strum_macros::Display)]
#[repr(u8)]
pub enum Microcode {
    /// Fetch and decode next op code
    FetchAndDecode,
    /// Fetch byte at the pc address, but do nothing, pc not increase
    FetchOnly,
    // -- LoadR(ValueSource, Register) expanded --
    LoadAImmediate,
    LoadXImmediate,
    LoadYImmediate,
    LoadAZeroPage,
    LoadXZeroPage,
    LoadYZeroPage,
    LoadAMem,
    LoadXMem,
    LoadYMem,
    // -- StoreR(ValueSource, Register) expanded (ZeroPage/Mem x A/X/Y) --
    StoreAZeroPage,
    StoreXZeroPage,
    StoreYZeroPage,
    StoreAMem,
    StoreXMem,
    StoreYMem,
    // -- StoreAlu(ValueSource) --
    StoreAluZeroPage,
    StoreAluMem,
    // addressing microcodes
    /// Take a byte from instruction data stream, set cpu address_bus field by read from memory use zero page addressing
    ZeroPage,
    /// Impl ZeroPageIndexedX addressing, work after ZeroPage, set abl = (abl + x) % 256, set abh = 00
    ZeroPageIndexedX,
    ZeroPageIndexedY,
    /// Take a byte from instruction data stream, set cpu abl field
    AbsoluteL,
    /// Take a byte from instruction data stream, set cpu abh field
    AbsoluteH,
    // -- IndexedXWithOp / IndexedYWithOp expanded --
    // FirstClock (read) path: dummy read only on page cross.
    IndexedXAnd,
    IndexedYAnd,
    IndexedXLoadA,
    IndexedYLoadA,
    IndexedYLoadX,
    IndexedXLoadY,
    IndexedXOra,
    IndexedYOra,
    IndexedXEor,
    IndexedYEor,
    IndexedYLax,
    IndexedYLas,
    IndexedXSbc,
    IndexedYSbc,
    IndexedXAdc,
    IndexedYAdc,
    IndexedXCmp,
    IndexedYCmp,
    IndexedXNop,
    // FirstClockAlways path: always the dummy read at (old high | new low).
    IndexedXLoadAluAlways,
    IndexedYLoadAluAlways,
    IndexedXStoreAAlways,
    IndexedYStoreAAlways,
    IndexedYShxAlways,
    IndexedXShyAlways,
    IndexedYShaAlways,
    IndexedYTasAlways,
    /// Dummy fetch at PC ("read next instruction byte and throw it away"):
    /// implied ops, stack-op padding, JSR cycle 3, the first taken-branch
    /// fixup cycle, and the interrupt/RESET dead cycles.
    Nop,
    /// Taken-branch page-cross fixup cycle: hardware re-reads the
    /// (old PCH | new PBL) address while fixing the high byte. The address
    /// is stored on `Cpu::dummy_read_addr` when the cycle is queued.
    DummyReadAt,
    /// Stack-page dummy read at $0100 | SP (the RTS/RTI/PLP pre-increment
    /// cycle): hardware reads and discards without changing SP.
    DummyStackRead,
    SkipDetectInterrupt,
    /// Load the low byte of the address latch (`(zp,X)`/`(zp),Y` pointer
    /// via `Mcu::read_zero_page`, or generic path for `JMP ($xxxx)`).
    IndexedLZeroPage,
    IndexedLMem,
    /// Load the high byte of the address latch, at the incremented (no
    /// carry) low byte.
    IndexedHZeroPage,
    /// Read immediate value from instruction data stream, but do not use it
    SkipImmediate,
    /// Load byte into alu at memory ab.
    LoadIntoAluZeroPage,
    LoadIntoAluMem,
    /// Store SHX result at memory ab
    Shx,
    /// Store SHY result at memory ab
    Shy,
    /// Store SHA result at memory ab
    Sha,
    /// Store TAS result at memory ab
    Tas,
    // -- ImmediateWithOp(ImmediateOp) expanded --
    ImmediateAdc,
    ImmediateAnd,
    ImmediateSbc,
    ImmediateCmp,
    ImmediateCpx,
    ImmediateCpy,
    ImmediateOra,
    ImmediateEor,
    /// Fetch a byte from memory, then add to accumulator with carry
    AdcZeroPage,
    AdcMem,
    /// Fetch a byte from memory, then subtract with carry from accumulator
    SbcZeroPage,
    SbcMem,
    /// Fetch a byte from memory, then compare with accumulator
    CmpZeroPage,
    CmpMem,
    /// Fetch a byte from memory, then compare with x register
    CpxZeroPage,
    CpxMem,
    /// Fetch a byte from memory, then compare with y register
    CpyZeroPage,
    CpyMem,
    /// Fetch a byte from memory, then or with accumulator
    OraZeroPage,
    OraMem,
    /// Fetch a byte from memory, then xor with accumulator
    EorZeroPage,
    EorMem,
    /// Fetch a byte from memory, then and with accumulator
    AndZeroPage,
    AndMem,
    /// Undocumented ANE/XAA: (A OR CONST) AND X AND oper -> A
    /// Implemented deterministically as A := X & oper
    AneImmediate,
    /// LAX immediate: load immediate into A and X in a single micro-op (two cycles total with Fetch)
    LaxImmediate,
    /// Execute LAS/LAR semantics
    Las,
    /// Test bits in accumulator against ALU value
    BitZeroPage,
    BitMem,
    /// Shift/rotate: accumulator form (no bus access), zero-page and
    /// absolute/pointer-target memory forms.
    AslAccumulator,
    AslZeroPage,
    AslMem,
    LsrAccumulator,
    LsrZeroPage,
    LsrMem,
    RolAccumulator,
    RolZeroPage,
    RolMem,
    RorAccumulator,
    RorZeroPage,
    RorMem,
    AlrImmediate,
    AncImmediate,
    ArrImmediate,
    AxsImmediate,
    /// Fetch a byte from memory, then load it into A and X.
    LaxZeroPage,
    LaxMem,
    /// The undocumented RMW and `SAX` micro-ops.
    SaxZeroPage,
    SaxMem,
    RlaZeroPage,
    RlaMem,
    DcpZeroPage,
    DcpMem,
    IscZeroPage,
    IscMem,
    RraZeroPage,
    RraMem,
    SloZeroPage,
    SloMem,
    SreZeroPage,
    SreMem,
    /// `JMP ($xxxx)`: load the address latch through the pointer's high
    /// byte and jump to it.
    IndexedHAndJump,
    SetCarry,
    SetDecimal,
    SetInterruptDisabled,
    ClearCarry,
    ClearDecimal,
    ClearInterruptDisabled,
    ClearOverflow,
    TransferAtoX,
    TransferXtoA,
    TransferAtoY,
    TransferYtoA,
    TransferSPtoX,
    TransferXtoSP,
    /// Increment/decrement a register (no memory access).
    IncX,
    IncY,
    DecX,
    DecY,
    /// Increment/decrement the ALU latch, writing the result back.
    IncAluZeroPage,
    IncAluMem,
    DecAluZeroPage,
    DecAluMem,
    /// Branch with the test statically encoded.
    BranchCarryClear,
    BranchCarrySet,
    BranchZeroSet,
    BranchZeroClear,
    BranchNegativeSet,
    BranchNegativeClear,
    BranchOverflowSet,
    BranchOverflowClear,
    /// Push cpu status register into stack (break flag statically encoded).
    PushStatusBreakSet,
    PushStatusBreakClear,
    /// Pop cpu status register from stack, used to return from interrupt handler, and PLP
    Plp,
    /// Pop low byte of PC from stack
    PopPcL,
    /// Pop high byte of PC from stack
    PopPcH,
    IncPc,
    PushA,
    PushPcl,
    PushPch,
    /// pop stack into alu
    PopStack,
    /// Set register A to alu
    UpdateAFromAlu,
    /// Cpu will trapped infinitely if execute this microcode, used to impl illegal instructions that will lock up the CPU
    /// Only reset can restore the CPU from this state
    #[default]
    Kill,
    LoadResetPcL,
    LoadResetPcH,
    LoadNmiPcL,
    LoadNmiPcH,
    /// Set pc to address_latch | absolute << 8
    LoadPcAbsoluteH,
    /// Fetch the interrupt vector low byte ($FFFE, or $FFFA when an NMI
    /// assertion hijacks the vector; see `Cpu::load_irq_pcl`).
    LoadIrqPcL,
    LoadIrqPcH,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[allow(clippy::enum_variant_names)]
pub enum BranchTest {
    IfCarryClear,
    IfCarrySet,
    IfZeroSet,
    IfZeroClear,
    IfNegativeSet,
    IfNegativeClear,
    IfOverflowSet,
    IfOverflowClear,
}

impl BranchTest {
    fn test(self, cpu: &Cpu) -> bool {
        match self {
            BranchTest::IfCarryClear => !cpu.flag(Flag::Carry),
            BranchTest::IfCarrySet => cpu.flag(Flag::Carry),
            BranchTest::IfZeroSet => cpu.flag(Flag::Zero),
            BranchTest::IfZeroClear => !cpu.flag(Flag::Zero),
            BranchTest::IfNegativeSet => cpu.flag(Flag::Negative),
            BranchTest::IfNegativeClear => !cpu.flag(Flag::Negative),
            BranchTest::IfOverflowSet => cpu.flag(Flag::Overflow),
            BranchTest::IfOverflowClear => !cpu.flag(Flag::Overflow),
        }
    }
}

/// Represents the opcode of a CPU instruction, the first byte of an instruction
///
/// The `Opcode` enum type was removed; these are defined as a module containing
/// module-level constants so callers can continue to use `Opcode::NAME` paths.
pub(crate) mod opcode {
    // Load and Save instructions

    pub const LDA_IMMEDIATE: u8 = 0xA9;
    pub const LDA_ZERO_PAGE: u8 = 0xA5;
    pub const LDA_ZERO_PAGE_X: u8 = 0xB5;
    pub const LDA_ABSOLUTE: u8 = 0xAD;
    pub const LDA_ABSOLUTE_INDEXED_X: u8 = 0xBD;
    pub const LDA_ABSOLUTE_INDEXED_Y: u8 = 0xB9;
    pub const LDA_INDIRECT_INDEXED: u8 = 0xA1;
    pub const LDA_INDIRECT_INDEXED_Y: u8 = 0xB1;

    pub const LDX_IMMEDIATE: u8 = 0xA2;
    pub const LDX_ZERO_PAGE: u8 = 0xA6;
    pub const LDX_ZERO_PAGE_Y: u8 = 0xB6;
    pub const LDX_ABSOLUTE: u8 = 0xAE;
    pub const LDX_ABSOLUTE_INDEXED_Y: u8 = 0xBE;

    pub const LDY_IMMEDIATE: u8 = 0xA0;
    pub const LDY_ZERO_PAGE: u8 = 0xA4;
    pub const LDY_ZERO_PAGE_X: u8 = 0xB4;
    pub const LDY_ABSOLUTE: u8 = 0xAC;
    pub const LDY_ABSOLUTE_INDEXED_X: u8 = 0xBC;

    pub const STA_ZERO_PAGE: u8 = 0x85;
    pub const STA_ZERO_PAGE_X: u8 = 0x95;
    pub const STA_ABSOLUTE: u8 = 0x8D;
    pub const STA_ABSOLUTE_INDEXED_X: u8 = 0x9D;
    pub const STA_ABSOLUTE_INDEXED_Y: u8 = 0x99;
    pub const STA_INDEXED_INDIRECT: u8 = 0x81;
    pub const STA_INDIRECT_INDEXED_Y: u8 = 0x91;

    pub const STX_ZERO_PAGE: u8 = 0x86;
    pub const STX_ZERO_PAGE_Y: u8 = 0x96;
    pub const STX_ABSOLUTE: u8 = 0x8E;

    pub const STY_ZERO_PAGE: u8 = 0x84;
    pub const STY_ZERO_PAGE_X: u8 = 0x94;
    pub const STY_ABSOLUTE: u8 = 0x8C;

    // Arithmetic instructions

    pub const ADC_IMMEDIATE: u8 = 0x69;
    pub const ADC_ZERO_PAGE: u8 = 0x65;
    pub const ADC_ZERO_PAGE_X: u8 = 0x75;
    pub const ADC_ABSOLUTE: u8 = 0x6D;
    pub const ADC_ABSOLUTE_INDEXED_X: u8 = 0x7D;
    pub const ADC_ABSOLUTE_INDEXED_Y: u8 = 0x79;
    pub const ADC_INDEXED_INDIRECT: u8 = 0x61;
    pub const ADC_INDIRECT_INDEXED: u8 = 0x71;

    pub const SBC_IMMEDIATE: u8 = 0xE9;
    pub const SBC_ZERO_PAGE: u8 = 0xE5;
    pub const SBC_ZERO_PAGE_X: u8 = 0xF5;
    pub const SBC_ABSOLUTE: u8 = 0xED;
    pub const SBC_ABSOLUTE_INDEXED_X: u8 = 0xFD;
    pub const SBC_ABSOLUTE_INDEXED_Y: u8 = 0xF9;
    pub const SBC_INDEXED_INDIRECT: u8 = 0xE1;
    pub const SBC_INDIRECT_INDEXED: u8 = 0xF1;

    pub const CMP_IMMEDIATE: u8 = 0xC9;
    pub const CMP_ZERO_PAGE: u8 = 0xC5;
    pub const CMP_ZERO_PAGE_X: u8 = 0xD5;
    pub const CMP_ABSOLUTE: u8 = 0xCD;
    pub const CMP_ABSOLUTE_INDEXED_X: u8 = 0xDD;
    pub const CMP_ABSOLUTE_INDEXED_Y: u8 = 0xD9;
    pub const CMP_INDEXED_INDIRECT: u8 = 0xC1;
    pub const CMP_INDIRECT_INDEXED: u8 = 0xD1;

    pub const CPX_IMMEDIATE: u8 = 0xE0;
    pub const CPX_ZERO_PAGE: u8 = 0xE4;
    pub const CPX_ABSOLUTE: u8 = 0xEC;

    pub const CPY_IMMEDIATE: u8 = 0xC0;
    pub const CPY_ZERO_PAGE: u8 = 0xC4;
    pub const CPY_ABSOLUTE: u8 = 0xCC;

    // Shift and Rotate instructions

    pub const ASL_ACCUMULATOR: u8 = 0x0A;
    pub const ASL_ZERO_PAGE: u8 = 0x06;
    pub const ASL_ZERO_PAGE_X: u8 = 0x16;
    pub const ASL_ABSOLUTE: u8 = 0x0E;
    pub const ASL_ABSOLUTE_INDEXED_X: u8 = 0x1E;

    pub const LSR_ACCUMULATOR: u8 = 0x4A;
    pub const LSR_ZERO_PAGE: u8 = 0x46;
    pub const LSR_ZERO_PAGE_X: u8 = 0x56;
    pub const LSR_ABSOLUTE: u8 = 0x4E;
    pub const LSR_ABSOLUTE_INDEXED_X: u8 = 0x5E;

    pub const ROL_ACCUMULATOR: u8 = 0x2A;
    pub const ROL_ZERO_PAGE: u8 = 0x26;
    pub const ROL_ZERO_PAGE_X: u8 = 0x36;
    pub const ROL_ABSOLUTE: u8 = 0x2E;
    pub const ROL_ABSOLUTE_INDEXED_X: u8 = 0x3E;

    pub const ROR_ACCUMULATOR: u8 = 0x6A;
    pub const ROR_ZERO_PAGE: u8 = 0x66;
    pub const ROR_ZERO_PAGE_X: u8 = 0x76;
    pub const ROR_ABSOLUTE: u8 = 0x6E;
    pub const ROR_ABSOLUTE_INDEXED_X: u8 = 0x7E;

    // Logic instructions

    pub const AND_IMMEDIATE: u8 = 0x29;
    pub const AND_ZERO_PAGE: u8 = 0x25;
    pub const AND_ZERO_PAGE_X: u8 = 0x35;
    pub const AND_ABSOLUTE: u8 = 0x2D;
    pub const AND_ABSOLUTE_INDEXED_X: u8 = 0x3D;
    pub const AND_ABSOLUTE_INDEXED_Y: u8 = 0x39;
    pub const AND_INDEXED_INDIRECT: u8 = 0x21;
    pub const AND_INDIRECT_INDEXED: u8 = 0x31;

    pub const ORA_IMMEDIATE: u8 = 0x09;
    pub const ORA_ZERO_PAGE: u8 = 0x05;
    pub const ORA_ZERO_PAGE_X: u8 = 0x15;
    pub const ORA_ABSOLUTE: u8 = 0x0D;
    pub const ORA_ABSOLUTE_INDEXED_X: u8 = 0x1D;
    pub const ORA_ABSOLUTE_INDEXED_Y: u8 = 0x19;
    pub const ORA_INDEXED_INDIRECT: u8 = 0x01;
    pub const ORA_INDIRECT_INDEXED: u8 = 0x11;

    pub const EOR_IMMEDIATE: u8 = 0x49;
    pub const EOR_ZERO_PAGE: u8 = 0x45;
    pub const EOR_ZERO_PAGE_X: u8 = 0x55;
    pub const EOR_ABSOLUTE: u8 = 0x4D;
    pub const EOR_ABSOLUTE_INDEXED_X: u8 = 0x5D;
    pub const EOR_ABSOLUTE_INDEXED_Y: u8 = 0x59;
    pub const EOR_INDEXED_INDIRECT: u8 = 0x41;
    pub const EOR_INDIRECT_INDEXED: u8 = 0x51;

    pub const BIT_ZERO_PAGE: u8 = 0x24;
    pub const BIT_ABSOLUTE: u8 = 0x2C;

    // Branch instructions

    pub const BCC: u8 = 0x90;
    pub const BCS: u8 = 0xB0;
    pub const BNE: u8 = 0xD0;
    pub const BEQ: u8 = 0xF0;
    pub const BPL: u8 = 0x10;
    pub const BMI: u8 = 0x30;
    pub const BVC: u8 = 0x50;
    pub const BVS: u8 = 0x70;

    // Transfer Instructions

    pub const TAX: u8 = 0xAA;
    pub const TXA: u8 = 0x8A;
    pub const TAY: u8 = 0xA8;
    pub const TYA: u8 = 0x98;
    pub const TSX: u8 = 0xBA;
    pub const TXS: u8 = 0x9A;
    pub const INX: u8 = 0xE8;
    pub const INY: u8 = 0xC8;
    pub const DEX: u8 = 0xCA;
    pub const DEY: u8 = 0x88;

    // Stack Instructions

    pub const PHA: u8 = 0x48;
    pub const PLA: u8 = 0x68;
    pub const PHP: u8 = 0x08;
    pub const PLP: u8 = 0x28;

    // Subroutine and Jump Instructions

    pub const JMP_ABSOLUTE: u8 = 0x4C;
    pub const JMP_INDIRECT: u8 = 0x6C;
    pub const JSR: u8 = 0x20;
    pub const RTS: u8 = 0x60;
    pub const RTI: u8 = 0x40;

    // Set and Clear Instructions
    pub const CLC: u8 = 0x18;
    pub const SEC: u8 = 0x38;
    pub const CLD: u8 = 0xD8;
    pub const SED: u8 = 0xF8;
    pub const CLI: u8 = 0x58;
    pub const SEI: u8 = 0x78;
    pub const CLV: u8 = 0xB8;

    // Miscellaneous Instructions
    pub const NOP: u8 = 0xEA;
    pub const BRK: u8 = 0x00;

    // Undocumented/Illegal Instructions

    //   Combined instructions

    pub const ALR: u8 = 0x4B;
    /// Undocumented ANE / XAA immediate
    pub const ANE: u8 = 0x8B;
    pub const ANC1: u8 = 0x0B;
    pub const ANC: u8 = 0x2B;
    pub const ANC2: u8 = 0x2B;
    pub const ARR: u8 = 0x6B;
    pub const AXS: u8 = 0xCB;
    pub const LAX_IMMEDIATE: u8 = 0xAB;

    pub const LAX_ZERO_PAGE: u8 = 0xA7;
    pub const LAX_ZERO_PAGE_Y: u8 = 0xB7;
    pub const LAX_ABSOLUTE: u8 = 0xAF;
    pub const LAX_ABSOLUTE_INDEXED_Y: u8 = 0xBF;
    pub const LAX_INDEXED_INDIRECT: u8 = 0xA3;
    pub const LAX_INDIRECT_INDEXED: u8 = 0xB3;

    pub const LAS_ABSOLUTE_INDEXED_Y: u8 = 0xBB;

    pub const SAX_ZERO_PAGE: u8 = 0x87;
    pub const SAX_ZERO_PAGE_Y: u8 = 0x97;
    pub const SAX_ABSOLUTE: u8 = 0x8F;
    pub const SAX_INDEXED_INDIRECT: u8 = 0x83;

    //   Read-modify-write instructions
    pub const DCP_ZERO_PAGE: u8 = 0xC7;
    pub const DCP_ZERO_PAGE_X: u8 = 0xD7;
    pub const DCP_ABSOLUTE: u8 = 0xCF;
    pub const DCP_ABSOLUTE_INDEXED_X: u8 = 0xDF;
    pub const DCP_ABSOLUTE_INDEXED_Y: u8 = 0xDB;
    pub const DCP_INDEXED_INDIRECT: u8 = 0xC3;
    pub const DCP_INDIRECT_INDEXED: u8 = 0xD3;

    pub const DEC_ZERO_PAGE: u8 = 0xC6;
    pub const DEC_ZERO_PAGE_X: u8 = 0xD6;
    pub const DEC_ABSOLUTE: u8 = 0xCE;
    pub const DEC_ABSOLUTE_INDEXED_X: u8 = 0xDE;

    pub const INC_ZERO_PAGE: u8 = 0xE6;
    pub const INC_ZERO_PAGE_X: u8 = 0xF6;
    pub const INC_ABSOLUTE: u8 = 0xEE;
    pub const INC_ABSOLUTE_INDEXED_X: u8 = 0xFE;

    pub const ISC_ZERO_PAGE: u8 = 0xE7;
    pub const ISC_ZERO_PAGE_X: u8 = 0xF7;
    pub const ISC_ABSOLUTE: u8 = 0xEF;
    pub const ISC_ABSOLUTE_INDEXED_X: u8 = 0xFF;
    pub const ISC_ABSOLUTE_INDEXED_Y: u8 = 0xFB;
    pub const ISC_INDEXED_INDIRECT: u8 = 0xE3;
    pub const ISC_INDIRECT_INDEXED: u8 = 0xF3;

    pub const RRA_ZERO_PAGE: u8 = 0x67;
    pub const RRA_ZERO_PAGE_X: u8 = 0x77;
    pub const RRA_ABSOLUTE: u8 = 0x6F;
    pub const RRA_ABSOLUTE_INDEXED_X: u8 = 0x7F;
    pub const RRA_ABSOLUTE_INDEXED_Y: u8 = 0x7B;
    pub const RRA_INDEXED_INDIRECT: u8 = 0x63;
    pub const RRA_INDIRECT_INDEXED: u8 = 0x73;

    pub const RLA_ZERO_PAGE: u8 = 0x27;
    pub const RLA_ZERO_PAGE_X: u8 = 0x37;
    pub const RLA_ABSOLUTE: u8 = 0x2F;
    pub const RLA_ABSOLUTE_INDEXED_X: u8 = 0x3F;
    pub const RLA_ABSOLUTE_INDEXED_Y: u8 = 0x3B;
    pub const RLA_INDEXED_INDIRECT: u8 = 0x23;
    pub const RLA_INDIRECT_INDEXED: u8 = 0x33;

    pub const SLO_ZERO_PAGE: u8 = 0x07;
    pub const SLO_ZERO_PAGE_X: u8 = 0x17;
    pub const SLO_ABSOLUTE: u8 = 0x0F;
    pub const SLO_ABSOLUTE_INDEXED_X: u8 = 0x1F;
    pub const SLO_ABSOLUTE_INDEXED_Y: u8 = 0x1B;
    pub const SLO_INDEXED_INDIRECT: u8 = 0x03;
    pub const SLO_INDIRECT_INDEXED: u8 = 0x13;

    pub const SRE_ZERO_PAGE: u8 = 0x47;
    pub const SRE_ZERO_PAGE_X: u8 = 0x57;
    pub const SRE_ABSOLUTE: u8 = 0x4F;
    pub const SRE_ABSOLUTE_INDEXED_X: u8 = 0x5F;
    pub const SRE_ABSOLUTE_INDEXED_Y: u8 = 0x5B;
    pub const SRE_INDEXED_INDIRECT: u8 = 0x43;
    pub const SRE_INDIRECT_INDEXED: u8 = 0x53;

    pub const SHX_ABSOLUTE_INDEXED_Y: u8 = 0x9E;
    pub const SHA_ABSOLUTE_INDEXED_Y: u8 = 0x9F;
    pub const SHY_ABSOLUTE_INDEXED_X: u8 = 0x9C;
    pub const SHA_INDIRECT_INDEXED_Y: u8 = 0x93;
    pub const TAS_ABSOLUTE_INDEXED_Y: u8 = 0x9B;

    // duplicated opcodes
    pub const USBC: u8 = 0xEB;
    pub const NOP1: u8 = 0x1A;
    pub const NOP2: u8 = 0x3A;
    pub const NOP3: u8 = 0x5A;
    pub const NOP4: u8 = 0x7A;
    pub const NOP5: u8 = 0xDA;
    pub const NOP6: u8 = 0xFA;
    pub const NOP_IMMEDIATE1: u8 = 0x80;
    pub const NOP_IMMEDIATE2: u8 = 0x82;
    pub const NOP_IMMEDIATE3: u8 = 0x89;
    pub const NOP_IMMEDIATE4: u8 = 0xC2;
    pub const NOP_IMMEDIATE5: u8 = 0xE2;
    pub const NOP_ZERO_PAGE1: u8 = 0x04;
    pub const NOP_ZERO_PAGE2: u8 = 0x44;
    pub const NOP_ZERO_PAGE3: u8 = 0x64;
    pub const NOP_ZERO_PAGE_X1: u8 = 0x14;
    pub const NOP_ZERO_PAGE_X2: u8 = 0x34;
    pub const NOP_ZERO_PAGE_X3: u8 = 0x54;
    pub const NOP_ZERO_PAGE_X4: u8 = 0x74;
    pub const NOP_ZERO_PAGE_X5: u8 = 0xD4;
    pub const NOP_ZERO_PAGE_X6: u8 = 0xF4;
    pub const NOP_ABSOLUTE: u8 = 0x0C;
    pub const NOP_ABSOLUTE_INDEXED_X1: u8 = 0x1C;
    pub const NOP_ABSOLUTE_INDEXED_X2: u8 = 0x3C;
    pub const NOP_ABSOLUTE_INDEXED_X3: u8 = 0x5C;
    pub const NOP_ABSOLUTE_INDEXED_X4: u8 = 0x7C;
    pub const NOP_ABSOLUTE_INDEXED_X5: u8 = 0xDC;
    pub const NOP_ABSOLUTE_INDEXED_X6: u8 = 0xFC;

    pub const KIL1: u8 = 0x02;
    pub const KIL2: u8 = 0x12;
    pub const KIL3: u8 = 0x22;
    pub const KIL4: u8 = 0x32;
    pub const KIL5: u8 = 0x42;
    pub const KIL6: u8 = 0x52;
    pub const KIL7: u8 = 0x62;
    pub const KIL8: u8 = 0x72;
    pub const KIL9: u8 = 0x92;
    pub const KIL10: u8 = 0xB2;
    pub const KIL11: u8 = 0xD2;
    pub const KIL12: u8 = 0xF2;
}

pub(super) fn fetch_and_decode<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
    const OPCODE_TABLE: [ArrayVec<[Microcode; 7]>; 256] = build_opcode_table();

    let opcode = cpu.inc_read_byte(bus);
    cpu.push_microcodes(&OPCODE_TABLE[opcode as usize]);
    if let Some(window) = InterruptSequences::window_for_opcode(opcode) {
        cpu.arm_interrupt_window(window, None);
    }
}

impl Microcode {
    /// Number of `Microcode` variants. Must equal the last discriminant + 1;
    /// the dispatch table has exactly one entry per variant so `exec`
    /// can index it by `self as u8`.
    pub(crate) const COUNT: usize = 175;

    /// Function-pointer dispatch table in discriminant order.
    /// `const fn` so each `M: Mcu` monomorphization gets its own table;
    /// the optimizer folds the array build into an indirect jump.
    const fn dispatch_table<M: Mcu>() -> [fn(&mut Cpu, &mut Bus<M>); Self::COUNT] {
        [
            Microcode::exec_fetch_and_decode::<M>,
            Microcode::exec_fetch_only::<M>,
            Microcode::exec_load_a_immediate::<M>,
            Microcode::exec_load_x_immediate::<M>,
            Microcode::exec_load_y_immediate::<M>,
            Microcode::exec_load_a_zero_page::<M>,
            Microcode::exec_load_x_zero_page::<M>,
            Microcode::exec_load_y_zero_page::<M>,
            Microcode::exec_load_a_mem::<M>,
            Microcode::exec_load_x_mem::<M>,
            Microcode::exec_load_y_mem::<M>,
            Microcode::exec_store_a_zero_page::<M>,
            Microcode::exec_store_x_zero_page::<M>,
            Microcode::exec_store_y_zero_page::<M>,
            Microcode::exec_store_a_mem::<M>,
            Microcode::exec_store_x_mem::<M>,
            Microcode::exec_store_y_mem::<M>,
            Microcode::exec_store_alu_zero_page::<M>,
            Microcode::exec_store_alu_mem::<M>,
            Microcode::exec_zero_page::<M>,
            Microcode::exec_zero_page_indexed_x::<M>,
            Microcode::exec_zero_page_indexed_y::<M>,
            Microcode::exec_absolute_l::<M>,
            Microcode::exec_absolute_h::<M>,
            Microcode::exec_indexed_x_and::<M>,
            Microcode::exec_indexed_y_and::<M>,
            Microcode::exec_indexed_x_load_a::<M>,
            Microcode::exec_indexed_y_load_a::<M>,
            Microcode::exec_indexed_y_load_x::<M>,
            Microcode::exec_indexed_x_load_y::<M>,
            Microcode::exec_indexed_x_ora::<M>,
            Microcode::exec_indexed_y_ora::<M>,
            Microcode::exec_indexed_x_eor::<M>,
            Microcode::exec_indexed_y_eor::<M>,
            Microcode::exec_indexed_y_lax::<M>,
            Microcode::exec_indexed_y_las::<M>,
            Microcode::exec_indexed_x_sbc::<M>,
            Microcode::exec_indexed_y_sbc::<M>,
            Microcode::exec_indexed_x_adc::<M>,
            Microcode::exec_indexed_y_adc::<M>,
            Microcode::exec_indexed_x_cmp::<M>,
            Microcode::exec_indexed_y_cmp::<M>,
            Microcode::exec_indexed_x_nop::<M>,
            Microcode::exec_indexed_x_load_alu_always::<M>,
            Microcode::exec_indexed_y_load_alu_always::<M>,
            Microcode::exec_indexed_x_store_a_always::<M>,
            Microcode::exec_indexed_y_store_a_always::<M>,
            Microcode::exec_indexed_y_shx_always::<M>,
            Microcode::exec_indexed_x_shy_always::<M>,
            Microcode::exec_indexed_y_sha_always::<M>,
            Microcode::exec_indexed_y_tas_always::<M>,
            Microcode::exec_nop::<M>,
            Microcode::exec_dummy_read_at::<M>,
            Microcode::exec_dummy_stack_read::<M>,
            Microcode::exec_skip_detect_interrupt::<M>,
            Microcode::exec_indexed_l_zero_page::<M>,
            Microcode::exec_indexed_l_mem::<M>,
            Microcode::exec_indexed_h_zero_page::<M>,
            Microcode::exec_skip_immediate::<M>,
            Microcode::exec_load_into_alu_zero_page::<M>,
            Microcode::exec_load_into_alu_mem::<M>,
            Microcode::exec_shx::<M>,
            Microcode::exec_shy::<M>,
            Microcode::exec_sha::<M>,
            Microcode::exec_tas::<M>,
            Microcode::exec_immediate_adc::<M>,
            Microcode::exec_immediate_and::<M>,
            Microcode::exec_immediate_sbc::<M>,
            Microcode::exec_immediate_cmp::<M>,
            Microcode::exec_immediate_cpx::<M>,
            Microcode::exec_immediate_cpy::<M>,
            Microcode::exec_immediate_ora::<M>,
            Microcode::exec_immediate_eor::<M>,
            Microcode::exec_adc_zero_page::<M>,
            Microcode::exec_adc_mem::<M>,
            Microcode::exec_sbc_zero_page::<M>,
            Microcode::exec_sbc_mem::<M>,
            Microcode::exec_cmp_zero_page::<M>,
            Microcode::exec_cmp_mem::<M>,
            Microcode::exec_cpx_zero_page::<M>,
            Microcode::exec_cpx_mem::<M>,
            Microcode::exec_cpy_zero_page::<M>,
            Microcode::exec_cpy_mem::<M>,
            Microcode::exec_ora_zero_page::<M>,
            Microcode::exec_ora_mem::<M>,
            Microcode::exec_eor_zero_page::<M>,
            Microcode::exec_eor_mem::<M>,
            Microcode::exec_and_zero_page::<M>,
            Microcode::exec_and_mem::<M>,
            Microcode::exec_ane_immediate::<M>,
            Microcode::exec_lax_immediate::<M>,
            Microcode::exec_las::<M>,
            Microcode::exec_bit_zero_page::<M>,
            Microcode::exec_bit_mem::<M>,
            Microcode::exec_asl_accumulator::<M>,
            Microcode::exec_asl_zero_page::<M>,
            Microcode::exec_asl_mem::<M>,
            Microcode::exec_lsr_accumulator::<M>,
            Microcode::exec_lsr_zero_page::<M>,
            Microcode::exec_lsr_mem::<M>,
            Microcode::exec_rol_accumulator::<M>,
            Microcode::exec_rol_zero_page::<M>,
            Microcode::exec_rol_mem::<M>,
            Microcode::exec_ror_accumulator::<M>,
            Microcode::exec_ror_zero_page::<M>,
            Microcode::exec_ror_mem::<M>,
            Microcode::exec_alr_immediate::<M>,
            Microcode::exec_anc_immediate::<M>,
            Microcode::exec_arr_immediate::<M>,
            Microcode::exec_axs_immediate::<M>,
            Microcode::exec_lax_zero_page::<M>,
            Microcode::exec_lax_mem::<M>,
            Microcode::exec_sax_zero_page::<M>,
            Microcode::exec_sax_mem::<M>,
            Microcode::exec_rla_zero_page::<M>,
            Microcode::exec_rla_mem::<M>,
            Microcode::exec_dcp_zero_page::<M>,
            Microcode::exec_dcp_mem::<M>,
            Microcode::exec_isc_zero_page::<M>,
            Microcode::exec_isc_mem::<M>,
            Microcode::exec_rra_zero_page::<M>,
            Microcode::exec_rra_mem::<M>,
            Microcode::exec_slo_zero_page::<M>,
            Microcode::exec_slo_mem::<M>,
            Microcode::exec_sre_zero_page::<M>,
            Microcode::exec_sre_mem::<M>,
            Microcode::exec_indexed_h_and_jump::<M>,
            Microcode::exec_set_carry::<M>,
            Microcode::exec_set_decimal::<M>,
            Microcode::exec_set_interrupt_disabled::<M>,
            Microcode::exec_clear_carry::<M>,
            Microcode::exec_clear_decimal::<M>,
            Microcode::exec_clear_interrupt_disabled::<M>,
            Microcode::exec_clear_overflow::<M>,
            Microcode::exec_transfer_ato_x::<M>,
            Microcode::exec_transfer_xto_a::<M>,
            Microcode::exec_transfer_ato_y::<M>,
            Microcode::exec_transfer_yto_a::<M>,
            Microcode::exec_transfer_s_pto_x::<M>,
            Microcode::exec_transfer_xto_s_p::<M>,
            Microcode::exec_inc_x::<M>,
            Microcode::exec_inc_y::<M>,
            Microcode::exec_dec_x::<M>,
            Microcode::exec_dec_y::<M>,
            Microcode::exec_inc_alu_zero_page::<M>,
            Microcode::exec_inc_alu_mem::<M>,
            Microcode::exec_dec_alu_zero_page::<M>,
            Microcode::exec_dec_alu_mem::<M>,
            Microcode::exec_branch_carry_clear::<M>,
            Microcode::exec_branch_carry_set::<M>,
            Microcode::exec_branch_zero_set::<M>,
            Microcode::exec_branch_zero_clear::<M>,
            Microcode::exec_branch_negative_set::<M>,
            Microcode::exec_branch_negative_clear::<M>,
            Microcode::exec_branch_overflow_set::<M>,
            Microcode::exec_branch_overflow_clear::<M>,
            Microcode::exec_push_status_break_set::<M>,
            Microcode::exec_push_status_break_clear::<M>,
            Microcode::exec_plp::<M>,
            Microcode::exec_pop_pc_l::<M>,
            Microcode::exec_pop_pc_h::<M>,
            Microcode::exec_inc_pc::<M>,
            Microcode::exec_push_a::<M>,
            Microcode::exec_push_pcl::<M>,
            Microcode::exec_push_pch::<M>,
            Microcode::exec_pop_stack::<M>,
            Microcode::exec_update_a_from_alu::<M>,
            Microcode::exec_kill::<M>,
            Microcode::exec_load_reset_pc_l::<M>,
            Microcode::exec_load_reset_pc_h::<M>,
            Microcode::exec_load_nmi_pc_l::<M>,
            Microcode::exec_load_nmi_pc_h::<M>,
            Microcode::exec_load_pc_absolute_h::<M>,
            Microcode::exec_load_irq_pc_l::<M>,
            Microcode::exec_load_irq_pc_h::<M>,
        ]
    }

    /// Execute this micro-op by dispatching through the function-pointer
    /// table (`dispatch_table`, indexed by the `repr(u8)` discriminant).
    /// Each table entry is the per-variant handler below; behavior is a
    /// pure move of the old `match` arms, so `bus_cycle` classification
    /// (ADR-0013) is unchanged.
    pub fn exec<M: Mcu>(self, cpu: &mut Cpu, bus: &mut Bus<M>) {
        debug_assert!((self as u8 as usize) < Self::COUNT);
        Self::dispatch_table::<M>()[self as u8 as usize](cpu, bus)
    }

    fn exec_fetch_and_decode<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            fetch_and_decode(cpu, bus);
        }
    }

    fn exec_fetch_only<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.read_pc_byte(bus);
    }

    fn exec_load_a_immediate<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        load_r::<Immediate, _>(cpu, bus, Register::A);
    }

    fn exec_load_x_immediate<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        load_r::<Immediate, _>(cpu, bus, Register::X);
    }

    fn exec_load_y_immediate<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        load_r::<Immediate, _>(cpu, bus, Register::Y);
    }

    fn exec_load_a_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        load_r::<ZeroPage, _>(cpu, bus, Register::A);
    }

    fn exec_load_x_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        load_r::<ZeroPage, _>(cpu, bus, Register::X);
    }

    fn exec_load_y_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        load_r::<ZeroPage, _>(cpu, bus, Register::Y);
    }

    fn exec_load_a_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        load_r::<Mem, _>(cpu, bus, Register::A);
    }

    fn exec_load_x_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        load_r::<Mem, _>(cpu, bus, Register::X);
    }

    fn exec_load_y_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        load_r::<Mem, _>(cpu, bus, Register::Y);
    }

    fn exec_store_a_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        ZeroPage::write(cpu, bus, cpu.a);
    }

    fn exec_store_x_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        ZeroPage::write(cpu, bus, cpu.x);
    }

    fn exec_store_y_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        ZeroPage::write(cpu, bus, cpu.y);
    }

    fn exec_store_a_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Mem::write(cpu, bus, cpu.a);
    }

    fn exec_store_x_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Mem::write(cpu, bus, cpu.x);
    }

    fn exec_store_y_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Mem::write(cpu, bus, cpu.y);
    }

    fn exec_store_alu_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::write_alu(cpu, bus, ValueSource::ZeroPage);
    }

    fn exec_store_alu_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::write_alu(cpu, bus, ValueSource::Mem);
    }

    fn exec_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            let addr = cpu.inc_read_byte(bus);
            cpu.ab.set(addr as u16);
        }
    }

    fn exec_zero_page_indexed_x<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.zero_page_indexed(bus, cpu.x);
    }

    fn exec_zero_page_indexed_y<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.zero_page_indexed(bus, cpu.y);
    }

    fn exec_absolute_l<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            let low = cpu.inc_read_byte(bus);
            cpu.set_abl(low);
        }
    }

    fn exec_absolute_h<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            let high = cpu.inc_read_byte(bus);
            cpu.set_abh(high);
        }
    }

    fn exec_indexed_x_and<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.x, Microcode::AndMem);
    }

    fn exec_indexed_y_and<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.y, Microcode::AndMem);
    }

    fn exec_indexed_x_load_a<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.x, Microcode::LoadAMem);
    }

    fn exec_indexed_y_load_a<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.y, Microcode::LoadAMem);
    }

    fn exec_indexed_y_load_x<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.y, Microcode::LoadXMem);
    }

    fn exec_indexed_x_load_y<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.x, Microcode::LoadYMem);
    }

    fn exec_indexed_x_ora<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.x, Microcode::OraMem);
    }

    fn exec_indexed_y_ora<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.y, Microcode::OraMem);
    }

    fn exec_indexed_x_eor<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.x, Microcode::EorMem);
    }

    fn exec_indexed_y_eor<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.y, Microcode::EorMem);
    }

    fn exec_indexed_y_lax<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.y, Microcode::LaxMem);
    }

    fn exec_indexed_y_las<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.y, Microcode::Las);
    }

    fn exec_indexed_x_sbc<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.x, Microcode::SbcMem);
    }

    fn exec_indexed_y_sbc<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.y, Microcode::SbcMem);
    }

    fn exec_indexed_x_adc<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.x, Microcode::AdcMem);
    }

    fn exec_indexed_y_adc<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.y, Microcode::AdcMem);
    }

    fn exec_indexed_x_cmp<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.x, Microcode::CmpMem);
    }

    fn exec_indexed_y_cmp<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.y, Microcode::CmpMem);
    }

    fn exec_indexed_x_nop<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_first_clock(cpu, bus, cpu.x, Microcode::LoadIntoAluMem)
    }

    fn exec_indexed_x_load_alu_always<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_always(cpu, bus, cpu.x);
    }

    fn exec_indexed_y_load_alu_always<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_always(cpu, bus, cpu.y);
    }

    fn exec_indexed_x_store_a_always<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_always(cpu, bus, cpu.x);
    }

    fn exec_indexed_y_store_a_always<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_always(cpu, bus, cpu.y);
    }

    fn exec_indexed_y_shx_always<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_always(cpu, bus, cpu.y);
    }

    fn exec_indexed_x_shy_always<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_always(cpu, bus, cpu.x);
    }

    fn exec_indexed_y_sha_always<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_always(cpu, bus, cpu.y);
    }

    fn exec_indexed_y_tas_always<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::indexed_always(cpu, bus, cpu.y);
    }

    fn exec_nop<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.read_pc_byte(bus);
    }

    fn exec_dummy_read_at<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.read_byte(bus, cpu.dummy_read_addr);
        }
    }

    fn exec_dummy_stack_read<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            let _ = bus.mcu_mut().read_stack_page(cpu.sp);
        }
    }

    fn exec_skip_detect_interrupt<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        cpu.request_detect_interrupt = Some(false);
    }

    fn exec_indexed_l_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.indexed_l::<ZeroPage, _>(bus);
    }

    fn exec_indexed_l_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.indexed_l::<Mem, _>(bus);
    }

    fn exec_indexed_h_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.indexed_h::<ZeroPage, _>(bus);
    }

    fn exec_skip_immediate<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.inc_read_byte(bus);
        }
    }

    fn exec_load_into_alu_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.load_alu::<ZeroPage, _>(bus);
        }
    }

    fn exec_load_into_alu_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.load_alu::<Mem, _>(bus);
        }
    }

    fn exec_shx<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.shx(bus);
    }

    fn exec_shy<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.shy(bus);
    }

    fn exec_sha<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.sha(bus);
    }

    fn exec_tas<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.tas(bus);
    }

    fn exec_immediate_adc<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::immediate_with_op(cpu, bus, ImmediateOp::Adc);
    }

    fn exec_immediate_and<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::immediate_with_op(cpu, bus, ImmediateOp::And);
    }

    fn exec_immediate_sbc<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::immediate_with_op(cpu, bus, ImmediateOp::Sbc);
    }

    fn exec_immediate_cmp<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::immediate_with_op(cpu, bus, ImmediateOp::Cmp);
    }

    fn exec_immediate_cpx<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::immediate_with_op(cpu, bus, ImmediateOp::Cpx);
    }

    fn exec_immediate_cpy<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::immediate_with_op(cpu, bus, ImmediateOp::Cpy);
    }

    fn exec_immediate_ora<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::immediate_with_op(cpu, bus, ImmediateOp::Ora);
    }

    fn exec_immediate_eor<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::immediate_with_op(cpu, bus, ImmediateOp::Eor);
    }

    fn exec_adc_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.adc::<ZeroPage, _>(bus);
    }

    fn exec_adc_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.adc::<Mem, _>(bus);
    }

    fn exec_sbc_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.sbc::<ZeroPage, _>(bus);
    }

    fn exec_sbc_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.sbc::<Mem, _>(bus);
    }

    fn exec_cmp_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.cmp::<ZeroPage, _>(bus);
    }

    fn exec_cmp_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.cmp::<Mem, _>(bus);
    }

    fn exec_cpx_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.cpx::<ZeroPage, _>(bus);
    }

    fn exec_cpx_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.cpx::<Mem, _>(bus);
    }

    fn exec_cpy_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.cpy::<ZeroPage, _>(bus);
    }

    fn exec_cpy_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.cpy::<Mem, _>(bus);
    }

    fn exec_ora_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.ora::<ZeroPage, _>(bus);
    }

    fn exec_ora_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.ora::<Mem, _>(bus);
    }

    fn exec_eor_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.eor::<ZeroPage, _>(bus);
    }

    fn exec_eor_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.eor::<Mem, _>(bus);
    }

    fn exec_and_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.and::<ZeroPage, _>(bus);
    }

    fn exec_and_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.and::<Mem, _>(bus);
    }

    fn exec_ane_immediate<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            let t = cpu.inc_read_byte(bus);
            cpu.set_register(Register::A, cpu.x & t);
        }
    }

    fn exec_lax_immediate<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            let t = cpu.inc_read_byte(bus);
            cpu.a = t;
            cpu.x = t;
            cpu.update_zero_negative_flags(t);
        }
    }

    fn exec_las<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.load_alu::<Mem, _>(bus);
            let v = cpu.alu & cpu.sp;
            cpu.a = v;
            cpu.x = v;
            cpu.sp = v;
            cpu.update_zero_negative_flags(v);
        }
    }

    fn exec_bit_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.bit::<ZeroPage, _>(bus);
    }

    fn exec_bit_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.bit::<Mem, _>(bus);
    }

    fn exec_asl_accumulator<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::shift_rotate_accumulator(cpu, ShiftRotateOp::Asl);
    }

    fn exec_asl_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::shift_rotate_mem(cpu, bus, ShiftRotateOp::Asl, ValueSource::ZeroPage)
    }

    fn exec_asl_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::shift_rotate_mem(cpu, bus, ShiftRotateOp::Asl, ValueSource::Mem);
    }

    fn exec_lsr_accumulator<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::shift_rotate_accumulator(cpu, ShiftRotateOp::Lsr);
    }

    fn exec_lsr_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::shift_rotate_mem(cpu, bus, ShiftRotateOp::Lsr, ValueSource::ZeroPage)
    }

    fn exec_lsr_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::shift_rotate_mem(cpu, bus, ShiftRotateOp::Lsr, ValueSource::Mem);
    }

    fn exec_rol_accumulator<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::shift_rotate_accumulator(cpu, ShiftRotateOp::Rol);
    }

    fn exec_rol_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::shift_rotate_mem(cpu, bus, ShiftRotateOp::Rol, ValueSource::ZeroPage)
    }

    fn exec_rol_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::shift_rotate_mem(cpu, bus, ShiftRotateOp::Rol, ValueSource::Mem);
    }

    fn exec_ror_accumulator<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::shift_rotate_accumulator(cpu, ShiftRotateOp::Ror);
    }

    fn exec_ror_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::shift_rotate_mem(cpu, bus, ShiftRotateOp::Ror, ValueSource::ZeroPage)
    }

    fn exec_ror_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::shift_rotate_mem(cpu, bus, ShiftRotateOp::Ror, ValueSource::Mem);
    }

    fn exec_alr_immediate<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.alu = cpu.inc_read_byte(bus);
            cpu.alr();
        }
    }

    fn exec_anc_immediate<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.alu = cpu.inc_read_byte(bus);
            cpu.anc();
        }
    }

    fn exec_arr_immediate<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.alu = cpu.inc_read_byte(bus);
            cpu.arr();
        }
    }

    fn exec_axs_immediate<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.alu = cpu.inc_read_byte(bus);
            cpu.axs();
        }
    }

    fn exec_lax_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.lax::<ZeroPage, _>(bus);
    }

    fn exec_lax_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.lax::<Mem, _>(bus);
    }

    fn exec_sax_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.sax(bus, ValueSource::ZeroPage);
    }

    fn exec_sax_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.sax(bus, ValueSource::Mem);
    }

    fn exec_rla_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.rla(bus, ValueSource::ZeroPage);
    }

    fn exec_rla_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.rla(bus, ValueSource::Mem);
    }

    fn exec_dcp_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.dcp(bus, ValueSource::ZeroPage);
    }

    fn exec_dcp_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.dcp(bus, ValueSource::Mem);
    }

    fn exec_isc_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.isc(bus, ValueSource::ZeroPage);
    }

    fn exec_isc_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.isc(bus, ValueSource::Mem);
    }

    fn exec_rra_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.rra(bus, ValueSource::ZeroPage);
    }

    fn exec_rra_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.rra(bus, ValueSource::Mem);
    }

    fn exec_slo_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.slo(bus, ValueSource::ZeroPage);
    }

    fn exec_slo_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.slo(bus, ValueSource::Mem);
    }

    fn exec_sre_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.sre(bus, ValueSource::ZeroPage);
    }

    fn exec_sre_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.sre(bus, ValueSource::Mem);
    }

    fn exec_indexed_h_and_jump<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.indexed_h::<Mem, _>(bus);
            cpu.set_pc_to_ab()
        }
    }

    fn exec_set_carry<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        {
            cpu.set_flag(Flag::Carry, true);
        }
    }

    fn exec_set_decimal<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        {
            cpu.set_flag(Flag::Decimal, true);
        }
    }

    fn exec_set_interrupt_disabled<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        {
            cpu.set_flag(Flag::InterruptDisabled, true);
        }
    }

    fn exec_clear_carry<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        {
            cpu.set_flag(Flag::Carry, false);
        }
    }

    fn exec_clear_decimal<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        {
            cpu.set_flag(Flag::Decimal, false);
        }
    }

    fn exec_clear_interrupt_disabled<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        {
            cpu.set_flag(Flag::InterruptDisabled, false);
        }
    }

    fn exec_clear_overflow<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        {
            cpu.set_flag(Flag::Overflow, false);
        }
    }

    fn exec_transfer_ato_x<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::transfer(cpu, TransferDirection::AtoX);
    }

    fn exec_transfer_xto_a<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::transfer(cpu, TransferDirection::XtoA);
    }

    fn exec_transfer_ato_y<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::transfer(cpu, TransferDirection::AtoY);
    }

    fn exec_transfer_yto_a<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::transfer(cpu, TransferDirection::YtoA);
    }

    fn exec_transfer_s_pto_x<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::transfer(cpu, TransferDirection::SPtoX);
    }

    fn exec_transfer_xto_s_p<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::transfer(cpu, TransferDirection::XtoSP);
    }

    fn exec_inc_x<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::inc_dec_register(cpu, Register::X, cpu.x.wrapping_add(1));
    }

    fn exec_inc_y<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::inc_dec_register(cpu, Register::Y, cpu.y.wrapping_add(1));
    }

    fn exec_dec_x<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::inc_dec_register(cpu, Register::X, cpu.x.wrapping_sub(1));
    }

    fn exec_dec_y<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        Microcode::inc_dec_register(cpu, Register::Y, cpu.y.wrapping_sub(1));
    }

    fn exec_inc_alu_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.alu = cpu.alu.wrapping_add(1);
            Microcode::write_alu(cpu, bus, ValueSource::ZeroPage);
            cpu.update_zero_negative_flags(cpu.alu);
        }
    }

    fn exec_inc_alu_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.alu = cpu.alu.wrapping_add(1);
            Microcode::write_alu(cpu, bus, ValueSource::Mem);
            cpu.update_zero_negative_flags(cpu.alu);
        }
    }

    fn exec_dec_alu_zero_page<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.alu = cpu.alu.wrapping_sub(1);
            Microcode::write_alu(cpu, bus, ValueSource::ZeroPage);
            cpu.update_zero_negative_flags(cpu.alu);
        }
    }

    fn exec_dec_alu_mem<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            cpu.alu = cpu.alu.wrapping_sub(1);
            Microcode::write_alu(cpu, bus, ValueSource::Mem);
            cpu.update_zero_negative_flags(cpu.alu);
        }
    }

    fn exec_branch_carry_clear<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::branch_relative(cpu, bus, BranchTest::IfCarryClear.test(cpu))
    }

    fn exec_branch_carry_set<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::branch_relative(cpu, bus, BranchTest::IfCarrySet.test(cpu))
    }

    fn exec_branch_zero_set<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::branch_relative(cpu, bus, BranchTest::IfZeroSet.test(cpu));
    }

    fn exec_branch_zero_clear<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::branch_relative(cpu, bus, BranchTest::IfZeroClear.test(cpu))
    }

    fn exec_branch_negative_set<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::branch_relative(cpu, bus, BranchTest::IfNegativeSet.test(cpu))
    }

    fn exec_branch_negative_clear<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::branch_relative(cpu, bus, BranchTest::IfNegativeClear.test(cpu))
    }

    fn exec_branch_overflow_set<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::branch_relative(cpu, bus, BranchTest::IfOverflowSet.test(cpu))
    }

    fn exec_branch_overflow_clear<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        Microcode::branch_relative(cpu, bus, BranchTest::IfOverflowClear.test(cpu))
    }

    fn exec_push_status_break_set<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.push_status(bus, true);
    }

    fn exec_push_status_break_clear<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.push_status(bus, false);
    }

    fn exec_plp<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.plp(bus);
    }

    fn exec_pop_pc_l<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            let low = cpu.pop_stack(bus);
            cpu.pc.set_low(low);
        }
    }

    fn exec_pop_pc_h<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        {
            let high = cpu.pop_stack(bus);
            cpu.pc.set_high(high);
        }
    }

    fn exec_inc_pc<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        cpu.pc += 1;
    }

    fn exec_push_a<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.push_stack(bus, cpu.a);
    }

    fn exec_push_pcl<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.push_stack(bus, cpu.pc.low());
    }

    fn exec_push_pch<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.push_stack(bus, cpu.pc.high());
    }

    fn exec_pop_stack<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.alu = cpu.pop_stack(bus);
    }

    fn exec_update_a_from_alu<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        cpu.set_register(Register::A, cpu.alu);
    }

    fn exec_kill<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) {
        cpu.halt();
    }

    fn exec_load_reset_pc_l<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.load_vector_low(bus, 0xFFFC, false);
    }

    fn exec_load_reset_pc_h<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.load_vector_high(bus, 0xFFFD);
    }

    fn exec_load_nmi_pc_l<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.load_nmi_pcl(bus);
    }

    fn exec_load_nmi_pc_h<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.load_nmi_pch(bus);
    }

    fn exec_load_pc_absolute_h<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        load_pc_absolute_h(cpu, bus);
    }

    fn exec_load_irq_pc_l<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.load_irq_pcl(bus);
    }

    fn exec_load_irq_pc_h<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
        cpu.load_irq_pch(bus);
    }
    fn branch_relative<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>, branch_test_result: bool) {
        let offset = cpu.inc_read_byte(bus);
        cpu.request_detect_interrupt = Some(true);
        if branch_test_result {
            let old_pch = cpu.pch();
            cpu.pc.wrapping_add((offset as i8) as u16);
            if old_pch != cpu.pch() {
                // Taken with page cross: cycle 3 re-reads PC (already final
                // in this model), cycle 4 drives the page-fault emulation
                // address (old PCH | new PBL) while PCH is fixed.
                cpu.retain_cycle();
                cpu.dummy_read_addr = ((old_pch as u16) << 8) | cpu.pc.low() as u16;
                cpu.push_microcode(Microcode::DummyReadAt);
            } else {
                cpu.push_microcode(Microcode::SkipDetectInterrupt);
            }
        }
    }

    fn indexed_always<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>, idx: u8) {
        let abh = cpu.abh();
        cpu.ab.wrapping_add(idx as u16);
        let dummy_addr = (u16::from(abh) << 8) | u16::from(cpu.abl());
        cpu.read_byte(bus, dummy_addr);
    }

    fn indexed_first_clock<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>, idx: u8, follow: Microcode) {
        let abh = cpu.abh();
        cpu.ab.wrapping_add(idx as u16);
        if abh != cpu.abh() {
            let dummy_addr = (u16::from(abh) << 8) | u16::from(cpu.abl());
            cpu.read_byte(bus, dummy_addr);
            cpu.push_microcode(follow);
        } else {
            follow.exec(cpu, bus);
        }
    }

    fn immediate_with_op<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>, op: ImmediateOp) {
        cpu.alu = cpu.inc_read_byte(bus);
        op.exec(cpu, bus);
    }

    #[inline(always)]
    fn transfer(cpu: &mut Cpu, direction: TransferDirection) {
        match direction {
            TransferDirection::AtoX => cpu.set_register(Register::X, cpu.a),
            TransferDirection::XtoA => cpu.set_register(Register::A, cpu.x),
            TransferDirection::AtoY => cpu.set_register(Register::Y, cpu.a),
            TransferDirection::YtoA => cpu.set_register(Register::A, cpu.y),
            TransferDirection::SPtoX => cpu.set_register(Register::X, cpu.sp),
            TransferDirection::XtoSP => {
                cpu.sp = cpu.x;
            }
        }
    }

    /// Writes the ALU latch through the write route carried by the
    /// micro-op — the one place an ALU result reaches memory. The zero-page
    /// RMW sequences are built with `ValueSource::ZeroPage`, so the second,
    /// real write of an `INC`/`DEC`/shift/rotate at `$zp` skips the bus
    /// address-range match exactly as the read and the dummy write already
    /// do. Everything else — absolute, absolute-indexed, and the register
    /// forms, which never reach here — writes through the generic path.
    fn write_alu<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>, src: ValueSource) {
        cpu.write_value(bus, src, cpu.alu);
    }

    /// Write an index register and mirror the new value into the ALU latch.
    #[inline]
    fn inc_dec_register(cpu: &mut Cpu, register: Register, value: u8) {
        cpu.set_register(register, value);
        cpu.alu = value;
    }

    fn shift_rotate_calc(cpu: &mut Cpu, op: ShiftRotateOp) {
        match op {
            ShiftRotateOp::Asl => {
                cpu.set_flag(Flag::Carry, cpu.alu & 0x80 != 0);
                cpu.alu <<= 1;
            }
            ShiftRotateOp::Lsr => {
                cpu.set_flag(Flag::Carry, cpu.alu & 0x01 != 0);
                cpu.alu >>= 1;
            }
            ShiftRotateOp::Rol => {
                let carry = cpu.alu & 0x80 != 0;
                cpu.alu = (cpu.alu << 1) | (cpu.flag(Flag::Carry) as u8);
                cpu.set_flag(Flag::Carry, carry);
            }
            ShiftRotateOp::Ror => {
                let carry = cpu.alu & 0x01 != 0;
                cpu.alu = (cpu.alu >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7);
                cpu.set_flag(Flag::Carry, carry);
            }
        }
        cpu.update_zero_negative_flags(cpu.alu);
    }

    fn shift_rotate_accumulator(cpu: &mut Cpu, op: ShiftRotateOp) {
        cpu.alu = cpu.a;
        Self::shift_rotate_calc(cpu, op);
        cpu.a = cpu.alu;
    }

    fn shift_rotate_mem<M: Mcu>(
        cpu: &mut Cpu,
        bus: &mut Bus<M>,
        op: ShiftRotateOp,
        src: ValueSource,
    ) {
        Self::shift_rotate_calc(cpu, op);
        Self::write_alu(cpu, bus, src);
    }
}

fn load_r<S: ValueSourceTrait, M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>, r: Register) {
    let value = S::value(cpu, bus);
    cpu.set_register(r, value);
}

fn load_pc_absolute_h<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) {
    let high = cpu.inc_read_byte(bus);
    cpu.pc.set_high(high);
    cpu.pc.set_low(cpu.ab.low());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dispatch_table_covers_every_variant() {
        // Discriminants are dense from 0: the table must hold exactly one
        // handler per variant so `exec` can index it by `self as u8`.
        assert_eq!(Microcode::COUNT, 175);
        assert_eq!(Microcode::LoadIrqPcH as u8 as usize + 1, Microcode::COUNT);
    }

    /// Golden captured 2026-08-28 from the blargg-green pre-refactor table
    /// (ADR-0006): for each opcode, the CPU cycle — counting the fetch as
    /// cycle 1 — whose tick performs the standard end-of-instruction
    /// interrupt poll (`1 + sequence length`). 0 marks a suppressed poll:
    /// BRK recognizes through its declared hijack sites instead.
    const GOLDEN_POLL_CYCLE: [u8; 256] = [
        0, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4,
        7, 7, 6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7,
        4, 4, 7, 7, 6, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4,
        2, 7, 4, 4, 7, 7, 6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6,
        2, 4, 2, 7, 4, 4, 7, 7, 2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, 2, 6, 2, 6, 4, 4,
        4, 4, 2, 5, 2, 5, 5, 5, 5, 5, 2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, 2, 5, 2, 5,
        4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4, 2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6, 2, 5,
        2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, 2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
        2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    ];

    #[test]
    fn per_opcode_poll_cycle_matches_golden() {
        let table = build_opcode_table();
        for (op, &expected) in GOLDEN_POLL_CYCLE.iter().enumerate() {
            let suppressed = InterruptSequences::window_for_opcode(op as u8).is_some();
            let actual = if suppressed {
                0
            } else {
                table[op].len() as u8 + 1
            };
            assert_eq!(actual, expected, "opcode {op:#04x} poll cycle drifted");
        }
    }

    #[test]
    fn branch_opcodes_poll_through_the_shifted_mechanism() {
        // The taken-branch poll shift — poll on the branch's second cycle;
        // final poll suppressed without a page cross — stays a runtime
        // mechanism (ADR-0006) whose observable timing is pinned by
        // `just branch_timing_tests`. This walk pins which opcodes carry
        // the mechanism; the golden's 2 for branches is the shared
        // final-cycle rule (fetch + branch cycle), not the shift itself.
        const BRANCHES: [u8; 8] = [0x10, 0x30, 0x50, 0x70, 0x90, 0xB0, 0xD0, 0xF0];
        let table = build_opcode_table();
        for op in BRANCHES {
            assert!(
                matches!(
                    table[op as usize][0],
                    Microcode::BranchCarryClear
                        | Microcode::BranchCarrySet
                        | Microcode::BranchZeroSet
                        | Microcode::BranchZeroClear
                        | Microcode::BranchNegativeSet
                        | Microcode::BranchNegativeClear
                        | Microcode::BranchOverflowSet
                        | Microcode::BranchOverflowClear
                ),
                "{op:#04x} must open with the branch micro-op"
            );
            assert_eq!(GOLDEN_POLL_CYCLE[op as usize], 2, "{op:#04x}");
        }
        for (op, seq) in table.iter().enumerate() {
            let is_branch = matches!(
                seq[0],
                Microcode::BranchCarryClear
                    | Microcode::BranchCarrySet
                    | Microcode::BranchZeroSet
                    | Microcode::BranchZeroClear
                    | Microcode::BranchNegativeSet
                    | Microcode::BranchNegativeClear
                    | Microcode::BranchOverflowSet
                    | Microcode::BranchOverflowClear
            );
            assert_eq!(
                is_branch,
                BRANCHES.contains(&(op as u8)),
                "{op:#04x} branch-mechanism membership drifted"
            );
        }
    }

    /// Golden captured 2026-08-28 from the blargg-green behavior
    /// (`nmi_hijacks_brk_only_through_its_third_cycle` pins the BRK
    /// boundary at dot granularity; the IRQ values ride the same
    /// conversion and the cpu_interrupts_v2 ROM gate). Deadlines are dots
    /// from sequence start — the cycle executing micro-op 0.
    #[test]
    fn interrupt_windows_match_golden() {
        let brk = InterruptSequences::BRK_WINDOW;
        assert_eq!(brk.sequence_cycles, 6);
        assert_eq!(
            brk.hijack_sites,
            &[
                HijackSite {
                    op_index: 3,
                    deadline_dots: 4,
                },
                HijackSite {
                    op_index: 4,
                    deadline_dots: 5,
                },
            ]
        );

        let irq = InterruptSequences::IRQ_WINDOW;
        assert_eq!(irq.sequence_cycles, 7);
        assert_eq!(
            irq.hijack_sites,
            &[
                HijackSite {
                    op_index: 4,
                    deadline_dots: 7,
                },
                HijackSite {
                    op_index: 5,
                    deadline_dots: 10,
                },
            ]
        );

        assert_eq!(InterruptSequences::NMI_WINDOW.sequence_cycles, 7);
        assert_eq!(InterruptSequences::RESET_WINDOW.sequence_cycles, 8);
        for window in [
            &InterruptSequences::NMI_WINDOW,
            &InterruptSequences::RESET_WINDOW,
        ] {
            assert_eq!(window.hijack_sites, &[]);
        }
    }

    #[test]
    fn interrupt_windows_are_consistent_with_sequences() {
        let brk = InterruptSequences::BRK;
        let windows: [(&[Microcode], &InterruptWindow); 4] = [
            (
                &InterruptSequences::RESET,
                &InterruptSequences::RESET_WINDOW,
            ),
            (&InterruptSequences::NMI, &InterruptSequences::NMI_WINDOW),
            (&InterruptSequences::IRQ, &InterruptSequences::IRQ_WINDOW),
            (brk.as_slice(), &InterruptSequences::BRK_WINDOW),
        ];
        for (ops, window) in windows {
            assert_eq!(usize::from(window.sequence_cycles), ops.len());
            for site in window.hijack_sites {
                let op = &ops[site.op_index as usize];
                if matches!(
                    op,
                    Microcode::PushStatusBreakSet | Microcode::PushStatusBreakClear
                ) {
                    // Latched-only cutoff = site tick − 5: a rise is
                    // latched strictly before the take tick iff it
                    // rose by then (2-dot sample lag plus the one full
                    // cycle between the last sampler tick and the
                    // take), see `NmiDetector` sampling.
                    assert_eq!(
                        u64::from(site.deadline_dots),
                        3 * u64::from(site.op_index) - 5
                    );
                } else {
                    assert!(matches!(op, Microcode::LoadIrqPcL));
                }
            }
            // Structural invariant: every suppressing sequence ends with a
            // vector-fetch op. (Runtime suppression now reads the armed
            // window, not op identity.)
            assert!(matches!(
                ops.last().unwrap(),
                Microcode::LoadResetPcH | Microcode::LoadNmiPcH | Microcode::LoadIrqPcH
            ));
        }
    }
}
