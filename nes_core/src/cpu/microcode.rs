use super::{Cpu, Flag, Register};
use crate::mcu::Mcu;
use tinyvec::ArrayVec;

macro_rules! microcode_arr {
    ($item1:expr) => {
        match ArrayVec::try_from_array_len(
            [
                $item1,
                Microcode::Nop,
                Microcode::Nop,
                Microcode::Nop,
                Microcode::Nop,
                Microcode::Nop,
                Microcode::Nop,
            ],
            1,
        ) {
            Ok(v) => v,
            Err(_) => panic!(),
        }
    };
    ($item1:expr, $item2:expr) => {
        match ArrayVec::try_from_array_len(
            [
                $item1,
                $item2,
                Microcode::Nop,
                Microcode::Nop,
                Microcode::Nop,
                Microcode::Nop,
                Microcode::Nop,
            ],
            2,
        ) {
            Ok(v) => v,
            Err(_) => panic!(),
        }
    };
    ($item1:expr, $item2:expr, $item3:expr) => {
        match ArrayVec::try_from_array_len(
            [
                $item1,
                $item2,
                $item3,
                Microcode::Nop,
                Microcode::Nop,
                Microcode::Nop,
                Microcode::Nop,
            ],
            3,
        ) {
            Ok(v) => v,
            Err(_) => panic!(),
        }
    };
    ($item1:expr, $item2:expr, $item3:expr, $item4:expr) => {
        match ArrayVec::try_from_array_len(
            [
                $item1,
                $item2,
                $item3,
                $item4,
                Microcode::Nop,
                Microcode::Nop,
                Microcode::Nop,
            ],
            4,
        ) {
            Ok(v) => v,
            Err(_) => panic!(),
        }
    };
    ($item1:expr, $item2:expr, $item3:expr, $item4:expr, $item5:expr) => {
        match ArrayVec::try_from_array_len(
            [
                $item1,
                $item2,
                $item3,
                $item4,
                $item5,
                Microcode::Nop,
                Microcode::Nop,
            ],
            5,
        ) {
            Ok(v) => v,
            Err(_) => panic!(),
        }
    };
    ($item1:expr, $item2:expr, $item3:expr, $item4:expr, $item5:expr, $item6:expr) => {
        match ArrayVec::try_from_array_len(
            [
                $item1,
                $item2,
                $item3,
                $item4,
                $item5,
                $item6,
                Microcode::Nop,
            ],
            6,
        ) {
            Ok(v) => v,
            Err(_) => panic!(),
        }
    };
    ($item1:expr, $item2:expr, $item3:expr, $item4:expr, $item5:expr, $item6:expr, $item7:expr) => {
        match ArrayVec::try_from_array_len(
            [$item1, $item2, $item3, $item4, $item5, $item6, $item7],
            7,
        ) {
            Ok(v) => v,
            Err(_) => panic!(),
        }
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

const fn absolute_indexed_x_op(
    op: OpAfterAddressing,
    first_clock: CrossPageBehavior,
) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH,
        Microcode::IndexedXWithOp { op, first_clock }
    )
}

const fn absolute_indexed_y_op(
    op: OpAfterAddressing,
    first_clock: CrossPageBehavior,
) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH,
        Microcode::IndexedYWithOp { op, first_clock }
    )
}

const fn indexed_indirect_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::ZeroPageIndexedX,
        Microcode::IndexedL,
        Microcode::IndexedH,
        op
    )
}

const fn indirect_indexed_op(
    op: OpAfterAddressing,
    first_clock: CrossPageBehavior,
) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::IndexedL,
        Microcode::IndexedH,
        Microcode::IndexedYWithOp { op, first_clock }
    )
}

const fn zero_page_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::LoadIntoAlu,
        Microcode::StoreAlu,
        op
    )
}

const fn zero_page_x_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::ZeroPageIndexedX,
        Microcode::LoadIntoAlu,
        Microcode::StoreAlu,
        op
    )
}

const fn absolute_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH,
        Microcode::LoadIntoAlu,
        Microcode::StoreAlu,
        op
    )
}

const fn absolute_indexed_x_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH,
        Microcode::IndexedXWithOp {
            op: OpAfterAddressing::LoadIntoAlu,
            first_clock: CrossPageBehavior::FirstClockAlways
        },
        Microcode::LoadIntoAlu,
        Microcode::StoreAlu,
        op
    )
}

const fn absolute_indexed_y_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH,
        Microcode::IndexedYWithOp {
            op: OpAfterAddressing::LoadIntoAlu,
            first_clock: CrossPageBehavior::FirstClockAlways
        },
        Microcode::LoadIntoAlu,
        Microcode::StoreAlu,
        op
    )
}

const fn indexed_indirect_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::ZeroPageIndexedX,
        Microcode::IndexedL,
        Microcode::IndexedH,
        Microcode::LoadIntoAlu,
        Microcode::StoreAlu,
        op
    )
}

const fn indirect_indexed_rmw_op(op: Microcode) -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::IndexedL,
        Microcode::IndexedH,
        Microcode::IndexedYWithOp {
            op: OpAfterAddressing::LoadIntoAlu,
            first_clock: CrossPageBehavior::FirstClockAlways
        },
        Microcode::LoadIntoAlu,
        Microcode::StoreAlu,
        op
    )
}

const fn absolute_indexed_x_store_a() -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH,
        Microcode::IndexedXWithOp {
            op: OpAfterAddressing::StoreA,
            first_clock: CrossPageBehavior::FirstClockAlways
        },
        Microcode::StoreR(Register::A)
    )
}

const fn absolute_indexed_y_store_a() -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH,
        Microcode::IndexedYWithOp {
            op: OpAfterAddressing::StoreA,
            first_clock: CrossPageBehavior::FirstClockAlways
        },
        Microcode::StoreR(Register::A)
    )
}

const fn indirect_indexed_store_a() -> ArrayVec<[Microcode; 7]> {
    microcode_arr!(
        Microcode::ZeroPage,
        Microcode::IndexedL,
        Microcode::IndexedH,
        Microcode::IndexedYWithOp {
            op: OpAfterAddressing::StoreA,
            first_clock: CrossPageBehavior::FirstClockAlways
        },
        Microcode::StoreR(Register::A)
    )
}

const fn build_opcode_table() -> [ArrayVec<[Microcode; 7]>; 256] {
    use super::Flag::*;
    use AOrMemory::*;
    use Microcode::*;
    use Register::*;
    use opcode::*;

    let mut r = include!("init_microtable.inc.rs");
    r[AND_IMMEDIATE as usize] = microcode_arr!(ImmediateWithOp(ImmediateOp::And));
    r[AND_ZERO_PAGE as usize] = zero_page_op(And);
    r[AND_ZERO_PAGE_X as usize] = zero_page_x_op(And);
    r[AND_ABSOLUTE as usize] = absolute_op(And);
    r[AND_ABSOLUTE_INDEXED_X as usize] =
        absolute_indexed_x_op(OpAfterAddressing::And, CrossPageBehavior::FirstClock);
    r[AND_ABSOLUTE_INDEXED_Y as usize] =
        absolute_indexed_y_op(OpAfterAddressing::And, CrossPageBehavior::FirstClock);
    r[AND_INDEXED_INDIRECT as usize] = indexed_indirect_op(And);
    r[AND_INDIRECT_INDEXED as usize] =
        indirect_indexed_op(OpAfterAddressing::And, CrossPageBehavior::FirstClock);
    r[LDA_IMMEDIATE as usize] = microcode_arr!(LoadImmediate(Register::A));
    r[LDA_ZERO_PAGE as usize] = zero_page_op(LoadR(A));
    r[LDA_ZERO_PAGE_X as usize] = zero_page_x_op(LoadR(A));
    r[LDA_ABSOLUTE as usize] = absolute_op(LoadR(A));
    r[LDA_ABSOLUTE_INDEXED_X as usize] =
        absolute_indexed_x_op(OpAfterAddressing::LoadIntoA, CrossPageBehavior::FirstClock);
    r[LDA_ABSOLUTE_INDEXED_Y as usize] =
        absolute_indexed_y_op(OpAfterAddressing::LoadIntoA, CrossPageBehavior::FirstClock);
    r[LDA_INDIRECT_INDEXED as usize] = indexed_indirect_op(LoadR(A));
    r[LDA_INDIRECT_INDEXED_Y as usize] =
        indirect_indexed_op(OpAfterAddressing::LoadIntoA, CrossPageBehavior::FirstClock);
    r[LDX_IMMEDIATE as usize] = microcode_arr!(LoadImmediate(Register::X));
    r[LDX_ZERO_PAGE as usize] = zero_page_op(LoadR(X));
    r[LDX_ZERO_PAGE_Y as usize] = zero_page_y_op(LoadR(X));
    r[LDX_ABSOLUTE as usize] = absolute_op(LoadR(X));
    r[LDX_ABSOLUTE_INDEXED_Y as usize] =
        absolute_indexed_y_op(OpAfterAddressing::LoadIntoX, CrossPageBehavior::FirstClock);
    r[LDY_IMMEDIATE as usize] = microcode_arr!(LoadImmediate(Register::Y));
    r[LDY_ZERO_PAGE as usize] = zero_page_op(LoadR(Y));
    r[LDY_ZERO_PAGE_X as usize] = zero_page_x_op(LoadR(Y));
    r[LDY_ABSOLUTE as usize] = absolute_op(LoadR(Y));
    r[LDY_ABSOLUTE_INDEXED_X as usize] =
        absolute_indexed_x_op(OpAfterAddressing::LoadIntoY, CrossPageBehavior::FirstClock);
    r[STA_ZERO_PAGE as usize] = zero_page_op(StoreR(A));
    r[STA_ZERO_PAGE_X as usize] = zero_page_x_op(StoreR(A));
    r[STA_ABSOLUTE as usize] = absolute_op(StoreR(A));
    r[STA_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_store_a();
    r[STA_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_store_a();
    r[STA_INDEXED_INDIRECT as usize] = indexed_indirect_op(StoreR(A));
    r[STA_INDIRECT_INDEXED_Y as usize] = indirect_indexed_store_a();
    r[STX_ZERO_PAGE as usize] = zero_page_op(StoreR(X));
    r[STX_ZERO_PAGE_Y as usize] = zero_page_y_op(StoreR(X));
    r[STX_ABSOLUTE as usize] = absolute_op(StoreR(X));
    r[STY_ZERO_PAGE as usize] = zero_page_op(StoreR(Y));
    r[STY_ZERO_PAGE_X as usize] = zero_page_x_op(StoreR(Y));
    r[STY_ABSOLUTE as usize] = absolute_op(StoreR(Y));
    r[BIT_ZERO_PAGE as usize] = zero_page_op(Bit);
    r[BIT_ABSOLUTE as usize] = absolute_op(Bit);
    r[ADC_IMMEDIATE as usize] = microcode_arr!(ImmediateWithOp(ImmediateOp::Adc));
    r[ADC_ZERO_PAGE as usize] = zero_page_op(Adc);
    r[ADC_ZERO_PAGE_X as usize] = zero_page_x_op(Adc);
    r[ADC_ABSOLUTE as usize] = absolute_op(Adc);
    r[ADC_ABSOLUTE_INDEXED_X as usize] =
        absolute_indexed_x_op(OpAfterAddressing::Adc, CrossPageBehavior::FirstClock);
    r[ADC_ABSOLUTE_INDEXED_Y as usize] =
        absolute_indexed_y_op(OpAfterAddressing::Adc, CrossPageBehavior::FirstClock);
    r[ADC_INDEXED_INDIRECT as usize] = indexed_indirect_op(Adc);
    r[ADC_INDIRECT_INDEXED as usize] =
        indirect_indexed_op(OpAfterAddressing::Adc, CrossPageBehavior::FirstClock);
    r[ASL_ACCUMULATOR as usize] = microcode_arr!(Asl(Accumulator));
    r[ASL_ZERO_PAGE as usize] = zero_page_rmw_op(Asl(Memory));
    r[ASL_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(Asl(Memory));
    r[ASL_ABSOLUTE as usize] = absolute_rmw_op(Asl(Memory));
    r[ASL_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(Asl(Memory));
    r[LSR_ACCUMULATOR as usize] = microcode_arr!(Lsr(Accumulator));
    r[LSR_ZERO_PAGE as usize] = zero_page_rmw_op(Lsr(Memory));
    r[LSR_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(Lsr(Memory));
    r[LSR_ABSOLUTE as usize] = absolute_rmw_op(Lsr(Memory));
    r[LSR_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(Lsr(Memory));
    r[ROL_ACCUMULATOR as usize] = microcode_arr!(Rol(Accumulator));
    r[ROL_ZERO_PAGE as usize] = zero_page_rmw_op(Rol(Memory));
    r[ROL_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(Rol(Memory));
    r[ROL_ABSOLUTE as usize] = absolute_rmw_op(Rol(Memory));
    r[ROL_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(Rol(Memory));
    r[ROR_ACCUMULATOR as usize] = microcode_arr!(Ror(Accumulator));
    r[ROR_ZERO_PAGE as usize] = zero_page_rmw_op(Ror(Memory));
    r[ROR_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(Ror(Memory));
    r[ROR_ABSOLUTE as usize] = absolute_rmw_op(Ror(Memory));
    r[ROR_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(Ror(Memory));
    r[BCC as usize] = microcode_arr!(BranchRelative(BranchTest::IfCarryClear));
    r[BCS as usize] = microcode_arr!(BranchRelative(BranchTest::IfCarrySet));
    r[BEQ as usize] = microcode_arr!(BranchRelative(BranchTest::IfZeroSet));
    r[BMI as usize] = microcode_arr!(BranchRelative(BranchTest::IfNegativeSet));
    r[BNE as usize] = microcode_arr!(BranchRelative(BranchTest::IfZeroClear));
    r[BPL as usize] = microcode_arr!(BranchRelative(BranchTest::IfNegativeClear));
    r[BVC as usize] = microcode_arr!(BranchRelative(BranchTest::IfOverflowClear));
    r[BVS as usize] = microcode_arr!(BranchRelative(BranchTest::IfOverflowSet));
    r[PHA as usize] = microcode_arr!(Nop, Pha);
    r[PLA as usize] = microcode_arr!(Nop, PopStack, UpdateAFromAlu);
    r[PHP as usize] = microcode_arr!(
        Nop,
        PushStatus {
            set_disable_interrupt: false,
            break_flag: true
        }
    );
    r[PLP as usize] = microcode_arr!(Nop, Nop, Plp);
    r[JMP_ABSOLUTE as usize] = microcode_arr!(AbsoluteL, LoadPcAbsoluteH);
    r[JMP_INDIRECT as usize] = microcode_arr!(AbsoluteL, AbsoluteH, IndexedL, IndexedHAndJump);
    r[JSR as usize] = microcode_arr!(AbsoluteL, Nop, PushPcH, PushPcL, LoadPcAbsoluteH);
    r[RTS as usize] = microcode_arr!(SkipImmediate, Nop, PopPcL, PopPcH, IncPc);
    r[RTI as usize] = microcode_arr!(SkipImmediate, Nop, Plp, PopPcL, PopPcH);
    r[CLC as usize] = microcode_arr!(ClearFlag(Carry));
    r[SEC as usize] = microcode_arr!(SetFlag(Carry));
    r[CLD as usize] = microcode_arr!(ClearFlag(Decimal));
    r[SED as usize] = microcode_arr!(SetFlag(Decimal));
    r[CLI as usize] = microcode_arr!(ClearFlag(InterruptDisabled));
    r[SEI as usize] = microcode_arr!(SetFlag(InterruptDisabled));
    r[CLV as usize] = microcode_arr!(ClearFlag(Overflow));
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
    r[NOP_ZERO_PAGE1 as usize] = microcode_arr!(SkipImmediate, Nop);
    r[NOP_ZERO_PAGE2 as usize] = microcode_arr!(SkipImmediate, Nop);
    r[NOP_ZERO_PAGE3 as usize] = microcode_arr!(SkipImmediate, Nop);
    r[NOP_ZERO_PAGE_X1 as usize] = zero_page_x_op(Nop);
    r[NOP_ZERO_PAGE_X2 as usize] = zero_page_x_op(Nop);
    r[NOP_ZERO_PAGE_X3 as usize] = zero_page_x_op(Nop);
    r[NOP_ZERO_PAGE_X4 as usize] = zero_page_x_op(Nop);
    r[NOP_ZERO_PAGE_X5 as usize] = zero_page_x_op(Nop);
    r[NOP_ZERO_PAGE_X6 as usize] = zero_page_x_op(Nop);
    r[NOP_ABSOLUTE as usize] = absolute_op(Nop);
    r[NOP_ABSOLUTE_INDEXED_X1 as usize] =
        absolute_indexed_x_op(OpAfterAddressing::Nop, CrossPageBehavior::FirstClock);
    r[NOP_ABSOLUTE_INDEXED_X2 as usize] =
        absolute_indexed_x_op(OpAfterAddressing::Nop, CrossPageBehavior::FirstClock);
    r[NOP_ABSOLUTE_INDEXED_X3 as usize] =
        absolute_indexed_x_op(OpAfterAddressing::Nop, CrossPageBehavior::FirstClock);
    r[NOP_ABSOLUTE_INDEXED_X4 as usize] =
        absolute_indexed_x_op(OpAfterAddressing::Nop, CrossPageBehavior::FirstClock);
    r[NOP_ABSOLUTE_INDEXED_X5 as usize] =
        absolute_indexed_x_op(OpAfterAddressing::Nop, CrossPageBehavior::FirstClock);
    r[NOP_ABSOLUTE_INDEXED_X6 as usize] =
        absolute_indexed_x_op(OpAfterAddressing::Nop, CrossPageBehavior::FirstClock);
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
    r[BRK as usize] = microcode_arr!(
        SkipImmediate,
        PushPcH,
        PushPcL,
        PushStatus {
            break_flag: true,
            set_disable_interrupt: true,
        },
        LoadIrqPcL,
        LoadIrqPcH
    );
    r[SBC_IMMEDIATE as usize] = microcode_arr!(ImmediateWithOp(ImmediateOp::Sbc));
    r[USBC as usize] = microcode_arr!(ImmediateWithOp(ImmediateOp::Sbc));
    r[SBC_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, Sbc);
    r[SBC_ZERO_PAGE_X as usize] = zero_page_x_op(Sbc);
    r[SBC_ABSOLUTE as usize] = absolute_op(Sbc);
    r[SBC_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedXWithOp {
            op: OpAfterAddressing::Sbc,
            first_clock: CrossPageBehavior::FirstClock
        }
    );
    r[SBC_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedYWithOp {
            op: OpAfterAddressing::Sbc,
            first_clock: CrossPageBehavior::FirstClock
        }
    );
    r[SBC_INDEXED_INDIRECT as usize] = indexed_indirect_op(Sbc);
    r[SBC_INDIRECT_INDEXED as usize] =
        indirect_indexed_op(OpAfterAddressing::Sbc, CrossPageBehavior::FirstClock);
    r[CMP_IMMEDIATE as usize] = microcode_arr!(ImmediateWithOp(ImmediateOp::Cmp));
    r[CMP_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, Cmp);
    r[CMP_ZERO_PAGE_X as usize] = zero_page_x_op(Cmp);
    r[CMP_ABSOLUTE as usize] = absolute_op(Cmp);
    r[CMP_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedXWithOp {
            op: OpAfterAddressing::Cmp,
            first_clock: CrossPageBehavior::FirstClock
        }
    );
    r[CMP_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedYWithOp {
            op: OpAfterAddressing::Cmp,
            first_clock: CrossPageBehavior::FirstClock
        }
    );
    r[CMP_INDEXED_INDIRECT as usize] = microcode_arr!(
        ZeroPage,
        LoadIntoAlu, // dummy read
        ZeroPageIndexedX,
        IndexedL,
        IndexedH,
        Cmp
    );
    r[CMP_INDIRECT_INDEXED as usize] =
        indirect_indexed_op(OpAfterAddressing::Cmp, CrossPageBehavior::FirstClock);
    r[CPX_IMMEDIATE as usize] = microcode_arr!(ImmediateWithOp(ImmediateOp::Cpx));
    r[CPX_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, Cpx);
    r[CPX_ABSOLUTE as usize] = absolute_op(Cpx);
    r[CPY_IMMEDIATE as usize] = microcode_arr!(ImmediateWithOp(ImmediateOp::Cpy));
    r[CPY_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, Cpy);
    r[CPY_ABSOLUTE as usize] = absolute_op(Cpy);
    r[TAX as usize] = microcode_arr!(Transfer(TransferDirection::AtoX));
    r[TXA as usize] = microcode_arr!(Transfer(TransferDirection::XtoA));
    r[TAY as usize] = microcode_arr!(Transfer(TransferDirection::AtoY));
    r[TYA as usize] = microcode_arr!(Transfer(TransferDirection::YtoA));
    r[TSX as usize] = microcode_arr!(Transfer(TransferDirection::SPtoX));
    r[TXS as usize] = microcode_arr!(Transfer(TransferDirection::XtoSP));
    r[INX as usize] = microcode_arr!(IncDec(IncDecTarget::IncrementX));
    r[INY as usize] = microcode_arr!(IncDec(IncDecTarget::IncrementY));
    r[DEX as usize] = microcode_arr!(IncDec(IncDecTarget::DecrementX));
    r[DEY as usize] = microcode_arr!(IncDec(IncDecTarget::DecrementY));
    r[ORA_IMMEDIATE as usize] = microcode_arr!(ImmediateWithOp(ImmediateOp::Ora));
    r[ORA_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, Ora);
    r[ORA_ZERO_PAGE_X as usize] = zero_page_x_op(Ora);
    r[ORA_ABSOLUTE as usize] = absolute_op(Ora);
    r[ORA_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedXWithOp {
            op: OpAfterAddressing::Ora,
            first_clock: CrossPageBehavior::FirstClock
        }
    );
    r[ORA_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedYWithOp {
            op: OpAfterAddressing::Ora,
            first_clock: CrossPageBehavior::FirstClock
        }
    );
    r[ORA_INDEXED_INDIRECT as usize] = indexed_indirect_op(Ora);
    r[ORA_INDIRECT_INDEXED as usize] =
        indirect_indexed_op(OpAfterAddressing::Ora, CrossPageBehavior::FirstClock);
    r[EOR_IMMEDIATE as usize] = microcode_arr!(ImmediateWithOp(ImmediateOp::Eor));
    r[EOR_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, Eor);
    r[EOR_ZERO_PAGE_X as usize] = zero_page_x_op(Eor);
    r[EOR_ABSOLUTE as usize] = absolute_op(Eor);
    r[EOR_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedXWithOp {
            op: OpAfterAddressing::Eor,
            first_clock: CrossPageBehavior::FirstClock
        }
    );
    r[EOR_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedYWithOp {
            op: OpAfterAddressing::Eor,
            first_clock: CrossPageBehavior::FirstClock
        }
    );
    r[EOR_INDEXED_INDIRECT as usize] = indexed_indirect_op(Eor);
    r[EOR_INDIRECT_INDEXED as usize] =
        indirect_indexed_op(OpAfterAddressing::Eor, CrossPageBehavior::FirstClock);
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
    r[LAX_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, Lax);
    r[LAX_ZERO_PAGE_Y as usize] = zero_page_y_op(Lax);
    r[LAX_ABSOLUTE as usize] = absolute_op(Lax);
    r[LAX_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedYWithOp {
            op: OpAfterAddressing::Lax,
            first_clock: CrossPageBehavior::FirstClock
        }
    );
    r[LAS_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedYWithOp {
            op: OpAfterAddressing::Las,
            first_clock: CrossPageBehavior::FirstClock
        }
    );
    r[LAX_INDEXED_INDIRECT as usize] = indexed_indirect_op(Lax);
    r[LAX_INDIRECT_INDEXED as usize] =
        indirect_indexed_op(OpAfterAddressing::Lax, CrossPageBehavior::FirstClock);
    r[SAX_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, Sax);
    r[SAX_ZERO_PAGE_Y as usize] = zero_page_y_op(Sax);
    r[SAX_ABSOLUTE as usize] = absolute_op(Sax);
    r[SAX_INDEXED_INDIRECT as usize] = indexed_indirect_op(Sax);
    r[DCP_ZERO_PAGE as usize] = microcode_arr!(ZeroPage, LoadIntoAlu, StoreAlu, Dcp);
    r[DCP_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(Dcp);
    r[DCP_ABSOLUTE as usize] = absolute_rmw_op(Dcp);
    r[DCP_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(Dcp);
    r[DCP_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_rmw_op(Dcp);
    r[DCP_INDEXED_INDIRECT as usize] = indexed_indirect_rmw_op(Dcp);
    r[DCP_INDIRECT_INDEXED as usize] = indirect_indexed_rmw_op(Dcp);
    r[DEC_ZERO_PAGE as usize] = zero_page_rmw_op(IncDec(IncDecTarget::DecrementAlu));
    r[DEC_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(IncDec(IncDecTarget::DecrementAlu));
    r[DEC_ABSOLUTE as usize] = absolute_rmw_op(IncDec(IncDecTarget::DecrementAlu));
    r[DEC_ABSOLUTE_INDEXED_X as usize] =
        absolute_indexed_x_rmw_op(IncDec(IncDecTarget::DecrementAlu));
    r[INC_ZERO_PAGE as usize] = zero_page_rmw_op(IncDec(IncDecTarget::IncrementAlu));
    r[INC_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(IncDec(IncDecTarget::IncrementAlu));
    r[INC_ABSOLUTE as usize] = absolute_rmw_op(IncDec(IncDecTarget::IncrementAlu));
    r[INC_ABSOLUTE_INDEXED_X as usize] =
        absolute_indexed_x_rmw_op(IncDec(IncDecTarget::IncrementAlu));
    r[ISC_ZERO_PAGE as usize] = zero_page_rmw_op(Isc);
    r[ISC_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(Isc);
    r[ISC_ABSOLUTE as usize] = absolute_rmw_op(Isc);
    r[ISC_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(Isc);
    r[ISC_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_rmw_op(Isc);
    r[ISC_INDEXED_INDIRECT as usize] = indexed_indirect_rmw_op(Isc);
    r[ISC_INDIRECT_INDEXED as usize] = indirect_indexed_rmw_op(Isc);
    r[RRA_ZERO_PAGE as usize] = zero_page_rmw_op(Rra);
    r[RRA_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(Rra);
    r[RRA_ABSOLUTE as usize] = absolute_rmw_op(Rra);
    r[RRA_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(Rra);
    r[RRA_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_rmw_op(Rra);
    r[RRA_INDEXED_INDIRECT as usize] = indexed_indirect_rmw_op(Rra);
    r[RRA_INDIRECT_INDEXED as usize] = indirect_indexed_rmw_op(Rra);
    r[RLA_ZERO_PAGE as usize] = zero_page_rmw_op(Rla);
    r[RLA_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(Rla);
    r[RLA_ABSOLUTE as usize] = absolute_rmw_op(Rla);
    r[RLA_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(Rla);
    r[RLA_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_rmw_op(Rla);
    r[RLA_INDEXED_INDIRECT as usize] = indexed_indirect_rmw_op(Rla);
    r[RLA_INDIRECT_INDEXED as usize] = indirect_indexed_rmw_op(Rla);
    r[SLO_ZERO_PAGE as usize] = zero_page_rmw_op(Slo);
    r[SLO_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(Slo);
    r[SLO_ABSOLUTE as usize] = absolute_rmw_op(Slo);
    r[SLO_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(Slo);
    r[SLO_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_rmw_op(Slo);
    r[SLO_INDEXED_INDIRECT as usize] = indexed_indirect_rmw_op(Slo);
    r[SLO_INDIRECT_INDEXED as usize] = indirect_indexed_rmw_op(Slo);
    r[SRE_ZERO_PAGE as usize] = zero_page_rmw_op(Sre);
    r[SRE_ZERO_PAGE_X as usize] = zero_page_x_rmw_op(Sre);
    r[SRE_ABSOLUTE as usize] = absolute_rmw_op(Sre);
    r[SRE_ABSOLUTE_INDEXED_X as usize] = absolute_indexed_x_rmw_op(Sre);
    r[SRE_ABSOLUTE_INDEXED_Y as usize] = absolute_indexed_y_rmw_op(Sre);
    r[SRE_INDEXED_INDIRECT as usize] = indexed_indirect_rmw_op(Sre);
    r[SRE_INDIRECT_INDEXED as usize] = indirect_indexed_rmw_op(Sre);
    r[SHX_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedYWithOp {
            op: OpAfterAddressing::Shx,
            first_clock: CrossPageBehavior::FirstClockAlways
        },
        Shx
    );
    r[SHA_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedYWithOp {
            op: OpAfterAddressing::Sha,
            first_clock: CrossPageBehavior::FirstClockAlways
        },
        Sha
    );
    r[SHY_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedXWithOp {
            op: OpAfterAddressing::Shy,
            first_clock: CrossPageBehavior::FirstClockAlways
        },
        Shy
    );
    r[TAS_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        AbsoluteL,
        AbsoluteH,
        IndexedYWithOp {
            op: OpAfterAddressing::Tas,
            first_clock: CrossPageBehavior::FirstClockAlways
        },
        Tas
    );
    r[SHA_INDIRECT_INDEXED_Y as usize] = microcode_arr!(
        ZeroPage,
        IndexedL,
        IndexedH,
        IndexedYWithOp {
            op: OpAfterAddressing::Sha,
            first_clock: CrossPageBehavior::FirstClockAlways
        },
        Sha
    );
    r
}

const OPCODE_TABLE: [ArrayVec<[Microcode; 7]>; 256] = build_opcode_table();

#[derive(Debug, Copy, Clone)]
pub enum OpAfterAddressing {
    And,
    /// Read byte from address_latch into register A,
    LoadIntoA,
    LoadIntoX,
    LoadIntoY,
    LoadIntoAlu,
    StoreA,
    Ora,
    Eor,
    Lax,
    Las,
    Sbc,
    Adc,
    Cmp,
    Shx,
    Shy,
    Sha,
    Tas,
    Nop,
}

impl OpAfterAddressing {
    const fn to_microcode(self) -> Microcode {
        match self {
            OpAfterAddressing::And => Microcode::And,
            OpAfterAddressing::LoadIntoA => Microcode::LoadR(Register::A),
            OpAfterAddressing::LoadIntoX => Microcode::LoadR(Register::X),
            OpAfterAddressing::LoadIntoY => Microcode::LoadR(Register::Y),
            OpAfterAddressing::LoadIntoAlu => Microcode::LoadIntoAlu,
            OpAfterAddressing::StoreA => Microcode::StoreR(Register::A),
            OpAfterAddressing::Ora => Microcode::Ora,
            OpAfterAddressing::Eor => Microcode::Eor,
            OpAfterAddressing::Lax => Microcode::Lax,
            OpAfterAddressing::Las => Microcode::Las,
            OpAfterAddressing::Sbc => Microcode::Sbc,
            OpAfterAddressing::Adc => Microcode::Adc,
            OpAfterAddressing::Shx => Microcode::Shx,
            OpAfterAddressing::Shy => Microcode::Shy,
            OpAfterAddressing::Sha => Microcode::Sha,
            OpAfterAddressing::Tas => Microcode::Tas,
            OpAfterAddressing::Nop => Microcode::Nop,
            OpAfterAddressing::Cmp => Microcode::Cmp,
        }
    }
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub enum TransferDirection {
    AtoX,
    XtoA,
    AtoY,
    YtoA,
    SPtoX,
    XtoSP,
}

#[derive(Debug, Clone, Copy)]
pub enum IncDecTarget {
    IncrementX,
    IncrementY,
    DecrementX,
    DecrementY,
    IncrementAlu,
    DecrementAlu,
}

#[derive(Debug, Clone, Copy)]
pub enum AOrMemory {
    Accumulator,
    Memory,
}

#[derive(Debug, Clone, Copy)]
pub enum ShiftRotateOp {
    Asl,
    Lsr,
    Rol,
    Ror,
}

impl ImmediateOp {
    pub fn exec<M: Mcu>(self, cpu: &mut Cpu<M>) {
        match self {
            ImmediateOp::Adc => cpu.adc(false),
            ImmediateOp::And => cpu.and(false),
            ImmediateOp::Sbc => cpu.sbc(false),
            ImmediateOp::Cmp => cpu.cmp(false),
            ImmediateOp::Cpx => cpu.cpx(false),
            ImmediateOp::Cpy => cpu.cpy(false),
            ImmediateOp::Ora => cpu.ora(false),
            ImmediateOp::Eor => cpu.eor(false),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CrossPageBehavior {
    /// If not cross paged, will not push the op microcode into cpu
    FirstClock,
    /// No matter cross paged or not, always execute the first clock dummy read
    FirstClockAlways,
}

/// Each Microcode instruction executed by the CPU in a single cycle
#[derive(Debug, Clone, Copy, Default)]
pub enum Microcode {
    /// Fetch and decode next op code
    FetchAndDecode,
    /// Load register with value from memory
    LoadR(Register),
    /// Store register value to memory
    StoreR(Register),
    /// Read immediate value into register
    LoadImmediate(Register),

    /// Store cpu alu register into memory at cpu.ab
    StoreAlu,

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
    IndexedXWithOp {
        op: OpAfterAddressing,
        /// if cross paged, push this Microcode with `first_clock` to be false
        first_clock: CrossPageBehavior,
    },
    /// address_latch += Y, doing and operation with register A
    IndexedYWithOp {
        op: OpAfterAddressing,
        /// if cross paged, push this Microcode with `first_clock` to be false
        first_clock: CrossPageBehavior,
    },
    /// Do nothing, used in "oops" cycles of AbsoluteIndexed and Indirect Indexed addressing
    Nop,
    /// Load low byte of address from zero page indirect location
    IndexedL,
    /// Load high byte of address from zero page indirect location
    IndexedH,
    /// Read immediate value from instruction data stream, but do not use it
    SkipImmediate,

    /// Load byte into alu at memory ab
    LoadIntoAlu,
    /// Store SHX result at memory ab
    Shx,
    /// Store SHY result at memory ab
    Shy,
    /// Store SHA result at memory ab
    Sha,
    /// Store TAS result at memory ab
    Tas,

    /// Take immediate value from instruction data stream and save into alu, then perform the operation.
    ImmediateWithOp(ImmediateOp),

    /// Fetch a byte from memory, then add to accumulator with carry
    Adc,
    /// Fetch a byte from memory, then subtract with carry from accumulator
    Sbc,
    /// Fetch a byte from memory, then compare with accumulator
    Cmp,
    /// Fetch a byte from memory, then compare with x register
    Cpx,
    /// Fetch a byte from memory, then compare with y register
    Cpy,
    /// Fetch a byte from memory, then or with accumulator
    Ora,
    /// Fetch a byte from memory, then xor with accumulator
    Eor,
    /// Fetch a byte from memory, then and with accumulator
    And,
    /// Undocumented ANE/XAA: (A OR CONST) AND X AND oper -> A
    /// Implemented deterministically as A := X & oper
    AneImmediate,
    /// LAX immediate: load immediate into A and X in a single micro-op (two cycles total with Fetch)
    LaxImmediate,
    /// Execute LAS/LAR semantics
    Las,
    /// Test bits in accumulator against ALU value
    Bit,

    Asl(AOrMemory),
    Lsr(AOrMemory),
    Rol(AOrMemory),
    Ror(AOrMemory),

    AlrImmediate,
    AncImmediate,
    ArrImmediate,
    AxsImmediate,

    /// Fetch a byte from memory, then load into accumulator and X
    Lax,
    Sax,
    Rla,
    Dcp,
    Isc,
    Rra,
    Slo,
    Sre,

    IndexedHAndJump,

    SetFlag(Flag),
    ClearFlag(Flag),

    Transfer(TransferDirection),
    IncDec(IncDecTarget),

    /// Read offset value from instruction data stream,
    /// If BranchTest is true, pc += offset, push one Noc if not cross page, push two Noc if cross page
    BranchRelative(BranchTest),

    PushPcL,
    PushPcH,
    /// Push cpu status register into stack
    PushStatus {
        /// after push status, set disable interrupt flag
        set_disable_interrupt: bool,
        /// break flag of status to set
        break_flag: bool,
    },
    /// Pop cpu status register from stack, used to return from interrupt handler, and PLP
    Plp,
    /// Pop low byte of PC from stack
    PopPcL,
    /// Pop high byte of PC from stack
    PopPcH,
    IncPc,
    /// Push register A to stack
    Pha,
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
    fn test<M: Mcu>(self, cpu: &Cpu<M>) -> bool {
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

impl Microcode {
    /// Returns true if this microcode performs a memory read operation.
    /// Used for DMC DMA phantom address detection.
    pub const fn is_read_operation(self) -> bool {
        matches!(
            self,
            Self::LoadR(_)
                | Self::Bit
                | Self::Lax
                | Self::LoadIntoAlu
                | Self::Adc
                | Self::Sbc
                | Self::Cmp
                | Self::Cpx
                | Self::Cpy
                | Self::Ora
                | Self::Eor
                | Self::And
        )
    }

    /// perform second phase of this Microcode
    pub fn exec<M: Mcu>(self, cpu: &mut Cpu<M>) {
        match self {
            Self::FetchAndDecode => Self::fetch_and_decode(cpu),
            Self::LoadR(r) => Self::load_register(cpu, r),
            Self::StoreR(r) => Self::store_register(cpu, r),
            Self::LoadImmediate(r) => Self::load_immediate(cpu, r),
            Self::ZeroPage => Self::zero_page(cpu),
            Self::ZeroPageIndexedX => Self::zero_page_indexed_x(cpu),
            Self::ZeroPageIndexedY => Self::zero_page_indexed_y(cpu),
            Self::AbsoluteL => Self::absolute_l(cpu),
            Self::AbsoluteH => Self::absolute_h(cpu),

            Self::Adc => cpu.adc(true),
            Self::Sbc => cpu.sbc(true),
            Self::Cmp => cpu.cmp(true),
            Self::Cpx => cpu.cpx(true),
            Self::Cpy => cpu.cpy(true),
            Self::Ora => cpu.ora(true),
            Self::Eor => cpu.eor(true),
            Self::And => cpu.and(true),
            Self::Shx => cpu.shx(),
            Self::Shy => cpu.shy(),
            Self::Sha => cpu.sha(),
            Self::Tas => cpu.tas(),
            Self::IndexedXWithOp { op, first_clock } => {
                Self::absolute_indexed_x_with_op(cpu, op, first_clock)
            }
            Self::IndexedYWithOp { op, first_clock } => {
                Self::absolute_indexed_y_with_op(cpu, op, first_clock)
            }
            Self::Bit => cpu.bit(),
            Self::StoreAlu => Self::store_alu(cpu),
            Self::Nop => {}
            Self::SkipImmediate => {
                cpu.inc_read_byte();
            }
            Self::IndexedL => Self::indexed_l(cpu),
            Self::IndexedH => Self::indexed_h(cpu),
            Self::Asl(target) => Self::shift_rotate(cpu, target, ShiftRotateOp::Asl),
            Self::Lsr(target) => Self::shift_rotate(cpu, target, ShiftRotateOp::Lsr),
            Self::Rol(target) => Self::shift_rotate(cpu, target, ShiftRotateOp::Rol),
            Self::Ror(target) => Self::shift_rotate(cpu, target, ShiftRotateOp::Ror),

            Self::AlrImmediate => Self::alr_immediate(cpu),
            Self::AncImmediate => Self::anc_immediate(cpu),
            Self::ArrImmediate => Self::arr_immediate(cpu),
            Self::AxsImmediate => Self::axs_immediate(cpu),
            Self::AneImmediate => Self::ane_immediate(cpu),
            Self::LaxImmediate => Self::lax_immediate(cpu),
            Self::Las => Self::las(cpu),

            Self::Lax => {
                cpu.load_alu();
                cpu.lax();
            }
            Self::Sax => cpu.sax(),
            Self::Rla => cpu.rla(),
            Self::Dcp => cpu.dcp(),
            Self::Isc => cpu.isc(),
            Self::Rra => cpu.rra(),
            Self::Slo => cpu.slo(),
            Self::Sre => cpu.sre(),

            Self::IndexedHAndJump => {
                Self::indexed_h(cpu);
                cpu.set_pc_to_ab()
            }

            Self::SetFlag(flag) => cpu.set_flag(flag, true),
            Self::ClearFlag(flag) => cpu.set_flag(flag, false),

            Self::Transfer(direction) => Self::transfer(cpu, direction),
            Self::IncDec(target) => Self::inc_dec(cpu, target),

            Self::BranchRelative(branch_test) => Self::branch_relative(cpu, branch_test),
            Self::Kill => cpu.halt(),

            Self::LoadIrqPcH => cpu.load_irq_pch(),
            Self::LoadIrqPcL => cpu.load_irq_pcl(),
            Self::LoadNmiPcL => {
                cpu.pc = cpu.read_byte(0xFFFA) as u16;
            }
            Self::LoadNmiPcH => {
                cpu.pc |= (cpu.read_byte(0xFFFB) as u16) << 8;
            }
            Self::PushPcH => {
                cpu.push_stack((cpu.pc >> 8) as u8);
            }
            Self::PushPcL => {
                cpu.push_stack((cpu.pc & 0xFF) as u8);
            }
            Self::PushStatus {
                set_disable_interrupt,
                break_flag,
            } => cpu.push_status(set_disable_interrupt, break_flag),
            Self::Plp => cpu.plp(),
            Self::PopPcL => {
                cpu.pc = cpu.pop_stack() as u16;
            }
            Self::PopPcH => {
                cpu.pc |= (cpu.pop_stack() as u16) << 8;
            }
            Self::IncPc => {
                cpu.pc += 1;
            }
            Self::Pha => cpu.pha(),
            Self::PopStack => cpu.pop_stack_into_alu(),
            Self::UpdateAFromAlu => cpu.set_a(cpu.alu),

            Self::LoadResetPcL => {
                cpu.pc = cpu.read_byte(0xFFFC) as u16;
            }
            Self::LoadResetPcH => {
                cpu.pc |= (cpu.read_byte(0xFFFD) as u16) << 8;
            }
            Self::LoadPcAbsoluteH => {
                cpu.pc = (cpu.ab & 0xff) | ((cpu.inc_read_byte() as u16) << 8);
            }
            Self::LoadIntoAlu => {
                cpu.load_alu();
            }
            Self::ImmediateWithOp(op) => Self::immediate_with_op(cpu, op),
        }
    }

    /// Execute the micro code

    fn fetch_and_decode<M: Mcu>(cpu: &mut Cpu<M>) {
        let opcode = cpu.inc_read_byte();
        cpu.opcode = opcode;
        cpu.push_microcodes(&OPCODE_TABLE[opcode as usize]);
    }

    fn load_register<M: Mcu>(cpu: &mut Cpu<M>, r: Register) {
        let value = cpu.read_byte(cpu.ab);
        match r {
            Register::A => cpu.set_a(value),
            Register::X => cpu.set_x(value),
            Register::Y => cpu.set_y(value),
        }
    }

    fn store_register<M: Mcu>(cpu: &mut Cpu<M>, r: Register) {
        match r {
            Register::A => cpu.write_byte(cpu.ab, cpu.a),
            Register::X => cpu.write_byte(cpu.ab, cpu.x),
            Register::Y => cpu.write_byte(cpu.ab, cpu.y),
        }
    }

    fn load_immediate<M: Mcu>(cpu: &mut Cpu<M>, r: Register) {
        let value = cpu.inc_read_byte();
        match r {
            Register::A => cpu.set_a(value),
            Register::X => cpu.set_x(value),
            Register::Y => cpu.set_y(value),
        }
    }

    fn ane_immediate<M: Mcu>(cpu: &mut Cpu<M>) {
        // Undocumented ANE/XAA: hardware behaviour uncertain; use deterministic
        // approximation: A := X & oper. Read immediate operand into alu first.
        let t = cpu.inc_read_byte();
        // Compute X & operand into A, update flags accordingly
        cpu.set_a(cpu.x & t);
    }

    fn las<M: Mcu>(cpu: &mut Cpu<M>) {
        // LAS/LAR: load memory into ALU already (addressing microcode sets cpu.alu)
        // Then perform: M AND SP -> A, X, SP
        let v = cpu.alu & cpu.sp;
        cpu.set_a(v);
        cpu.set_x(v);
        cpu.sp = v;
    }

    fn lax_immediate<M: Mcu>(cpu: &mut Cpu<M>) {
        // Read immediate operand into alu and perform LAX semantics in one micro-op.
        let t = cpu.inc_read_byte();
        cpu.set_a(t);
        cpu.set_x(t);
    }

    fn alr_immediate<M: Mcu>(cpu: &mut Cpu<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.alr();
    }

    fn anc_immediate<M: Mcu>(cpu: &mut Cpu<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.anc();
    }

    fn arr_immediate<M: Mcu>(cpu: &mut Cpu<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.arr();
    }

    fn axs_immediate<M: Mcu>(cpu: &mut Cpu<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.axs();
    }

    fn zero_page<M: Mcu>(cpu: &mut Cpu<M>) {
        let addr = cpu.inc_read_byte();
        cpu.ab = addr as u16;
    }

    fn zero_page_indexed_x<M: Mcu>(cpu: &mut Cpu<M>) {
        cpu.ab = cpu.abl().wrapping_add(cpu.x) as u16;
    }

    fn zero_page_indexed_y<M: Mcu>(cpu: &mut Cpu<M>) {
        cpu.ab = cpu.abl().wrapping_add(cpu.y) as u16;
    }

    fn absolute_l<M: Mcu>(cpu: &mut Cpu<M>) {
        let low = cpu.inc_read_byte();
        cpu.set_abl(low);
    }

    fn absolute_h<M: Mcu>(cpu: &mut Cpu<M>) {
        let high = cpu.inc_read_byte();
        cpu.set_abh(high);
    }

    fn indexed_l<M: Mcu>(cpu: &mut Cpu<M>) {
        cpu.db = cpu.read_byte(cpu.ab);
    }

    fn indexed_h<M: Mcu>(cpu: &mut Cpu<M>) {
        let page = cpu.ab & 0xFF00;
        let addr = page | (cpu.ab as u8 & 0xFF).wrapping_add(1) as u16;
        cpu.ab = cpu.db as u16 | ((cpu.read_byte(addr) as u16) << 8);
    }

    fn store_alu<M: Mcu>(cpu: &mut Cpu<M>) {
        cpu.write_byte(cpu.ab, cpu.alu);
    }

    fn branch_relative<M: Mcu>(cpu: &mut Cpu<M>, branch_test: BranchTest) {
        let offset = cpu.inc_read_byte();
        if branch_test.test(cpu) {
            let pch = cpu.pch();
            cpu.pc = cpu.pc.wrapping_add((offset as i8) as u16);
            cpu.retain_cycle();
            if pch != cpu.pch() {
                cpu.retain_cycle();
            } else {
                // A taken non-page-crossing branch suppresses IRQ
                // detection on its last (3rd) cycle.  Set a flag so
                // the next instruction-boundary IRQ check is deferred.
                cpu.branch_irq_defer = true;
            }
        }
    }

    fn absolute_indexed_x_with_op<M: Mcu>(
        cpu: &mut Cpu<M>,
        op: OpAfterAddressing,
        first_clock: CrossPageBehavior,
    ) {
        Self::absolute_indexed_with_op_generic(cpu, op, first_clock, cpu.x)
    }

    fn absolute_indexed_y_with_op<M: Mcu>(
        cpu: &mut Cpu<M>,
        op: OpAfterAddressing,
        first_clock: CrossPageBehavior,
    ) {
        Self::absolute_indexed_with_op_generic(cpu, op, first_clock, cpu.y)
    }

    fn absolute_indexed_with_op_generic<M: Mcu>(
        cpu: &mut Cpu<M>,
        op: OpAfterAddressing,
        first_clock: CrossPageBehavior,
        idx: u8,
    ) {
        let abh = cpu.abh();
        cpu.ab = cpu.ab.wrapping_add(idx as u16);
        let is_first_clock_always = matches!(first_clock, CrossPageBehavior::FirstClockAlways);

        if is_first_clock_always || abh != cpu.abh() {
            let dummy_addr = (u16::from(abh) << 8) | u16::from(cpu.abl());
            cpu.read_byte(dummy_addr);
            if !is_first_clock_always {
                cpu.push_microcode(op.to_microcode());
            }
        } else {
            op.to_microcode().exec(cpu);
        }
    }

    fn immediate_with_op<M: Mcu>(cpu: &mut Cpu<M>, op: ImmediateOp) {
        cpu.alu = cpu.inc_read_byte();
        op.exec(cpu);
    }

    fn transfer<M: Mcu>(cpu: &mut Cpu<M>, direction: TransferDirection) {
        match direction {
            TransferDirection::AtoX => cpu.set_x(cpu.a),
            TransferDirection::XtoA => cpu.set_a(cpu.x),
            TransferDirection::AtoY => cpu.set_y(cpu.a),
            TransferDirection::YtoA => cpu.set_a(cpu.y),
            TransferDirection::SPtoX => cpu.set_x(cpu.sp),
            TransferDirection::XtoSP => {
                cpu.sp = cpu.x;
            }
        }
    }

    fn inc_dec<M: Mcu>(cpu: &mut Cpu<M>, target: IncDecTarget) {
        match target {
            IncDecTarget::IncrementX => {
                cpu.set_x(cpu.x.wrapping_add(1));
                cpu.alu = cpu.x;
            }
            IncDecTarget::IncrementY => {
                cpu.set_y(cpu.y.wrapping_add(1));
                cpu.alu = cpu.y;
            }
            IncDecTarget::DecrementX => {
                cpu.set_x(cpu.x.wrapping_sub(1));
                cpu.alu = cpu.x;
            }
            IncDecTarget::DecrementY => {
                cpu.set_y(cpu.y.wrapping_sub(1));
                cpu.alu = cpu.y;
            }
            IncDecTarget::IncrementAlu => {
                cpu.alu = cpu.alu.wrapping_add(1);
                cpu.write_byte(cpu.ab, cpu.alu);
                cpu.update_zero_negative_flags(cpu.alu);
            }
            IncDecTarget::DecrementAlu => {
                cpu.alu = cpu.alu.wrapping_sub(1);
                cpu.write_byte(cpu.ab, cpu.alu);
                cpu.update_zero_negative_flags(cpu.alu);
            }
        }
    }

    fn shift_rotate<M: Mcu>(cpu: &mut Cpu<M>, target: AOrMemory, op: ShiftRotateOp) {
        fn calc<M: Mcu>(cpu: &mut Cpu<M>, op: ShiftRotateOp) {
            match op {
                ShiftRotateOp::Asl => {
                    cpu.inner_set_flag(Flag::Carry, cpu.alu & 0x80 != 0);
                    cpu.alu <<= 1;
                }
                ShiftRotateOp::Lsr => {
                    cpu.inner_set_flag(Flag::Carry, cpu.alu & 0x01 != 0);
                    cpu.alu >>= 1;
                }
                ShiftRotateOp::Rol => {
                    let carry = cpu.alu & 0x80 != 0;
                    cpu.alu = (cpu.alu << 1) | (cpu.flag(Flag::Carry) as u8);
                    cpu.inner_set_flag(Flag::Carry, carry);
                }
                ShiftRotateOp::Ror => {
                    let carry = cpu.alu & 0x01 != 0;
                    cpu.alu = (cpu.alu >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7);
                    cpu.inner_set_flag(Flag::Carry, carry);
                }
            }

            cpu.update_zero_negative_flags(cpu.alu);
        }

        match target {
            AOrMemory::Accumulator => {
                cpu.alu = cpu.a;
                calc(cpu, op);
                cpu.a = cpu.alu;
            }
            AOrMemory::Memory => {
                calc(cpu, op);
                Self::store_alu(cpu);
            }
        }
    }
}
