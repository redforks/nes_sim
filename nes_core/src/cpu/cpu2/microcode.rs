use tinyvec::ArrayVec;

use super::Cpu2;
use crate::{Flag, mcu::Mcu};

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

const fn build_opcode_table() -> [ArrayVec<[Microcode; 7]>; 256] {
    let mut r = include!("init_microtable.inc.rs");
    r[Opcode::AND_IMMEDIATE as usize] = microcode_arr!(Microcode::AndImmediate);
    r[Opcode::AND_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_load_alu(), Microcode::And);
    r[Opcode::AND_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::And
    );
    r[Opcode::AND_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::And
    );
    r[Opcode::AND_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: true,
            load_into_alu: true
        },
        Microcode::And
    );
    r[Opcode::AND_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: true,
            load_into_alu: true
        },
        Microcode::And
    );
    r[Opcode::AND_INDEXED_INDIRECT as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: true
        },
        Microcode::Nop,
        Microcode::And
    );
    r[Opcode::AND_INDIRECT_INDEXED as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: true,
            load_into_alu: true
        },
        Microcode::And
    );
    r[Opcode::LDA_IMMEDIATE as usize] = microcode_arr!(Microcode::LoadImmediateA);
    r[Opcode::LDA_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_addr(), Microcode::LoadA);
    r[Opcode::LDA_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::LoadA
    );
    r[Opcode::LDA_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: false
        },
        Microcode::LoadA
    );
    r[Opcode::LDA_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: true,
            load_into_alu: false
        },
        Microcode::LoadA
    );
    r[Opcode::LDA_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: true,
            load_into_alu: false
        },
        Microcode::LoadA
    );
    r[Opcode::LDA_INDIRECT_INDEXED as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::Nop,
        Microcode::LoadA
    );
    r[Opcode::LDA_INDIRECT_INDEXED_Y as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: true,
            load_into_alu: false
        },
        Microcode::LoadA
    );
    r[Opcode::LDX_IMMEDIATE as usize] = microcode_arr!(Microcode::LoadImmediateX);
    r[Opcode::LDX_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_addr(), Microcode::LoadX);
    r[Opcode::LDX_ZERO_PAGE_Y as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedY {
            load_into_alu: false
        },
        Microcode::LoadX
    );
    r[Opcode::LDX_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: false
        },
        Microcode::LoadX
    );
    r[Opcode::LDX_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: true,
            load_into_alu: false
        },
        Microcode::LoadX
    );
    r[Opcode::LDY_IMMEDIATE as usize] = microcode_arr!(Microcode::LoadImmediateY);
    r[Opcode::LDY_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_addr(), Microcode::LoadY);
    r[Opcode::LDY_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::LoadY
    );
    r[Opcode::LDY_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: false
        },
        Microcode::LoadY
    );
    r[Opcode::LDY_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: true,
            load_into_alu: false
        },
        Microcode::LoadY
    );
    r[Opcode::STA_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_addr(), Microcode::StoreA);
    r[Opcode::STA_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::StoreA
    );
    r[Opcode::STA_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: false
        },
        Microcode::StoreA
    );
    r[Opcode::STA_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: false
        },
        Microcode::StoreA
    );
    r[Opcode::STA_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: false,
            load_into_alu: false
        },
        Microcode::StoreA
    );
    r[Opcode::STA_INDEXED_INDIRECT as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::Nop,
        Microcode::StoreA
    );
    r[Opcode::STA_INDIRECT_INDEXED_Y as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: false,
            load_into_alu: false
        },
        Microcode::StoreA
    );
    r[Opcode::STX_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_addr(), Microcode::StoreX);
    r[Opcode::STX_ZERO_PAGE_Y as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedY {
            load_into_alu: false
        },
        Microcode::StoreX
    );
    r[Opcode::STX_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: false
        },
        Microcode::StoreX
    );
    r[Opcode::STY_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_addr(), Microcode::StoreY);
    r[Opcode::STY_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::StoreY
    );
    r[Opcode::STY_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: false
        },
        Microcode::StoreY
    );
    r[Opcode::BIT_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_load_alu(), Microcode::Bit);
    r[Opcode::BIT_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::Bit
    );
    r[Opcode::ADC_IMMEDIATE as usize] = microcode_arr!(Microcode::AdcImmediate);
    r[Opcode::ADC_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_load_alu(), Microcode::Adc);
    r[Opcode::ADC_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::Adc
    );
    r[Opcode::ADC_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::Adc
    );
    r[Opcode::ADC_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: true,
            load_into_alu: true
        },
        Microcode::Adc
    );
    r[Opcode::ADC_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: true,
            load_into_alu: true
        },
        Microcode::Adc
    );
    r[Opcode::ADC_INDEXED_INDIRECT as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: true
        },
        Microcode::Nop,
        Microcode::Adc
    );
    r[Opcode::ADC_INDIRECT_INDEXED as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: true,
            load_into_alu: true
        },
        Microcode::Adc
    );
    r[Opcode::ASL_ACCUMULATOR as usize] = microcode_arr!(Microcode::AslAccumulator);
    r[Opcode::ASL_ZERO_PAGE as usize] = microcode_arr!(
        Microcode::zero_page_load_alu(),
        Microcode::StoreAlu,
        Microcode::Asl,
        Microcode::StoreAlu
    );
    r[Opcode::ASL_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Asl,
        Microcode::StoreAlu
    );
    r[Opcode::ASL_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Asl,
        Microcode::StoreAlu
    );
    r[Opcode::ASL_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Asl,
        Microcode::StoreAlu
    );
    r[Opcode::LSR_ACCUMULATOR as usize] = microcode_arr!(Microcode::LsrAccumulator);
    r[Opcode::LSR_ZERO_PAGE as usize] = microcode_arr!(
        Microcode::zero_page_load_alu(),
        Microcode::StoreAlu,
        Microcode::Lsr,
        Microcode::StoreAlu
    );
    r[Opcode::LSR_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Lsr,
        Microcode::StoreAlu
    );
    r[Opcode::LSR_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Lsr,
        Microcode::StoreAlu
    );
    r[Opcode::LSR_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Lsr,
        Microcode::StoreAlu
    );
    r[Opcode::ROL_ACCUMULATOR as usize] = microcode_arr!(Microcode::RolAccumulator);
    r[Opcode::ROL_ZERO_PAGE as usize] = microcode_arr!(
        Microcode::zero_page_load_alu(),
        Microcode::StoreAlu,
        Microcode::Rol,
        Microcode::StoreAlu
    );
    r[Opcode::ROL_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Rol,
        Microcode::StoreAlu
    );
    r[Opcode::ROL_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Rol,
        Microcode::StoreAlu
    );
    r[Opcode::ROL_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Rol,
        Microcode::StoreAlu
    );
    r[Opcode::ROR_ACCUMULATOR as usize] = microcode_arr!(Microcode::RorAccumulator);
    r[Opcode::ROR_ZERO_PAGE as usize] = microcode_arr!(
        Microcode::zero_page_load_alu(),
        Microcode::StoreAlu,
        Microcode::Ror,
        Microcode::StoreAlu
    );
    r[Opcode::ROR_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Ror,
        Microcode::StoreAlu
    );
    r[Opcode::ROR_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Ror,
        Microcode::StoreAlu
    );
    r[Opcode::ROR_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Ror,
        Microcode::StoreAlu
    );
    r[Opcode::BCC as usize] = microcode_arr!(Microcode::BranchRelative(BranchTest::IfCarryClear));
    r[Opcode::BCS as usize] = microcode_arr!(Microcode::BranchRelative(BranchTest::IfCarrySet));
    r[Opcode::BEQ as usize] = microcode_arr!(Microcode::BranchRelative(BranchTest::IfZeroSet));
    r[Opcode::BMI as usize] = microcode_arr!(Microcode::BranchRelative(BranchTest::IfNegativeSet));
    r[Opcode::BNE as usize] = microcode_arr!(Microcode::BranchRelative(BranchTest::IfZeroClear));
    r[Opcode::BPL as usize] =
        microcode_arr!(Microcode::BranchRelative(BranchTest::IfNegativeClear));
    r[Opcode::BVC as usize] =
        microcode_arr!(Microcode::BranchRelative(BranchTest::IfOverflowClear));
    r[Opcode::BVS as usize] = microcode_arr!(Microcode::BranchRelative(BranchTest::IfOverflowSet));
    r[Opcode::PHA as usize] = microcode_arr!(Microcode::Pha);
    r[Opcode::PLA as usize] = microcode_arr!(Microcode::Pla);
    r[Opcode::PHP as usize] = microcode_arr!(Microcode::Php);
    r[Opcode::PLP as usize] = microcode_arr!(Microcode::Plp);
    r[Opcode::JMP_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: false
        },
        Microcode::JmpAbsolute
    );
    r[Opcode::JMP_INDIRECT as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: false
        },
        Microcode::JmpIndirect
    );
    r[Opcode::JSR as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: false
        },
        Microcode::Jsr
    );
    r[Opcode::RTS as usize] = microcode_arr!(Microcode::Rts);
    r[Opcode::RTI as usize] = microcode_arr!(Microcode::Rti);
    r[Opcode::CLC as usize] = microcode_arr!(Microcode::Clc);
    r[Opcode::SEC as usize] = microcode_arr!(Microcode::Sec);
    r[Opcode::CLD as usize] = microcode_arr!(Microcode::Cld);
    r[Opcode::SED as usize] = microcode_arr!(Microcode::Sed);
    r[Opcode::CLI as usize] = microcode_arr!(Microcode::Cli);
    r[Opcode::SEI as usize] = microcode_arr!(Microcode::Sei);
    r[Opcode::CLV as usize] = microcode_arr!(Microcode::Clv);
    r[Opcode::NOP as usize] = microcode_arr!(Microcode::Nop);
    r[Opcode::NOP1 as usize] = microcode_arr!(Microcode::Nop);
    r[Opcode::NOP2 as usize] = microcode_arr!(Microcode::Nop);
    r[Opcode::NOP3 as usize] = microcode_arr!(Microcode::Nop);
    r[Opcode::NOP4 as usize] = microcode_arr!(Microcode::Nop);
    r[Opcode::NOP5 as usize] = microcode_arr!(Microcode::Nop);
    r[Opcode::NOP6 as usize] = microcode_arr!(Microcode::Nop);
    r[Opcode::NOP_IMMEDIATE1 as usize] = microcode_arr!(Microcode::SkipImmediate, Microcode::Nop);
    r[Opcode::NOP_IMMEDIATE2 as usize] = microcode_arr!(Microcode::SkipImmediate, Microcode::Nop);
    r[Opcode::NOP_IMMEDIATE3 as usize] = microcode_arr!(Microcode::SkipImmediate, Microcode::Nop);
    r[Opcode::NOP_IMMEDIATE4 as usize] = microcode_arr!(Microcode::SkipImmediate, Microcode::Nop);
    r[Opcode::NOP_IMMEDIATE5 as usize] = microcode_arr!(Microcode::SkipImmediate, Microcode::Nop);
    r[Opcode::NOP_ZERO_PAGE1 as usize] = microcode_arr!(Microcode::Nop);
    r[Opcode::NOP_ZERO_PAGE2 as usize] = microcode_arr!(Microcode::Nop);
    r[Opcode::NOP_ZERO_PAGE3 as usize] = microcode_arr!(Microcode::Nop);
    r[Opcode::NOP_ZERO_PAGE_X1 as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Nop
    );
    r[Opcode::NOP_ZERO_PAGE_X2 as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Nop
    );
    r[Opcode::NOP_ZERO_PAGE_X3 as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Nop
    );
    r[Opcode::NOP_ZERO_PAGE_X4 as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Nop
    );
    r[Opcode::NOP_ZERO_PAGE_X5 as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Nop
    );
    r[Opcode::NOP_ZERO_PAGE_X6 as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Nop
    );
    r[Opcode::NOP_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: false
        },
        Microcode::Nop
    );
    r[Opcode::NOP_ABSOLUTE_INDEXED_X1 as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: false
        },
        Microcode::Nop
    );
    r[Opcode::NOP_ABSOLUTE_INDEXED_X2 as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: false
        },
        Microcode::Nop
    );
    r[Opcode::NOP_ABSOLUTE_INDEXED_X3 as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: false
        },
        Microcode::Nop
    );
    r[Opcode::NOP_ABSOLUTE_INDEXED_X4 as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: false
        },
        Microcode::Nop
    );
    r[Opcode::NOP_ABSOLUTE_INDEXED_X5 as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: false
        },
        Microcode::Nop
    );
    r[Opcode::NOP_ABSOLUTE_INDEXED_X6 as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: false
        },
        Microcode::Nop
    );
    r[Opcode::KIL1 as usize] = microcode_arr!(Microcode::Kill);
    r[Opcode::KIL2 as usize] = microcode_arr!(Microcode::Kill);
    r[Opcode::KIL3 as usize] = microcode_arr!(Microcode::Kill);
    r[Opcode::KIL4 as usize] = microcode_arr!(Microcode::Kill);
    r[Opcode::KIL5 as usize] = microcode_arr!(Microcode::Kill);
    r[Opcode::KIL6 as usize] = microcode_arr!(Microcode::Kill);
    r[Opcode::KIL7 as usize] = microcode_arr!(Microcode::Kill);
    r[Opcode::KIL8 as usize] = microcode_arr!(Microcode::Kill);
    r[Opcode::KIL9 as usize] = microcode_arr!(Microcode::Kill);
    r[Opcode::KIL10 as usize] = microcode_arr!(Microcode::Kill);
    r[Opcode::KIL11 as usize] = microcode_arr!(Microcode::Kill);
    r[Opcode::KIL12 as usize] = microcode_arr!(Microcode::Kill);
    r[Opcode::BRK as usize] = microcode_arr!(Microcode::Brk);
    r[Opcode::SBC_IMMEDIATE as usize] = microcode_arr!(Microcode::SbcImmediate);
    r[Opcode::USBC as usize] = microcode_arr!(Microcode::SbcImmediate);
    r[Opcode::SBC_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_load_alu(), Microcode::Sbc);
    r[Opcode::SBC_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::Sbc
    );
    r[Opcode::SBC_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::Sbc
    );
    r[Opcode::SBC_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: true,
            load_into_alu: true
        },
        Microcode::Sbc
    );
    r[Opcode::SBC_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: true,
            load_into_alu: true
        },
        Microcode::Sbc
    );
    r[Opcode::SBC_INDEXED_INDIRECT as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: true
        },
        Microcode::Nop,
        Microcode::Sbc
    );
    r[Opcode::SBC_INDIRECT_INDEXED as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: true,
            load_into_alu: true
        },
        Microcode::Sbc
    );
    r[Opcode::CMP_IMMEDIATE as usize] = microcode_arr!(Microcode::CmpImmediate);
    r[Opcode::CMP_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_load_alu(), Microcode::Cmp);
    r[Opcode::CMP_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::Cmp
    );
    r[Opcode::CMP_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::Cmp
    );
    r[Opcode::CMP_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: true,
            load_into_alu: true
        },
        Microcode::Cmp
    );
    r[Opcode::CMP_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: true,
            load_into_alu: true
        },
        Microcode::Cmp
    );
    r[Opcode::CMP_INDEXED_INDIRECT as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: true
        },
        Microcode::Nop,
        Microcode::Cmp
    );
    r[Opcode::CMP_INDIRECT_INDEXED as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: true,
            load_into_alu: true
        },
        Microcode::Cmp
    );
    r[Opcode::CPX_IMMEDIATE as usize] = microcode_arr!(Microcode::CpxImmediate);
    r[Opcode::CPX_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_load_alu(), Microcode::Cpx);
    r[Opcode::CPX_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::Cpx
    );
    r[Opcode::CPY_IMMEDIATE as usize] = microcode_arr!(Microcode::CpyImmediate);
    r[Opcode::CPY_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_load_alu(), Microcode::Cpy);
    r[Opcode::CPY_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::Cpy
    );
    r[Opcode::TAX as usize] = microcode_arr!(Microcode::Tax);
    r[Opcode::TXA as usize] = microcode_arr!(Microcode::Txa);
    r[Opcode::TAY as usize] = microcode_arr!(Microcode::Tay);
    r[Opcode::TYA as usize] = microcode_arr!(Microcode::Tya);
    r[Opcode::TSX as usize] = microcode_arr!(Microcode::Tsx);
    r[Opcode::TXS as usize] = microcode_arr!(Microcode::Txs);
    r[Opcode::ORA_IMMEDIATE as usize] = microcode_arr!(Microcode::OraImmediate);
    r[Opcode::ORA_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_load_alu(), Microcode::Ora);
    r[Opcode::ORA_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::Ora
    );
    r[Opcode::ORA_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::Ora
    );
    r[Opcode::ORA_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: true,
            load_into_alu: true
        },
        Microcode::Ora
    );
    r[Opcode::ORA_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: true,
            load_into_alu: true
        },
        Microcode::Ora
    );
    r[Opcode::ORA_INDEXED_INDIRECT as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: true
        },
        Microcode::Nop,
        Microcode::Ora
    );
    r[Opcode::ORA_INDIRECT_INDEXED as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: true,
            load_into_alu: true
        },
        Microcode::Ora
    );
    r[Opcode::EOR_IMMEDIATE as usize] = microcode_arr!(Microcode::EorImmediate);
    r[Opcode::EOR_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_load_alu(), Microcode::Eor);
    r[Opcode::EOR_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::Eor
    );
    r[Opcode::EOR_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::Eor
    );
    r[Opcode::EOR_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: true,
            load_into_alu: true
        },
        Microcode::Eor
    );
    r[Opcode::EOR_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: true,
            load_into_alu: true,
        },
        Microcode::Eor
    );
    r[Opcode::EOR_INDEXED_INDIRECT as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: true
        },
        Microcode::Nop,
        Microcode::Eor
    );
    r[Opcode::EOR_INDIRECT_INDEXED as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: true,
            load_into_alu: true
        },
        Microcode::Eor
    );
    r[Opcode::ALR as usize] = microcode_arr!(Microcode::AlrImmediate);
    r[Opcode::ANC as usize] = microcode_arr!(Microcode::AncImmediate);
    r[Opcode::ARR as usize] = microcode_arr!(Microcode::ArrImmediate);
    r[Opcode::AXS as usize] = microcode_arr!(Microcode::AxsImmediate);
    r[Opcode::LAX_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_load_alu(), Microcode::Lax);
    r[Opcode::LAX_ZERO_PAGE_Y as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedY {
            load_into_alu: true
        },
        Microcode::Lax
    );
    r[Opcode::LAX_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::Lax
    );
    r[Opcode::LAX_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: true,
            load_into_alu: true
        },
        Microcode::Lax
    );
    r[Opcode::LAX_INDEXED_INDIRECT as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: true
        },
        Microcode::Nop,
        Microcode::Lax
    );
    r[Opcode::LAX_INDIRECT_INDEXED as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: true,
            load_into_alu: true
        },
        Microcode::Lax
    );
    r[Opcode::SAX_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::Sax, Microcode::zero_page_save_alu());
    r[Opcode::SAX_ZERO_PAGE_Y as usize] = microcode_arr!(Microcode::Sax);
    r[Opcode::SAX_ABSOLUTE as usize] = microcode_arr!(Microcode::Sax);
    r[Opcode::SAX_INDEXED_INDIRECT as usize] = microcode_arr!(Microcode::Sax);
    r[Opcode::DCP_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_load_alu(), Microcode::Dcp);
    r[Opcode::DCP_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::Dcp
    );
    r[Opcode::DCP_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::Dcp
    );
    r[Opcode::DCP_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: true,
            load_into_alu: true
        },
        Microcode::Dcp
    );
    r[Opcode::DCP_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: true,
            load_into_alu: true
        },
        Microcode::Dcp
    );
    r[Opcode::DCP_INDEXED_INDIRECT as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: true
        },
        Microcode::Nop,
        Microcode::Dcp
    );
    r[Opcode::DCP_INDIRECT_INDEXED as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: true,
            load_into_alu: true
        },
        Microcode::Dcp
    );
    r[Opcode::ISC_ZERO_PAGE as usize] =
        microcode_arr!(Microcode::zero_page_load_alu(), Microcode::Isc);
    r[Opcode::ISC_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::Isc
    );
    r[Opcode::ISC_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::Isc
    );
    r[Opcode::ISC_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: true,
            load_into_alu: true
        },
        Microcode::Isc
    );
    r[Opcode::ISC_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: true,
            load_into_alu: true
        },
        Microcode::Isc
    );
    r[Opcode::ISC_INDEXED_INDIRECT as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: true
        },
        Microcode::Nop,
        Microcode::Isc
    );
    r[Opcode::ISC_INDIRECT_INDEXED as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: true,
            load_into_alu: true
        },
        Microcode::Isc
    );
    r[Opcode::RRA_ZERO_PAGE as usize] = microcode_arr!(
        Microcode::zero_page_load_alu(),
        Microcode::StoreAlu,
        Microcode::Rra,
        Microcode::StoreAlu
    );
    r[Opcode::RRA_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Rra,
        Microcode::StoreAlu
    );
    r[Opcode::RRA_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Rra,
        Microcode::StoreAlu
    );
    r[Opcode::RRA_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Rra,
        Microcode::StoreAlu
    );
    r[Opcode::RRA_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: false,
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Rra,
        Microcode::StoreAlu
    );
    r[Opcode::RRA_INDEXED_INDIRECT as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: true
        },
        Microcode::Nop,
        Microcode::StoreAlu,
        Microcode::Rra,
        Microcode::StoreAlu
    );
    r[Opcode::RRA_INDIRECT_INDEXED as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: false,
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Rra,
        Microcode::StoreAlu
    );
    r[Opcode::SLO_ZERO_PAGE as usize] = microcode_arr!(
        Microcode::zero_page_load_alu(),
        Microcode::StoreAlu,
        Microcode::Asl,
        Microcode::StoreAlu
    );
    r[Opcode::SLO_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Asl,
        Microcode::StoreAlu
    );
    r[Opcode::SLO_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Asl,
        Microcode::StoreAlu
    );
    r[Opcode::SLO_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Asl,
        Microcode::StoreAlu
    );
    r[Opcode::SLO_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: false,
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Asl,
        Microcode::StoreAlu
    );
    r[Opcode::SLO_INDEXED_INDIRECT as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: true
        },
        Microcode::Nop,
        Microcode::StoreAlu,
        Microcode::Asl,
        Microcode::StoreAlu
    );
    r[Opcode::SLO_INDIRECT_INDEXED as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: false,
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Asl,
        Microcode::StoreAlu
    );
    r[Opcode::SRE_ZERO_PAGE as usize] = microcode_arr!(
        Microcode::zero_page_load_alu(),
        Microcode::StoreAlu,
        Microcode::Lsr,
        Microcode::StoreAlu
    );
    r[Opcode::SRE_ZERO_PAGE_X as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Lsr,
        Microcode::StoreAlu
    );
    r[Opcode::SRE_ABSOLUTE as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteH {
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Lsr,
        Microcode::StoreAlu
    );
    r[Opcode::SRE_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Lsr,
        Microcode::StoreAlu
    );
    r[Opcode::SRE_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: false,
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Lsr,
        Microcode::StoreAlu
    );
    r[Opcode::SRE_INDEXED_INDIRECT as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::ZeroPageIndexedX {
            load_into_alu: false
        },
        Microcode::Indexed {
            load_into_alu: true
        },
        Microcode::Nop,
        Microcode::StoreAlu,
        Microcode::Lsr,
        Microcode::StoreAlu
    );
    r[Opcode::SRE_INDIRECT_INDEXED as usize] = microcode_arr!(
        Microcode::zero_page_addr(),
        Microcode::Indexed {
            load_into_alu: false
        },
        Microcode::AbsoluteIndexedYWithoutHigh {
            oops: false,
            load_into_alu: true
        },
        Microcode::StoreAlu,
        Microcode::Lsr,
        Microcode::StoreAlu
    );
    r[Opcode::SHX_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: false,
            load_into_alu: true
        },
        Microcode::Shx
    );
    r[Opcode::SHY_ABSOLUTE_INDEXED_X as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: true
        },
        Microcode::Shy
    );
    r[Opcode::TAS_ABSOLUTE_INDEXED_Y as usize] = microcode_arr!(
        Microcode::AbsoluteL,
        Microcode::AbsoluteIndexedY {
            oops: false,
            load_into_alu: true
        },
        Microcode::Tas
    );
    r
}

const OPCODE_TABLE: [ArrayVec<[Microcode; 7]>; 256] = build_opcode_table();

/// Each Microcode instruction executed by the CPU in a single cycle
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum Microcode {
    /// Fetch and decode next op code
    FetchAndDecode,
    /// Load accumulator with value from memory
    LoadA,
    /// Load X register with value from memory
    LoadX,
    /// Load Y register with value from memory
    LoadY,
    /// Store accumulator value to memory
    StoreA,
    /// Store X register value to memory
    StoreX,
    /// Store Y register value to memory
    StoreY,
    /// Read immediate value into accumulator
    LoadImmediateA,
    /// Read immediate value into X register
    LoadImmediateX,
    /// Read immediate value into Y register
    LoadImmediateY,

    /// Store cpu alu register into memory at cpu.ab
    StoreAlu,

    // addressing microcodes
    /// Take a byte from instruction data stream, set cpu address_bus field by read from memory use zero page addressing
    ZeroPage {
        load_into_alu: bool,
        save_alu: bool,
    },
    /// Impl ZeroPageIndexedX addressing, work after ZeroPage, set abl = (abl + x) % 256, set abh = 00
    ZeroPageIndexedX {
        load_into_alu: bool,
    },
    ZeroPageIndexedY {
        load_into_alu: bool,
    },
    /// Take a byte from instruction data stream, set cpu abl field
    AbsoluteL,
    /// Take a byte from instruction data stream, set cpu abh field
    AbsoluteH {
        load_into_alu: bool,
    },
    /// Take a byte from instruction data stream, set cpu abh field, add ab with
    /// x register value, set cpu ab field, retain_cycle if the 8bit plus
    /// operation overflows
    ///
    /// Must after AbsoluteL
    AbsoluteIndexedX {
        oops: bool,
        load_into_alu: bool,
    },
    /// Take a byte from instruction data stream, set cpu abh field, add ab with
    /// y register value, set cpu ab field, retain_cycle if the 8bit plus
    /// operation overflows
    ///
    /// Must after AbsoluteL
    AbsoluteIndexedY {
        oops: bool,
        load_into_alu: bool,
    },
    /// AbsoluteIndexedY but do not take high byte from instruction data stream, used to impl IndirectIndexed
    AbsoluteIndexedYWithoutHigh {
        oops: bool,
        load_into_alu: bool,
    },
    /// Do nothing, used in "oops" cycles of AbsoluteIndexed and Indirect Indexed addressing
    Nop,
    /// Load abl from memory at [ab], load abh from memory at [ab+1]
    /// append with Nop, because it is actually need two cycles
    Indexed {
        load_into_alu: bool,
    },
    /// Read immediate value from instruction data stream, but do not use it
    SkipImmediate,

    /// Take immediate value from instruction data stream, add to accumulator with carry
    AdcImmediate,
    /// Fetch a byte from memory use address saved in `data_latch`, then add to accumulator with carry
    Adc,

    /// Take immediate value from instruction data stream, subtract with carry from accumulator
    SbcImmediate,
    /// Fetch a byte from memory use address saved in `data_latch`, then subtract with carry from accumulator
    Sbc,

    /// Take immediate value from instruction data stream, compare with accumulator
    CmpImmediate,
    /// Fetch a byte from memory use address saved in `data_latch`, then compare with accumulator
    Cmp,

    /// Take immediate value from instruction data stream, compare with x register
    CpxImmediate,
    /// Fetch a byte from memory use address saved in `data_latch`, then compare with x register
    Cpx,

    /// Take immediate value from instruction data stream, compare with y register
    CpyImmediate,
    /// Fetch a byte from memory use address saved in `data_latch`, then compare with y register
    Cpy,

    /// Take immediate value from instruction data stream, or it with accumulator
    OraImmediate,
    /// Fetch a byte from memory use address saved in `data_latch`, then or with accumulator
    Ora,

    /// Take immediate value from instruction data stream, xor it with accumulator
    EorImmediate,
    /// Fetch a byte from memory use address saved in `data_latch`, then xor with accumulator
    Eor,

    /// Take immediate value from instruction data stream, and it with accumulator
    AndImmediate,
    /// Fetch a byte from memory use address saved in `data_latch`, then and with accumulator
    And,

    /// Test bits in accumulator against ALU value
    Bit,

    AslAccumulator,
    Asl,

    LsrAccumulator,
    Lsr,

    RolAccumulator,
    Rol,

    RorAccumulator,
    Ror,

    AlrImmediate,
    AncImmediate,
    ArrImmediate,
    AxsImmediate,

    Lax,
    Sax,
    Dcp,
    Isc,
    Rra,
    Slo,
    Sre,
    Shx,
    Shy,
    Tas,

    Pha,
    Pla,
    Php,
    Plp,

    JmpAbsolute,
    JmpIndirect,
    Jsr,
    Rts,
    Rti,

    Clc,
    Sec,
    Cld,
    Sed,
    Cli,
    Sei,
    Clv,

    Brk,

    Tax,
    Txa,
    Tay,
    Tya,
    Tsx,
    Txs,

    /// Read offset value from instruction data stream,
    /// If BranchTest is true, pc += offset, push one Noc if not cross page, push two Noc if cross page
    BranchRelative(BranchTest),

    /// Cpu will trapped infinitely if execute this microcode, used to impl illegal instructions that will lock up the CPU
    /// Only reset can restore the CPU from this state
    #[default]
    Kill,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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
    fn test<M: Mcu>(self, cpu: &Cpu2<M>) -> bool {
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
pub enum Opcode {}

impl Opcode {
    // Load and Save instructions

    const LDA_IMMEDIATE: u8 = 0xA9;
    const LDA_ZERO_PAGE: u8 = 0xA5;
    const LDA_ZERO_PAGE_X: u8 = 0xB5;
    const LDA_ABSOLUTE: u8 = 0xAD;
    const LDA_ABSOLUTE_INDEXED_X: u8 = 0xBD;
    const LDA_ABSOLUTE_INDEXED_Y: u8 = 0xB9;
    const LDA_INDIRECT_INDEXED: u8 = 0xA1;
    const LDA_INDIRECT_INDEXED_Y: u8 = 0xB1;

    const LDX_IMMEDIATE: u8 = 0xA2;
    const LDX_ZERO_PAGE: u8 = 0xA6;
    const LDX_ZERO_PAGE_Y: u8 = 0xB6;
    const LDX_ABSOLUTE: u8 = 0xAE;
    const LDX_ABSOLUTE_INDEXED_Y: u8 = 0xBE;

    const LDY_IMMEDIATE: u8 = 0xA0;
    const LDY_ZERO_PAGE: u8 = 0xA4;
    const LDY_ZERO_PAGE_X: u8 = 0xB4;
    const LDY_ABSOLUTE: u8 = 0xAC;
    const LDY_ABSOLUTE_INDEXED_X: u8 = 0xBC;

    const STA_ZERO_PAGE: u8 = 0x85;
    const STA_ZERO_PAGE_X: u8 = 0x95;
    const STA_ABSOLUTE: u8 = 0x8D;
    const STA_ABSOLUTE_INDEXED_X: u8 = 0x9D;
    const STA_ABSOLUTE_INDEXED_Y: u8 = 0x99;
    const STA_INDEXED_INDIRECT: u8 = 0x81;
    const STA_INDIRECT_INDEXED_Y: u8 = 0x91;

    const STX_ZERO_PAGE: u8 = 0x86;
    const STX_ZERO_PAGE_Y: u8 = 0x96;
    const STX_ABSOLUTE: u8 = 0x8E;

    const STY_ZERO_PAGE: u8 = 0x84;
    const STY_ZERO_PAGE_X: u8 = 0x94;
    const STY_ABSOLUTE: u8 = 0x8C;

    // Arithmetic instructions

    const ADC_IMMEDIATE: u8 = 0x69;
    const ADC_ZERO_PAGE: u8 = 0x65;
    const ADC_ZERO_PAGE_X: u8 = 0x75;
    const ADC_ABSOLUTE: u8 = 0x6D;
    const ADC_ABSOLUTE_INDEXED_X: u8 = 0x7D;
    const ADC_ABSOLUTE_INDEXED_Y: u8 = 0x79;
    const ADC_INDEXED_INDIRECT: u8 = 0x61;
    const ADC_INDIRECT_INDEXED: u8 = 0x71;

    const SBC_IMMEDIATE: u8 = 0xE9;
    const SBC_ZERO_PAGE: u8 = 0xE5;
    const SBC_ZERO_PAGE_X: u8 = 0xF5;
    const SBC_ABSOLUTE: u8 = 0xED;
    const SBC_ABSOLUTE_INDEXED_X: u8 = 0xFD;
    const SBC_ABSOLUTE_INDEXED_Y: u8 = 0xF9;
    const SBC_INDEXED_INDIRECT: u8 = 0xE1;
    const SBC_INDIRECT_INDEXED: u8 = 0xF1;

    const CMP_IMMEDIATE: u8 = 0xC9;
    const CMP_ZERO_PAGE: u8 = 0xC5;
    const CMP_ZERO_PAGE_X: u8 = 0xD5;
    const CMP_ABSOLUTE: u8 = 0xCD;
    const CMP_ABSOLUTE_INDEXED_X: u8 = 0xDD;
    const CMP_ABSOLUTE_INDEXED_Y: u8 = 0xD9;
    const CMP_INDEXED_INDIRECT: u8 = 0xC1;
    const CMP_INDIRECT_INDEXED: u8 = 0xD1;

    const CPX_IMMEDIATE: u8 = 0xE0;
    const CPX_ZERO_PAGE: u8 = 0xE4;
    const CPX_ABSOLUTE: u8 = 0xEC;

    const CPY_IMMEDIATE: u8 = 0xC0;
    const CPY_ZERO_PAGE: u8 = 0xC4;
    const CPY_ABSOLUTE: u8 = 0xCC;

    // Shift and Rotate instructions

    const ASL_ACCUMULATOR: u8 = 0x0A;
    const ASL_ZERO_PAGE: u8 = 0x06;
    const ASL_ZERO_PAGE_X: u8 = 0x16;
    const ASL_ABSOLUTE: u8 = 0x0E;
    const ASL_ABSOLUTE_INDEXED_X: u8 = 0x1E;

    const LSR_ACCUMULATOR: u8 = 0x4A;
    const LSR_ZERO_PAGE: u8 = 0x46;
    const LSR_ZERO_PAGE_X: u8 = 0x56;
    const LSR_ABSOLUTE: u8 = 0x4E;
    const LSR_ABSOLUTE_INDEXED_X: u8 = 0x5E;

    const ROL_ACCUMULATOR: u8 = 0x2A;
    const ROL_ZERO_PAGE: u8 = 0x26;
    const ROL_ZERO_PAGE_X: u8 = 0x36;
    const ROL_ABSOLUTE: u8 = 0x2E;
    const ROL_ABSOLUTE_INDEXED_X: u8 = 0x3E;

    const ROR_ACCUMULATOR: u8 = 0x6A;
    const ROR_ZERO_PAGE: u8 = 0x66;
    const ROR_ZERO_PAGE_X: u8 = 0x76;
    const ROR_ABSOLUTE: u8 = 0x6E;
    const ROR_ABSOLUTE_INDEXED_X: u8 = 0x7E;

    // Logic instructions

    const AND_IMMEDIATE: u8 = 0x29;
    const AND_ZERO_PAGE: u8 = 0x25;
    const AND_ZERO_PAGE_X: u8 = 0x35;
    const AND_ABSOLUTE: u8 = 0x2D;
    const AND_ABSOLUTE_INDEXED_X: u8 = 0x3D;
    const AND_ABSOLUTE_INDEXED_Y: u8 = 0x39;
    const AND_INDEXED_INDIRECT: u8 = 0x21;
    const AND_INDIRECT_INDEXED: u8 = 0x31;

    const ORA_IMMEDIATE: u8 = 0x09;
    const ORA_ZERO_PAGE: u8 = 0x05;
    const ORA_ZERO_PAGE_X: u8 = 0x15;
    const ORA_ABSOLUTE: u8 = 0x0D;
    const ORA_ABSOLUTE_INDEXED_X: u8 = 0x1D;
    const ORA_ABSOLUTE_INDEXED_Y: u8 = 0x19;
    const ORA_INDEXED_INDIRECT: u8 = 0x01;
    const ORA_INDIRECT_INDEXED: u8 = 0x11;

    const EOR_IMMEDIATE: u8 = 0x49;
    const EOR_ZERO_PAGE: u8 = 0x45;
    const EOR_ZERO_PAGE_X: u8 = 0x55;
    const EOR_ABSOLUTE: u8 = 0x4D;
    const EOR_ABSOLUTE_INDEXED_X: u8 = 0x5D;
    const EOR_ABSOLUTE_INDEXED_Y: u8 = 0x59;
    const EOR_INDEXED_INDIRECT: u8 = 0x41;
    const EOR_INDIRECT_INDEXED: u8 = 0x51;

    const BIT_ZERO_PAGE: u8 = 0x24;
    const BIT_ABSOLUTE: u8 = 0x2C;

    // Branch instructions

    const BCC: u8 = 0x90;
    const BCS: u8 = 0xB0;
    const BNE: u8 = 0xD0;
    const BEQ: u8 = 0xF0;
    const BPL: u8 = 0x10;
    const BMI: u8 = 0x30;
    const BVC: u8 = 0x50;
    const BVS: u8 = 0x70;

    // Transfer Instructions

    const TAX: u8 = 0xAA;
    const TXA: u8 = 0x8A;
    const TAY: u8 = 0xA8;
    const TYA: u8 = 0x98;
    const TSX: u8 = 0xBA;
    const TXS: u8 = 0x9A;

    // Stack Instructions

    const PHA: u8 = 0x48;
    const PLA: u8 = 0x68;
    const PHP: u8 = 0x08;
    const PLP: u8 = 0x28;

    // Subroutine and Jump Instructions

    const JMP_ABSOLUTE: u8 = 0x4C;
    const JMP_INDIRECT: u8 = 0x6C;
    const JSR: u8 = 0x20;
    const RTS: u8 = 0x60;
    const RTI: u8 = 0x40;

    // Set and Clear Instructions
    const CLC: u8 = 0x18;
    const SEC: u8 = 0x38;
    const CLD: u8 = 0xD8;
    const SED: u8 = 0xF8;
    const CLI: u8 = 0x58;
    const SEI: u8 = 0x78;
    const CLV: u8 = 0xB8;

    // Miscellaneous Instructions
    const NOP: u8 = 0xEA;
    const BRK: u8 = 0x00;

    // Undocumented/Illegal Instructions

    //   Combined instructions

    const ALR: u8 = 0x4B;
    const ANC: u8 = 0x2B;
    const ARR: u8 = 0x6B;
    const AXS: u8 = 0xCB;

    const LAX_ZERO_PAGE: u8 = 0xA7;
    const LAX_ZERO_PAGE_Y: u8 = 0xB7;
    const LAX_ABSOLUTE: u8 = 0xAF;
    const LAX_ABSOLUTE_INDEXED_Y: u8 = 0xBF;
    const LAX_INDEXED_INDIRECT: u8 = 0xA3;
    const LAX_INDIRECT_INDEXED: u8 = 0xB3;

    const SAX_ZERO_PAGE: u8 = 0x87;
    const SAX_ZERO_PAGE_Y: u8 = 0x97;
    const SAX_ABSOLUTE: u8 = 0x8F;
    const SAX_INDEXED_INDIRECT: u8 = 0x83;

    //   Read-modify-write instructions
    const DCP_ZERO_PAGE: u8 = 0xC7;
    const DCP_ZERO_PAGE_X: u8 = 0xD7;
    const DCP_ABSOLUTE: u8 = 0xCF;
    const DCP_ABSOLUTE_INDEXED_X: u8 = 0xDF;
    const DCP_ABSOLUTE_INDEXED_Y: u8 = 0xDB;
    const DCP_INDEXED_INDIRECT: u8 = 0xC3;
    const DCP_INDIRECT_INDEXED: u8 = 0xD3;

    const ISC_ZERO_PAGE: u8 = 0xE7;
    const ISC_ZERO_PAGE_X: u8 = 0xF7;
    const ISC_ABSOLUTE: u8 = 0xEF;
    const ISC_ABSOLUTE_INDEXED_X: u8 = 0xFF;
    const ISC_ABSOLUTE_INDEXED_Y: u8 = 0xFB;
    const ISC_INDEXED_INDIRECT: u8 = 0xE3;
    const ISC_INDIRECT_INDEXED: u8 = 0xF3;

    const RRA_ZERO_PAGE: u8 = 0x67;
    const RRA_ZERO_PAGE_X: u8 = 0x77;
    const RRA_ABSOLUTE: u8 = 0x6F;
    const RRA_ABSOLUTE_INDEXED_X: u8 = 0x7F;
    const RRA_ABSOLUTE_INDEXED_Y: u8 = 0x7B;
    const RRA_INDEXED_INDIRECT: u8 = 0x63;
    const RRA_INDIRECT_INDEXED: u8 = 0x73;

    const SLO_ZERO_PAGE: u8 = 0x07;
    const SLO_ZERO_PAGE_X: u8 = 0x17;
    const SLO_ABSOLUTE: u8 = 0x0F;
    const SLO_ABSOLUTE_INDEXED_X: u8 = 0x1F;
    const SLO_ABSOLUTE_INDEXED_Y: u8 = 0x1B;
    const SLO_INDEXED_INDIRECT: u8 = 0x03;
    const SLO_INDIRECT_INDEXED: u8 = 0x13;

    const SRE_ZERO_PAGE: u8 = 0x47;
    const SRE_ZERO_PAGE_X: u8 = 0x57;
    const SRE_ABSOLUTE: u8 = 0x4F;
    const SRE_ABSOLUTE_INDEXED_X: u8 = 0x5F;
    const SRE_ABSOLUTE_INDEXED_Y: u8 = 0x5B;
    const SRE_INDEXED_INDIRECT: u8 = 0x43;
    const SRE_INDIRECT_INDEXED: u8 = 0x53;

    const SHX_ABSOLUTE_INDEXED_Y: u8 = 0x9E;
    const SHY_ABSOLUTE_INDEXED_X: u8 = 0x9C;
    const TAS_ABSOLUTE_INDEXED_Y: u8 = 0x9B;

    // duplicated opcodes
    const USBC: u8 = 0xEB;
    const NOP1: u8 = 0x1A;
    const NOP2: u8 = 0x3A;
    const NOP3: u8 = 0x5A;
    const NOP4: u8 = 0x7A;
    const NOP5: u8 = 0xDA;
    const NOP6: u8 = 0xFA;
    const NOP_IMMEDIATE1: u8 = 0x80;
    const NOP_IMMEDIATE2: u8 = 0x82;
    const NOP_IMMEDIATE3: u8 = 0x89;
    const NOP_IMMEDIATE4: u8 = 0xC2;
    const NOP_IMMEDIATE5: u8 = 0xE2;
    const NOP_ZERO_PAGE1: u8 = 0x04;
    const NOP_ZERO_PAGE2: u8 = 0x44;
    const NOP_ZERO_PAGE3: u8 = 0x64;
    const NOP_ZERO_PAGE_X1: u8 = 0x14;
    const NOP_ZERO_PAGE_X2: u8 = 0x34;
    const NOP_ZERO_PAGE_X3: u8 = 0x54;
    const NOP_ZERO_PAGE_X4: u8 = 0x74;
    const NOP_ZERO_PAGE_X5: u8 = 0xD4;
    const NOP_ZERO_PAGE_X6: u8 = 0xF4;
    const NOP_ABSOLUTE: u8 = 0x0C;
    const NOP_ABSOLUTE_INDEXED_X1: u8 = 0x1C;
    const NOP_ABSOLUTE_INDEXED_X2: u8 = 0x3C;
    const NOP_ABSOLUTE_INDEXED_X3: u8 = 0x5C;
    const NOP_ABSOLUTE_INDEXED_X4: u8 = 0x7C;
    const NOP_ABSOLUTE_INDEXED_X5: u8 = 0xDC;
    const NOP_ABSOLUTE_INDEXED_X6: u8 = 0xFC;

    const KIL1: u8 = 0x02;
    const KIL2: u8 = 0x12;
    const KIL3: u8 = 0x22;
    const KIL4: u8 = 0x32;
    const KIL5: u8 = 0x42;
    const KIL6: u8 = 0x52;
    const KIL7: u8 = 0x62;
    const KIL8: u8 = 0x72;
    const KIL9: u8 = 0x92;
    const KIL10: u8 = 0xB2;
    const KIL11: u8 = 0xD2;
    const KIL12: u8 = 0xF2;
}

impl Microcode {
    /// Execute the micro code
    pub fn exec<M: Mcu>(self, cpu: &mut Cpu2<M>) {
        match self {
            Microcode::FetchAndDecode => Self::fetch_and_decode(cpu),
            Microcode::LoadA => Self::load_a(cpu),
            Microcode::LoadX => Self::load_x(cpu),
            Microcode::LoadY => Self::load_y(cpu),
            Microcode::StoreA => Self::store_a(cpu),
            Microcode::StoreX => Self::store_x(cpu),
            Microcode::StoreY => Self::store_y(cpu),
            Microcode::LoadImmediateA => Self::load_immediate_a(cpu),
            Microcode::LoadImmediateX => Self::load_immediate_x(cpu),
            Microcode::LoadImmediateY => Self::load_immediate_y(cpu),
            Microcode::ZeroPage {
                load_into_alu,
                save_alu,
            } => Self::zero_page(cpu, load_into_alu, save_alu),
            Microcode::ZeroPageIndexedX { load_into_alu } => {
                Self::zero_page_indexed_x(cpu, load_into_alu)
            }
            Microcode::ZeroPageIndexedY { load_into_alu } => {
                Self::zero_page_indexed_y(cpu, load_into_alu)
            }
            Microcode::AbsoluteL => Self::absolute_l(cpu),
            Microcode::AbsoluteH { load_into_alu } => Self::absolute_h(cpu, load_into_alu),
            Microcode::AbsoluteIndexedX {
                oops,
                load_into_alu,
            } => Self::absolute_indexed_x(cpu, oops, load_into_alu),
            Microcode::AbsoluteIndexedY {
                oops,
                load_into_alu,
            } => Self::absolute_indexed_y(cpu, oops, load_into_alu),
            Microcode::AbsoluteIndexedYWithoutHigh {
                oops,
                load_into_alu,
            } => Self::absolute_indexed_y_without_high(cpu, oops, load_into_alu),
            Microcode::AdcImmediate => Self::adc_immediate(cpu),
            Microcode::Adc => cpu.adc(),
            Microcode::SbcImmediate => Self::sbc_immediate(cpu),
            Microcode::Sbc => cpu.sbc(),

            Microcode::CmpImmediate => Self::cmp_immediate(cpu),
            Microcode::Cmp => cpu.cmp(),
            Microcode::CpxImmediate => Self::cpx_immediate(cpu),
            Microcode::Cpx => cpu.cpx(),
            Microcode::CpyImmediate => Self::cpy_immediate(cpu),
            Microcode::Cpy => cpu.cpy(),

            Microcode::OraImmediate => Self::ora_immediate(cpu),
            Microcode::Ora => cpu.ora(),
            Microcode::EorImmediate => Self::eor_immediate(cpu),
            Microcode::Eor => cpu.eor(),

            Microcode::AndImmediate => Self::and_immediate(cpu),
            Microcode::And => cpu.and(),
            Microcode::Bit => cpu.bit(),
            Microcode::StoreAlu => Self::store_alu(cpu),
            Microcode::Nop => {}
            Microcode::SkipImmediate => {
                cpu.inc_read_byte();
            }
            Microcode::Indexed { load_into_alu } => Self::indexed(cpu, load_into_alu),
            Microcode::AslAccumulator => Self::asl_accumulator(cpu),
            Microcode::Asl => Self::asl(cpu),
            Microcode::LsrAccumulator => Self::lsr_accumulator(cpu),
            Microcode::Lsr => Self::lsr(cpu),
            Microcode::RolAccumulator => Self::rol_accumulator(cpu),
            Microcode::Rol => Self::rol(cpu),
            Microcode::RorAccumulator => Self::ror_accumulator(cpu),
            Microcode::Ror => Self::ror(cpu),

            Microcode::AlrImmediate => Self::alr_immediate(cpu),
            Microcode::AncImmediate => Self::anc_immediate(cpu),
            Microcode::ArrImmediate => Self::arr_immediate(cpu),
            Microcode::AxsImmediate => Self::axs_immediate(cpu),

            Microcode::Lax => cpu.lax(),
            Microcode::Sax => cpu.sax(),
            Microcode::Dcp => cpu.dcp(),
            Microcode::Isc => cpu.isc(),
            Microcode::Rra => cpu.rra(),
            Microcode::Slo => cpu.slo(),
            Microcode::Sre => cpu.sre(),
            Microcode::Shx => cpu.shx(),
            Microcode::Shy => cpu.shy(),
            Microcode::Tas => cpu.tas(),

            Microcode::Pha => cpu.pha(),
            Microcode::Pla => cpu.pla(),
            Microcode::Php => cpu.php(),
            Microcode::Plp => cpu.plp(),

            Microcode::JmpAbsolute => cpu.jmp_absolute(),
            Microcode::JmpIndirect => cpu.jmp_indirect(),
            Microcode::Jsr => cpu.jsr(),
            Microcode::Rts => cpu.rts(),
            Microcode::Rti => cpu.rti(),

            Microcode::Clc => cpu.set_flag(Flag::Carry, false),
            Microcode::Sec => cpu.set_flag(Flag::Carry, true),
            Microcode::Cld => cpu.set_flag(Flag::Decimal, false),
            Microcode::Sed => cpu.set_flag(Flag::Decimal, true),
            Microcode::Cli => cpu.set_flag(Flag::InterruptDisabled, false),
            Microcode::Sei => cpu.set_flag(Flag::InterruptDisabled, true),
            Microcode::Clv => cpu.set_flag(Flag::Overflow, false),

            Microcode::Brk => cpu.brk(),

            Microcode::Tax => cpu.tax(),
            Microcode::Txa => cpu.txa(),
            Microcode::Tay => cpu.tay(),
            Microcode::Tya => cpu.tya(),
            Microcode::Tsx => cpu.tsx(),
            Microcode::Txs => cpu.txs(),

            Microcode::BranchRelative(branch_test) => Self::branch_relative(cpu, branch_test),
            Microcode::Kill => cpu.freezed = true,
        }
    }

    fn fetch_and_decode<M: Mcu>(cpu: &mut Cpu2<M>) {
        let opcode = cpu.inc_read_byte();
        cpu.opcode = opcode;
        cpu.push_microcodes(&OPCODE_TABLE[opcode as usize]);
    }

    fn load_a<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.a = cpu.read_byte(cpu.ab);
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
    }

    fn load_x<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.x = cpu.read_byte(cpu.ab);
        cpu.update_negative_flag(cpu.x);
        cpu.update_zero_flag(cpu.x);
    }

    fn load_y<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.y = cpu.read_byte(cpu.ab);
        cpu.update_negative_flag(cpu.y);
        cpu.update_zero_flag(cpu.y);
    }

    fn store_a<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.write_byte(cpu.ab, cpu.a);
    }

    fn store_x<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.write_byte(cpu.ab, cpu.x);
    }

    fn store_y<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.write_byte(cpu.ab, cpu.y);
    }

    fn load_immediate_a<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.a = cpu.inc_read_byte();
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
    }

    fn load_immediate_x<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.x = cpu.inc_read_byte();
        cpu.update_negative_flag(cpu.x);
        cpu.update_zero_flag(cpu.x);
    }

    fn load_immediate_y<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.y = cpu.inc_read_byte();
        cpu.update_negative_flag(cpu.y);
        cpu.update_zero_flag(cpu.y);
    }

    fn and_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.and();
    }

    fn alr_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.alr();
    }

    fn anc_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.anc();
    }

    fn arr_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.arr();
    }

    fn axs_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.axs();
    }

    fn zero_page<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool, save_alu: bool) {
        let addr = cpu.inc_read_byte();
        cpu.ab = addr as u16;
        if load_into_alu {
            cpu.load_alu();
        }
        if save_alu {
            cpu.write_byte(cpu.ab, cpu.alu);
        }
    }

    fn zero_page_indexed_x<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
        cpu.ab = cpu.abl().wrapping_add(cpu.x) as u16;
        if load_into_alu {
            cpu.load_alu();
        }
    }

    fn zero_page_indexed_y<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
        cpu.ab = cpu.abl().wrapping_add(cpu.y) as u16;
        if load_into_alu {
            cpu.load_alu();
        }
    }

    fn absolute_l<M: Mcu>(cpu: &mut Cpu2<M>) {
        let low = cpu.inc_read_byte();
        cpu.set_abl(low);
    }

    fn absolute_h<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
        let high = cpu.inc_read_byte();
        cpu.set_abh(high);
        if load_into_alu {
            cpu.load_alu();
        }
    }

    fn adc_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.adc();
    }

    fn sbc_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.sbc();
    }

    fn cmp_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.cmp();
    }

    fn cpx_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.cpx();
    }

    fn cpy_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.cpy();
    }

    fn ora_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.ora();
    }

    fn eor_immediate<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.inc_read_byte();
        cpu.eor();
    }

    fn absolute_indexed_x<M: Mcu>(cpu: &mut Cpu2<M>, oops: bool, load_into_alu: bool) {
        let abh = cpu.inc_read_byte();
        cpu.set_abh(abh);
        cpu.ab = cpu.ab.wrapping_add(cpu.x as u16);
        if load_into_alu {
            cpu.load_alu();
        }
        if oops && abh != cpu.abh() {
            cpu.retain_cycle();
        }
    }

    fn absolute_indexed_y<M: Mcu>(cpu: &mut Cpu2<M>, oops: bool, load_into_alu: bool) {
        let abh = cpu.inc_read_byte();
        cpu.set_abh(abh);
        Self::absolute_indexed_y_without_high(cpu, oops, load_into_alu);
    }

    fn absolute_indexed_y_without_high<M: Mcu>(cpu: &mut Cpu2<M>, oops: bool, load_into_alu: bool) {
        let abh = cpu.abh();
        cpu.ab = cpu.ab.wrapping_add(cpu.y as u16);
        if oops && abh != cpu.abh() {
            cpu.retain_cycle();
        }
        if load_into_alu {
            cpu.load_alu();
        }
    }

    fn store_alu<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.write_byte(cpu.ab, cpu.alu);
    }

    fn indexed<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
        let addr_low = cpu.read_byte(cpu.ab);
        let addr_high = cpu.read_byte(cpu.ab.wrapping_add(1));
        cpu.ab = (addr_high as u16) << 8 | addr_low as u16;
        if load_into_alu {
            cpu.alu = cpu.read_byte(cpu.ab);
        }
    }

    fn asl_accumulator<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.a = cpu.asl(cpu.a);
    }

    fn asl<M: Mcu>(cpu: &mut Cpu2<M>) {
        cpu.alu = cpu.asl(cpu.alu);
    }

    fn lsr_accumulator<M: Mcu>(cpu: &mut Cpu2<M>) {
        let v = cpu.a;
        cpu.set_flag(Flag::Carry, v & 0x01 != 0);
        cpu.a = v >> 1;
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
    }

    fn lsr<M: Mcu>(cpu: &mut Cpu2<M>) {
        let v = cpu.alu;
        cpu.set_flag(Flag::Carry, v & 0x01 != 0);
        cpu.alu = v >> 1;
        cpu.update_negative_flag(cpu.alu);
        cpu.update_zero_flag(cpu.alu);
    }

    fn rol_accumulator<M: Mcu>(cpu: &mut Cpu2<M>) {
        let v = cpu.a;
        let new = (v << 1) | (cpu.flag(Flag::Carry) as u8);
        cpu.set_flag(Flag::Carry, v & 0x80 != 0);
        cpu.a = new;
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
    }

    fn rol<M: Mcu>(cpu: &mut Cpu2<M>) {
        let v = cpu.alu;
        let new = (v << 1) | (cpu.flag(Flag::Carry) as u8);
        cpu.set_flag(Flag::Carry, v & 0x80 != 0);
        cpu.alu = new;
        cpu.update_negative_flag(cpu.alu);
        cpu.update_zero_flag(cpu.alu);
    }

    fn ror_accumulator<M: Mcu>(cpu: &mut Cpu2<M>) {
        let v = cpu.a;
        let new = (v >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7);
        cpu.set_flag(Flag::Carry, v & 0x01 != 0);
        cpu.a = new;
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
    }

    fn ror<M: Mcu>(cpu: &mut Cpu2<M>) {
        let v = cpu.alu;
        let new = (v >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7);
        cpu.set_flag(Flag::Carry, v & 0x01 != 0);
        cpu.alu = new;
        cpu.update_negative_flag(cpu.alu);
        cpu.update_zero_flag(cpu.alu);
    }

    fn branch_relative<M: Mcu>(cpu: &mut Cpu2<M>, branch_test: BranchTest) {
        let offset = cpu.inc_read_byte();
        if branch_test.test(cpu) {
            let pch = cpu.pch();
            cpu.pc = cpu.pc.wrapping_add((offset as i8) as u16);
            cpu.retain_cycle();
            if pch != cpu.pch() {
                cpu.retain_cycle();
            }
        }
    }

    const fn zero_page_load_alu() -> Self {
        Self::ZeroPage {
            load_into_alu: true,
            save_alu: false,
        }
    }

    const fn zero_page_save_alu() -> Self {
        Self::ZeroPage {
            load_into_alu: false,
            save_alu: true,
        }
    }

    const fn zero_page_addr() -> Self {
        Self::ZeroPage {
            load_into_alu: false,
            save_alu: false,
        }
    }
}

#[cfg(test)]
mod tests;
