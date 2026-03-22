use super::Cpu2;
use crate::{mcu::Mcu, Flag};

/// Each Microcode instruction executed by the CPU in a single cycle
#[derive(Debug, Clone, Copy, PartialEq)]
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

    /// Take immediate value from instruction data stream, add to accumulator with carry
    AdcImmediate,
    /// Fetch a byte from memory use address saved in `data_latch`, then add to accumulator with carry
    Adc,

    /// Take immediate value from instruction data stream, subtract with carry from accumulator
    SbcImmediate,
    /// Fetch a byte from memory use address saved in `data_latch`, then subtract with carry from accumulator
    Sbc,

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

    /// Read offset value from instruction data stream,
    /// If BranchTest is true, pc += offset, push one Noc if not cross page, push two Noc if cross page
    BranchRelative(BranchTest),
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
    pub const BCC: u8 = 0x90;
    pub const BCS: u8 = 0xB0;
    pub const BEQ: u8 = 0xF0;
    pub const BMI: u8 = 0x30;
    pub const BNE: u8 = 0xD0;
    pub const BPL: u8 = 0x10;
    pub const BVC: u8 = 0x50;
    pub const BVS: u8 = 0x70;

    pub const BIT_ZERO_PAGE: u8 = 0x24;
    pub const BIT_ABSOLUTE: u8 = 0x2C;

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
    pub const STA_INDIRECT_INDEXED_X: u8 = 0x81;
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
            Microcode::ZeroPage { load_into_alu } => Self::zero_page(cpu, load_into_alu),
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

            Microcode::OraImmediate => Self::ora_immediate(cpu),
            Microcode::Ora => cpu.ora(),
            Microcode::EorImmediate => Self::eor_immediate(cpu),
            Microcode::Eor => cpu.eor(),

            Microcode::AndImmediate => Self::and_immediate(cpu),
            Microcode::And => cpu.and(),
            Microcode::Bit => cpu.bit(),
            Microcode::StoreAlu => Self::store_alu(cpu),
            Microcode::Nop => {}
            Microcode::Indexed { load_into_alu } => Self::indexed(cpu, load_into_alu),
            Microcode::AslAccumulator => Self::asl_accumulator(cpu),
            Microcode::Asl => Self::asl(cpu),
            Microcode::LsrAccumulator => Self::lsr_accumulator(cpu),
            Microcode::Lsr => Self::lsr(cpu),
            Microcode::RolAccumulator => Self::rol_accumulator(cpu),
            Microcode::Rol => Self::rol(cpu),
            Microcode::RorAccumulator => Self::ror_accumulator(cpu),
            Microcode::Ror => Self::ror(cpu),

            Microcode::BranchRelative(branch_test) => Self::branch_relative(cpu, branch_test),
        }
    }

    fn fetch_and_decode<M: Mcu>(cpu: &mut Cpu2<M>) {
        let opcode = cpu.inc_read_byte();
        cpu.opcode = opcode;
        match opcode {
            Opcode::AND_IMMEDIATE => {
                cpu.push_microcode(Microcode::AndImmediate);
            }
            Opcode::AND_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: true,
                });
                cpu.push_microcode(Microcode::And);
            }
            Opcode::AND_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, true);
                cpu.push_microcode(Microcode::And);
            }
            Opcode::AND_ABSOLUTE => {
                absolute_addressing(cpu, true);
                cpu.push_microcode(Microcode::And);
            }
            Opcode::AND_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::And);
            }
            Opcode::AND_ABSOLUTE_INDEXED_Y => {
                absolute_indexed_y_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::And);
            }
            Opcode::AND_INDEXED_INDIRECT => {
                indexed_indirect_addressing(cpu, true);
                cpu.push_microcode(Microcode::And);
            }
            Opcode::AND_INDIRECT_INDEXED => {
                indirect_indexed_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::And);
            }

            Opcode::LDA_IMMEDIATE => {
                cpu.push_microcode(Microcode::LoadImmediateA);
            }
            Opcode::LDA_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: false,
                });
                cpu.push_microcode(Microcode::LoadA);
            }
            Opcode::LDA_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, false);
                cpu.push_microcode(Microcode::LoadA);
            }
            Opcode::LDA_ABSOLUTE => {
                absolute_addressing(cpu, false);
                cpu.push_microcode(Microcode::LoadA);
            }
            Opcode::LDA_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, true, false);
                cpu.push_microcode(Microcode::LoadA);
            }
            Opcode::LDA_ABSOLUTE_INDEXED_Y => {
                absolute_indexed_y_addressing(cpu, true, false);
                cpu.push_microcode(Microcode::LoadA);
            }
            Opcode::LDA_INDIRECT_INDEXED => {
                indexed_indirect_addressing(cpu, false);
                cpu.push_microcode(Microcode::LoadA);
            }
            Opcode::LDA_INDIRECT_INDEXED_Y => {
                indirect_indexed_addressing(cpu, true, false);
                cpu.push_microcode(Microcode::LoadA);
            }

            Opcode::LDX_IMMEDIATE => {
                cpu.push_microcode(Microcode::LoadImmediateX);
            }
            Opcode::LDX_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: false,
                });
                cpu.push_microcode(Microcode::LoadX);
            }
            Opcode::LDX_ZERO_PAGE_Y => {
                zero_page_indexed_y_addressing(cpu, false);
                cpu.push_microcode(Microcode::LoadX);
            }
            Opcode::LDX_ABSOLUTE => {
                absolute_addressing(cpu, false);
                cpu.push_microcode(Microcode::LoadX);
            }
            Opcode::LDX_ABSOLUTE_INDEXED_Y => {
                absolute_indexed_y_addressing(cpu, true, false);
                cpu.push_microcode(Microcode::LoadX);
            }

            Opcode::LDY_IMMEDIATE => {
                cpu.push_microcode(Microcode::LoadImmediateY);
            }
            Opcode::LDY_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: false,
                });
                cpu.push_microcode(Microcode::LoadY);
            }
            Opcode::LDY_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, false);
                cpu.push_microcode(Microcode::LoadY);
            }
            Opcode::LDY_ABSOLUTE => {
                absolute_addressing(cpu, false);
                cpu.push_microcode(Microcode::LoadY);
            }
            Opcode::LDY_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, true, false);
                cpu.push_microcode(Microcode::LoadY);
            }

            Opcode::STA_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: false,
                });
                cpu.push_microcode(Microcode::StoreA);
            }
            Opcode::STA_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, false);
                cpu.push_microcode(Microcode::StoreA);
            }
            Opcode::STA_ABSOLUTE => {
                absolute_addressing(cpu, false);
                cpu.push_microcode(Microcode::StoreA);
            }
            Opcode::STA_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, false, false);
                cpu.push_microcode(Microcode::StoreA);
            }
            Opcode::STA_ABSOLUTE_INDEXED_Y => {
                absolute_indexed_y_addressing(cpu, false, false);
                cpu.push_microcode(Microcode::StoreA);
            }
            Opcode::STA_INDIRECT_INDEXED_X => {
                indexed_indirect_addressing(cpu, false);
                cpu.push_microcode(Microcode::StoreA);
            }
            Opcode::STA_INDIRECT_INDEXED_Y => {
                indirect_indexed_addressing(cpu, false, false);
                cpu.push_microcode(Microcode::StoreA);
            }

            Opcode::STX_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: false,
                });
                cpu.push_microcode(Microcode::StoreX);
            }
            Opcode::STX_ZERO_PAGE_Y => {
                zero_page_indexed_y_addressing(cpu, false);
                cpu.push_microcode(Microcode::StoreX);
            }
            Opcode::STX_ABSOLUTE => {
                absolute_addressing(cpu, false);
                cpu.push_microcode(Microcode::StoreX);
            }

            Opcode::STY_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: false,
                });
                cpu.push_microcode(Microcode::StoreY);
            }
            Opcode::STY_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, false);
                cpu.push_microcode(Microcode::StoreY);
            }
            Opcode::STY_ABSOLUTE => {
                absolute_addressing(cpu, false);
                cpu.push_microcode(Microcode::StoreY);
            }

            Opcode::BIT_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: true,
                });
                cpu.push_microcode(Microcode::Bit);
            }
            Opcode::BIT_ABSOLUTE => {
                absolute_addressing(cpu, true);
                cpu.push_microcode(Microcode::Bit);
            }

            Opcode::ADC_IMMEDIATE => {
                cpu.push_microcode(Microcode::AdcImmediate);
            }
            Opcode::ADC_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: true,
                });
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, true);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_ABSOLUTE => {
                absolute_addressing(cpu, true);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_ABSOLUTE_INDEXED_Y => {
                absolute_indexed_y_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_INDEXED_INDIRECT => {
                indexed_indirect_addressing(cpu, true);
                cpu.push_microcode(Microcode::Adc);
            }
            Opcode::ADC_INDIRECT_INDEXED => {
                indirect_indexed_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::Adc);
            }

            Opcode::ASL_ACCUMULATOR => {
                cpu.push_microcode(Microcode::AslAccumulator);
            }
            Opcode::ASL_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: true,
                });
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Asl);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::ASL_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Asl);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::ASL_ABSOLUTE => {
                absolute_addressing(cpu, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Asl);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::ASL_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, false, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Asl);
                cpu.push_microcode(Microcode::StoreAlu);
            }

            Opcode::LSR_ACCUMULATOR => {
                cpu.push_microcode(Microcode::LsrAccumulator);
            }
            Opcode::LSR_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: true,
                });
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Lsr);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::LSR_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Lsr);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::LSR_ABSOLUTE => {
                absolute_addressing(cpu, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Lsr);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::LSR_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, false, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Lsr);
                cpu.push_microcode(Microcode::StoreAlu);
            }

            Opcode::ROL_ACCUMULATOR => {
                cpu.push_microcode(Microcode::RolAccumulator);
            }
            Opcode::ROL_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: true,
                });
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Rol);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::ROL_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Rol);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::ROL_ABSOLUTE => {
                absolute_addressing(cpu, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Rol);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::ROL_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, false, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Rol);
                cpu.push_microcode(Microcode::StoreAlu);
            }

            Opcode::ROR_ACCUMULATOR => {
                cpu.push_microcode(Microcode::RorAccumulator);
            }
            Opcode::ROR_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: true,
                });
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Ror);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::ROR_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Ror);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::ROR_ABSOLUTE => {
                absolute_addressing(cpu, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Ror);
                cpu.push_microcode(Microcode::StoreAlu);
            }
            Opcode::ROR_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, false, true);
                cpu.push_microcode(Microcode::StoreAlu);
                cpu.push_microcode(Microcode::Ror);
                cpu.push_microcode(Microcode::StoreAlu);
            }

            Opcode::BCC => {
                cpu.push_microcode(Microcode::BranchRelative(BranchTest::IfCarryClear));
            }
            Opcode::BCS => {
                cpu.push_microcode(Microcode::BranchRelative(BranchTest::IfCarrySet));
            }
            Opcode::BEQ => {
                cpu.push_microcode(Microcode::BranchRelative(BranchTest::IfZeroSet));
            }
            Opcode::BMI => {
                cpu.push_microcode(Microcode::BranchRelative(BranchTest::IfNegativeSet));
            }
            Opcode::BNE => {
                cpu.push_microcode(Microcode::BranchRelative(BranchTest::IfZeroClear));
            }
            Opcode::BPL => {
                cpu.push_microcode(Microcode::BranchRelative(BranchTest::IfNegativeClear));
            }
            Opcode::BVC => {
                cpu.push_microcode(Microcode::BranchRelative(BranchTest::IfOverflowClear));
            }
            Opcode::BVS => {
                cpu.push_microcode(Microcode::BranchRelative(BranchTest::IfOverflowSet));
            }

            Opcode::SBC_IMMEDIATE => {
                cpu.push_microcode(Microcode::SbcImmediate);
            }
            Opcode::SBC_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: true,
                });
                cpu.push_microcode(Microcode::Sbc);
            }
            Opcode::SBC_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, true);
                cpu.push_microcode(Microcode::Sbc);
            }
            Opcode::SBC_ABSOLUTE => {
                absolute_addressing(cpu, true);
                cpu.push_microcode(Microcode::Sbc);
            }
            Opcode::SBC_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::Sbc);
            }
            Opcode::SBC_ABSOLUTE_INDEXED_Y => {
                absolute_indexed_y_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::Sbc);
            }
            Opcode::SBC_INDEXED_INDIRECT => {
                indexed_indirect_addressing(cpu, true);
                cpu.push_microcode(Microcode::Sbc);
            }
            Opcode::SBC_INDIRECT_INDEXED => {
                indirect_indexed_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::Sbc);
            }

            Opcode::ORA_IMMEDIATE => {
                cpu.push_microcode(Microcode::OraImmediate);
            }
            Opcode::ORA_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: true,
                });
                cpu.push_microcode(Microcode::Ora);
            }
            Opcode::ORA_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, true);
                cpu.push_microcode(Microcode::Ora);
            }
            Opcode::ORA_ABSOLUTE => {
                absolute_addressing(cpu, true);
                cpu.push_microcode(Microcode::Ora);
            }
            Opcode::ORA_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::Ora);
            }
            Opcode::ORA_ABSOLUTE_INDEXED_Y => {
                absolute_indexed_y_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::Ora);
            }
            Opcode::ORA_INDEXED_INDIRECT => {
                indexed_indirect_addressing(cpu, true);
                cpu.push_microcode(Microcode::Ora);
            }
            Opcode::ORA_INDIRECT_INDEXED => {
                indirect_indexed_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::Ora);
            }

            Opcode::EOR_IMMEDIATE => {
                cpu.push_microcode(Microcode::EorImmediate);
            }
            Opcode::EOR_ZERO_PAGE => {
                cpu.push_microcode(Microcode::ZeroPage {
                    load_into_alu: true,
                });
                cpu.push_microcode(Microcode::Eor);
            }
            Opcode::EOR_ZERO_PAGE_X => {
                zero_page_indexed_x_addressing(cpu, true);
                cpu.push_microcode(Microcode::Eor);
            }
            Opcode::EOR_ABSOLUTE => {
                absolute_addressing(cpu, true);
                cpu.push_microcode(Microcode::Eor);
            }
            Opcode::EOR_ABSOLUTE_INDEXED_X => {
                absolute_indexed_x_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::Eor);
            }
            Opcode::EOR_ABSOLUTE_INDEXED_Y => {
                absolute_indexed_y_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::Eor);
            }
            Opcode::EOR_INDEXED_INDIRECT => {
                indexed_indirect_addressing(cpu, true);
                cpu.push_microcode(Microcode::Eor);
            }
            Opcode::EOR_INDIRECT_INDEXED => {
                indirect_indexed_addressing(cpu, true, true);
                cpu.push_microcode(Microcode::Eor);
            }
            _ => panic!("Unknown opcode: {}", opcode),
        }
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

    fn zero_page<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
        let addr = cpu.inc_read_byte();
        cpu.ab = addr as u16;
        if load_into_alu {
            cpu.load_alu();
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
}

// TODO: if load_into_alu always is true in addressing functions, remove this argument

/// Push Microcodes for zero page indexed x addressing
fn zero_page_indexed_x_addressing<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
    cpu.push_microcode(Microcode::ZeroPage {
        load_into_alu: false,
    });
    cpu.push_microcode(Microcode::ZeroPageIndexedX { load_into_alu });
}

/// Push Microcodes for zero page indexed y addressing
fn zero_page_indexed_y_addressing<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
    cpu.push_microcode(Microcode::ZeroPage {
        load_into_alu: false,
    });
    cpu.push_microcode(Microcode::ZeroPageIndexedY { load_into_alu });
}

/// Push Microcodes for absolute addressing
fn absolute_addressing<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
    cpu.push_microcode(Microcode::AbsoluteL);
    cpu.push_microcode(Microcode::AbsoluteH { load_into_alu });
}

/// Push Microcodes for absolute indexed x addressing
fn absolute_indexed_x_addressing<M: Mcu>(cpu: &mut Cpu2<M>, oops: bool, load_into_alu: bool) {
    cpu.push_microcode(Microcode::AbsoluteL);
    cpu.push_microcode(Microcode::AbsoluteIndexedX {
        oops,
        load_into_alu,
    }); // may add nop for extra cycle
}

/// Push Microcodes for absolute indexed y addressing
fn absolute_indexed_y_addressing<M: Mcu>(cpu: &mut Cpu2<M>, oops: bool, load_into_alu: bool) {
    cpu.push_microcode(Microcode::AbsoluteL);
    cpu.push_microcode(Microcode::AbsoluteIndexedY {
        oops,
        load_into_alu,
    }); // may add nop for extra cycle
}

/// Push Microcodes for zero page indexed indirect addressing
fn indexed_indirect_addressing<M: Mcu>(cpu: &mut Cpu2<M>, load_into_alu: bool) {
    cpu.push_microcode(Microcode::ZeroPage {
        load_into_alu: false,
    });
    cpu.push_microcode(Microcode::ZeroPageIndexedX {
        load_into_alu: false,
    });
    cpu.push_microcode(Microcode::Indexed { load_into_alu });
    cpu.push_microcode(Microcode::Nop);
}

/// Push Microcodes for indirect indexed addressing
fn indirect_indexed_addressing<M: Mcu>(cpu: &mut Cpu2<M>, oops: bool, load_into_alu: bool) {
    cpu.push_microcode(Microcode::ZeroPage {
        load_into_alu: false,
    });
    cpu.push_microcode(Microcode::Indexed {
        load_into_alu: false,
    });
    cpu.push_microcode(Microcode::AbsoluteIndexedYWithoutHigh {
        oops,
        load_into_alu,
    }); // may add nop for extra cycle
}

#[cfg(test)]
mod tests;
