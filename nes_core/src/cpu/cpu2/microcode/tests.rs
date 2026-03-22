use super::*;
use crate::cpu::Flag;

struct TestMcu {
    mem: [u8; 0x10000],
    ticks: usize,
    writes: Vec<(u16, u8)>,
}

impl Default for TestMcu {
    fn default() -> Self {
        Self {
            mem: [0; 0x10000],
            ticks: 0,
            writes: Vec::new(),
        }
    }
}

impl Mcu for TestMcu {
    fn read(&mut self, address: u16) -> u8 {
        self.mem[address as usize]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.mem[address as usize] = value;
        self.writes.push((address, value));
    }

    fn tick(&mut self) {
        self.ticks += 1;
    }
}

fn cpu_with_memory(program_start: u16, bytes: &[(u16, u8)]) -> Cpu2<TestMcu> {
    let mut mcu = TestMcu::default();
    for (addr, value) in bytes {
        mcu.mem[*addr as usize] = *value;
    }
    let mut cpu = Cpu2::new(mcu);
    cpu.pc = program_start;
    cpu
}

#[test]
fn fetch_and_decode_queues_adc_immediate() {
    let mut cpu = cpu_with_memory(0x8000, &[(0x8000, Opcode::ADC_IMMEDIATE)]);

    Microcode::FetchAndDecode.exec(&mut cpu);

    assert_eq!(cpu.opcode, Opcode::ADC_IMMEDIATE);
    assert_eq!(cpu.pc, 0x8001);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AdcImmediate));
    assert!(cpu.pop_microcode().is_none());
}

#[test]
fn fetch_and_decode_queues_and_immediate() {
    let mut cpu = cpu_with_memory(0x8000, &[(0x8000, Opcode::AND_IMMEDIATE)]);

    Microcode::FetchAndDecode.exec(&mut cpu);

    assert_eq!(cpu.opcode, Opcode::AND_IMMEDIATE);
    assert_eq!(cpu.pc, 0x8001);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AndImmediate));
    assert!(cpu.pop_microcode().is_none());
}

#[test]
fn fetch_and_decode_queues_bit_sequences() {
    let mut cpu = cpu_with_memory(
        0x8000,
        &[
            (0x8000, Opcode::BIT_ZERO_PAGE),
            (0x8001, Opcode::BIT_ABSOLUTE),
        ],
    );

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::BIT_ZERO_PAGE);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: true,
            save_alu: false,
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Bit));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::BIT_ABSOLUTE);
    assert_eq!(cpu.pc, 0x8002);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AbsoluteL));
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::AbsoluteH {
            load_into_alu: true
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Bit));
}

#[test]
fn fetch_and_decode_queues_branch_sequences() {
    let mut cpu = cpu_with_memory(
        0x8000,
        &[
            (0x8000, Opcode::BMI),
            (0x8001, Opcode::BNE),
            (0x8002, Opcode::BPL),
        ],
    );

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::BMI);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::BranchRelative(BranchTest::IfNegativeSet))
    );

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::BNE);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::BranchRelative(BranchTest::IfZeroClear))
    );

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::BPL);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::BranchRelative(BranchTest::IfNegativeClear))
    );
}

#[test]
fn fetch_and_decode_queues_overflow_branch_sequences() {
    let mut cpu = cpu_with_memory(0x8000, &[(0x8000, Opcode::BVC), (0x8001, Opcode::BVS)]);

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::BVC);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::BranchRelative(BranchTest::IfOverflowClear))
    );

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::BVS);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::BranchRelative(BranchTest::IfOverflowSet))
    );
}

#[test]
fn branch_relative_taken_and_not_taken_behaves() {
    let mut cpu = cpu_with_memory(0x8000, &[(0x8000, 0x02), (0x8001, 0xFE)]);

    Microcode::BranchRelative(BranchTest::IfZeroClear).exec(&mut cpu);
    assert_eq!(cpu.pc, 0x8003);

    cpu.set_flag(Flag::Zero, true);
    Microcode::BranchRelative(BranchTest::IfZeroClear).exec(&mut cpu);
    assert_eq!(cpu.pc, 0x8004);
}

#[test]
fn branch_relative_cross_page_retains_extra_cycles() {
    let mut cpu = cpu_with_memory(0x80FE, &[(0x80FE, 0x02)]);

    Microcode::BranchRelative(BranchTest::IfCarryClear).exec(&mut cpu);

    assert_eq!(cpu.pc, 0x8101);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Nop));
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Nop));
    assert!(cpu.pop_microcode().is_none());
}

#[test]
fn fetch_and_decode_queues_sbc_sequences() {
    let mut cpu = cpu_with_memory(
        0x8000,
        &[
            (0x8000, Opcode::SBC_IMMEDIATE),
            (0x8001, Opcode::SBC_ZERO_PAGE),
            (0x8002, Opcode::SBC_ABSOLUTE),
        ],
    );

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::SBC_IMMEDIATE);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::SbcImmediate));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::SBC_ZERO_PAGE);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: true,
            save_alu: false
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Sbc));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::SBC_ABSOLUTE);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AbsoluteL));
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::AbsoluteH {
            load_into_alu: true
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Sbc));
}

#[test]
fn fetch_and_decode_queues_load_and_store_sequences() {
    let mut cpu = cpu_with_memory(
        0x8000,
        &[
            (0x8000, Opcode::LDA_IMMEDIATE),
            (0x8001, Opcode::LDX_ZERO_PAGE),
            (0x8002, Opcode::LDY_ABSOLUTE),
            (0x8003, Opcode::STA_ZERO_PAGE),
            (0x8004, Opcode::STX_ABSOLUTE),
            (0x8005, Opcode::STY_ZERO_PAGE_X),
        ],
    );

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::LDA_IMMEDIATE);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::LoadImmediateA));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::LDX_ZERO_PAGE);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: false,
            save_alu: false,
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::LoadX));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::LDY_ABSOLUTE);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AbsoluteL));
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::AbsoluteH {
            load_into_alu: false
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::LoadY));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::STA_ZERO_PAGE);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: false,
            save_alu: false,
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::StoreA));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::STX_ABSOLUTE);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AbsoluteL));
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::AbsoluteH {
            load_into_alu: false
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::StoreX));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::STY_ZERO_PAGE_X);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: false,
            save_alu: false,
        })
    );
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPageIndexedX {
            load_into_alu: false
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::StoreY));
}

#[test]
fn fetch_and_decode_queues_transfer_sequences() {
    let mut cpu = cpu_with_memory(
        0x8000,
        &[
            (0x8000, Opcode::TAX),
            (0x8001, Opcode::TXA),
            (0x8002, Opcode::TAY),
            (0x8003, Opcode::TYA),
            (0x8004, Opcode::TSX),
            (0x8005, Opcode::TXS),
        ],
    );

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::TAX);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Tax));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::TXA);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Txa));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::TAY);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Tay));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::TYA);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Tya));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::TSX);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Tsx));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::TXS);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Txs));
}

#[test]
fn fetch_and_decode_queues_stack_subroutine_jump_flag_misc_sequences() {
    let mut cpu = cpu_with_memory(
        0x8000,
        &[
            (0x8000, Opcode::PHA),
            (0x8001, Opcode::PLA),
            (0x8002, Opcode::PHP),
            (0x8003, Opcode::PLP),
            (0x8004, Opcode::JMP_ABSOLUTE),
            (0x8005, Opcode::JMP_INDIRECT),
            (0x8006, Opcode::JSR),
            (0x8007, Opcode::RTS),
            (0x8008, Opcode::RTI),
            (0x8009, Opcode::CLC),
            (0x800A, Opcode::SEC),
            (0x800B, Opcode::CLD),
            (0x800C, Opcode::SED),
            (0x800D, Opcode::CLI),
            (0x800E, Opcode::SEI),
            (0x800F, Opcode::CLV),
            (0x8010, Opcode::NOP),
            (0x8011, Opcode::BRK),
        ],
    );

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::PHA);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Pha));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::PLA);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Pla));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::PHP);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Php));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::PLP);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Plp));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::JMP_ABSOLUTE);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AbsoluteL));
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::AbsoluteH {
            load_into_alu: false
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::JmpAbsolute));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::JMP_INDIRECT);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AbsoluteL));
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::AbsoluteH {
            load_into_alu: false
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::JmpIndirect));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::JSR);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AbsoluteL));
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::AbsoluteH {
            load_into_alu: false
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Jsr));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::RTS);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Rts));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::RTI);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Rti));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::CLC);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Clc));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::SEC);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Sec));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::CLD);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Cld));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::SED);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Sed));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::CLI);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Cli));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::SEI);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Sei));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::CLV);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Clv));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::NOP);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Nop));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::BRK);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Brk));
}

#[test]
fn fetch_and_decode_queues_shift_and_rotate_sequences() {
    let mut cpu = cpu_with_memory(
        0x8000,
        &[
            (0x8000, Opcode::LSR_ACCUMULATOR),
            (0x8001, Opcode::ROL_ZERO_PAGE),
            (0x8002, Opcode::ROR_ABSOLUTE_INDEXED_X),
        ],
    );

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::LSR_ACCUMULATOR);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::LsrAccumulator));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::ROL_ZERO_PAGE);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: true,
            save_alu: false
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::StoreAlu));
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Rol));
    assert_eq!(cpu.pop_microcode(), Some(Microcode::StoreAlu));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::ROR_ABSOLUTE_INDEXED_X);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AbsoluteL));
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: true
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::StoreAlu));
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Ror));
    assert_eq!(cpu.pop_microcode(), Some(Microcode::StoreAlu));
}

#[test]
fn fetch_and_decode_queues_ora_and_eor_sequences() {
    let mut cpu = cpu_with_memory(
        0x8000,
        &[
            (0x8000, Opcode::ORA_IMMEDIATE),
            (0x8001, Opcode::EOR_ZERO_PAGE),
            (0x8002, Opcode::EOR_ABSOLUTE_INDEXED_X),
        ],
    );

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::ORA_IMMEDIATE);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::OraImmediate));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::EOR_ZERO_PAGE);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: true,
            save_alu: false
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Eor));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::EOR_ABSOLUTE_INDEXED_X);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AbsoluteL));
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::AbsoluteIndexedX {
            oops: true,
            load_into_alu: true
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Eor));
}

#[test]
fn fetch_and_decode_queues_compare_and_bit_sequences() {
    let mut cpu = cpu_with_memory(
        0x8000,
        &[
            (0x8000, Opcode::CMP_IMMEDIATE),
            (0x8001, Opcode::CPX_ZERO_PAGE),
            (0x8002, Opcode::CPY_ABSOLUTE),
            (0x8003, Opcode::BIT_ZERO_PAGE),
        ],
    );

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::CMP_IMMEDIATE);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::CmpImmediate));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::CPX_ZERO_PAGE);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: true,
            save_alu: false
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Cpx));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::CPY_ABSOLUTE);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AbsoluteL));
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::AbsoluteH {
            load_into_alu: true
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Cpy));

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.opcode, Opcode::BIT_ZERO_PAGE);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: true,
            save_alu: false
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Bit));
}

#[test]
fn load_microcodes_read_memory_and_update_flags() {
    let mut cpu = cpu_with_memory(0x0000, &[(0x0042, 0x80), (0x0043, 0x00), (0x0044, 0x7F)]);

    cpu.ab = 0x0042;
    Microcode::LoadA.exec(&mut cpu);
    assert_eq!(cpu.a, 0x80);
    assert!(cpu.flag(Flag::Negative));
    assert!(!cpu.flag(Flag::Zero));

    cpu.ab = 0x0043;
    Microcode::LoadX.exec(&mut cpu);
    assert_eq!(cpu.x, 0x00);
    assert!(cpu.flag(Flag::Zero));
    assert!(!cpu.flag(Flag::Negative));

    cpu.ab = 0x0044;
    Microcode::LoadY.exec(&mut cpu);
    assert_eq!(cpu.y, 0x7F);
    assert!(!cpu.flag(Flag::Zero));
    assert!(!cpu.flag(Flag::Negative));
}

#[test]
fn load_immediate_x_and_y_read_operand_and_update_flags() {
    let mut cpu = cpu_with_memory(0x8000, &[(0x8000, 0x80), (0x8001, 0x00)]);

    Microcode::LoadImmediateX.exec(&mut cpu);
    assert_eq!(cpu.x, 0x80);
    assert_eq!(cpu.pc, 0x8001);
    assert!(cpu.flag(Flag::Negative));
    assert!(!cpu.flag(Flag::Zero));

    Microcode::LoadImmediateY.exec(&mut cpu);
    assert_eq!(cpu.y, 0x00);
    assert_eq!(cpu.pc, 0x8002);
    assert!(cpu.flag(Flag::Zero));
    assert!(!cpu.flag(Flag::Negative));
}

#[test]
fn store_microcodes_write_registers_to_memory() {
    let mut cpu = cpu_with_memory(0x0000, &[]);
    cpu.ab = 0x1234;
    cpu.a = 0x11;
    cpu.x = 0x22;
    cpu.y = 0x33;

    Microcode::StoreA.exec(&mut cpu);
    cpu.ab = 0x1235;
    Microcode::StoreX.exec(&mut cpu);
    cpu.ab = 0x1236;
    Microcode::StoreY.exec(&mut cpu);

    assert_eq!(cpu.mcu().mem[0x1234], 0x11);
    assert_eq!(cpu.mcu().mem[0x1235], 0x22);
    assert_eq!(cpu.mcu().mem[0x1236], 0x33);
    assert_eq!(
        cpu.mcu().writes,
        vec![(0x1234, 0x11), (0x1235, 0x22), (0x1236, 0x33)]
    );
}

#[test]
fn store_and_load_microcodes_use_alu_and_memory() {
    let mut cpu = cpu_with_memory(0x0000, &[(0x0042, 0x44), (0x0055, 0x55)]);

    cpu.ab = 0x0042;
    cpu.load_alu();
    assert_eq!(cpu.alu, 0x44);

    Microcode::LoadA.exec(&mut cpu);
    assert_eq!(cpu.a, 0x44);

    cpu.ab = 0x0055;
    cpu.a = 0xAA;
    Microcode::StoreA.exec(&mut cpu);
    assert_eq!(cpu.mcu().mem[0x0055], 0xAA);
}

#[test]
fn shift_and_rotate_microcodes_update_accumulator_and_alu() {
    let mut cpu = cpu_with_memory(0x0000, &[]);

    cpu.a = 0b1000_0001;
    Microcode::LsrAccumulator.exec(&mut cpu);
    assert_eq!(cpu.a, 0b0100_0000);
    assert!(cpu.flag(Flag::Carry));
    assert!(!cpu.flag(Flag::Negative));

    cpu.a = 0b0100_0001;
    cpu.set_flag(Flag::Carry, true);
    Microcode::RolAccumulator.exec(&mut cpu);
    assert_eq!(cpu.a, 0b1000_0011);
    assert!(!cpu.flag(Flag::Carry));
    assert!(cpu.flag(Flag::Negative));

    cpu.alu = 0b0000_0001;
    cpu.set_flag(Flag::Carry, true);
    Microcode::Ror.exec(&mut cpu);
    assert_eq!(cpu.alu, 0b1000_0000);
    assert!(cpu.flag(Flag::Carry));
    assert!(cpu.flag(Flag::Negative));
}

#[test]
fn ora_and_eor_microcodes_update_accumulator_and_flags() {
    let mut cpu = cpu_with_memory(0x8000, &[(0x8000, 0b0000_1111), (0x0042, 0b1111_0000)]);
    cpu.a = 0b0101_0000;

    Microcode::OraImmediate.exec(&mut cpu);
    assert_eq!(cpu.a, 0b0101_1111);
    assert!(!cpu.flag(Flag::Zero));
    assert!(!cpu.flag(Flag::Negative));

    cpu.ab = 0x0042;
    cpu.alu = 0b1111_0000;
    Microcode::Eor.exec(&mut cpu);
    assert_eq!(cpu.a, 0b1010_1111);
    assert!(!cpu.flag(Flag::Zero));
    assert!(cpu.flag(Flag::Negative));
}

#[test]
fn compare_and_bit_microcodes_update_flags() {
    let mut cpu = cpu_with_memory(0x8000, &[(0x8000, 0x10), (0x0001, 0x40)]);
    cpu.a = 0x20;
    cpu.x = 0x11;
    cpu.y = 0x10;

    Microcode::CmpImmediate.exec(&mut cpu);
    assert!(cpu.flag(Flag::Carry));
    assert!(!cpu.flag(Flag::Zero));
    assert!(!cpu.flag(Flag::Negative));

    cpu.a = 0x41;
    cpu.ab = 0x0001;
    cpu.load_alu();
    Microcode::Bit.exec(&mut cpu);
    assert!(cpu.flag(Flag::Overflow));
    assert!(!cpu.flag(Flag::Negative));
    assert!(!cpu.flag(Flag::Zero));

    cpu.alu = 0x11;
    Microcode::Cpx.exec(&mut cpu);
    assert!(cpu.flag(Flag::Carry));
    assert!(cpu.flag(Flag::Zero));

    cpu.alu = 0x0F;
    Microcode::Cpy.exec(&mut cpu);
    assert!(cpu.flag(Flag::Carry));
    assert!(!cpu.flag(Flag::Zero));
}

#[test]
fn transfer_microcodes_copy_registers_and_update_flags() {
    let mut cpu = cpu_with_memory(0x0000, &[]);
    cpu.a = 0x80;
    cpu.x = 0x00;
    cpu.y = 0x7F;
    cpu.sp = 0x42;

    Microcode::Tax.exec(&mut cpu);
    assert_eq!(cpu.x, 0x80);
    assert!(cpu.flag(Flag::Negative));

    Microcode::Txa.exec(&mut cpu);
    assert_eq!(cpu.a, 0x80);

    Microcode::Tay.exec(&mut cpu);
    assert_eq!(cpu.y, 0x80);

    Microcode::Tya.exec(&mut cpu);
    assert_eq!(cpu.a, 0x80);

    Microcode::Tsx.exec(&mut cpu);
    assert_eq!(cpu.x, 0x42);
    assert!(!cpu.flag(Flag::Zero));

    cpu.x = 0x55;
    Microcode::Txs.exec(&mut cpu);
    assert_eq!(cpu.sp, 0x55);
}

#[test]
fn stack_and_misc_microcodes_manipulate_state() {
    let mut cpu = cpu_with_memory(0x8000, &[(0xFFFE, 0x34), (0xFFFF, 0x12)]);
    cpu.a = 0xAB;
    cpu.sp = 0xFF;
    cpu.status = Flag::Carry as u8;

    Microcode::Pha.exec(&mut cpu);
    assert_eq!(cpu.sp, 0xFE);
    assert_eq!(cpu.mcu().mem[0x01FF], 0xAB);

    cpu.a = 0x00;
    Microcode::Pla.exec(&mut cpu);
    assert_eq!(cpu.a, 0xAB);

    Microcode::Php.exec(&mut cpu);
    assert_eq!(
        cpu.mcu().mem[0x01FF] & (Flag::Break as u8),
        Flag::Break as u8
    );

    cpu.status = 0;
    cpu.sp = 0xFD;
    cpu.mcu_mut().mem[0x01FE] = Flag::Carry as u8;
    Microcode::Plp.exec(&mut cpu);
    assert_eq!(cpu.status & Flag::Carry as u8, Flag::Carry as u8);

    cpu.ab = 0x1234;
    Microcode::JmpAbsolute.exec(&mut cpu);
    assert_eq!(cpu.pc, 0x1234);

    cpu.ab = 0x1234;
    cpu.mcu_mut().mem[0x1234] = 0x34;
    cpu.mcu_mut().mem[0x1235] = 0x12;
    Microcode::JmpIndirect.exec(&mut cpu);
    assert_eq!(cpu.pc, 0x1234);

    cpu.ab = 0x1234;
    cpu.pc = 0x8005;
    cpu.sp = 0xFF;
    Microcode::Jsr.exec(&mut cpu);
    assert_eq!(cpu.pc, 0x1234);

    cpu.sp = 0xFD;
    cpu.mcu_mut().mem[0x01FE] = 0x00;
    cpu.mcu_mut().mem[0x01FF] = 0x80;
    Microcode::Rts.exec(&mut cpu);
    assert_eq!(cpu.pc, 0x8001);

    cpu.sp = 0xFC;
    cpu.mcu_mut().mem[0x01FD] = Flag::Carry as u8;
    cpu.mcu_mut().mem[0x01FE] = 0x78;
    cpu.mcu_mut().mem[0x01FF] = 0x56;
    Microcode::Rti.exec(&mut cpu);
    assert_eq!(cpu.status & Flag::Carry as u8, Flag::Carry as u8);
    assert_eq!(cpu.pc, 0x5678);

    cpu.status = Flag::Carry as u8;
    Microcode::Clc.exec(&mut cpu);
    assert!(!cpu.flag(Flag::Carry));
    Microcode::Sec.exec(&mut cpu);
    assert!(cpu.flag(Flag::Carry));
    cpu.status = Flag::Decimal as u8;
    Microcode::Cld.exec(&mut cpu);
    assert!(!cpu.flag(Flag::Decimal));
    Microcode::Sed.exec(&mut cpu);
    assert!(cpu.flag(Flag::Decimal));
    Microcode::Cli.exec(&mut cpu);
    assert!(!cpu.flag(Flag::InterruptDisabled));
    Microcode::Sei.exec(&mut cpu);
    assert!(cpu.flag(Flag::InterruptDisabled));
    cpu.status = Flag::Overflow as u8;
    Microcode::Clv.exec(&mut cpu);
    assert!(!cpu.flag(Flag::Overflow));
}

#[test]
fn undocumented_microcodes_update_state() {
    let mut cpu = cpu_with_memory(
        0x8000,
        &[
            (0x8000, 0x80),
            (0x0042, 0x10),
            (0x0043, 0x01),
            (0x1234, 0x55),
        ],
    );

    cpu.a = 0xF0;
    cpu.x = 0x0F;
    cpu.y = 0x33;
    cpu.ab = 0x0042;
    Microcode::AlrImmediate.exec(&mut cpu);
    assert_eq!(cpu.a, 0x40);

    cpu.alu = 0x80;
    Microcode::AncImmediate.exec(&mut cpu);
    assert_eq!(cpu.a, 0x00);

    cpu.a = 0x7F;
    cpu.alu = 0xFF;
    cpu.pc = 0x8002;
    cpu.mcu_mut().mem[0x8002] = 0xFF;
    Microcode::ArrImmediate.exec(&mut cpu);
    assert_eq!(cpu.a, 0x3F);

    cpu.a = 0x1F;
    cpu.x = 0x0F;
    cpu.pc = 0x8003;
    cpu.mcu_mut().mem[0x8003] = 0x01;
    cpu.alu = 0x01;
    Microcode::AxsImmediate.exec(&mut cpu);
    assert_eq!(cpu.x, 0x0E);

    cpu.alu = 0x01;
    cpu.ab = 0x1234;
    Microcode::Lax.exec(&mut cpu);
    assert_eq!(cpu.a, 0x01);
    assert_eq!(cpu.x, 0x01);

    cpu.a = 0xAA;
    cpu.x = 0x0F;
    Microcode::Sax.exec(&mut cpu);
    assert_eq!(cpu.mcu().writes.last(), Some(&(0x1234, 0x0A)));

    cpu.alu = 0x10;
    cpu.ab = 0x0042;
    Microcode::Dcp.exec(&mut cpu);
    assert_eq!(cpu.mcu().writes.last(), Some(&(0x0042, 0x0F)));

    cpu.alu = 0x01;
    cpu.ab = 0x0043;
    Microcode::Isc.exec(&mut cpu);
    assert_eq!(cpu.mcu().writes.last(), Some(&(0x0043, 0x02)));

    cpu.a = 0x01;
    cpu.alu = 0x02;
    cpu.set_flag(Flag::Carry, false);
    cpu.ab = 0x1234;
    Microcode::Rra.exec(&mut cpu);
    assert!(cpu.a != 0x01);

    cpu.a = 0x01;
    cpu.alu = 0x81;
    cpu.ab = 0x1234;
    Microcode::Slo.exec(&mut cpu);
    assert!(cpu.a != 0x01);

    cpu.a = 0xFF;
    cpu.alu = 0x03;
    cpu.ab = 0x1234;
    Microcode::Sre.exec(&mut cpu);
    assert!(cpu.a != 0xFF);

    cpu.x = 0xF0;
    cpu.alu = 0x12;
    cpu.ab = 0x1234;
    Microcode::Shx.exec(&mut cpu);
    Microcode::Shy.exec(&mut cpu);
    Microcode::Tas.exec(&mut cpu);
}

#[test]
fn undocumented_decode_sequences_exist() {
    let mut cpu = cpu_with_memory(
        0x8000,
        &[
            (0x8000, Opcode::ALR),
            (0x8001, Opcode::ANC),
            (0x8002, Opcode::ARR),
            (0x8003, Opcode::AXS),
            (0x8004, Opcode::LAX_ZERO_PAGE),
            (0x8005, Opcode::SAX_ZERO_PAGE),
            (0x8006, Opcode::DCP_ZERO_PAGE),
            (0x8007, Opcode::ISC_ZERO_PAGE),
            (0x8008, Opcode::RRA_ZERO_PAGE),
            (0x8009, Opcode::SLO_ZERO_PAGE),
            (0x800A, Opcode::SRE_ZERO_PAGE),
            (0x800B, Opcode::SHX_ABSOLUTE_INDEXED_Y),
            (0x800C, Opcode::SHY_ABSOLUTE_INDEXED_X),
            (0x800D, Opcode::TAS_ABSOLUTE_INDEXED_Y),
            (0x800E, Opcode::KIL1),
            (0x800F, Opcode::USBC),
        ],
    );

    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AlrImmediate));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AncImmediate));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::ArrImmediate));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AxsImmediate));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: true,
            save_alu: false,
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Lax));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Sax));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: true,
            save_alu: false,
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Dcp));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: true,
            save_alu: false,
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Isc));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: true,
            save_alu: false,
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::StoreAlu));
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Rra));
    assert_eq!(cpu.pop_microcode(), Some(Microcode::StoreAlu));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: true,
            save_alu: false,
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::StoreAlu));
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Asl));
    assert_eq!(cpu.pop_microcode(), Some(Microcode::StoreAlu));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::ZeroPage {
            load_into_alu: true,
            save_alu: false,
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::StoreAlu));
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Lsr));
    assert_eq!(cpu.pop_microcode(), Some(Microcode::StoreAlu));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AbsoluteL));
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::AbsoluteIndexedY {
            oops: false,
            load_into_alu: true
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Shx));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AbsoluteL));
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::AbsoluteIndexedX {
            oops: false,
            load_into_alu: true
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Shy));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::AbsoluteL));
    assert_eq!(
        cpu.pop_microcode(),
        Some(Microcode::AbsoluteIndexedY {
            oops: false,
            load_into_alu: true
        })
    );
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Tas));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::Kill));
    Microcode::FetchAndDecode.exec(&mut cpu);
    assert_eq!(cpu.pop_microcode(), Some(Microcode::SbcImmediate));
}

#[test]
fn load_immediate_a_reads_operand_and_updates_flags() {
    let mut cpu = cpu_with_memory(0x8000, &[(0x8000, 0x00)]);
    cpu.status = Flag::Negative as u8;

    Microcode::LoadImmediateA.exec(&mut cpu);

    assert_eq!(cpu.a, 0x00);
    assert_eq!(cpu.pc, 0x8001);
    assert!(cpu.flag(Flag::Zero));
    assert!(!cpu.flag(Flag::Negative));
}

#[test]
fn adc_immediate_uses_carry_and_updates_flags() {
    let mut cpu = cpu_with_memory(0x8000, &[(0x8000, 0x30)]);
    cpu.a = 0x50;

    Microcode::AdcImmediate.exec(&mut cpu);

    assert_eq!(cpu.a, 0x80);
    assert_eq!(cpu.pc, 0x8001);
    assert!(cpu.flag(Flag::Negative));
    assert!(cpu.flag(Flag::Overflow));
    assert!(!cpu.flag(Flag::Carry));
    assert!(!cpu.flag(Flag::Zero));
}

#[test]
fn sbc_immediate_and_memory_microcodes_update_accumulator_and_flags() {
    let mut cpu = cpu_with_memory(0x8000, &[(0x8000, 0x10), (0x0042, 0x01)]);
    cpu.a = 0x20;
    cpu.set_flag(Flag::Carry, true);

    Microcode::SbcImmediate.exec(&mut cpu);
    assert_eq!(cpu.a, 0x10);
    assert_eq!(cpu.pc, 0x8001);
    assert!(cpu.flag(Flag::Carry));
    assert!(!cpu.flag(Flag::Zero));

    cpu.ab = 0x0042;
    cpu.alu = 0x01;
    Microcode::Sbc.exec(&mut cpu);
    assert_eq!(cpu.a, 0x0F);
    assert!(cpu.flag(Flag::Carry));
    assert!(!cpu.flag(Flag::Zero));
    assert!(!cpu.flag(Flag::Negative));
}

#[test]
fn adc_reads_from_address_bus_and_sets_zero_and_carry() {
    let mut cpu = cpu_with_memory(0x0000, &[(0x0042, 0xFF)]);
    cpu.ab = 0x0042;
    cpu.a = 0x00;
    cpu.alu = 0xFF;
    cpu.set_flag(Flag::Carry, true);

    Microcode::Adc.exec(&mut cpu);

    assert_eq!(cpu.a, 0x00);
    assert!(cpu.flag(Flag::Carry));
    assert!(cpu.flag(Flag::Zero));
    assert!(!cpu.flag(Flag::Negative));
    assert!(!cpu.flag(Flag::Overflow));
}

#[test]
fn and_immediate_and_memory_microcodes_update_accumulator_and_flags() {
    let mut cpu = cpu_with_memory(0x8000, &[(0x8000, 0b1100_1100), (0x0042, 0b1010_1010)]);
    cpu.a = 0b1111_0000;

    Microcode::AndImmediate.exec(&mut cpu);
    assert_eq!(cpu.a, 0b1100_0000);
    assert_eq!(cpu.pc, 0x8001);
    assert!(!cpu.flag(Flag::Zero));
    assert!(cpu.flag(Flag::Negative));

    cpu.ab = 0x0042;
    cpu.alu = 0b1010_1010;
    Microcode::And.exec(&mut cpu);
    assert_eq!(cpu.a, 0b1000_0000);
    assert!(!cpu.flag(Flag::Zero));
    assert!(cpu.flag(Flag::Negative));
}

#[test]
fn bit_updates_flags_from_alu_and_accumulator() {
    let mut cpu = cpu_with_memory(0x0000, &[]);
    cpu.a = 0b0011_0000;
    cpu.alu = 0b1100_0000;
    cpu.status = Flag::Zero as u8;

    Microcode::Bit.exec(&mut cpu);

    assert!(cpu.flag(Flag::Negative));
    assert!(cpu.flag(Flag::Overflow));
    assert!(cpu.flag(Flag::Zero));

    cpu.a = 0b1111_0000;
    cpu.alu = 0b0100_0000;
    Microcode::Bit.exec(&mut cpu);
    assert!(!cpu.flag(Flag::Negative));
    assert!(cpu.flag(Flag::Overflow));
    assert!(!cpu.flag(Flag::Zero));
}
