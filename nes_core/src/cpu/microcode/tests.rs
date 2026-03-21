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
            load_into_alu: true
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
