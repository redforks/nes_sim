use super::*;

#[test]
fn literal_format() {
    assert_eq!(format!("{}", Literal(0x12)), "#$12");
}

#[test]
fn zero_page_format() {
    assert_eq!(format!("{}", ZeroPage(0x12)), "$12");
}

#[test]
fn absolute_format() {
    assert_eq!(format!("{}", Absolute(0x1234)), "$1234");
}

#[test]
fn zero_page_x_format() {
    assert_eq!(format!("{}", ZeroPageX(0x12)), "$12,X");
}

#[test]
fn zero_page_y_format() {
    assert_eq!(format!("{}", ZeroPageY(0x12)), "$12,Y");
}

#[test]
fn absolute_x_format() {
    assert_eq!(format!("{}", AbsoluteX(0x1234)), "$1234,X");
}

#[test]
fn absolute_y_format() {
    assert_eq!(format!("{}", AbsoluteY(0x1234)), "$1234,Y");
}

#[test]
fn indirect_x_format() {
    assert_eq!(format!("{}", IndirectX(0x12)), "($12,X)");
}

#[test]
fn indirect_y_format() {
    assert_eq!(format!("{}", IndirectY(0x12)), "($12),Y");
}

#[test]
fn register_x_format() {
    assert_eq!(format!("{}", RegisterX()), "X");
}

#[test]
fn register_y_format() {
    assert_eq!(format!("{}", RegisterY()), "Y");
}

#[test]
fn register_sp_format() {
    assert_eq!(format!("{}", RegisterSP()), "SP");
}

#[test]
fn flag_format() {
    assert_eq!(format!("{}", FlagAddr(Flag::Carry)), "C");
}

// BranchAddressing Display tests
#[test]
fn branch_relative_format() {
    assert_eq!(format!("{}", BranchAddressing::Relative(0x12)), "$12");
    assert_eq!(format!("{}", BranchAddressing::Relative(0xff)), "$ff");
}

#[test]
fn branch_absolute_indirect_format() {
    assert_eq!(
        format!("{}", BranchAddressing::AbsoluteIndirect(0x1234)),
        "($1234)"
    );
    assert_eq!(
        format!("{}", BranchAddressing::AbsoluteIndirect(0xffff)),
        "($ffff)"
    );
}

#[test]
fn test_display() {
    assert_eq!(format!("{}", Addressing::Accumulator), "");
    assert_eq!(format!("{}", Addressing::Implied), "");
    assert_eq!(format!("{}", Addressing::Immediate(0x42)), "#$42");
    assert_eq!(format!("{}", Addressing::ZeroPage(0x10)), "$10");
    assert_eq!(format!("{}", Addressing::Absolute(0x1234)), "$1234");
    assert_eq!(
        format!("{}", Addressing::AbsoluteIndexedWithX(0x2000)),
        "$2000,X"
    );
    assert_eq!(
        format!("{}", Addressing::ZeroPageIndexedIndirect(0x20)),
        "($20,X)"
    );
    assert_eq!(
        format!("{}", Addressing::ZeroPageIndexedIndirectWithY(0x20)),
        "($20),Y"
    );
}

#[test]
fn test_read_cycles() {
    let mut cpu = Cpu::new(crate::test_utils::MockMcu::new());
    // Non-indexed/indirect modes return 0 cycles
    assert_eq!(Addressing::Immediate(0x7f).read(&mut cpu), (0x7f, 0));
    assert_eq!(Addressing::ZeroPage(0x01).read(&mut cpu), (0, 0));
    assert_eq!(Addressing::Absolute(0x1234).read(&mut cpu), (0, 0));
    assert_eq!(Addressing::Accumulator.read(&mut cpu), (0, 0));
    // Indexed/indirect modes return their cycle counts
    assert_eq!(
        Addressing::AbsoluteIndexedWithX(0x1000).read(&mut cpu),
        (0, 4)
    );
    assert_eq!(
        Addressing::ZeroPageIndexedIndirect(0x12).read(&mut cpu),
        (0, 6)
    );
}

#[test]
fn test_read_cycles_with_page_cross() {
    let mut cpu = Cpu::new(crate::test_utils::MockMcu::new());

    // AbsoluteIndexedWithX: 0x10ff + 1 = 0x1100 (crosses page boundary)
    cpu.x = 1;
    cpu.write_byte(0x1100, 0x42);
    let (val, cycles) = Addressing::AbsoluteIndexedWithX(0x10ff).read(&mut cpu);
    assert_eq!(val, 0x42, "should read byte from 0x1100");
    assert_eq!(cycles, 5); // 4 + 1 for page cross

    // AbsoluteIndexedWithY: 0x10ff + 1 = 0x1100 (crosses page boundary)
    cpu.y = 1;
    cpu.write_byte(0x1100, 0x55);
    let (val, cycles) = Addressing::AbsoluteIndexedWithY(0x10ff).read(&mut cpu);
    assert_eq!(val, 0x55, "should read byte from 0x1100");
    assert_eq!(cycles, 5); // 4 + 1 for page cross

    // ZeroPageIndexedIndirectWithY: set up indirect address at 0x10 pointing to 0x10ff
    cpu.write_byte(0x10, 0xff);
    cpu.write_byte(0x11, 0x10);
    cpu.write_byte(0x1100, 0x78);
    cpu.y = 1;
    let (val, cycles) = Addressing::ZeroPageIndexedIndirectWithY(0x10).read(&mut cpu);
    assert_eq!(
        val, 0x78,
        "should read byte from indirect address 0x10ff + Y = 0x1100"
    );
    assert_eq!(cycles, 6); // 5 + 1 for page cross
}

#[test]
fn test_write_cycles() {
    let mut cpu = Cpu::new(crate::test_utils::MockMcu::new());
    // Non-indexed/indirect modes return 0 cycles
    assert_eq!(Addressing::Accumulator.write(&mut cpu, 0), 0);
    assert_eq!(Addressing::ZeroPage(0x01).write(&mut cpu, 0), 0);
    assert_eq!(Addressing::Absolute(0x1000).write(&mut cpu, 0), 0);
    // Indexed/indirect modes return their cycle counts
    assert_eq!(Addressing::ZeroPageIndexedWithX(0x10).write(&mut cpu, 0), 4);
    assert_eq!(
        Addressing::AbsoluteIndexedWithX(0x1000).write(&mut cpu, 0),
        5
    );
}

#[test]
fn test_write_verify_memory() {
    let mut cpu = Cpu::new(crate::test_utils::MockMcu::new());

    // Test ZeroPage write
    Addressing::ZeroPage(0x20).write(&mut cpu, 0xAB);
    assert_eq!(
        cpu.read_byte(0x20),
        0xAB,
        "should write to zero page address"
    );

    // Test Absolute write
    Addressing::Absolute(0x1234).write(&mut cpu, 0xCD);
    assert_eq!(
        cpu.read_byte(0x1234),
        0xCD,
        "should write to absolute address"
    );

    // Test ZeroPageIndexedWithX write
    cpu.x = 5;
    Addressing::ZeroPageIndexedWithX(0x10).write(&mut cpu, 0x34);
    assert_eq!(cpu.read_byte(0x15), 0x34, "should write to 0x10 + X = 0x15");

    // Test AbsoluteIndexedWithX write
    cpu.x = 0x20;
    Addressing::AbsoluteIndexedWithX(0x1000).write(&mut cpu, 0x56);
    assert_eq!(
        cpu.read_byte(0x1020),
        0x56,
        "should write to 0x1000 + X = 0x1020"
    );

    // Test AbsoluteIndexedWithY write
    cpu.y = 0x30;
    Addressing::AbsoluteIndexedWithY(0x2000).write(&mut cpu, 0x78);
    assert_eq!(
        cpu.read_byte(0x2030),
        0x78,
        "should write to 0x2000 + Y = 0x2030"
    );

    // Test ZeroPageIndexedIndirect write (indirect indexed with X)
    cpu.x = 2;
    // Set up indirect pointer at 0x10 + X = 0x12 pointing to 0x3456
    cpu.write_byte(0x12, 0x56);
    cpu.write_byte(0x13, 0x34);
    Addressing::ZeroPageIndexedIndirect(0x10).write(&mut cpu, 0x9A);
    assert_eq!(
        cpu.read_byte(0x3456),
        0x9A,
        "should write to (0x10, X) = (0x12) = 0x3456"
    );

    // Test ZeroPageIndexedIndirectWithY write (indirect with Y post-index)
    cpu.y = 0x10;
    // Set up indirect pointer at 0x20 pointing to 0x4000
    cpu.write_byte(0x20, 0x00);
    cpu.write_byte(0x21, 0x40);
    Addressing::ZeroPageIndexedIndirectWithY(0x20).write(&mut cpu, 0xBC);
    assert_eq!(
        cpu.read_byte(0x4010),
        0xBC,
        "should write to (0x20) + Y = 0x4000 + 0x10 = 0x4010"
    );
}

#[test]
fn test_write_cycles_no_page_cross() {
    let mut cpu = Cpu::new(crate::test_utils::MockMcu::new());

    // AbsoluteIndexedWithX: 0x1000 + 1 = 0x1001 (no page cross)
    cpu.x = 1;
    cpu.write_byte(0x1001, 0xFF); // initial value
    let cycles = Addressing::AbsoluteIndexedWithX(0x1000).write(&mut cpu, 0x12);
    assert_eq!(cycles, 5);
    assert_eq!(cpu.read_byte(0x1001), 0x12, "should write byte to 0x1001");

    // AbsoluteIndexedWithY: 0x1000 + 1 = 0x1001 (no page cross)
    cpu.y = 1;
    let cycles = Addressing::AbsoluteIndexedWithY(0x1000).write(&mut cpu, 0x34);
    assert_eq!(cycles, 5);
    assert_eq!(cpu.read_byte(0x1001), 0x34, "should write byte to 0x1001");
}

#[test]
#[should_panic]
fn test_write_immediate_panics() {
    let mut cpu = Cpu::new(crate::test_utils::MockMcu::new());
    Addressing::Immediate(0x1).write(&mut cpu, 0);
}

// BranchAddressing calc_addr tests
#[test]
fn branch_relative_calc_addr() {
    let mcu = crate::test_utils::MockMcu::new();
    let mut cpu = Cpu::new(mcu);
    cpu.pc = 0x1000;

    // Positive offset
    let addr = BranchAddressing::Relative(0x10).calc_addr(&mut cpu);
    assert_eq!(addr, 0x1010);

    // Negative offset (0xfe is -2 in two's complement)
    cpu.pc = 0x1000;
    let addr = BranchAddressing::Relative(0xfe).calc_addr(&mut cpu);
    assert_eq!(addr, 0x0ffe);

    // Zero offset
    cpu.pc = 0x2000;
    let addr = BranchAddressing::Relative(0x00).calc_addr(&mut cpu);
    assert_eq!(addr, 0x2000);
}

#[test]
fn branch_absolute_indirect_calc_addr() {
    let mcu = crate::test_utils::MockMcu::new();
    // Set up the vector: at 0x1234-0x1235, store the target address 0xABCD
    mcu.write_word(0x1234, 0xABCD);

    let mut cpu = Cpu::new(mcu);

    let addr = BranchAddressing::AbsoluteIndirect(0x1234).calc_addr(&mut cpu);
    assert_eq!(addr, 0xABCD);
}
