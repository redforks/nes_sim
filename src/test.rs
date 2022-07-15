#![cfg(test)]

use super::*;

#[test]
fn zero_page() {
    let cpu = Cpu::new();
    let addr = Agu::ZeroPage(0x10);
    assert_eq!(addr.address(&cpu), (0x10, 1, 1));
}

#[test]
fn absolute() {
    let cpu = Cpu::new();
    let addr = Agu::Absolute(0x10);
    assert_eq!(addr.address(&cpu), (0x10, 2, 2));
}

#[test]
fn zero_page_x() {
    let mut cpu = Cpu::new();
    cpu.x = 10;
    assert_eq!((11, 2, 1), Agu::ZeroPageX(1).address(&cpu));

    // wrap if overflow
    assert_eq!((9, 2, 1), Agu::ZeroPageX(0xff).address(&cpu));
}

#[test]
fn zero_page_y() {
    let mut cpu = Cpu::new();
    cpu.y = 10;
    assert_eq!((11, 2, 1), Agu::ZeroPageY(1).address(&cpu));

    // wrap if overflow
    assert_eq!((9, 2, 1), Agu::ZeroPageY(0xff).address(&cpu));
}

#[test]
fn absolute_x() {
    let mut cpu = Cpu::new();
    cpu.x = 0x10;
    assert_eq!((0x1010, 2, 2), Agu::AbsoluteX(0x1000).address(&cpu));

    // wrap if overflow, add extra tick because of cross page
    cpu.x = 1;
    assert_eq!((0, 3, 2), Agu::AbsoluteX(0xffff).address(&cpu));
}

#[test]
fn absolute_y() {
    let mut cpu = Cpu::new();
    cpu.y = 0x10;
    assert_eq!((0x1010, 2, 2), Agu::AbsoluteY(0x1000).address(&cpu));

    // wrap if overflow, add extra tick because of cross page
    cpu.y = 1;
    assert_eq!((0, 3, 2), Agu::AbsoluteY(0xffff).address(&cpu));
}

#[test]
fn indirect_x() {
    let mut cpu = Cpu::new();
    cpu.write_word(0x12, 0x1000);
    cpu.x = 0x10;
    assert_eq!((0x1000, 4, 1), Agu::IndirectX(0x2).address(&cpu));

    // read address from first and last bytes of zero page.
    cpu.write_byte(0xff, 0x20); // low byte
    cpu.write_byte(0x00, 0x10); // high byte
    cpu.x = 0x1;
    assert_eq!((0x1020, 4, 1), Agu::IndirectX(0xfe).address(&cpu));
}

#[test]
fn indirect_y() {
    let mut cpu = Cpu::new();
    cpu.write_word(0x2, 0x1000);
    cpu.y = 0x10;
    assert_eq!((0x1010, 3, 1), Agu::IndirectY(0x2).address(&cpu));

    // wrap if overflow, add extra tick because of cross page
    cpu.write_word(0x2, 0xffff);
    cpu.y = 0x1;
    assert_eq!((0x0, 4, 1), Agu::IndirectY(0x2).address(&cpu));
}

#[test]
fn read_write_byte() {
    let mut cpu = Cpu::new();
    cpu.write_byte(0x1000, 0x10);
    assert_eq!(0x10, cpu.read_byte(0x1000));
    cpu.write_byte(0x1000, 0x20);
    assert_eq!(0x20, cpu.read_byte(0x1000));
}

#[test]
fn read_write_word() {
    let mut cpu = Cpu::new();
    cpu.write_word(0x1000, 0x1020);
    assert_eq!(0x1020, cpu.read_word(0x1000));
    assert_eq!(0x20, cpu.read_byte(0x1000));
    assert_eq!(0x10, cpu.read_byte(0x1001));
}

#[test]
fn read_write_zero_page() {
    let mut cpu = Cpu::new();
    cpu.write_word(0x10, 0x1020);
    assert_eq!(0x1020, cpu.read_zero_page_word(0x10));

    cpu.write_byte(0xff, 0x20);
    cpu.write_byte(0x00, 0x10);
    assert_eq!(0x1020, cpu.read_zero_page_word(0xff));
}

#[test]
fn inc_pc() {
    let mut cpu = Cpu::new();
    cpu.inc_pc(2);
    assert_eq!(cpu.pc, 2);

    // wrap
    cpu.pc = 0xFFFF;
    cpu.inc_pc(2);
    assert_eq!(cpu.pc, 1);
}

struct TestSyncInstructionCycle (u8);

impl SyncInstructionCycle for TestSyncInstructionCycle {
    fn start(&mut self) {
    }

    fn end(&mut self, cycles: u8) {
        self.0 = cycles;
    }
}

impl TestSyncInstructionCycle    {
    fn cycles(&self) -> u8 {
        self.0
    }
}

#[test]
fn adc() {
    let mut cpu = Cpu::new();

    // zero
    cpu.set_flag(CarryFlag, true);
    let adc = Adc(Agu::Literal(0));
    assert_eq!((2, 2), adc.execute(&mut cpu));
    assert_eq!(1, cpu.a);
    assert_eq!(cpu.flag(ZeroFlag), false);
    assert_eq!(cpu.flag(OverflowFlag), false);
    assert_eq!(cpu.flag(CarryFlag), false);
    assert_eq!(cpu.flag(NegativeFlag), false);

    // carry
    cpu.a = 0x80;
    let adc = Adc(Agu::Literal(0x80));
    assert_eq!((2, 2), adc.execute(&mut cpu));
    assert_eq!(0, cpu.a);
    assert_eq!(cpu.flag(ZeroFlag), false);
    assert_eq!(cpu.flag(OverflowFlag), true);
    assert_eq!(cpu.flag(CarryFlag), true);
    assert_eq!(cpu.flag(NegativeFlag), true);
}

#[test]
fn and() {
    let mut cpu = Cpu::new();
    cpu.a = 0b1010_1010;
    let and = And(Agu::Literal(0b1100_1100));
    assert_eq!((2, 2), and.execute(&mut cpu));
    assert_eq!(0b1000_1000, cpu.a);
    assert_eq!(cpu.flag(ZeroFlag), false);
    assert_eq!(cpu.flag(NegativeFlag), true);
}

#[test]
fn cpu_execute() {
    let mut cpu = Cpu::new();
    let mut cycle_sync = TestSyncInstructionCycle(0);

    // carry
    cpu.a = 0xFF;
    cpu.write_byte(0x1000, 10);
    cpu.execute(Instruction::Adc(Adc(Agu::Absolute(0x1000))), &mut cycle_sync);
    assert_eq!(cpu.a, 9);
    assert!(cpu.flag(CarryFlag));
    assert_eq!(cpu.pc, 3);
    assert_eq!(cycle_sync.cycles(), 4);

    // reset carry if no overflow
    cpu.a = 1;
    cpu.execute(Instruction::Adc(Adc(Agu::Literal(10))), &mut cycle_sync);
    assert_eq!(cpu.a, 12);
    assert!(!cpu.flag(CarryFlag));
    assert_eq!(cpu.pc, 5);
    assert_eq!(cycle_sync.cycles(), 2);
}

struct IgnoredFlag;

impl FlagBit for IgnoredFlag {
    const BIT: u8 = 0x20;
}

#[test]
fn test_get() {
    let mut cpu = Cpu::new();
    cpu.write_byte(0x1000, 0x10);
    assert_eq!((0x10, 2, 2), cpu.get(&Agu::Absolute(0x1000)));

    // get literal
    assert_eq!((0x10, 1, 0), cpu.get(&Agu::Literal(0x10)));

    // get registerA
    cpu.a = 0x10;
    assert_eq!((0x10, 0, 0), cpu.get(&Agu::RegisterA));

    // get status
    cpu.status = 0x00;
    cpu.set_flag(BreakFlag, false);
    cpu.set_flag(IgnoredFlag, false);
    assert_eq!((0b0011_0000, 0, 0), cpu.get(&Agu::Status));
}

#[test]
fn test_put() {
    let mut cpu = Cpu::new();
    cpu.write_byte(0x1000, 0x10);
    assert_eq!((2, 2), cpu.put(&Agu::Absolute(0x1000), 0x20));
    assert_eq!(0x20, cpu.read_byte(0x1000));

    // set status not touch break and not used flags
    cpu.status = 0x00;
    assert_eq!((0, 0), cpu.put(&Agu::Status, 0xff));
    assert_eq!(0b1100_1111, cpu.status);
    cpu.status = 0xff;
    cpu.put(&Agu::Status, 0x00);
    assert_eq!(0b0011_0000, cpu.status);
}

#[test]
fn update_zero_flag() {
    let mut cpu = Cpu::new();
    cpu.update_zero_flag(0x10);
    assert_eq!(cpu.flag(ZeroFlag), false);

    cpu.update_zero_flag(0x00);
    assert_eq!(cpu.flag(ZeroFlag), true);
}

#[test]
fn update_negative_flag() {
    let mut cpu = Cpu::new();
    cpu.update_negative_flag(0x10);
    assert_eq!(cpu.flag(NegativeFlag), false);

    cpu.update_negative_flag(0x80);
    assert_eq!(cpu.flag(NegativeFlag), true);
}

#[test]
fn push_stack() {
    let mut cpu = Cpu::new();
    cpu.push_stack(0x10);
    assert_eq!(0x10, cpu.read_byte(0x100));
    assert_eq!(0xFF, cpu.sp);
    cpu.push_stack(0x20);
    assert_eq!(0xFE, cpu.sp);

    assert_eq!(0x20, cpu.pop_stack());
    assert_eq!(0xFF, cpu.sp);
    assert_eq!(0x10, cpu.pop_stack());
    assert_eq!(0x0, cpu.sp);
}
