use super::*;
use crate::test_utils::MockMcu;

#[test]
fn test_machine_creation() {
    let mcu = MockMcu::new();
    let machine = Machine::new(mcu);
    assert_eq!(machine.cpu.pc, 0);
}

#[test]
fn test_machine_with_plugin() {
    let mcu = MockMcu::new();
    let machine = Machine::with_plugin(EmptyPlugin::new(), mcu);
    assert_eq!(machine.cpu.pc, 0);
}

#[test]
fn test_set_pc() {
    let mcu = MockMcu::new();
    let mut machine = Machine::new(mcu);

    machine.set_pc(0x1234);
    assert_eq!(machine.cpu.pc, 0x1234);
}

#[test]
fn test_reset() {
    let mcu = MockMcu::new();
    let mut machine = Machine::new(mcu);

    machine.set_pc(0x5678);
    machine.reset();
    // Reset reads PC from 0xFFFC
    assert_eq!(machine.cpu.pc, 0);
}
