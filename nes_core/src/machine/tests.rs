use super::*;
use crate::test_utils::MockMcu;

#[test]
fn test_machine_creation() {
    let mcu = MockMcu::new();
    let machine = Machine::new(mcu);
    assert_eq!(machine.cpu().pc(), 0);
}

#[test]
fn test_machine_with_plugin() {
    let mcu = MockMcu::new();
    let machine = Machine::with_plugin(EmptyPlugin::new(), mcu);
    assert_eq!(machine.cpu().pc(), 0);
}

#[test]
fn test_set_pc() {
    let mcu = MockMcu::new();
    let mut machine = Machine::new(mcu);

    let mut plugin = EmptyPlugin::new();
    machine.drain_to_instruction_boundary(&mut plugin);
    machine.set_pc(0x1234);
    assert_eq!(machine.cpu().pc(), 0x1234);
}

#[test]
fn test_set_pc_drains_pending_reset_microcodes() {
    let mcu = MockMcu::new();
    let mut machine = Machine::new(mcu);

    machine.reset();
    let mut plugin = EmptyPlugin::new();
    machine.drain_to_instruction_boundary(&mut plugin);
    machine.set_pc(0x5678);

    assert_eq!(machine.cpu().pc(), 0x5678);
}

#[test]
fn test_reset() {
    let mcu = MockMcu::new();
    let mut machine = Machine::new(mcu);

    let mut plugin = EmptyPlugin::new();
    machine.drain_to_instruction_boundary(&mut plugin);
    machine.set_pc(0x5678);
    machine.reset();
    // Reset enqueues microcodes that load the reset vector; run them to apply
    // Run the reset sequence to completion (the CPU boundary records each
    // instruction as finished only when its queue drains).
    machine.run_to_instruction_boundary();
    // Reset reads PC from 0xFFFC
    assert_eq!(machine.cpu().pc(), 0);
}
