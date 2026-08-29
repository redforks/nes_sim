use super::*;
use crate::{SystemClock, test_utils::MockMcu};

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

    let mut clock = SystemClock::default();
    let mut plugin = EmptyPlugin::new();
    machine
        .cpu_mut()
        .run_to_instruction_boundary(&mut plugin, &mut clock);
    machine.set_pc(0x1234);
    assert_eq!(machine.cpu().pc(), 0x1234);
}

#[test]
fn test_set_pc_drains_pending_reset_microcodes() {
    let mcu = MockMcu::new();
    let mut machine = Machine::new(mcu);

    machine.reset();
    let mut plugin = EmptyPlugin::new();
    let mut clock = SystemClock::default();
    machine
        .cpu_mut()
        .run_to_instruction_boundary(&mut plugin, &mut clock);
    machine.set_pc(0x5678);

    assert_eq!(machine.cpu().pc(), 0x5678);
}

#[test]
fn test_reset() {
    let mcu = MockMcu::new();
    let mut machine = Machine::new(mcu);

    let mut clock = SystemClock::default();
    let mut plugin = EmptyPlugin::new();
    machine
        .cpu_mut()
        .run_to_instruction_boundary(&mut plugin, &mut clock);
    machine.set_pc(0x5678);
    machine.reset();
    // Reset enqueues microcodes that load the reset vector; run them to apply
    let mut plugin = EmptyPlugin::<MockMcu>::new();
    let mut clock = SystemClock::default();
    // Tick until the CPU reports the current instruction is finished
    while !machine
        .cpu_mut()
        .tick(&mut plugin, clock)
        .instruction_complete
    {
        clock = clock.inc();
    }
    // Reset reads PC from 0xFFFC
    assert_eq!(machine.cpu().pc(), 0);
}
