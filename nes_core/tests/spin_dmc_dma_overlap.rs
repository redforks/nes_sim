//! A RAM flag spin and DMC DMA must coexist: the DMA transfer owns the bus
//! while it lasts, and releasing it must not silently wake the spin.
//!
//! The hand-built NROM program arms one DMC sample, then spins on a RAM flag.
//! This is the integration counterpart of the CPU/MCU seam unit tests: it
//! exercises the real `Mcu`, `DmaBus`, and `NesMachine` dot interleaving.

use nes_core::mcu::Mcu;
use nes_core::{EmptyPlugin, SystemClock, ines::INesFile, nes_machine::NesMachine};

#[path = "../src/test_utils/wait_rom.rs"]
mod wait_rom;

fn dmc_spin_rom() -> Vec<u8> {
    wait_rom::dmc_ram_spin_image()
}

#[test]
fn dmc_dma_completes_while_a_ram_spin_is_declared() {
    let file = INesFile::new(dmc_spin_rom()).expect("valid iNES file");
    let mut machine = NesMachine::new(&file, EmptyPlugin::new());
    machine.mcu_mut().write(0x0010, 1, SystemClock::default());

    let mut reached_wait_pc = false;
    for _ in 0..100_000 {
        machine.tick();
        if !reached_wait_pc && machine.cpu().pc() == 0x8014 {
            reached_wait_pc = true;
        }
    }

    assert!(reached_wait_pc, "the program must reach its RAM flag spin");
    assert_eq!(
        machine.mcu().peek(0x4015) & 0x10,
        0,
        "the DMC DMA must consume its sample and clear the active bit"
    );
}
