//! A RAM flag spin or a `$4015` device poll and DMC DMA must coexist: the
//! DMA owns the bus while it lasts, and releasing it must not silently wake
//! the spin (GLOSSARY "DMA ... never wake a spin"). The hand-built NROM
//! programs arm one DMC sample, then spin on either a RAM flag or the real
//! APU status register, exercising `NesMachine`'s dot interleaving.

use nes_core::mcu::Mcu;
use nes_core::{EmptyPlugin, SystemClock, ines::INesFile, nes_machine::NesMachine};

#[path = "../src/test_utils/wait_rom.rs"]
mod wait_rom;

/// Loop-top PC of both DMC test programs: `LDA <source>` at `$8014`, `BNE
/// $8014` at `$8017`, and a `JMP $8019` parking loop after the fall-through.
const LOOP_TOP: u16 = 0x8014;
/// Last in-loop PC: the fall-through after `BNE` is `$8019`.
const LOOP_END: u16 = 0x8018;

fn machine_for(rom: Vec<u8>) -> NesMachine<EmptyPlugin<nes_core::nes::NesMcu>> {
    let file = INesFile::new(rom).expect("valid iNES file");
    NesMachine::new(&file, EmptyPlugin::new())
}

/// Dot-step until the program first reaches its wait loop.
fn run_to_wait(machine: &mut NesMachine<EmptyPlugin<nes_core::nes::NesMcu>>) {
    for _ in 0..100_000 {
        machine.tick();
        if machine.cpu().pc() == LOOP_TOP {
            return;
        }
    }
    panic!("the program must reach its wait loop at {LOOP_TOP:#06x}");
}

#[test]
fn dmc_dma_completes_while_a_ram_spin_is_declared() {
    let mut machine = machine_for(wait_rom::dmc_ram_spin_image());
    machine.mcu_mut().write(0x0010, 1, SystemClock::default());
    run_to_wait(&mut machine);

    // The RAM flag never clears, so the spin stays declared for the rest of
    // the run while DMA completes underneath it.
    for _ in 0..100_000 {
        machine.tick();
    }

    assert_eq!(
        machine.cpu().pc(),
        LOOP_TOP,
        "the RAM spin must stay pinned at its loop top"
    );
    assert_eq!(
        machine.mcu().peek(0x4015) & 0x10,
        0,
        "the DMC DMA must consume its sample and clear the active bit"
    );
}

#[test]
fn dmc_dma_completes_without_waking_a_device_status_poll() {
    let mut machine = machine_for(wait_rom::dmc_device_poll_image());
    run_to_wait(&mut machine);

    // While DMC is active the frozen `$4015` poll must not walk its loop:
    // any PC past `LOOP_END` with bit 4 still set means the DMA woke it.
    let mut left_loop_while_active = None;
    let mut completed = false;
    for step in 0..100_000 {
        machine.tick();
        let active = machine.mcu().peek(0x4015) & 0x10 != 0;
        if active && left_loop_while_active.is_none() && machine.cpu().pc() > LOOP_END {
            left_loop_while_active = Some(step);
        }
        if !active {
            completed = true;
            break;
        }
    }

    assert_eq!(
        left_loop_while_active, None,
        "the DMA must never wake the frozen $4015 poll"
    );
    assert!(
        completed,
        "the DMC DMA must complete even while the device poll is frozen"
    );
    assert_eq!(
        machine.mcu().peek(0x4015) & 0x10,
        0,
        "the DMC DMA must consume its sample and clear the active bit"
    );
}
