//! Regression test for the DMC DMA deadlock introduced by the DmcDma
//! phase-alignment refactor.
//!
//! `sprdma_and_dmc_dma.nes` polls `$4015` bit 4 waiting for the DMC
//! bytes-remaining flag to clear. With a broken DMA completion cycle the poll
//! never observes the clear and the ROM spins forever. The ROM must reach
//! completion (pass or fail verdict both count — the hang is the bug).

use nes_core::nes::NesMcu;
use nes_core::{EmptyPlugin, ines::INesFile, nes_machine::NesMachine};

#[test]
fn sprdma_and_dmc_dma_rom_terminates() {
    // The test ROM lives in the shared nesdev test-roms checkout next to this
    // repository (see justfile). Skip when it is not available.
    let rom_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../nes-test-roms/sprdma_and_dmc_dma/sprdma_and_dmc_dma.nes");
    let Ok(rom) = std::fs::read(&rom_path) else {
        eprintln!("skipping: {} not found", rom_path.display());
        return;
    };

    let ines = INesFile::new(rom).expect("valid iNES file");
    let mut machine = NesMachine::new(&ines, EmptyPlugin::new(), (), ());

    // Blargg test ROM interface: `$6000-$6003` must hold the signature
    // `DE B0 61`; `$6000` is `$80` while running and the verdict afterwards.
    let status = |machine: &NesMachine<EmptyPlugin<NesMcu<(), ()>>, (), ()>| {
        let cpu = machine.cpu();
        if cpu.peek_byte(0x6001) != 0xDE
            || cpu.peek_byte(0x6002) != 0xB0
            || cpu.peek_byte(0x6003) != 0x61
        {
            None
        } else {
            Some(cpu.peek_byte(0x6000))
        }
    };

    // The fixed emulator completes the ROM at ~12.4M clocks; 30M leaves >2x
    // headroom while keeping the red run reasonably fast.
    let clock_budget: u64 = 30_000_000;

    for tick in 0..clock_budget {
        if tick % 256 == 0
            && let Some(s) = status(&machine)
            && s != 0x80
        {
            return;
        }
        machine.tick();
    }

    panic!(
        "ROM still running after {clock_budget} clocks: DMC DMA never completes \
         relative to the $4015 poll (deadlock)"
    );
}
