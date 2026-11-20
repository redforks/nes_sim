//! A RAM flag spin and DMC DMA must coexist: the DMA transfer owns the bus
//! while it lasts, and releasing it must not silently wake the spin.
//!
//! The hand-built NROM program arms one DMC sample, then spins on a RAM flag.
//! This is the integration counterpart of the CPU/MCU seam unit tests: it
//! exercises the real `Mcu`, `DmaBus`, and `NesMachine` dot interleaving.

use nes_core::mcu::Mcu;
use nes_core::{EmptyPlugin, SystemClock, ines::INesFile, nes_machine::NesMachine};

fn dmc_spin_rom() -> Vec<u8> {
    let mut rom = b"NES\x1a".to_vec();
    rom.extend_from_slice(&[1, 1, 0, 0]); // 16 KiB PRG, 8 KiB CHR, mapper 0
    rom.extend_from_slice(&[0; 8]);

    let mut prg = vec![0x80u8; 16 * 1024]; // DMC sample bytes
    let program = [
        0xA9, 0x0F, 0x8D, 0x10, 0x40, // $8000 LDA #$0F / STA $4010 (rate)
        0xA9, 0x00, 0x8D, 0x12, 0x40, // $8005 LDA #$00 / STA $4012 (address $C000)
        0xA9, 0x00, 0x8D, 0x13, 0x40, // $800A LDA #$00 / STA $4013 (length)
        0xA9, 0x10, 0x8D, 0x15, 0x40, // $800F LDA #$10 / STA $4015 (enable DMC)
        0xAD, 0x10, 0x00, // $8014 LDA $0010
        0xD0, 0xFB, // $8017 BNE $8014
        0x4C, 0x19, 0x80, // $8019 JMP $8019
    ];
    prg[..program.len()].copy_from_slice(&program);
    let reset = prg.len() - 4;
    prg[reset..reset + 2].copy_from_slice(&0x8000u16.to_le_bytes());
    rom.extend_from_slice(&prg);
    rom.extend_from_slice(&[0u8; 8 * 1024]);
    rom
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
