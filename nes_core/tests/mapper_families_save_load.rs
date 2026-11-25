//! Ticket #194: per-mapper-family save-at-N / load / continue tests.
//!
//! Mapper IDs 1, 2, 3, 4, 5, 7, 21, 22, 23, 25, 28, 34, 87 (ID 0 is covered
//! by #193's `resumed_run_is_bit_exact_with_continuous_run`). One synthetic
//! ROM per family extends the #193 NROM pattern: reset code enables NMI and
//! rendering (so every saved frame serviced a vblank NMI), programs the APU
//! (including the DMC loop, so DMA traffic never stops), pokes the mapper
//! registers to non-default banks, and programs the mapper IRQ where the
//! mapper has one — leaving it pending at the save point (the IRQ handler
//! never acknowledges; SEI keeps the CPU masked so the run stays fast).
//!
//! Each test: run HEAD frames, save, load into a fresh machine built from
//! the same ROM, continue TAIL frames. Frame bytes and audio must equal the
//! continuous run. Save-point note: the only loadable save point is
//! scanline 241, dot 2..=4 (ADR 0024; load refuses anything else), at
//! the end of the CPU cycle containing the VBlank set dot. A save therefore
//! lands just inside vblank. Every ROM here keeps NMI + rendering live, which
//! is the closest the fixed save-point contract allows.
//!
//! External behavior only: no DTO field-order assertions. The premise
//! checks below assert *values* in the public save JSON to prove the test
//! setup (the writes really executed and the IRQ really is pending), plus a
//! uniform fresh-boot-vs-save cartridge comparison as a backstop.

use nes_core::{
    EmptyPlugin, ExecuteResult,
    ines::INesFile,
    nes_machine::NesMachine,
    snapshot::{load_state_json, save_state_json},
};

const HEAD_FRAMES: usize = 5;
const TAIL_FRAMES: usize = 8;

/// One mapper-register poke: either a plain `LDA #v / STA addr`, or one
/// full MMC1 serial sequence (`LDA #$80 / STA` reset, then the five value
/// bits LSB-first through the same range).
enum Op {
    Sta(u16, u8),
    Mmc1(u16, u8),
}

fn push_sta(code: &mut Vec<u8>, addr: u16, value: u8) {
    code.push(0xA9);
    code.push(value);
    code.push(0x8D);
    code.push((addr & 0xFF) as u8);
    code.push((addr >> 8) as u8);
}

/// #193's rich-NROM init (NMI + rendering + APU + strobe), then the mapper
/// pokes, then `INC $00` spin. NMI does `INC $02`, IRQ does `INC $03`.
/// The image is one 8 KiB block (code at block start, vectors at block end)
/// repeated across PRG, so the reset vector and entry point read correctly
/// no matter which bank each mapper windows in. `entry` is the reset
/// vector: `$8000` everywhere except MMC5, whose power-on `$8000-$DFFF`
/// windows show WRAM (bank values without bit 7 select RAM), so it must
/// enter at `$E000` (ROM from power-on) and map ROM over the low windows
/// itself.
fn build_rom(mapper: u8, prg_pages: u8, chr_pages: u8, entry: u16, ops: &[Op]) -> INesFile {
    let mut code: Vec<u8> = Vec::new();
    // SEI CLD LDX #$FF TXS.
    code.extend_from_slice(&[0x78, 0xD8, 0xA2, 0xFF, 0x9A]);
    push_sta(&mut code, 0x2000, 0x88);
    push_sta(&mut code, 0x2001, 0x1E);
    push_sta(&mut code, 0x2005, 0x00);
    code.extend_from_slice(&[0x8D, 0x05, 0x20]); // STA $2005
    code.extend_from_slice(&[0x8D, 0x06, 0x20, 0x8D, 0x06, 0x20]); // STA $2006 x2
    push_sta(&mut code, 0x2007, 0x55);
    push_sta(&mut code, 0x2003, 0x10);
    push_sta(&mut code, 0x2004, 0xAA);
    push_sta(&mut code, 0x4016, 0x01);
    push_sta(&mut code, 0x4016, 0x00);
    push_sta(&mut code, 0x4000, 0xBF);
    push_sta(&mut code, 0x4001, 0x08);
    push_sta(&mut code, 0x4002, 0x50);
    push_sta(&mut code, 0x4003, 0x08);
    push_sta(&mut code, 0x4008, 0x80);
    push_sta(&mut code, 0x400A, 0x40);
    push_sta(&mut code, 0x400B, 0x08);
    push_sta(&mut code, 0x400C, 0xBF);
    push_sta(&mut code, 0x400E, 0x05);
    push_sta(&mut code, 0x400F, 0x08);
    push_sta(&mut code, 0x4010, 0x80);
    push_sta(&mut code, 0x4012, 0x00);
    code.extend_from_slice(&[0x8D, 0x13, 0x40]); // STA $4013 (A == 0)
    push_sta(&mut code, 0x4015, 0x1F);
    push_sta(&mut code, 0x4017, 0x00);
    for op in ops {
        match *op {
            Op::Sta(addr, value) => push_sta(&mut code, addr, value),
            Op::Mmc1(range, value) => {
                push_sta(&mut code, range, 0x80);
                for bit in 0..5 {
                    push_sta(&mut code, range, (value >> bit) & 0x01);
                }
            }
        }
    }
    // Main loop: INC $00; JMP loop. NMI: INC $02; RTI. IRQ: INC $03; RTI.
    let loop_addr = 0x8000 + code.len() as u16;
    code.push(0xE6);
    code.push(0x00);
    code.push(0x4C);
    code.push((loop_addr & 0xFF) as u8);
    code.push((loop_addr >> 8) as u8);
    let nmi_addr = 0x8000 + code.len() as u16;
    code.extend_from_slice(&[0xE6, 0x02, 0x40]);
    let irq_addr = 0x8000 + code.len() as u16;
    code.push(0xE6);
    code.push(0x03);
    code.push(0x40);

    let mut block = vec![0u8; 8 * 1024];
    block[..code.len()].copy_from_slice(&code);
    let put = |block: &mut Vec<u8>, off: usize, value: u16| {
        block[off] = (value & 0xFF) as u8;
        block[off + 1] = (value >> 8) as u8;
    };
    put(&mut block, 0x1FFA, nmi_addr);
    put(&mut block, 0x1FFC, entry);
    put(&mut block, 0x1FFE, irq_addr);

    let mut prg = Vec::new();
    for _ in 0..prg_pages as usize * 2 {
        prg.extend_from_slice(&block);
    }
    let mut chr = Vec::new();
    for page in 0..chr_pages as usize {
        for i in 0..8 * 1024 {
            chr.push((page as u8).wrapping_mul(0x23).wrapping_add(i as u8));
        }
    }
    let mut rom = vec![
        0x4e,
        0x45,
        0x53,
        0x1a, //
        prg_pages,
        chr_pages, //
        (mapper & 0x0F) << 4,
        mapper & 0xF0, //
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
    ];
    rom.extend_from_slice(&prg);
    rom.extend_from_slice(&chr);
    INesFile::new(rom).expect("valid synthetic iNES file")
}

fn run_frames(
    machine: &mut NesMachine<EmptyPlugin<nes_core::nes::NesMcu>>,
    frames: usize,
    tag: &str,
) -> (Vec<Vec<u8>>, Vec<Vec<f32>>) {
    let mut pixels = Vec::with_capacity(frames);
    let mut audio = Vec::with_capacity(frames);
    for frame in 0..frames {
        let out = machine.process_frame();
        assert_eq!(
            out.status,
            ExecuteResult::Continue,
            "{tag}: frame {frame} did not complete"
        );
        pixels.push(out.frame.to_vec());
        audio.push(out.audio.to_vec());
    }
    (pixels, audio)
}

/// Save after HEAD frames, load into a fresh machine, continue TAIL frames:
/// pixels and audio must equal the continuous run. `check_state` asserts
/// mapper-specific values in the public save JSON (proves the pokes
/// executed); every case also asserts the saved cartridge section differs
/// from a fresh boot's (backstop, covers stateless mappers too).
fn run_case(
    tag: &str,
    mapper: u8,
    prg_pages: u8,
    chr_pages: u8,
    entry: u16,
    ops: &[Op],
    check_state: Option<fn(&serde_json::Value, &str)>,
) {
    let rom = build_rom(mapper, prg_pages, chr_pages, entry, ops);

    // Fresh-boot cartridge section: the power-on baseline.
    let boot = NesMachine::new(&rom, EmptyPlugin::new());
    let boot_save = save_state_json(&boot).expect("boot save must succeed");
    let boot_parsed: serde_json::Value =
        serde_json::from_str(&boot_save).expect("save is valid JSON");
    let boot_cart = boot_parsed["cartridge"].clone();

    let mut continuous = NesMachine::new(&rom, EmptyPlugin::new());
    run_frames(&mut continuous, HEAD_FRAMES, tag);
    let save = save_state_json(&continuous).expect("save must succeed");
    let parsed: serde_json::Value = serde_json::from_str(&save).expect("save is valid JSON");
    assert_ne!(
        parsed["cartridge"], boot_cart,
        "{tag}: mapper pokes must change the saved cartridge state"
    );
    if let Some(check) = check_state {
        check(&parsed["cartridge"]["state"], tag);
    }
    let saved_frame = continuous.frame_no();
    let saved_clock = continuous.clock_cycles();
    let (tail_pixels, tail_audio) = run_frames(&mut continuous, TAIL_FRAMES, tag);

    let mut resumed = NesMachine::new(&rom, EmptyPlugin::new());
    load_state_json(&mut resumed, &save).expect("load must succeed");
    assert_eq!(
        resumed.frame_no(),
        saved_frame,
        "{tag}: resume at same frame"
    );
    assert_eq!(
        resumed.clock_cycles(),
        saved_clock,
        "{tag}: resume at same clock"
    );
    let (re_pixels, re_audio) = run_frames(&mut resumed, TAIL_FRAMES, tag);

    assert_eq!(re_pixels, tail_pixels, "{tag}: framebuffer bit-exact");
    assert_eq!(re_audio, tail_audio, "{tag}: audio bit-exact");
    assert_eq!(resumed.frame_no(), continuous.frame_no());
    assert_eq!(resumed.clock_cycles(), continuous.clock_cycles());
}

fn expect_irq_pending(state: &serde_json::Value, tag: &str) {
    assert_eq!(
        state["irq_pending"],
        serde_json::Value::Bool(true),
        "{tag}: mapper IRQ must be pending at the save point"
    );
}

// --- MMC1 (mapper 1): serial control/CHR/PRG programming, no IRQ. ---

fn check_mmc1(state: &serde_json::Value, tag: &str) {
    assert_eq!(state["control"], 0x1Cu8, "{tag}: control");
    assert_eq!(state["prg_bank"], 1u8, "{tag}: prg_bank");
}

#[test]
fn mmc1_save_load_continue_is_bit_exact() {
    run_case(
        "mmc1",
        1,
        2,
        2,
        0x8000,
        &[
            Op::Mmc1(0x8000, 0x1C), // 4 KiB CHR mode, fix-$C000 16 KiB PRG
            Op::Mmc1(0xA000, 0x02), // CHR bank 0 = 2
            Op::Mmc1(0xC000, 0x03), // CHR bank 1 = 3
            Op::Mmc1(0xE000, 0x01), // PRG bank = 1
        ],
        Some(check_mmc1),
    );
}

// --- UxROM (mapper 2): 16 KiB bank select, no IRQ. ---

fn check_uxrom(state: &serde_json::Value, tag: &str) {
    assert_eq!(state["selected_prg_bank"], 2u16, "{tag}: selected_prg_bank");
}

#[test]
fn uxrom_save_load_continue_is_bit_exact() {
    run_case(
        "uxrom",
        2,
        4,
        1,
        0x8000,
        &[Op::Sta(0x8000, 0x02)],
        Some(check_uxrom),
    );
}

// --- CNROM (mapper 3): 8 KiB CHR bank select, no IRQ. The mapper DTO is
// empty (the bank lives in the shared memory windows), so the
// fresh-boot-vs-save cartridge comparison carries the premise. ---

#[test]
fn cnrom_save_load_continue_is_bit_exact() {
    run_case("cnrom", 3, 2, 4, 0x8000, &[Op::Sta(0x8000, 0x03)], None);
}

// --- MMC3 (mapper 4): CHR/PRG banks + scanline IRQ left pending. ---

fn check_mmc3(state: &serde_json::Value, tag: &str) {
    assert_eq!(state["bank_select"], 7u8, "{tag}: bank_select");
    expect_irq_pending(state, tag);
}

#[test]
fn mmc3_save_load_continue_is_bit_exact_with_irq_pending() {
    run_case(
        "mmc3",
        4,
        2,
        2,
        0x8000,
        &[
            Op::Sta(0x8000, 0x00),
            Op::Sta(0x8001, 0x04), // 2 KiB CHR at $0000
            Op::Sta(0x8000, 0x06),
            Op::Sta(0x8001, 0x01), // 8 KiB PRG at $8000 = bank 1
            Op::Sta(0x8000, 0x07),
            Op::Sta(0x8001, 0x02), // 8 KiB PRG at $A000 = bank 2
            Op::Sta(0xA000, 0x01), // horizontal mirroring
            Op::Sta(0xC000, 0x08), // IRQ latch 8
            Op::Sta(0xC001, 0x00), // reload counter
            Op::Sta(0xE001, 0x00), // enable IRQ (never acked: stays pending)
        ],
        Some(check_mmc3),
    );
}

// --- MMC5 (mapper 5): unlock + mode-3 PRG banks, multiplier, scanline IRQ
// pending. Values with bit 7 set select ROM (bare values select WRAM). ---

fn check_mmc5(state: &serde_json::Value, tag: &str) {
    assert_eq!(state["mult_a"], 6u8, "{tag}: mult_a");
    assert_eq!(state["mult_b"], 7u8, "{tag}: mult_b");
    assert_eq!(state["mult_result"], 42u16, "{tag}: mult_result");
    assert_eq!(state["irq_target"], 0x60u8, "{tag}: irq_target");
    expect_irq_pending(state, tag);
}

#[test]
fn mmc5_save_load_continue_is_bit_exact_with_irq_pending() {
    run_case(
        "mmc5",
        5,
        4,
        2,
        0xE000,
        &[
            Op::Sta(0x5102, 0x02),
            Op::Sta(0x5103, 0x01), // unlock PRG registers
            Op::Sta(0x5100, 0x03), // 8 KiB PRG mode
            Op::Sta(0x5114, 0x81),
            Op::Sta(0x5115, 0x82),
            Op::Sta(0x5116, 0x83),
            Op::Sta(0x5117, 0x80), // ROM banks at $8000-$FFFF
            Op::Sta(0x5113, 0x00), // WRAM bank at $6000
            Op::Sta(0x5205, 0x06),
            Op::Sta(0x5206, 0x07), // multiplier 6 * 7
            Op::Sta(0x5203, 0x60), // IRQ target scanline 96
            Op::Sta(0x5204, 0x80), // enable IRQ (never acked: stays pending)
        ],
        Some(check_mmc5),
    );
}

// --- AxROM (mapper 7): 32 KiB bank + mirroring bit, no IRQ. ---

fn check_axrom(state: &serde_json::Value, tag: &str) {
    assert_eq!(state["selected_prg_bank"], 1u8, "{tag}: selected_prg_bank");
}

#[test]
fn axrom_save_load_continue_is_bit_exact() {
    run_case(
        "axrom",
        7,
        4,
        1,
        0x8000,
        &[Op::Sta(0x8000, 0x11)],
        Some(check_axrom),
    );
}

// --- VRC2/4 (mappers 21/22/23/25): PRG/CHR banks; IRQ where wired.
// Mapper 22 is VRC2a, whose $F000 range carries no IRQ registers, so it is
// the non-IRQ case of the family. ---

fn check_vrc(state: &serde_json::Value, tag: &str) {
    assert_eq!(state["prg_select_0"], 2u8, "{tag}: prg_select_0");
    assert_eq!(state["prg_select_1"], 1u8, "{tag}: prg_select_1");
}

fn check_vrc_irq(state: &serde_json::Value, tag: &str) {
    check_vrc(state, tag);
    assert_eq!(state["irq_latch"], 5u8, "{tag}: irq_latch");
    expect_irq_pending(state, tag);
}

#[test]
fn vrc21_save_load_continue_is_bit_exact_with_irq_pending() {
    run_case(
        "vrc21",
        21,
        2,
        2,
        0x8000,
        &[
            Op::Sta(0x8000, 0x02),
            Op::Sta(0xA000, 0x01),
            Op::Sta(0x9000, 0x01),
            Op::Sta(0xB000, 0x03),
            Op::Sta(0xB002, 0x02),
            Op::Sta(0xF000, 0x05), // IRQ latch lo (VRC4a layout)
            Op::Sta(0xF002, 0x00), // IRQ latch hi
            Op::Sta(0xF004, 0x02), // enable (never acked: stays pending)
        ],
        Some(check_vrc_irq),
    );
}

#[test]
fn vrc22_save_load_continue_is_bit_exact() {
    run_case(
        "vrc22",
        22,
        2,
        2,
        0x8000,
        &[
            Op::Sta(0x8000, 0x02),
            Op::Sta(0xA000, 0x01),
            Op::Sta(0x9000, 0x01),
            Op::Sta(0xB000, 0x03),
            Op::Sta(0xB002, 0x02),
        ],
        Some(check_vrc),
    );
}

#[test]
fn vrc23_save_load_continue_is_bit_exact_with_irq_pending() {
    run_case(
        "vrc23",
        23,
        2,
        2,
        0x8000,
        &[
            Op::Sta(0x8000, 0x02),
            Op::Sta(0xA000, 0x01),
            Op::Sta(0x9000, 0x01),
            Op::Sta(0xB000, 0x03),
            Op::Sta(0xB001, 0x02),
            Op::Sta(0xF000, 0x05), // IRQ latch lo (VRC4f layout)
            Op::Sta(0xF001, 0x00), // IRQ latch hi
            Op::Sta(0xF002, 0x02), // enable (never acked: stays pending)
        ],
        Some(check_vrc_irq),
    );
}

#[test]
fn vrc25_save_load_continue_is_bit_exact_with_irq_pending() {
    run_case(
        "vrc25",
        25,
        2,
        2,
        0x8000,
        &[
            Op::Sta(0x8000, 0x02),
            Op::Sta(0xA000, 0x01),
            Op::Sta(0x9000, 0x01),
            Op::Sta(0xB000, 0x03),
            Op::Sta(0xB002, 0x02),
            Op::Sta(0xF000, 0x05), // IRQ latch lo (VRC4b layout)
            Op::Sta(0xF002, 0x00), // IRQ latch hi
            Op::Sta(0xF001, 0x02), // enable (never acked: stays pending)
        ],
        Some(check_vrc_irq),
    );
}

// --- Action53 (mapper 28): select/value register file, no IRQ. ---

fn check_action53(state: &serde_json::Value, tag: &str) {
    // regs file [CHR, inner, mode, outer] as one lowercase hex string.
    assert_eq!(state["regs"], "01020800", "{tag}: regs");
}

#[test]
fn action53_save_load_continue_is_bit_exact() {
    run_case(
        "action53",
        28,
        4,
        1,
        0x8000,
        &[
            Op::Sta(0x5000, 0x01),
            Op::Sta(0x8000, 0x02), // inner = 2
            Op::Sta(0x5000, 0x81),
            Op::Sta(0x8000, 0x00), // outer = 0
            Op::Sta(0x5000, 0x80),
            Op::Sta(0x8000, 0x08), // fixed-16 KiB mode
            Op::Sta(0x5000, 0x00),
            Op::Sta(0x8000, 0x01), // CHR reg = 1
        ],
        Some(check_action53),
    );
}

// --- Mapper 34: Nina001 half (CHR > 8 KiB) and BxROM half (CHR <= 8 KiB).
// The cartridge factory picks the concrete type by CHR size, so both halves
// get a test. ---

fn check_nina001(state: &serde_json::Value, tag: &str) {
    assert_eq!(state["selected_prg_bank"], 1u8, "{tag}: selected_prg_bank");
    assert_eq!(state["chr_bank0"], 2u8, "{tag}: chr_bank0");
    assert_eq!(state["chr_bank1"], 3u8, "{tag}: chr_bank1");
}

#[test]
fn nina001_mapper34_save_load_continue_is_bit_exact() {
    run_case(
        "nina001",
        34,
        4,
        2,
        0x8000,
        &[
            Op::Sta(0x7FFE, 0x02), // CHR bank 0
            Op::Sta(0x7FFF, 0x03), // CHR bank 1
            Op::Sta(0x8000, 0x01), // 32 KiB PRG bank 1
        ],
        Some(check_nina001),
    );
}

fn check_bxrom(state: &serde_json::Value, tag: &str) {
    assert_eq!(state["selected_prg_bank"], 1u16, "{tag}: selected_prg_bank");
}

#[test]
fn bxrom_mapper34_save_load_continue_is_bit_exact() {
    run_case(
        "bxrom",
        34,
        4,
        1,
        0x8000,
        &[Op::Sta(0x8000, 0x01)],
        Some(check_bxrom),
    );
}

// --- J87 (mapper 87): stateless CHR-band switch, no IRQ. The mapper DTO is
// empty (the band lives in the shared memory windows), so the
// fresh-boot-vs-save cartridge comparison carries the premise. ---

#[test]
fn j87_save_load_continue_is_bit_exact() {
    run_case("j87", 87, 2, 4, 0x8000, &[Op::Sta(0x6000, 0x01)], None);
}
