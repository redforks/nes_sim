use super::*;
use crate::SystemClock;
use crate::ines::INesFile;
use test_case::test_case;

/// Creates a minimal valid iNES ROM for testing
fn create_test_nes(mapper: u8, prg_pages: u8, chr_pages: u8) -> Vec<u8> {
    let mut rom = Vec::new();

    // NES signature
    rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a]);

    // PRG ROM pages
    rom.push(prg_pages);

    // CHR ROM pages
    rom.push(chr_pages);

    // Control byte 1: MSB 4 bits are mapper_low (bits 7-4), LSB 4 bits are flags (bits 3-0)
    let control1 = (mapper & 0x0f) << 4;
    rom.push(control1);

    // Control byte 2: MSB 4 bits are mapper_high (bits 7-4), LSB 4 bits are unused/reserved
    let control2 = ((mapper & 0xf0) >> 4) << 4;
    rom.push(control2);

    // 8 bytes of padding
    rom.extend_from_slice(&[0; 8]);

    // PRG ROM data (16KB per page)
    let prg_size = 16 * 1024 * prg_pages as usize;
    rom.extend(std::iter::repeat_n(0, prg_size));

    // CHR ROM data (8KB per page)
    let chr_size = 8 * 1024 * chr_pages as usize;
    rom.extend(std::iter::repeat_n(0, chr_size));

    rom
}

#[test_case(Mirroring::LowerBank, 0x2000 => 0)]
#[test_case(Mirroring::LowerBank, 0x2001 => 1)]
#[test_case(Mirroring::LowerBank, 0x23ff => 1023)]
#[test_case(Mirroring::LowerBank, 0x27ff => 1023)]
#[test_case(Mirroring::UpperBank, 0x2000 => 0x400)]
#[test_case(Mirroring::UpperBank, 0x2001 => 0x401)]
#[test_case(Mirroring::UpperBank, 0x23ff => 0x7ff)]
#[test_case(Mirroring::UpperBank, 0x27ff => 0x7ff)]
#[test_case(Mirroring::Vertical, 0x2000 => 0)]
#[test_case(Mirroring::Vertical, 0x2001 => 1)]
#[test_case(Mirroring::Vertical, 0x2400 => 0x400)]
#[test_case(Mirroring::Vertical, 0x2401 => 0x401)]
#[test_case(Mirroring::Vertical, 0x2800 => 0)]
#[test_case(Mirroring::Vertical, 0x2801 => 1)]
#[test_case(Mirroring::Vertical, 0x2c00 => 0x400)]
#[test_case(Mirroring::Vertical, 0x2c01 => 0x401)]
#[test_case(Mirroring::Horizontal, 0x2000 => 0)]
#[test_case(Mirroring::Horizontal, 0x2001 => 1)]
#[test_case(Mirroring::Horizontal, 0x2400 => 0)]
#[test_case(Mirroring::Horizontal, 0x2401 => 1)]
#[test_case(Mirroring::Horizontal, 0x2800 => 0x400)]
#[test_case(Mirroring::Horizontal, 0x2801 => 0x401)]
#[test_case(Mirroring::Horizontal, 0x2c00 => 0x400)]
#[test_case(Mirroring::Horizontal, 0x2c02 => 0x402)]
#[test_case(Mirroring::Four, 0x2000 => 0)]
#[test_case(Mirroring::Four, 0x2400 => 0x400)]
#[test_case(Mirroring::Four, 0x2800 => 0x800)]
#[test_case(Mirroring::Four, 0x2c00 => 0xc00)]
fn test_name_table_offset(mirroring: Mirroring, addr: u16) -> u16 {
    mirroring.name_table_offset(addr)
}

#[test]
fn create_cartridge_mapper0() {
    let rom = create_test_nes(0, 1, 1);
    let file = INesFile::new(rom).unwrap();
    let (cartridge, _mirroring) =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    let val = cartridge.read(CARTRIDGE_START_ADDR);
    assert_eq!(val, 0);
    assert_eq!(cartridge.read_chr(0), 0);
}

#[test]
fn create_cartridge_mapper1() {
    let rom = create_test_nes(1, 2, 1);
    let file = INesFile::new(rom).unwrap();
    let (cartridge, _mirroring) =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    let val = cartridge.read(0x8000);
    assert_eq!(val, 0);
    assert_eq!(cartridge.read_chr(0), 0);
}

#[test]
fn create_cartridge_mapper2() {
    let rom = create_test_nes(2, 2, 1);
    let file = INesFile::new(rom).unwrap();
    let (cartridge, _mirroring) =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    let val = cartridge.read(0x8000);
    assert_eq!(val, 0);
    assert_eq!(cartridge.read_chr(0), 0);
}

#[test]
fn create_cartridge_mapper3() {
    let rom = create_test_nes(3, 2, 2);
    let file = INesFile::new(rom).unwrap();
    let (cartridge, _mirroring) =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    let val = cartridge.read(0x8000);
    assert_eq!(val, 0);
    assert_eq!(cartridge.read_chr(0), 0);
}

#[test]
fn create_cartridge_mapper4() {
    let rom = create_test_nes(4, 4, 1);
    let file = INesFile::new(rom).unwrap();
    let (cartridge, _mirroring) =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    let val = cartridge.read(0x8000);
    assert_eq!(val, 0);
    assert_eq!(cartridge.read_chr(0), 0);
}

#[test]
fn create_cartridge_mapper7() {
    let rom = create_test_nes(7, 4, 0);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    assert_eq!(cartridge.read(0x8000), 0);
    assert_eq!(cartridge.read_chr(0x0010), 0);
    cartridge.write_chr(0x0010, 0xab);
    assert_eq!(cartridge.read_chr(0x0010), 0xab);
}

#[test]
fn create_cartridge_mapper34_bnrom() {
    let rom = create_test_nes(34, 4, 0);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    assert_eq!(cartridge.read(0x8000), 0);
    assert_eq!(cartridge.read_chr(0x0010), 0);
    cartridge.write_chr(0x0010, 0xab);
    assert_eq!(cartridge.read_chr(0x0010), 0xab);
}

#[test]
fn create_cartridge_mapper4_with_chr_ram() {
    let rom = create_test_nes(4, 4, 0);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    assert_eq!(cartridge.read_chr(0x0010), 0);
    cartridge.write_chr(0x0010, 0xab);
    assert_eq!(cartridge.read_chr(0x0010), 0xab);
}

#[test]
#[should_panic(expected = "Unsupported cartridge mapper no")]
fn create_cartridge_unsupported_mapper() {
    let rom = create_test_nes(99, 1, 1);
    let file = INesFile::new(rom).unwrap();
    let (_cartridge, _mirroring) =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);
}

// ---------------------------------------------------------------------------
// MMC3 IRQ revision detection predicate (issue #32): decision order pinned
// by the #31 audit — explicit override -> NES 2.0 submapper 004:4 (MMC3A)
// -> content signatures -> Standard by default.
//
// `mmc3_irq_revision_is_alternate` is the single testable predicate: it
// takes PRG bytes directly, so these tests need no ROM probing.
// ---------------------------------------------------------------------------

#[test]
fn mmc3_revision_override_forces_alternate_without_other_signals() {
    assert!(mmc3_irq_revision_is_alternate(
        Mmc3IrqOverride::ForceAlternate,
        None,
        &[]
    ));
    assert!(mmc3_irq_revision_is_alternate(
        Mmc3IrqOverride::ForceAlternate,
        Some(0),
        &[],
    ));
}

#[test]
fn mmc3_revision_override_forces_standard_over_submapper_and_signatures() {
    assert!(!mmc3_irq_revision_is_alternate(
        Mmc3IrqOverride::ForceStandard,
        Some(4),
        &[],
    ));
    assert!(!mmc3_irq_revision_is_alternate(
        Mmc3IrqOverride::ForceStandard,
        None,
        b"xx6-MMC6yy",
    ));
}

#[test]
fn mmc3_revision_submapper_4_maps_to_revision_a() {
    assert!(mmc3_irq_revision_is_alternate(
        Mmc3IrqOverride::Auto,
        Some(4),
        &[]
    ));
}

#[test]
fn mmc3_revision_other_submappers_default_to_standard() {
    for submapper in [None, Some(0), Some(1), Some(2), Some(3)] {
        assert!(!mmc3_irq_revision_is_alternate(
            Mmc3IrqOverride::Auto,
            submapper,
            &[]
        ));
    }
}

#[test]
fn mmc3_revision_content_signatures_fall_back_to_alternate() {
    assert!(mmc3_irq_revision_is_alternate(
        Mmc3IrqOverride::Auto,
        None,
        b"xx6-MMC6yy"
    ));
    assert!(mmc3_irq_revision_is_alternate(
        Mmc3IrqOverride::Auto,
        Some(0),
        b"zz6-MMC3_altzz",
    ));
}

#[test]
fn mmc3_revision_defaults_to_standard() {
    assert!(!mmc3_irq_revision_is_alternate(
        Mmc3IrqOverride::Auto,
        None,
        &[]
    ));
    assert!(!mmc3_irq_revision_is_alternate(
        Mmc3IrqOverride::Auto,
        None,
        b"no signature here",
    ));
}

// Behavior probe through the public Cartridge surface: the two revisions are
// distinguished by what happens when a zero latch is reloaded after the
// counter reached 0 naturally (see mmc3.rs clock_irq and its tests).
use super::mmc3::MMC3_A12_LOW_FILTER_TICKS; // shared with the mapper implementation

/// Drop A12 low, wait out the filter window, then raise A12 -> one IRQ clock.
fn mmc3_irq_clock(cartridge: &mut Box<dyn Cartridge>) {
    cartridge.notify_vram_address(0x0000);
    for _ in 0..=MMC3_A12_LOW_FILTER_TICKS {
        cartridge.on_ppu_tick(0);
    }
    cartridge.notify_vram_address(0x1000);
}

/// Arm latch=1 with a pending reload, enable IRQ, clock twice: the counter
/// lands on 0 via decrement and the IRQ asserts on both revisions.
fn mmc3_arm_counter_one_and_reach_zero(cartridge: &mut Box<dyn Cartridge>) {
    cartridge.write(0xc000, 1, SystemClock::default()); // latch = 1
    cartridge.write(0xc001, 0, SystemClock::default()); // request reload
    cartridge.write(0xe001, 0, SystemClock::default()); // enable IRQ

    mmc3_irq_clock(cartridge); // requested reload -> counter = 1
    mmc3_irq_clock(cartridge); // decrement -> 0 -> IRQ asserted
    assert!(cartridge.irq_pending());

    cartridge.write(0xe000, 0, SystemClock::default()); // disable + ack
    cartridge.write(0xe001, 0, SystemClock::default()); // re-enable
}

/// The rev-A / Standard discriminator: with latch 0 and the counter already
/// at 0 naturally, does one more clock re-assert the IRQ?
fn mmc3_zero_latch_reload(cartridge: &mut Box<dyn Cartridge>) {
    cartridge.write(0xc000, 0, SystemClock::default());
    mmc3_irq_clock(cartridge);
}

#[test]
fn mmc3_override_rev_a_skips_irq_on_reload_after_natural_zero() {
    let rom = create_test_nes(4, 2, 1);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::ForceAlternate);

    mmc3_arm_counter_one_and_reach_zero(&mut cartridge);
    mmc3_zero_latch_reload(&mut cartridge);
    assert!(!cartridge.irq_pending());
}

#[test]
fn mmc3_override_standard_asserts_irq_every_clock_with_zero_latch() {
    let rom = create_test_nes(4, 2, 1);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::ForceStandard);

    mmc3_arm_counter_one_and_reach_zero(&mut cartridge);
    for _ in 0..3 {
        mmc3_zero_latch_reload(&mut cartridge);
        assert!(cartridge.irq_pending());
        cartridge.write(0xe000, 0, SystemClock::default()); // ack
        cartridge.write(0xe001, 0, SystemClock::default()); // re-enable
    }
}

/// Builds a NES 2.0 header (16-byte) for mapper 4 with the given submapper.
fn nes20_mapper4_rom(submapper: u8) -> Vec<u8> {
    let mut rom = Vec::new();
    rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a]);
    rom.push(2); // PRG pages (16 KB each)
    rom.push(1); // CHR pages (8 KB)
    rom.push(0x40); // flags6: mapper low nibble 4, horizontal mirroring bit clear
    rom.push(0x08); // flags7: NES 2.0
    rom.push(submapper << 4); // flags8: submapper high nibble, mapper 8-11 = 0
    rom.extend_from_slice(&[0; 8]); // flags9..15
    rom.extend(std::iter::repeat_n(0, 32 * 1024)); // PRG ROM
    rom.extend(std::iter::repeat_n(0, 8 * 1024)); // CHR ROM
    rom
}

#[test]
fn nes20_submapper_4_auto_detects_revision_a() {
    let file = INesFile::new(nes20_mapper4_rom(4)).unwrap();
    assert_eq!(Some(4), file.header().submapper_no);
    let (mut cartridge, _mirroring) =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    mmc3_arm_counter_one_and_reach_zero(&mut cartridge);
    mmc3_zero_latch_reload(&mut cartridge);
    assert!(!cartridge.irq_pending());
}

#[test]
fn nes20_submapper_0_defaults_to_standard() {
    let file = INesFile::new(nes20_mapper4_rom(0)).unwrap();
    assert_eq!(Some(0), file.header().submapper_no);
    let (mut cartridge, _mirroring) =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    mmc3_arm_counter_one_and_reach_zero(&mut cartridge);
    mmc3_zero_latch_reload(&mut cartridge);
    assert!(cartridge.irq_pending());
}
