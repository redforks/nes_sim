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

/// Mirroring is *which* nametable bank each of the four slots shows: write
/// through `$2000` and read back through a slot that may alias it.
#[test_case(Mirroring::LowerBank, 0x2405, true ; "single lower: all slots alias")]
#[test_case(Mirroring::LowerBank, 0x2c05, true ; "single lower: third slot aliases")]
#[test_case(Mirroring::UpperBank, 0x2405, true ; "single upper: second slot aliases")]
#[test_case(Mirroring::Vertical, 0x2805, true ; "vertical: third slot repeats first")]
#[test_case(Mirroring::Vertical, 0x2405, false ; "vertical: second slot is distinct")]
#[test_case(Mirroring::Horizontal, 0x2405, true ; "horizontal: second slot repeats first")]
#[test_case(Mirroring::Horizontal, 0x2805, false ; "horizontal: third slot is distinct")]
#[test_case(Mirroring::Four, 0x2405, false ; "four screen: second slot distinct")]
#[test_case(Mirroring::Four, 0x2805, false ; "four screen: third slot distinct")]
fn mirroring_selects_nametable_banks(mirroring: Mirroring, probe: u16, aliases: bool) {
    let mut memory = CartridgeMemory::from_rom(&[0; 0x8000], &[], None, mirroring);
    memory.write_vram(0x2005, 0x5a);
    assert_eq!(memory.read_vram(0x2005), 0x5a);
    assert_eq!(memory.read_vram(probe) == 0x5a, aliases);
}

/// `$3000-$3EFF` is the hardware alias of `$2000-$2EFF`: each alias window
/// slot carries the same nametable bank as the slot it repeats, so writes and
/// reads agree in both directions.
#[test]
fn vram_alias_slots_repeat_the_nametable_slots() {
    let mut memory = CartridgeMemory::from_rom(&[0; 0x8000], &[], None, Mirroring::Horizontal);
    for (base, alias, value) in [
        (0x2005u16, 0x3005u16, 0x11u8),
        (0x2405, 0x3405, 0x22),
        (0x2805, 0x3805, 0x33),
        (0x2c05, 0x3c05, 0x44),
    ] {
        memory.write_vram(base, value);
        assert_eq!(memory.read_vram(alias), value, "alias of {base:#06x}");
        memory.write_vram(alias, !value);
        assert_eq!(memory.read_vram(base), !value, "write through {alias:#06x}");
    }
}

#[test]
fn create_cartridge_mapper0() {
    let rom = create_test_nes(0, 1, 1);
    let file = INesFile::new(rom).unwrap();
    let cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    let val = cartridge.read(0x6000);
    assert_eq!(val, 0);
    assert_eq!(cartridge.read_vram(0), 0);
}

#[test]
fn create_cartridge_mapper1() {
    let rom = create_test_nes(1, 2, 1);
    let file = INesFile::new(rom).unwrap();
    let cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    let val = cartridge.read(0x8000);
    assert_eq!(val, 0);
    assert_eq!(cartridge.read_vram(0), 0);
}

#[test]
fn create_cartridge_mapper2() {
    let rom = create_test_nes(2, 2, 1);
    let file = INesFile::new(rom).unwrap();
    let cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    let val = cartridge.read(0x8000);
    assert_eq!(val, 0);
    assert_eq!(cartridge.read_vram(0), 0);
}

#[test]
fn create_cartridge_mapper3() {
    let rom = create_test_nes(3, 2, 2);
    let file = INesFile::new(rom).unwrap();
    let cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    let val = cartridge.read(0x8000);
    assert_eq!(val, 0);
    assert_eq!(cartridge.read_vram(0), 0);
}

#[test]
fn create_cartridge_mapper4() {
    let rom = create_test_nes(4, 4, 1);
    let file = INesFile::new(rom).unwrap();
    let cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    let val = cartridge.read(0x8000);
    assert_eq!(val, 0);
    assert_eq!(cartridge.read_vram(0), 0);
}

#[test]
fn create_cartridge_mapper7() {
    let rom = create_test_nes(7, 4, 0);
    let file = INesFile::new(rom).unwrap();
    let mut cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    assert_eq!(cartridge.read(0x8000), 0);
    assert_eq!(cartridge.read_vram(0x0010), 0);
    cartridge.write_vram(0x0010, 0xab);
    assert_eq!(cartridge.read_vram(0x0010), 0xab);
}

#[test]
fn create_cartridge_mapper34_bnrom() {
    let rom = create_test_nes(34, 4, 0);
    let file = INesFile::new(rom).unwrap();
    let mut cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    assert_eq!(cartridge.read(0x8000), 0);
    assert_eq!(cartridge.read_vram(0x0010), 0);
    cartridge.write_vram(0x0010, 0xab);
    assert_eq!(cartridge.read_vram(0x0010), 0xab);
}

#[test]
fn create_cartridge_mapper4_with_chr_ram() {
    let rom = create_test_nes(4, 4, 0);
    let file = INesFile::new(rom).unwrap();
    let mut cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    assert_eq!(cartridge.read_vram(0x0010), 0);
    cartridge.write_vram(0x0010, 0xab);
    assert_eq!(cartridge.read_vram(0x0010), 0xab);
}

#[test]
#[should_panic(expected = "Unsupported cartridge mapper no")]
fn create_cartridge_unsupported_mapper() {
    let rom = create_test_nes(99, 1, 1);
    let file = INesFile::new(rom).unwrap();
    let _cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);
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
fn mmc3_irq_clock(cartridge: &mut Cartridge) {
    cartridge.notify_vram_address(0x0000);
    for _ in 0..=MMC3_A12_LOW_FILTER_TICKS {
        cartridge.on_ppu_tick(0);
    }
    cartridge.notify_vram_address(0x1000);
}

/// Arm latch=1 with a pending reload, enable IRQ, clock twice: the counter
/// lands on 0 via decrement and the IRQ asserts on both revisions.
fn mmc3_arm_counter_one_and_reach_zero(cartridge: &mut Cartridge) {
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
fn mmc3_zero_latch_reload(cartridge: &mut Cartridge) {
    cartridge.write(0xc000, 0, SystemClock::default());
    mmc3_irq_clock(cartridge);
}

#[test]
fn mmc3_override_rev_a_skips_irq_on_reload_after_natural_zero() {
    let rom = create_test_nes(4, 2, 1);
    let file = INesFile::new(rom).unwrap();
    let mut cartridge =
        create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::ForceAlternate);

    mmc3_arm_counter_one_and_reach_zero(&mut cartridge);
    mmc3_zero_latch_reload(&mut cartridge);
    assert!(!cartridge.irq_pending());
}

#[test]
fn mmc3_override_standard_asserts_irq_every_clock_with_zero_latch() {
    let rom = create_test_nes(4, 2, 1);
    let file = INesFile::new(rom).unwrap();
    let mut cartridge =
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
    let mut cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    mmc3_arm_counter_one_and_reach_zero(&mut cartridge);
    mmc3_zero_latch_reload(&mut cartridge);
    assert!(!cartridge.irq_pending());
}

#[test]
fn nes20_submapper_0_defaults_to_standard() {
    let file = INesFile::new(nes20_mapper4_rom(0)).unwrap();
    assert_eq!(Some(0), file.header().submapper_no);
    let mut cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);

    mmc3_arm_counter_one_and_reach_zero(&mut cartridge);
    mmc3_zero_latch_reload(&mut cartridge);
    assert!(cartridge.irq_pending());
}

/// The capability snapshot is the only gate on a mapper's PPU hooks: a board
/// that declares none receives no call, even though `TestMapper` implements
/// `notify_vram_address` unconditionally.
#[test]
fn declared_capabilities_gate_the_mapper_hooks() {
    let mapper = TestMapper::new();
    let notified = mapper.notified.clone();
    let mut cartridge = Cartridge::new(Box::new(mapper), CartridgeMemory::test(&[], &[]));

    assert_eq!(CartridgeCaps::default(), cartridge.caps);
    cartridge.notify_vram_address(0x1000);
    cartridge.on_ppu_tick(0);
    assert!(notified.borrow().is_empty());
    assert!(!cartridge.irq_pending());

    // The same board declaring the notification does receive it.
    let (mut cartridge, notified) = Cartridge::test_recording();
    cartridge.notify_vram_address(0x1000);
    assert_eq!(vec![0x1000], *notified.borrow());
}

const NO_CAPS: CartridgeCaps = CartridgeCaps {
    on_ppu_tick: false,
    notify_vram_address: false,
    irq_pending: false,
};
const SCANLINE_AND_IRQ: CartridgeCaps = CartridgeCaps {
    on_ppu_tick: true,
    notify_vram_address: false,
    irq_pending: true,
};
const ALL_CAPS: CartridgeCaps = CartridgeCaps {
    on_ppu_tick: true,
    notify_vram_address: true,
    irq_pending: true,
};

/// The declaration table: what each board tells the cartridge it implements.
/// This is the table the machine dispatches on, not a behaviour test: a new
/// mapper adds a row, and a new override in an existing mapper must set the
/// matching flag here, or `just passed` is the only thing that would notice.
#[test_case(0, NO_CAPS ; "NROM")]
#[test_case(1, NO_CAPS ; "MMC1")]
#[test_case(2, NO_CAPS ; "UxROM")]
#[test_case(3, NO_CAPS ; "CNROM")]
#[test_case(4, ALL_CAPS ; "MMC3")]
#[test_case(7, NO_CAPS ; "AxROM")]
#[test_case(34, NO_CAPS ; "BNROM, 8 KB CHR")]
#[test_case(87, NO_CAPS ; "J87")]
#[test_case(21, SCANLINE_AND_IRQ ; "VRC4a")]
#[test_case(22, SCANLINE_AND_IRQ ; "VRC2a")]
#[test_case(23, SCANLINE_AND_IRQ ; "VRC4f")]
#[test_case(25, SCANLINE_AND_IRQ ; "VRC4b")]
fn mapper_declares_its_ppu_capabilities(mapper_no: u8, expected: CartridgeCaps) {
    let file = INesFile::new(create_test_nes(mapper_no, 2, 1)).unwrap();
    let cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);
    assert_eq!(expected, cartridge.caps, "mapper {}", mapper_no);
}
