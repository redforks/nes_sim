use super::*;
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
    // The bits parser reads MSB-first, so we place mapper_low in the high nibble
    let control1 = (mapper & 0x0f) << 4; // mapper in bits 7-4, flags all 0
    rom.push(control1);

    // Control byte 2: MSB 4 bits are mapper_high (bits 7-4), LSB 4 bits are unused/reserved
    let control2 = ((mapper & 0xf0) >> 4) << 4; // mapper in bits 7-4
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
    let (mut cartridge, _mirroring) = create_cartridge(&file);

    // Test that we can read from cartridge
    let val = cartridge.read(CARTRIDGE_START_ADDR);
    assert_eq!(val, 0);
}

#[test]
fn create_cartridge_mapper1() {
    let rom = create_test_nes(1, 2, 1);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) = create_cartridge(&file);

    // Test that we can read from cartridge
    let val = cartridge.read(0x8000);
    assert_eq!(val, 0);
}

#[test]
fn create_cartridge_mapper1_with_chr_ram() {
    let rom = create_test_nes(1, 2, 0);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) = create_cartridge(&file);

    cartridge.write_pattern(0x0010, 0xab);

    assert_eq!(cartridge.read_pattern(0x0010), 0xab);
}

#[test]
fn create_cartridge_mapper2() {
    let rom = create_test_nes(2, 2, 1);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) = create_cartridge(&file);

    let val = cartridge.read(0x8000);
    assert_eq!(val, 0);
}

#[test]
fn create_cartridge_mapper2_with_chr_ram() {
    let rom = create_test_nes(2, 2, 0);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) = create_cartridge(&file);

    cartridge.write_pattern(0x0010, 0xab);

    assert_eq!(cartridge.read_pattern(0x0010), 0xab);
}

#[test]
fn create_cartridge_mapper3() {
    let rom = create_test_nes(3, 2, 2);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) = create_cartridge(&file);

    let val = cartridge.read(0x8000);
    assert_eq!(val, 0);
}

#[test]
fn create_cartridge_mapper4() {
    let rom = create_test_nes(4, 4, 1);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) = create_cartridge(&file);

    let val = cartridge.read(0x8000);
    assert_eq!(val, 0);
}

#[test]
fn create_cartridge_mapper7() {
    let rom = create_test_nes(7, 4, 0);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) = create_cartridge(&file);

    cartridge.write_pattern(0x0010, 0xab);

    assert_eq!(cartridge.read(0x8000), 0);
    assert_eq!(cartridge.read_pattern(0x0010), 0xab);
}

#[test]
fn create_cartridge_mapper34_bnrom() {
    let rom = create_test_nes(34, 4, 0);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) = create_cartridge(&file);

    cartridge.write_pattern(0x0010, 0xab);

    assert_eq!(cartridge.read(0x8000), 0);
    assert_eq!(cartridge.read_pattern(0x0010), 0xab);
}

#[test]
fn create_cartridge_mapper4_with_chr_ram() {
    let rom = create_test_nes(4, 4, 0);
    let file = INesFile::new(rom).unwrap();
    let (mut cartridge, _mirroring) = create_cartridge(&file);

    cartridge.write_pattern(0x0010, 0xab);

    assert_eq!(cartridge.read_pattern(0x0010), 0xab);
}

#[test]
#[should_panic(expected = "Unsupported cartridge mapper no")]
fn create_cartridge_unsupported_mapper() {
    let rom = create_test_nes(99, 1, 1);
    let file = INesFile::new(rom).unwrap();
    // This should panic
    let (_cartridge, _mirroring) = create_cartridge(&file);
}
