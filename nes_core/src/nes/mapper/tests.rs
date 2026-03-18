use super::*;
use crate::ines::INesFile;

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

#[test]
fn create_cartridge_mapper0() {
    let rom = create_test_nes(0, 1, 1);
    let file = INesFile::new(rom).unwrap();
    let mut cartridge = create_cartridge(&file);

    // Test that we can read from cartridge
    let val = cartridge.read(CARTRIDGE_START_ADDR);
    assert_eq!(val, 0);

    // Test pattern reference
    assert_eq!(cartridge.pattern_ref().len(), 8 * 1024);
}

#[test]
fn create_cartridge_mapper1() {
    let rom = create_test_nes(1, 2, 1);
    let file = INesFile::new(rom).unwrap();
    let mut cartridge = create_cartridge(&file);

    // Test that we can read from cartridge
    let val = cartridge.read(0x8000);
    assert_eq!(val, 0);

    // Test pattern reference
    assert_eq!(cartridge.pattern_ref().len(), 8 * 1024);
}

#[test]
#[should_panic(expected = "Unsupported cartridge mapper no")]
fn create_cartridge_unsupported_mapper() {
    let rom = create_test_nes(99, 1, 1);
    let file = INesFile::new(rom).unwrap();
    // This should panic
    let _cartridge = create_cartridge(&file);
}

#[test]
fn create_cartridge_different_sizes() {
    // Test with 2 PRG pages (Mapper0 supports up to 32KB PRG ROM)
    let rom = create_test_nes(0, 2, 1);
    let file = INesFile::new(rom).unwrap();
    let cartridge = create_cartridge(&file);

    // Mapper0 has fixed 8KB CHR ROM regardless of input size
    assert_eq!(cartridge.pattern_ref().len(), 8 * 1024);
}
