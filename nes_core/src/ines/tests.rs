use super::*;
use crate::ines::NametableArrangement;

fn build_rom(
    header: [u8; 16],
    trainer: Option<Vec<u8>>,
    prg_len: usize,
    chr_len: usize,
) -> Vec<u8> {
    let mut rom = header.to_vec();
    if let Some(trainer) = trainer {
        rom.extend(trainer);
    }
    rom.extend(vec![0u8; prg_len]);
    rom.extend(vec![0u8; chr_len]);
    rom
}

#[test]
fn test_format_error_display() {
    let error = FormatError::InvalidHeader;
    assert_eq!(error.to_string(), "invalid iNES header");
}

#[test]
fn test_ines_file_new_invalid_signature() {
    // Invalid ROM without proper signature
    let invalid_rom = vec![0x00, 0x01, 0x02, 0x03];
    let result = INesFile::new(invalid_rom);
    assert!(result.is_err());
    match result {
        Err(FormatError::InvalidHeader) => {}
        _ => panic!("Expected InvalidHeader error"),
    }
}

#[test]
fn test_ines_file_new_empty() {
    let empty_rom = vec![];
    let result = INesFile::new(empty_rom);
    assert!(result.is_err());
    match result {
        Err(FormatError::InvalidHeader) => {}
        _ => panic!("Expected InvalidHeader error"),
    }
}

#[test]
fn test_ines_file_new_too_short() {
    // Header requires at least 16 bytes
    let short_rom = vec![0x4e, 0x45, 0x53, 0x1a, 0x01, 0x01];
    let result = INesFile::new(short_rom);
    assert!(result.is_err());
}

#[test]
fn test_is_valid_with_valid_rom() {
    let valid_rom = vec![
        0x4e, 0x45, 0x53, 0x1a, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
        0x01,
    ];
    assert!(INesFile::is_valid(&valid_rom));
}

#[test]
fn test_is_valid_with_invalid_rom() {
    let invalid_rom = vec![0x00, 0x01, 0x02, 0x03];
    assert!(!INesFile::is_valid(&invalid_rom));
}

#[test]
fn test_is_valid_with_empty() {
    let empty_rom = vec![];
    assert!(!INesFile::is_valid(&empty_rom));
}

#[test]
fn test_is_valid_with_too_short() {
    let short_rom = vec![0x4e, 0x45];
    assert!(!INesFile::is_valid(&short_rom));
}

#[test]
fn test_ines_header_reports_file_version() {
    let rom = build_rom(
        [
            0x4e, 0x45, 0x53, 0x1a, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00,
        ],
        None,
        16 * 1024,
        8 * 1024,
    );
    let file = INesFile::new(rom).unwrap();

    assert_eq!(FileVersion::INes, file.file_version());
}

#[test]
fn test_nes20_header_fields_are_parsed() {
    let rom = build_rom(
        [
            0x4e,
            0x45,
            0x53,
            0x1a,
            0x02,
            0x03,
            0b1011_1111,
            0b0101_1001,
            0b0111_0001,
            0x00,
            0b1000_0111,
            0b0110_0101,
            0x03,
            0b0100_1010,
            0x02,
            0x21,
        ],
        Some(vec![0u8; 512]),
        2 * 16 * 1024,
        3 * 8 * 1024,
    );
    let file = INesFile::new(rom).unwrap();
    let header = file.header();

    assert_eq!(FileVersion::Nes20, file.file_version());
    assert_eq!(0x15b, header.mapper_no);
    assert_eq!(Some(0x07), header.submapper_no);
    assert_eq!(2, header.n_prg_pages);
    assert_eq!(3, header.n_chr_pages);
    assert_eq!(2 * 16 * 1024, header.prg_rom_size);
    assert_eq!(3 * 8 * 1024, header.chr_rom_size);
    assert!(header.ignore_mirror_control);
    assert!(header.has_trainer);
    assert!(header.battery_backed_ram);
    assert_eq!(
        header.nametable_arrangement,
        NametableArrangement::Horizontal
    );
    assert_eq!(1, header.console_type);
    assert_eq!(Some(8 * 1024), header.prg_ram_size);
    assert_eq!(Some(16 * 1024), header.prg_nvram_size);
    assert_eq!(Some(2 * 1024), header.chr_ram_size);
    assert_eq!(Some(4 * 1024), header.chr_nvram_size);
    assert_eq!(Some(3), header.timing_mode);
    assert_eq!(Some(0x0a), header.vs_ppu_type);
    assert_eq!(Some(0x04), header.vs_hardware_type);
    assert_eq!(None, header.extended_console_type);
    assert_eq!(Some(2), header.misc_rom_count);
    assert_eq!(Some(0x21), header.default_expansion_device);
    assert_eq!(2 * 16 * 1024, file.read_prg_rom().len());
    assert_eq!(3 * 8 * 1024, file.read_chr_rom().len());
}

#[test]
fn test_nes20_extended_console_and_exponent_sizes_are_parsed() {
    let rom = build_rom(
        [
            0x4e,
            0x45,
            0x53,
            0x1a,
            0b0001_1110,
            0b0001_1001,
            0x00,
            0x08 | 0x03,
            0b0010_0001,
            0b1111_1111,
            0x00,
            0x00,
            0x02,
            0x09,
            0x01,
            0x00,
        ],
        None,
        128 * 5,
        64 * 3,
    );
    let file = INesFile::new(rom).unwrap();
    let header = file.header();

    assert_eq!(FileVersion::Nes20, file.file_version());
    assert_eq!(0x100, header.mapper_no);
    assert_eq!(Some(0x02), header.submapper_no);
    assert_eq!(640, header.prg_rom_size);
    assert_eq!(192, header.chr_rom_size);
    assert_eq!(3, header.console_type);
    assert_eq!(Some(2), header.timing_mode);
    assert_eq!(Some(0x09), header.extended_console_type);
    assert_eq!(None, header.vs_ppu_type);
    assert_eq!(None, header.vs_hardware_type);
    assert_eq!(Some(1), header.misc_rom_count);
    assert_eq!(Some(0), header.default_expansion_device);
    assert_eq!(640, file.read_prg_rom().len());
    assert_eq!(192, file.read_chr_rom().len());
}
