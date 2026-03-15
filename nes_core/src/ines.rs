mod parser;

#[derive(Debug)]
pub struct Header {
    /// Each page is 16k
    pub n_prg_pages: u8,
    /// Each page is 8k
    pub n_chr_pages: u8,

    pub mapper_no: u8,
    pub ignore_mirror_control: bool,
    pub has_trainer: bool,
    pub battery_backed_ram: bool,
    pub ver_or_hor_arrangement: bool,
}

#[derive(Debug)]
pub struct INesFile {
    header: Header,
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
}

impl INesFile {
    pub fn new(image: Vec<u8>) -> Result<Self, FormatError> {
        parser::parse_file(&image).map_err(|_| FormatError::InvalidHeader)
    }

    pub fn header(&self) -> &Header {
        &self.header
    }

    pub fn read_chr_rom(&self) -> &[u8] {
        &self.chr_rom
    }

    pub fn read_prg_rom(&self) -> &[u8] {
        &self.prg_rom
    }

    pub fn is_valid(image: &[u8]) -> bool {
        parser::check_signature(image)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum FormatError {
    #[error("invalid iNES header")]
    InvalidHeader,
}

#[cfg(test)]
mod tests {
    use super::*;

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
            0x4e, 0x45, 0x53, 0x1a, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
            0x01, 0x01,
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
}
