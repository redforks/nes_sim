mod parser;

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

pub struct INesFile {
    header: Header,
    prg_ram: Vec<u8>,
    chr_ram: Vec<u8>,
}

impl INesFile {
    pub fn new(image: Vec<u8>) -> Result<Self, FormatError> {
        parser::parse_file(&image).map_err(|_| FormatError::InvalidHeader)
    }

    pub fn header(&self) -> &Header {
        &self.header
    }

    pub fn read_chr(&self) -> &[u8] {
        &self.chr_ram
    }

    pub fn read_prg(&self) -> &[u8] {
        &self.prg_ram
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
