mod parser;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FileVersion {
    INes,
    Nes20,
}

#[derive(Debug)]
pub struct Header {
    /// Each page is 16k
    pub n_prg_pages: u8,
    /// Each page is 8k
    pub n_chr_pages: u8,

    pub prg_rom_size: usize,
    pub chr_rom_size: usize,

    pub mapper_no: u16,
    pub submapper_no: Option<u8>,
    pub ignore_mirror_control: bool,
    pub has_trainer: bool,
    pub battery_backed_ram: bool,
    pub ver_or_hor_arrangement: bool,
    pub console_type: u8,
    pub prg_ram_size: Option<usize>,
    pub prg_nvram_size: Option<usize>,
    pub chr_ram_size: Option<usize>,
    pub chr_nvram_size: Option<usize>,
    pub timing_mode: Option<u8>,
    pub vs_ppu_type: Option<u8>,
    pub vs_hardware_type: Option<u8>,
    pub extended_console_type: Option<u8>,
    pub misc_rom_count: Option<u8>,
    pub default_expansion_device: Option<u8>,
}

#[derive(Debug)]
pub struct INesFile {
    file_version: FileVersion,
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

    pub fn file_version(&self) -> FileVersion {
        self.file_version
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
mod tests;
