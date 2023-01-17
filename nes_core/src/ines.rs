mod parser;

pub struct INesFile {
    image: Vec<u8>,
    prg_size: u8,
}

impl INesFile {
    pub fn new(image: Vec<u8>) -> Result<Self, FormatError> {
        if !Self::is_valid(&image) {
            return Err(FormatError::InvalidHeader);
        }

        Ok(Self {
            prg_size: image[4],
            image,
        })
    }

    pub fn read_prg(&self) -> &[u8] {
        let start = 16;
        let end = start + (self.prg_size as usize) * 16 * 1024;
        &self.image[start..end]
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
