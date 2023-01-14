pub struct INesFile {
    image: Vec<u8>,
    prg_size: u8,
}

impl INesFile {
    pub fn new(image: Vec<u8>) -> Result<Self, FormatError> {
        if image.len() < 16 || image[0..=3] != [0x4e, 0x45, 0x53, 0x1a] {
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
}

#[derive(Debug, thiserror::Error)]
pub enum FormatError {
    #[error("invalid iNES header")]
    InvalidHeader,
}
