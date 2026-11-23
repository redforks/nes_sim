mod convert;
mod info;
mod run;

pub use convert::ConvertAction;
pub use info::InfoAction;
pub use run::RunAction;

use nes_core::{ines::INesFile, nes::ppu::palette::ColorTheme};
use std::{fs::File, io::Read, path::Path};

/// Read an iNES file from disk.
pub fn read_ines_file(path: &Path) -> Result<INesFile, anyhow::Error> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(INesFile::new(buffer)?)
}

/// Load a 192-byte `.pal` palette, when one was given.
pub fn load_palette(path: Option<&Path>) -> Result<Option<ColorTheme>, anyhow::Error> {
    let Some(path) = path else {
        return Ok(None);
    };
    let mut buf = [0u8; 192];
    let mut file = File::open(path)?;
    file.read_exact(&mut buf)?;
    Ok(Some(ColorTheme::load_from_pal_buf(&buf)))
}
