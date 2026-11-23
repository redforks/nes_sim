use anyhow::{Context as _, Result};
use nes_core::{ines::INesFile, mesen, movie};
use std::{fs, path::PathBuf};

#[derive(clap::Args)]
pub struct ConvertAction {
    /// Input FCEUX .fm2 movie
    input: PathBuf,
    /// Output Mesen movie [default: the input path with a .mmo extension]
    #[arg(short, long)]
    output: Option<PathBuf>,
    /// ROM the movie was recorded against; supplies GameFile and SHA1 metadata
    #[arg(long)]
    rom: Option<PathBuf>,
}

impl ConvertAction {
    pub fn run(&self) -> Result<()> {
        let fm2_bytes =
            fs::read(&self.input).with_context(|| format!("reading {}", self.input.display()))?;
        let fm2 = movie::parse(&fm2_bytes)
            .with_context(|| format!("parsing {}", self.input.display()))?;

        let rom = self
            .rom
            .as_ref()
            .map(|path| fs::read(path).with_context(|| format!("reading {}", path.display())))
            .transpose()?;
        let rom_name = self
            .rom
            .as_ref()
            .and_then(|path| path.file_name())
            .and_then(|name| name.to_str());

        if let (Some(path), Some(bytes)) = (&self.rom, &rom) {
            warn_if_rom_mismatch(path, bytes, &fm2);
        }

        let output = self
            .output
            .clone()
            .unwrap_or_else(|| self.input.with_extension("mmo"));
        let bytes = mesen::from_fm2(&fm2, rom.as_deref(), rom_name)?;
        fs::write(&output, bytes).with_context(|| format!("writing {}", output.display()))?;

        println!(
            "Converted {} frame(s) to {}",
            fm2.input_logs.len(),
            output.display()
        );
        Ok(())
    }
}

/// Warn when the supplied ROM does not match the FM2's `romChecksum`.
fn warn_if_rom_mismatch(path: &std::path::Path, bytes: &[u8], fm2: &movie::Fm2File) {
    let Ok(ines) = INesFile::new(bytes.to_vec()) else {
        eprintln!("warning: {} is not a valid iNES file", path.display());
        return;
    };
    match fm2.valid_rom_file(&ines) {
        Ok(true) => {}
        Ok(false) => eprintln!(
            "warning: {} does not match the ROM checksum in the FM2 file",
            path.display()
        ),
        Err(error) => eprintln!("warning: cannot verify the FM2 ROM checksum: {error}"),
    }
}
