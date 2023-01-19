use image::{ImageOutputFormat, Rgba, RgbaImage};
use nes_core::ines::INesFile;
use nes_core::nes::ppu::{draw_pattern, PatternBand};
use std::io::{stdout, Cursor, Write};

#[derive(clap::Args)]
pub struct ReadChrAction {}

impl ReadChrAction {
    pub fn run(&self, f: &INesFile) -> Result<(), anyhow::Error> {
        let band = PatternBand::new(f.read_chr_rom());
        let total_bands = band.iter().count() as u32;
        if total_bands == 0 {
            // print to stderr and exit
            eprintln!("No CHR ROM found");
            return Ok(());
        }

        let img: RgbaImage = draw_pattern(&band);

        let mut buf = Cursor::new(Vec::new());
        img.write_to(&mut buf, ImageOutputFormat::Png)?;
        Ok(stdout().write_all(buf.get_ref())?)
    }
}
