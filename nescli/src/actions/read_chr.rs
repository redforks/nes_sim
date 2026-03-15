use image::codecs::png::PngEncoder;
use image::{DynamicImage, ImageEncoder};
use nes_core::ines::INesFile;
use nes_core::nes::ppu::{PatternBand, draw_pattern};
use std::io::{Cursor, Write, stdout};

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

        let img = DynamicImage::from(draw_pattern(&band));

        let mut buf = Cursor::new(Vec::new());
        let encoder = PngEncoder::new(&mut buf);
        encoder.write_image(
            img.as_bytes(),
            img.width(),
            img.height(),
            image::ExtendedColorType::Rgba8,
        )?;
        Ok(stdout().write_all(buf.get_ref())?)
    }
}
