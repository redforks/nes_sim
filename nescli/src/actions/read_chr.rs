use image::{ImageOutputFormat, Rgba, RgbaImage};
use nes_core::ines::INesFile;
use nes_core::nes::ppu::PatternBand;
use std::io::{stdout, Cursor, Write};

#[derive(clap::Args)]
pub struct ReadChrAction {}

const PALLET: [Rgba<u8>; 4] = [
    Rgba([0xff, 0xff, 0xff, 0xff]),
    Rgba([0x7f, 0x0, 0x0, 0xff]),
    Rgba([0x0, 0x00, 0x7f, 0xff]),
    Rgba([0x0, 0x7f, 0x0, 0xff]),
];

impl ReadChrAction {
    pub fn run(&self, f: &INesFile) -> Result<(), anyhow::Error> {
        let band = PatternBand::new(f.read_chr_rom());
        let total_bands = band.iter().count() as u32;
        if total_bands == 0 {
            // print to stderr and exit
            eprintln!("No CHR ROM found");
            return Ok(());
        }

        let mut img: RgbaImage = RgbaImage::new(128, 128 * total_bands + 32 * (total_bands - 1));
        for (band_idx, pattern) in band.iter().enumerate() {
            let start_y = band_idx * (128 + 32);
            for (tile_idx, tile) in pattern.iter().enumerate() {
                let tile_x = tile_idx % 16;
                let tile_y = tile_idx / 16;
                for (x, y, pixel) in tile.iter() {
                    let image_x = (tile_x * 8 + x) as u32;
                    let image_y = (start_y + tile_y * 8 + y) as u32;
                    img.put_pixel(image_x, image_y, PALLET[pixel as usize]);
                }
            }
        }

        let mut buf = Cursor::new(Vec::new());
        img.write_to(&mut buf, ImageOutputFormat::Png)?;
        Ok(stdout().write_all(buf.get_ref())?)
    }
}
