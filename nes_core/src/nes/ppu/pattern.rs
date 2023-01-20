use image::{Rgba, RgbaImage};

/// slice must be 16 bytes long
#[derive(Copy, Clone)]
pub struct Tile<'a>(&'a [u8]);

impl<'a> Tile<'a> {
    /// Get pixel color at (x, y). x and y must be in range [0, 7].
    pub fn pixel(&self, x: usize, y: usize) -> u8 {
        let low = self.0[y];
        let high = self.0[y + 8];
        let bit = 7 - x;
        ((low >> bit) & 1) | (((high >> bit) & 1) << 1)
    }

    pub fn iter(&self) -> impl Iterator<Item = (usize, usize, u8)> + '_ {
        itertools::iproduct!(0..8, 0..8).map(move |(x, y)| (x, y, self.pixel(x, y)))
    }
}

#[derive(Copy, Clone)]
pub struct Pattern<'a>(&'a [u8]); // slice must be 16 * 256 (4096) bytes long

impl<'a> Pattern<'a> {
    /// Get tile at index. Index is 0-255.
    pub fn tile(&self, index: usize) -> Tile<'a> {
        let offset = index * 16;
        Tile(&self.0[offset..offset + 16])
    }

    pub fn iter(&self) -> impl Iterator<Item = Tile> + '_ {
        (0..256).map(move |i| self.tile(i))
    }
}

#[derive(Copy, Clone)]
pub struct PatternBand<'a>(&'a [u8]);

impl<'a> PatternBand<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self(data)
    }

    pub fn pattern(&self, index: usize) -> Pattern<'a> {
        let offset = index * 4096;
        Pattern(&self.0[offset..offset + 4096])
    }

    pub fn iter(&self) -> impl Iterator<Item = Pattern> + '_ {
        let n = self.0.len() / 4096;
        (0..n).map(move |i| self.pattern(i))
    }
}

const PALLET: [Rgba<u8>; 4] = [
    Rgba([0xff, 0xff, 0xff, 0xff]),
    Rgba([0x7f, 0x0, 0x0, 0xff]),
    Rgba([0x0, 0x00, 0x7f, 0xff]),
    Rgba([0x0, 0x7f, 0x0, 0xff]),
];
pub fn draw_pattern(band: &PatternBand<'_>) -> RgbaImage {
    let total_bands = band.iter().count() as u32;

    let mut img: RgbaImage =
        RgbaImage::new(128, (128 * total_bands + 32 * (total_bands - 1)).max(0));
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
    img
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tile() {
        #[rustfmt::skip]
        let tile = Tile(&[
            0b01000001,
            0b11000010,
            0b00000000,
            0b00000000,
            0b00000000,
            0b00000000,
            0b00000000,
            0b00000000,

            0b00000001,
            0b00000010,
            0b00000000,
            0b00000000,
            0b00000000,
            0b00000000,
            0b00000000,
            0b00000001,
        ]);
        assert_eq!(tile.pixel(0, 0), 0);
        assert_eq!(tile.pixel(1, 0), 1);
        assert_eq!(tile.pixel(7, 0), 3);
        assert_eq!(tile.pixel(0, 1), 1);
        assert_eq!(tile.pixel(7, 7), 2);

        let mut iter = tile.iter();
        assert_eq!(iter.next(), Some((0, 0, 0)));
        assert_eq!(iter.next(), Some((0, 1, 1)));
        assert_eq!(iter.last(), Some((7, 7, 2)));
    }
}
