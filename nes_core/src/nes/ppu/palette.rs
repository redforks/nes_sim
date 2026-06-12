#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pixel(pub [u8; 4]);

impl Pixel {
    pub const fn new(r: u8, g: u8, b: u8) -> Self {
        Self([r, g, b, 0xff])
    }

    pub fn to_u32(self) -> u32 {
        u32::from_le_bytes(self.0)
    }

    pub fn to_rgb(self) -> (u8, u8, u8) {
        (self.0[0], self.0[1], self.0[2])
    }
}

/// Memory layout of .pal file, contains 64 pixels, each pixes has three bytes (r, g, b)
pub type PalBuf = [u8; 192];

pub struct ColorTheme([Pixel; 64]);

impl Default for ColorTheme {
    fn default() -> Self {
        Self::load_from_pal_buf(include_bytes!("./default.pal"))
    }
}

impl ColorTheme {
    pub fn load_from_pal_buf(buf: &PalBuf) -> Self {
        let mut pixels = [Pixel::new(0, 0, 0); 64];
        for (i, chunk) in buf.chunks_exact(3).enumerate() {
            pixels[i] = Pixel::new(chunk[0], chunk[1], chunk[2]);
        }
        Self(pixels)
    }

    pub fn color(&self, index: u8) -> Pixel {
        self.0[(index as usize) & 0x3F]
    }
}

#[derive(Default)]
pub struct Palette {
    pub data: [u8; 0x20],
}

const fn addr_to_index(addr: u16) -> u16 {
    let addr = addr & 0x1f;
    if matches!(addr, 0x10 | 0x14 | 0x18 | 0x1c) {
        addr & 0xf
    } else {
        addr
    }
}

impl Palette {
    pub fn read(&self, address: u16) -> u8 {
        self.data[addr_to_index(address) as usize]
    }

    pub fn write(&mut self, address: u16, value: u8) {
        self.data[addr_to_index(address) as usize] = value;
    }

    pub fn disabled_color_index(&self, address: u16) -> u8 {
        if (address & 0xff00) == 0x3f00 {
            self.read(address)
        } else {
            self.data[0]
        }
    }

    fn get_color_index(&self, start: u8, palette_idx: u8, idx: u8) -> u8 {
        let offset = if idx == 0 {
            0
        } else {
            (start | idx | (palette_idx << 2)) as usize
        };
        self.data[offset & 0x1f]
    }

    pub fn get_background_color_index(&self, palette_idx: u8, idx: u8) -> u8 {
        self.get_color_index(0, palette_idx, idx)
    }

    pub fn get_sprite_color_index(&self, palette_idx: u8, idx: u8) -> u8 {
        self.get_color_index(0x10, palette_idx, idx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test]
    fn palette_ram_read_write() {
        let mut p = Palette::default();
        for i in 0..0x20 {
            p.write(0x3f00 + i, i as u8);
            assert_eq!(i as u8, p.read(0x3f00 + i));
        }
        for i in 0..0x20 {
            if i % 4 != 0 {
                // exclude mirrored address
                assert_eq!(i as u8, p.read(0x3f00 + i));
            }
        }
    }

    #[test]
    fn palette_ram_get_color() {
        let mut p = Palette::default();
        p.write(0x3f00, 15);
        p.write(0x3f01, 16);
        p.write(0x3f02, 17);
        p.write(0x3f03, 18);
        p.write(0x3f05, 19);
        assert_eq!(15, p.get_background_color_index(0, 0));
        assert_eq!(16, p.get_background_color_index(0, 1));
        assert_eq!(17, p.get_background_color_index(0, 2));
        assert_eq!(18, p.get_background_color_index(0, 3));
        assert_eq!(19, p.get_background_color_index(1, 1));
    }

    #[test_case(0x3f00 => 0)]
    #[test_case(0x3f11 => 0x11)]
    #[test_case(0x3f1f => 0x1f)]
    fn test_addr_to_index(addr: u16) -> u16 {
        addr_to_index(addr)
    }

    #[test]
    fn load_from_pal_buf_all_black() {
        let buf = [0u8; 192];
        let theme = ColorTheme::load_from_pal_buf(&buf);
        for pixel in theme.0.iter() {
            assert_eq!(*pixel, Pixel::new(0, 0, 0));
        }
    }

    #[test]
    fn load_from_pal_buf_specific_colors() {
        let mut buf = [0u8; 192];
        buf[0..3].copy_from_slice(&[255, 0, 0]);
        buf[3..6].copy_from_slice(&[0, 255, 0]);
        buf[6..9].copy_from_slice(&[0, 0, 255]);
        let theme = ColorTheme::load_from_pal_buf(&buf);
        assert_eq!(theme.0[0], Pixel::new(255, 0, 0));
        assert_eq!(theme.0[1], Pixel::new(0, 255, 0));
        assert_eq!(theme.0[2], Pixel::new(0, 0, 255));
    }

    #[test_case(0x3f00, 0x3f10, 0)]
    #[test_case(0x3f04, 0x3f14, 0x04)]
    #[test_case(0x3f08, 0x3f18, 0x08)]
    #[test_case(0x3f0c, 0x3f1c, 0x0c)]
    fn background_sprite_shares_transparent(back_addr: u16, sprite_addr: u16, exp_index: u16) {
        let back = addr_to_index(back_addr);
        let sprite = addr_to_index(sprite_addr);
        assert_eq!(back, exp_index);
        assert_eq!(sprite, exp_index);
    }
}
