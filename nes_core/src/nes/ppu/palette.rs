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

const fn rgb(v: [u8; 3]) -> Pixel {
    let [r, g, b] = v;
    Pixel::new(r, g, b)
}

#[rustfmt::skip]
const COLORS: [Pixel; 64] = [
    // 2C02
    rgb([102, 102, 102]), rgb([0, 42, 136]), rgb([20, 18, 167]), rgb([59, 0, 164]),
    rgb([92, 0, 126]), rgb([110, 0, 64]), rgb([108, 6, 0]), rgb([86, 29, 0]),
    rgb([51, 53, 0]), rgb([11, 72, 0]), rgb([0, 82, 0]), rgb([0, 79, 8]),
    rgb([0, 64, 77]), rgb([0, 0, 0]), rgb([0, 0, 0]), rgb([0, 0, 0]),

    rgb([173, 173, 173]), rgb([21, 95, 217]), rgb([66, 64, 255]), rgb([117, 39, 254]),
    rgb([160, 26, 204]), rgb([183, 30, 123]), rgb([181, 49, 32]), rgb([153, 78, 0]),
    rgb([107, 109, 0]), rgb([56, 135, 0]), rgb([12, 147, 0]), rgb([0, 143, 50]),
    rgb([0, 124, 141]), rgb([0, 0, 0]), rgb([0, 0, 0]), rgb([0, 0, 0]),

    rgb([255, 254, 255]), rgb([100, 176, 255]), rgb([146, 144, 255]), rgb([198, 118, 255]),
    rgb([243, 106, 255]), rgb([254, 110, 204]), rgb([254, 129, 112]), rgb([234, 158, 34]),
    rgb([188, 190, 0]), rgb([136, 216, 0]), rgb([92, 228, 48]), rgb([69, 224, 130]),
    rgb([72, 205, 222]), rgb([79, 79, 79]), rgb([0, 0, 0]), rgb([0, 0, 0]),

    rgb([255, 254, 255]), rgb([192, 223, 255]), rgb([211, 210, 255]), rgb([232, 200, 255]),
    rgb([251, 194, 255]), rgb([254, 196, 234]), rgb([254, 204, 197]), rgb([247, 216, 165]),
    rgb([228, 229, 148]), rgb([207, 239, 150]), rgb([189, 244, 171]), rgb([179, 243, 204]),
    rgb([179, 235, 242]), rgb([184, 184, 184]), rgb([0, 0, 0]), rgb([0, 0, 0]),
];

/// Return palette color by index (0..63).
pub fn color(index: u8) -> Pixel {
    COLORS[(index as usize) & 0x3F]
}

/// Memory layout of .pal file, contains 64 pixels, each pixes has three bytes (r, g, b)
pub type PalBuf = [u8; 192];

pub struct ColorTheme([Pixel; 64]);

impl ColorTheme {
    pub fn load_from_pal_buf(buf: &PalBuf) -> Self {
        let mut pixels = [Pixel::new(0, 0, 0); 64];
        for (i, chunk) in buf.chunks_exact(3).enumerate() {
            pixels[i] = Pixel::new(chunk[0], chunk[1], chunk[2]);
        }
        Self(pixels)
    }

    pub fn into_pal_buf(self) -> PalBuf {
        let mut buf = [0u8; 192];
        for (i, pixel) in self.0.iter().enumerate() {
            let (r, g, b) = pixel.to_rgb();
            let offset = i * 3;
            buf[offset] = r;
            buf[offset + 1] = g;
            buf[offset + 2] = b;
        }
        buf
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
    fn load_from_pal_buf_roundtrip() {
        let mut buf = [0u8; 192];
        for i in 0..64u8 {
            let offset = i as usize * 3;
            buf[offset] = i.wrapping_mul(11);
            buf[offset + 1] = i.wrapping_mul(23);
            buf[offset + 2] = i.wrapping_mul(37);
        }
        let theme = ColorTheme::load_from_pal_buf(&buf);
        let out = theme.into_pal_buf();
        assert_eq!(buf, out);
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

    #[test]
    fn into_pal_buf_expected_layout() {
        let pixels = [Pixel::new(0, 0, 0); 64];
        let theme = ColorTheme(pixels);
        let buf = theme.into_pal_buf();
        assert_eq!(buf.len(), 192);
        for (i, chunk) in buf.chunks_exact(3).enumerate() {
            assert_eq!(chunk, &[0, 0, 0], "pixel {i} is not zero");
        }
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

    #[test]
    fn save_default_pal() {
        let theme = ColorTheme(COLORS);
        let buf = theme.into_pal_buf();
        let mut f = std::fs::File::create("/tmp/default.pal").unwrap();
        std::io::Write::write_all(&mut f, &buf).unwrap();
    }
}
