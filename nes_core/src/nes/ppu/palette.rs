use image::Rgba;

pub type Pixel = Rgba<u8>;

const fn rgb(v: [u8; 3]) -> Pixel {
    let [r, g, b] = v;
    Rgba::<u8>([r, g, b, 0xff])
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
pub fn color(index: usize) -> Pixel {
    COLORS[index & 0x3F]
}

#[derive(Default)]
pub struct Palette {
    pub data: [u8; 0x20],
}

impl Palette {
    pub fn render_disabled_color(&self, address: u16) -> Pixel {
        if (0x3f00..0x4000).contains(&address) {
            color(self.read(address) as usize)
        } else {
            color(self.read(0x3f00) as usize)
        }
    }

    pub fn read(&self, address: u16) -> u8 {
        self.data[Self::index(address)]
    }

    pub fn write(&mut self, address: u16, value: u8) {
        self.data[Self::index(address)] = value;
    }

    fn index(addr: u16) -> usize {
        let addr = addr & 0x1f;
        if let 0x10 | 0x14 | 0x18 | 0x1C = addr {
            (addr & 0xf) as usize
        } else {
            addr as usize
        }
    }

    pub fn black_color(&self) -> Pixel {
        color(self.data[0] as usize)
    }

    fn get_color(&self, start: u8, palette_idx: u8, idx: u8) -> Pixel {
        let offset = if idx == 0 {
            0
        } else {
            (start | idx | (palette_idx << 2)) as usize
        };
        COLORS[self.data[offset] as usize]
    }

    pub fn get_background_color(&self, palette_idx: u8, idx: u8) -> Pixel {
        self.get_color(0, palette_idx, idx)
    }

    pub fn get_sprit_color(&self, palette_idx: u8, idx: u8) -> Pixel {
        self.get_color(0x10, palette_idx, idx)
    }
}
