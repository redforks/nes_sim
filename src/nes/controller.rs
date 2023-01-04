// nes Controller struct
pub struct Controller {
    mask: u8,
    bits: u8,
}

impl Controller {
    pub fn new() -> Controller {
        Controller { mask: 1, bits: 0 }
    }

    pub fn reset_for_read(&mut self) {
        self.mask = 1;
    }

    pub fn read(&mut self) -> u8 {
        let r = self.bits & self.mask;
        self.mask = self.mask.rotate_left(1);

        if r != 0 {
            0x41
        } else {
            0x40
        }
    }

    pub fn press(&mut self, btn: Button) {
        self.bits |= btn as u8;
    }

    pub fn release(&mut self, btn: Button) {
        self.bits &= !(btn as u8);
    }
}

pub enum Button {
    A = 1,
    B = 0x2,
    Select = 0x4,
    Start = 0x8,
    Up = 0x10,
    Down = 0x20,
    Left = 0x40,
    Right = 0x80,
}
