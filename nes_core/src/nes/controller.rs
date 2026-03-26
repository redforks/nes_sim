use crate::mcu::{DefinedRegion, Mcu};

/// nes Controller struct that represent a controller of nes.
pub struct AController {
    mask: u8,
    bits: u8,
}

#[allow(clippy::new_without_default)]
impl AController {
    pub fn new() -> AController {
        AController { mask: 1, bits: 0 }
    }

    fn reset_for_read(&mut self) {
        self.mask = 1;
    }

    fn read(&mut self) -> u8 {
        let r = self.bits & self.mask;
        self.mask = self.mask.rotate_left(1);

        if r != 0 {
            0x40
        } else {
            0x41
        }
    }

    pub fn press(&mut self, btn: Button) {
        self.bits |= btn as u8;
    }

    pub fn release(&mut self, btn: Button) {
        self.bits &= !(btn as u8);
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

pub struct Controller {
    /// The first controller.
    pub a: AController,
    /// The second controller.
    pub b: AController,
}

#[allow(clippy::new_without_default)]
impl Controller {
    pub fn new() -> Controller {
        Controller {
            a: AController::new(),
            b: AController::new(),
        }
    }
}

impl Mcu for Controller {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x4016 => self.a.read(),
            0x4017 => self.b.read(),
            _ => panic!("read address out of range: {:04x}", address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x4016 => {
                if value & 1 == 0 {
                    self.a.reset_for_read();
                    self.b.reset_for_read();
                }
            }
            0x4017 => {}
            _ => panic!("write address out of range: {:04x}", address),
        }
    }
}

impl DefinedRegion for Controller {
    fn region(&self) -> (u16, u16) {
        (0x4016, 0x4017)
    }
}

#[cfg(test)]
mod tests;
