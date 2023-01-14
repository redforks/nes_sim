use crate::mcu::{DefinedRegion, Mcu};
use std::cell::RefCell;

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
    pub a: RefCell<AController>,
    /// The second controller.
    pub b: RefCell<AController>,
}

#[allow(clippy::new_without_default)]
impl Controller {
    pub fn new() -> Controller {
        Controller {
            a: RefCell::new(AController::new()),
            b: RefCell::new(AController::new()),
        }
    }
}

impl Mcu for Controller {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x4016 => self.a.borrow_mut().read(),
            0x4017 => self.b.borrow_mut().read(),
            _ => panic!("read address out of range: {:04x}", address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x4016 => {
                if value & 1 == 0 {
                    self.a.borrow_mut().reset_for_read();
                    self.b.borrow_mut().reset_for_read();
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
mod tests {
    use super::*;

    #[test]
    fn test_a_controller() {
        let mut a = AController::new();
        a.press(Button::A);
        a.press(Button::Left);
        a.reset_for_read();
        assert_eq!(a.read(), 0x40); // A
        assert_eq!(a.read(), 0x41); // B
        assert_eq!(a.read(), 0x41); // Select
        assert_eq!(a.read(), 0x41); // Start
        assert_eq!(a.read(), 0x41); // Up
        assert_eq!(a.read(), 0x41); // Down
        assert_eq!(a.read(), 0x40); // Left
        assert_eq!(a.read(), 0x41); // Right

        a.release(Button::A);
        a.press(Button::B);
        a.reset_for_read();
        assert_eq!(a.read(), 0x41); // A
        assert_eq!(a.read(), 0x40); // B
        assert_eq!(a.read(), 0x41); // Select
        assert_eq!(a.read(), 0x41); // Start
        assert_eq!(a.read(), 0x41); // Up
        assert_eq!(a.read(), 0x41); // Down
        assert_eq!(a.read(), 0x40); // Left
        assert_eq!(a.read(), 0x41); // Right
    }
}
