use crate::mcu::Mcu;

/// nes Controller struct that represent a controller of nes.
pub struct AController {
    stroke: bool,
    /// current button state
    bits: u8,
    /// copy of bits that is used for locking the state when reading
    locked_bits: u8,
    /// the position of the bit that is currently being read
    bit_position: u8,
}

#[allow(clippy::new_without_default)]
impl AController {
    pub fn new() -> AController {
        AController {
            stroke: false,
            bits: 0,
            locked_bits: 0,
            bit_position: 0,
        }
    }

    fn reset_for_read(&mut self) {
        self.bit_position = 0;
        self.locked_bits = self.bits;
    }

    fn peek(&self) -> u8 {
        let pressed = if self.stroke {
            self.bits & Button::A as u8 != 0
        } else if self.bit_position < 8 {
            (self.locked_bits >> self.bit_position) & 1 != 0
        } else {
            false
        };

        if pressed {
            0x41
        } else {
            0x40
        }
    }

    fn read(&mut self) -> u8 {
        let pressed = if self.stroke {
            self.bits & Button::A as u8 != 0
        } else if self.bit_position < 8 {
            let r = (self.locked_bits >> self.bit_position) & 1 != 0;
            self.bit_position = self.bit_position.saturating_add(1);
            r
        } else {
            false
        };

        if pressed {
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
            _ => 0,
        }
    }

    fn peek(&self, address: u16) -> u8 {
        match address {
            0x4016 => self.a.peek(),
            0x4017 => self.b.peek(),
            _ => 0,
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        if address == 0x4016 {
            self.a.stroke = value & 1 != 0;
            self.b.stroke = value & 1 != 0;
            self.a.reset_for_read();
            self.b.reset_for_read();
        }
    }
}

// hard-coded addresses where needed.

#[cfg(test)]
mod tests;
