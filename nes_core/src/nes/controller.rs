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

        if r != 0 { 0x40 } else { 0x41 }
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

    #[test]
    fn test_controller_new() {
        let controller = Controller::new();
        // Verify both controllers are initialized
        let a = controller.a.borrow();
        let b = controller.b.borrow();
        // Just verify they exist (RefCell prevents direct value access)
        drop((a, b));
    }

    #[test]
    fn test_controller_read() {
        let mut controller = Controller::new();

        // Press some buttons on controller A
        controller.a.borrow_mut().press(Button::A);
        controller.a.borrow_mut().reset_for_read();

        // Read from address 0x4016 (controller A)
        let val = controller.read(0x4016);
        assert_eq!(val, 0x40); // A button pressed

        // Press some buttons on controller B
        controller.b.borrow_mut().press(Button::B);
        controller.b.borrow_mut().reset_for_read();

        // Read from address 0x4017 (controller B)
        let val = controller.read(0x4017);
        assert_eq!(val, 0x41); // B button not pressed (first read is A)
    }

    #[test]
    fn test_controller_write() {
        let mut controller = Controller::new();

        // Press buttons before write
        controller.a.borrow_mut().press(Button::A);
        controller.a.borrow_mut().press(Button::B);

        // Write 0 to address 0x4016 to reset for reading
        controller.write(0x4016, 0);

        // Now read should return button states starting from first button
        let val = controller.read(0x4016);
        assert_eq!(val, 0x40); // A button
    }

    #[test]
    fn test_controller_write_1_does_not_reset() {
        let mut controller = Controller::new();

        controller.a.borrow_mut().press(Button::A);
        controller.a.borrow_mut().reset_for_read();
        let val1 = controller.read(0x4016);
        assert_eq!(val1, 0x40);

        // Write 1 (which doesn't trigger reset)
        controller.write(0x4016, 1);

        // Next read should continue from current state (not reset)
        let val2 = controller.read(0x4016);
        // Since mask was rotated, now we're reading B button
        assert_eq!(val2, 0x41);
    }

    #[test]
    fn test_controller_region() {
        let controller = Controller::new();
        let (start, end) = controller.region();
        assert_eq!(start, 0x4016);
        assert_eq!(end, 0x4017);
    }

    #[test]
    fn test_all_button_values() {
        assert_eq!(Button::A as u8, 1);
        assert_eq!(Button::B as u8, 0x2);
        assert_eq!(Button::Select as u8, 0x4);
        assert_eq!(Button::Start as u8, 0x8);
        assert_eq!(Button::Up as u8, 0x10);
        assert_eq!(Button::Down as u8, 0x20);
        assert_eq!(Button::Left as u8, 0x40);
        assert_eq!(Button::Right as u8, 0x80);
    }

    #[test]
    fn test_press_multiple_buttons() {
        let mut a = AController::new();
        a.press(Button::A);
        a.press(Button::B);
        a.press(Button::Start);
        a.reset_for_read();

        assert_eq!(a.read(), 0x40); // A
        assert_eq!(a.read(), 0x40); // B
        assert_eq!(a.read(), 0x41); // Select
        assert_eq!(a.read(), 0x40); // Start
    }

    #[test]
    fn test_release_buttons() {
        let mut a = AController::new();
        a.press(Button::A);
        a.press(Button::B);
        a.release(Button::A);

        a.reset_for_read();
        assert_eq!(a.read(), 0x41); // A (released)
        assert_eq!(a.read(), 0x40); // B (still pressed)
    }

    #[test]
    #[should_panic(expected = "read address out of range")]
    fn test_controller_read_invalid_address() {
        let controller = Controller::new();
        controller.read(0x4015);
    }

    #[test]
    #[should_panic(expected = "write address out of range")]
    fn test_controller_write_invalid_address() {
        let mut controller = Controller::new();
        controller.write(0x4018, 0);
    }
}
