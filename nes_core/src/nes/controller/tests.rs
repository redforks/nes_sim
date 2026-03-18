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
    let controller = Controller::new();

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
    let controller = Controller::new();

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
    let controller = Controller::new();

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
fn test_controller_write_4017_does_nothing() {
    let controller = Controller::new();

    controller.a.borrow_mut().press(Button::A);
    controller.a.borrow_mut().reset_for_read();

    // Write to 0x4017 should do nothing
    controller.write(0x4017, 0xFF);

    // Controller state should be unchanged
    let val = controller.read(0x4016);
    assert_eq!(val, 0x40); // A button still pressed
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
    let controller = Controller::new();
    controller.write(0x4018, 0);
}
