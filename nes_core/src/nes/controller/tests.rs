use super::*;

#[test]
fn test_a_controller() {
    let mut a = AController::new();
    a.press(Button::A);
    a.press(Button::Left);
    a.reset_for_read();
    assert_eq!(a.read(), 0x41); // A
    assert_eq!(a.read(), 0x40); // B
    assert_eq!(a.read(), 0x40); // Select
    assert_eq!(a.read(), 0x40); // Start
    assert_eq!(a.read(), 0x40); // Up
    assert_eq!(a.read(), 0x40); // Down
    assert_eq!(a.read(), 0x41); // Left
    assert_eq!(a.read(), 0x40); // Right
    assert_eq!(a.read(), 0x41); // Open bus/high after 8 reads

    a.release(Button::A);
    a.press(Button::B);
    a.reset_for_read();
    assert_eq!(a.read(), 0x40); // A
    assert_eq!(a.read(), 0x41); // B
    assert_eq!(a.read(), 0x40); // Select
    assert_eq!(a.read(), 0x40); // Start
    assert_eq!(a.read(), 0x40); // Up
    assert_eq!(a.read(), 0x40); // Down
    assert_eq!(a.read(), 0x41); // Left
    assert_eq!(a.read(), 0x40); // Right
    assert_eq!(a.read(), 0x41); // Open bus/high after 8 reads
}

#[test]
fn test_controller_reads_return_one_after_eight_buttons() {
    let mut a = AController::new();
    a.reset_for_read();

    for _ in 0..8 {
        assert_eq!(a.read(), 0x40);
    }

    assert_eq!(a.read(), 0x41);
    assert_eq!(a.read(), 0x41);
}

#[test]
fn test_a_controller_strobe_mode_reads_a_button_only() {
    let mut a = AController::new();
    a.press(Button::A);
    a.press(Button::B);
    a.strobe = true;

    assert_eq!(a.read(), 0x41);
    assert_eq!(a.read(), 0x41);
}

#[test]
fn test_controller_new() {
    let controller = Controller::new();
    // Verify both controllers are initialized
    let _a = &controller.a;
    let _b = &controller.b;
    // Controllers exist and are accessible
}

#[test]
fn test_controller_read() {
    let mut controller = Controller::new();

    // Press some buttons on controller A
    controller.a.press(Button::A);
    controller.a.reset_for_read();

    // Read from address 0x4016 (controller A)
    let val = controller.read(0x4016);
    assert_eq!(val, 0x41); // A button pressed

    // Press some buttons on controller B
    controller.b.press(Button::B);
    controller.b.reset_for_read();

    // Read from address 0x4017 (controller B)
    let val = controller.read(0x4017);
    assert_eq!(val, 0x40); // B button not pressed (first read is A)
}

#[test]
fn test_controller_write() {
    let mut controller = Controller::new();

    // Press buttons before strobing
    controller.a.press(Button::A);
    controller.a.press(Button::B);

    // Hardware polling sequence: raise strobe, lower strobe, then read.
    // The falling edge freezes button state and rewinds the poll.
    controller.write(0x4016, 1);
    controller.write(0x4016, 0);

    // Now read should return button states starting from first button
    let val = controller.read(0x4016);
    assert_eq!(val, 0x41); // A button
}

#[test]
fn test_controller_write_1_resets_and_latches() {
    let mut controller = Controller::new();

    controller.a.press(Button::A);
    controller.a.reset_for_read();
    let val1 = controller.read(0x4016);
    assert_eq!(val1, 0x41);

    // Write 1 enables strobe mode and latches the current state
    controller.write(0x4016, 1);

    // Next read should stay on A while strobe is enabled
    let val2 = controller.read(0x4016);
    assert_eq!(val2, 0x41);
}

#[test]
fn test_controller_write_4017_does_nothing() {
    let mut controller = Controller::new();

    controller.a.press(Button::A);
    controller.a.reset_for_read();

    // Write to 0x4017 should do nothing
    controller.write(0x4017, 0xFF);

    // Controller state should be unchanged
    let val = controller.read(0x4016);
    assert_eq!(val, 0x41); // A button still pressed
}

#[test]
fn test_strobe_low_write_mid_poll_does_not_rewind_poll() {
    let mut controller = Controller::new();

    controller.a.press(Button::A);
    controller.write(0x4016, 1); // raise strobe
    controller.write(0x4016, 0); // falling edge freezes and rewinds

    // Poll starts at A...
    assert_eq!(controller.read(0x4016), 0x41);

    // ...a defensive strobe-low write mid-poll must not rewind the poll...
    controller.write(0x4016, 0);

    // ...so the next read continues at B instead of reading A again.
    assert_eq!(controller.read(0x4016), 0x40);
}

#[test]
fn test_mid_poll_input_change_reads_frozen_register() {
    let mut controller = Controller::new();

    controller.a.press(Button::A);
    controller.a.press(Button::B);
    controller.write(0x4016, 1); // raise strobe
    controller.write(0x4016, 0); // falling edge freezes A+B

    assert_eq!(controller.read(0x4016), 0x41); // A

    // Input changes after the falling edge must not leak into the
    // running poll: remaining reads come from the frozen register.
    controller.a.release(Button::A);
    controller.a.release(Button::B);
    controller.a.press(Button::Start);

    // A defensive strobe-low write after the fall must not refresh
    // the snapshot either: the poll keeps serving the frozen register.
    controller.write(0x4016, 0);

    assert_eq!(controller.read(0x4016), 0x41); // B still pressed in the snapshot
    assert_eq!(controller.read(0x4016), 0x40); // Select
    assert_eq!(controller.read(0x4016), 0x40); // Start not in the snapshot
}

#[test]
fn test_re_strobe_mid_poll_restarts_poll_with_fresh_snapshot() {
    let mut controller = Controller::new();

    controller.a.press(Button::A);
    controller.write(0x4016, 1);
    controller.write(0x4016, 0);

    assert_eq!(controller.read(0x4016), 0x41); // A

    // Re-strobe mid-poll: B was pressed since the last falling edge.
    controller.a.press(Button::B);
    controller.write(0x4016, 1);
    controller.write(0x4016, 0); // falling edge takes a fresh snapshot

    // The poll restarts at bit position zero with fresh button state.
    assert_eq!(controller.read(0x4016), 0x41); // A
    assert_eq!(controller.read(0x4016), 0x41); // B
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

    assert_eq!(a.read(), 0x41); // A
    assert_eq!(a.read(), 0x41); // B
    assert_eq!(a.read(), 0x40); // Select
    assert_eq!(a.read(), 0x41); // Start
}

#[test]
fn test_release_buttons() {
    let mut a = AController::new();
    a.press(Button::A);
    a.press(Button::B);
    a.release(Button::A);

    a.reset_for_read();
    assert_eq!(a.read(), 0x40); // A (released)
    assert_eq!(a.read(), 0x41); // B (still pressed)
}
