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

// --- Zapper (light gun on controller port 2) ---

/// All-bright frame: every pixel senses as light.
fn bright(_: u32, _: u32) -> u32 {
    255 * 3
}

/// All-black frame: no pixel ever senses light.
fn dark(_: u32, _: u32) -> u32 {
    0
}

#[test]
fn zapper_disconnected_reads_zero() {
    let mut zapper = Zapper::new();
    zapper.trigger();

    // Trigger pulled and beam over a bright screen, but the gun is not
    // plugged in: port 2 must show plain open-controller bits, not zapper
    // state.
    assert_eq!(zapper.read(100, 128, bright), 0x00);
}

#[test]
fn zapper_dark_frame_reports_no_light() {
    let mut zapper = Zapper::new();
    zapper.set_connected(true);
    zapper.aim(100, 100);

    assert_eq!(zapper.read(50, 128, dark), 0x08);
}

#[test]
fn zapper_bright_aperture_reports_light_detected() {
    let mut zapper = Zapper::new();
    zapper.set_connected(true);
    zapper.aim(100, 100);

    // Beam at scanline 110 has already drawn every aperture row (97..=103)
    // within the 20-scanline persistence window.
    assert_eq!(zapper.read(110, 128, bright), 0x00);
}

#[test]
fn zapper_trigger_bit_holds_for_release_delay() {
    let mut zapper = Zapper::new();
    zapper.set_connected(true);
    zapper.aim(100, 100);

    zapper.trigger();
    assert_eq!(zapper.read(0, 0, dark), 0x10 | 0x08);

    for _ in 0..ZAPPER_TRIGGER_RELEASE_DELAY - 1 {
        zapper.clock();
    }
    assert_eq!(
        zapper.read(0, 0, dark),
        0x10 | 0x08,
        "trigger must still read held one cycle before the delay elapses"
    );

    zapper.clock();
    assert_eq!(
        zapper.read(0, 0, dark),
        0x08,
        "trigger released after ~100 ms"
    );
}

#[test]
fn zapper_retrigger_does_not_extend_in_flight_hold() {
    let mut zapper = Zapper::new();
    zapper.set_connected(true);

    zapper.trigger();
    for _ in 0..ZAPPER_TRIGGER_RELEASE_DELAY - 5 {
        zapper.clock();
    }

    // Second pull while still held must not restart the release timer.
    zapper.trigger();
    for _ in 0..5 {
        zapper.clock();
    }
    assert_eq!(
        zapper.read(0, 0, dark) & 0x10,
        0x00,
        "hold must end on the original schedule"
    );
}

#[test]
fn zapper_aim_row_needs_beam_past_pixel() {
    // One bright pixel exactly at the aim point.
    let spot_at_aim = |x: u32, y: u32| u32::from(x == 100 && y == 100) * 255 * 3;
    let mut zapper = Zapper::new();
    zapper.set_connected(true);
    zapper.aim(100, 100);

    // Beam has not reached x=100 on the aim row yet.
    assert_eq!(zapper.read(100, 100, spot_at_aim), 0x08);
    // One dot later the pixel is behind the beam and senses.
    assert_eq!(zapper.read(100, 101, spot_at_aim), 0x00);
}

#[test]
fn zapper_persistence_window_is_twenty_scanlines() {
    // Bright row at the aim row only; sampled while scanning rows below it.
    let row_100 = |_: u32, y: u32| u32::from(y == 100) * 255 * 3;
    let mut zapper = Zapper::new();
    zapper.set_connected(true);
    zapper.aim(100, 100);

    // Phosphor from rows above decays over ~20 scanlines.
    for scanline in [101u16, 110, 120] {
        assert_eq!(
            zapper.read(scanline, 0, row_100),
            0x00,
            "scanline {scanline}"
        );
    }
    assert_eq!(zapper.read(121, 0, row_100), 0x08, "decay window closed");
    // Rows below the aim are never sensed: the beam has not drawn them yet.
    assert_eq!(zapper.read(99, 200, row_100), 0x08);
}

#[test]
fn zapper_aperture_spans_radius_pixels() {
    let mut zapper = Zapper::new();
    zapper.set_connected(true);
    zapper.aim(100, 100);

    // radius=3 aperture: a bright pixel at either edge is enough…
    let spot =
        |x: u32, y: u32| u32::from((x == 103 && y == 100) || (x == 97 && y == 103)) * 255 * 3;
    assert_eq!(zapper.read(120, 0, spot), 0x00);
    // …one pixel beyond the edge is not.
    let outside =
        |x: u32, y: u32| u32::from((x == 104 && y == 100) || (x == 96 && y == 100)) * 255 * 3;
    assert_eq!(zapper.read(120, 0, outside), 0x08);
}

#[test]
fn zapper_brightness_threshold_is_eighty_five() {
    let mut zapper = Zapper::new();
    zapper.set_connected(true);
    zapper.aim(100, 100);

    let of_brightness = |v: u32| move |_: u32, _: u32| v;
    assert_eq!(zapper.read(120, 0, of_brightness(84)), 0x08);
    assert_eq!(zapper.read(120, 0, of_brightness(85)), 0x00);
}

#[test]
fn zapper_aim_clamps_to_screen_edges() {
    let mut zapper = Zapper::new();
    zapper.set_connected(true);
    zapper.aim(0, 0);
    assert_eq!(zapper.read(10, 0, bright), 0x00); // aperture clamps at x/y 0
    zapper.aim(255, 239);
    assert_eq!(zapper.read(250, 340, bright), 0x00); // clamps at x 255 / y 239
}
