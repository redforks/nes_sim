use super::*;
use crate::movie::{Command, ExpPortDevice, Fm2Header, GamepadInput, InputDevice, InputLog};
use std::io::Read;

fn fm2(pal: bool, fourscore: bool, input_logs: Vec<InputLog>) -> Fm2File {
    Fm2File {
        header: Fm2Header {
            version: 3,
            emu_version: 240,
            pal_flag: Some(pal),
            fourscore,
            port0: InputDevice::Gamepad,
            port1: InputDevice::Gamepad,
            port2: ExpPortDevice::None,
            rom_filename: "game.nes".to_string(),
            ..Default::default()
        },
        input_logs: input_logs.into_boxed_slice(),
    }
}

fn read_entry(bytes: &[u8], name: &str) -> String {
    let mut archive = zip::ZipArchive::new(Cursor::new(bytes.to_vec())).unwrap();
    let mut entry = archive.by_name(name).unwrap();
    let mut text = String::new();
    entry.read_to_string(&mut text).unwrap();
    text
}

#[test]
fn input_rows_follow_mesen_device_order() {
    let logs = vec![
        InputLog::new(Command::default(), GamepadInput::new(), GamepadInput::new()),
        InputLog::new(
            Command::default(),
            GamepadInput::new()
                .with_up(true)
                .with_select(true)
                .with_b(true)
                .with_a(true),
            GamepadInput::new().with_down(true),
        ),
        InputLog::new(
            Command::new().with_soft_reset(true),
            GamepadInput::default(),
            GamepadInput::default(),
        ),
        InputLog::new(
            Command::new().with_hard_reset(true),
            GamepadInput::default(),
            GamepadInput::default(),
        ),
        InputLog::new(
            Command::new().with_soft_reset(true).with_hard_reset(true),
            GamepadInput::default(),
            GamepadInput::default(),
        ),
    ];

    let bytes = from_fm2(&fm2(false, false, logs), None, None).unwrap();

    assert_eq!(
        read_entry(&bytes, "Input.txt"),
        "|..|........|........\n\
         |..|U....sBA|.D......\n\
         |R.|........|........\n\
         |.P|........|........\n\
         |RP|........|........\n"
    );
}

#[test]
fn game_settings_name_the_console_and_controllers() {
    let bytes = from_fm2(&fm2(false, false, Vec::new()), None, None).unwrap();
    let settings = read_entry(&bytes, "GameSettings.txt");

    for line in [
        "MesenVersion 2.0.0",
        "MovieFormatVersion 2",
        "GameFile game.nes",
        "emu.consoleType Nes",
        "nes.region Ntsc",
        "nes.ramPowerOnState AllZeros",
        "nes.port1.type NesController",
        "nes.port2.type NesController",
        "nes.expPort.type None",
    ] {
        assert!(
            settings.lines().any(|candidate| candidate == line),
            "missing {line:?} in\n{settings}"
        );
    }
    assert!(!settings.contains("SHA1 "));
}

#[test]
fn pal_flag_selects_pal_region() {
    let bytes = from_fm2(&fm2(true, false, Vec::new()), None, None).unwrap();
    let settings = read_entry(&bytes, "GameSettings.txt");
    assert!(settings.lines().any(|line| line == "nes.region Pal"));
}

#[test]
fn rom_supplies_sha1_and_filename() {
    let bytes = from_fm2(
        &fm2(false, false, Vec::new()),
        Some(b"abc"),
        Some("other.nes"),
    )
    .unwrap();
    let settings = read_entry(&bytes, "GameSettings.txt");

    assert!(settings.lines().any(|line| line == "GameFile other.nes"));
    // SHA-1 of "abc".
    assert!(
        settings
            .lines()
            .any(|line| line == "SHA1 a9993e364706816aba3e25717850c26c9cd0d89d")
    );
}

#[test]
fn empty_fm2_filename_falls_back() {
    let mut movie = fm2(false, false, Vec::new());
    movie.header.rom_filename.clear();

    let bytes = from_fm2(&movie, None, None).unwrap();
    let settings = read_entry(&bytes, "GameSettings.txt");
    assert!(settings.lines().any(|line| line == "GameFile movie.nes"));
}

#[test]
fn fourscore_is_rejected() {
    let result = from_fm2(&fm2(false, true, Vec::new()), None, None);
    assert!(matches!(
        result,
        Err(WriteMesenMovieError::FourScoreUnsupported)
    ));
}

#[test]
fn fds_command_is_rejected_at_its_frame() {
    let logs = vec![
        InputLog::new(
            Command::default(),
            GamepadInput::default(),
            GamepadInput::default(),
        ),
        InputLog::new(
            Command::new().with_fds_disk_insert(true),
            GamepadInput::default(),
            GamepadInput::default(),
        ),
    ];
    let result = from_fm2(&fm2(false, false, logs), None, None);
    assert!(matches!(
        result,
        Err(WriteMesenMovieError::UnsupportedCommand { frame: 1 })
    ));
}
