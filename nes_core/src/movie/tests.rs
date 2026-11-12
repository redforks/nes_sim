use super::*;

#[test]
fn command_parser() {
    let mut p = new_command_parser::<_, ContextError>();
    assert_eq!(p.parse(b"0".as_slice()).unwrap(), Command::default());
    assert_eq!(
        p.parse(b"1".as_slice()).unwrap(),
        Command::new().with_soft_reset(true)
    );
    assert_eq!(
        p.parse(b"31".as_slice()).unwrap(),
        Command::new()
            .with_soft_reset(true)
            .with_hard_reset(true)
            .with_fds_disk_insert(true)
            .with_fds_disk_select(true)
            .with_vs_insert_coin(true)
    );
}

#[test]
fn gamepad_flag() {
    let mut p = new_gamepad_flag_parser::<_, ContextError>();
    assert!(!p.parse(b".".as_slice()).unwrap());
    assert!(!p.parse(b" ".as_slice()).unwrap());
    assert!(p.parse(b"R".as_slice()).unwrap());
    assert!(p.parse(b"c".as_slice()).unwrap());
}

#[test]
fn gamepad_input_parser() {
    let mut p = new_gamepad_input_parser::<_, ContextError>();
    assert_eq!(
        p.parse(b"...... .".as_slice()).unwrap(),
        GamepadInput::new()
    );
    assert_eq!(
        p.parse(b"R.... B.".as_slice()).unwrap(),
        GamepadInput::new().with_right(true).with_b(true)
    );
}

#[test]
fn parse_file() {
    let buf = br#"version 3
emuVersion 9828
rerecordCount 6283
palFlag 0
romFilename Super Mario Bros. (1985)(Nintendo)(J)
romChecksum base64:VYmON/RqC5V5eZa+cou4Og==
guid 2EDB1A82-6AE2-A0DA-167A-B6ABC24A3EED
fourscore 0
port0 1
port1 1
port2 0
comment author  HappyLee
|1|........|........||
|0|........|........||
|0|R.....B.|........||
|0|.L....B.|........||
"#;

    let f = parse(buf).unwrap();
    assert_eq!(
        f.header,
        Fm2Header {
            version: 3,
            emu_version: 9828,
            rerecord_count: Some(6283),
            pal_flag: Some(false),
            new_ppu: None,
            fds: None,
            fourscore: false,
            microphone: None,
            port0: InputDevice::Gamepad,
            port1: InputDevice::Gamepad,
            port2: ExpPortDevice::None,
            binary: None,
            length: None,
            rom_filename: "Super Mario Bros. (1985)(Nintendo)(J)".to_owned(),
            comment: vec!["author  HappyLee".to_owned()],
            subtitle: vec![],
            guid: "2EDB1A82-6AE2-A0DA-167A-B6ABC24A3EED".to_owned(),
            rom_checksum: "base64:VYmON/RqC5V5eZa+cou4Og==".to_owned(),
            save_state: None,
        }
    );

    assert_eq!(
        &f.input_logs[..],
        &[
            InputLog::new(
                Command::new().with_soft_reset(true),
                GamepadInput::new(),
                GamepadInput::new()
            ),
            InputLog::new(Command::new(), GamepadInput::new(), GamepadInput::new()),
            InputLog::new(
                Command::new(),
                GamepadInput::new().with_right(true).with_b(true),
                GamepadInput::new(),
            ),
            InputLog::new(
                Command::new(),
                GamepadInput::new().with_left(true).with_b(true),
                GamepadInput::new(),
            )
        ][..]
    );
}

/// A real movie from tasvideos.org repeats both `comment` and `subtitle`, which
/// the fm2 conventions allow. See `adelikat2-contrapacifist.fm2`.
const MULTI_COMMENT_BUF: &[u8] = br#"version 3
emuVersion 21010
rerecordCount 15434
palFlag 0
romFilename Contra (J)
romChecksum base64:0wbFTM/fXLT47FiPGbPjPQ==
guid 6031304D-F4AB-7C91-F0E2-FB6ADDF548A7
fourscore 0
port0 1
port1 1
port2 0
comment author  adelikat
comment Version v3
comment Category pacifist
subtitle 1 Contra JPN "pacifist" in 9:41.36 played by adelikat
subtitle 340 Rerecord count: 15434
|1|........|........||
|0|........|........||
"#;

#[test]
fn parse_file_multiple_comments_and_subtitles() {
    let f = parse(MULTI_COMMENT_BUF).unwrap();
    assert_eq!(
        f.header.comment,
        ["author  adelikat", "Version v3", "Category pacifist"]
    );
    assert_eq!(
        f.header.subtitle,
        [
            "1 Contra JPN \"pacifist\" in 9:41.36 played by adelikat",
            "340 Rerecord count: 15434",
        ]
    );
    assert_eq!(f.header.port0, InputDevice::Gamepad);
}

#[test]
fn parse_file_rejects_duplicate_non_repeatable_field() {
    let mut buf = MULTI_COMMENT_BUF.to_vec();
    // A second `romFilename` on line 5 would otherwise silently shadow the first.
    buf.splice(0..0, *b"version 3\n");
    let err = parse(&buf).unwrap_err();
    assert!(
        matches!(
            err,
            ParseFm2FileError::DuplicateField { ref key, line: 2 } if key == "version"
        ),
        "{err:?}"
    );
}

#[test]
fn parse_file_ignores_unknown_fields() {
    let mut buf = MULTI_COMMENT_BUF.to_vec();
    buf.splice(0..0, *b"someFutureKey 42\n");
    assert_eq!(parse(&buf).unwrap().header.comment.len(), 3);
}

#[test]
fn parse_file_reports_malformed_header_line() {
    let mut buf = MULTI_COMMENT_BUF.to_vec();
    buf.splice(0..0, *b"noValueHere\n");
    let err = parse(&buf).unwrap_err();
    assert!(
        matches!(err, ParseFm2FileError::MalformedHeaderLine { line: 1 }),
        "{err:?}"
    );
}

#[test]
fn parse_file_reports_invalid_value_with_line() {
    let mut buf = MULTI_COMMENT_BUF.to_vec();
    buf.splice(0..0, *b"NewPPU maybe\n");
    let err = parse(&buf).unwrap_err();
    assert!(
        matches!(
            err,
            ParseFm2FileError::InvalidFieldValue { key: "NewPPU", value: ref v, line: 1 }
                if v == "maybe"
        ),
        "{err:?}"
    );
}
