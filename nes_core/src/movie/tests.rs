use super::*;
use serde::Deserialize;

#[test]
fn test_deserializer() {
    #[derive(Deserialize, Debug, PartialEq)]
    enum EnumType {
        OptionA,
        OptionB,
    }

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(rename_all = "camelCase")]
    struct Foo {
        bool_true: bool,
        bool_false: bool,
        i32_field: i32,
        str_field: String,
        option_bool: Option<bool>,
        option_bool_true: Option<bool>,
        enum_field: EnumType,
    }
    let buf = r#"boolTrue 1
boolFalse 0
i32Field 12
strField foo bar
optionBoolTrue 1
enumField OptionB
"#;

    let foo: Foo = from_str(buf).unwrap();
    assert_eq!(
        foo,
        Foo {
            bool_true: true,
            bool_false: false,
            i32_field: 12,
            str_field: "foo bar".to_owned(),
            option_bool: None,
            option_bool_true: Some(true),
            enum_field: EnumType::OptionB,
        }
    )
}

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
            comment: "author  HappyLee".to_owned(),
            subtitle: "".to_owned(),
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
