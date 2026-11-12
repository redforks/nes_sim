//! https://fceux.com/web/FM2.html

use std::{collections::HashSet, str::from_utf8};

use anyhow::Context;
use bitfield_struct::bitfield;
use winnow::{
    ascii::dec_uint,
    combinator::{alt, opt, repeat, seq, terminated},
    error::{ContextError, ParserError},
    prelude::*,
    stream::{AsBStr, AsChar, Compare, Stream, StreamIsPartial},
    token::{any, literal},
};

use crate::ines::INesFile;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum InputDevice {
    #[default]
    None,
    Gamepad,
    Zapper,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum ExpPortDevice {
    #[default]
    None,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Fm2Header {
    /// the version of the movie file format; for now it is always 3
    pub version: i32,
    /// the version of the emulator used to produce the movie
    pub emu_version: i32,
    /// the rerecord count
    pub rerecord_count: Option<i32>,
    /// true if the movie uses PAL timing
    pub pal_flag: Option<bool>,
    /// true if the movie uses New PPU
    pub new_ppu: Option<bool>,
    /// true if the movie was recorded on a Famicom Disk System (FDS) game
    pub fds: Option<bool>,
    /// true if a fourscore was used. If fourscore is not used, then port0 and port1 are required
    pub fourscore: bool,
    /// whether microphone is enabled, this field not defined in document, see fceux/src/movic.h
    pub microphone: Option<bool>,
    /// indicates the type of input device attached to the port 0. Supported values are:
    pub port0: InputDevice,
    /// indicates the type of input device attached to the port 1. Supported values are:
    pub port1: InputDevice,
    /// indicates the type of the FCExp port device which was attached. Supported values are:
    pub port2: ExpPortDevice,
    /// true if input log is stored in binary format
    pub binary: Option<bool>,
    /// movie size (number of frames in the input log). If this key is specified
    /// and the number is >= 0, the input log ends after specified number of
    /// records, and any remaining data should not be parsed. This key is used
    /// in fm3 format to allow storing extra data after the end of input log
    pub length: Option<i32>,

    /// romFilename (required) - the name of the file used to record the movie
    pub rom_filename: String,
    /// simply a memo
    ///     by convention, the first token in the comment value is the subject of the comment
    ///     by convention, subsequent comments with the same subject should have their ordering preserved and may be used to approximate multi-line comments
    ///     by convention, the author of the movie should be stored in comment(s) with a subject of: author
    ///     example:
    ///         comment author adelikat
    ///
    /// The key may repeat; values keep their file order.
    pub comment: Vec<String>,
    /// a message that will be displayed on screen when movie is played back
    /// (unless Subtitles are turned off, see Movie options)
    ///     by convention, subtitles begin with the word "subtitle"
    ///     by convention, an integer value following the word "subtitle" indicates the frame that the subtitle will be displayed
    ///     by convention, any remaining text after the integer is considered to be the string displayed
    ///     example:
    ///      subtitle 1000 Level Two
    ///      At frame 1000 the words "Level Two" will be displayed on the screen
    ///
    /// The key may repeat; values keep their file order.
    pub subtitle: Vec<String>,
    /// a unique identifier for a movie, generated when the
    /// movie is created, which is used when loading a savestate to make sure it
    /// belongs to the current movie.
    /// GUID keys have a value which is in the
    /// standard GUID format: 452DE2C3-EF43-2FA9-77AC-0677FC51543B
    pub guid: String,
    /// the base64 of the hexified MD5 hash of the ROM which was used to record the movie (don't ask)
    pub rom_checksum: String,
    /// a fcs savestate blob, in case a movie was recorded from savestate
    /// Hex string keys (used for binary blobs) will have a value that is like 0x0123456789ABCDEF...
    pub save_state: Option<String>,
}

#[bitfield(u8)]
#[derive(PartialEq)]
pub struct Command {
    pub soft_reset: bool,
    pub hard_reset: bool,
    pub fds_disk_insert: bool,
    pub fds_disk_select: bool,
    pub vs_insert_coin: bool,
    #[bits(3)]
    __: u8,
}

#[bitfield(u8)]
#[derive(PartialEq)]
pub struct GamepadInput {
    pub a: bool,
    pub b: bool,
    pub select: bool,
    pub start: bool,
    pub up: bool,
    pub down: bool,
    pub left: bool,
    pub right: bool,
}

impl GamepadInput {
    /// Returns true if all buttons are not pressed
    pub fn is_empty(self) -> bool {
        u8::from(self) == 0
    }
}

pub struct ZapperInput {
    pub z: i32,
    pub q: u8,
    pub b: u8,
    pub y: u16,
    pub x: u16,
}

#[derive(Debug, PartialEq)]
pub struct InputLog {
    pub command: Command,
    pub port0: GamepadInput,
    pub port1: GamepadInput,
}

impl InputLog {
    pub fn new(command: Command, port0: GamepadInput, port1: GamepadInput) -> Self {
        Self {
            command,
            port0,
            port1,
        }
    }
}

#[derive(Debug)]
pub struct Fm2File {
    pub header: Fm2Header,
    pub input_logs: Box<[InputLog]>,
}

impl Fm2File {
    /// Use checksum to valid rom file
    pub fn valid_rom_file(&self, file: &INesFile) -> Result<bool, anyhow::Error> {
        use base64::prelude::*;
        let checksum = self
            .header
            .rom_checksum
            .as_bytes()
            .strip_prefix(b"base64:")
            .unwrap_or(self.header.rom_checksum.as_bytes());
        let checksum = BASE64_STANDARD
            .decode(checksum)
            .context("decode base64 encoded rom checksum")?;
        let mut md5context = md5::Context::new();
        md5context.consume(file.read_prg_rom());
        md5context.consume(file.read_chr_rom());
        let act = md5context.finalize();
        eprintln!("{:x}", act);
        eprintln!("{:?}", checksum.as_slice());
        Ok(act.0.as_slice() == checksum.as_slice())
    }
}

fn new_input_log_parser<Input, E>() -> impl Parser<Input, InputLog, E>
where
    Input: Stream + StreamIsPartial + Compare<&'static [u8]> + Compare<u8>,
    Input::Slice: AsBStr,
    Input::Token: AsChar + Clone + PartialEq<u8>,
    E: ParserError<Input>,
{
    seq! {
        InputLog {
        _: b'|',
        command: new_command_parser(),
        _: b'|',
        port0: new_gamepad_input_parser(),
        _: b'|',
        port1: new_gamepad_input_parser(),
        _: b'|',
        _: b'|',
        }
    }
}

fn new_gamepad_flag_parser<Input, E>() -> impl Parser<Input, bool, E>
where
    Input: Stream + StreamIsPartial + Compare<&'static [u8]>,
    Input::Token: AsChar + Clone + PartialEq<u8>,
    E: ParserError<Input>,
{
    any.map(|v| v != b'.' && v != b' ')
}

fn new_command_parser<Input, E>() -> impl Parser<Input, Command, E>
where
    Input: Stream + StreamIsPartial + Compare<&'static [u8]>,
    Input::Slice: AsBStr,
    Input::Token: AsChar + Clone,
    E: ParserError<Input>,
{
    dec_uint::<Input, u8, E>.output_into::<Command>()
}

fn new_gamepad_input_parser<Input, E>() -> impl Parser<Input, GamepadInput, E>
where
    Input: Stream + StreamIsPartial + Compare<&'static [u8]>,
    Input::Slice: AsBStr,
    Input::Token: AsChar + Clone + PartialEq<u8>,
    E: ParserError<Input>,
{
    struct Context {
        val: u8,
        pos: u8,
    }

    impl Context {
        fn new() -> Self {
            Self { val: 0, pos: 8 }
        }

        fn push_flag(mut self, flag: bool) -> Self {
            self.pos -= 1;
            let mut v = if flag { 1 } else { 0 };
            v <<= self.pos;
            self.val |= v;
            self
        }

        fn into_value(self) -> GamepadInput {
            self.val.into()
        }
    }

    repeat(8, new_gamepad_flag_parser())
        .fold(Context::new, |ctx, v| ctx.push_flag(v))
        .map(Context::into_value)
}

fn new_input_logs_parser<Input, E>(
    port0: InputDevice,
    port1: InputDevice,
    port2: ExpPortDevice,
) -> impl Parser<Input, Vec<InputLog>, E>
where
    Input: Stream + StreamIsPartial + Compare<&'static [u8]> + Compare<u8>,
    Input::Slice: AsBStr,
    Input::Token: AsChar + Clone + PartialEq<u8>,
    E: ParserError<Input>,
{
    assert_eq!(
        port0,
        InputDevice::Gamepad,
        "only Gamepad supported in fm2 file"
    );
    assert_eq!(
        port1,
        InputDevice::Gamepad,
        "only Gamepad supported in fm2 file"
    );
    assert_eq!(port2, ExpPortDevice::None);

    repeat(
        1..,
        terminated(
            new_input_log_parser(),
            opt(alt((
                literal(b"\n".as_slice()),
                literal(b"\r\n".as_slice()),
            ))),
        ),
    )
}

pub fn parse(buf: &[u8]) -> Result<Fm2File, ParseFm2FileError> {
    let pos = buf
        .iter()
        .position(|v| *v == b'|')
        .ok_or(ParseFm2FileError::MissingInputLog)?;
    let header_part = from_utf8(&buf[0..pos]).map_err(|_| ParseFm2FileError::ExpectAsciiFormat)?;
    let header = parse_header(header_part)?;
    assert!(
        header.binary.is_none_or(|v| !v),
        "binary fm2 format not supported"
    );
    let input_logs_part = &buf[pos..];
    let input_logs =
        // new_input_logs_parser::<_, InputError<_>>(header.port0, header.port1, header.port2)
        //     .parse(input_logs_part)
        //     .map_err(|e| {
        //         e.into_inner()
        //             .map_input(|v| format!("{:?}", String::from_utf8_lossy(v)))
        //     })?;
    new_input_logs_parser::<_, ContextError<_>>(header.port0, header.port1, header.port2)
        .parse(input_logs_part)
        .map_err(|e| e.into_inner())?;
    Ok(Fm2File {
        header,
        input_logs: input_logs.into(),
    })
}

#[derive(thiserror::Error, Debug)]
pub enum ParseFm2FileError {
    #[error("File not ascii format")]
    ExpectAsciiFormat,
    #[error("Missing input logs")]
    MissingInputLog,
    #[error("Malformed header line {line}: expected `key value`")]
    MalformedHeaderLine { line: usize },
    #[error("Duplicate field `{key}` at line {line}")]
    DuplicateField { key: String, line: usize },
    #[error("Missing required field `{key}`")]
    MissingField { key: &'static str },
    #[error("Invalid value `{value}` for field `{key}` at line {line}")]
    InvalidFieldValue {
        key: &'static str,
        value: String,
        line: usize,
    },
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

impl From<ContextError> for ParseFm2FileError {
    fn from(value: ContextError) -> Self {
        anyhow::anyhow!(value.to_string()).into()
    }
}

/// The fm2 header is one `key value` pair per line, separated by spaces.
///
/// `comment` and `subtitle` may repeat; every other key must appear at most
/// once. Repeated keys keep their file order because the fm2 conventions give
/// that order meaning (`subtitle <frame> <text>`, and successive comments with
/// the same subject form a multi-line comment).
struct HeaderFields<'a> {
    /// `(key, value, 1-based line number)` in file order.
    fields: Vec<(&'a str, &'a str, usize)>,
}

/// Keys that the fm2 format allows to repeat.
const REPEATABLE_KEYS: [&str; 2] = ["comment", "subtitle"];

impl<'a> HeaderFields<'a> {
    fn parse(header: &'a str) -> Result<Self, ParseFm2FileError> {
        let mut fields = Vec::new();
        let mut seen = HashSet::new();
        for (index, raw) in header.lines().enumerate() {
            let line = index + 1;
            let raw = raw.trim_end();
            if raw.is_empty() {
                continue;
            }
            let Some((key, value)) = raw.split_once([' ', '\t']) else {
                return Err(ParseFm2FileError::MalformedHeaderLine { line });
            };
            let value = value.trim_start();
            if !REPEATABLE_KEYS.contains(&key) && !seen.insert(key) {
                return Err(ParseFm2FileError::DuplicateField {
                    key: key.to_owned(),
                    line,
                });
            }
            fields.push((key, value, line));
        }
        Ok(Self { fields })
    }

    /// The value of the first `key` occurrence and its 1-based line number.
    fn find(&self, key: &str) -> Option<(&'a str, usize)> {
        self.fields
            .iter()
            .find(|(k, _, _)| *k == key)
            .map(|(_, value, line)| (*value, *line))
    }

    /// Every value of a repeatable `key`, in file order.
    fn all(&self, key: &str) -> Vec<String> {
        self.fields
            .iter()
            .filter(|(k, _, _)| *k == key)
            .map(|(_, value, _)| (*value).to_owned())
            .collect()
    }

    fn required<T: Fm2Value>(&self, key: &'static str) -> Result<T, ParseFm2FileError> {
        let (value, line) = self
            .find(key)
            .ok_or(ParseFm2FileError::MissingField { key })?;
        T::parse_fm2(key, value, line)
    }

    fn optional<T: Fm2Value>(&self, key: &'static str) -> Result<Option<T>, ParseFm2FileError> {
        self.find(key)
            .map(|(value, line)| T::parse_fm2(key, value, line))
            .transpose()
    }
}

/// A header value type that knows how fm2 writes it.
trait Fm2Value: Sized {
    fn parse_fm2(key: &'static str, value: &str, line: usize) -> Result<Self, ParseFm2FileError>;
}

impl ParseFm2FileError {
    fn invalid_field(key: &'static str, value: &str, line: usize) -> Self {
        Self::InvalidFieldValue {
            key,
            value: value.to_owned(),
            line,
        }
    }
}

impl Fm2Value for String {
    fn parse_fm2(_key: &'static str, value: &str, _line: usize) -> Result<Self, ParseFm2FileError> {
        Ok(value.to_owned())
    }
}

impl Fm2Value for i32 {
    fn parse_fm2(key: &'static str, value: &str, line: usize) -> Result<Self, ParseFm2FileError> {
        value
            .parse()
            .map_err(|_| ParseFm2FileError::invalid_field(key, value, line))
    }
}

impl Fm2Value for bool {
    fn parse_fm2(key: &'static str, value: &str, line: usize) -> Result<Self, ParseFm2FileError> {
        match value {
            "0" => Ok(false),
            "1" => Ok(true),
            _ => Err(ParseFm2FileError::invalid_field(key, value, line)),
        }
    }
}

impl Fm2Value for InputDevice {
    fn parse_fm2(key: &'static str, value: &str, line: usize) -> Result<Self, ParseFm2FileError> {
        match value {
            "0" => Ok(Self::None),
            "1" => Ok(Self::Gamepad),
            "2" => Ok(Self::Zapper),
            _ => Err(ParseFm2FileError::invalid_field(key, value, line)),
        }
    }
}

impl Fm2Value for ExpPortDevice {
    fn parse_fm2(key: &'static str, value: &str, line: usize) -> Result<Self, ParseFm2FileError> {
        match value {
            "0" => Ok(Self::None),
            _ => Err(ParseFm2FileError::invalid_field(key, value, line)),
        }
    }
}

/// Parse the header of an fm2 file, i.e. the part before the first input log.
fn parse_header(header: &str) -> Result<Fm2Header, ParseFm2FileError> {
    let fields = HeaderFields::parse(header)?;
    Ok(Fm2Header {
        version: fields.required("version")?,
        emu_version: fields.required("emuVersion")?,
        rerecord_count: fields.optional("rerecordCount")?,
        pal_flag: fields.optional("palFlag")?,
        new_ppu: fields.optional("NewPPU")?,
        fds: fields.optional("FDS")?,
        fourscore: fields.required("fourscore")?,
        microphone: fields.optional("microphone")?,
        port0: fields.required("port0")?,
        port1: fields.required("port1")?,
        port2: fields.required("port2")?,
        binary: fields.optional("binary")?,
        length: fields.optional("length")?,
        rom_filename: fields.required("romFilename")?,
        comment: fields.all("comment"),
        subtitle: fields.all("subtitle"),
        guid: fields.required("guid")?,
        rom_checksum: fields.required("romChecksum")?,
        save_state: fields.optional("saveState")?,
    })
}

#[cfg(test)]
mod tests;
