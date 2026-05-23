//! https://fceux.com/web/FM2.html

use std::{fmt::Display, str::from_utf8};

use anyhow::Context;
use bitfield_struct::bitfield;
use serde::{
    Deserialize, Deserializer,
    de::{IntoDeserializer as _, MapAccess},
};
use winnow::{
    ascii::{alphanumeric1, dec_int, dec_uint, line_ending, space1, till_line_ending},
    combinator::{alt, opt, repeat, seq, terminated},
    error::{ContextError, InputError, ParserError},
    prelude::*,
    stream::{AsBStr, AsChar, Compare, Stream, StreamIsPartial},
    token::{any, literal},
};

#[derive(Deserialize, Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum InputDevice {
    #[default]
    #[serde(rename = "0")]
    None,
    #[serde(rename = "1")]
    Gamepad,
    #[serde(rename = "2")]
    Zapper,
}

#[derive(Deserialize, Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum ExpPortDevice {
    #[default]
    #[serde(rename = "0")]
    None,
}

#[derive(Deserialize, Debug, PartialEq, Eq, Default)]
#[serde(rename_all = "camelCase")]
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
    #[serde(rename = "NewPPU")]
    pub new_ppu: Option<bool>,
    /// true if the movie was recorded on a Famicom Disk System (FDS) game
    #[serde(rename = "FDS")]
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
    #[serde(default)]
    pub comment: String,
    /// a message that will be displayed on screen when movie is played back
    /// (unless Subtitles are turned off, see Movie options)
    ///     by convention, subtitles begin with the word "subtitle"
    ///     by convention, an integer value following the word "subtitle" indicates the frame that the subtitle will be displayed
    ///     by convention, any remaining text after the integer is considered to be the string displayed
    ///     example:
    ///      subtitle 1000 Level Two
    ///      At frame 1000 the words "Level Two" will be displayed on the screen
    #[serde(default)]
    pub subtitle: String,
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
    soft_reset: bool,
    hard_reset: bool,
    fds_disk_insert: bool,
    fds_disk_select: bool,
    vs_insert_coin: bool,
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
    pub fn valid_rom_file(&self, file_content: &[u8]) -> Result<bool, anyhow::Error> {
        use base64::prelude::*;
        let checksum = self
            .header
            .rom_checksum
            .as_bytes()
            .strip_prefix(b"base64:")
            .unwrap_or_else(|| self.header.rom_checksum.as_bytes());
        let checksum = BASE64_STANDARD
            .decode(checksum)
            .context("decode base64 encoded rom checksum")?;
        let act = md5::compute(file_content);
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
    let input_logs_part = &buf[pos..];
    let header: Fm2Header = from_str(header_part)?;
    assert!(
        header.binary.is_none_or(|v| !v),
        "binary fm2 format not supported"
    );
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
    #[error("Buf not consumed")]
    TrailingCharacters,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
    #[error("Unknown error: {0}")]
    Unknown(String),
}

impl ParseFm2FileError {
    fn from_winnow_error(e: ContextError) -> Self {
        e.into()
    }
}

impl<I: Clone + Display> From<InputError<I>> for ParseFm2FileError {
    fn from(value: InputError<I>) -> Self {
        anyhow::anyhow!(value.to_string()).into()
    }
}

impl From<ContextError> for ParseFm2FileError {
    fn from(value: ContextError) -> Self {
        anyhow::anyhow!(value.to_string()).into()
    }
}

impl serde::de::Error for ParseFm2FileError {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        Self::Unknown(msg.to_string())
    }
}

struct SpaceSeparated<'a, 'de: 'a> {
    de: &'a mut SpacedKeyValueDeserialize<'de>,
}

impl<'a, 'de: 'a> SpaceSeparated<'a, 'de> {
    fn new(de: &'a mut SpacedKeyValueDeserialize<'de>) -> Self {
        Self { de }
    }
}

impl<'a, 'de: 'a> MapAccess<'de> for SpaceSeparated<'a, 'de> {
    type Error = ParseFm2FileError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: serde::de::DeserializeSeed<'de>,
    {
        if self.de.is_empty() {
            return Ok(None);
        }

        seed.deserialize(&mut *self.de).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        self.de.take_kv_sep_space()?;
        let r = seed.deserialize(&mut *self.de)?;
        self.de.take_endline()?;
        Ok(r)
    }
}
/// A custom serde deserializer to parse fm2 file header
struct SpacedKeyValueDeserialize<'de> {
    buf: &'de str,
}

impl<'de> SpacedKeyValueDeserialize<'de> {
    fn from_buf(buf: &'de str) -> Self {
        Self { buf }
    }

    fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    fn take_kv_sep_space(&mut self) -> Result<(), ParseFm2FileError> {
        space1
            .value(())
            .parse_next(&mut self.buf)
            .map_err(ParseFm2FileError::from_winnow_error)
    }

    fn take_endline(&mut self) -> Result<(), ParseFm2FileError> {
        line_ending
            .value(())
            .parse_next(&mut self.buf)
            .map_err(ParseFm2FileError::from_winnow_error)
    }
}

fn from_str<'a, T>(buf: &'a str) -> Result<T, ParseFm2FileError>
where
    T: Deserialize<'a>,
{
    let mut deserializer = SpacedKeyValueDeserialize::from_buf(buf);
    let t = T::deserialize(&mut deserializer)?;
    if deserializer.buf.is_empty() {
        Ok(t)
    } else {
        Err(ParseFm2FileError::TrailingCharacters)
    }
}

impl<'de, 'a> Deserializer<'de> for &'a mut SpacedKeyValueDeserialize<'de> {
    type Error = ParseFm2FileError;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_bool(
            alt(('0'.value(false), '1'.value(true)))
                .parse_next(&mut self.buf)
                .map_err(ParseFm2FileError::from_winnow_error)?,
        )
    }

    fn deserialize_i8<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_i16<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_i32(
            dec_int::<_, _, ContextError>
                .parse_next(&mut self.buf)
                .map_err(ParseFm2FileError::from)?,
        )
    }

    fn deserialize_i64<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u8<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u16<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u32<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u64<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_f32<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_f64<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_char<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_str(
            till_line_ending
                .parse_next(&mut self.buf)
                .map_err(ParseFm2FileError::from_winnow_error)?,
        )
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_seq<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_tuple<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Ok(visitor.visit_map(SpaceSeparated::new(self))?)
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_enum(
            alphanumeric1
                .parse_next(&mut self.buf)
                .map_err(ParseFm2FileError::from_winnow_error)?
                .to_owned()
                .into_deserializer(),
        )
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        let s = alphanumeric1
            .parse_next(&mut self.buf)
            .map_err(ParseFm2FileError::from_winnow_error)?;
        visitor.visit_str(s)
    }

    fn deserialize_ignored_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }
}

#[cfg(test)]
mod tests;
