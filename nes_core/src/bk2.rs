//! BizHawk BK2 movie playback (power-on NES movies, dual standard gamepads).
//!
//! A `.bk2` file is a plain zip: `Header.txt` (`Key value` lines),
//! `Input Log.txt` (`[Input]` … `LogKey:` … `|…|` frame lines … `[/Input]`),
//! plus optional lumps (`SyncSettings.json`, `Comments.txt`, `Subtitles.txt`)
//! and savestate-anchor lumps (`Core.bin`, `CoreText.txt`, `CoreState`,
//! `CoreState.bin`, `MovieSaveRam.bin`).
//!
//! The parse seam mirrors [`crate::movie::parse`]: [`parse`] takes raw bytes
//! and returns a [`Bk2Movie`] (header plus per-frame [`InputLog`]s reusing the
//! FM2 [`Command`](crate::movie::Command) / [`GamepadInput`](crate::movie::GamepadInput)
//! value types). Trust policy lives in [`check_movie`]: hard refusals
//! ([`Bk2Refusal`]) for non-NES platforms, non-NTSC regions, non-gamepad
//! controls and savestate-anchored starts, warnings for everything else.

use std::{
    collections::HashMap,
    io::{Cursor, Read},
};

use crate::{
    ines::INesFile,
    movie::{Command, GamepadInput, InputLog},
};

/// A parsed BK2 movie: header fields plus one [`InputLog`] per `|…|` frame line.
#[derive(Debug)]
pub struct Bk2Movie {
    pub header: Bk2Header,
    pub input_logs: Box<[InputLog]>,
    /// Parsed `SyncSettings.json`, if the archive contained one.
    pub sync_settings: Option<serde_json::Value>,
    /// Which anchor lump (if any) makes this movie savestate-anchored.
    pub anchored: Option<AnchoredKind>,
}

/// Header.txt parsed as raw `Key -> value` pairs (key lookup is case-sensitive
/// on the canonical BizHawk spellings; unknown keys are kept, not rejected).
#[derive(Debug, Default)]
pub struct Bk2Header {
    fields: HashMap<String, String>,
}

impl Bk2Header {
    fn get(&self, key: &str) -> Option<&str> {
        self.fields.get(key).map(String::as_str)
    }

    /// `Platform` header key (`NES` for the movies we play).
    pub fn platform(&self) -> Option<&str> {
        self.get("Platform")
    }

    /// `Core` header key (`NesHawk` for the core we emulate).
    pub fn core(&self) -> Option<&str> {
        self.get("Core")
    }

    /// `BoardName` header key (mapper board, informational only).
    pub fn board_name(&self) -> Option<&str> {
        self.get("BoardName")
    }

    /// `PAL` header key (`1` when the recording region is PAL).
    pub fn pal_flag(&self) -> Option<&str> {
        self.get("PAL")
    }

    /// Raw digest candidates: the `SHA1` key (misleading name — it may hold a
    /// CRC32, MD5 or SHA1 hex string) plus any explicit
    /// `SHA256`/`MD5`/`CRC32` keys.
    pub fn digest_candidates(&self) -> Vec<&str> {
        ["SHA1", "SHA256", "MD5", "CRC32"]
            .into_iter()
            .filter_map(|k| self.get(k))
            .collect()
    }
}

/// Which anchor lump marks a movie as savestate-anchored. Detection is by
/// lump presence (case-insensitive), regardless of the `StartsFromSavestate`
/// flag value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnchoredKind {
    CoreState,
    SaveRam,
}

impl AnchoredKind {
    fn lump_hint(self) -> &'static str {
        match self {
            AnchoredKind::CoreState => "CoreState",
            AnchoredKind::SaveRam => "SaveRAM",
        }
    }
}

/// Hard refusal: the movie cannot sync, so playback bails (non-zero exit).
#[derive(thiserror::Error, Debug, PartialEq, Eq)]
pub enum Bk2Refusal {
    #[error("BK2 movie targets platform '{found}': only NES movies can be played back")]
    Platform { found: String },
    #[error("BK2 movie targets non-NTSC region ({detail}): timing differs, playback refused")]
    Region { detail: String },
    #[error(
        "BK2 movie uses non-gamepad controls ({detail}): only dual standard gamepads are supported"
    )]
    Controls { detail: String },
    #[error(
        "BK2 movie is savestate-anchored ({kind} lump present): frame 1 is not power-on; refusing (retry with --play-from-power-on to play from power-on anyway)"
    )]
    Anchored { kind: &'static str },
}

/// Ways [`parse`] can fail. Anchor / platform / region problems are NOT parse
/// errors — they surface through [`check_movie`] so the `--play-from-power-on`
/// override can downgrade the anchored case to warn-and-play.
#[derive(thiserror::Error, Debug)]
pub enum Bk2ParseError {
    #[error("invalid zip archive: {0}")]
    Zip(#[from] zip::result::ZipError),
    #[error("I/O error reading zip entry: {0}")]
    Io(#[from] std::io::Error),
    #[error("BK2 archive is missing required entry '{0}'")]
    MissingEntry(&'static str),
    #[error("Header.txt is not valid text: {0}")]
    HeaderText(String),
    #[error("Input Log.txt is not valid text: {0}")]
    LogText(String),
    #[error("Input Log.txt has no LogKey: line")]
    MissingLogKey,
    #[error("frame {index}: {problem}")]
    Frame { index: usize, problem: String },
    #[error("SyncSettings.json is not valid JSON: {0}")]
    SyncSettings(String),
}

/// Parse BK2 bytes into a [`Bk2Movie`], mirroring the FM2 parse seam.
///
/// Never refuses on trust grounds: platform, region, controls and anchor
/// policy are reported by [`check_movie`] instead.
pub fn parse(buf: &[u8]) -> Result<Bk2Movie, Bk2ParseError> {
    let mut archive = zip::ZipArchive::new(Cursor::new(buf))?;
    let names: Vec<String> = archive.file_names().map(str::to_owned).collect();
    let find = |want: &str| names.iter().find(|n| n.eq_ignore_ascii_case(want)).cloned();

    let header_name = find("Header.txt").ok_or(Bk2ParseError::MissingEntry("Header.txt"))?;
    let log_name = find("Input Log.txt").ok_or(Bk2ParseError::MissingEntry("Input Log.txt"))?;

    let mut header_text = String::new();
    archive
        .by_name(&header_name)?
        .read_to_string(&mut header_text)
        .map_err(|e| Bk2ParseError::HeaderText(e.to_string()))?;
    let header = parse_header(&header_text);

    let mut log_text = String::new();
    archive
        .by_name(&log_name)?
        .read_to_string(&mut log_text)
        .map_err(|e| Bk2ParseError::LogText(e.to_string()))?;

    let sync_settings = find("SyncSettings.json")
        .map(|name| {
            let mut text = String::new();
            archive
                .by_name(&name)?
                .read_to_string(&mut text)
                .map_err(|e| Bk2ParseError::LogText(e.to_string()))?;
            serde_json::from_str(&text).map_err(|e| Bk2ParseError::SyncSettings(e.to_string()))
        })
        .transpose()?;

    let anchored = detect_anchor(&names);
    let input_logs = parse_input_log(&log_text)?;

    Ok(Bk2Movie {
        header,
        input_logs: input_logs.into(),
        sync_settings,
        anchored,
    })
}

fn parse_header(text: &str) -> Bk2Header {
    let mut fields = HashMap::new();
    for line in text.lines() {
        let line = line.trim_end_matches(['\r', '\n']);
        if line.is_empty() {
            continue;
        }
        match line.find([' ', '\t']) {
            Some(i) => {
                fields.insert(line[..i].to_owned(), line[i + 1..].to_owned());
            }
            None => {
                fields.insert(line.to_owned(), String::new());
            }
        }
    }
    Bk2Header { fields }
}

/// Anchor lumps recognised case-insensitively. `Framebuffer.bmp` is a
/// savestate thumbnail, not an anchor, and is ignored.
fn detect_anchor(names: &[String]) -> Option<AnchoredKind> {
    let lower: Vec<String> = names.iter().map(|n| n.to_ascii_lowercase()).collect();
    if ["core.bin", "coretext.txt", "corestate", "corestate.bin"]
        .iter()
        .any(|w| lower.iter().any(|n| n == w))
    {
        return Some(AnchoredKind::CoreState);
    }
    if lower.iter().any(|n| n == "moviesaveram.bin") {
        return Some(AnchoredKind::SaveRam);
    }
    None
}

/// One parsed LogKey group: the console group holds `Power`/`Reset` slots,
/// pad groups hold one slot per button name (or axis slot).
#[derive(Debug)]
struct LogColumn {
    kind: ColumnKind,
}

#[derive(Debug)]
enum ColumnKind {
    Power,
    Reset,
    PadButton { pad: usize, button: Button },
    Axis,
    Skip,
}

#[derive(Debug, Clone, Copy)]
enum Button {
    Up,
    Down,
    Left,
    Right,
    Start,
    Select,
    B,
    A,
}

fn parse_button_name(base: &str) -> Option<Button> {
    match base.to_ascii_lowercase().as_str() {
        "up" => Some(Button::Up),
        "down" => Some(Button::Down),
        "left" => Some(Button::Left),
        "right" => Some(Button::Right),
        "start" => Some(Button::Start),
        "select" => Some(Button::Select),
        "b" => Some(Button::B),
        "a" => Some(Button::A),
        _ => None,
    }
}

/// Axis slots render as one logical column of 5 space-padded digits plus a
/// comma (6 chars). Only matched by name, never by position.
fn is_axis_name(base: &str) -> bool {
    let lower = base.to_ascii_lowercase();
    lower.contains("cycle") || lower.ends_with(" x") || lower.ends_with(" y")
}

fn parse_log_key(line: &str) -> Result<Vec<Vec<LogColumn>>, Bk2ParseError> {
    let spec = line
        .strip_prefix("LogKey:")
        .ok_or(Bk2ParseError::MissingLogKey)?;
    // Groups are `#`-separated; names inside a group are `|`-separated.
    // The leading `#` leaves an empty first split — logical group 0 is the
    // console group, 1/2 are P1/P2, anything after is skipped by name.
    let mut groups = Vec::new();
    for raw in spec.split('#') {
        if raw.trim_matches(['|', ' ']).is_empty() {
            continue;
        }
        let group_index = groups.len();
        let mut columns = Vec::new();
        for name in raw.split('|') {
            let name = name.trim();
            if name.is_empty() {
                continue;
            }
            columns.push(resolve_column(group_index, name));
        }
        if !columns.is_empty() {
            groups.push(columns);
        }
    }
    if groups.is_empty() {
        return Err(Bk2ParseError::MissingLogKey);
    }
    Ok(groups)
}

/// Resolve one LogKey column by name. Group 0 is the console group
/// (`Power`, `Reset`); groups 1 and 2 are P1 and P2; anything else
/// (extra pads, axes, coins) is skipped by name, never guessed.
fn resolve_column(group_index: usize, name: &str) -> LogColumn {
    // Strip a `Pn ` deck prefix when present (`P1 Up` -> `Up`).
    let base = match name.split_once(' ') {
        Some((deck, rest)) if deck.len() == 2 && deck.starts_with('P') => rest,
        _ => name,
    };
    let kind = if group_index == 0 {
        match base.to_ascii_lowercase().as_str() {
            "power" => ColumnKind::Power,
            "reset" => ColumnKind::Reset,
            _ if is_axis_name(base) => ColumnKind::Axis,
            _ => ColumnKind::Skip,
        }
    } else if group_index == 1 || group_index == 2 {
        let pad = group_index - 1;
        if is_axis_name(base) {
            ColumnKind::Axis
        } else {
            match parse_button_name(base) {
                Some(button) => ColumnKind::PadButton { pad, button },
                None => ColumnKind::Skip,
            }
        }
    } else {
        ColumnKind::Skip
    };
    LogColumn { kind }
}

fn parse_input_log(text: &str) -> Result<Vec<InputLog>, Bk2ParseError> {
    let mut log_key: Option<Vec<Vec<LogColumn>>> = None;
    let mut in_input = false;
    let mut frames = Vec::new();

    for line in text.lines() {
        let line = line.trim_end_matches('\r');
        if line == "[Input]" {
            in_input = true;
            continue;
        }
        if line == "[/Input]" {
            break;
        }
        if !in_input {
            continue;
        }
        if line.starts_with("LogKey:") {
            log_key = Some(parse_log_key(line)?);
            continue;
        }
        if line.starts_with('|') {
            let groups = log_key.as_ref().ok_or(Bk2ParseError::MissingLogKey)?;
            let index = frames.len();
            frames.push(
                parse_frame_line(line, groups)
                    .map_err(|problem| Bk2ParseError::Frame { index, problem })?,
            );
        }
    }

    if log_key.is_none() {
        return Err(Bk2ParseError::MissingLogKey);
    }
    Ok(frames)
}

/// A cell is off for `.` (or space, like the FM2 parser); any other
/// character counts as pressed, so mnemonic variants across BizHawk
/// versions all map correctly.
fn cell_on(cell: u8) -> bool {
    cell != b'.' && cell != b' '
}

fn parse_frame_line(line: &str, groups: &[Vec<LogColumn>]) -> Result<InputLog, String> {
    let bytes = line.as_bytes();
    // Segments between `|`s; segment 0 is before the first pipe.
    let mut segments: Vec<&[u8]> = Vec::new();
    let mut start = 0;
    for (i, b) in bytes.iter().enumerate() {
        if *b == b'|' {
            segments.push(&bytes[start..i]);
            start = i + 1;
        }
    }
    segments.push(&bytes[start..]);

    // `|a|b|c|` splits into ["", a, b, c, ""]: pad groups live at 1.. .
    let mut command = Command::new();
    let mut pads = [GamepadInput::new(), GamepadInput::new()];

    for (group_index, columns) in groups.iter().enumerate().take(3) {
        let segment = segments.get(group_index + 1).copied().unwrap_or_default();
        let mut pos = 0;
        for column in columns {
            match column.kind {
                ColumnKind::Power => {
                    let cell = *segment
                        .get(pos)
                        .ok_or_else(|| "console segment too short for Power column".to_owned())?;
                    if cell_on(cell) {
                        command = command.with_hard_reset(true);
                    }
                    pos += 1;
                }
                ColumnKind::Reset => {
                    let cell = *segment
                        .get(pos)
                        .ok_or_else(|| "console segment too short for Reset column".to_owned())?;
                    if cell_on(cell) {
                        command = command.with_soft_reset(true);
                    }
                    pos += 1;
                }
                ColumnKind::PadButton { pad, button } => {
                    let cell = *segment
                        .get(pos)
                        .ok_or_else(|| "pad segment too short for button column".to_owned())?;
                    if cell_on(cell) {
                        pads[pad] = set_button(pads[pad], button, true);
                    }
                    pos += 1;
                }
                ColumnKind::Axis => {
                    if segment.len() < pos + 6 {
                        return Err("segment too short for axis column".to_owned());
                    }
                    pos += 6;
                }
                ColumnKind::Skip => {
                    if segment.len() <= pos {
                        return Err("segment too short for column".to_owned());
                    }
                    pos += 1;
                }
            }
        }
    }

    Ok(InputLog::new(command, pads[0], pads[1]))
}

fn set_button(mut input: GamepadInput, button: Button, pressed: bool) -> GamepadInput {
    match button {
        Button::Up => input.set_up(pressed),
        Button::Down => input.set_down(pressed),
        Button::Left => input.set_left(pressed),
        Button::Right => input.set_right(pressed),
        Button::Start => input.set_start(pressed),
        Button::Select => input.set_select(pressed),
        Button::B => input.set_b(pressed),
        Button::A => input.set_a(pressed),
    }
    input
}

/// Trust policy for a parsed movie against the loaded ROM.
///
/// Returns the warnings to print on stderr (playback continues, exit 0), or
/// a [`Bk2Refusal`] naming why playback cannot sync. Savestate-anchored
/// movies are refused unless `allow_power_on_override` (the
/// `--play-from-power-on` flag) downgrades them to warn-and-play.
pub fn check_movie(
    movie: &Bk2Movie,
    rom: &INesFile,
    allow_power_on_override: bool,
) -> Result<Vec<String>, Bk2Refusal> {
    // Platform gate.
    match movie.header.platform() {
        Some(p) if p.eq_ignore_ascii_case("NES") => {}
        Some(p) => {
            return Err(Bk2Refusal::Platform {
                found: p.to_owned(),
            });
        }
        None => {
            return Err(Bk2Refusal::Platform {
                found: "(missing)".to_owned(),
            });
        }
    }

    // Effective region: SyncSettings RegionOverride wins, else the PAL flag.
    // Anything but NTSC refuses — cycle timing would desync with certainty.
    let override_region = movie
        .sync_settings
        .as_ref()
        .and_then(|v| v.get("RegionOverride"))
        .and_then(serde_json::Value::as_str)
        .unwrap_or("Default");
    let pal = movie.header.pal_flag().is_some_and(|v| v.trim() == "1");
    let mut warnings = Vec::new();
    let effective_ntsc = if override_region.eq_ignore_ascii_case("PAL")
        || override_region.eq_ignore_ascii_case("Dendy")
    {
        false
    } else {
        // Default / NTSC / absent override: NTSC unless the PAL flag says so.
        !pal
    };
    if !effective_ntsc {
        return Err(Bk2Refusal::Region {
            detail: format!("RegionOverride={override_region}, PAL flag={pal}"),
        });
    }
    if pal && override_region.eq_ignore_ascii_case("NTSC") {
        warnings.push(
            "BK2 PAL header flag conflicts with RegionOverride=NTSC; playing as NTSC".to_owned(),
        );
    }

    // Controls gate: anything beyond dual standard gamepads refuses.
    if let Some(detail) = non_gamepad_controls(movie) {
        return Err(Bk2Refusal::Controls { detail });
    }

    // Savestate / SaveRAM anchors refuse by default (frame 1 is not power-on,
    // so power-on play provably desyncs); the override downgrades to warn.
    if let Some(kind) = movie.anchored {
        if allow_power_on_override {
            warnings.push(format!(
                "BK2 movie is savestate-anchored ({} lump present); playing from power-on anyway by request",
                kind.lump_hint()
            ));
        } else {
            return Err(Bk2Refusal::Anchored {
                kind: kind.lump_hint(),
            });
        }
    } else if starts_from_savestate_flag(movie) {
        // The file claims a savestate start but carries no anchor lump: frame
        // 1 still is not power-on, so refuse the same way.
        if allow_power_on_override {
            warnings.push(
                "BK2 movie claims StartsFromSavestate with no anchor lump; playing from power-on anyway by request"
                    .to_owned(),
            );
        } else {
            return Err(Bk2Refusal::Anchored {
                kind: "claimed savestate",
            });
        }
    }

    // Warn-level checks: digest, board, core, unhonoured sync settings.
    warnings.extend(check_digest(movie, rom));
    warnings.extend(check_board_and_core(movie));
    warnings.extend(check_sync_settings(movie));

    Ok(warnings)
}

fn starts_from_savestate_flag(movie: &Bk2Movie) -> bool {
    let flag = |key: &str| {
        movie.header.get(key).is_some_and(|v| {
            v.trim().eq_ignore_ascii_case("1") || v.trim().eq_ignore_ascii_case("true")
        })
    };
    flag("StartsFromSavestate") || flag("StartsFromSaveRam")
}

/// Substrings (lowercase) marking a SyncSettings control device that is not
/// a standard NES gamepad.
const NON_GAMEPAD_MARKERS: &[&str] = &[
    "zapper",
    "arkanoid",
    "fourscore",
    "four score",
    "multitap",
    "powerpad",
    "power pad",
    "expansion",
    "famicom",
    "subor",
    "hyper",
    "mahjong",
    "oeka",
    "pec-586",
    "party tap",
    "exciting boxing",
    "vs ",
];

fn collect_strings(value: &serde_json::Value, out: &mut Vec<String>) {
    match value {
        serde_json::Value::String(s) => out.push(s.clone()),
        serde_json::Value::Array(items) => {
            for item in items {
                collect_strings(item, out);
            }
        }
        serde_json::Value::Object(map) => {
            for (k, v) in map {
                out.push(k.clone());
                collect_strings(v, out);
            }
        }
        _ => {}
    }
}

fn non_gamepad_controls(movie: &Bk2Movie) -> Option<String> {
    let settings = movie.sync_settings.as_ref()?;
    let controls = settings
        .get("Controls")
        .or_else(|| settings.get("controls"))?;
    let mut strings = Vec::new();
    collect_strings(controls, &mut strings);
    for s in &strings {
        let lower = s.to_ascii_lowercase();
        // Plain "gamepad"/"controller"/"none"/port labels are fine.
        if [
            "gamepad",
            "controller",
            "nes controller",
            "none",
            "port1",
            "port2",
            "port 1",
            "port 2",
        ]
        .iter()
        .any(|ok| lower == *ok || lower.contains("gamepad"))
        {
            continue;
        }
        if NON_GAMEPAD_MARKERS.iter().any(|m| lower.contains(m)) {
            return Some(s.clone());
        }
        // Any other explicit non-empty device name is suspect: refuse rather
        // than silently desync. Bare port labels and gamepads pass above.
        if !lower.is_empty()
            && !["port1", "port2", "port 1", "port 2", "player1", "player2"]
                .contains(&lower.as_str())
        {
            // Unknown device name: only refuse when it names hardware we
            // know we cannot emulate. Names like "NES Controller" contain
            // "controller" and passed above; anything left that is not a
            // known-good label refuses if it is not empty and not numeric.
            if lower.chars().any(|c| c.is_alphabetic()) && lower.len() > 3 {
                return Some(s.clone());
            }
        }
    }
    None
}

fn rom_hashes(rom: &INesFile) -> (u32, [u8; 16], [u8; 20], [u8; 32]) {
    use sha1::Digest as _;
    let mut crc = crc32fast::Hasher::new();
    crc.update(rom.read_prg_rom());
    crc.update(rom.read_chr_rom());
    let crc = crc.finalize();

    let mut md5 = md5::Context::new();
    md5.consume(rom.read_prg_rom());
    md5.consume(rom.read_chr_rom());
    let md5 = md5.finalize().0;

    let mut sha1 = sha1::Sha1::new();
    sha1.update(rom.read_prg_rom());
    sha1.update(rom.read_chr_rom());
    let sha1 = sha1.finalize().into();

    let mut sha256 = sha2::Sha256::new();
    sha256.update(rom.read_prg_rom());
    sha256.update(rom.read_chr_rom());
    let sha256 = sha256.finalize().into();

    (crc, md5, sha1, sha256)
}

fn hex_bytes(text: &str) -> Option<Vec<u8>> {
    let t = text.trim().trim_start_matches("0x").trim();
    if t.len().is_multiple_of(2) && !t.is_empty() && t.chars().all(|c| c.is_ascii_hexdigit()) {
        (0..t.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&t[i..i + 2], 16).ok())
            .collect()
    } else {
        None
    }
}

fn check_digest(movie: &Bk2Movie, rom: &INesFile) -> Vec<String> {
    let candidates = movie.header.digest_candidates();
    if candidates.is_empty() {
        return Vec::new();
    }
    let (crc, md5, sha1, sha256) = rom_hashes(rom);
    for candidate in &candidates {
        let Some(bytes) = hex_bytes(candidate) else {
            continue;
        };
        let matched = match bytes.len() {
            4 => bytes.as_slice() == crc.to_be_bytes(),
            16 => bytes.as_slice() == md5,
            20 => bytes.as_slice() == sha1,
            32 => bytes.as_slice() == sha256,
            _ => false,
        };
        if matched {
            return Vec::new();
        }
    }
    vec![format!(
        "Warning: BK2 ROM digest does not match the loaded ROM ({}); playing anyway",
        candidates.join(", ")
    )]
}

fn check_board_and_core(movie: &Bk2Movie) -> Vec<String> {
    let mut warnings = Vec::new();
    if let Some(board) = movie.header.board_name()
        && !board.trim().is_empty()
    {
        warnings.push(format!(
            "Warning: BK2 BoardName '{board}' is informational only; the board re-derives from ROM data"
        ));
    }
    if let Some(core) = movie.header.core()
        && !core.eq_ignore_ascii_case("NesHawk")
    {
        warnings.push(format!(
            "Warning: BK2 Core '{core}' is not NesHawk; input still applies but timing may drift"
        ));
    }
    warnings
}

fn check_sync_settings(movie: &Bk2Movie) -> Vec<String> {
    let mut warnings = Vec::new();
    let Some(settings) = movie.sync_settings.as_ref() else {
        return warnings;
    };
    if let Some(board) = settings.get("BoardProperties")
        && !is_empty_json(board)
    {
        warnings.push(
            "Warning: BK2 SyncSettings BoardProperties are not honoured; playing with the ROM-derived board"
                .to_owned(),
        );
    }
    if let Some(pattern) = settings
        .get("InitialWRamStatePattern")
        .and_then(serde_json::Value::as_str)
        && !pattern.eq_ignore_ascii_case("Default")
    {
        warnings.push(format!(
            "Warning: BK2 SyncSettings InitialWRamStatePattern '{pattern}' is not honoured; playing with power-on WRAM init"
        ));
    }
    for key in ["VSDipswitches", "IsVS"] {
        if let Some(v) = settings.get(key)
            && !is_empty_json(v)
        {
            warnings.push(format!(
                "Warning: BK2 SyncSettings '{key}' is not honoured; VS hardware is out of scope"
            ));
        }
    }
    warnings
}

fn is_empty_json(value: &serde_json::Value) -> bool {
    match value {
        serde_json::Value::Null => true,
        serde_json::Value::Bool(false) => true,
        serde_json::Value::String(s) => {
            s.is_empty() || s.eq_ignore_ascii_case("default") || s == "0"
        }
        serde_json::Value::Array(items) => items.is_empty(),
        serde_json::Value::Object(map) => map.is_empty(),
        _ => false,
    }
}

#[cfg(test)]
mod tests;
