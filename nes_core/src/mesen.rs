//! Writer for Mesen's `.mmo` movie format (Mesen 2).
//!
//! A Mesen movie is a zip archive holding `GameSettings.txt` and
//! `Input.txt`. `Input.txt` has one line per input poll. Each polled device
//! contributes a `|`-prefixed field, in Mesen's poll order:
//!
//! 1. the system action device, two characters (`R` = reset, `P` = power),
//! 2. controller port 1,
//! 3. controller port 2.
//!
//! A standard controller field is eight characters in Mesen's order
//! `UDLRSsBA` (up, down, left, right, start, select, B, A), with `.` for a
//! released button.

use crate::movie::{Fm2File, GamepadInput, InputLog};
use sha1::{Digest, Sha1};
use std::io::{Cursor, Write};
use zip::ZipWriter;
use zip::result::ZipError;
use zip::write::SimpleFileOptions;

/// Written to `GameSettings.txt`. Mesen 2 refuses movies whose version starts
/// with `0.` or `1.`, and movie format version 2 is the Mesen 2 format.
const MESEN_VERSION: &str = "2.0.0";
const MOVIE_FORMAT_VERSION: u32 = 2;
const DEFAULT_ROM_FILENAME: &str = "movie.nes";

#[derive(Debug, thiserror::Error)]
pub enum WriteMesenMovieError {
    #[error("four-score FM2 movies are not supported")]
    FourScoreUnsupported,
    #[error("frame {frame} uses an FDS/VS system action, which a Mesen movie cannot represent")]
    UnsupportedCommand { frame: usize },
    #[error("cannot write Mesen movie: {0}")]
    Zip(#[from] ZipError),
}

/// Serialize an FM2 movie as a Mesen `.mmo` archive.
///
/// `rom` is the optional ROM the FM2 was recorded against: its SHA-1 goes into
/// `GameSettings.txt`. `rom_filename` overrides the name from the FM2 header
/// (Mesen uses it to find the ROM; playback itself does not need it).
pub fn from_fm2(
    fm2: &Fm2File,
    rom: Option<&[u8]>,
    rom_filename: Option<&str>,
) -> Result<Vec<u8>, WriteMesenMovieError> {
    if fm2.header.fourscore {
        return Err(WriteMesenMovieError::FourScoreUnsupported);
    }

    let rom_filename = rom_filename
        .or(Some(fm2.header.rom_filename.as_str()))
        .filter(|name| !name.is_empty())
        .unwrap_or(DEFAULT_ROM_FILENAME);
    let pal = fm2.header.pal_flag == Some(true);
    let sha1 = rom.map(sha1_hex);
    let settings = settings_txt(rom_filename, sha1.as_deref(), pal);
    let inputs = inputs_txt(&fm2.input_logs)?;

    Ok(write_archive(&settings, &inputs)?)
}

fn write_archive(settings: &str, inputs: &str) -> Result<Vec<u8>, ZipError> {
    let mut writer = ZipWriter::new(Cursor::new(Vec::new()));
    let options = SimpleFileOptions::default();
    writer.start_file("GameSettings.txt", options)?;
    writer.write_all(settings.as_bytes())?;
    writer.start_file("Input.txt", options)?;
    writer.write_all(inputs.as_bytes())?;
    Ok(writer.finish()?.into_inner())
}

fn settings_txt(rom_filename: &str, sha1: Option<&str>, pal: bool) -> String {
    let mut out = String::new();
    let mut push = |key: &str, value: &str| {
        out.push_str(key);
        out.push(' ');
        out.push_str(value);
        out.push('\n');
    };
    push("MesenVersion", MESEN_VERSION);
    push("MovieFormatVersion", &MOVIE_FORMAT_VERSION.to_string());
    push("GameFile", rom_filename);
    if let Some(sha1) = sha1 {
        push("SHA1", sha1);
    }
    // Key casing matters: these are the names Mesen's own recorder emits.
    push("emu.consoleType", "Nes");
    push("video.integerFpsMode", "false");
    push("emulation.runAheadFrames", "0");
    push("game.dipSwitches", "0");
    push("nes.consoleType", "Nes001");
    push("nes.region", if pal { "Pal" } else { "Ntsc" });
    push("nes.ramPowerOnState", "AllZeros");
    push("nes.randomizeMapperPowerOnState", "false");
    push("nes.randomizeCpuPpuAlignment", "false");
    push("nes.disableOamAddrBug", "false");
    push("nes.disablePaletteRead", "false");
    push("nes.disablePpu2004Reads", "false");
    push("nes.disableGameGenieBusConflicts", "false");
    push("nes.disablePpuReset", "false");
    push("nes.enableOamDecay", "false");
    push("nes.enablePpu2000ScrollGlitch", "false");
    push("nes.enablePpu2006ScrollGlitch", "false");
    push("nes.enablePpuOamRowCorruption", "false");
    push("nes.restrictPpuAccessOnFirstFrame", "false");
    push("nes.enableCpuTestMode", "false");
    push("nes.enableDmcSampleDuplicationGlitch", "false");
    push("nes.enablePpuSpriteEvalBug", "false");
    push("nes.ppuExtraScanlinesAfterNmi", "0");
    push("nes.ppuExtraScanlinesBeforeNmi", "0");
    push("nes.lightDetectionRadius", "0");
    push("nes.port1.type", "NesController");
    push("nes.port2.type", "NesController");
    push("nes.expPort.type", "None");
    out
}

fn inputs_txt(input_logs: &[InputLog]) -> Result<String, WriteMesenMovieError> {
    let mut out = String::with_capacity(input_logs.len() * 24);
    for (frame, log) in input_logs.iter().enumerate() {
        let command = log.command;
        if command.fds_disk_insert() || command.fds_disk_select() || command.vs_insert_coin() {
            return Err(WriteMesenMovieError::UnsupportedCommand { frame });
        }
        out.push('|');
        out.push(if command.soft_reset() { 'R' } else { '.' });
        out.push(if command.hard_reset() { 'P' } else { '.' });
        out.push('|');
        push_pad(&mut out, log.port0);
        out.push('|');
        push_pad(&mut out, log.port1);
        out.push('\n');
    }
    Ok(out)
}

/// Append one standard-controller state in Mesen's `UDLRSsBA` order.
fn push_pad(out: &mut String, pad: GamepadInput) {
    out.push(if pad.up() { 'U' } else { '.' });
    out.push(if pad.down() { 'D' } else { '.' });
    out.push(if pad.left() { 'L' } else { '.' });
    out.push(if pad.right() { 'R' } else { '.' });
    out.push(if pad.start() { 'S' } else { '.' });
    out.push(if pad.select() { 's' } else { '.' });
    out.push(if pad.b() { 'B' } else { '.' });
    out.push(if pad.a() { 'A' } else { '.' });
}

fn sha1_hex(bytes: &[u8]) -> String {
    let mut hasher = Sha1::new();
    hasher.update(bytes);
    format!("{:x}", hasher.finalize())
}

#[cfg(test)]
mod tests;
