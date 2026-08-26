use ansi_term::Color;
use is_terminal::IsTerminal;
use nes_core::nes::NesMcu;
use nes_core::render::Render;
use nes_core::{Cpu, ExecuteResult, Plugin, SystemClock};
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::LazyLock;

static IS_TERMINAL: LazyLock<bool> = LazyLock::new(|| std::io::stdout().is_terminal());

const NAMETABLE_START: u16 = 0x2000;
const NAMETABLE_LEN: usize = 0x03C0;
const SCREEN_WIDTH: usize = 32;
const SCREEN_HEIGHT: usize = 30;

#[derive(Default)]
enum Decoder {
    #[default]
    Plain,
    TallText,
}

enum SuccessCondition {
    PassedOrFailed,
    MagicSuccessWord(String),
    MagicSuccessWordUnlessFailed(String),
}

pub struct NametableConsole {
    stop: Option<ExecuteResult>,
    decoder: Decoder,
    success_condition: SuccessCondition,
    last_lines: Vec<String>,
    sample_without_rendering: bool,
    full_nametable_scan: bool,
}

impl NametableConsole {
    pub fn new() -> Self {
        Self {
            stop: None,
            decoder: Decoder::Plain,
            success_condition: SuccessCondition::PassedOrFailed,
            last_lines: Vec::new(),
            sample_without_rendering: false,
            full_nametable_scan: false,
        }
    }

    pub fn with_magic_success_word(word: &str) -> Self {
        Self {
            success_condition: SuccessCondition::MagicSuccessWord(word.to_owned()),
            ..Self::new()
        }
    }

    pub fn with_magic_success_word_unless_failed(word: &str) -> Self {
        Self {
            success_condition: SuccessCondition::MagicSuccessWordUnlessFailed(word.to_owned()),
            ..Self::new()
        }
    }

    /// Opts the console into sampling while PPU rendering is disabled.
    /// blargg's shell zeroes PPUCTRL immediately after printing its final
    /// message and dead-loops, so a strictly rendering-gated sampler never
    /// sees the verdict text.
    pub fn sampling_without_rendering(mut self) -> Self {
        self.sample_without_rendering = true;
        self
    }

    /// Opts the console into scanning the whole nametable address space
    /// (0x2000-0x2FFF, through every mirroring fold) instead of the fixed
    /// 960-tile window at 0x2000. ROMs whose console scrolls (blargg's CPU
    /// test set v5 moves its text up as sub-tests complete) leave the fixed
    /// window holding a stale prefix, so only the full scan keeps seeing the
    /// verdict text.
    pub fn with_full_nametable_scan(mut self) -> Self {
        self.full_nametable_scan = true;
        self
    }

    pub fn with_tall_text_magic_success_word(word: &str) -> Self {
        Self {
            decoder: Decoder::TallText,
            success_condition: SuccessCondition::MagicSuccessWord(word.to_owned()),
            ..Self::new()
        }
    }
}

impl Default for NametableConsole {
    fn default() -> Self {
        Self::new()
    }
}

impl<R: Render> Plugin<NesMcu<R, ()>> for NametableConsole {
    fn start(&mut self, _: &Cpu<NesMcu<R, ()>>, _: SystemClock) {}

    fn end(&mut self, cpu: &Cpu<NesMcu<R, ()>>, _: SystemClock) {
        if !cpu.mcu().ppu().in_vblank()
            || (!self.sample_without_rendering && !cpu.mcu().ppu().rendering_enabled())
        {
            return;
        }

        let buf = read_console(cpu, &self.decoder, self.full_nametable_scan);
        if buf.is_empty() {
            return;
        }

        // Rolling diff against the last sampled screen: print only genuinely
        // new lines (right-trimmed, empty rows dropped), so ROMs that scroll
        // or re-dump their screen don't repeat themselves.
        let (lines, transcript) = next_transcript(&self.last_lines, &buf);
        if lines == self.last_lines {
            return;
        }

        output(transcript); // empty when every sampled line was already printed
        self.stop = evaluate_result(&buf, &self.success_condition);
        self.last_lines = lines;
    }

    fn should_stop(&self) -> ExecuteResult {
        self.stop.unwrap_or(ExecuteResult::Continue)
    }
}

/// Normalizes a raw console buffer into right-trimmed, non-empty lines and
/// computes what to print given the previously seen lines: a line prints once
/// when it first appears. Lines are deduplicated within a single sample
/// (address-space folding makes full-nametable scans see the same row several
/// times) and against the previous sample's line set, so screens that scroll
/// or re-dump themselves print each distinct line exactly once. Returns the
/// updated state and the transcript chunk to emit (empty when nothing is
/// new). Pure, so the rolling-diff policy is unit-testable without a machine.
fn next_transcript(last: &[String], buf: &str) -> (Vec<String>, String) {
    let mut counts: HashMap<&str, usize> = HashMap::new();
    for line in last {
        *counts.entry(line.as_str()).or_default() += 1;
    }
    let mut fresh = Vec::new();
    let mut lines = Vec::with_capacity(buf.lines().count());
    let mut seen = HashSet::new();
    for line in buf
        .lines()
        .map(str::trim_end)
        .filter(|line| !line.is_empty())
    {
        if !seen.insert(line) {
            continue;
        }
        match counts.get_mut(line) {
            Some(0) | None => fresh.push(line),
            Some(count) => *count -= 1,
        }
        lines.push(line.to_owned());
    }
    if fresh.is_empty() {
        return (lines, String::new());
    }
    let mut transcript = fresh.join("\n");
    transcript.push('\n');
    (lines, transcript)
}

fn read_console<R: Render>(
    cpu: &Cpu<NesMcu<R, ()>>,
    decoder: &Decoder,
    full_nametable_scan: bool,
) -> String {
    match decoder {
        Decoder::Plain => read_plain_console(cpu, full_nametable_scan),
        Decoder::TallText => read_tall_console(cpu),
    }
}

fn read_plain_console<R: Render>(cpu: &Cpu<NesMcu<R, ()>>, full_nametable_scan: bool) -> String {
    if full_nametable_scan {
        return read_full_nametable(cpu);
    }
    let mut buf = Vec::with_capacity(NAMETABLE_LEN);
    // Some ROMs (e.g. blargg's forum APU tests) place their text at an offset
    // instead of the nametable origin, so skip leading NUL tiles; the scan
    // still stops at the first NUL after the first non-NUL byte.
    let first_nonzero = (0..NAMETABLE_LEN as u16)
        .find(|&offset| cpu.mcu().read_vram(NAMETABLE_START + offset) != 0);
    let Some(start) = first_nonzero else {
        return String::new();
    };
    for offset in start..NAMETABLE_LEN as u16 {
        let value = cpu.mcu().read_vram(NAMETABLE_START + offset);
        if value == 0 {
            break;
        }
        buf.push(value);
        if offset % 32 == 31 {
            let trimmed = buf.trim_ascii_end();
            buf.truncate(trimmed.len());
            buf.push(b'\n');
        }
    }

    let mut r = String::from_utf8_lossy(&buf).as_ref().to_owned();
    loop {
        let last_len = r.len();
        r = r.replace("\n\n", "\n");
        if r.len() == last_len {
            break;
        }
    }
    r
}

/// Scans the whole nametable address space (0x2000-0x2FFF; mirroring folds
/// the physical 2 KiB twice) and joins every run of non-zero tiles with
/// newlines. Verdict matching is substring-based, so where the ROM's console
/// scroll placed each line doesn't matter.
fn read_full_nametable<R: Render>(cpu: &Cpu<NesMcu<R, ()>>) -> String {
    let mut all = Vec::with_capacity(0x1000);
    for offset in 0..0x1000u16 {
        all.push(cpu.mcu().read_vram(NAMETABLE_START + offset));
    }
    all.split(|&b| b == 0)
        .map(|run| String::from_utf8_lossy(run).to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

fn read_tall_console<R: Render>(cpu: &Cpu<NesMcu<R, ()>>) -> String {
    let mut lines = Vec::with_capacity(SCREEN_HEIGHT / 2);

    for row in (0..SCREEN_HEIGHT).step_by(2) {
        let mut line = String::with_capacity(SCREEN_WIDTH);
        for col in 0..SCREEN_WIDTH {
            let top = read_nametable_char(cpu, row, col);
            let bottom = read_nametable_char(cpu, row + 1, col);
            line.push(decode_tall_char(top, bottom).unwrap_or(' '));
        }
        let trimmed = line.trim_end();
        if !trimmed.is_empty() {
            lines.push(trimmed.to_owned());
        }
    }

    lines.join("\n")
}

fn read_nametable_char<R: Render>(cpu: &Cpu<NesMcu<R, ()>>, row: usize, col: usize) -> u8 {
    cpu.mcu()
        .read_vram(NAMETABLE_START + (row * SCREEN_WIDTH + col) as u16)
}

fn decode_tall_char(top: u8, bottom: u8) -> Option<char> {
    if top == 0 && bottom == 0 {
        return Some(' ');
    }

    if top & 1 != 0 || bottom != top.wrapping_add(1) {
        return None;
    }

    Some((top >> 1) as char)
}

fn evaluate_result(buf: &str, success_condition: &SuccessCondition) -> Option<ExecuteResult> {
    match success_condition {
        SuccessCondition::PassedOrFailed => {
            if contains_passed(buf) {
                Some(ExecuteResult::Stop(0))
            } else if contains_failed(buf) {
                Some(ExecuteResult::Stop(1))
            } else {
                None
            }
        }
        SuccessCondition::MagicSuccessWord(magic_success_word) => buf
            .contains(magic_success_word)
            .then_some(ExecuteResult::Stop(0)),
        SuccessCondition::MagicSuccessWordUnlessFailed(magic_success_word) => {
            if contains_failed(buf) {
                Some(ExecuteResult::Stop(1))
            } else {
                buf.contains(magic_success_word)
                    .then_some(ExecuteResult::Stop(0))
            }
        }
    }
}

fn contains_passed(s: &str) -> bool {
    s.contains("PASSED") || s.contains("Passed\n")
}

fn contains_nonzero_error_count(s: &str) -> bool {
    s.match_indices("Errors:").any(|(i, _)| {
        s[i + "Errors:".len()..]
            .trim_start()
            .starts_with(|c: char| c.is_ascii_digit() && c != '0')
    })
}

fn contains_failed(s: &str) -> bool {
    s.contains("FAILED")
        || s.contains("Error ")
        || s.contains("Failed")
        || contains_nonzero_error_count(s)
}

fn output<S: AsRef<str>>(s: S) {
    if s.as_ref().is_empty() {
        return;
    }

    if *IS_TERMINAL {
        print!("{}", Color::Green.paint(s.as_ref()));
    } else {
        print!("{}", s.as_ref());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decodes_tall_text_characters() {
        assert_eq!(decode_tall_char(b'A' << 1, (b'A' << 1) + 1), Some('A'));
        assert_eq!(decode_tall_char(0, 0), Some(' '));
        assert_eq!(decode_tall_char(3, 4), None);
    }

    #[test]
    fn matches_magic_success_word() {
        assert_eq!(
            evaluate_result(
                "Accessible PRG banks:\n0123456789ABCDEF",
                &SuccessCondition::MagicSuccessWord("0123456789ABCDEF".to_owned())
            ),
            Some(ExecuteResult::Stop(0))
        );
    }

    #[test]
    fn magic_word_unless_failed_rejects_epilogue_with_errors() {
        let cond =
            || SuccessCondition::MagicSuccessWordUnlessFailed("All tests complete".to_owned());
        assert_eq!(
            evaluate_result("Test: 01-implied\nFailed\nAll tests complete\n", &cond()),
            Some(ExecuteResult::Stop(1))
        );
        assert_eq!(
            evaluate_result("Running tests...\nErrors: 2\nAll tests complete\n", &cond()),
            Some(ExecuteResult::Stop(1))
        );
    }

    #[test]
    fn magic_word_unless_failed_accepts_clean_completion() {
        let cond = SuccessCondition::MagicSuccessWordUnlessFailed("All tests complete".to_owned());
        assert_eq!(
            evaluate_result("Running tests...\nAll tests complete\nErrors: 0\n", &cond),
            Some(ExecuteResult::Stop(0))
        );
        assert_eq!(
            evaluate_result("Running tests...\nTest: 01-implied\n", &cond),
            None
        );
    }

    #[test]
    fn transcript_drops_blank_and_padded_rows() {
        let (lines, out) = next_transcript(&[], "  \n   Running tests...   \n\n\n 01-implied \n");
        assert_eq!(out, "   Running tests...\n 01-implied\n");
        assert_eq!(lines, ["   Running tests...", " 01-implied"]);
    }

    #[test]
    fn transcript_suppresses_unchanged_screen_resamples() {
        let screen = "-----------------------------\n 01-implied\n 02-immediate\n";
        let (state, out) = next_transcript(&[], screen);
        assert_eq!(
            out,
            "-----------------------------\n 01-implied\n 02-immediate\n"
        );
        let (_, out) = next_transcript(&state, screen);
        assert_eq!(out, "");
    }

    #[test]
    fn transcript_emits_only_new_line_when_screen_scrolls() {
        let (s1, _) = next_transcript(&[], "A\nB\nC\n");
        let (s2, out) = next_transcript(&s1, "B\nC\nD\n");
        assert_eq!(out, "D\n");
        let (_, out) = next_transcript(&s2, "C\nD\nA\n");
        assert_eq!(out, "A\n");
    }

    #[test]
    fn transcript_collapses_lines_duplicated_within_one_sample() {
        // Full-nametable scans fold mirrored pages, duplicating every row;
        // one sample must print each distinct line exactly once.
        let (_, out) = next_transcript(&[], "A\nA\nA\nA\n");
        assert_eq!(out, "A\n");
        let (s1, _) = next_transcript(&[], "A\nB\nC\nC\n");
        let (_, out) = next_transcript(&s1, "B\nC\nD\nD\n");
        assert_eq!(out, "D\n");
    }

    #[test]
    fn transcript_state_tracks_lines_scrolled_off_screen() {
        let (s1, _) = next_transcript(&[], "A\nB\n");
        let (s2, out) = next_transcript(&s1, "B\n");
        assert_eq!(out, "");
        let (_, out) = next_transcript(&s2, "B\nA\n");
        assert_eq!(out, "A\n");
    }
}
