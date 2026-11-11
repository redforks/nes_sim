//! Tests for the BK2 parser seam and trust policy.
//!
//! The three synthetic fixtures under `testdata/bk2/` pin the container and
//! grammar corners; policy corners (refuse vs warn) are pinned with
//! in-memory BK2s built by [`build_bk2`], never by asserting ROM matching
//! against the fixtures' illustrative ROM identity fields.

use super::*;
use std::io::Write as _;

const BLANK_BK2: &[u8] = include_bytes!("../../../testdata/bk2/bk2-blank-3f.bk2");
const INPUTS_BK2: &[u8] = include_bytes!("../../../testdata/bk2/bk2-inputs-5f.bk2");
const RESET_POWER_BK2: &[u8] = include_bytes!("../../../testdata/bk2/bk2-reset-power-3f.bk2");

const LOG_KEY: &str = "LogKey:#Power|Reset|#P1 Up|P1 Down|P1 Left|P1 Right|P1 Start|P1 Select|P1 B|P1 A|#P2 Up|P2 Down|P2 Left|P2 Right|P2 Start|P2 Select|P2 B|P2 A|";

/// Minimal valid NROM ROM (1x16K PRG + 8K CHR, all zero) for policy tests.
/// Its hashes never match fixture digests, which is exactly the warn path.
fn test_rom() -> INesFile {
    let mut image = Vec::new();
    image.extend_from_slice(b"NES\x1a");
    image.push(1); // 1 x 16 KiB PRG
    image.push(1); // 1 x 8 KiB CHR
    image.extend_from_slice(&[0u8; 10]);
    image.extend_from_slice(&[0u8; 16 * 1024]);
    image.extend_from_slice(&[0u8; 8 * 1024]);
    INesFile::new(image).expect("synthetic test ROM must parse")
}

/// Build a minimal in-memory BK2: Header.txt + Input Log.txt plus extra lumps.
fn build_bk2(header: &[&str], frames: &[&str], extra: &[(&str, &str)]) -> Vec<u8> {
    build_bk2_with_log_key(header, LOG_KEY, frames, extra)
}

fn build_bk2_with_log_key(
    header: &[&str],
    log_key: &str,
    frames: &[&str],
    extra: &[(&str, &str)],
) -> Vec<u8> {
    let mut out = Cursor::new(Vec::new());
    let mut zip = zip::ZipWriter::new(&mut out);
    let options = zip::write::SimpleFileOptions::default();
    zip.start_file("Header.txt", options).unwrap();
    for line in header {
        writeln!(zip, "{line}").unwrap();
    }
    zip.start_file("Input Log.txt", options).unwrap();
    writeln!(zip, "[Input]").unwrap();
    writeln!(zip, "{log_key}").unwrap();
    for frame in frames {
        writeln!(zip, "{frame}").unwrap();
    }
    writeln!(zip, "[/Input]").unwrap();
    for (name, data) in extra {
        zip.start_file(*name, options).unwrap();
        zip.write_all(data.as_bytes()).unwrap();
    }
    zip.finish().unwrap();
    out.into_inner()
}

fn base_header() -> Vec<&'static str> {
    vec![
        "MovieVersion 1",
        "emuVersion 2.9.1",
        "Platform NES",
        "GameName synthetic",
        "Core NesHawk",
        "StartsFromSavestate 0",
    ]
}

fn blank_frames3() -> Vec<&'static str> {
    vec![
        "|..|........|........|",
        "|..|........|........|",
        "|..|........|........|",
    ]
}

#[test]
fn blank_fixture_parses_to_three_all_off_inputs() {
    let movie = parse(BLANK_BK2).expect("blank fixture must parse");
    assert_eq!(movie.input_logs.len(), 3);
    for log in movie.input_logs.iter() {
        assert_eq!(
            *log,
            InputLog::new(Command::new(), GamepadInput::new(), GamepadInput::new())
        );
    }
    assert!(movie.anchored.is_none());
}

#[test]
fn inputs_fixture_parses_to_expected_per_frame_inputs() {
    let movie = parse(INPUTS_BK2).expect("inputs fixture must parse");
    assert_eq!(movie.input_logs.len(), 5);

    let blank = InputLog::new(Command::new(), GamepadInput::new(), GamepadInput::new());
    assert_eq!(movie.input_logs[0], blank);
    assert_eq!(
        movie.input_logs[1],
        InputLog::new(
            Command::new(),
            GamepadInput::new().with_right(true).with_b(true),
            GamepadInput::new(),
        )
    );
    assert_eq!(
        movie.input_logs[2],
        InputLog::new(
            Command::new(),
            GamepadInput::new().with_up(true).with_a(true),
            GamepadInput::new().with_left(true).with_start(true),
        )
    );
    let all = GamepadInput::new()
        .with_up(true)
        .with_down(true)
        .with_left(true)
        .with_right(true)
        .with_start(true)
        .with_select(true)
        .with_b(true)
        .with_a(true);
    assert_eq!(movie.input_logs[3], InputLog::new(Command::new(), all, all));
    assert_eq!(movie.input_logs[4], blank);
}

#[test]
fn reset_power_fixture_maps_console_columns_to_commands() {
    let movie = parse(RESET_POWER_BK2).expect("reset/power fixture must parse");
    assert_eq!(movie.input_logs.len(), 3);
    assert_eq!(
        movie.input_logs[0],
        InputLog::new(Command::new(), GamepadInput::new(), GamepadInput::new())
    );
    assert!(movie.input_logs[1].command.soft_reset());
    assert!(!movie.input_logs[1].command.hard_reset());
    assert!(!movie.input_logs[2].command.soft_reset());
    assert!(movie.input_logs[2].command.hard_reset());
}

#[test]
fn non_mnemonic_on_chars_count_as_pressed_and_unknown_columns_skip() {
    // `X`/`Y` are not BizHawk mnemonics but must count as pressed; `P1 Turbo`
    // is unknown and skipped; group 3 (`P3`) is ignored wholesale.
    let log_key = "LogKey:#Power|Reset|#P1 Up|P1 Turbo|P1 A|#P2 B|#P3 A|";
    let bytes = build_bk2_with_log_key(&base_header(), log_key, &["|X.|Y.Y|X|Y|"], &[]);
    let movie = parse(&bytes).expect("custom log must parse");
    assert_eq!(movie.input_logs.len(), 1);
    let log = &movie.input_logs[0];
    assert!(log.command.hard_reset());
    assert!(!log.command.soft_reset());
    assert!(log.port0.up());
    assert!(log.port0.a());
    assert!(log.port1.b());
}

#[test]
fn axis_columns_consume_six_chars_without_misaligning_buttons() {
    // A console `Reset Cycle` axis slot (5 digits + comma) must not shift the
    // Reset column that follows it.
    let log_key = "LogKey:#Power|Reset Cycle|Reset|#P1 A|";
    let bytes = build_bk2_with_log_key(&base_header(), log_key, &["|.    0,r|Y|"], &[]);
    let movie = parse(&bytes).expect("axis log must parse");
    assert_eq!(movie.input_logs.len(), 1);
    assert!(movie.input_logs[0].command.soft_reset());
    assert!(movie.input_logs[0].port0.a());
}

#[test]
fn blank_movie_is_trusted_silently() {
    let movie = parse(BLANK_BK2).unwrap();
    let warnings = check_movie(&movie, &test_rom(), false).expect("blank must play");
    assert!(warnings.is_empty(), "unexpected warnings: {warnings:?}");
}

#[test]
fn digest_mismatch_and_board_drift_warn_but_play() {
    // Warn paths are pinned with synthetic headers, never with the fixtures'
    // illustrative ROM identity fields.
    let mut header = base_header();
    header.push("SHA1 DEADBEEF");
    header.push("BoardName NROM");
    let movie = parse(&build_bk2(&header, &blank_frames3(), &[])).unwrap();
    let warnings = check_movie(&movie, &test_rom(), false).expect("drift must play");
    assert!(
        warnings.iter().any(|w| w.contains("digest")),
        "expected a digest warning, got {warnings:?}"
    );
    assert!(
        warnings.iter().any(|w| w.contains("BoardName")),
        "expected a BoardName warning, got {warnings:?}"
    );
}

#[test]
fn matching_digest_suppresses_the_board_warning() {
    // A matching digest implies the board via the same detection path, so
    // BoardName stays silent; Core still warns on drift.
    use sha1::Digest as _;
    let mut image = Vec::new();
    image.extend_from_slice(b"NES\x1a");
    image.push(1);
    image.push(1);
    image.extend_from_slice(&[0u8; 10]);
    image.extend_from_slice(&[0u8; 16 * 1024]);
    image.extend_from_slice(&[0u8; 8 * 1024]);
    let rom = INesFile::new(image).unwrap();
    let mut hasher = sha1::Sha1::new();
    hasher.update(rom.read_prg_rom());
    hasher.update(rom.read_chr_rom());
    let digest = format!("{:x}", hasher.finalize());
    let mut header = base_header();
    let header_line = format!("SHA1 {digest}");
    // `header` holds `&str`s; leak the owned line for the test's lifetime.
    let header_line: &'static str = Box::leak(header_line.into_boxed_str());
    header.push(header_line);
    header.push("BoardName NROM");
    let movie = parse(&build_bk2(&header, &blank_frames3(), &[])).unwrap();
    let warnings = check_movie(&movie, &rom, false).expect("exact ROM must play");
    assert!(
        warnings
            .iter()
            .all(|w| !w.contains("digest") && !w.contains("BoardName")),
        "unexpected warnings: {warnings:?}"
    );
}

#[test]
fn core_mismatch_warns_and_plays() {
    let mut header = base_header();
    header.push("Core QuickNES");
    let bytes = build_bk2(&header, &blank_frames3(), &[]);
    let movie = parse(&bytes).unwrap();
    let warnings = check_movie(&movie, &test_rom(), false).expect("core drift must play");
    assert!(
        warnings.iter().any(|w| w.contains("QuickNES")),
        "{warnings:?}"
    );
}

#[test]
fn non_nes_platform_is_refused() {
    let mut header = base_header();
    header[2] = "Platform SNES";
    let bytes = build_bk2(&header, &blank_frames3(), &[]);
    let movie = parse(&bytes).unwrap();
    assert_eq!(
        check_movie(&movie, &test_rom(), false),
        Err(Bk2Refusal::Platform {
            found: "SNES".to_owned()
        })
    );
}

#[test]
fn pal_flag_is_refused() {
    let mut header = base_header();
    header.push("PAL 1");
    let bytes = build_bk2(&header, &blank_frames3(), &[]);
    let movie = parse(&bytes).unwrap();
    assert!(matches!(
        check_movie(&movie, &test_rom(), false),
        Err(Bk2Refusal::Region { .. })
    ));
}

#[test]
fn pal_region_override_is_refused() {
    let bytes = build_bk2(
        &base_header(),
        &blank_frames3(),
        &[("SyncSettings.json", r#"{"RegionOverride":"PAL"}"#)],
    );
    let movie = parse(&bytes).unwrap();
    assert!(matches!(
        check_movie(&movie, &test_rom(), false),
        Err(Bk2Refusal::Region { .. })
    ));
}

#[test]
fn zapper_controls_are_refused() {
    let bytes = build_bk2(
        &base_header(),
        &blank_frames3(),
        &[("SyncSettings.json", r#"{"Controls":{"Port1":"Zapper"}}"#)],
    );
    let movie = parse(&bytes).unwrap();
    assert!(matches!(
        check_movie(&movie, &test_rom(), false),
        Err(Bk2Refusal::Controls { .. })
    ));
}

#[test]
fn anchored_synthetic_hits_the_named_refusal() {
    // Recipe from testdata/bk2/README.md: blank fixture plus a CoreState lump.
    let mut out = Cursor::new(Vec::new());
    {
        let mut src = zip::ZipArchive::new(Cursor::new(BLANK_BK2)).unwrap();
        let mut dst = zip::ZipWriter::new(&mut out);
        let options = zip::write::SimpleFileOptions::default();
        let names: Vec<String> = src.file_names().map(str::to_owned).collect();
        for name in &names {
            let mut data = Vec::new();
            src.by_name(name).unwrap().read_to_end(&mut data).unwrap();
            dst.start_file(name, options).unwrap();
            dst.write_all(&data).unwrap();
        }
        dst.start_file("CoreState.bin", options).unwrap();
        dst.write_all(b"fake savestate").unwrap();
        dst.finish().unwrap();
    }
    let movie = parse(&out.into_inner()).unwrap();
    assert_eq!(movie.anchored, Some(AnchoredKind::CoreState));
    assert_eq!(
        check_movie(&movie, &test_rom(), false),
        Err(Bk2Refusal::Anchored {
            kind: AnchoredKind::CoreState
        })
    );
}

#[test]
fn anchored_override_downgrades_to_warn_and_play() {
    let bytes = build_bk2(&base_header(), &blank_frames3(), &[("Core.bin", "state")]);
    let movie = parse(&bytes).unwrap();
    let warnings = check_movie(&movie, &test_rom(), true).expect("override must play");
    assert!(
        warnings.iter().any(|w| w.contains("savestate-anchored")),
        "{warnings:?}"
    );
}

#[test]
fn saveram_anchor_is_refused() {
    let bytes = build_bk2(
        &base_header(),
        &blank_frames3(),
        &[("MovieSaveRam.bin", "sram")],
    );
    let movie = parse(&bytes).unwrap();
    assert_eq!(movie.anchored, Some(AnchoredKind::SaveRam));
    assert!(matches!(
        check_movie(&movie, &test_rom(), false),
        Err(Bk2Refusal::Anchored { .. })
    ));
}

#[test]
fn missing_platform_is_refused() {
    let header: Vec<&str> = base_header()
        .into_iter()
        .filter(|l| !l.starts_with("Platform"))
        .collect();
    let bytes = build_bk2(&header, &blank_frames3(), &[]);
    let movie = parse(&bytes).unwrap();
    assert!(matches!(
        check_movie(&movie, &test_rom(), false),
        Err(Bk2Refusal::Platform { .. })
    ));
}

#[test]
fn missing_input_log_is_a_parse_error() {
    let mut out = Cursor::new(Vec::new());
    let mut zip = zip::ZipWriter::new(&mut out);
    let options = zip::write::SimpleFileOptions::default();
    zip.start_file("Header.txt", options).unwrap();
    writeln!(zip, "Platform NES").unwrap();
    zip.finish().unwrap();
    assert!(matches!(
        parse(&out.into_inner()),
        Err(Bk2ParseError::MissingEntry("Input Log.txt"))
    ));
}
