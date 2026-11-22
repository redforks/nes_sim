//! F5 save-state file helpers (ticket #195).
//!
//! Path rules and `std::fs` live here so `nes_core` stays filesystem-free:
//! F5 saves to `<rom-dir>/<rom-stem>.state`, silently overwriting any
//! existing file. Success and failure lines go to stderr via `eprintln!`
//! at the call site, matching the F2/F4 style.

use nes_core::{Plugin, nes::NesMcu, nes_machine::NesMachine, snapshot};
use std::path::{Path, PathBuf};

/// Save target for a ROM: `<rom-dir>/<rom-stem>.state` beside the ROM.
pub fn state_path_for_rom(rom_path: &Path) -> PathBuf {
    rom_path.with_extension("state")
}

/// Success line for stderr. The text is fixed by spec #184.
pub fn save_success_message(path: &Path) -> String {
    format!("saved state to {}", path.display())
}

/// Success line for stderr. The text is fixed by spec #184.
pub fn load_success_message(path: &Path) -> String {
    format!("loaded state from {}", path.display())
}

/// Missing-file line for stderr. The text is fixed by spec #184.
pub fn missing_state_message(path: &Path) -> String {
    format!("no save state at {}", path.display())
}

/// Failure line for stderr. Spec #184 fixes no exact save-write text, so
/// this follows the same stderr-and-continue pattern as the F7 texts.
pub fn save_error_message(path: &Path, reason: &str) -> String {
    format!("cannot save state to {}: {reason}", path.display())
}

/// Serialize `machine` and write it to `path`, overwriting silently.
/// Returns the exact stderr line to print on success or failure.
pub fn save_machine_to_path<P>(machine: &NesMachine<P>, path: &Path) -> Result<String, String>
where
    P: Plugin<NesMcu>,
{
    let text = snapshot::save_state_json(machine)
        .map_err(|error| save_error_message(path, &error.to_string()))?;
    std::fs::write(path, text).map_err(|error| save_error_message(path, &error.to_string()))?;
    Ok(save_success_message(path))
}

/// Load the state file at `path` into `machine` in place (no new machine
/// is built). Takes an explicit path plus machine so `--load-state` (#197)
/// reuses this same code path. All validation runs inside
/// `nes_core::snapshot::load_state_json` before any mutation, so a failed
/// load leaves the machine untouched. Returns the exact stderr line to
/// print on success or failure; the failure texts are fixed by spec #184.
pub fn load_machine_from_path<P>(machine: &mut NesMachine<P>, path: &Path) -> Result<String, String>
where
    P: Plugin<NesMcu>,
{
    let text = match std::fs::read_to_string(path) {
        Ok(text) => text,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            return Err(missing_state_message(path));
        }
        Err(error) => return Err(format!("save state is corrupt: {error}")),
    };
    snapshot::load_state_json(machine, &text).map_err(|error| error.to_string())?;
    Ok(load_success_message(path))
}

#[cfg(test)]
mod tests {
    use super::*;
    use nes_core::EmptyPlugin;
    use nes_core::ines::INesFile;

    /// Minimal valid NROM iNES file: `JMP $8000` loop, 1x16 KiB PRG,
    /// 1x8 KiB CHR (mirrors the `nes_machine` test body).
    fn nrom_file() -> INesFile {
        let mut rom = Vec::new();
        rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a, 0x01, 0x01, 0x00, 0x00]);
        rom.extend_from_slice(&[0; 8]);
        let prg_len = 16 * 1024;
        rom.extend_from_slice(&[0x4C, 0x00, 0x80]);
        rom.extend(std::iter::repeat_n(0, prg_len - 3));
        rom.extend(std::iter::repeat_n(0, 8 * 1024));
        INesFile::new(rom).unwrap()
    }

    fn machine_for(file: &INesFile) -> NesMachine<EmptyPlugin<NesMcu>> {
        NesMachine::new(file, EmptyPlugin::new())
    }

    fn scratch_dir(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("nescli_f5_{}_{}", std::process::id(), name));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    /// NROM file with different PRG bytes than [`nrom_file`]: same mapper,
    /// different ROM identity.
    fn other_nrom_file() -> INesFile {
        let mut rom = Vec::new();
        rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a, 0x01, 0x01, 0x00, 0x00]);
        rom.extend_from_slice(&[0; 8]);
        let prg_len = 16 * 1024;
        rom.extend_from_slice(&[0x4C, 0x01, 0x80]);
        rom.extend(std::iter::repeat_n(0, prg_len - 3));
        rom.extend(std::iter::repeat_n(0, 8 * 1024));
        INesFile::new(rom).unwrap()
    }

    /// Minimal MMC1 file (mapper 1 in the header's low nibble), same sizes
    /// as [`nrom_file`]: different mapper, so an NROM save must refuse it.
    fn mmc1_file() -> INesFile {
        let mut rom = Vec::new();
        rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a, 0x01, 0x01, 0x10, 0x00]);
        rom.extend_from_slice(&[0; 8]);
        let prg_len = 16 * 1024;
        rom.extend_from_slice(&[0x4C, 0x00, 0x80]);
        rom.extend(std::iter::repeat_n(0, prg_len - 3));
        rom.extend(std::iter::repeat_n(0, 8 * 1024));
        INesFile::new(rom).unwrap()
    }

    /// The exact nes_core state text of `machine`: a failed load must leave
    /// this byte-identical.
    fn state_text_of(file_machine: &NesMachine<EmptyPlugin<NesMcu>>) -> String {
        snapshot::save_state_json(file_machine).unwrap()
    }

    #[test]
    fn state_path_replaces_extension_beside_the_rom() {
        assert_eq!(
            state_path_for_rom(Path::new("/games/mario.nes")),
            PathBuf::from("/games/mario.state")
        );
        assert_eq!(
            state_path_for_rom(Path::new("/games/sub/dir/mario.NES")),
            PathBuf::from("/games/sub/dir/mario.state")
        );
    }

    #[test]
    fn state_path_without_rom_dir_stays_relative() {
        assert_eq!(
            state_path_for_rom(Path::new("mario.nes")),
            PathBuf::from("mario.state")
        );
    }

    #[test]
    fn state_path_without_extension_appends_state() {
        assert_eq!(
            state_path_for_rom(Path::new("/games/mario")),
            PathBuf::from("/games/mario.state")
        );
    }

    #[test]
    fn success_message_names_the_path() {
        assert_eq!(
            save_success_message(Path::new("/games/mario.state")),
            "saved state to /games/mario.state"
        );
    }

    #[test]
    fn error_message_names_the_path_and_reason() {
        assert_eq!(
            save_error_message(Path::new("/games/mario.state"), "permission denied"),
            "cannot save state to /games/mario.state: permission denied"
        );
    }

    #[test]
    fn written_file_round_trips_through_the_nes_core_load_api() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        for _ in 0..3 {
            machine.process_frame();
        }
        let dir = scratch_dir("roundtrip");
        let path = dir.join("mario.state");

        let message = save_machine_to_path(&machine, &path).expect("save must succeed");
        assert_eq!(message, format!("saved state to {}", path.display()));
        assert_eq!(
            std::fs::read_to_string(&path).unwrap(),
            snapshot::save_state_json(&machine).unwrap()
        );

        let mut loaded = machine_for(&file);
        let text = std::fs::read_to_string(&path).unwrap();
        snapshot::load_state_json(&mut loaded, &text).expect("written file must load");
        assert_eq!(loaded.frame_no(), machine.frame_no());
        assert_eq!(loaded.clock_cycles(), machine.clock_cycles());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn second_save_silently_overwrites_the_first() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        machine.process_frame();
        let dir = scratch_dir("overwrite");
        let path = dir.join("mario.state");

        save_machine_to_path(&machine, &path).unwrap();
        machine.process_frame();
        machine.process_frame();
        save_machine_to_path(&machine, &path).unwrap();

        let text = std::fs::read_to_string(&path).unwrap();
        let mut loaded = machine_for(&file);
        snapshot::load_state_json(&mut loaded, &text).unwrap();
        assert_eq!(loaded.frame_no(), machine.frame_no());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn load_success_message_names_the_path() {
        assert_eq!(
            load_success_message(Path::new("/games/mario.state")),
            "loaded state from /games/mario.state"
        );
    }

    #[test]
    fn missing_message_names_the_path() {
        assert_eq!(
            missing_state_message(Path::new("/games/mario.state")),
            "no save state at /games/mario.state"
        );
    }

    #[test]
    fn load_continues_from_the_saved_frame_bit_exact() {
        // Reference: run 5 frames, save, run 3 more. Loaded: fresh
        // machine loads the save, runs 3. Same frames, same state.
        let file = nrom_file();
        let mut reference = machine_for(&file);
        for _ in 0..5 {
            reference.process_frame();
        }
        let dir = scratch_dir("load-continues");
        let path = dir.join("mario.state");
        let message = save_machine_to_path(&reference, &path).expect("save must succeed");
        assert_eq!(message, format!("saved state to {}", path.display()));

        let mut loaded = machine_for(&file);
        let message = load_machine_from_path(&mut loaded, &path).expect("load must succeed");
        assert_eq!(message, format!("loaded state from {}", path.display()));
        assert_eq!(loaded.frame_no(), reference.frame_no());
        assert_eq!(loaded.clock_cycles(), reference.clock_cycles());

        let mut reference_frames = Vec::new();
        let mut loaded_frames = Vec::new();
        for _ in 0..3 {
            let out = reference.process_frame();
            reference_frames.push((out.frame.to_vec(), out.audio.to_vec()));
            let out = loaded.process_frame();
            loaded_frames.push((out.frame.to_vec(), out.audio.to_vec()));
        }
        assert_eq!(loaded_frames, reference_frames);
        assert_eq!(state_text_of(&loaded), state_text_of(&reference));
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn missing_file_leaves_machine_untouched_with_exact_text() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        for _ in 0..3 {
            machine.process_frame();
        }
        let before = state_text_of(&machine);
        let path = PathBuf::from("/nonexistent_dir_nescli_f7/mario.state");

        let error = load_machine_from_path(&mut machine, &path).expect_err("load must fail");
        assert_eq!(error, format!("no save state at {}", path.display()));
        assert_eq!(state_text_of(&machine), before);
        assert_eq!(machine.frame_no(), 3);
    }

    #[test]
    fn corrupt_file_leaves_machine_untouched_with_exact_text() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        for _ in 0..3 {
            machine.process_frame();
        }
        let before = state_text_of(&machine);
        let dir = scratch_dir("load-corrupt");
        let path = dir.join("mario.state");
        std::fs::write(&path, "not json{{{").unwrap();

        let error = load_machine_from_path(&mut machine, &path).expect_err("load must fail");
        assert!(
            error.starts_with("save state is corrupt: "),
            "unexpected error line: {error}"
        );
        assert_eq!(state_text_of(&machine), before);
        assert_eq!(machine.frame_no(), 3);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn wrong_version_leaves_machine_untouched_with_exact_text() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        for _ in 0..3 {
            machine.process_frame();
        }
        let before = state_text_of(&machine);
        let dir = scratch_dir("load-version");
        let path = dir.join("mario.state");
        save_machine_to_path(&machine, &path).unwrap();
        let text = std::fs::read_to_string(&path).unwrap();
        let edited = text.replacen(
            &format!("\"format_version\": {}", snapshot::SAVE_FORMAT_VERSION),
            "\"format_version\": 7",
            1,
        );
        assert_ne!(edited, text, "fixture must contain the version key");
        std::fs::write(&path, edited).unwrap();

        let error = load_machine_from_path(&mut machine, &path).expect_err("load must fail");
        assert_eq!(
            error,
            format!(
                "save state format version 7 is not supported; this build reads version {}",
                snapshot::SAVE_FORMAT_VERSION
            )
        );
        assert_eq!(state_text_of(&machine), before);
        assert_eq!(machine.frame_no(), 3);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn wrong_rom_leaves_machine_untouched_with_exact_text() {
        let file = nrom_file();
        let other = other_nrom_file();
        let mut saver = machine_for(&file);
        for _ in 0..3 {
            saver.process_frame();
        }
        let dir = scratch_dir("load-rom");
        let path = dir.join("mario.state");
        save_machine_to_path(&saver, &path).unwrap();

        let mut machine = machine_for(&other);
        for _ in 0..3 {
            machine.process_frame();
        }
        let before = state_text_of(&machine);
        let expected = hex_of(&snapshot::rom_sha256(&file));
        let actual = hex_of(&snapshot::rom_sha256(&other));

        let error = load_machine_from_path(&mut machine, &path).expect_err("load must fail");
        assert_eq!(
            error,
            format!(
                "save state is for a different ROM: expected sha256 {expected}, ROM is {actual}"
            )
        );
        assert_eq!(state_text_of(&machine), before);
        assert_eq!(machine.frame_no(), 3);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn wrong_mapper_leaves_machine_untouched_with_exact_text() {
        let file = nrom_file();
        let mmc1 = mmc1_file();
        let mut saver = machine_for(&file);
        for _ in 0..3 {
            saver.process_frame();
        }
        let dir = scratch_dir("load-mapper");
        let path = dir.join("mario.state");
        save_machine_to_path(&saver, &path).unwrap();

        let mut machine = machine_for(&mmc1);
        for _ in 0..3 {
            machine.process_frame();
        }
        let before = state_text_of(&machine);

        let error = load_machine_from_path(&mut machine, &path).expect_err("load must fail");
        assert_eq!(
            error,
            "save state mapper NRom does not match ROM mapper MMC1"
        );
        assert_eq!(state_text_of(&machine), before);
        assert_eq!(machine.frame_no(), 3);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn unknown_enum_leaves_machine_untouched_with_exact_text() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        for _ in 0..3 {
            machine.process_frame();
        }
        let before = state_text_of(&machine);
        let dir = scratch_dir("load-enum");
        let path = dir.join("mario.state");
        save_machine_to_path(&machine, &path).unwrap();
        let text = std::fs::read_to_string(&path).unwrap();
        let edited = text.replacen("\"mapper\": \"NRom\"", "\"mapper\": \"Bogus\"", 1);
        assert_ne!(edited, text, "fixture must contain the mapper tag");
        std::fs::write(&path, edited).unwrap();

        let error = load_machine_from_path(&mut machine, &path).expect_err("load must fail");
        assert_eq!(error, "save state has unknown Mapper variant \"Bogus\"");
        assert_eq!(state_text_of(&machine), before);
        assert_eq!(machine.frame_no(), 3);
        let _ = std::fs::remove_dir_all(&dir);
    }

    fn hex_of(bytes: &[u8; 32]) -> String {
        bytes.iter().map(|b| format!("{b:02x}")).collect()
    }

    #[test]
    fn unwritable_path_returns_the_error_line() {
        let file = nrom_file();
        let machine = machine_for(&file);
        let path = PathBuf::from("/nonexistent_dir_nescli_f5/mario.state");

        let error = save_machine_to_path(&machine, &path).expect_err("write must fail");
        assert!(
            error.starts_with(&format!("cannot save state to {}: ", path.display())),
            "unexpected error line: {error}"
        );
    }
}
