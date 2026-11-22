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
