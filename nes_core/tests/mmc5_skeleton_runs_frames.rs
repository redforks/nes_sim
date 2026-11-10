//! Ticket #76 skeleton acceptance: MMC5 ROMs construct through the cartridge
//! factory and run frames without panic (mode-3 PRG banking with power-on
//! vectors visible). Pixel-correct rendering is later tickets' job; here the
//! bar is boot + frames complete.
//!
//! The test ROMs live in the shared nesdev test-roms checkout next to this
//! repository (see justfile). Skip when they are not available.

use nes_core::{EmptyPlugin, ExecuteResult, ines::INesFile, nes_machine::NesMachine};

const FRAMES: usize = 5;

/// Test-ROM path: `$NES_TEST_ROMS_DIR` when set (worktrees and other checkout
/// layouts), otherwise the manifest-relative shared checkout next to this
/// repository (see justfile).
fn rom_path(manifest_rel: &str, dir_rel: &str) -> std::path::PathBuf {
    if let Ok(dir) = std::env::var("NES_TEST_ROMS_DIR") {
        return std::path::Path::new(&dir).join(dir_rel);
    }
    std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join(manifest_rel)
}

#[test]
fn mmc5_roms_run_frames_without_panic() {
    let roms = [
        (
            "mmc5test",
            "../../nes-test-roms/mmc5test/mmc5test.nes",
            "mmc5test/mmc5test.nes",
        ),
        (
            "mmc5test_v2",
            "../../nes-test-roms/mmc5test_v2/mmc5test.nes",
            "mmc5test_v2/mmc5test.nes",
        ),
        (
            "exram",
            "../../nes-test-roms/exram/mmc5exram.nes",
            "exram/mmc5exram.nes",
        ),
    ];

    for (name, manifest_rel, dir_rel) in roms {
        let rom_path = rom_path(manifest_rel, dir_rel);
        let Ok(rom) = std::fs::read(&rom_path) else {
            eprintln!("skipping: {} not found", rom_path.display());
            continue;
        };

        let ines = INesFile::new(rom).expect("valid iNES file");
        assert_eq!(
            ines.header().mapper_no,
            5,
            "{name}: expected mapper 5, got {}",
            ines.header().mapper_no
        );
        let mut machine = NesMachine::new(&ines, EmptyPlugin::new(), (), ());

        for frame in 0..FRAMES {
            assert_eq!(
                machine.process_frame(),
                ExecuteResult::Continue,
                "{name}: frame {frame} did not complete"
            );
        }
    }
}
