//! Per-frame emulation throughput, one benchmark per game.
//!
//! Measured scope: exactly one [`NesMachine::process_frame`] call
//! (CPU+PPU+APU through vblank entry), with the result folded through
//! `black_box`. SDL presentation, file IO, and save-state restore stay
//! outside the timed section.
//!
//! Fixture: each game advances from power-on with a fixed input script to
//! a sprite-heavy gameplay frame, then the state is captured with
//! [`snapshot::save_state_json`]. Every sample restores that same state in
//! `iter_batched` setup (excluded from measurement) so all samples time
//! the identical busy frame.
//!
//! ROMs: read from `$NES_BENCH_ROM_DIR`, never vendored. Missing dir or
//! missing file skips that game with a message; exit status stays green.

use std::{env, fs, hint::black_box, path::PathBuf};

use criterion::{BatchSize, BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use nes_core::{
    EmptyPlugin, ines::INesFile, nes::NesMcu, nes::controller::Button, nes_machine::NesMachine,
    snapshot,
};

type BenchMachine = NesMachine<EmptyPlugin<NesMcu>>;

const ROM_DIR_ENV: &str = "NES_BENCH_ROM_DIR";
/// Power-on frame where the Start press begins (past boot, on the title).
const START_FRAME: usize = 90;
/// Frame where the Start press releases.
const RELEASE_FRAME: usize = 100;
/// First gameplay frame where held inputs apply.
const PLAY_FRAME: usize = 101;

struct Game {
    id: &'static str,
    file_name: &'static str,
    /// Power-on frames advanced in setup before capturing the fixture.
    setup_frames: usize,
    /// Held/tapped inputs applied during setup (after [`PLAY_FRAME`]).
    input: fn(&mut BenchMachine, frame: usize),
}

fn hold_from(machine: &mut BenchMachine, frame: usize, buttons: &[Button]) {
    if frame == PLAY_FRAME {
        for button in buttons {
            machine.press_controller_a(*button);
        }
    }
}

fn tap_every(machine: &mut BenchMachine, frame: usize, period: usize, button: Button) {
    let since = frame.saturating_sub(PLAY_FRAME);
    if frame >= PLAY_FRAME && since % period == 0 {
        machine.press_controller_a(button);
    } else if frame >= PLAY_FRAME && since % period == 5 {
        machine.release_controller_a(button);
    }
}

/// World 1-1, scrolled with enemies on screen: run right with B held.
fn smb_input(machine: &mut BenchMachine, frame: usize) {
    hold_from(machine, frame, &[Button::Right, Button::B]);
}

/// Stage 1 combat: advance right while firing A on a fixed cadence.
fn contra_input(machine: &mut BenchMachine, frame: usize) {
    hold_from(machine, frame, &[Button::Right]);
    tap_every(machine, frame, 30, Button::A);
}

/// Active dive wave: drift and fire on a fixed cadence.
fn galaxian_input(machine: &mut BenchMachine, frame: usize) {
    if frame == PLAY_FRAME {
        machine.press_controller_a(Button::Right);
    } else if frame >= PLAY_FRAME && (frame - PLAY_FRAME) % 120 == 60 {
        machine.release_controller_a(Button::Right);
        machine.press_controller_a(Button::Left);
    } else if frame >= PLAY_FRAME && (frame - PLAY_FRAME) % 120 == 0 && frame > PLAY_FRAME {
        machine.release_controller_a(Button::Left);
        machine.press_controller_a(Button::Right);
    }
    tap_every(machine, frame, 20, Button::A);
}

const GAMES: &[Game] = &[
    Game {
        id: "smb",
        file_name: "Super Mario Bros. (World).nes",
        setup_frames: 900,
        input: smb_input,
    },
    Game {
        id: "contra",
        file_name: "Contra (USA).nes",
        setup_frames: 1200,
        input: contra_input,
    },
    Game {
        id: "galaxian",
        file_name: "Galaxian (Japan).nes",
        setup_frames: 700,
        input: galaxian_input,
    },
];

fn build_fixture(game: &Game, rom_dir: &PathBuf) -> Option<(INesFile, String)> {
    let path = rom_dir.join(game.file_name);
    if !path.exists() {
        eprintln!(
            "frame bench: skipping {}: missing {}",
            game.id,
            path.display()
        );
        return None;
    }
    let bytes = fs::read(&path).unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
    let file = INesFile::new(bytes).expect("valid iNES image");
    let mut machine = BenchMachine::new(&file, EmptyPlugin::new());
    for frame in 0..game.setup_frames {
        if frame == START_FRAME {
            machine.press_controller_a(Button::Start);
        }
        if frame == RELEASE_FRAME {
            machine.release_controller_a(Button::Start);
        }
        (game.input)(&mut machine, frame);
        let output = machine.process_frame();
        black_box(output.frame.len());
    }
    let json = snapshot::save_state_json(&machine).expect("fixture saves at save point");
    Some((file, json))
}

fn bench_frame(criterion: &mut Criterion) {
    let rom_dir: PathBuf = match env::var(ROM_DIR_ENV) {
        Ok(dir) => PathBuf::from(dir),
        Err(_) => {
            eprintln!("frame bench: skipping: {ROM_DIR_ENV} unset");
            return;
        }
    };
    let mut group = criterion.benchmark_group("frame");
    group.throughput(Throughput::Elements(1));
    for game in GAMES {
        let Some((file, json)) = build_fixture(game, &rom_dir) else {
            continue;
        };
        group.bench_with_input(
            BenchmarkId::from_parameter(game.id),
            &game.id,
            |bench, _| {
                bench.iter_batched(
                    || {
                        let mut machine = BenchMachine::new(&file, EmptyPlugin::new());
                        snapshot::load_state_json(&mut machine, &json).expect("fixture must load");
                        machine
                    },
                    |mut machine| {
                        let output = machine.process_frame();
                        // Fold every rendered byte so the framebuffer read cannot sink.
                        let pixels = output
                            .frame
                            .iter()
                            .fold(0u8, |acc, byte| acc.wrapping_add(*byte));
                        black_box(pixels);
                        black_box(output.audio.len());
                    },
                    BatchSize::SmallInput,
                );
            },
        );
    }
    group.finish();
}

criterion_group!(benches, bench_frame);
criterion_main!(benches);
