use crate::actions::{InfoAction, PlayMovieAction, ReadChrAction, RunAction};
use clap::Parser;
use nes_core::ines::INesFile;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

mod actions;

/// NES emulator cli tools
#[derive(clap::Parser)]
struct Args {
    #[command(subcommand)]
    action: Action,
    nes_file: PathBuf,
}

type AppResult<T> = Result<T, anyhow::Error>;

impl Args {
    pub fn read_file(&self) -> AppResult<INesFile> {
        let mut file = File::open(&self.nes_file)?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;
        Ok(INesFile::new(buffer)?)
    }
}

#[derive(clap::Subcommand)]
enum Action {
    /// Show information about the ines file
    Info(InfoAction),
    /// Read CHR ROM and output as PNG into stdout
    ReadChr(ReadChrAction),
    /// Run the NES simulator with SDL2 display
    Run(RunAction),
    /// Play back a .fm2 movie file
    PlayMovie(PlayMovieAction),
}

impl Action {
    fn run(&self, file_path: PathBuf, nes_file: INesFile) -> AppResult<()> {
        match self {
            Action::Info(action) => action.run(&nes_file),
            Action::ReadChr(action) => action.run(&nes_file),
            Action::Run(action) => action.run(&nes_file),
            Action::PlayMovie(action) => action.run(file_path, &nes_file),
        }
    }
}

fn main() -> AppResult<()> {
    env_logger::init();
    let args = Args::parse();
    let ines_file = args.read_file()?;
    let file_path = args.nes_file;
    args.action.run(file_path, ines_file)
}
