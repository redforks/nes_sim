use crate::actions::{InfoAction, ReadChrAction};
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
    /// Read CHR ROM and output as PNG into stdout, run as:
    ///
    ///   nescli <file.nes> read-chr | feh -Z --force-aliasing -
    ///
    /// To view the image
    ReadChr(ReadChrAction),
}

impl Action {
    fn run(&self, nes_file: INesFile) -> AppResult<()> {
        match self {
            Action::Info(action) => action.run(&nes_file),
            Action::ReadChr(action) => action.run(&nes_file),
        }
    }
}

fn main() -> AppResult<()> {
    let args = Args::parse();
    args.action.run(args.read_file()?)
}
