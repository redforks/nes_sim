use crate::actions::{InfoAction, PlayMovieAction, RunAction};
use clap::Parser;
use nes_core::{ines::INesFile, nes::ppu::palette::ColorTheme};
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
    /// Path to a .pal palette file (64 RGB entries, 192 bytes)
    #[arg(long)]
    palette: Option<PathBuf>,
}

type AppResult<T> = Result<T, anyhow::Error>;

impl Args {
    pub fn read_file(&self) -> AppResult<INesFile> {
        let mut file = File::open(&self.nes_file)?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;
        Ok(INesFile::new(buffer)?)
    }

    pub fn load_palette(&self) -> AppResult<Option<ColorTheme>> {
        let Some(path) = &self.palette else {
            return Ok(None);
        };
        let mut buf = [0u8; 192];
        let mut file = File::open(path)?;
        file.read_exact(&mut buf)?;
        Ok(Some(ColorTheme::load_from_pal_buf(&buf)))
    }
}

#[derive(clap::Subcommand)]
enum Action {
    /// Show information about the ines file
    Info(InfoAction),
    /// Run the NES simulator with SDL2 display
    Run(RunAction),
    /// Play back a .fm2 movie file
    PlayMovie(PlayMovieAction),
}

impl Action {
    fn run(&self, nes_file: INesFile, palette: Option<ColorTheme>) -> AppResult<()> {
        match self {
            Action::Info(action) => action.run(&nes_file),
            Action::Run(action) => action.run(&nes_file, palette),
            Action::PlayMovie(action) => action.run(&nes_file, palette),
        }
    }
}

fn main() -> AppResult<()> {
    env_logger::init();
    let args = Args::parse();
    let palette = args.load_palette()?;
    args.action.run(args.read_file()?, palette)
}
