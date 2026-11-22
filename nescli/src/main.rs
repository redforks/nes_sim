use crate::actions::{ConvertAction, InfoAction, RunAction};
use clap::Parser;

mod actions;
mod recording_send;
mod save_state;
mod sdl;

/// NES emulator cli tools
#[derive(clap::Parser)]
struct Args {
    #[command(subcommand)]
    action: Action,
}

type AppResult<T> = Result<T, anyhow::Error>;

#[derive(clap::Subcommand)]
enum Action {
    /// Show information about the ines file
    Info(InfoAction),
    /// Run the NES simulator with SDL2 display
    Run(RunAction),
    /// Convert an FCEUX .fm2 movie to a Mesen .mmo movie
    Convert(ConvertAction),
}

impl Action {
    fn run(&self) -> AppResult<()> {
        match self {
            Action::Info(action) => action.run(),
            Action::Run(action) => action.run(),
            Action::Convert(action) => action.run(),
        }
    }
}

#[hotpath::main]
fn main() -> AppResult<()> {
    env_logger::init();
    Args::parse().action.run()
}
