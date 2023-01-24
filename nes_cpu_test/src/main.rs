use crate::image::Image;
use ansi_term::Color;
use clap::Parser;
use nes_core::nes::Machine;
use nes_core::{ExecuteResult, Plugin};

mod image;
mod plugin;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short)]
    f: String,

    #[arg(short, long)]
    quiet: bool,
}

fn main() {
    let args = Args::parse();
    let Args { f, quiet, .. } = args;

    env_logger::init();

    let image = image::load_image(&f).unwrap();
    let mut machine = image.create_machine(quiet);
    match image {
        Image::Bin(_) => {
            exec(&mut machine, |m| m.run_ticks(1));
        }
        Image::INes(_) => {
            exec(&mut machine, |m| m.process_frame(16.67).1);
        }
    }
}

fn exec<F, P>(m: &mut Machine<P>, f: F)
where
    F: Fn(&mut Machine<P>) -> ExecuteResult,
    P: Plugin,
{
    loop {
        match f(m) {
            ExecuteResult::Continue => {}
            ExecuteResult::ShouldReset => {
                eprintln!("{}", Color::Red.paint("RESET"));
                m.reset();
            }
            ExecuteResult::Stop(result) => std::process::exit(result as i32),
        }
    }
}
