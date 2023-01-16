use clap::Parser;
use nes_core::{Cpu, ExecuteResult};
use std::io::Write;

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
    let Args { f, quiet } = args;

    env_logger::Builder::new()
        .filter(
            None,
            if quiet {
                log::LevelFilter::Warn
            } else {
                log::LevelFilter::Debug
            },
        )
        .format(|buf, record| writeln!(buf, "{}", record.args()))
        .init();

    let image = image::load_image(&f).unwrap();
    let mut cpu = Cpu::new(image.create_mcu());
    cpu.reset();
    if let Some(pc) = image.start_addr() {
        cpu.pc = pc;
    }
    let mut plugin = image.create_plugin(quiet);
    loop {
        match cpu.clock_tick(&mut plugin) {
            ExecuteResult::Continue => {}
            ExecuteResult::ShouldReset => cpu.reset(),
            ExecuteResult::Stop(result) => std::process::exit(result as i32),
        }
    }
}
