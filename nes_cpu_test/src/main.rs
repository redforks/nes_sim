use std::path::PathBuf;

use crate::image::MachineWrapper;
use ansi_term::Color;
use clap::Parser;
use nes_core::ExecuteResult;

mod image;
mod plugin;

#[cfg(feature = "tcp-server")]
mod tcp_server;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short)]
    f: PathBuf,

    #[arg(short, long)]
    quiet: bool,
    /// Exit if more than this many instructions executed (0 = disabled)
    #[arg(long = "max-instructions", default_value_t = 0)]
    max_instructions: u64,
    /// Set the CPU start PC after reset (hex with 0x prefix or decimal)
    #[arg(long = "start-pc")]
    start_pc: Option<String>,
    /// Start TCP server on port 28800 for MCP communication
    #[arg(long = "tcp-server")]
    tcp_server: bool,
}

fn main() {
    let Args {
        f,
        quiet,
        max_instructions,
        start_pc,
        tcp_server,
    } = Args::parse();

    env_logger::builder().format_timestamp(None).init();

    // TCP server mode
    #[cfg(feature = "tcp-server")]
    if tcp_server {
        tcp_server::run_tcp_server(f, quiet, start_pc, max_instructions);
    }

    #[cfg(not(feature = "tcp-server"))]
    if tcp_server {
        eprintln!("Error: --tcp-server requires the tcp-server feature");
        std::process::exit(1);
    }

    let image = image::load_image(f).unwrap();

    let start_pc = match start_pc {
        Some(s) => {
            let parsed = if s.starts_with("0x") || s.starts_with("0X") {
                u16::from_str_radix(&s[2..], 16)
            } else {
                s.parse::<u16>()
            };
            Some(parsed.expect("invalid --start-pc value"))
        }
        None => None,
    };

    let mut machine = image.create_machine(quiet, start_pc, max_instructions);
    exec(&mut machine, |m| m.tick());
}

fn exec<F>(m: &mut MachineWrapper, f: F)
where
    F: Fn(&mut MachineWrapper) -> ExecuteResult,
{
    loop {
        match f(m) {
            ExecuteResult::Continue => {}
            ExecuteResult::ShouldReset => {
                eprintln!("{}", Color::Red.paint("RESET"));
                m.reset();
            }
            ExecuteResult::Stop(result) => std::process::exit(result as i32),
            ExecuteResult::Halt => std::process::exit(128),
        }
    }
}
