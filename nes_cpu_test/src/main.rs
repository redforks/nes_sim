use clap::Parser;
use nes_core::ines::INesFile;
use nes_core::mcu::{Mcu, RamMcu};
use nes_core::{Cpu, Flag, Plugin};
use std::io::{Read, Write};

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

    let mut cpu = Cpu::new(load_file(&f));
    cpu.reset();
    // the test rom reset position, not the start position.
    cpu.pc = 0x400;
    let mut sync_cycle = ReportPlugin::new(quiet);
    while !sync_cycle.should_exit() {
        cpu.clock_tick(&mut sync_cycle);
    }
}

fn load_file(f: &str) -> Box<dyn Mcu> {
    if f.ends_with(".bin") {
        load_bin(f)
    } else if f.ends_with(".nes") {
        load_rom(f)
    } else {
        panic!("unknown file type");
    }
}

fn load_bin(f: &str) -> Box<dyn Mcu> {
    let mut buf = read_file_bytes(f);
    assert_eq!(buf.len(), 64 * 1024);
    let mut arr: [u8; 64 * 1024] = buf.try_into().expect("image file length is not 64k");
    Box::new(RamMcu::new(arr))
}

fn read_file_bytes(f: &str) -> Vec<u8> {
    let mut f = std::fs::File::open(f).unwrap();
    let mut buf = Vec::new();
    f.read_to_end(&mut buf).unwrap();
    buf
}

fn load_rom(f: &str) -> Box<dyn Mcu> {
    INesFile::new(read_file_bytes(f))
        .map(|f| {
            let prg = f.read_prg();
            let mut arr: [u8; 64 * 1024] = [0; 64 * 1024];
            arr[0x8000..0x8000 + prg.len()].copy_from_slice(prg);
            Box::new(RamMcu::new(arr)) as Box<dyn Mcu>
        })
        .unwrap()
}

struct ReportPlugin {
    verbose: bool,
    count: u32,
    last_pc: Option<u16>,
    should_exit: bool,
}

impl ReportPlugin {
    fn new(quiet: bool) -> ReportPlugin {
        ReportPlugin {
            verbose: !quiet,
            count: 0,
            last_pc: None,
            should_exit: false,
        }
    }

    fn should_exit(&self) -> bool {
        self.should_exit
    }
}

impl Plugin for ReportPlugin {
    fn start(&mut self, cpu: &Cpu) {
        self.count += 1;

        if self.verbose {
            println!("[{}] pc: ${:04x}", self.count, cpu.pc);
        }
    }

    fn end(&mut self, cpu: &Cpu) {
        if self.verbose {
            let flags = format_flags(cpu);
            let top = cpu.peek_stack();
            println!(
                "a: ${:02x}, x: ${:02x}, y: ${:02x}, sp: ${:02x}, p: ${:02x} {} top: ${:02x}\n",
                cpu.a, cpu.x, cpu.y, cpu.sp, cpu.status, flags, top
            );
        }

        if let Some(last) = self.last_pc {
            if last == cpu.pc {
                self.should_exit = true;
                if cpu.flag(Flag::Decimal) {
                    // decimal mode not implemented, it is okay to exit test on decimal error,
                    // decimal test is the last of opCode test.
                    println!("test succeed!");
                    return;
                }

                println!("test failed: pc repeated");
                return;
            }
        }
        self.last_pc = Some(cpu.pc);
    }
}

fn format_flags(cpu: &Cpu) -> String {
    let mut r = String::new();

    r.push(if cpu.flag(Flag::Negative) { 'N' } else { 'n' });
    r.push(if cpu.flag(Flag::Overflow) { 'V' } else { 'v' });
    r.push(if cpu.flag(Flag::Break) { 'B' } else { 'b' });
    r.push(if cpu.flag(Flag::Decimal) { 'D' } else { 'd' });
    r.push(if cpu.flag(Flag::Interrupt) { 'I' } else { 'i' });
    r.push(if cpu.flag(Flag::Zero) { 'Z' } else { 'z' });
    r.push(if cpu.flag(Flag::Carry) { 'C' } else { 'c' });

    r
}
