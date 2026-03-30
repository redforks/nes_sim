use crate::plugin::{ExitTestPlugin, NesReportPlugin, ReportNesTestResult};

use super::plugin::{CompositePlugin, Console, MonitorTestStatus, ReportPlugin};
use super::plugin::{DetectDeadLoop, ImageExit, MaxInstructions};
use nes_core::Plugin;
use nes_core::ines::INesFile;
use nes_core::machine::Machine;
use nes_core::mcu::RamMcu;
use nes_core::nes_machine::NesMachine;
use std::io::Read;
use std::path::{Path, PathBuf};

mod driver;

pub enum Image {
    Bin(Box<[u8; 64 * 1024]>),
    INes {
        nes_file: Box<INesFile>,
        file_name: PathBuf,
    },
}

impl Image {
    pub fn create_machine(
        &self,
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
    ) -> MachineWrapper {
        match self {
            Image::Bin(arr) => self.create_bin_machine(arr, quiet, start_pc, max_instructions),
            Image::INes {
                nes_file,
                file_name,
            } => self.create_ines_machine(nes_file, file_name, quiet, start_pc, max_instructions),
        }
    }

    fn create_bin_machine(
        &self,
        arr: &[u8; 64 * 1024],
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
    ) -> MachineWrapper {
        let mcu = RamMcu::new(*arr);
        let mut plugins: Vec<Box<dyn Plugin<_>>> = vec![
            Box::new(ReportPlugin::create(quiet)),
            Box::new(ExitTestPlugin::new()),
            Box::<ImageExit>::default(),
        ];
        if max_instructions > 0 {
            plugins.push(Box::new(MaxInstructions::new(max_instructions)));
        }
        let plugin = CompositePlugin::new(plugins);
        let mut machine = Machine::with_plugin(plugin, mcu);
        match start_pc {
            Some(pc) => machine.set_pc(pc),
            None => machine.set_pc(0x400),
        }
        MachineWrapper::Bin(Box::new(machine))
    }

    fn create_ines_machine(
        &self,
        ines: &INesFile,
        file_name: &Path,
        quiet: bool,
        start_pc: Option<u16>,
        max_instructions: u64,
    ) -> MachineWrapper {
        // Build the composite plugin step by step to handle type coercion
        let mut plugins: Vec<Box<dyn Plugin<nes_core::nes::NesMcu<(), ()>>>> = vec![
            Box::<Console>::default(),
            Box::new(NesReportPlugin::create(quiet)),
            Box::<MonitorTestStatus>::default(),
            Box::new(DetectDeadLoop::<1>::new()),
            Box::new(DetectDeadLoop::<2>::new()),
        ];
        if file_name.file_name().is_some_and(|f| f == "nestest.nes") {
            plugins.push(Box::new(ReportNesTestResult::new()));
        } else {
            plugins.push(Box::new(ExitTestPlugin::new()));
        }
        if max_instructions > 0 {
            // MaxInstructions is generic over Mcu; need to coerce type. MaxInstructions doesn't use Mcu methods so this is fine.
            plugins.push(Box::new(MaxInstructions::new(max_instructions)));
        }
        let plugin = CompositePlugin::new(plugins);
        let mut machine = NesMachine::new(ines, plugin, (), ());
        if let Some(pc) = start_pc {
            machine.set_pc(pc);
        }
        MachineWrapper::INes(Box::new(machine))
    }
}

// Type aliases to avoid >> parsing issues in enums
mod machine_types {
    use super::*;
    use nes_core::nes_machine::NesMachine;

    pub type BinMcu = RamMcu<{ 64 * 1024 }>;
    pub type BinPlugin = CompositePlugin<BinMcu>;
    pub type BinMachine = Machine<BinPlugin, BinMcu>;

    pub type INesPlugin = CompositePlugin<nes_core::nes::NesMcu<(), ()>>;
    pub type INesMachine = NesMachine<INesPlugin, (), ()>;
}

pub enum MachineWrapper {
    Bin(Box<machine_types::BinMachine>),
    INes(Box<machine_types::INesMachine>),
}

impl MachineWrapper {
    pub fn tick(&mut self) -> nes_core::ExecuteResult {
        match self {
            MachineWrapper::Bin(m) => m.tick(),
            MachineWrapper::INes(m) => m.tick(),
        }
    }

    pub fn reset(&mut self) {
        match self {
            MachineWrapper::Bin(m) => m.reset(),
            MachineWrapper::INes(m) => m.reset(),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum LoadError {
    #[error("IO error")]
    IOError(#[from] std::io::Error),
    #[error("invalid iNES file format")]
    InvalidINes(#[from] nes_core::ines::FormatError),
}

fn read_file_bytes(f: &Path) -> Result<Vec<u8>, LoadError> {
    let mut f = std::fs::File::open(f)?;
    let mut buf = Vec::new();
    f.read_to_end(&mut buf).unwrap();
    Ok(buf)
}

pub fn load_image(f: PathBuf) -> Result<Image, LoadError> {
    if f.extension().is_some_and(|ext| ext == "bin") {
        load_bin(&f)
    } else if is_nes_file(&f) {
        load_rom(f)
    } else {
        panic!("unknown file type");
    }
}

fn load_bin(f: &Path) -> Result<Image, LoadError> {
    let buf = read_file_bytes(f)?;
    assert_eq!(buf.len(), 64 * 1024);
    let arr: [u8; 64 * 1024] = buf.try_into().expect("image file length is not 64k");
    Ok(Image::Bin(Box::new(arr)))
}

fn is_nes_file(f: &Path) -> bool {
    let buf = read_file_bytes(f).unwrap();
    INesFile::is_valid(&buf)
}

fn load_rom(f: PathBuf) -> Result<Image, LoadError> {
    Ok(Image::INes {
        nes_file: Box::new(INesFile::new(read_file_bytes(&f)?)?),
        file_name: f,
    })
}
