use super::plugin::{CompositePlugin, Console, MonitorTestStatus, ReportPlugin};
use super::plugin::{DetectDeadLoop, ImageExit};
use nes_core::ines::INesFile;
use nes_core::mcu::{Mcu, RamMcu};
use nes_core::nes::{nes_mcu, Machine};
use nes_core::Plugin;
use std::io::Read;

mod driver;

pub enum Image {
    Bin(Box<[u8; 64 * 1024]>),
    INes(Box<INesFile>),
}

impl Image {
    fn create_mcu(&self) -> Box<dyn Mcu> {
        match self {
            Image::Bin(arr) => Box::new(RamMcu::new(**arr)),
            Image::INes(ines) => Box::new(nes_mcu::build(ines)),
        }
    }

    fn create_plugin(&self, quiet: bool) -> Box<dyn Plugin> {
        match self {
            Image::Bin(_) => Box::new(CompositePlugin::new(vec![
                Box::new(ReportPlugin::new(quiet)),
                Box::<ImageExit>::default(),
            ])),
            Image::INes(_) => Box::new(CompositePlugin::new(vec![
                Box::<Console>::default(),
                Box::new(ReportPlugin::new(quiet)),
                Box::<MonitorTestStatus>::default(),
                Box::new(DetectDeadLoop::<1>::new()),
                Box::new(DetectDeadLoop::<2>::new()),
            ])),
        }
    }

    pub fn create_machine(&self, quiet: bool) -> Machine<Box<dyn Plugin>> {
        match self {
            Image::Bin(_) => self.create_bin_machine(quiet),
            Image::INes(_) => self.create_ines_machine(quiet),
        }
    }

    fn create_bin_machine(&self, quiet: bool) -> Machine<Box<dyn Plugin>> {
        let plugin = self.create_plugin(quiet);
        let mut r = Machine::with_plugin(plugin, self.create_mcu());
        r.reset();
        r.set_pc(0x400);
        r
    }

    fn create_ines_machine(&self, quiet: bool) -> Machine<Box<dyn Plugin>> {
        let plugin = self.create_plugin(quiet);
        let mut r = Machine::with_plugin(plugin, self.create_mcu());
        r.reset();
        r
    }
}

#[derive(Debug, thiserror::Error)]
pub enum LoadError {
    #[error("IO error")]
    IOError(#[from] std::io::Error),
    #[error("invalid iNES file format")]
    InvalidINes(#[from] nes_core::ines::FormatError),
}

fn read_file_bytes(f: &str) -> Result<Vec<u8>, LoadError> {
    let mut f = std::fs::File::open(f)?;
    let mut buf = Vec::new();
    f.read_to_end(&mut buf).unwrap();
    Ok(buf)
}

pub fn load_image(f: &str) -> Result<Image, LoadError> {
    if f.ends_with(".bin") {
        load_bin(f)
    } else if is_nes_file(f) {
        load_rom(f)
    } else {
        panic!("unknown file type");
    }
}

fn load_bin(f: &str) -> Result<Image, LoadError> {
    let buf = read_file_bytes(f)?;
    assert_eq!(buf.len(), 64 * 1024);
    let arr: [u8; 64 * 1024] = buf.try_into().expect("image file length is not 64k");
    Ok(Image::Bin(Box::new(arr)))
}

fn is_nes_file(f: &str) -> bool {
    let buf = read_file_bytes(f).unwrap();
    INesFile::is_valid(&buf)
}

fn load_rom(f: &str) -> Result<Image, LoadError> {
    Ok(Image::INes(Box::new(INesFile::new(read_file_bytes(f)?)?)))
}
