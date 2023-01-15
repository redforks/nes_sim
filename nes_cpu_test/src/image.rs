use super::plugin::{CompositePlugin, Console, MonitorTestStatus, ReportPlugin};
use nes_core::ines::INesFile;
use nes_core::mcu::{Mcu, RamMcu};
use nes_core::Plugin;
use std::io::Read;

pub enum Image {
    Bin([u8; 64 * 1024]),
    INes(INesFile),
}

impl Image {
    pub fn create_mcu(&self) -> Box<dyn Mcu> {
        match self {
            Image::Bin(arr) => Box::new(RamMcu::new(*arr)),
            Image::INes(ines) => {
                let prg = ines.read_prg();
                let mut arr: [u8; 64 * 1024] = [0; 64 * 1024];
                arr[0x8000..0x8000 + prg.len()].copy_from_slice(prg);
                Box::new(RamMcu::new(arr)) as Box<dyn Mcu>
            }
        }
    }

    pub fn create_plugin(&self, quiet: bool) -> Box<dyn Plugin> {
        match self {
            Image::Bin(_) => Box::new(ReportPlugin::new(quiet)),
            Image::INes(_) => Box::new(CompositePlugin::new(vec![
                Box::new(Console()),
                Box::new(MonitorTestStatus::default()),
            ])),
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

fn read_file_bytes(f: &str) -> Result<Vec<u8>, LoadError> {
    let mut f = std::fs::File::open(f)?;
    let mut buf = Vec::new();
    f.read_to_end(&mut buf).unwrap();
    Ok(buf)
}

pub fn load_image(f: &str) -> Result<Image, LoadError> {
    if f.ends_with(".bin") {
        load_bin(f)
    } else if f.ends_with(".nes") {
        load_rom(f)
    } else {
        panic!("unknown file type");
    }
}

fn load_bin(f: &str) -> Result<Image, LoadError> {
    let buf = read_file_bytes(f)?;
    assert_eq!(buf.len(), 64 * 1024);
    let arr: [u8; 64 * 1024] = buf.try_into().expect("image file length is not 64k");
    Ok(Image::Bin(arr))
}

fn load_rom(f: &str) -> Result<Image, LoadError> {
    Ok(Image::INes(INesFile::new(read_file_bytes(f)?)?))
}
