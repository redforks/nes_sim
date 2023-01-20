use crate::ines::INesFile;
use crate::mcu::{MappingMcu, Mcu, RamMcu, Region};

pub mod apu;
pub mod controller;
mod lower_ram;
mod mapper;
pub mod ppu;

pub fn create_mcu(file: &INesFile) -> impl Mcu {
    let after_ppu = RamMcu::start_from(0x4000, [0; 0x20]);
    let devices = [
        Region::with_defined(lower_ram::LowerRam::new()),
        Region::with_defined(ppu::new()),
        Region::with_defined(after_ppu),
    ]
    .into_iter();
    let devices = devices.chain(mapper::create_cartridge(file));

    MappingMcu::new(devices.collect())
}

#[macro_use]
mod macros {
    #[macro_export]
    macro_rules! to_from_u8 {
        ($t: ty) => {
            impl From<$t> for u8 {
                fn from(n: $t) -> Self {
                    n.into_bytes()[0]
                }
            }

            impl From<u8> for $t {
                fn from(v: u8) -> Self {
                    <$t>::from_bytes([v])
                }
            }
        };
    }
}
