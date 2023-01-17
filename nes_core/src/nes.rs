use crate::ines::INesFile;
use crate::mcu::{AddrRemap, MappingMcu, Mcu, Mirror, RamMcu, Region};
use crate::nes::ppu::PpuDriver;

pub mod apu;
pub mod controller;
mod mapper;
pub mod ppu;

pub fn create_mcu<PD>(file: &INesFile, pd: PD) -> impl Mcu
where
    PD: PpuDriver + 'static,
{
    let low_ram = RamMcu::new([0; 0x2000]);
    let after_ppu = RamMcu::start_from(0x4000, [0; 0x20]);
    let devices = [
        Region::with_defined(low_ram),
        Region::with_defined(ppu::new(pd)),
        Region::with_defined(after_ppu),
    ]
    .into_iter();
    let devices = devices.chain(mapper::create_cartridge(file));

    let mcu = MappingMcu::new(devices.collect());
    setup_mem_mirror(mcu)
}

// TODO: remove pub
pub fn setup_mem_mirror<M: Mcu>(m: M) -> impl Mcu {
    Mirror::new(addr_remaps().collect(), m)
}

fn addr_remaps() -> impl Iterator<Item = AddrRemap> {
    [
        AddrRemap::new(0x0800, 0x0fff, 0x0000),
        AddrRemap::new(0x1000, 0x17ff, 0x0000),
        AddrRemap::new(0x1800, 0x1fff, 0x0000),
    ]
    .into_iter()
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
