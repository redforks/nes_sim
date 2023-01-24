use crate::ines::INesFile;
use crate::mcu::Mcu;

pub mod apu;
pub mod controller;
mod lower_ram;
mod machine;
mod mapper;
pub mod nes_mcu;
pub mod ppu;
pub use machine::Machine;

pub fn create_mcu(file: &INesFile) -> impl Mcu {
    nes_mcu::build(file)
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
