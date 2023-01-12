use crate::mcu::{AddrRemap, Mcu, Mirror};

pub mod apu;
pub mod controller;
pub mod ppu;

pub fn setup_mem_mirror<M: Mcu>(m: M) -> impl Mcu {
    Mirror::new(addr_remaps().collect(), m)
}

fn addr_remaps() -> impl Iterator<Item = AddrRemap> {
    let r = [
        AddrRemap::new(0x0800, 0x0fff, 0x0000),
        AddrRemap::new(0x1000, 0x17ff, 0x0000),
        AddrRemap::new(0x1800, 0x1fff, 0x0000),
    ]
    .into_iter();
    let ppu_ios =
        ppu_remap_start_addresses().map(move |addr| AddrRemap::new(addr, addr + 7, 0x2000));
    r.chain(ppu_ios)
}

fn ppu_remap_start_addresses() -> impl Iterator<Item = u16> {
    (0x2008u16..=0x3ff8u16).step_by(8)
}

#[macro_use]
mod macros {
    #[macro_export] macro_rules! to_from_u8 {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nes::ppu_remap_start_addresses;

    #[test]
    fn test_ppu_addr_remaps() {
        let maps: Vec<_> = ppu_remap_start_addresses().collect();
        assert_eq!(0x2008, *maps.first().unwrap());
        assert_eq!(0x3ff8, *maps.last().unwrap());
    }
}
