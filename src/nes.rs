use crate::mcu::{AddrRemap, Mcu, Mirror};

pub mod apu;
pub mod controller;

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ppu_addr_remaps() {
        let maps: Vec<_> = ppu_remap_start_addresses().collect();
        assert_eq!(0x2008, *maps.first().unwrap());
        assert_eq!(0x3ff8, *maps.last().unwrap());
    }
}
