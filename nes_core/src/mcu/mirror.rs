use crate::mcu::Mcu;

#[derive(Copy, Clone)]
pub struct AddrRemap {
    /// Start address that map from
    from: u16,
    /// end address that map from
    end: u16,
    /// Start address that map to
    to: u16,
}

impl AddrRemap {
    pub fn new(from: u16, end: u16, to: u16) -> AddrRemap {
        AddrRemap { from, end, to }
    }
}

pub struct Mirror<M: Mcu> {
    maps: Vec<AddrRemap>,
    inner: M,
}

impl<M: Mcu> Mirror<M> {
    pub fn new(maps: Vec<AddrRemap>, inner: M) -> Mirror<M> {
        // assert maps are sorted
        let mut last_end = 0;
        for map in &maps {
            assert!(map.from > last_end);
            last_end = map.end;
        }

        Mirror { maps, inner }
    }

    fn remap(&self, address: u16) -> u16 {
        for map in &self.maps {
            if address < map.from {
                break;
            }

            if address >= map.from && address <= map.end {
                return map.to + (address - map.from);
            }
        }
        address
    }
}

impl<M: Mcu> Mcu for Mirror<M> {
    fn read(&self, address: u16) -> u8 {
        let address = self.remap(address);
        self.inner.read(address)
    }

    fn write(&mut self, address: u16, value: u8) {
        let address = self.remap(address);
        self.inner.write(address, value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mcu::RamMcu;

    #[test]
    fn test() {
        let mut mcu = Mirror::new(
            vec![AddrRemap::new(0x8000, 0xffff, 0x0000)],
            RamMcu::new([0; 0x10000]),
        );
        mcu.write(0x8000, 0x12);
        assert_eq!(mcu.read(0x0000), 0x12);
        assert_eq!(mcu.read(0x8000), 0x12);
    }
}
