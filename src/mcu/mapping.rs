use super::Mcu;

/// Mapping mcu that can be used to map a memory region to sub memory Mcu.
pub struct Region {
    /// The start address of the memory region.
    start: u16,
    /// The mapping region length
    end: u16,
    /// The sub memory Mcu.
    mcu: Box<dyn Mcu>,
}

impl Region {
    /// Create a new mapping.
    pub fn new(start: u16, end: u16, mcu: Box<dyn Mcu>) -> Region {
        Region { start, end, mcu }
    }
}

pub struct MappingMcu {
    /// The list of mappings.
    regions: Vec<Region>,
}

impl MappingMcu {
    /// Create a new mapping mcu.
    pub fn new(regions: Vec<Region>) -> MappingMcu {
        MappingMcu {
            regions,
        }
    }
}

impl Mcu for MappingMcu {
    fn read(&self, address: u16) -> u8 {
        for region in &self.regions {
            if address >= region.start && address < region.end {
                return region.mcu.read(address);
            }
        }
        panic!("read address out of range: {:04x}", address);
    }

    fn write(&mut self, address: u16, value: u8) {
        for region in &mut self.regions {
            if address >= region.start && address < region.end {
                return region.mcu.write(address, value);
            }
        }
        panic!("write address out of range: {:04x}", address);
    }
}