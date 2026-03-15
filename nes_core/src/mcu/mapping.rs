use super::Mcu;

/// Mapping mcu that can be used to map a memory region to sub memory Mcu.
pub struct Region {
    /// The start address of the memory region.
    start: u16,
    /// The mapping region last address.
    end: u16,
    /// The sub memory Mcu.
    mcu: Box<dyn Mcu>,
}

impl Region {
    /// Create a new mapping.
    pub fn new(start: u16, end: u16, mcu: Box<dyn Mcu>) -> Self {
        Region { start, end, mcu }
    }

    pub fn with_defined<M: Mcu + DefinedRegion + 'static>(mcu: M) -> Self {
        let (start, end) = mcu.region();
        Region::new(start, end, Box::new(mcu))
    }
}

pub struct MappingMcu {
    /// The list of mappings.
    regions: Vec<Region>,
}

impl MappingMcu {
    /// Create a new mapping mcu.
    pub fn new(regions: Vec<Region>) -> MappingMcu {
        MappingMcu { regions }
    }
}

impl Mcu for MappingMcu {
    fn read(&self, address: u16) -> u8 {
        for region in &self.regions {
            if address >= region.start && address <= region.end {
                return region.mcu.read(address);
            }
        }
        panic!("read address out of range: {:04x}", address);
    }

    fn write(&mut self, address: u16, value: u8) {
        for region in &mut self.regions {
            if address >= region.start && address <= region.end {
                return region.mcu.write(address, value);
            }
        }
        panic!("write address out of range: {:04x}", address);
    }
}

pub trait DefinedRegion {
    fn region(&self) -> (u16, u16); // (start, end)
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MockMcu {
        data: [u8; 256],
        start: u16,
        end: u16,
    }

    impl MockMcu {
        fn new(start: u16, end: u16) -> Self {
            MockMcu {
                data: [0; 256],
                start,
                end,
            }
        }
    }

    impl Mcu for MockMcu {
        fn read(&self, address: u16) -> u8 {
            self.data[(address - self.start) as usize]
        }

        fn write(&mut self, address: u16, value: u8) {
            self.data[(address - self.start) as usize] = value;
        }
    }

    impl DefinedRegion for MockMcu {
        fn region(&self) -> (u16, u16) {
            (self.start, self.end)
        }
    }

    #[test]
    fn region_new() {
        let mcu = Box::new(MockMcu::new(0x1000, 0x10FF));
        let region = Region::new(0x1000, 0x10FF, mcu);
        assert_eq!(region.start, 0x1000);
        assert_eq!(region.end, 0x10FF);
    }

    #[test]
    fn region_with_defined() {
        let mcu = MockMcu::new(0x2000, 0x20FF);
        let region = Region::with_defined(mcu);
        assert_eq!(region.start, 0x2000);
        assert_eq!(region.end, 0x20FF);
    }

    #[test]
    fn mapping_mcu_new() {
        let regions = vec![
            Region::new(0x0000, 0x00FF, Box::new(MockMcu::new(0x0000, 0x00FF))),
            Region::new(0x1000, 0x10FF, Box::new(MockMcu::new(0x1000, 0x10FF))),
        ];
        let mapping = MappingMcu::new(regions);
        assert_eq!(mapping.regions.len(), 2);
    }

    #[test]
    fn mapping_mcu_read() {
        let mut mcu1 = MockMcu::new(0x0000, 0x00FF);
        mcu1.write(0x0050, 0xAB);
        let mut mcu2 = MockMcu::new(0x1000, 0x10FF);
        mcu2.write(0x1050, 0xCD);

        let regions = vec![
            Region::new(0x0000, 0x00FF, Box::new(mcu1)),
            Region::new(0x1000, 0x10FF, Box::new(mcu2)),
        ];
        let mapping = MappingMcu::new(regions);

        assert_eq!(mapping.read(0x0050), 0xAB);
        assert_eq!(mapping.read(0x1050), 0xCD);
    }

    #[test]
    fn mapping_mcu_write() {
        let mcu1 = MockMcu::new(0x0000, 0x00FF);
        let mcu2 = MockMcu::new(0x1000, 0x10FF);

        let regions = vec![
            Region::new(0x0000, 0x00FF, Box::new(mcu1)),
            Region::new(0x1000, 0x10FF, Box::new(mcu2)),
        ];
        let mapping = &mut MappingMcu::new(regions);
        mapping.write(0x0050, 0xEF);
        mapping.write(0x1050, 0xDA);

        // Verify reads return the written values
        assert_eq!(mapping.read(0x0050), 0xEF);
        assert_eq!(mapping.read(0x1050), 0xDA);
    }

    #[test]
    #[should_panic(expected = "read address out of range")]
    fn mapping_mcu_read_out_of_range() {
        let regions = vec![Region::new(
            0x0000,
            0x00FF,
            Box::new(MockMcu::new(0x0000, 0x00FF)),
        )];
        let mapping = MappingMcu::new(regions);
        mapping.read(0x5000); // Out of range
    }

    #[test]
    #[should_panic(expected = "write address out of range")]
    fn mapping_mcu_write_out_of_range() {
        let regions = vec![Region::new(
            0x0000,
            0x00FF,
            Box::new(MockMcu::new(0x0000, 0x00FF)),
        )];
        let mut mapping = MappingMcu::new(regions);
        mapping.write(0x5000, 0xFF); // Out of range
    }
}
