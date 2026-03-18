use super::*;
use std::cell::RefCell;

struct MockMcu {
    data: RefCell<[u8; 256]>,
    start: u16,
    end: u16,
}

impl MockMcu {
    fn new(start: u16, end: u16) -> Self {
        MockMcu {
            data: RefCell::new([0; 256]),
            start,
            end,
        }
    }
}

impl Mcu for MockMcu {
    fn read(&mut self, address: u16) -> u8 {
        self.data.borrow()[(address - self.start) as usize]
    }
    fn write(&mut self, address: u16, value: u8) {
        self.data.borrow_mut()[(address - self.start) as usize] = value;
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
    let mut mapping = MappingMcu::new(regions);
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
    let mut mapping = MappingMcu::new(regions);

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
    let mut mapping = MappingMcu::new(regions);
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
    let mut mapping = MappingMcu::new(regions);
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
