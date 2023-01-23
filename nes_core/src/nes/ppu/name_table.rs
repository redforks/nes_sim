use crate::mcu::Mcu;
use crate::nes::ppu::{Pattern, Tile, NAME_TABLE_MEM_START, TILES_PER_COL, TILES_PER_ROW};

#[derive(Copy, Clone)]
pub struct AttributeTable<'a>(&'a [u8]);

impl<'a> AttributeTable<'a> {
    fn new(data: &'a [u8]) -> Self {
        debug_assert_eq!(data.len(), 0x40);
        AttributeTable(data)
    }

    pub fn palette_idx(&self, tile_x: u8, tile_y: u8) -> u8 {
        debug_assert!(tile_x < TILES_PER_ROW);
        debug_assert!(tile_y < TILES_PER_COL);
        let x = tile_x / 4;
        let y = tile_y / 4;
        let idx = (y * 8 + x) as usize;
        let byte = self.0[idx];
        match (tile_x % 2, tile_y % 2) {
            (0, 0) => byte & 0b11,
            (1, 0) => (byte >> 2) & 0b11,
            (0, 1) => (byte >> 4) & 0b11,
            (1, 1) => (byte >> 6) & 0b11,
            _ => unreachable!(),
        }
    }
}

#[derive(Copy, Clone)]
pub struct NameTable<'a>(&'a [u8]);

impl<'a> NameTable<'a> {
    fn new(data: &'a [u8]) -> Self {
        debug_assert_eq!(data.len(), TILES_PER_ROW as usize * TILES_PER_COL as usize);
        NameTable(data)
    }

    fn tile_idx(&self, tile_x: u8, tile_y: u8) -> u8 {
        debug_assert!(tile_x < TILES_PER_ROW);
        debug_assert!(tile_y < TILES_PER_COL);
        self.0[(tile_y as usize * TILES_PER_ROW as usize) + tile_x as usize]
    }

    pub fn tile<'b>(&self, patten: Pattern<'b>, tile_x: u8, tile_y: u8) -> Tile<'b> {
        patten.tile(self.tile_idx(tile_x, tile_y))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Mirroring {
    LowerBank, // single screen use lower bank
    UpperBank, // single screen use upper bank
    Horizontal,
    Vertical,
    Four,
}

pub struct NameTableControl {
    mem: [u8; 4096],
    band_start_offset: [u16; 4],
}

impl NameTableControl {
    pub fn new() -> Self {
        Self {
            mem: [0; 4096],
            band_start_offset: [0, 1024, 2048, 3072],
        }
    }

    pub fn set_mirroring(&mut self, mirroring: Mirroring) {
        match mirroring {
            Mirroring::LowerBank => {
                self.band_start_offset = [0, 0, 0, 0];
            }
            Mirroring::UpperBank => {
                self.band_start_offset = [1024, 1024, 1024, 1024];
            }
            Mirroring::Vertical => {
                self.band_start_offset = [0, 1024, 0, 1024];
            }
            Mirroring::Horizontal => {
                self.band_start_offset = [0, 0, 1024, 1024];
            }
            Mirroring::Four => {
                self.band_start_offset = [0, 1024, 2048, 3072];
            }
        }
    }

    pub fn nth(&self, idx: u8) -> NameTable<'_> {
        let start = self.band_start_offset[idx as usize] as usize;
        NameTable::new(&self.mem[start..start + 960])
    }

    pub fn attribute_table(&self, idx: u8) -> AttributeTable<'_> {
        let start = self.band_start_offset[idx as usize] as usize + 960;
        AttributeTable::new(&self.mem[start..start + 64])
    }

    fn offset(&self, addr: u16) -> usize {
        let r = addr - NAME_TABLE_MEM_START;
        (self.band_start_offset[r as usize / 1024] + r % 1024) as usize
    }
}

impl Mcu for NameTableControl {
    fn read(&self, address: u16) -> u8 {
        self.mem[self.offset(address)]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.mem[self.offset(address)] = value;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn name_table() {
        let mut arr = [0; 960];
        arr[0] = 1;
        arr[31] = 2;
        arr[32] = 3;
        arr[959] = 4;
        let name_tables = NameTable::new(&arr);
        assert_eq!(1, name_tables.tile_idx(0, 0));
        assert_eq!(2, name_tables.tile_idx(31, 0));
        assert_eq!(3, name_tables.tile_idx(0, 1));
        assert_eq!(4, name_tables.tile_idx(31, 29));
    }

    #[test]
    fn attribute_table() {
        let mut arr = [0; 64];
        arr[0] = 0b00_01_10_11;
        arr[7] = 0b01_10_11_00;
        arr[8] = 0b10_11_00_01;
        arr[63] = 0b11_00_01_10;
        let tbl = AttributeTable(&arr);

        assert_eq!(0b11, tbl.palette_idx(0, 0));
        assert_eq!(0b10, tbl.palette_idx(1, 0));
        assert_eq!(0b01, tbl.palette_idx(0, 1));
        assert_eq!(0b00, tbl.palette_idx(1, 1));

        // palette are same for 2-2 tiles
        assert_eq!(0b11, tbl.palette_idx(2, 0));
        assert_eq!(0b11, tbl.palette_idx(0, 2));
        assert_eq!(0b11, tbl.palette_idx(2, 2));

        assert_eq!(0b00, tbl.palette_idx(30, 0));
        assert_eq!(0b11, tbl.palette_idx(31, 0));
        assert_eq!(0b10, tbl.palette_idx(30, 1));
        assert_eq!(0b01, tbl.palette_idx(31, 1));

        assert_eq!(0b01, tbl.palette_idx(0, 4));
        assert_eq!(0b11, tbl.palette_idx(31, 29));
    }

    #[test]
    fn name_table_control() {
        fn assert_start_addr(control: &NameTableControl, v1: u8, v2: u8, v3: u8, v4: u8) {
            assert_eq!(v1, control.read(0x2000));
            assert_eq!(v2, control.read(0x2400));
            assert_eq!(v3, control.read(0x2800));
            assert_eq!(v4, control.read(0x2c00));
        }

        let mut control = NameTableControl::new();

        // default is four screen
        control.write(0x2000, 1);
        control.write(0x2400, 2);
        control.write(0x2800, 3);
        control.write(0x2c00, 4);
        control.write(0x23ff, 11);
        control.write(0x27ff, 12);
        control.write(0x2bff, 13);
        control.write(0x2fff, 14);
        assert_start_addr(&control, 1, 2, 3, 4);
        assert_eq!(11, control.mem[1023]);
        assert_eq!(1, control.nth(0).0[0]);
        assert_eq!(2, control.nth(1).0[0]);
        assert_eq!(3, control.nth(2).0[0]);
        assert_eq!(4, control.nth(3).0[0]);
        assert_eq!(11, control.attribute_table(0).0[63]);
        assert_eq!(12, control.attribute_table(1).0[63]);
        assert_eq!(13, control.attribute_table(2).0[63]);
        assert_eq!(14, control.attribute_table(3).0[63]);

        control.set_mirroring(Mirroring::LowerBank);
        assert_start_addr(&control, 1, 1, 1, 1);

        control.set_mirroring(Mirroring::UpperBank);
        assert_start_addr(&control, 2, 2, 2, 2);

        control.set_mirroring(Mirroring::Horizontal);
        assert_start_addr(&control, 1, 1, 2, 2);

        control.set_mirroring(Mirroring::Vertical);
        assert_start_addr(&control, 1, 2, 1, 2);

        control.set_mirroring(Mirroring::Four);
        assert_start_addr(&control, 1, 2, 3, 4);
    }
}
