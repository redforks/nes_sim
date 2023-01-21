use crate::mcu::Mcu;
use crate::nes::ppu::{Pattern, Tile, TILES_PER_COL, TILES_PER_ROW};

#[derive(Copy, Clone)]
pub struct AttributeTable<'a>(&'a [u8]);

impl<'a> AttributeTable<'a> {
    fn new(data: &'a [u8]) -> Self {
        debug_assert_eq!(data.len(), 0x40);
        AttributeTable(data)
    }

    fn palette_idx(&self, tile_x: u8, tile_y: u8) -> u8 {
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

    fn tile<'b>(&self, patten: Pattern<'b>, tile_x: u8, tile_y: u8) -> Tile<'b> {
        patten.tile(self.tile_idx(tile_x, tile_y))
    }
}

trait NameTableAddr {
    fn addr(&self, address: u16) -> u16;
}

impl<T> Mcu for T
where
    T: NameTableAddr + AsRef<[u8]> + AsMut<[u8]>,
{
    fn read(&self, address: u16) -> u8 {
        self.as_ref()[self.addr(address) as usize]
    }

    fn write(&mut self, address: u16, value: u8) {
        let addr = self.addr(address) as usize;
        self.as_mut()[addr] = value;
    }
}

/// NameTable and attribute tables
pub trait NameTables {
    fn nth(&self, idx: u8) -> NameTable<'_>;
    fn attribute_table(&self, idx: u8) -> AttributeTable<'_>;
}

impl<T> NameTables for T
where
    T: NameTableAddr + AsRef<[u8]>,
{
    fn nth(&self, idx: u8) -> NameTable<'_> {
        debug_assert!(idx < 4);
        let start = self.addr(idx as u16 * 0x400) as usize;
        let end = start + 0x3c0;
        NameTable(&self.as_ref()[start..end])
    }

    fn attribute_table(&self, idx: u8) -> AttributeTable<'_> {
        debug_assert!(idx < 4);
        let start = self.addr(idx as u16 * 0x400) as usize + 0x3c0;
        let end = start + 0x40;
        AttributeTable(&self.as_ref()[start..end])
    }
}

macro_rules! define_name_table {
    ($name: ident, $size: expr) => {
        pub struct $name([u8; $size]);

        impl $name {
            pub fn new() -> Self {
                Self([0; $size])
            }
        }

        impl AsRef<[u8]> for $name {
            fn as_ref(&self) -> &[u8] {
                &self.0
            }
        }

        impl AsMut<[u8]> for $name {
            fn as_mut(&mut self) -> &mut [u8] {
                &mut self.0
            }
        }
    };
}

define_name_table!(OneScreenNameTables, 0x400);
define_name_table!(FourScreenNameTables, 0x1000);
define_name_table!(HorizontalNameTables, 0x800);
define_name_table!(VerticalNameTables, 0x800);

impl NameTableAddr for OneScreenNameTables {
    fn addr(&self, address: u16) -> u16 {
        address & 0x3ff
    }
}

impl NameTableAddr for FourScreenNameTables {
    fn addr(&self, address: u16) -> u16 {
        address - 0x2000
    }
}

impl NameTableAddr for HorizontalNameTables {
    fn addr(&self, address: u16) -> u16 {
        let r = address - 0x2000;
        if r < 0x800 {
            r % 0x400
        } else {
            r % 0x400 + 0x400
        }
    }
}

impl NameTableAddr for VerticalNameTables {
    fn addr(&self, address: u16) -> u16 {
        address & 0x7ff
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_screen_name_tables() {
        let mut name_tables = OneScreenNameTables::new();
        assert_eq!(name_tables.read(0x2000), 0);
        name_tables.write(0x2000, 1);
        name_tables.write(0x23ff, 11);
        assert_eq!(name_tables.read(0x2000), 1);
        assert_eq!(name_tables.read(0x23ff), 11);
        // all mapped to 0x2000
        assert_eq!(name_tables.read(0x2400), 1);
        assert_eq!(name_tables.read(0x27ff), 11);
        assert_eq!(name_tables.read(0x2800), 1);
        assert_eq!(name_tables.read(0x2bff), 11);
        assert_eq!(name_tables.read(0x2c00), 1);
        assert_eq!(name_tables.read(0x2fff), 11);

        assert_eq!(name_tables.nth(0).0[0], 1);
        assert_eq!(name_tables.nth(1).0[0], 1);
        assert_eq!(name_tables.nth(2).0[0], 1);
        assert_eq!(name_tables.nth(3).0[0], 1);

        name_tables.write(0x23c0, 2);
        assert_eq!(name_tables.attribute_table(0).0[0], 2);
        assert_eq!(name_tables.attribute_table(1).0[0], 2);
        assert_eq!(name_tables.attribute_table(2).0[0], 2);
        assert_eq!(name_tables.attribute_table(3).0[0], 2);
        assert_eq!(name_tables.attribute_table(3).0[63], 11);
        assert_eq!(name_tables.attribute_table(3).0.len(), 64);
    }

    #[test]
    fn four_screen_name_tables() {
        let mut name_tabels = FourScreenNameTables::new();
        name_tabels.write(0x2000, 1);
        name_tabels.write(0x2400, 2);
        name_tabels.write(0x2800, 3);
        name_tabels.write(0x2C00, 4);
        assert_eq!(name_tabels.read(0x2000), 1);
        assert_eq!(name_tabels.read(0x2400), 2);
        assert_eq!(name_tabels.read(0x2800), 3);
        assert_eq!(name_tabels.read(0x2c00), 4);
    }

    #[test]
    fn horizontal_name_tables() {
        let mut name_tabels = HorizontalNameTables::new();
        name_tabels.write(0x2000, 1);
        name_tabels.write(0x2800, 2);
        assert_eq!(name_tabels.read(0x2000), 1);
        assert_eq!(name_tabels.read(0x2400), 1);
        assert_eq!(name_tabels.read(0x2800), 2);
        assert_eq!(name_tabels.read(0x2c00), 2);
    }

    #[test]
    fn vertical_name_tables() {
        let mut name_tabels = VerticalNameTables::new();
        name_tabels.write(0x2000, 1);
        name_tabels.write(0x2400, 2);
        assert_eq!(name_tabels.read(0x2000), 1);
        assert_eq!(name_tabels.read(0x2400), 2);
        assert_eq!(name_tabels.read(0x2800), 1);
        assert_eq!(name_tabels.read(0x2c00), 2);
    }

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
}
