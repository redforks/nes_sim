use crate::mcu::Mcu;
use crate::nes::ppu::{Pattern, Tile, TILES_PER_ROW};

pub enum Mirror {
    Horizontal,
    Vertical,
    FourScreen,
    OneScreen,
}

pub struct AttributeTable<'a>(&'a [u8]);

#[derive(Copy, Clone)]
pub struct NameTable<'a>(&'a [u8]);

impl<'a> NameTable<'a> {
    fn tile_idx(&self, x: u8, y: u8) -> u8 {
        (y * TILES_PER_ROW) + x
    }

    fn tile<'b>(&self, patten: Pattern<'b>, x: u8, y: u8) -> Tile<'b> {
        todo!()
    }

    fn palette(&self, x: u8, y: u8) -> u8 {
        todo!()
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
}
