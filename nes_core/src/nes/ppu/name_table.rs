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

/// NameTable and attribute tables
pub trait NameTables: Mcu {
    fn nth(&self, idx: u8) -> NameTable<'_>;
    fn attribute_table(&self, idx: u8) -> AttributeTable<'_>;
}

pub struct OneScreenNameTables([u8; 0x400]);

impl OneScreenNameTables {
    pub fn new() -> Self {
        Self([0; 0x400])
    }

    fn addr(address: u16) -> u16 {
        address & 0x3ff
    }
}

impl NameTables for OneScreenNameTables {
    fn nth(&self, _: u8) -> NameTable<'_> {
        NameTable(&self.0[0..0x3c0])
    }

    fn attribute_table(&self, _: u8) -> AttributeTable<'_> {
        AttributeTable(&self.0[0x3c0..])
    }
}

impl Mcu for OneScreenNameTables {
    fn read(&self, address: u16) -> u8 {
        self.0[Self::addr(address) as usize]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.0[Self::addr(address) as usize] = value;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_screen_name_table() {
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
}
