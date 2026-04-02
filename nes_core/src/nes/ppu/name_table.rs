use crate::nes::ppu::NAME_TABLE_MEM_START;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
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
    mirroring: Mirroring,
}

impl Default for NameTableControl {
    fn default() -> Self {
        Self {
            mem: [0; 4096],
            band_start_offset: [0, 1024, 2048, 3072],
            mirroring: Mirroring::Four,
        }
    }
}

impl NameTableControl {
    pub fn mirroring(&self) -> Mirroring {
        self.mirroring
    }

    pub fn set_mirroring(&mut self, mirroring: Mirroring) {
        if mirroring == self.mirroring {
            return;
        }

        self.mirroring = mirroring;
        self.band_start_offset = match mirroring {
            Mirroring::LowerBank => [0, 0, 0, 0],
            Mirroring::UpperBank => [1024, 1024, 1024, 1024],
            Mirroring::Vertical => [0, 1024, 0, 1024],
            Mirroring::Horizontal => [0, 0, 1024, 1024],
            Mirroring::Four => [0, 1024, 2048, 3072],
        };
    }

    fn offset(&self, addr: u16) -> usize {
        let r = addr - NAME_TABLE_MEM_START;
        (self.band_start_offset[r as usize / 1024] + r % 1024) as usize
    }

    pub(super) fn read(&mut self, address: u16) -> u8 {
        self.mem[self.offset(address)]
    }

    pub(super) fn write(&mut self, address: u16, value: u8) {
        self.mem[self.offset(address)] = value;
    }
}

#[cfg(test)]
mod tests;
