use crate::nes::mapper::Mirroring;

pub struct Nametable {
    mem: [u8; 4096],
    mirroring: Mirroring,
}

impl Nametable {
    pub fn new(mirroring: Mirroring) -> Self {
        Self {
            mem: [0; 4096],
            mirroring,
        }
    }

    pub fn set_mirroring(&mut self, mirroring: Mirroring) {
        self.mirroring = mirroring;
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.mem[self.mirroring.name_table_offset(addr) as usize]
    }

    pub fn write(&mut self, addr: u16, v: u8) {
        self.mem[self.mirroring.name_table_offset(addr) as usize] = v;
    }
}
