use crate::nes::mapper::Mirroring;

pub struct Nametable {
    mem: [u8; 4096],
}

impl Nametable {
    pub fn new() -> Self {
        Self { mem: [0; 4096] }
    }

    pub fn read(&self, mirroring: Mirroring, addr: u16) -> u8 {
        self.mem[mirroring.name_table_offset(addr) as usize & 4095]
    }

    pub fn write(&mut self, mirroring: Mirroring, addr: u16, v: u8) {
        self.mem[mirroring.name_table_offset(addr) as usize & 4095] = v;
    }
}
