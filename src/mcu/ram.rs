pub struct RamMcu([u8; 0x10000]);

impl RamMcu {
    pub fn new(buf: [u8; 0x10000]) -> RamMcu {
        RamMcu(buf)
    }
}

impl super::Mcu for RamMcu {
    fn read(&self, addr: u16) -> u8 {
        self.0[addr as usize]
    }
    fn write(&mut self, addr: u16, val: u8) {
        self.0[addr as usize] = val;
    }
}
