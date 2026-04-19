use crate::mcu::Mcu;

/// Lower nes RAM (0x0000-0x07FF) and remap to 0x0800-0x1FFF
pub struct LowerRam {
    ram: [u8; 0xf00],
    address_latch: Option<u16>,
}

impl LowerRam {
    pub fn new() -> LowerRam {
        LowerRam { ram: [0; 0xf00], address_latch: None }
    }
}

impl Mcu for LowerRam {
    fn peek(&self, address: u16) -> u8 {
        self.ram[address as usize & 0x7ff]
    }

    fn prepare_read(&mut self, address: u16) {
        debug_assert!(self.address_latch.is_none(), "address latch should be empty when prepare_read");
        self.address_latch = Some(address);
    }

    fn new_read(&mut self) -> u8 {
        let address = self.address_latch.take().expect("address latch should have value when new_read");
        self.ram[address as usize & 0x7ff]
    }

    fn prepare_write(&mut self, address: u16) {
        debug_assert!(self.address_latch.is_none(), "address latch should be empty when prepare_write");
        self.address_latch = Some(address);
    }

    fn new_write(&mut self, value: u8) {
        let address = self.address_latch.take().expect("address latch should have value when new_write");
        self.ram[address as usize & 0x7ff] = value;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut mcu = LowerRam::new();
        mcu.prepare_write(0x0000);
        mcu.new_write(0x12);
        assert_eq!(mcu.peek(0x0000), 0x12);
        mcu.prepare_write(0x07ff);
        mcu.new_write(0x13);
        assert_eq!(mcu.peek(0x07ff), 0x13);

        assert_eq!(mcu.peek(0x0800), 0x12);
        assert_eq!(mcu.peek(0x0fff), 0x13);
        assert_eq!(mcu.peek(0x1000), 0x12);
        assert_eq!(mcu.peek(0x17ff), 0x13);
        assert_eq!(mcu.peek(0x1800), 0x12);
        assert_eq!(mcu.peek(0x1fff), 0x13);
    }
}
