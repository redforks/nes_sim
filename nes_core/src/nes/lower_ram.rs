use crate::mcu::Mcu;

/// Lower nes RAM (0x0000-0x07FF) and remap to 0x0800-0x1FFF
pub struct LowerRam {
    ram: [u8; 0xf00],
}

impl LowerRam {
    pub fn new() -> LowerRam {
        LowerRam { ram: [0; 0xf00] }
    }
}

impl Mcu for LowerRam {
    fn read(&self, address: u16) -> u8 {
        self.ram[address as usize & 0x7ff]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.ram[address as usize & 0x7ff] = value;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut mcu = LowerRam { ram: [0; 0xf00] };
        mcu.write(0x0000, 0x12);
        assert_eq!(mcu.read(0x0000), 0x12);
        mcu.write(0x07ff, 0x13);
        assert_eq!(mcu.read(0x07ff), 0x13);

        assert_eq!(mcu.read(0x0800), 0x12);
        assert_eq!(mcu.read(0x0fff), 0x13);
        assert_eq!(mcu.read(0x1000), 0x12);
        assert_eq!(mcu.read(0x17ff), 0x13);
        assert_eq!(mcu.read(0x1800), 0x12);
        assert_eq!(mcu.read(0x1fff), 0x13);
    }
}
