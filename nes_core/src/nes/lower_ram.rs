use crate::SystemClock;
use crate::mcu::Mcu;

/// Lower nes RAM (0x0000-0x07FF) and remap to 0x0800-0x1FFF
pub struct LowerRam {
    ram: Box<[u8; 0xf00]>,
}

/// Power-up contents, matching FCEUX: a byte is 0xFF when address bit 2 is
/// set, else 0x00. Real power-on RAM is random; this pattern keeps runs
/// frame-exact against the FCEUX reference.
fn power_up_pattern(ram: &mut [u8]) {
    for (i, byte) in ram.iter_mut().enumerate() {
        *byte = if i & 4 != 0 { 0xFF } else { 0x00 };
    }
}

impl LowerRam {
    pub fn new() -> LowerRam {
        let mut ram = Box::new([0; 0xf00]);
        power_up_pattern(&mut ram[..]);
        LowerRam { ram }
    }

    /// Hard reset (power cycle): RAM returns to its power-up contents.
    /// Soft reset leaves RAM untouched, so this is hard-only.
    pub fn reset_hard(&mut self) {
        power_up_pattern(&mut self.ram[..]);
    }
}

impl Mcu for LowerRam {
    fn read(&mut self, address: u16) -> u8 {
        self.ram[address as usize & 0x7ff]
    }

    fn peek(&self, address: u16) -> u8 {
        self.ram[address as usize & 0x7ff]
    }

    fn write(&mut self, address: u16, value: u8, _clock: SystemClock) {
        self.ram[address as usize & 0x7ff] = value;
    }

    fn read_zero_page(&mut self, address: u8) -> u8 {
        self.ram[address as usize]
    }

    fn read_stack_page(&mut self, address: u8) -> u8 {
        self.ram[0x100 + address as usize]
    }

    fn write_zero_page(&mut self, address: u8, value: u8, _clock: SystemClock) {
        self.ram[address as usize] = value;
    }

    fn write_stack_page(&mut self, address: u8, value: u8, _clock: SystemClock) {
        self.ram[0x100 + address as usize] = value;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn power_up_contents_match_fceux_pattern() {
        let mcu = LowerRam::new();
        assert_eq!(mcu.peek(0x0000), 0x00);
        assert_eq!(mcu.peek(0x0003), 0x00);
        assert_eq!(mcu.peek(0x0004), 0xFF);
        assert_eq!(mcu.peek(0x0007), 0xFF);
        assert_eq!(mcu.peek(0x0008), 0x00);
        assert_eq!(mcu.peek(0x07fc), 0xFF);
    }

    #[test]
    fn hard_reset_restores_power_up_pattern() {
        let mut mcu = LowerRam::new();
        mcu.write(0x0004, 0x12, SystemClock::default());
        mcu.write(0x0000, 0x34, SystemClock::default());
        mcu.reset_hard();
        assert_eq!(mcu.peek(0x0000), 0x00);
        assert_eq!(mcu.peek(0x0004), 0xFF);
    }

    #[test]
    fn test() {
        let mut mcu = LowerRam::new();
        mcu.write(0x0000, 0x12, SystemClock::default());
        assert_eq!(mcu.read(0x0000), 0x12);
        mcu.write(0x07ff, 0x13, SystemClock::default());
        assert_eq!(mcu.read(0x07ff), 0x13);

        assert_eq!(mcu.read(0x0800), 0x12);
        assert_eq!(mcu.read(0x0fff), 0x13);
        assert_eq!(mcu.read(0x1000), 0x12);
        assert_eq!(mcu.read(0x17ff), 0x13);
        assert_eq!(mcu.read(0x1800), 0x12);
        assert_eq!(mcu.read(0x1fff), 0x13);
    }
}
