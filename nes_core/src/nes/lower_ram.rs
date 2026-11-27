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

    /// Capture the reachable system RAM window (`0x0000-0x07FF`) for
    /// save-states (ticket #184). Every access masks with 0x7FF, so the
    /// reachable window is exactly the first 0x800 bytes of the box;
    /// bytes 0x800..0xF00 are never read and never saved.
    pub(crate) fn snapshot_ram(&self) -> [u8; 0x800] {
        let mut out = [0u8; 0x800];
        out.copy_from_slice(&self.ram[..0x800]);
        out
    }

    /// Restore a window previously produced by [`Self::snapshot_ram`].
    pub(crate) fn restore_ram(&mut self, bytes: &[u8; 0x800]) {
        self.ram[..0x800].copy_from_slice(bytes);
    }
}

impl Mcu for LowerRam {
    #[inline]
    fn read(&mut self, address: u16) -> u8 {
        // SAFETY: `address & 0x7ff` is below the 0xf00 box length.
        unsafe { *self.ram.get_unchecked((address as usize) & 0x7ff) }
    }

    #[inline]
    fn peek(&self, address: u16) -> u8 {
        // SAFETY: `address & 0x7ff` is below the 0xf00 box length.
        unsafe { *self.ram.get_unchecked((address as usize) & 0x7ff) }
    }

    #[inline]
    fn write(&mut self, address: u16, value: u8, _clock: SystemClock) {
        // SAFETY: `address & 0x7ff` is below the 0xf00 box length.
        unsafe {
            *self.ram.get_unchecked_mut((address as usize) & 0x7ff) = value;
        }
    }

    #[inline]
    fn read_zero_page(&mut self, address: u8) -> u8 {
        // SAFETY: `u8` is below the 0xf00 box length.
        unsafe { *self.ram.get_unchecked(address as usize) }
    }

    #[inline]
    fn read_stack_page(&mut self, address: u8) -> u8 {
        // SAFETY: `0x100 + u8` is below the 0xf00 box length.
        unsafe { *self.ram.get_unchecked(0x100 + address as usize) }
    }

    #[inline]
    fn write_zero_page(&mut self, address: u8, value: u8, _clock: SystemClock) {
        // SAFETY: `u8` is below the 0xf00 box length.
        unsafe {
            *self.ram.get_unchecked_mut(address as usize) = value;
        }
    }

    #[inline]
    fn write_stack_page(&mut self, address: u8, value: u8, _clock: SystemClock) {
        // SAFETY: `0x100 + u8` is below the 0xf00 box length.
        unsafe {
            *self.ram.get_unchecked_mut(0x100 + address as usize) = value;
        }
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
