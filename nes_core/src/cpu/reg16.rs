/// A struct to represent 16 bit register, can be access as u16, and two
/// high/low u8 byte.
#[derive(Debug, Clone, Copy, Default)]
pub struct Register16 {
    bytes: [u8; 2],
}

impl Register16 {
    pub fn get(self) -> u16 {
        u16::from_ne_bytes(self.bytes)
    }

    pub fn set(&mut self, v: u16) {
        self.bytes = v.to_ne_bytes();
    }

    /// Return low byte
    pub fn low(&self) -> u8 {
        cfg_select! {
            target_endian = "little" => {
                self.bytes[0]
            }
            target_endian = "big" => {
                self.bytes[1]
            }
        }
    }

    /// set low byte
    pub fn set_low(&mut self, v: u8) {
        cfg_select! {
            target_endian = "little" => {
                self.bytes[0] = v;
            }
            target_endian = "big" => {
                self.bytes[1] = v;
            }
        }
    }

    /// Return high byte
    pub fn high(&self) -> u8 {
        cfg_select! {
            target_endian = "little" => {
                self.bytes[1]
            }
            target_endian = "big" => {
                self.bytes[0]
            }
        }
    }

    /// set high byte
    pub fn set_high(&mut self, v: u8) {
        cfg_select! {
            target_endian = "little" => {
                self.bytes[1] = v;
            }
            target_endian = "big" => {
                self.bytes[0] = v;
            }
        }
    }

    /// Wrapping addition — delegates to u16::wrapping_add
    pub fn wrapping_add(&mut self, v: u16) {
        self.set(self.get().wrapping_add(v));
    }
}

impl core::ops::AddAssign<u16> for Register16 {
    fn add_assign(&mut self, rhs: u16) {
        self.set(self.get().wrapping_add(rhs));
    }
}

impl core::ops::BitOrAssign<u16> for Register16 {
    fn bitor_assign(&mut self, rhs: u16) {
        self.set(self.get() | rhs);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_bytes(r: &Register16, low: u8, high: u8) {
        cfg_select! {
            target_endian = "little" => {
                assert_eq!(r.low(), low, "low byte mismatch (LE)");
                assert_eq!(r.high(), high, "high byte mismatch (LE)");
            }
            target_endian = "big" => {
                assert_eq!(r.low(), low, "low byte mismatch (BE)");
                assert_eq!(r.high(), high, "high byte mismatch (BE)");
            }
        }
    }

    #[test]
    fn test_default() {
        let r = Register16::default();
        assert_eq!(r.get(), 0);
        assert_bytes(&r, 0, 0);
    }

    #[test]
    fn test_set_zero() {
        let mut r = Register16::default();
        r.set(0);
        assert_eq!(r.get(), 0);
        assert_bytes(&r, 0, 0);
    }

    #[test]
    fn test_set_get_u16() {
        let mut r = Register16::default();
        r.set(0xABCD);
        assert_eq!(r.get(), 0xABCD);
    }

    #[test]
    fn test_set_and_get_max() {
        let mut r = Register16::default();
        r.set(u16::MAX);
        assert_eq!(r.get(), u16::MAX);
    }

    #[test]
    fn test_low_byte() {
        let mut r = Register16::default();
        r.set(0x1234);
        assert_eq!(r.low(), 0x34);
    }

    #[test]
    fn test_high_byte() {
        let mut r = Register16::default();
        r.set(0x1234);
        assert_eq!(r.high(), 0x12);
    }

    #[test]
    fn test_set_low() {
        let mut r = Register16::default();
        r.set(0x1200);
        r.set_low(0xAB);
        assert_eq!(r.get(), 0x12AB);
        assert_eq!(r.low(), 0xAB);
        assert_eq!(r.high(), 0x12);
    }

    #[test]
    fn test_set_high() {
        let mut r = Register16::default();
        r.set(0x00CD);
        r.set_high(0xAB);
        assert_eq!(r.get(), 0xABCD);
        assert_eq!(r.low(), 0xCD);
        assert_eq!(r.high(), 0xAB);
    }

    #[test]
    fn test_set_low_preserves_high() {
        let mut r = Register16::default();
        r.set(0xDEAD);
        r.set_low(0x00);
        assert_eq!(r.get(), 0xDE00);
    }

    #[test]
    fn test_set_high_preserves_low() {
        let mut r = Register16::default();
        r.set(0xDEAD);
        r.set_high(0x00);
        assert_eq!(r.get(), 0x00AD);
    }

    #[test]
    fn test_get_set_roundtrip() {
        let mut r = Register16::default();
        let values = [
            0x0000, 0x0001, 0x0100, 0xFFFF, 0x8080, 0x1234, 0xDEAD, 0xBEEF, 0xCAFE, 0x0FF0,
        ];
        for &v in &values {
            r.set(v);
            assert_eq!(r.get(), v, "roundtrip failed for value 0x{v:04X}");
        }
    }

    #[test]
    fn test_low_byte_after_get_with_various() {
        let mut r = Register16::default();
        r.set(0xABCD);
        assert_eq!(r.low(), r.get() as u8);
    }

    #[test]
    fn test_high_byte_after_get() {
        let mut r = Register16::default();
        r.set(0xABCD);
        assert_eq!(r.high(), (r.get() >> 8) as u8);
    }

    #[test]
    fn test_default_trait() {
        let r: Register16 = Default::default();
        assert_eq!(r.get(), 0);
    }

    #[test]
    fn test_clone() {
        let a = Register16::default();
        let mut b = a;
        b.set(0x1234);
        assert_eq!(a.get(), 0);
        assert_eq!(b.get(), 0x1234);
    }

    #[test]
    fn test_copy() {
        let a = Register16::default();
        let b = a;
        assert_eq!(a.get(), b.get());
    }

    #[test]
    fn test_debug() {
        let r = Register16::default();
        let debug = format!("{r:?}");
        assert!(!debug.is_empty());
    }

    #[test]
    fn test_set_low_zero_then_high_only() {
        let mut r = Register16::default();
        r.set_low(0);
        r.set_high(0xFF);
        assert_eq!(r.get(), 0xFF00);
    }

    #[test]
    fn test_set_high_zero_then_low_only() {
        let mut r = Register16::default();
        r.set_high(0);
        r.set_low(0xFF);
        assert_eq!(r.get(), 0x00FF);
    }

    #[test]
    fn test_high_low_symmetry() {
        let mut r = Register16::default();
        r.set_high(0x12);
        r.set_low(0x34);
        assert_eq!(r.low(), 0x34);
        assert_eq!(r.high(), 0x12);
    }

    #[test]
    fn test_add_assign() {
        let mut r = Register16::default();
        r.set(0x0001);
        r += 1u16;
        assert_eq!(r.get(), 0x0002);
    }

    #[test]
    fn test_add_assign_wrapping() {
        let mut r = Register16::default();
        r.set(u16::MAX);
        r += 1u16;
        assert_eq!(r.get(), 0);
    }

    #[test]
    fn test_bitor_assign() {
        let mut r = Register16::default();
        r.set(0x0F00);
        r |= 0x00F0u16;
        assert_eq!(r.get(), 0x0FF0);
    }

    #[test]
    fn test_wrapping_add() {
        let mut r = Register16::default();
        r.set(0xFFFE);
        r.wrapping_add(3);
        assert_eq!(r.get(), 1);
    }

    #[test]
    fn test_wrapping_add_zero() {
        let mut r = Register16::default();
        r.set(0x1234);
        r.wrapping_add(0);
        assert_eq!(r.get(), 0x1234);
    }
}
