#[rustfmt::skip]
const LENGTH_TABLE: [u8; 32] = [
    0x0a, 0xfe, 0x14, 0x02, 0x28, 0x04, 0x50, 0x06, 0xa0, 0x08, 0x3c, 0x0a, 0x0e, 0x0c, 0x1a, 0x0e,
    0x0c, 0x10, 0x18, 0x12, 0x30, 0x14, 0x60, 0x16, 0xc0, 0x18, 0x48, 0x1a, 0x10, 0x1c, 0x20, 0x1e,
];

#[derive(Debug, Default)]
pub struct LengthControl {
    counter: u8,
}

impl LengthControl {
    /// Should zero output if counter reaches zero
    pub fn disabled(&self) -> bool {
        self.counter == 0
    }

    /// Clear the length counter.
    pub fn clear(&mut self) {
        self.counter = 0;
    }

    /// Load counter from length timer high 5 bits. Length timer high also reloads the counter.
    pub fn load(&mut self, idx: u8) {
        self.counter = LENGTH_TABLE[idx as usize];
    }

    /// Decrement counter if it's > 0.
    pub fn tick(&mut self) {
        self.counter = self.counter.saturating_sub(1);
    }
}
