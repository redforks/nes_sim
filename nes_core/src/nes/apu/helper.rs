const LENGTH_TABLE: [u8; 32] = [
    10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96, 22,
    192, 24, 72, 26, 16, 28, 32, 30,
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
