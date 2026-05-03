use crate::nes::apu::{
    LengthTimerHigh3Bits, NoiseControlBits, NoiseLength, PulseControlBits, TriangleControlBits,
};

/// Abstract register that can be used to read length halt bit for noise and pulse channels.
pub trait GetLengthHalt {
    fn length_halt(&self) -> bool;
}

impl GetLengthHalt for PulseControlBits {
    fn length_halt(&self) -> bool {
        self.loop_and_is_halt()
    }
}

impl GetLengthHalt for TriangleControlBits {
    fn length_halt(&self) -> bool {
        self.loop_and_is_halt()
    }
}

impl GetLengthHalt for NoiseControlBits {
    fn length_halt(&self) -> bool {
        self.loop_and_is_halt()
    }
}

/// Abstract register that can be used to read length index for noise and pulse channels.
pub trait GetLengthIndex {
    fn length_idx(&self) -> u8;
}

impl GetLengthIndex for LengthTimerHigh3Bits {
    fn length_idx(&self) -> u8 {
        self.length_idx()
    }
}

impl GetLengthIndex for NoiseLength {
    fn length_idx(&self) -> u8 {
        self.length_idx()
    }
}

#[rustfmt::skip]
const LENGTH_TABLE: [u8; 32] = [
    0x0a, 0xfe, 0x14, 0x02, 0x28, 0x04, 0x50, 0x06, 0xa0, 0x08, 0x3c, 0x0a, 0x0e, 0x0c, 0x1a, 0x0e,
    0x0c, 0x10, 0x18, 0x12, 0x30, 0x14, 0x60, 0x16, 0xc0, 0x18, 0x48, 0x1a, 0x10, 0x1c, 0x20, 0x1e,
];

#[derive(Debug, Default)]
pub struct LengthControl {
    counter: u8,
    is_halt: bool,
}

impl LengthControl {
    pub fn set_halt(&mut self, bits: impl GetLengthHalt) {
        self.is_halt = bits.length_halt();
    }

    /// Should zero output if counter reaches zero
    pub fn disabled(&self) -> bool {
        self.counter == 0
    }

    /// Clear the length counter.
    pub fn clear(&mut self) {
        self.counter = 0;
    }

    /// Load counter from length timer high 5 bits. Length timer high also reloads the counter.
    pub fn load(&mut self, bits: impl GetLengthIndex) {
        self.counter = LENGTH_TABLE[bits.length_idx() as usize];
    }

    /// Decrement counter if it's > 0.
    pub fn tick(&mut self) {
        if !self.is_halt {
            self.counter = self.counter.saturating_sub(1);
        }
    }
}

#[derive(Default, Debug)]
pub struct Envelope {
    start: bool,
    divider: u8,
    decay: u8,
}

impl Envelope {
    pub fn restart(&mut self) {
        self.start = true;
    }

    pub fn tick(&mut self, period: u8, looping: bool) {
        if self.start {
            self.start = false;
            self.decay = 15;
            self.divider = period;
            return;
        }

        if self.divider == 0 {
            self.divider = period;
            if self.decay == 0 {
                if looping {
                    self.decay = 15;
                }
            } else {
                self.decay -= 1;
            }
        } else {
            self.divider -= 1;
        }
    }

    pub fn decay(&self) -> u8 {
        self.decay
    }
}
