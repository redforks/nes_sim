use super::*;

const TRIANGLE_SEQUENCE: [u8; 32] = [
    15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
    13, 14, 15,
];

#[derive(Debug, Default)]
struct Linear {
    counter: u8,
    halt: bool,
    reload_value: u8,
}

impl Linear {
    fn tick(&mut self, control_flag: bool) {
        if self.halt {
            // When the halt flag is set, the linear counter is reloaded with the value from the control register.
            self.counter = self.reload_value;
        } else if self.counter > 0 {
            // If the halt flag is clear and the linear counter is not zero, it is decremented.
            self.counter -= 1;
        }

        if !control_flag {
            // If the control flag is clear, the halt flag is cleared.
            self.halt = false;
        }
    }

    fn set_halt(&mut self) {
        self.halt = true;
    }
}

#[derive(Debug)]
pub struct Triangle {
    timer: Divider<u16>,
    length: LengthControl,
    sequencer: Sequencer<u8>,
    linear: Linear,
}

impl Default for Triangle {
    fn default() -> Self {
        Self {
            timer: Divider::new(u16::MAX),
            length: Default::default(),
            sequencer: Sequencer::new(&TRIANGLE_SEQUENCE),
            linear: Default::default(),
        }
    }
}

impl Triangle {
    pub fn set_enabled(&mut self, enabled: bool) {
        self.length.set_enabled(enabled);
    }

    pub fn write_control(&mut self, value: TriangleControlBits) {
        self.length.set_halt(value);
        self.linear.reload_value = value.counter();
    }

    /// Reset the channel while preserving the length-counter halt flag,
    /// which survives an APU reset.
    pub fn reset(&mut self) {
        let length_counter_halt = self.length.is_halt();
        *self = Self::default();
        self.length.set_halt_flag(length_counter_halt);
    }

    pub fn write_timer_low(&mut self, value: u8) {
        self.timer.set_period_low(value);
    }

    pub fn write_timer_high(&mut self, load: LengthTimerHigh3Bits, suppress_length: bool) {
        self.timer.set_period_high(load.high3());
        if !suppress_length {
            self.length.load(load);
        }

        // When register $400B is written to, the halt flag is set.
        self.linear.set_halt();
    }

    pub fn tick_timer(&mut self) {
        if self.timer.tick() && !self.length.is_zero() && self.linear.counter > 0 {
            self.sequencer.tick();
        }
    }

    pub fn tick_linear(&mut self) {
        self.linear.tick(self.length.is_halt());
    }

    pub fn tick_length(&mut self) -> bool {
        self.length.tick()
    }

    pub fn output(&self) -> u8 {
        if self.length.is_zero() {
            0
        } else {
            self.sequencer.output()
        }
    }

    pub fn status_bit(&self) -> bool {
        !self.length.is_zero()
    }
}
