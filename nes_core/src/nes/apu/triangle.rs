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

        if control_flag {
            // If the control flag is clear, the halt flag is cleared.
            self.halt = false;
        }
    }

    fn set_halt(&mut self) {
        self.halt = true;
    }
}

impl ControlGate for Linear {
    fn control(&self) -> u8 {
        self.counter
    }
}

#[derive(Debug)]
pub struct Triangle {
    control: TriangleControlBits,
    timer: Divider<u16>,
    length: LengthControl,
    sequence_step: usize,
    linear: Linear,
}

impl Default for Triangle {
    fn default() -> Self {
        Self {
            control: Default::default(),
            timer: Divider::new(u16::MAX),
            length: Default::default(),
            sequence_step: Default::default(),
            linear: Default::default(),
        }
    }
}

impl Triangle {
    pub fn set_enabled(&mut self, enabled: bool) {
        self.length.set_enabled(enabled);
    }

    pub fn write_control(&mut self, value: TriangleControlBits) {
        self.control = value;
        self.length.set_halt(value);
        self.linear.reload_value = value.counter();
    }

    pub fn is_halt(&self) -> bool {
        self.control.loop_and_is_halt()
    }

    pub fn restore_is_halt_flag(&mut self, is_halt: bool) {
        self.control = TriangleControlBits::new().with_loop_and_is_halt(is_halt);
        self.length.set_halt(self.control);
    }

    pub fn write_timer_low(&mut self, value: u8) {
        self.timer.set_period_low(value);
    }

    pub fn write_timer_high(&mut self, load: LengthTimerHigh3Bits) {
        self.timer.set_period_high(load.high3());
        self.length.load(load);

        // When register $400B is written to, the halt flag is set.
        self.linear.set_halt();
    }

    pub fn tick_timer(&mut self) {
        if self.timer.tick()
            && !self.length.is_zero()
            && self.linear.counter > 0
            && self.timer.period > 1
        {
            self.sequence_step = (self.sequence_step + 1) % TRIANGLE_SEQUENCE.len();
        }
    }

    pub fn tick_linear(&mut self) {
        self.linear.tick(self.control.loop_and_is_halt());
    }

    pub fn tick_length(&mut self) {
        self.length.tick();
    }

    pub fn output(&self) -> u8 {
        TRIANGLE_SEQUENCE[self.sequence_step]
    }

    pub fn status_bit(&self) -> bool {
        !self.length.is_zero()
    }
}
