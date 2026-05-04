use super::*;

const TRIANGLE_SEQUENCE: [u8; 32] = [
    15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
    13, 14, 15,
];

#[derive(Debug)]
pub struct Triangle {
    control: TriangleControlBits,
    timer: Divider<u16>,
    length_control: LengthControl,
    linear_counter: u8,
    linear_counter_reload: bool,
    enabled: bool,
    sequence_step: usize,
}

impl Default for Triangle {
    fn default() -> Self {
        Self {
            control: Default::default(),
            timer: Divider::new(u16::MAX),
            length_control: Default::default(),
            linear_counter: Default::default(),
            linear_counter_reload: Default::default(),
            enabled: Default::default(),
            sequence_step: Default::default(),
        }
    }
}

impl Triangle {
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.length_control.clear();
        }
    }

    pub fn write_control(&mut self, value: TriangleControlBits) {
        self.control = value;
        self.length_control.set_halt(value);
    }

    pub fn is_halt(&self) -> bool {
        self.control.loop_and_is_halt()
    }

    pub fn restore_is_halt_flag(&mut self, is_halt: bool) {
        self.control = TriangleControlBits::new().with_loop_and_is_halt(is_halt);
    }

    pub fn write_timer_low(&mut self, value: u8) {
        self.timer.set_period_low(value);
    }

    pub fn write_timer_high(&mut self, load: LengthTimerHigh3Bits) {
        self.timer.set_period_high(load.high3());
        if self.enabled {
            self.length_control.load(load);
        }
        self.linear_counter_reload = true;
    }

    pub fn step_timer(&mut self) {
        if self.timer.tick()
            && !self.length_control.is_zero()
            && self.linear_counter > 0
            && self.timer.period > 1
        {
            self.sequence_step = (self.sequence_step + 1) % TRIANGLE_SEQUENCE.len();
        }
    }

    pub fn step_linear_counter(&mut self) {
        if self.linear_counter_reload {
            self.linear_counter = self.control.counter();
        } else if self.linear_counter > 0 {
            self.linear_counter -= 1;
        }

        if !self.control.loop_and_is_halt() {
            self.linear_counter_reload = false;
        }
    }

    pub fn step_length_counter(&mut self) {
        self.length_control.tick();
    }

    pub fn output(&self) -> u8 {
        if !self.enabled || self.length_control.is_zero() || self.linear_counter == 0 {
            0
        } else {
            TRIANGLE_SEQUENCE[self.sequence_step]
        }
    }

    pub fn status_bit(&self) -> bool {
        !self.length_control.is_zero()
    }

    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
}
