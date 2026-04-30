use super::*;

#[derive(Default, Debug)]
pub struct Triangle {
    control: TriangleControlBits,
    timer: Timer<u16>,
    length_control: LengthControl,
    linear_counter: u8,
    linear_counter_reload: bool,
    enabled: bool,
    sequence_step: usize,
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
    }

    pub fn reload_flag(&self) -> bool {
        self.control.reload_flag()
    }

    pub fn restore_reload_flag(&mut self, reload_flag: bool) {
        self.control = TriangleControlBits::new().with_reload_flag(reload_flag);
    }

    pub fn write_timer_low(&mut self, value: u8) {
        self.timer.set_period_low(value);
    }

    pub fn write_timer_high(&mut self, load: LengthTimerHigh3Bits) {
        self.timer.set_period_high(load.high3());
        if self.enabled {
            self.length_control.load(load.length());
        }
        self.linear_counter_reload = true;
    }

    pub fn step_timer(&mut self) {
        if self.timer.tick()
            && !self.length_control.disabled()
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

        if !self.control.reload_flag() {
            self.linear_counter_reload = false;
        }
    }

    pub fn step_length_counter(&mut self) {
        if !self.control.reload_flag() {
            self.length_control.tick();
        }
    }

    pub fn output(&self) -> u8 {
        if !self.enabled || self.length_control.disabled() || self.linear_counter == 0 {
            0
        } else {
            TRIANGLE_SEQUENCE[self.sequence_step]
        }
    }

    pub fn status_enabled(&self) -> bool {
        self.enabled && !self.length_control.disabled()
    }

    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
}
