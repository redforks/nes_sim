use crate::mcu::{MappingMcu, Mcu, Region};

/// ```
/// use nes_sim::nes::apu::Sweep;
/// let mut sweep: Sweep = 0x9Au8.into();
/// assert!(sweep.enabled());
/// assert_eq!(sweep.shift_count(), 2);
/// assert_eq!(sweep.divider(), 1);
/// assert!(sweep.negate());
/// ```
#[derive(Copy, Clone, Default)]
pub struct Sweep {
    v: u8,
}

impl From<Sweep> for u8 {
    fn from(s: Sweep) -> Self {
        s.v
    }
}

impl From<u8> for Sweep {
    fn from(v: u8) -> Self {
        Sweep { v }
    }
}

impl Sweep {
    /// ```
    /// use nes_sim::nes::apu::Sweep;
    /// let mut sweep = Sweep::default();
    /// assert!(!sweep.enabled());
    ///
    /// sweep.set_enabled(true);
    /// assert_eq!(sweep.enabled(), true);
    ///
    /// sweep.set_enabled(false);
    /// assert_eq!(sweep.enabled(), false);
    /// ```
    pub fn enabled(&self) -> bool {
        self.v & 0x80 != 0
    }

    pub fn set_enabled(&mut self, v: bool) {
        if v {
            self.v |= 0x80;
        } else {
            self.v &= !0x80;
        }
    }

    /// ```
    /// use nes_sim::nes::apu::Sweep;
    /// let mut sweep = Sweep::default();
    /// assert!(!sweep.negate());
    /// sweep.set_negate(true);
    /// assert_eq!(sweep.negate(), true);
    /// sweep.set_negate(false);
    /// assert_eq!(sweep.negate(), false);
    /// ```
    pub fn negate(&self) -> bool {
        self.v & 0x08 != 0
    }

    pub fn set_negate(&mut self, v: bool) {
        if v {
            self.v |= 0x08;
        } else {
            self.v &= !0x08;
        }
    }

    /// ```
    /// use nes_sim::nes::apu::Sweep;
    /// let mut sweep = Sweep::default();
    /// assert_eq!(sweep.divider(), 0);
    /// sweep.set_divider(0b111);
    /// assert_eq!(sweep.divider(), 0b111);
    /// ```
    pub fn divider(&self) -> u8 {
        (self.v >> 4) & 0x07
    }

    pub fn set_divider(&mut self, v: u8) {
        self.v = (self.v & !0x70) | ((v & 0x07) << 4);
    }

    /// ```
    /// use nes_sim::nes::apu::Sweep;
    /// let mut sweep = Sweep::default();
    /// assert_eq!(sweep.shift_count(), 0);
    /// sweep.set_shift_count(3);
    /// assert_eq!(sweep.shift_count(), 3);
    /// ```
    pub fn shift_count(&self) -> u8 {
        self.v & 0x07
    }

    pub fn set_shift_count(&mut self, v: u8) {
        self.v = (self.v & !0x07) | (v & 0x07);
    }
}

#[derive(Copy, Clone, Default)]
pub struct DutyCycle {
    v: u8,
}

impl From<DutyCycle> for u8 {
    fn from(d: DutyCycle) -> Self {
        d.v
    }
}

impl From<u8> for DutyCycle {
    fn from(v: u8) -> Self {
        DutyCycle { v }
    }
}

impl DutyCycle {
    /// ```
    /// use nes_sim::nes::apu::DutyCycle;
    /// let mut duty_cycle = DutyCycle::default();
    /// assert_eq!(duty_cycle.duty(), 0);
    /// duty_cycle.set_duty(0b10);
    /// assert_eq!(duty_cycle.duty(), 0b10);
    /// duty_cycle.set_duty(0b11);
    /// assert_eq!(duty_cycle.duty(), 0b11);
    /// ```
    pub fn duty(&self) -> u8 {
        (self.v & 0xC0) >> 6
    }

    pub fn set_duty(&mut self, v: u8) {
        assert!(v <= 0b11);
        self.v = (self.v & !0xC0) | ((v & 0x03) << 6);
    }

    /// ```
    /// use nes_sim::nes::apu::DutyCycle;
    /// let mut duty_cycle = DutyCycle::default();
    /// assert_eq!(duty_cycle.length_counter_halt(), false);
    /// duty_cycle.set_length_counter_halt(true);
    /// assert_eq!(duty_cycle.length_counter_halt(), true);
    /// duty_cycle.set_length_counter_halt(false);
    /// assert_eq!(duty_cycle.length_counter_halt(), false);
    /// ```
    pub fn length_counter_halt(&self) -> bool {
        self.v & 0x20 != 0
    }

    pub fn set_length_counter_halt(&mut self, v: bool) {
        if v {
            self.v |= 0x20;
        } else {
            self.v &= !0x20;
        }
    }

    /// ```
    /// use nes_sim::nes::apu::DutyCycle;
    /// let mut duty_cycle = DutyCycle::default();
    /// assert_eq!(duty_cycle.constant_volume(), false);
    /// duty_cycle.set_constant_volume(true);
    /// assert_eq!(duty_cycle.constant_volume(), true);
    /// duty_cycle.set_constant_volume(false);
    /// assert_eq!(duty_cycle.constant_volume(), false);
    /// ```
    pub fn constant_volume(&self) -> bool {
        self.v & 0x10 != 0
    }

    pub fn set_constant_volume(&mut self, v: bool) {
        if v {
            self.v |= 0x10;
        } else {
            self.v &= !0x10;
        }
    }

    /// ```
    /// use nes_sim::nes::apu::DutyCycle;
    /// let mut duty_cycle = DutyCycle::default();
    /// assert_eq!(duty_cycle.volume(), 0);
    /// duty_cycle.set_volume(0b111);
    /// assert_eq!(duty_cycle.volume(), 0b111);
    /// ```
    pub fn volume(&self) -> u8 {
        self.v & 0x0F
    }

    pub fn set_volume(&mut self, v: u8) {
        self.v = (self.v & !0x0F) | (v & 0x0F);
    }
}

#[derive(Copy, Clone, Default)]
pub struct LengthCounterLoad {
    low_byte: u8,
    high_byte: u8,
}

impl LengthCounterLoad {
    fn write_low_byte(&mut self, v: u8) {
        self.low_byte = v;
    }

    fn write_high_byte(&mut self, v: u8) {
        self.high_byte = v;
    }

    pub fn length_count(&self) -> u8 {
        self.high_byte & 0xF8 >> 3
    }

    pub fn timer(&self) -> u16 {
        (((self.high_byte & 0x07) as u16) << 8) | (self.low_byte as u16)
    }
}

#[derive(Copy, Clone, Default)]
pub struct LinearCounterControl {
    v: u8,
}

impl From<LinearCounterControl> for u8 {
    fn from(l: LinearCounterControl) -> Self {
        l.v
    }
}

impl From<u8> for LinearCounterControl {
    fn from(v: u8) -> Self {
        LinearCounterControl { v }
    }
}

impl LinearCounterControl {
    /// ```
    /// use nes_sim::nes::apu::LinearCounterControl;
    /// let mut linear_counter_control = LinearCounterControl::default();
    /// assert_eq!(linear_counter_control.reload_flag(), false);
    /// linear_counter_control.set_reload_flag(true);
    /// assert_eq!(linear_counter_control.reload_flag(), true);
    /// linear_counter_control.set_reload_flag(false);
    /// assert_eq!(linear_counter_control.reload_flag(), false);
    /// ```
    pub fn reload_flag(&self) -> bool {
        self.v & 0x80 != 0
    }

    pub fn set_reload_flag(&mut self, v: bool) {
        if v {
            self.v |= 0x80;
        } else {
            self.v &= !0x80;
        }
    }

    /// ```
    /// use nes_sim::nes::apu::LinearCounterControl;
    /// let mut linear_counter_control = LinearCounterControl::default();
    /// assert_eq!(linear_counter_control.counter(), 0);
    /// linear_counter_control.set_counter(0b1111);
    /// assert_eq!(linear_counter_control.counter(), 0b1111);
    /// ```
    pub fn counter(&self) -> u8 {
        self.v & 0x7F
    }

    pub fn set_counter(&mut self, v: u8) {
        self.v = (self.v & !0x7F) | (v & 0x7F);
    }
}

pub trait PulseDriver {
    fn set_duty_cycle(&mut self, duty_cycle: DutyCycle);

    fn set_sweep(&mut self, sweep: Sweep);

    fn set_length_counter_load(&mut self, length_counter: LengthCounterLoad);
}

pub struct PulseChannel<D: PulseDriver> {
    start_addr: u16,
    driver: D,
    length_counter_load: LengthCounterLoad,
}

impl<D: PulseDriver> Mcu for PulseChannel<D> {
    fn read(&self, _: u16) -> u8 {
        panic!("Can not from PulseChannel");
    }

    fn write(&mut self, address: u16, value: u8) {
        match address - self.start_addr {
            0 => self.driver.set_duty_cycle(value.into()),
            1 => self.driver.set_sweep(value.into()),
            2 => self.length_counter_load.write_low_byte(value),
            3 => {
                self.length_counter_load.write_high_byte(value);
                self.driver
                    .set_length_counter_load(self.length_counter_load);
            }
            _ => panic!("Can not write to PulseChannel at address {}", address),
        }
    }
}

impl<D: PulseDriver> PulseChannel<D> {
    pub fn new(start_addr: u16, driver: D) -> Self {
        PulseChannel {
            start_addr,
            driver,
            length_counter_load: LengthCounterLoad::default(),
        }
    }
}

pub trait TriangleDriver {
    fn set_linear_counter_control(&mut self, linear_counter_control: LinearCounterControl);
    fn set_length_counter_load(&mut self, length_counter: LengthCounterLoad);
}

pub struct TriangleChannel<D: TriangleDriver> {
    driver: D,
    length_counter_load: LengthCounterLoad,
}

impl<D: TriangleDriver> TriangleChannel<D> {
    pub fn new(driver: D) -> Self {
        TriangleChannel {
            driver,
            length_counter_load: LengthCounterLoad::default(),
        }
    }
}

impl<D: TriangleDriver> Mcu for TriangleChannel<D> {
    fn read(&self, _: u16) -> u8 {
        panic!("Can not read from TriangleChannel");
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x4008 => self.driver.set_linear_counter_control(value.into()),
            0x400A => self.length_counter_load.write_low_byte(value),
            0x400B => {
                self.length_counter_load.write_high_byte(value);
                self.driver
                    .set_length_counter_load(self.length_counter_load);
            }
            _ => panic!("Can not write to TriangleChannel at address {}", address),
        }
    }
}

pub fn new<PD, TD>(pd1: PD, pd2: PD, td: TD) -> Vec<Region>
where
    PD: PulseDriver + 'static,
    TD: TriangleDriver + 'static,
{
    vec![
        Region::new(0x4000, 0x4003, Box::new(PulseChannel::new(0x4000, pd1))),
        Region::new(0x4004, 0x4007, Box::new(PulseChannel::new(0x4004, pd2))),
        Region::new(0x4008, 0x400B, Box::new(TriangleChannel::new(td))),
    ]
}
