use crate::mcu::{Mcu, Region};
use modular_bitfield::prelude::*;

macro_rules! to_from_u8 {
    ($t: ty) => {
        impl From<$t> for u8 {
            fn from(n: $t) -> Self {
                n.into_bytes()[0]
            }
        }

        impl From<u8> for $t {
            fn from(v: u8) -> Self {
                <$t>::from_bytes([v])
            }
        }
    };
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct Sweep {
    pub enabled: bool,
    pub period: B3,
    pub negate: bool,
    pub shift: B3,
}

to_from_u8!(Sweep);

#[bitfield]
pub struct DutyCycle {
    pub duty: B2,
    pub length_counter_halt: bool,
    pub constant_volume: bool,
    pub volume: B4,
}

to_from_u8!(DutyCycle);

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

#[derive(Copy, Clone)]
#[bitfield]
pub struct LinearCounterControl {
    pub reload_flag: bool,
    pub counter: B7,
}

to_from_u8!(LinearCounterControl);

pub trait PulseDriver {
    fn set_duty_cycle(&mut self, duty_cycle: DutyCycle);

    fn set_sweep(&mut self, sweep: Sweep);

    fn set_length_counter_load(&mut self, length_counter: LengthCounterLoad);
}

struct PulseChannel<D: PulseDriver> {
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

struct TriangleChannel<D: TriangleDriver> {
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

#[derive(Copy, Clone)]
#[bitfield]
pub struct NoiseEnvelop {
    #[allow(non_snake_case)]
    #[skip]
    __: B2,
    pub loop_flag: bool,
    pub constant_volume: bool,
    pub volume: B4,
}

to_from_u8!(NoiseEnvelop);

#[derive(Copy, Clone)]
#[bitfield]
pub struct NoisePeriod {
    pub enabled: bool,

    #[allow(non_snake_case)]
    #[skip]
    __: B3,

    pub period: B4,
}

to_from_u8!(NoisePeriod);

#[derive(Copy, Clone)]
#[bitfield]
pub struct NoiseLength {
    pub length: B5,
    #[allow(non_snake_case)]
    #[skip]
    __: B3,
}

to_from_u8!(NoiseLength);

pub trait NoiseDriver {
    fn set_envelop(&mut self, envelop: NoiseEnvelop);
    fn set_period(&mut self, period: NoisePeriod);
    fn set_length(&mut self, length: NoiseLength);
}

struct NoiseChannel<D: NoiseDriver> {
    driver: D,
}

impl<D: NoiseDriver> Mcu for NoiseChannel<D> {
    fn read(&self, _: u16) -> u8 {
        panic!("Can not from PulseChannel");
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x400C => self.driver.set_envelop(value.into()),
            0x400E => self.driver.set_period(value.into()),
            0x400F => self.driver.set_length(value.into()),
            _ => panic!("Can not write to NoiseChannel at address {}", address),
        }
    }
}

pub fn new<PD, TD, ND>(pd1: PD, pd2: PD, td: TD, nd: ND) -> Vec<Region>
where
    PD: PulseDriver + 'static,
    TD: TriangleDriver + 'static,
    ND: NoiseDriver + 'static,
{
    vec![
        Region::new(0x4000, 0x4003, Box::new(PulseChannel::new(0x4000, pd1))),
        Region::new(0x4004, 0x4007, Box::new(PulseChannel::new(0x4004, pd2))),
        Region::new(0x4008, 0x400B, Box::new(TriangleChannel::new(td))),
        Region::new(0x400C, 0x400F, Box::new(NoiseChannel { driver: nd })),
    ]
}
