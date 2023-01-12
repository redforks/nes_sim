use crate::mcu::{Mcu, Region};
use crate::to_from_u8;
use modular_bitfield::prelude::*;

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

struct NoiseChannel<D: NoiseDriver>(D);

impl<D: NoiseDriver> Mcu for NoiseChannel<D> {
    fn read(&self, _: u16) -> u8 {
        panic!("Can not from PulseChannel");
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x400C => self.0.set_envelop(value.into()),
            0x400E => self.0.set_period(value.into()),
            0x400F => self.0.set_length(value.into()),
            _ => panic!("Can not write to NoiseChannel at address {}", address),
        }
    }
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct DmcIRQLoopFreq {
    pub irq_enabled: bool,
    pub loop_flag: bool,
    #[allow(non_snake_case)]
    #[skip]
    __: B2,
    pub freq: B4,
}
to_from_u8!(DmcIRQLoopFreq);

pub trait DmcDriver {
    fn set_irq_loop_freq(&mut self, irq_loop_freq: DmcIRQLoopFreq);
    fn set_load_counter(&mut self, counter: u8);
    fn set_sample_address(&mut self, addr: u8);
    fn set_sample_length(&mut self, length: u8);
}

struct DmcChannel<D: DmcDriver>(D);

impl<D: DmcDriver> Mcu for DmcChannel<D> {
    fn read(&self, address: u16) -> u8 {
        panic!("Can not read from DmcChannel at address {}", address);
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x4010 => self.0.set_irq_loop_freq(value.into()),
            0x4011 => self.0.set_load_counter(value),
            0x4012 => self.0.set_sample_address(value),
            0x4013 => self.0.set_sample_length(value),
            _ => panic!("Can not write to DmcChannel at address {}", address),
        }
    }
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct ControlFlags {
    #[allow(non_snake_case)]
    #[skip]
    __: B3,
    pub dmc_enabled: bool,
    pub noise_enabled: bool,
    pub triangle_enabled: bool,
    pub pulse1_enabled: bool,
    pub pulse2_enabled: bool,
}
to_from_u8!(ControlFlags);

#[derive(Copy, Clone)]
#[bitfield]
pub struct APUStatus {
    pub dmc_interrupt: bool,
    pub frame_interrupt: bool,
    #[allow(non_snake_case)]
    #[skip]
    __: B1,
    pub dmc_enabled: bool,
    pub noise_enabled: bool,
    pub triangle_enabled: bool,
    pub pulse1_enabled: bool,
    pub pulse2_enabled: bool,
}
to_from_u8!(APUStatus);

#[derive(Copy, Clone)]
#[bitfield]
pub struct FrameCounter {
    pub mode: bool,
    pub interrupt_flag: bool,
    #[allow(non_snake_case)]
    #[skip]
    __: B6,
}
to_from_u8!(FrameCounter);

pub trait APUControllerDriver {
    fn set_control_flags(&mut self, flags: ControlFlags);
    fn set_frame_counter(&mut self, counter: FrameCounter);
    fn read_status(&self) -> APUStatus;
}

struct APUController<D: APUControllerDriver>(D);

impl<D: APUControllerDriver> Mcu for APUController<D> {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x4015 => self.0.read_status().into(),
            _ => panic!("Can not read from APUController at address {}", address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x4015 => self.0.set_control_flags(value.into()),
            0x4017 => self.0.set_frame_counter(value.into()),
            _ => panic!("Can not write to APUController at address {}", address),
        }
    }
}

pub fn new<PD, TD, ND, DD, CD>(pd1: PD, pd2: PD, td: TD, nd: ND, dd: DD, cd: CD) -> Vec<Region>
where
    PD: PulseDriver + 'static,
    TD: TriangleDriver + 'static,
    ND: NoiseDriver + 'static,
    DD: DmcDriver + 'static,
    CD: APUControllerDriver + 'static,
{
    vec![
        Region::new(0x4000, 0x4003, Box::new(PulseChannel::new(0x4000, pd1))),
        Region::new(0x4004, 0x4007, Box::new(PulseChannel::new(0x4004, pd2))),
        Region::new(0x4008, 0x400B, Box::new(TriangleChannel::new(td))),
        Region::new(0x400C, 0x400F, Box::new(NoiseChannel(nd))),
        Region::new(0x4010, 0x4013, Box::new(DmcChannel(dd))),
        Region::new(0x4015, 0x4015, Box::new(APUController(cd))),
    ]
}
