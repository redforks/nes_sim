use crate::mcu::{DefinedRegion, Mcu, Region};
use crate::to_from_u8;
use modular_bitfield::prelude::*;
use std::cell::RefCell;

#[derive(Copy, Clone)]
#[bitfield]
pub struct Sweep {
    pub shift: B3,
    pub negate: bool,
    pub period: B3,
    pub enabled: bool,
}

to_from_u8!(Sweep);

impl Default for Sweep {
    fn default() -> Self {
        0u8.into()
    }
}

#[bitfield]
pub struct DutyCycle {
    pub volume: B4,
    pub constant_volume: bool,
    pub length_counter_halt: bool,
    pub duty: B2,
}

to_from_u8!(DutyCycle);

impl Default for DutyCycle {
    fn default() -> Self {
        0u8.into()
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

    pub fn from_registers(low_byte: u8, high_byte: u8) -> Self {
        Self {
            low_byte,
            high_byte,
        }
    }

    pub fn length_count(&self) -> u8 {
        (self.high_byte & 0xF8) >> 3
    }

    pub fn timer(&self) -> u16 {
        (((self.high_byte & 0x07) as u16) << 8) | (self.low_byte as u16)
    }
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct LinearCounterControl {
    pub counter: B7,
    pub reload_flag: bool,
}

to_from_u8!(LinearCounterControl);

impl Default for LinearCounterControl {
    fn default() -> Self {
        0u8.into()
    }
}

pub trait PulseDriver {
    fn set_duty_cycle(&mut self, duty_cycle: DutyCycle);

    fn set_sweep(&mut self, sweep: Sweep);

    fn set_length_counter_load(&mut self, length_counter: LengthCounterLoad);
}

struct PulseChannel<D: PulseDriver> {
    start_addr: u16,
    driver: RefCell<D>,
    length_counter_load: RefCell<LengthCounterLoad>,
}

impl<D: PulseDriver> Mcu for PulseChannel<D> {
    fn read(&mut self, _: u16) -> u8 {
        panic!("Can not from PulseChannel");
    }

    fn write(&mut self, address: u16, value: u8) {
        match address - self.start_addr {
            0 => self.driver.borrow_mut().set_duty_cycle(value.into()),
            1 => self.driver.borrow_mut().set_sweep(value.into()),
            2 => self.length_counter_load.borrow_mut().write_low_byte(value),
            3 => {
                let mut load = self.length_counter_load.borrow_mut();
                load.write_high_byte(value);
                self.driver.borrow_mut().set_length_counter_load(*load);
            }
            _ => panic!("Can not write to PulseChannel at address {}", address),
        }
    }
}

impl<D: PulseDriver> DefinedRegion for PulseChannel<D> {
    fn region(&self) -> (u16, u16) {
        (self.start_addr, self.start_addr + 4 - 1)
    }
}

impl<D: PulseDriver> PulseChannel<D> {
    pub fn new(start_addr: u16, driver: D) -> Self {
        PulseChannel {
            start_addr,
            driver: RefCell::new(driver),
            length_counter_load: RefCell::new(LengthCounterLoad::default()),
        }
    }
}

pub trait TriangleDriver {
    fn set_linear_counter_control(&mut self, linear_counter_control: LinearCounterControl);
    fn set_length_counter_load(&mut self, length_counter: LengthCounterLoad);
}

struct TriangleChannel<D: TriangleDriver> {
    driver: RefCell<D>,
    length_counter_load: RefCell<LengthCounterLoad>,
}

impl<D: TriangleDriver> TriangleChannel<D> {
    pub fn new(driver: D) -> Self {
        TriangleChannel {
            driver: RefCell::new(driver),
            length_counter_load: RefCell::new(LengthCounterLoad::default()),
        }
    }
}

impl<D: TriangleDriver> DefinedRegion for TriangleChannel<D> {
    fn region(&self) -> (u16, u16) {
        (0x4008, 0x400B)
    }
}

impl<D: TriangleDriver> Mcu for TriangleChannel<D> {
    fn read(&mut self, _: u16) -> u8 {
        panic!("Can not read from TriangleChannel");
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x4008 => self
                .driver
                .borrow_mut()
                .set_linear_counter_control(value.into()),
            0x400A => self.length_counter_load.borrow_mut().write_low_byte(value),
            0x400B => {
                let mut load = self.length_counter_load.borrow_mut();
                load.write_high_byte(value);
                self.driver.borrow_mut().set_length_counter_load(*load);
            }
            _ => panic!("Can not write to TriangleChannel at address {}", address),
        }
    }
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct NoiseEnvelop {
    pub volume: B4,
    pub constant_volume: bool,
    pub loop_flag: bool,
    #[allow(non_snake_case)]
    #[skip]
    __: B2,
}

to_from_u8!(NoiseEnvelop);

impl Default for NoiseEnvelop {
    fn default() -> Self {
        0u8.into()
    }
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct NoisePeriod {
    pub period: B4,

    #[allow(non_snake_case)]
    #[skip]
    __: B3,

    pub enabled: bool,
}

to_from_u8!(NoisePeriod);

impl Default for NoisePeriod {
    fn default() -> Self {
        0u8.into()
    }
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct NoiseLength {
    #[allow(non_snake_case)]
    #[skip]
    __: B3,

    pub length: B5,
}

to_from_u8!(NoiseLength);

impl Default for NoiseLength {
    fn default() -> Self {
        0u8.into()
    }
}

pub trait NoiseDriver {
    fn set_envelop(&mut self, envelop: NoiseEnvelop);
    fn set_period(&mut self, period: NoisePeriod);
    fn set_length(&mut self, length: NoiseLength);
}

struct NoiseChannel<D: NoiseDriver>(RefCell<D>);

impl<D: NoiseDriver> Mcu for NoiseChannel<D> {
    fn read(&mut self, _: u16) -> u8 {
        panic!("Can not from PulseChannel");
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x400C => self.0.borrow_mut().set_envelop(value.into()),
            0x400E => self.0.borrow_mut().set_period(value.into()),
            0x400F => self.0.borrow_mut().set_length(value.into()),
            _ => panic!("Can not write to NoiseChannel at address {}", address),
        }
    }
}

impl<D: NoiseDriver> DefinedRegion for NoiseChannel<D> {
    fn region(&self) -> (u16, u16) {
        (0x400C, 0x400F)
    }
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct DmcIRQLoopFreq {
    pub freq: B4,
    #[allow(non_snake_case)]
    #[skip]
    __: B2,
    pub loop_flag: bool,
    pub irq_enabled: bool,
}
to_from_u8!(DmcIRQLoopFreq);

impl Default for DmcIRQLoopFreq {
    fn default() -> Self {
        0u8.into()
    }
}

pub trait DmcDriver {
    fn set_irq_loop_freq(&mut self, irq_loop_freq: DmcIRQLoopFreq);
    fn set_load_counter(&mut self, counter: u8);
    fn set_sample_address(&mut self, addr: u8);
    fn set_sample_length(&mut self, length: u8);
}

struct DmcChannel<D: DmcDriver>(RefCell<D>);

impl<D: DmcDriver> Mcu for DmcChannel<D> {
    fn read(&mut self, address: u16) -> u8 {
        panic!("Can not read from DmcChannel at address {}", address);
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x4010 => self.0.borrow_mut().set_irq_loop_freq(value.into()),
            0x4011 => self.0.borrow_mut().set_load_counter(value),
            0x4012 => self.0.borrow_mut().set_sample_address(value),
            0x4013 => self.0.borrow_mut().set_sample_length(value),
            _ => panic!("Can not write to DmcChannel at address {}", address),
        }
    }
}

impl<D: DmcDriver> DefinedRegion for DmcChannel<D> {
    fn region(&self) -> (u16, u16) {
        (0x4010, 0x4013)
    }
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct ControlFlags {
    pub pulse1_enabled: bool,
    pub pulse2_enabled: bool,
    pub triangle_enabled: bool,
    pub noise_enabled: bool,
    pub dmc_enabled: bool,
    #[allow(non_snake_case)]
    #[skip]
    __: B3,
}
to_from_u8!(ControlFlags);

impl Default for ControlFlags {
    fn default() -> Self {
        0u8.into()
    }
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct APUStatus {
    pub pulse1_enabled: bool,
    pub pulse2_enabled: bool,
    pub triangle_enabled: bool,
    pub noise_enabled: bool,
    pub dmc_enabled: bool,
    #[allow(non_snake_case)]
    #[skip]
    __: B1,
    pub frame_interrupt: bool,
    pub dmc_interrupt: bool,
}
to_from_u8!(APUStatus);

impl Default for APUStatus {
    fn default() -> Self {
        0u8.into()
    }
}

#[derive(Copy, Clone)]
#[bitfield]
pub struct FrameCounter {
    #[allow(non_snake_case)]
    #[skip]
    __: B6,
    pub interrupt_flag: bool,
    pub mode: bool,
}
to_from_u8!(FrameCounter);

impl Default for FrameCounter {
    fn default() -> Self {
        0u8.into()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LengthCounterChannel {
    Pulse1,
    Pulse2,
    Triangle,
    Noise,
}

impl LengthCounterChannel {
    fn index(self) -> usize {
        match self {
            LengthCounterChannel::Pulse1 => 0,
            LengthCounterChannel::Pulse2 => 1,
            LengthCounterChannel::Triangle => 2,
            LengthCounterChannel::Noise => 3,
        }
    }
}

pub trait APUControllerDriver {
    fn set_control_flags(&mut self, flags: ControlFlags);
    fn set_frame_counter(&mut self, counter: FrameCounter);
    fn read_status(&mut self) -> APUStatus;

    fn tick(&mut self) -> bool {
        false
    }

    fn request_irq(&self) -> bool {
        false
    }

    fn set_length_counter_halt(&mut self, _channel: LengthCounterChannel, _halt: bool) {}

    fn set_length_counter_load(
        &mut self,
        _channel: LengthCounterChannel,
        _length_counter: LengthCounterLoad,
    ) {
    }
}

pub struct ApuController<D: APUControllerDriver>(RefCell<D>);

impl<D: APUControllerDriver> ApuController<D> {
    pub fn new(driver: D) -> Self {
        Self(RefCell::new(driver))
    }

    pub fn tick(&self) -> bool {
        self.0.borrow_mut().tick()
    }

    pub fn request_irq(&self) -> bool {
        self.0.borrow().request_irq()
    }

    pub fn set_length_counter_halt(&self, channel: LengthCounterChannel, halt: bool) {
        self.0.borrow_mut().set_length_counter_halt(channel, halt);
    }

    pub fn set_length_counter_load(
        &self,
        channel: LengthCounterChannel,
        length_counter: LengthCounterLoad,
    ) {
        self.0
            .borrow_mut()
            .set_length_counter_load(channel, length_counter);
    }
}

impl<D: APUControllerDriver> Mcu for ApuController<D> {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x4015 => self.0.borrow_mut().read_status().into(),
            _ => panic!("Can not read from ApuController at address {}", address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x4015 => self.0.borrow_mut().set_control_flags(value.into()),
            0x4017 => self.0.borrow_mut().set_frame_counter(value.into()),
            _ => panic!("Can not write to ApuController at address {}", address),
        }
    }
}

impl<D: APUControllerDriver> DefinedRegion for ApuController<D> {
    fn region(&self) -> (u16, u16) {
        (0x4015, 0x4017)
    }
}

pub struct FakeApuControllerDriver {
    apu_cycle: u64,
    apu_even_cycle: bool,
    frame_counter_mode: bool,
    frame_interrupt_inhibit: bool,
    frame_interrupt: bool,
    dmc_interrupt: bool,
    length_counters: [u8; 4],
    length_counter_halt: [bool; 4],
    channel_enabled: [bool; 5],
}

impl Default for FakeApuControllerDriver {
    fn default() -> Self {
        Self {
            apu_cycle: 0,
            apu_even_cycle: false,
            frame_counter_mode: false,
            frame_interrupt_inhibit: false,
            frame_interrupt: false,
            dmc_interrupt: false,
            length_counters: [0; 4],
            length_counter_halt: [false; 4],
            channel_enabled: [false; 5],
        }
    }
}

impl FakeApuControllerDriver {
    const LENGTH_TABLE: [u8; 32] = [
        10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96,
        22, 192, 24, 72, 26, 16, 28, 32, 30,
    ];

    fn clock_length_counters(&mut self) {
        for i in 0..self.length_counters.len() {
            if self.channel_enabled[i]
                && !self.length_counter_halt[i]
                && self.length_counters[i] > 0
            {
                self.length_counters[i] -= 1;
            }
        }
    }

    fn set_channel_enabled(&mut self, channel: usize, enabled: bool) {
        self.channel_enabled[channel] = enabled;
        if channel < self.length_counters.len() && !enabled {
            self.length_counters[channel] = 0;
        }
    }

    fn status_channel_enabled(&self, channel: usize) -> bool {
        self.channel_enabled[channel] && self.length_counters[channel] > 0
    }
}

impl APUControllerDriver for FakeApuControllerDriver {
    fn set_control_flags(&mut self, flags: ControlFlags) {
        self.set_channel_enabled(0, flags.pulse1_enabled());
        self.set_channel_enabled(1, flags.pulse2_enabled());
        self.set_channel_enabled(2, flags.triangle_enabled());
        self.set_channel_enabled(3, flags.noise_enabled());
        self.channel_enabled[4] = flags.dmc_enabled();
        if !self.channel_enabled[4] {
            self.dmc_interrupt = false;
        }
    }

    fn set_frame_counter(&mut self, counter: FrameCounter) {
        self.frame_counter_mode = counter.mode();
        self.frame_interrupt_inhibit = counter.interrupt_flag();
        if self.frame_interrupt_inhibit {
            self.frame_interrupt = false;
        }
        self.apu_cycle = 0;
        self.apu_even_cycle = false;

        if self.frame_counter_mode {
            self.clock_length_counters();
        }
    }

    fn read_status(&mut self) -> APUStatus {
        let mut status = APUStatus::new();
        status.set_pulse1_enabled(self.status_channel_enabled(0));
        status.set_pulse2_enabled(self.status_channel_enabled(1));
        status.set_triangle_enabled(self.status_channel_enabled(2));
        status.set_noise_enabled(self.status_channel_enabled(3));
        status.set_dmc_enabled(self.channel_enabled[4]);
        status.set_frame_interrupt(self.frame_interrupt);
        status.set_dmc_interrupt(self.dmc_interrupt);
        self.frame_interrupt = false;
        status
    }

    fn tick(&mut self) -> bool {
        self.apu_even_cycle = !self.apu_even_cycle;
        if !self.apu_even_cycle {
            return false;
        }

        self.apu_cycle = self.apu_cycle.wrapping_add(1);

        if self.frame_counter_mode {
            let cycle_mod = self.apu_cycle % 18641;
            if cycle_mod == 7456 || cycle_mod == 18640 {
                self.clock_length_counters();
            }
            return false;
        }

        let cycle_mod = self.apu_cycle % 14915;
        if cycle_mod == 7456 || cycle_mod == 14914 {
            self.clock_length_counters();
        }
        if cycle_mod == 14914 && !self.frame_interrupt_inhibit {
            self.frame_interrupt = true;
            return true;
        }

        false
    }

    fn request_irq(&self) -> bool {
        self.frame_interrupt
    }

    fn set_length_counter_halt(&mut self, channel: LengthCounterChannel, halt: bool) {
        self.length_counter_halt[channel.index()] = halt;
    }

    fn set_length_counter_load(
        &mut self,
        channel: LengthCounterChannel,
        length_counter: LengthCounterLoad,
    ) {
        self.length_counters[channel.index()] =
            Self::LENGTH_TABLE[length_counter.length_count() as usize];
    }
}

pub fn new<PD, TD, ND, DD, CD>(
    pd1: PD,
    pd2: PD,
    td: TD,
    nd: ND,
    dd: DD,
    cd: CD,
) -> impl IntoIterator<Item = Region>
where
    PD: PulseDriver + 'static,
    TD: TriangleDriver + 'static,
    ND: NoiseDriver + 'static,
    DD: DmcDriver + 'static,
    CD: APUControllerDriver + 'static,
{
    [
        Region::with_defined(PulseChannel::new(0x4000, pd1)),
        Region::with_defined(PulseChannel::new(0x4004, pd2)),
        Region::with_defined(TriangleChannel::new(td)),
        Region::with_defined(NoiseChannel(RefCell::new(nd))),
        Region::with_defined(DmcChannel(RefCell::new(dd))),
        Region::with_defined(ApuController::new(cd)),
    ]
}

#[cfg(test)]
mod tests;
