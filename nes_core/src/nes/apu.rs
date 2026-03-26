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

#[derive(Copy, Clone)]
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

pub trait AudioDriver {
    fn sample_rate(&self) -> u32;

    fn push_sample(&mut self, sample: f32);

    fn flush(&mut self) {}
}

#[derive(Default)]
pub struct NullAudioDriver;

impl AudioDriver for NullAudioDriver {
    fn sample_rate(&self) -> u32 {
        44_100
    }

    fn push_sample(&mut self, _sample: f32) {}
}

const CPU_CLOCK_HZ: u64 = 1_789_773;
const PULSE_DUTY_TABLE: [[u8; 8]; 4] = [
    [0, 1, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 1, 1, 0, 0, 0],
    [1, 0, 0, 1, 1, 1, 1, 1],
];
const TRIANGLE_SEQUENCE: [u8; 32] = [
    15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
    13, 14, 15,
];
const NOISE_PERIOD_TABLE: [u16; 16] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
];

#[derive(Clone, Default)]
struct EnvelopeState {
    start: bool,
    divider: u8,
    decay: u8,
}

impl EnvelopeState {
    fn restart(&mut self) {
        self.start = true;
    }

    fn tick(&mut self, period: u8, looping: bool) {
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
}

#[derive(Clone)]
struct PulseState {
    control: DutyCycle,
    sweep: Sweep,
    timer_period: u16,
    timer_counter: u16,
    length_counter: u8,
    enabled: bool,
    sequence_step: usize,
    envelope: EnvelopeState,
    sweep_divider: u8,
    sweep_reload: bool,
    ones_complement_negate: bool,
}

impl PulseState {
    fn new(ones_complement_negate: bool) -> Self {
        Self {
            control: DutyCycle::default(),
            sweep: Sweep::default(),
            timer_period: 0,
            timer_counter: 0,
            length_counter: 0,
            enabled: false,
            sequence_step: 0,
            envelope: EnvelopeState::default(),
            sweep_divider: 0,
            sweep_reload: false,
            ones_complement_negate,
        }
    }

    fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.length_counter = 0;
        }
    }

    fn write_control(&mut self, value: DutyCycle) {
        self.control = value;
    }

    fn write_sweep(&mut self, value: Sweep) {
        self.sweep = value;
        self.sweep_reload = true;
    }

    fn write_timer_low(&mut self, value: u8) {
        self.timer_period = (self.timer_period & 0x0700) | value as u16;
    }

    fn write_timer_high(&mut self, load: LengthCounterLoad) {
        self.timer_period = load.timer();
        self.length_counter = FakeApuControllerDriver::LENGTH_TABLE[load.length_count() as usize];
        self.sequence_step = 0;
        self.envelope.restart();
    }

    fn step_timer(&mut self) {
        if self.timer_counter == 0 {
            self.timer_counter = self.timer_period;
            self.sequence_step = (self.sequence_step + 1) % 8;
        } else {
            self.timer_counter -= 1;
        }
    }

    fn step_envelope(&mut self) {
        self.envelope
            .tick(self.control.volume(), self.control.length_counter_halt());
    }

    fn step_length_counter(&mut self) {
        if !self.control.length_counter_halt() && self.length_counter > 0 {
            self.length_counter -= 1;
        }
    }

    fn step_sweep(&mut self) {
        let apply = self.sweep_divider == 0
            && self.sweep.enabled()
            && self.sweep.shift() > 0
            && !self.sweep_mutes_channel();
        if apply {
            self.timer_period = self.sweep_target_period();
        }

        if self.sweep_divider == 0 || self.sweep_reload {
            self.sweep_divider = self.sweep.period();
            self.sweep_reload = false;
        } else {
            self.sweep_divider -= 1;
        }
    }

    fn current_volume(&self) -> u8 {
        if self.control.constant_volume() {
            self.control.volume()
        } else {
            self.envelope.decay
        }
    }

    fn sweep_target_period(&self) -> u16 {
        let change = self.timer_period >> self.sweep.shift();
        if self.sweep.negate() {
            self.timer_period
                .saturating_sub(change + u16::from(self.ones_complement_negate))
        } else {
            self.timer_period.saturating_add(change)
        }
    }

    fn sweep_mutes_channel(&self) -> bool {
        self.timer_period < 8 || self.sweep_target_period() > 0x07FF
    }

    fn output(&self) -> u8 {
        if !self.enabled || self.length_counter == 0 || self.sweep_mutes_channel() {
            return 0;
        }

        PULSE_DUTY_TABLE[self.control.duty() as usize][self.sequence_step] * self.current_volume()
    }

    fn status_enabled(&self) -> bool {
        self.enabled && self.length_counter > 0
    }
}

#[derive(Clone, Default)]
struct TriangleState {
    control: LinearCounterControl,
    timer_period: u16,
    timer_counter: u16,
    length_counter: u8,
    linear_counter: u8,
    linear_counter_reload: bool,
    enabled: bool,
    sequence_step: usize,
}

impl TriangleState {
    fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.length_counter = 0;
        }
    }

    fn write_control(&mut self, value: LinearCounterControl) {
        self.control = value;
    }

    fn write_timer_low(&mut self, value: u8) {
        self.timer_period = (self.timer_period & 0x0700) | value as u16;
    }

    fn write_timer_high(&mut self, load: LengthCounterLoad) {
        self.timer_period = load.timer();
        self.length_counter = FakeApuControllerDriver::LENGTH_TABLE[load.length_count() as usize];
        self.linear_counter_reload = true;
    }

    fn step_timer(&mut self) {
        if self.timer_counter == 0 {
            self.timer_counter = self.timer_period;
            if self.length_counter > 0 && self.linear_counter > 0 && self.timer_period > 1 {
                self.sequence_step = (self.sequence_step + 1) % TRIANGLE_SEQUENCE.len();
            }
        } else {
            self.timer_counter -= 1;
        }
    }

    fn step_linear_counter(&mut self) {
        if self.linear_counter_reload {
            self.linear_counter = self.control.counter();
        } else if self.linear_counter > 0 {
            self.linear_counter -= 1;
        }

        if !self.control.reload_flag() {
            self.linear_counter_reload = false;
        }
    }

    fn step_length_counter(&mut self) {
        if !self.control.reload_flag() && self.length_counter > 0 {
            self.length_counter -= 1;
        }
    }

    fn output(&self) -> u8 {
        if !self.enabled || self.length_counter == 0 || self.linear_counter == 0 {
            0
        } else {
            TRIANGLE_SEQUENCE[self.sequence_step]
        }
    }

    fn status_enabled(&self) -> bool {
        self.enabled && self.length_counter > 0
    }
}

#[derive(Clone)]
struct NoiseState {
    envelope: NoiseEnvelop,
    period: NoisePeriod,
    timer_counter: u16,
    length_counter: u8,
    enabled: bool,
    shift_register: u16,
    envelope_state: EnvelopeState,
}

impl Default for NoiseState {
    fn default() -> Self {
        Self {
            envelope: NoiseEnvelop::default(),
            period: NoisePeriod::default(),
            timer_counter: 0,
            length_counter: 0,
            enabled: false,
            shift_register: 1,
            envelope_state: EnvelopeState::default(),
        }
    }
}

impl NoiseState {
    fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.length_counter = 0;
        }
    }

    fn write_envelope(&mut self, value: NoiseEnvelop) {
        self.envelope = value;
    }

    fn write_period(&mut self, value: NoisePeriod) {
        self.period = value;
    }

    fn write_length(&mut self, value: NoiseLength) {
        self.length_counter = FakeApuControllerDriver::LENGTH_TABLE[value.length() as usize];
        self.envelope_state.restart();
    }

    fn step_timer(&mut self) {
        if self.timer_counter == 0 {
            self.timer_counter = NOISE_PERIOD_TABLE[self.period.period() as usize];
            let tap = if self.period.enabled() { 6 } else { 1 };
            let feedback = (self.shift_register ^ (self.shift_register >> tap)) & 0x0001;
            self.shift_register = (self.shift_register >> 1) | (feedback << 14);
        } else {
            self.timer_counter -= 1;
        }
    }

    fn step_envelope(&mut self) {
        self.envelope_state
            .tick(self.envelope.volume(), self.envelope.loop_flag());
    }

    fn step_length_counter(&mut self) {
        if !self.envelope.loop_flag() && self.length_counter > 0 {
            self.length_counter -= 1;
        }
    }

    fn current_volume(&self) -> u8 {
        if self.envelope.constant_volume() {
            self.envelope.volume()
        } else {
            self.envelope_state.decay
        }
    }

    fn output(&self) -> u8 {
        if !self.enabled || self.length_counter == 0 || (self.shift_register & 0x0001) != 0 {
            0
        } else {
            self.current_volume()
        }
    }

    fn status_enabled(&self) -> bool {
        self.enabled && self.length_counter > 0
    }
}

#[derive(Clone, Default)]
struct DmcState {
    irq_loop_freq: DmcIRQLoopFreq,
    load_counter: u8,
    sample_address: u8,
    sample_length: u8,
    enabled: bool,
}

impl DmcState {
    fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    fn status_enabled(&self) -> bool {
        self.enabled
    }
}

pub struct Apu {
    pulse1: PulseState,
    pulse2: PulseState,
    triangle: TriangleState,
    noise: NoiseState,
    dmc: DmcState,
    driver: Box<dyn AudioDriver>,
    sample_rate: u32,
    sample_accumulator: u64,
    apu_cycle: u64,
    apu_even_cycle: bool,
    frame_counter_mode: bool,
    frame_interrupt_inhibit: bool,
    frame_interrupt: bool,
    dmc_interrupt: bool,
    dc_last_input: f32,
    dc_last_output: f32,
}

impl Default for Apu {
    fn default() -> Self {
        Self::new(Box::<NullAudioDriver>::default())
    }
}

impl Apu {
    pub fn new(driver: Box<dyn AudioDriver>) -> Self {
        let sample_rate = driver.sample_rate().max(1);
        Self {
            pulse1: PulseState::new(true),
            pulse2: PulseState::new(false),
            triangle: TriangleState::default(),
            noise: NoiseState::default(),
            dmc: DmcState::default(),
            driver,
            sample_rate,
            sample_accumulator: 0,
            apu_cycle: 0,
            apu_even_cycle: false,
            frame_counter_mode: false,
            frame_interrupt_inhibit: false,
            frame_interrupt: false,
            dmc_interrupt: false,
            dc_last_input: 0.0,
            dc_last_output: 0.0,
        }
    }

    pub fn tick(&mut self) -> bool {
        self.triangle.step_timer();

        self.apu_even_cycle = !self.apu_even_cycle;
        if self.apu_even_cycle {
            self.apu_cycle = self.apu_cycle.wrapping_add(1);
            self.pulse1.step_timer();
            self.pulse2.step_timer();
            self.noise.step_timer();
        }

        let irq_triggered = self.tick_frame_counter();
        self.emit_samples();
        irq_triggered
    }

    pub fn request_irq(&self) -> bool {
        self.frame_interrupt || self.dmc_interrupt
    }

    pub fn flush(&mut self) {
        self.driver.flush();
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match address {
            0x4015 => {
                let mut status = APUStatus::new();
                status.set_pulse1_enabled(self.pulse1.status_enabled());
                status.set_pulse2_enabled(self.pulse2.status_enabled());
                status.set_triangle_enabled(self.triangle.status_enabled());
                status.set_noise_enabled(self.noise.status_enabled());
                status.set_dmc_enabled(self.dmc.status_enabled());
                status.set_frame_interrupt(self.frame_interrupt);
                status.set_dmc_interrupt(self.dmc_interrupt);
                self.frame_interrupt = false;
                status.into()
            }
            _ => panic!("Can not read from Apu at address {}", address),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            0x4000 => self.pulse1.write_control(value.into()),
            0x4001 => self.pulse1.write_sweep(value.into()),
            0x4002 => self.pulse1.write_timer_low(value),
            0x4003 => self
                .pulse1
                .write_timer_high(LengthCounterLoad::from_registers(
                    self.pulse1.timer_period as u8,
                    value,
                )),
            0x4004 => self.pulse2.write_control(value.into()),
            0x4005 => self.pulse2.write_sweep(value.into()),
            0x4006 => self.pulse2.write_timer_low(value),
            0x4007 => self
                .pulse2
                .write_timer_high(LengthCounterLoad::from_registers(
                    self.pulse2.timer_period as u8,
                    value,
                )),
            0x4008 => self.triangle.write_control(value.into()),
            0x400A => self.triangle.write_timer_low(value),
            0x400B => self
                .triangle
                .write_timer_high(LengthCounterLoad::from_registers(
                    self.triangle.timer_period as u8,
                    value,
                )),
            0x400C => self.noise.write_envelope(value.into()),
            0x400E => self.noise.write_period(value.into()),
            0x400F => self.noise.write_length(value.into()),
            0x4010 => self.dmc.irq_loop_freq = value.into(),
            0x4011 => self.dmc.load_counter = value & 0x7F,
            0x4012 => self.dmc.sample_address = value,
            0x4013 => self.dmc.sample_length = value,
            0x4015 => self.set_control_flags(value.into()),
            0x4017 => self.set_frame_counter(value.into()),
            0x4009 | 0x400D => {}
            _ => panic!("Can not write to Apu at address {}", address),
        }
    }

    fn set_control_flags(&mut self, flags: ControlFlags) {
        self.pulse1.set_enabled(flags.pulse1_enabled());
        self.pulse2.set_enabled(flags.pulse2_enabled());
        self.triangle.set_enabled(flags.triangle_enabled());
        self.noise.set_enabled(flags.noise_enabled());
        self.dmc.set_enabled(flags.dmc_enabled());

        if !flags.dmc_enabled() {
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
            self.clock_quarter_frame();
            self.clock_half_frame();
        }
    }

    fn tick_frame_counter(&mut self) -> bool {
        if !self.apu_even_cycle {
            return false;
        }

        if self.frame_counter_mode {
            let cycle = self.apu_cycle % 18_641;
            match cycle {
                3_728 | 11_185 => self.clock_quarter_frame(),
                7_456 | 18_640 => {
                    self.clock_quarter_frame();
                    self.clock_half_frame();
                }
                _ => {}
            }
            return false;
        }

        let cycle = self.apu_cycle % 14_915;
        match cycle {
            3_728 | 11_185 => self.clock_quarter_frame(),
            7_456 => {
                self.clock_quarter_frame();
                self.clock_half_frame();
            }
            14_914 => {
                self.clock_quarter_frame();
                self.clock_half_frame();
                if !self.frame_interrupt_inhibit {
                    self.frame_interrupt = true;
                    return true;
                }
            }
            _ => {}
        }

        false
    }

    fn clock_quarter_frame(&mut self) {
        self.pulse1.step_envelope();
        self.pulse2.step_envelope();
        self.triangle.step_linear_counter();
        self.noise.step_envelope();
    }

    fn clock_half_frame(&mut self) {
        self.clock_length_counters();
        self.pulse1.step_sweep();
        self.pulse2.step_sweep();
    }

    fn clock_length_counters(&mut self) {
        self.pulse1.step_length_counter();
        self.pulse2.step_length_counter();
        self.triangle.step_length_counter();
        self.noise.step_length_counter();
    }

    fn emit_samples(&mut self) {
        self.sample_accumulator = self
            .sample_accumulator
            .wrapping_add(self.sample_rate as u64);
        while self.sample_accumulator >= CPU_CLOCK_HZ {
            self.sample_accumulator -= CPU_CLOCK_HZ;
            let sample = self.mix_sample();
            self.driver.push_sample(sample);
        }
    }

    fn mix_sample(&mut self) -> f32 {
        let pulse_1 = self.pulse1.output() as f32;
        let pulse_2 = self.pulse2.output() as f32;
        let triangle = self.triangle.output() as f32;
        let noise = self.noise.output() as f32;
        let dmc = 0.0;

        let pulse_mix = if pulse_1 + pulse_2 == 0.0 {
            0.0
        } else {
            95.88 / ((8128.0 / (pulse_1 + pulse_2)) + 100.0)
        };

        let tnd_input = triangle / 8227.0 + noise / 12241.0 + dmc / 22638.0;
        let tnd_mix = if tnd_input == 0.0 {
            0.0
        } else {
            159.79 / ((1.0 / tnd_input) + 100.0)
        };

        let input = pulse_mix + tnd_mix;
        let output = input - self.dc_last_input + (0.995 * self.dc_last_output);
        self.dc_last_input = input;
        self.dc_last_output = output;
        output.clamp(-1.0, 1.0)
    }
}

#[cfg(test)]
mod tests;
