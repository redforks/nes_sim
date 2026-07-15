use bitfield_struct::{bitenum, bitfield};

#[bitfield(u8)]
pub struct SweepBits {
    #[bits(3)]
    pub shift: u8,
    pub negate: bool,
    #[bits(3)]
    pub period: u8,
    pub enabled: bool,
}

#[bitfield(u8)]
pub struct PulseControlBits {
    #[bits(4)]
    pub volume: u8,
    pub constant_volume: bool,
    pub loop_and_is_halt: bool,
    #[bits(2)]
    pub duty: u8,
}

#[bitfield(u8)]
pub struct LengthTimerHigh3Bits {
    #[bits(3)]
    pub high3: u8,
    #[bits(5)]
    pub length_idx: u8,
}

#[bitfield(u8)]
pub struct TriangleControlBits {
    #[bits(7)]
    pub counter: u8,
    pub loop_and_is_halt: bool,
}

#[bitfield(u8)]
pub struct NoiseControlBits {
    #[bits(4)]
    pub volume: u8,
    pub constant_volume: bool,
    pub loop_and_is_halt: bool,
    #[bits(2)]
    __: u8,
}

#[bitfield(u8)]
pub struct NoisePeriod {
    #[bits(4)]
    pub period: u8,
    #[bits(3)]
    __: u8,
    pub is_halt: bool,
}

#[bitfield(u8)]
pub struct NoiseLength {
    #[bits(3)]
    __: u8,
    #[bits(5)]
    pub length_idx: u8,
}

#[bitfield(u8)]
pub struct DmcIRQLoopFreq {
    #[bits(4)]
    pub freq: u8,
    #[bits(2)]
    __: u8,
    pub loop_flag: bool,
    pub irq_enabled: bool,
}

#[bitfield(u8)]
pub struct ControlFlags {
    pub pulse1_enabled: bool,
    pub pulse2_enabled: bool,
    pub triangle_enabled: bool,
    pub noise_enabled: bool,
    pub dmc_enabled: bool,
    #[bits(3)]
    __: u8,
}

#[bitfield(u8)]
pub struct APUStatus {
    pub pulse1_enabled: bool,
    pub pulse2_enabled: bool,
    pub triangle_enabled: bool,
    pub noise_enabled: bool,
    pub dmc_enabled: bool,
    #[bits(1)]
    __: u8,
    pub frame_interrupt: bool,
    pub dmc_interrupt: bool,
}

#[repr(u8)]
#[bitenum]
#[derive(Debug, Default, PartialEq, Eq)]
pub enum FrameSequencerMode {
    #[fallback]
    #[default]
    FourStep,
    FiveStep,
}

#[bitfield(u8)]
pub struct FrameSequencerBits {
    #[bits(6)]
    __: u8,
    pub disable_interrupt: bool,
    #[bits(1)]
    pub mode: FrameSequencerMode,
}

#[bitfield(u8)]
pub struct DmcDacBits {
    #[bits(7)]
    pub dac_value: u8,
    #[bits(1)]
    __: u8,
}
