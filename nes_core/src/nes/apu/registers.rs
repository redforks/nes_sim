use modular_bitfield::prelude::*;

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct SweepBits {
    pub shift: B3,
    pub negate: bool,
    pub period: B3,
    pub enabled: bool,
}

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct PulseControlBits {
    pub volume: B4,
    pub constant_volume: bool,
    pub loop_and_is_halt: bool,
    pub duty: B2,
}

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct LengthTimerHigh3Bits {
    pub high3: B3,
    pub length_idx: B5,
}

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct TriangleControlBits {
    pub counter: B7,
    pub loop_and_is_halt: bool,
}

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct NoiseControlBits {
    pub volume: B4,
    pub constant_volume: bool,
    pub loop_and_is_halt: bool,
    #[skip]
    __: B2,
}

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct NoisePeriod {
    pub period: B4,
    #[skip]
    __: B3,
    pub is_halt: bool,
}

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct NoiseLength {
    #[skip]
    __: B3,
    pub length_idx: B5,
}

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct DmcIRQLoopFreq {
    pub freq: B4,
    #[skip]
    __: B2,
    pub loop_flag: bool,
    pub irq_enabled: bool,
}

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ControlFlags {
    pub pulse1_enabled: bool,
    pub pulse2_enabled: bool,
    pub triangle_enabled: bool,
    pub noise_enabled: bool,
    pub dmc_enabled: bool,
    #[skip]
    __: B3,
}

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct APUStatus {
    pub pulse1_enabled: bool,
    pub pulse2_enabled: bool,
    pub triangle_enabled: bool,
    pub noise_enabled: bool,
    pub dmc_enabled: bool,
    #[skip]
    __: B1,
    pub frame_interrupt: bool,
    pub dmc_interrupt: bool,
}

#[derive(Specifier, Debug, Clone, Copy, Default, PartialEq, Eq)]
#[bits = 1]
pub enum FrameSequencerMode {
    #[default]
    FourStep,
    FiveStep,
}

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct FrameSequencerBits {
    #[skip]
    __: B6,
    pub disable_interrupt: bool,
    #[bits = 1]
    pub mode: FrameSequencerMode,
}

#[bitfield]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct DmcDacBits {
    pub dac_value: B7,
    #[skip]
    __: B1,
}
