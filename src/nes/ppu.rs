use crate::to_from_u8;
use modular_bitfield::prelude::*;

#[derive(Copy, Clone)]
#[bitfield]
pub struct PpuCtrl {
    pub nmi_enable: bool,
    pub ppu_master: bool,
    pub sprite_size: bool,
    pub background_pattern_table: bool,
    pub sprite_pattern_table: bool,
    pub increment_mode: bool,
    pub name_table_select: B2,
}
to_from_u8!(PpuCtrl);

#[derive(Copy, Clone)]
#[bitfield]
pub struct PpuMask {
    pub blue_tint: bool,
    pub green_tint: bool,
    pub red_tint: bool,
    pub sprite_enable: bool,
    pub background_enable: bool,
    pub grayscale: bool,
    pub sprite_left: bool,
    pub background_left: bool,
}
to_from_u8!(PpuMask);

#[derive(Copy, Clone)]
#[bitfield]
pub struct PpuStatus {
    pub v_blank: bool,
    pub sprite_zero_hit: bool,
    pub sprite_overflow: bool,
    #[allow(non_snake_case)]
    #[skip]
    __: B5,
}
to_from_u8!(PpuStatus);
