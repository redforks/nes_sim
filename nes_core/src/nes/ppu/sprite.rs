use crate::nes::{mapper::Cartridge, ppu::registers::Registers};

use super::PatternAccess;

const MAX_SPRITES_PER_SCANLINE: usize = 8;

#[derive(Copy, Clone)]
pub(crate) struct SpritePixel {
    pub(crate) palette_idx: u8,
    pub(crate) color_idx: u8,
    pub(crate) behind_bg: bool,
}

#[derive(Copy, Clone, Default)]
enum SpriteOverflowEvalMode {
    #[default]
    Idle,
    ScanY,
    CopySprite {
        remaining_bytes: u8,
    },
    OverflowSearchDelay,
    OverflowSearch,
    Done,
}

#[derive(Copy, Clone, Default)]
struct SpriteOverflowEval {
    scanline: u16,
    target_scanline: u16,
    oam_index: usize,
    byte_index: usize,
    visible_sprites: usize,
    mode: SpriteOverflowEvalMode,
}

pub(crate) struct SpriteManager {
    zero_hit_pending: bool,
    overflow_pending: bool,
    sprite_overflow_eval: SpriteOverflowEval,
}

fn sprite_in_range(y_byte: u8, target_scanline: u16, sprite_height: i16) -> bool {
    let top = y_byte as i16 + 1;
    let sprite_y = target_scanline as i16 - top;
    (0..sprite_height).contains(&sprite_y)
}

fn read_sprite_color(
    cartridge: &Cartridge,
    tile_idx: u8,
    src_x: usize,
    src_y: u8,
    sprite_size_16: bool,
    sprite_pattern_table: bool,
    read_pattern_pixel: &impl Fn(&Cartridge, u16, u8, usize, usize, PatternAccess) -> u8,
) -> u8 {
    if sprite_size_16 {
        let pattern_table_idx = (tile_idx & 0x01) as u16;
        let tile_base = tile_idx & 0xFE;
        let tile_offset = src_y / 8;
        let tile_row = (src_y % 8) as usize;
        read_pattern_pixel(
            cartridge,
            pattern_table_idx * 0x1000,
            tile_base.wrapping_add(tile_offset),
            src_x,
            tile_row,
            PatternAccess::Sprite,
        )
    } else {
        let pattern_table_idx = if sprite_pattern_table { 0x1000 } else { 0 };
        read_pattern_pixel(
            cartridge,
            pattern_table_idx,
            tile_idx,
            src_x,
            src_y as usize,
            PatternAccess::Sprite,
        )
    }
}

impl SpriteManager {
    pub fn new() -> Self {
        Self {
            zero_hit_pending: false,
            overflow_pending: false,
            sprite_overflow_eval: SpriteOverflowEval::default(),
        }
    }

    pub fn reset(&mut self) {
        self.zero_hit_pending = false;
        self.overflow_pending = false;
        self.sprite_overflow_eval = SpriteOverflowEval::default();
    }

    /// Update sprite status to ppu status register
    pub fn update_ctrl_status(&mut self, registers: &mut Registers) {
        if std::mem::take(&mut self.zero_hit_pending) {
            registers.status.set_sprite_zero_hit(true);
        }

        if std::mem::take(&mut self.overflow_pending) {
            registers.status.set_sprite_overflow(true);
        }
    }

    pub fn set_zero_hit_pending(&mut self) {
        self.zero_hit_pending = true;
    }

    pub fn clear_pending(&mut self) {
        self.zero_hit_pending = false;
        self.overflow_pending = false;
    }

    pub fn begin_sprite_overflow_eval(&mut self, scanline: u16) {
        self.sprite_overflow_eval = SpriteOverflowEval {
            scanline,
            target_scanline: scanline + 1,
            oam_index: 0,
            byte_index: 0,
            visible_sprites: 0,
            mode: SpriteOverflowEvalMode::ScanY,
        };
    }

    pub fn step_sprite_overflow_eval(
        &mut self,
        scanline: u16,
        sprite_size_16: bool,
        oam_data: &[u8; 256],
    ) {
        if self.sprite_overflow_eval.scanline != scanline {
            return;
        }

        let sprite_height: i16 = if sprite_size_16 { 16 } else { 8 };

        match self.sprite_overflow_eval.mode {
            SpriteOverflowEvalMode::Idle | SpriteOverflowEvalMode::Done => {}
            SpriteOverflowEvalMode::ScanY => {
                if self.sprite_overflow_eval.oam_index >= 64 {
                    self.sprite_overflow_eval.mode = SpriteOverflowEvalMode::Done;
                    return;
                }

                let oam_index = self.sprite_overflow_eval.oam_index;
                let target_scanline = self.sprite_overflow_eval.target_scanline;
                let y_byte = oam_data[oam_index * 4];
                if sprite_in_range(y_byte, target_scanline, sprite_height) {
                    self.sprite_overflow_eval.visible_sprites += 1;
                    if self.sprite_overflow_eval.visible_sprites > MAX_SPRITES_PER_SCANLINE {
                        self.overflow_pending = true;
                        self.sprite_overflow_eval.mode = SpriteOverflowEvalMode::Done;
                    } else {
                        self.sprite_overflow_eval.mode =
                            SpriteOverflowEvalMode::CopySprite { remaining_bytes: 3 };
                    }
                } else {
                    self.sprite_overflow_eval.oam_index += 1;
                }
            }
            SpriteOverflowEvalMode::CopySprite { remaining_bytes } => {
                if remaining_bytes > 1 {
                    self.sprite_overflow_eval.mode = SpriteOverflowEvalMode::CopySprite {
                        remaining_bytes: remaining_bytes - 1,
                    };
                } else {
                    self.sprite_overflow_eval.oam_index += 1;
                    self.sprite_overflow_eval.byte_index = 0;
                    self.sprite_overflow_eval.mode =
                        if self.sprite_overflow_eval.visible_sprites >= MAX_SPRITES_PER_SCANLINE {
                            SpriteOverflowEvalMode::OverflowSearchDelay
                        } else {
                            SpriteOverflowEvalMode::ScanY
                        };
                }
            }
            SpriteOverflowEvalMode::OverflowSearchDelay => {
                self.sprite_overflow_eval.mode = SpriteOverflowEvalMode::OverflowSearch;
            }
            SpriteOverflowEvalMode::OverflowSearch => {
                if self.sprite_overflow_eval.oam_index >= 64 {
                    self.sprite_overflow_eval.mode = SpriteOverflowEvalMode::Done;
                    return;
                }

                let oam_index = self.sprite_overflow_eval.oam_index;
                let byte_index = self.sprite_overflow_eval.byte_index;
                let target_scanline = self.sprite_overflow_eval.target_scanline;
                let byte_idx = oam_index * 4 + byte_index;
                let y_byte = oam_data[byte_idx];
                if sprite_in_range(y_byte, target_scanline, sprite_height) {
                    self.overflow_pending = true;
                    self.sprite_overflow_eval.mode = SpriteOverflowEvalMode::Done;
                } else {
                    self.sprite_overflow_eval.oam_index += 1;
                    self.sprite_overflow_eval.byte_index = (byte_index + 1) & 0x03;
                }
            }
        }
    }

    pub fn find_sprite_pixel(
        &self,
        oam_data: &[u8; 256],
        sprite_size_16: bool,
        sprite_pattern_table: bool,
        sprite_left_enabled: bool,
        cartridge: &Cartridge,
        screen_x: u8,
        screen_y: u8,
        read_pattern_pixel: impl Fn(&Cartridge, u16, u8, usize, usize, PatternAccess) -> u8,
    ) -> Option<SpritePixel> {
        if !sprite_left_enabled && screen_x < 8 {
            return None;
        }

        let sprite_height: i16 = if sprite_size_16 { 16 } else { 8 };
        let mut visible_count = 0usize;

        for sprite_idx in 0..64 {
            let byte_idx = sprite_idx * 4;
            let y_byte = oam_data[byte_idx];
            if !sprite_in_range(y_byte, screen_y as u16, sprite_height) {
                continue;
            }

            visible_count += 1;
            if visible_count > MAX_SPRITES_PER_SCANLINE {
                break;
            }

            let x = oam_data[byte_idx + 3];
            let rel_x = screen_x as i16 - x as i16;
            if !(0..8).contains(&rel_x) {
                continue;
            }

            let tile_idx = oam_data[byte_idx + 1];
            let attributes = oam_data[byte_idx + 2];
            let flip_vertical = (attributes & 0x80) != 0;
            let flip_horizontal = (attributes & 0x40) != 0;
            let palette_idx = attributes & 0x03;
            let behind_bg = (attributes & 0x20) != 0;

            let top = y_byte as i16 + 1;
            let sprite_y = screen_y as i16 - top;
            let src_x = if flip_horizontal {
                (7 - rel_x) as usize
            } else {
                rel_x as usize
            };
            let src_y = if flip_vertical {
                (sprite_height - 1 - sprite_y) as u8
            } else {
                sprite_y as u8
            };

            let color_idx = read_sprite_color(
                cartridge,
                tile_idx,
                src_x,
                src_y,
                sprite_size_16,
                sprite_pattern_table,
                &read_pattern_pixel,
            );

            if color_idx != 0 {
                return Some(SpritePixel {
                    palette_idx,
                    color_idx,
                    behind_bg,
                });
            }
        }

        None
    }

    pub fn sprite_zero_opaque_at(
        &self,
        oam_data: &[u8; 256],
        sprite_size_16: bool,
        sprite_pattern_table: bool,
        cartridge: &Cartridge,
        screen_x: u8,
        screen_y: u8,
        read_pattern_pixel: impl Fn(&Cartridge, u16, u8, usize, usize, PatternAccess) -> u8,
    ) -> bool {
        let sprite_height: i16 = if sprite_size_16 { 16 } else { 8 };
        let y_byte = oam_data[0];
        if !sprite_in_range(y_byte, screen_y as u16, sprite_height) {
            return false;
        }

        let tile_idx = oam_data[1];
        let attributes = oam_data[2];
        let x = oam_data[3];
        let flip_vertical = (attributes & 0x80) != 0;
        let flip_horizontal = (attributes & 0x40) != 0;

        let rel_x = screen_x as i16 - x as i16;
        if !(0..8).contains(&rel_x) {
            return false;
        }

        let top = y_byte as i16 + 1;
        let sprite_y = screen_y as i16 - top;
        let src_x = if flip_horizontal {
            (7 - rel_x) as usize
        } else {
            rel_x as usize
        };
        let src_y = if flip_vertical {
            (sprite_height - 1 - sprite_y) as u8
        } else {
            sprite_y as u8
        };

        let color_idx = read_sprite_color(
            cartridge,
            tile_idx,
            src_x,
            src_y,
            sprite_size_16,
            sprite_pattern_table,
            &read_pattern_pixel,
        );

        color_idx != 0
    }
}
