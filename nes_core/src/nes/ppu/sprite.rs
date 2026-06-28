use super::oam::{Oam, Sprite};
use super::read_pattern_pixel;
use crate::nes::mapper::Cartridge;
use crate::nes::ppu::registers::{PpuCtrl, PpuMask, PpuStatus};

const MAX_SPRITES_PER_SCANLINE: u8 = 8;

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
    visible_sprites: u8,
    mode: SpriteOverflowEvalMode,
}

pub struct SpriteManager {
    zero_hit_pending: bool,
    overflow_pending: bool,
    sprite_overflow_eval: SpriteOverflowEval,
}

#[inline]
fn sprite_in_range(y: u8, target_scanline: u16, sprite_height: u8) -> bool {
    let top = y as i16 + 1;
    let sprite_y = target_scanline as i16 - top;
    sprite_y >= 0 && sprite_y < (sprite_height as i16)
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
    pub fn update_ctrl_status(&mut self, status: &mut PpuStatus) {
        if std::mem::take(&mut self.zero_hit_pending) {
            status.set_sprite_zero_hit(true);
        }

        if std::mem::take(&mut self.overflow_pending) {
            status.set_sprite_overflow(true);
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

    pub fn step_sprite_overflow_eval(&mut self, scanline: u16, ctrl: PpuCtrl, oam: &Oam) {
        if self.sprite_overflow_eval.scanline != scanline {
            return;
        }

        match self.sprite_overflow_eval.mode {
            SpriteOverflowEvalMode::Idle | SpriteOverflowEvalMode::Done => {}
            SpriteOverflowEvalMode::ScanY => {
                if self.sprite_overflow_eval.oam_index >= 64 {
                    self.sprite_overflow_eval.mode = SpriteOverflowEvalMode::Done;
                    return;
                }

                let oam_index = self.sprite_overflow_eval.oam_index;
                let target_scanline = self.sprite_overflow_eval.target_scanline;
                if sprite_in_range(
                    oam.sprites[oam_index].y,
                    target_scanline,
                    ctrl.sprite_height(),
                ) {
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
                let y_byte = oam.as_bytes()[byte_idx];
                if sprite_in_range(y_byte, target_scanline, ctrl.sprite_height()) {
                    self.overflow_pending = true;
                    self.sprite_overflow_eval.mode = SpriteOverflowEvalMode::Done;
                } else {
                    self.sprite_overflow_eval.oam_index += 1;
                    self.sprite_overflow_eval.byte_index = (byte_index + 1) & 0x03;
                }
            }
        }
    }
}

#[derive(Copy, Clone)]
pub struct SpritePixel {
    pub palette_idx: u8,
    pub color_idx: u8,
    pub behind_bg: bool,
}

fn evaluate_sprite(
    sprite: Sprite,
    ctrl: PpuCtrl,
    cartridge: &dyn Cartridge,
    screen_x: u8,
    screen_y: u8,
) -> Option<SpritePixel> {
    let sprite_height = ctrl.sprite_height();
    if !sprite_in_range(sprite.y, screen_y as u16, sprite_height) {
        return None;
    }

    let rel_x = screen_x as i16 - sprite.x as i16;
    if !(0..8).contains(&rel_x) {
        return None;
    }

    let top = sprite.y as i16 + 1;
    let sprite_y = screen_y as i16 - top;
    let src_x = if sprite.attributes.flip_horizontally() {
        (7 - rel_x) as u8
    } else {
        rel_x as u8
    };
    let src_y = if sprite.attributes.flip_vertically() {
        (sprite_height as i16 - 1 - sprite_y) as u8
    } else {
        sprite_y as u8
    };

    let tile_position = sprite.tile_position(ctrl);
    let color_idx = read_pattern_pixel(cartridge, tile_position, src_x, src_y);

    if color_idx == 0 {
        return None;
    }

    Some(SpritePixel {
        palette_idx: sprite.attributes.palette(),
        color_idx,
        behind_bg: sprite.attributes.behind_background(),
    })
}

pub fn find_sprite_pixel(
    oam: &Oam,
    ctrl: PpuCtrl,
    mask: PpuMask,
    cartridge: &dyn Cartridge,
    screen_x: u8,
    screen_y: u8,
) -> Option<SpritePixel> {
    if !mask.sprite_left_enabled() && screen_x < 8 {
        return None;
    }

    let mut visible_count = 0u8;

    for sprite in &oam.sprites {
        if !sprite_in_range(sprite.y, screen_y as u16, ctrl.sprite_height()) {
            continue;
        }

        visible_count += 1;
        if visible_count > MAX_SPRITES_PER_SCANLINE {
            break;
        }

        if let Some(pixel) = evaluate_sprite(*sprite, ctrl, cartridge, screen_x, screen_y) {
            return Some(pixel);
        }
    }

    None
}

pub fn sprite_zero_opaque_at(
    oam: &Oam,
    ctrl: PpuCtrl,
    cartridge: &dyn Cartridge,
    screen_x: u8,
    screen_y: u8,
) -> bool {
    evaluate_sprite(oam.sprites[0], ctrl, cartridge, screen_x, screen_y).is_some()
}
