use super::oam::{Oam, Sprite};
use super::read_pattern_pixel;
use crate::nes::mapper::Cartridge;
use crate::nes::ppu::registers::{PpuCtrl, PpuMask, PpuStatus};
use tinyvec::ArrayVec;

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
    oam_index: u8,
    byte_index: u8,
    visible_sprites: u8,
    mode: SpriteOverflowEvalMode,
    pending_sprite_bytes: [u8; 4],
}

pub struct SpriteManager {
    zero_hit_pending: bool,
    overflow_pending: bool,
    sprite_overflow_eval: SpriteOverflowEval,
    current_scanline_oam: ArrayVec<[Sprite; 8]>,
    next_scanline_oam: ArrayVec<[Sprite; 8]>,
}

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
            current_scanline_oam: ArrayVec::new(),
            next_scanline_oam: ArrayVec::new(),
        }
    }

    pub fn reset(&mut self) {
        self.zero_hit_pending = false;
        self.overflow_pending = false;
        self.sprite_overflow_eval = SpriteOverflowEval::default();
        self.current_scanline_oam.clear();
        self.next_scanline_oam.clear();
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

    pub fn swap_secondary_oam(&mut self) {
        std::mem::swap(&mut self.current_scanline_oam, &mut self.next_scanline_oam);
        self.next_scanline_oam.clear();
    }

    pub fn begin_sprite_overflow_eval(&mut self) {
        self.sprite_overflow_eval = SpriteOverflowEval {
            oam_index: 0,
            byte_index: 0,
            visible_sprites: 0,
            mode: SpriteOverflowEvalMode::ScanY,
            pending_sprite_bytes: [0u8; 4],
        };
    }

    pub fn step_sprite_overflow_eval(&mut self, scanline: u16, ctrl: PpuCtrl, oam: &Oam) {
        fn target_scanline(scanline: u16) -> u16 {
            (scanline + 1) % 262
        }

        match self.sprite_overflow_eval.mode {
            SpriteOverflowEvalMode::Idle | SpriteOverflowEvalMode::Done => {}
            SpriteOverflowEvalMode::ScanY => {
                if self.sprite_overflow_eval.oam_index >= 64 {
                    self.sprite_overflow_eval.mode = SpriteOverflowEvalMode::Done;
                    return;
                }

                let y = oam.sprites[self.sprite_overflow_eval.oam_index as usize].y;
                if sprite_in_range(y, target_scanline(scanline), ctrl.sprite_height()) {
                    self.sprite_overflow_eval.visible_sprites += 1;
                    if self.sprite_overflow_eval.visible_sprites > 8 {
                        self.overflow_pending = true;
                        self.sprite_overflow_eval.mode = SpriteOverflowEvalMode::Done;
                    } else {
                        self.sprite_overflow_eval.pending_sprite_bytes[0] = y;
                        self.sprite_overflow_eval.mode =
                            SpriteOverflowEvalMode::CopySprite { remaining_bytes: 3 };
                    }
                } else {
                    self.sprite_overflow_eval.oam_index += 1;
                }
            }
            SpriteOverflowEvalMode::CopySprite { remaining_bytes } => {
                let oam_index = self.sprite_overflow_eval.oam_index;
                let byte_offset = 4 - remaining_bytes;
                let oam_byte = oam.get_byte((oam_index * 4 + byte_offset) as u8);
                self.sprite_overflow_eval.pending_sprite_bytes[(byte_offset as usize) & 0x3] =
                    oam_byte;

                if remaining_bytes > 1 {
                    self.sprite_overflow_eval.mode = SpriteOverflowEvalMode::CopySprite {
                        remaining_bytes: remaining_bytes - 1,
                    };
                } else {
                    let sprite = bytemuck::cast::<[u8; 4], Sprite>(
                        self.sprite_overflow_eval.pending_sprite_bytes,
                    );
                    self.next_scanline_oam.push(sprite);
                    self.sprite_overflow_eval.oam_index += 1;
                    self.sprite_overflow_eval.byte_index = 0;
                    self.sprite_overflow_eval.mode =
                        if self.sprite_overflow_eval.visible_sprites >= 8 {
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

                let byte_idx =
                    self.sprite_overflow_eval.oam_index * 4 + self.sprite_overflow_eval.byte_index;
                let y_byte = oam.get_byte(byte_idx as u8);
                if sprite_in_range(y_byte, target_scanline(scanline), ctrl.sprite_height()) {
                    self.overflow_pending = true;
                    self.sprite_overflow_eval.mode = SpriteOverflowEvalMode::Done;
                } else {
                    self.sprite_overflow_eval.oam_index += 1;
                    self.sprite_overflow_eval.byte_index =
                        (self.sprite_overflow_eval.byte_index + 1) & 0x03;
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

fn evaluate_sprite_from_secondary(
    sprite: &Sprite,
    ctrl: PpuCtrl,
    cartridge: &dyn Cartridge,
    screen_x: u8,
    screen_y: u8,
) -> Option<SpritePixel> {
    let rel_x = screen_x as i16 - sprite.x as i16;
    if !(0..8).contains(&rel_x) {
        return None;
    }

    let sprite_height = ctrl.sprite_height();
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

fn evaluate_sprite(
    sprite: Sprite,
    ctrl: PpuCtrl,
    cartridge: &dyn Cartridge,
    screen_x: u8,
    screen_y: u8,
) -> Option<SpritePixel> {
    if !sprite_in_range(sprite.y, screen_y as u16, ctrl.sprite_height()) {
        return None;
    }
    evaluate_sprite_from_secondary(&sprite, ctrl, cartridge, screen_x, screen_y)
}

impl SpriteManager {
    pub fn find_sprite_pixel(
        &self,
        ctrl: PpuCtrl,
        mask: PpuMask,
        cartridge: &dyn Cartridge,
        screen_x: u8,
        screen_y: u8,
    ) -> Option<SpritePixel> {
        if !mask.sprite_left_enabled() && screen_x < 8 {
            return None;
        }

        for sprite in &self.current_scanline_oam {
            if let Some(pixel) =
                evaluate_sprite_from_secondary(sprite, ctrl, cartridge, screen_x, screen_y)
            {
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
}
