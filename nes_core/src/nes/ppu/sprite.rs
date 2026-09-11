use super::oam::{Oam, Sprite};
use super::read_pattern_pixel;
use crate::nes::mapper::Cartridge;
use crate::nes::ppu::registers::{PpuCtrl, PpuStatus};
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
    /// Count of sprites scanned so far this line (0..=64); the effective
    /// OAM index is (start_index + oam_index) % 64 — hardware evaluation
    /// starts at OAMADDR and wraps.
    oam_index: u8,
    byte_index: u8,
    visible_sprites: u8,
    start_index: u8,
    mode: SpriteOverflowEvalMode,
    pending_sprite_bytes: [u8; 4],
}

impl SpriteOverflowEval {
    /// OAM index this evaluation step reads: hardware evaluation starts at
    /// OAMADDR and wraps at 64, so the effective index is
    /// (start_index + oam_index) mod 64 while `oam_index` counts 0..=64.
    fn effective_index(&self) -> u8 {
        self.start_index.wrapping_add(self.oam_index) & 0x3f
    }
}

pub struct SpriteManager {
    zero_hit_pending: bool,
    overflow_pending: bool,
    sprite_overflow_eval: SpriteOverflowEval,
    current_scanline_oam: ArrayVec<[Sprite; 8]>,
    next_scanline_oam: ArrayVec<[Sprite; 8]>,
    /// OAM entry 0 as evaluated onto the current/next scanline. The
    /// sprite-0 hit applies only to a sprite 0 that evaluation actually
    /// picked (≤8 in-range sprites, evaluation starting at OAMADDR).
    current_zero_sprite: Option<Sprite>,
    next_zero_sprite: Option<Sprite>,
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
            current_zero_sprite: None,
            next_zero_sprite: None,
        }
    }

    pub fn reset(&mut self) {
        self.zero_hit_pending = false;
        self.overflow_pending = false;
        self.sprite_overflow_eval = SpriteOverflowEval::default();
        self.current_scanline_oam.clear();
        self.next_scanline_oam.clear();
        self.current_zero_sprite = None;
        self.next_zero_sprite = None;
    }

    /// Update sprite status to ppu status register
    pub fn update_ctrl_status(&mut self, status: &mut PpuStatus) {
        let mut v = PpuStatus::new();
        if self.zero_hit_pending {
            self.zero_hit_pending = false;
            v.set_sprite_zero_hit(true);
        }

        if self.overflow_pending {
            self.overflow_pending = false;
            v.set_sprite_overflow(true);
        }

        *status = PpuStatus::from_bits(status.into_bits() | v.into_bits());
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
        std::mem::swap(&mut self.current_zero_sprite, &mut self.next_zero_sprite);
        self.next_zero_sprite = None;
    }

    pub fn begin_sprite_overflow_eval(&mut self, start_index: u8) {
        self.sprite_overflow_eval = SpriteOverflowEval {
            oam_index: 0,
            byte_index: 0,
            visible_sprites: 0,
            start_index,
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

                let y = oam.sprites[self.sprite_overflow_eval.effective_index() as usize].y;
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
                let oam_index = self.sprite_overflow_eval.effective_index();
                let byte_offset = 4 - remaining_bytes;
                let oam_byte = oam.get_byte(oam_index * 4 + byte_offset);
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
                    if self.sprite_overflow_eval.effective_index() == 0 {
                        self.next_zero_sprite = Some(sprite);
                    }
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

                let byte_idx = self.sprite_overflow_eval.effective_index() * 4
                    + self.sprite_overflow_eval.byte_index;
                let y_byte = oam.get_byte(byte_idx);
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

    /// Read-model for tests: count of evaluated sprites on the live line.
    #[cfg(test)]
    pub(crate) fn secondary_oam_len(&self) -> usize {
        self.current_scanline_oam.len()
    }

    /// Read-model for tests: count of evaluated sprites for the next line.
    #[cfg(test)]
    pub(crate) fn next_scanline_oam_len(&self) -> usize {
        self.next_scanline_oam.len()
    }

    /// Read-model for tests: whether OAM entry 0 survived evaluation.
    #[cfg(test)]
    pub(crate) fn current_zero_sprite(&self) -> Option<Sprite> {
        self.current_zero_sprite
    }

    pub fn find_sprite_pixel(
        &self,
        ctrl: PpuCtrl,
        cartridge: &dyn Cartridge,
        screen_x: u8,
        screen_y: u8,
    ) -> Option<SpritePixel> {
        for sprite in &self.current_scanline_oam {
            if let Some(pixel) =
                evaluate_sprite_from_secondary(sprite, ctrl, cartridge, screen_x, screen_y)
            {
                return Some(pixel);
            }
        }

        None
    }

    /// Sprite-0 hit source pixel: opaque only when OAM entry 0 survived
    /// this scanline's evaluation and its pixel overlaps the beam.
    pub fn sprite_zero_pixel_opaque(
        &self,
        ctrl: PpuCtrl,
        cartridge: &dyn Cartridge,
        screen_x: u8,
        screen_y: u8,
    ) -> bool {
        match self.current_zero_sprite {
            Some(zero) => {
                evaluate_sprite_from_secondary(&zero, ctrl, cartridge, screen_x, screen_y)
                    .is_some_and(|p| p.color_idx != 0)
            }
            None => false,
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
