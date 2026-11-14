use super::oam::{Oam, Sprite};
use crate::nes::mapper::{Cartridge, PpuFetchKind, PpuRenderState};
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
    overflow_pending: bool,
    sprite_overflow_eval: SpriteOverflowEval,
    current_scanline_oam: ArrayVec<[Sprite; 8]>,
    next_scanline_oam: ArrayVec<[Sprite; 8]>,
    /// OAM entry 0 as evaluated onto the current/next scanline. The
    /// sprite-0 hit applies only to a sprite 0 that evaluation actually
    /// picked (≤8 in-range sprites, evaluation starting at OAMADDR).
    current_zero_sprite: Option<Sprite>,
    next_zero_sprite: Option<Sprite>,
    /// Pattern row cache, index-parallel to current_scanline_oam: one entry
    /// per evaluated Sprite for the scanline being rendered. Filled lazily on
    /// the first pixel request of a scanline (one resolve_pixel_addr plus two
    /// cartridge VRAM reads per Sprite), then decoded per pixel.
    rows: ArrayVec<[SpriteRow; 8]>,
    /// Screen Y the rows were resolved for; None means the cache is empty or
    /// invalidated.
    rows_scanline: Option<u8>,
    /// Index of the evaluated OAM entry 0 within rows, when it survived
    /// evaluation; the sprite-0 hit reads that cached row.
    zero_row_idx: Option<usize>,
    /// Internal OAM bus latch exposed by $2004 reads mid-render (Mesen
    /// `_oamCopybuffer`): $FF during the 1-64 clear phase, the last primary
    /// OAM byte read on odd eval dots during 65-256. Written only by the
    /// eval machine; the register layer observes it through `copy_buffer()`
    /// and never mutates it.
    oam_copybuffer: u8,
}

fn sprite_in_range(y: u8, target_scanline: u16, sprite_height: u8) -> bool {
    let top = y as i16 + 1;
    let sprite_y = target_scanline as i16 - top;
    sprite_y >= 0 && sprite_y < (sprite_height as i16)
}

impl SpriteManager {
    pub fn new() -> Self {
        Self {
            overflow_pending: false,
            sprite_overflow_eval: SpriteOverflowEval::default(),
            current_scanline_oam: ArrayVec::new(),
            next_scanline_oam: ArrayVec::new(),
            current_zero_sprite: None,
            next_zero_sprite: None,
            rows: ArrayVec::new(),
            rows_scanline: None,
            zero_row_idx: None,
            oam_copybuffer: 0xFF,
        }
    }

    pub fn reset(&mut self) {
        self.overflow_pending = false;
        self.sprite_overflow_eval = SpriteOverflowEval::default();
        self.current_scanline_oam.clear();
        self.next_scanline_oam.clear();
        self.current_zero_sprite = None;
        self.next_zero_sprite = None;
        self.invalidate_rows();
        self.oam_copybuffer = 0xFF;
    }

    /// Drop the scanline cached pattern rows: the next pixel request refills
    /// them from the cartridge. Called at dot 0 (secondary-OAM swap), on
    /// $2000 control writes, on PPUDATA writes below the pattern-table
    /// boundary, and on reset.
    pub(crate) fn invalidate_rows(&mut self) {
        self.rows.clear();
        self.rows_scanline = None;
        self.zero_row_idx = None;
    }

    /// Update sprite status to ppu status register
    pub fn update_ctrl_status(&mut self, status: &mut PpuStatus) {
        if std::mem::take(&mut self.overflow_pending) {
            status.set_sprite_overflow(true);
        }
    }

    pub fn clear_pending(&mut self) {
        self.overflow_pending = false;
    }

    pub fn swap_secondary_oam(&mut self) {
        self.current_scanline_oam = std::mem::take(&mut self.next_scanline_oam);
        self.current_zero_sprite = self.next_zero_sprite.take();
        // The incoming line has its own pattern rows.
        self.invalidate_rows();
        // A new line starts with an empty copy buffer (clear phase returns
        // $FF); evaluation steps on odd dots 65-256 overwrite it byte by byte.
        self.oam_copybuffer = 0xFF;
    }

    pub fn begin_sprite_overflow_eval(&mut self, start_index: u8) {
        self.sprite_overflow_eval = SpriteOverflowEval {
            oam_index: 0,
            byte_index: 0,
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
                // Odd-cycle primary-OAM read lands on the internal bus: a $2004
                // read during dots 65-256 observes this copy buffer.
                self.oam_copybuffer = y;
                if sprite_in_range(y, target_scanline(scanline), ctrl.sprite_height()) {
                    if self.next_scanline_oam.len() >= 8 {
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
                self.oam_copybuffer = oam_byte;
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
                    self.sprite_overflow_eval.mode = if self.next_scanline_oam.len() >= 8 {
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
                self.oam_copybuffer = y_byte;
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

    /// Read-only projection of the evaluation copy buffer for $2004 reads
    /// on dots 65-256 of visible lines (rendering on). Takes `&self` only:
    /// the register layer observes eval traffic without mutating it.
    pub fn copy_buffer(&self) -> u8 {
        self.oam_copybuffer
    }

    /// Read-only projection of secondary OAM (the next-line buffer under
    /// construction) for $2004 reads on dots 257-320. Unwritten slots read
    /// as $FF, matching the hardware clear phase. Takes `&self` only.
    pub fn secondary_byte(&self, index: usize) -> u8 {
        let sprite_idx = index / 4;
        let byte = index % 4;
        if sprite_idx < self.next_scanline_oam.len() {
            let raw: [u8; 4] = bytemuck::cast(self.next_scanline_oam[sprite_idx]);
            raw[byte]
        } else {
            0xFF
        }
    }

    /// Fill the row cache for one scanline, lazily on the first pixel
    /// request. One fill per evaluated Sprite resolves its vertical position
    /// (vertical flip and 8x16 height applied here) and reads the two pattern
    /// planes; afterwards the per-pixel path decodes from the cached planes.
    fn ensure_rows(
        &mut self,
        ctrl: PpuCtrl,
        cartridge: &Cartridge,
        screen_y: u8,
        render: &PpuRenderState,
    ) {
        if self.rows_scanline == Some(screen_y) {
            return;
        }

        self.rows.clear();
        self.zero_row_idx = None;
        let sprite_height = ctrl.sprite_height();

        for i in 0..self.current_scanline_oam.len() {
            let sprite = self.current_scanline_oam[i];
            let src_y = sprite_src_y(&sprite, sprite_height, screen_y);
            let tile_position = sprite.tile_position(ctrl);
            let addr = tile_position.resolve_pixel_addr(src_y);
            // Ticket #78: sprite fetches carry the Sprite kind so MMC5 picks
            // the sprite CHR bank set in 8x16 mode.
            let spr_render = PpuRenderState {
                fetch: PpuFetchKind::Sprite,
                ..*render
            };
            let low = cartridge.read_vram(addr.0, &spr_render);
            let high = cartridge.read_vram(addr.second_plane_addr(), &spr_render);

            self.rows.push(SpriteRow {
                x: sprite.x,
                palette: sprite.attributes.palette(),
                behind_bg: sprite.attributes.behind_background(),
                flip_horizontally: sprite.attributes.flip_horizontally(),
                low,
                high,
            });

            if self.current_zero_sprite == Some(sprite) {
                self.zero_row_idx = Some(i);
            }
        }

        self.rows_scanline = Some(screen_y);
    }

    pub fn find_sprite_pixel(
        &mut self,
        ctrl: PpuCtrl,
        cartridge: &Cartridge,
        screen_x: u8,
        screen_y: u8,
        render: &PpuRenderState,
    ) -> Option<SpritePixel> {
        self.ensure_rows(ctrl, cartridge, screen_y, render);
        self.rows.iter().find_map(|row| row.pixel(screen_x))
    }

    /// Sprite-0 hit source pixel: opaque only when OAM entry 0 survived
    /// this scanline's evaluation and its pixel overlaps the beam. Reads the
    /// already-filled cached row of the evaluated zero Sprite and never
    /// fetches from the cartridge itself. When the row cache has not been
    /// filled for this scanline, no pixel request reached the Sprite path,
    /// and the hit cannot fire in that window, so this reports transparent.
    pub fn sprite_zero_pixel_opaque(&self, screen_x: u8, screen_y: u8) -> bool {
        if self.rows_scanline != Some(screen_y) {
            return false;
        }
        self.zero_row_idx
            .and_then(|idx| self.rows[idx].pixel(screen_x))
            .is_some_and(|pixel| pixel.color_idx != 0)
    }
}

/// One evaluated Sprite decoded pattern row for the current scanline.
/// Vertical flip and 8x16 height are already applied to the plane bytes;
/// horizontal flip stays a per-pixel selection of 7 - rel_x.
#[derive(Copy, Clone, Default)]
struct SpriteRow {
    x: u8,
    palette: u8,
    behind_bg: bool,
    flip_horizontally: bool,
    low: u8,
    high: u8,
}

impl SpriteRow {
    fn pixel(&self, screen_x: u8) -> Option<SpritePixel> {
        let rel_x = screen_x as i16 - self.x as i16;
        if !(0..8).contains(&rel_x) {
            return None;
        }

        let src_x = if self.flip_horizontally {
            (7 - rel_x) as u8
        } else {
            rel_x as u8
        };
        let color_idx = ((self.low >> (7 - src_x)) & 1) | (((self.high >> (7 - src_x)) & 1) << 1);

        if color_idx == 0 {
            return None;
        }

        Some(SpritePixel {
            palette_idx: self.palette,
            color_idx,
            behind_bg: self.behind_bg,
        })
    }
}

/// Vertical source row for one Sprite: vertical flip and 8x16 height are
/// applied here, when src_y is resolved, not per pixel.
fn sprite_src_y(sprite: &Sprite, sprite_height: u8, screen_y: u8) -> u8 {
    let top = sprite.y as i16 + 1;
    let sprite_y = screen_y as i16 - top;
    if sprite.attributes.flip_vertically() {
        (sprite_height as i16 - 1 - sprite_y) as u8
    } else {
        sprite_y as u8
    }
}

#[derive(Copy, Clone)]
pub struct SpritePixel {
    pub palette_idx: u8,
    pub color_idx: u8,
    pub behind_bg: bool,
}
