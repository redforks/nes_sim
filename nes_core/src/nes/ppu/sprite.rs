use super::oam::{Attribute, Oam, Sprite};
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
    /// Per-column bitmask of the rows whose 8-pixel span covers that column:
    /// bit `i` set means `rows[i]` reaches column `x`. Rebuilt whenever the
    /// rows are (one scanline at a time), so `find_sprite_pixel` loads one
    /// byte and visits only the rows that can draw at `screen_x` instead of
    /// range-checking all eight. Geometric only: transparency is still
    /// decided by [`SpriteRow::pixel`], the single place that also applies
    /// the horizontal flip.
    column_mask: [u8; 256],
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

/// Save-state mirror of [`SpriteOverflowEvalMode`]: the tag plus the
/// `CopySprite` countdown (unused for the other modes).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SpriteEvalModeTag {
    Idle,
    ScanY,
    CopySprite(u8),
    OverflowSearchDelay,
    OverflowSearch,
    Done,
}

/// Save-state mirror of [`SpriteOverflowEval`] in core types.
#[derive(Debug, Clone, Copy)]
pub(crate) struct SpriteOverflowEvalState {
    pub oam_index: u8,
    pub byte_index: u8,
    pub start_index: u8,
    pub mode: SpriteEvalModeTag,
    pub pending_sprite_bytes: [u8; 4],
}

/// Save-state mirror of one decoded [`SpriteRow`].
#[derive(Debug, Clone, Copy)]
pub(crate) struct SpriteRowState {
    pub x: u8,
    pub attributes: u8,
    pub low: u8,
    pub high: u8,
}

/// Full [`SpriteManager`] execution state for save-states (ticket #184).
#[derive(Debug, Clone)]
pub(crate) struct SpriteManagerState {
    pub overflow_pending: bool,
    pub eval: SpriteOverflowEvalState,
    pub current_scanline_oam: Vec<Sprite>,
    pub next_scanline_oam: Vec<Sprite>,
    pub current_zero_sprite: Option<Sprite>,
    pub next_zero_sprite: Option<Sprite>,
    pub rows: Vec<SpriteRowState>,
    pub rows_scanline: Option<u8>,
    pub zero_row_idx: Option<usize>,
    pub oam_copybuffer: u8,
}

impl SpriteManager {
    /// Capture the full evaluation/render state.
    pub(crate) fn capture_sprite_state(&self) -> SpriteManagerState {
        let mode = match self.sprite_overflow_eval.mode {
            SpriteOverflowEvalMode::Idle => SpriteEvalModeTag::Idle,
            SpriteOverflowEvalMode::ScanY => SpriteEvalModeTag::ScanY,
            SpriteOverflowEvalMode::CopySprite { remaining_bytes } => {
                SpriteEvalModeTag::CopySprite(remaining_bytes)
            }
            SpriteOverflowEvalMode::OverflowSearchDelay => SpriteEvalModeTag::OverflowSearchDelay,
            SpriteOverflowEvalMode::OverflowSearch => SpriteEvalModeTag::OverflowSearch,
            SpriteOverflowEvalMode::Done => SpriteEvalModeTag::Done,
        };
        SpriteManagerState {
            overflow_pending: self.overflow_pending,
            eval: SpriteOverflowEvalState {
                oam_index: self.sprite_overflow_eval.oam_index,
                byte_index: self.sprite_overflow_eval.byte_index,
                start_index: self.sprite_overflow_eval.start_index,
                mode,
                pending_sprite_bytes: self.sprite_overflow_eval.pending_sprite_bytes,
            },
            current_scanline_oam: self.current_scanline_oam.iter().copied().collect(),
            next_scanline_oam: self.next_scanline_oam.iter().copied().collect(),
            current_zero_sprite: self.current_zero_sprite,
            next_zero_sprite: self.next_zero_sprite,
            rows: self
                .rows
                .iter()
                .map(|r| SpriteRowState {
                    x: r.x,
                    attributes: r.attributes.into_bytes()[0],
                    low: r.low,
                    high: r.high,
                })
                .collect(),
            rows_scanline: self.rows_scanline,
            zero_row_idx: self.zero_row_idx,
            oam_copybuffer: self.oam_copybuffer,
        }
    }

    /// Restore a state previously produced by [`Self::capture_sprite_state`].
    pub(crate) fn restore_sprite_state(&mut self, s: &SpriteManagerState) {
        self.overflow_pending = s.overflow_pending;
        self.sprite_overflow_eval.oam_index = s.eval.oam_index;
        self.sprite_overflow_eval.byte_index = s.eval.byte_index;
        self.sprite_overflow_eval.start_index = s.eval.start_index;
        self.sprite_overflow_eval.mode = match s.eval.mode {
            SpriteEvalModeTag::Idle => SpriteOverflowEvalMode::Idle,
            SpriteEvalModeTag::ScanY => SpriteOverflowEvalMode::ScanY,
            SpriteEvalModeTag::CopySprite(remaining_bytes) => {
                SpriteOverflowEvalMode::CopySprite { remaining_bytes }
            }
            SpriteEvalModeTag::OverflowSearchDelay => SpriteOverflowEvalMode::OverflowSearchDelay,
            SpriteEvalModeTag::OverflowSearch => SpriteOverflowEvalMode::OverflowSearch,
            SpriteEvalModeTag::Done => SpriteOverflowEvalMode::Done,
        };
        self.sprite_overflow_eval.pending_sprite_bytes = s.eval.pending_sprite_bytes;
        self.current_scanline_oam.clear();
        self.current_scanline_oam
            .extend(s.current_scanline_oam.iter().copied());
        self.next_scanline_oam.clear();
        self.next_scanline_oam
            .extend(s.next_scanline_oam.iter().copied());
        self.current_zero_sprite = s.current_zero_sprite;
        self.next_zero_sprite = s.next_zero_sprite;
        self.rows.clear();
        self.rows.extend(s.rows.iter().map(|r| SpriteRow {
            x: r.x,
            attributes: Attribute::from_bytes([r.attributes]),
            low: r.low,
            high: r.high,
        }));
        // Derived, never serialized: rebuild from the restored rows.
        self.rebuild_column_mask();
        self.rows_scanline = s.rows_scanline;
        self.zero_row_idx = s.zero_row_idx;
        self.oam_copybuffer = s.oam_copybuffer;
    }
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
            column_mask: [0; 256],
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
        self.column_mask.fill(0);
    }

    /// Update sprite status to ppu status register
    #[inline]
    pub fn update_ctrl_status(&mut self, status: &mut PpuStatus) {
        if self.overflow_pending {
            self.overflow_pending = false;
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

    #[inline]
    pub fn is_eval_done(&self) -> bool {
        matches!(self.sprite_overflow_eval.mode, SpriteOverflowEvalMode::Done)
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
            scanline + 1
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
    #[inline]
    fn ensure_rows(
        &mut self,
        ctrl: PpuCtrl,
        cartridge: &Cartridge,
        screen_y: u8,
        mut render: PpuRenderState,
    ) {
        if self.rows_scanline == Some(screen_y) {
            return;
        }

        // Ticket #78: sprite fetches carry the Sprite kind so MMC5 picks
        // the sprite CHR bank set in 8x16 mode.
        render.fetch = PpuFetchKind::Sprite;
        self.rows_scanline = Some(screen_y);
        self.rows.clear();
        self.zero_row_idx = None;
        let sprite_height = ctrl.sprite_height();

        for (i, sprite) in self.current_scanline_oam.iter().copied().enumerate() {
            let src_y = sprite_src_y(&sprite, sprite_height, screen_y);
            let tile_position = sprite.tile_position(ctrl);
            let addr = tile_position.resolve_pixel_addr(src_y);
            let (low, high) = cartridge.read_tile_low_high(addr.0, render);

            self.rows.push(SpriteRow {
                x: sprite.x,
                attributes: sprite.attributes,
                low,
                high,
            });

            if self.current_zero_sprite == Some(sprite) {
                self.zero_row_idx = Some(i);
            }
        }

        self.rebuild_column_mask();
    }

    /// Rebuild [`Self::column_mask`] from the current `rows`. Called after
    /// `ensure_rows` fills them and after a save-state restore puts them back.
    fn rebuild_column_mask(&mut self) {
        self.column_mask.fill(0);
        for (i, row) in self.rows.iter().enumerate() {
            // A row at x=250 spans columns 250..=257; the last two fall off
            // the screen and get no bits.
            for dx in 0..8u16 {
                let x = row.x as u16 + dx;
                if x < 256 {
                    self.column_mask[x as usize] |= 1 << i;
                }
            }
        }
    }

    #[inline]
    pub fn find_sprite_pixel(
        &mut self,
        ctrl: PpuCtrl,
        cartridge: &Cartridge,
        screen_x: u8,
        screen_y: u8,
        render: PpuRenderState,
    ) -> Option<SpritePixel> {
        self.ensure_rows(ctrl, cartridge, screen_y, render);
        let mut candidates = self.column_mask[screen_x as usize];
        while candidates != 0 {
            let idx = candidates.trailing_zeros() as usize;
            if let Some(pixel) = self.rows[idx].pixel(screen_x) {
                return Some(pixel);
            }
            // Geometric coverage only: this row spans the column but its
            // pixel is transparent, so try the next covering row in OAM order.
            candidates &= candidates - 1;
        }
        None
    }

    /// Whether the cached rows carry sprite 0 for `screen_y` at all: the
    /// cheap prefix of [`Self::sprite_zero_pixel_opaque`]. The hit test in
    /// `render_pixel` asks this first, so a scanline without sprite 0 skips
    /// the other six terms of its conjunction. `ensure_rows` sets
    /// `rows_scanline` on the first pixel request of a line whether or not
    /// sprite 0 is in range, so this is false for a whole line exactly when
    /// the row lookup would be.
    #[inline]
    pub fn zero_row_on_scanline(&self, screen_y: u8) -> bool {
        self.rows_scanline == Some(screen_y) && self.zero_row_idx.is_some()
    }

    /// Sprite-0 hit source pixel: opaque only when OAM entry 0 survived
    /// this scanline's evaluation and its pixel overlaps the beam. Reads the
    /// already-filled cached row of the evaluated zero Sprite and never
    /// fetches from the cartridge itself. When the row cache has not been
    /// filled for this scanline, no pixel request reached the Sprite path,
    /// and the hit cannot fire in that window, so this reports transparent.
    pub fn sprite_zero_pixel_opaque(&self, screen_x: u8, screen_y: u8) -> bool {
        if !self.zero_row_on_scanline(screen_y) {
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
    attributes: Attribute,
    low: u8,
    high: u8,
}

impl SpriteRow {
    fn pixel(&self, screen_x: u8) -> Option<SpritePixel> {
        let rel_x = screen_x as i16 - self.x as i16;
        if !(0..8).contains(&rel_x) {
            return None;
        }

        let src_x = if self.attributes.flip_horizontally() {
            (7 - rel_x) as u8
        } else {
            rel_x as u8
        };
        let color_idx = ((self.low >> (7 - src_x)) & 1) | (((self.high >> (7 - src_x)) & 1) << 1);

        if color_idx == 0 {
            return None;
        }

        Some(SpritePixel {
            palette_idx: self.attributes.palette(),
            color_idx,
            behind_bg: self.attributes.behind_background(),
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

#[cfg(test)]
impl SpriteManager {
    /// Install pattern rows directly, as if `ensure_rows` had just filled them
    /// for `scanline`. Test-only: the mask-equivalence test drives rows that
    /// would be awkward to reach through OAM and CHR.
    pub(crate) fn set_test_rows(&mut self, scanline: u8, rows: &[(u8, u8, u8, u8)]) {
        self.rows.clear();
        for &(x, attributes, low, high) in rows {
            self.rows.push(SpriteRow {
                x,
                attributes: Attribute::from_bytes([attributes]),
                low,
                high,
            });
        }
        self.rows_scanline = Some(scanline);
        self.zero_row_idx = None;
        self.rebuild_column_mask();
    }

    /// The pre-mask lookup: linear scan over every cached row.
    pub(crate) fn scan_rows_pixel(&self, screen_x: u8) -> Option<SpritePixel> {
        self.rows.iter().find_map(|row| row.pixel(screen_x))
    }
}
