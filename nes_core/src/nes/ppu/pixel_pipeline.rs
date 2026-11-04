//! Two-dot pixel output pipeline.
//!
//! The PPU does not write a rendered pixel to the framebuffer on the dot it
//! is rendered: it leaves the output stage two dots later, after any CPU
//! register write that landed in between has colored it. This module holds
//! those in-flight pixels.

/// A rendered pixel waiting in the output pipeline.
///
/// `pixel_index` is the palette index the color-modifying stage will look
/// up at commit time; it is a 6-bit value, so `$3F` is its ceiling. The
/// value `0xFF` marks an empty slot and can never be a real pixel index.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(super) struct PendingPixel {
    pub x: u8,
    pub y: u16,
    pub pixel_index: u8,
}

impl PendingPixel {
    pub const EMPTY: Self = Self {
        x: 0,
        y: 0,
        pixel_index: 0xFF,
    };

    pub fn is_empty(self) -> bool {
        self.pixel_index == 0xFF
    }
}

impl Default for PendingPixel {
    fn default() -> Self {
        Self::EMPTY
    }
}

/// Fixed two-slot output pipeline: at most two pixels are ever in flight.
///
/// Call [`Self::take_due`] once per PPU dot, before pushing the pixel the
/// dot rendered. A pixel pushed on dot `N` becomes due on dot `N + 2`: the
/// push lands in `back`, the next dot's rotation moves it to `front`, and
/// the dot after that retires it.
#[derive(Default)]
pub(super) struct PixelPipeline {
    front: PendingPixel,
    back: PendingPixel,
}

impl PixelPipeline {
    pub fn new() -> Self {
        Self::default()
    }

    /// Retire the pixel due on this dot, then rotate so the pixel pushed on
    /// the previous dot becomes due on the next one.
    ///
    /// Returns [`PendingPixel::EMPTY`] on dots where no pixel falls out of
    /// the pipeline; callers check [`PendingPixel::is_empty`].
    pub fn take_due(&mut self) -> PendingPixel {
        let due = std::mem::replace(&mut self.front, PendingPixel::EMPTY);
        std::mem::swap(&mut self.front, &mut self.back);
        due
    }

    /// Queue the pixel rendered on the current dot; it becomes due two dots
    /// later. [`Self::take_due`] always vacates `back` first, so a push
    /// never overwrites a pixel that has not had its two dots yet.
    pub fn push(&mut self, x: u8, y: u16, pixel_index: u8) {
        debug_assert!(self.back.is_empty(), "push without a take_due this dot");
        self.back = PendingPixel { x, y, pixel_index };
    }

    /// Drop every in-flight pixel. A reset landing mid-frame must not let
    /// already-rendered pixels of the old frame commit into the fresh one.
    pub fn clear(&mut self) {
        *self = Self::default();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pixel(x: u8, y: u16, pixel_index: u8) -> PendingPixel {
        PendingPixel { x, y, pixel_index }
    }

    #[test]
    fn pixel_becomes_due_two_dots_after_push() {
        let mut pipeline = PixelPipeline::new();

        // Dot 1: nothing in flight yet, then render.
        assert!(pipeline.take_due().is_empty());
        pipeline.push(0, 100, 0x16);
        // Dot 2: the dot-1 pixel still needs another dot.
        assert!(pipeline.take_due().is_empty());
        pipeline.push(1, 100, 0x16);
        // Dot 3: the dot-1 pixel is due, the dot-2 pixel is not.
        assert_eq!(pipeline.take_due(), pixel(0, 100, 0x16));
        pipeline.push(2, 100, 0x16);
        assert_eq!(pipeline.take_due(), pixel(1, 100, 0x16));
    }

    #[test]
    fn take_due_without_push_returns_empty() {
        let mut pipeline = PixelPipeline::new();
        for _ in 0..4 {
            assert!(pipeline.take_due().is_empty());
        }
    }

    #[test]
    fn in_flight_pixels_drain_across_dots_without_push() {
        let mut pipeline = PixelPipeline::new();
        // Dots 1 and 2 of a scanline render the last two pixels...
        pipeline.take_due();
        pipeline.push(255, 100, 0x21);
        pipeline.take_due();
        pipeline.push(0, 101, 0x21);
        // ...and they commit on their own dots +2, after the visible span
        // (dot 258 is already the next scanline's idle dot).
        assert_eq!(pipeline.take_due(), pixel(255, 100, 0x21));
        assert_eq!(pipeline.take_due(), pixel(0, 101, 0x21));
        assert!(pipeline.take_due().is_empty());
    }

    #[test]
    fn clear_drops_both_in_flight_pixels() {
        let mut pipeline = PixelPipeline::new();
        pipeline.take_due();
        pipeline.push(0, 100, 0x16);
        pipeline.take_due();
        pipeline.push(1, 100, 0x16);
        // Both slots hold a pixel here; a reset must drop both.
        pipeline.clear();
        assert!(pipeline.take_due().is_empty());
        assert!(pipeline.take_due().is_empty());
        pipeline.push(2, 100, 0x16);
        assert!(pipeline.take_due().is_empty());
        assert_eq!(pipeline.take_due(), pixel(2, 100, 0x16));
    }

    #[test]
    fn highest_palette_index_round_trips() {
        let mut pipeline = PixelPipeline::new();
        pipeline.take_due();
        // Palette indices are 6-bit ($3F max), one below the $FF marker
        // that marks an empty slot.
        pipeline.push(0, 0, 0x3F);
        pipeline.take_due();
        assert_eq!(pipeline.take_due(), pixel(0, 0, 0x3F));
        assert!(pipeline.take_due().is_empty());
    }
}
