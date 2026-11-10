mod batch_convert;
mod oam;
pub mod palette;
// The former two-dot `pixel_pipeline` output stage is retired (ticket
// #62): per-dot geometry writes only the palette index into the flat
// buffer, and the frame is flushed in linear raster order at the VBL
// set dot — see `Ppu::flush_palette_index_buffer` in `ppu.rs`.
mod registers;
mod sprite;

use crate::{
    nes::{
        mapper::{Cartridge, PpuRenderState},
        ppu::{palette::ColorTheme, sprite::SpriteManager},
    },
    render::Render,
};
use oam::Oam;
use palette::{Palette, Pixel};
use registers::{ColorConfig, PpuCtrl, PpuMask, PpuStatus, Registers};

// Pixel, palette data and COLORS are defined in the submodule `palette`.

/// Visible frame dimensions: the flat palette-index buffer holds exactly
/// one entry per visible beam position (`y * 256 + x`).
const VISIBLE_WIDTH: usize = 256;
const VISIBLE_HEIGHT: usize = 240;

// PPU Timing Constants
const PPU_OPEN_BUS_DECAY_TICKS: u64 = 3_221_591 * crate::SYSTEM_CYCLES_PER_PPU_CYCLE;
/// Width of the back-to-back $2007 PPUDATA ignore window, in PPU dots
/// (~2 CPU cycles): a second read landing inside it is fully ignored.
/// Mesen2 sets `_ignoreVramRead = 6` at the read and decrements once per
/// PPU cycle, so read cycles through +5 are ignored and +6 is normal.
const PPUDATA_IGNORE_WINDOW_DOTS: u64 = 5;

#[derive(Copy, Clone, Default)]
struct TileCache {
    bitplane_low: u8,
    bitplane_high: u8,
    palette_idx: u8,
    remaining: u8,
}

impl TileCache {
    fn consume(&mut self) -> (u8, u8) {
        let sub_tile_x = 8 - self.remaining;
        self.remaining -= 1;
        let bit = 7 - sub_tile_x;
        let color_idx = ((self.bitplane_low >> bit) & 1) | (((self.bitplane_high >> bit) & 1) << 1);
        (self.palette_idx, color_idx)
    }
}

/// A mid-frame `$2001` color change (ticket #63): the beam position at
/// which the change takes effect, plus the color configuration in force
/// before the write. The flush applies `prior` below `index` and the new
/// configuration at/after it — the new configuration is the next split's
/// `prior`, or the live mask at flush time for the final segment.
#[derive(Copy, Clone)]
struct ColorSplit {
    index: usize,
    prior: ColorConfig,
}

#[derive(Copy, Clone)]
struct BackgroundActivation {
    screen_x: u8,
    vram_addr: u16,
    fine_x: u8,
    ctrl: PpuCtrl,
}

impl BackgroundActivation {
    fn snapshot(ppu: &Ppu<impl Render>, screen_x: u8) -> Self {
        Self {
            screen_x,
            vram_addr: ppu.registers.vram_addr,
            fine_x: ppu.registers.fine_x,
            ctrl: ppu.registers.ctrl,
        }
    }
}

#[derive(Copy, Clone, Default)]
pub struct Timing {
    scanline: u16,
    dot: u16,
    odd_frame: bool,
    frame_no: usize,
}

impl Timing {
    fn new() -> Self {
        Self {
            scanline: 261,
            dot: 0,
            odd_frame: false,
            frame_no: 0,
        }
    }

    fn reset(&mut self) {
        self.odd_frame = false;
    }

    fn next_visible_x(&self) -> Option<u8> {
        self.is_visible().then(|| (self.dot - 1) as u8)
    }

    /// Return true if scanline and dot is in visible area
    const fn is_visible(&self) -> bool {
        self.in_visible_scanline() && self.dot >= 1 && self.dot <= 256
    }

    /// Return true if dot is in visible dots (65-256), after the
    /// secondary-OAM clear phase
    const fn in_visible_dots(&self) -> bool {
        self.dot >= 65 && self.dot <= 256
    }

    /// Return true if scanline and dot is in visible area after sprite
    /// evaluation (dots 65-256 of visible scanlines)
    const fn in_visible_area(&self) -> bool {
        self.in_visible_scanline() && self.in_visible_dots()
    }

    fn advance(&mut self, ren_latched_at_338: bool) {
        match self.dot {
            ..339 => self.dot += 1,
            339 if self.scanline != 261 || (!self.odd_frame || !ren_latched_at_338) => {
                self.dot += 1
            }
            _ => {
                self.dot = 0;
                self.scanline += 1;
                if self.scanline >= 262 {
                    self.scanline = 0;
                    self.odd_frame = !self.odd_frame;
                    self.frame_no += 1;
                }
            }
        }
    }

    /// Return true if just enter vblank timing, scanline 241, dot 1, should set vblank flag
    fn enter_vblank(&self) -> bool {
        self.scanline == 241 && self.dot == 1
    }

    /// Return true if just leave vblank timing, scanline 261, dot 1, should clear vblank flag
    fn leave_vblank(&self) -> bool {
        self.scanline == 261 && self.dot == 1
    }

    pub fn frame_no(&self) -> usize {
        self.frame_no
    }

    pub fn scanline(&self) -> u16 {
        self.scanline
    }

    pub fn dot(&self) -> u16 {
        self.dot
    }

    const fn in_visible_scanline(&self) -> bool {
        self.scanline < 240
    }

    /// Return true if scanline is 261, in this scanline ppu preparing for the next frame
    fn is_prepare_line(&self) -> bool {
        self.scanline == 261
    }

    /// Return true if ppu should active for current scanline.
    /// Note, if ppu rendering is disabled (both front and background), ppu is also disactivated
    fn in_ppu_active_line(&self) -> bool {
        // although line 261 is not visible, but ppu need to prepare for next frame first line
        self.in_visible_scanline() || self.is_prepare_line()
    }
}

pub struct Ppu<R: Render = ()> {
    registers: Registers,
    palette: Palette,
    oam: Oam,
    oam_addr: u8,
    color_theme: ColorTheme,
    renderer: R,

    timing: Timing,

    background_anchor: Option<BackgroundActivation>,
    pending_background_activation: Option<BackgroundActivation>,
    tile_cache: TileCache,
    sprite: SpriteManager,
    /// Set when a $2002 read lands one PPU clock before VBL flag set
    /// (scanline 241 dot 0); the next VBL set is suppressed entirely —
    /// flag never rises, NMI never asserts.
    suppress_vblank_pending: bool,

    /// Rendering-enabled sampled at pre-render dot 338. The odd-frame skipped
    /// clock decision uses this value, not live status at dot 339: blargg
    /// ppu_vbl_nmi 10-even_odd_timing requires a BG enable that becomes
    /// visible only at dot 339 to miss the skip, while one visible at 338
    /// still catches it.
    ren_latched_at_338: bool,

    /// Cumulative system cycle counter, incremented each tick.
    /// Replaces the global `get_system_cycles()` for PPU-internal timing.
    cycle: u64,
    /// Cycle and returned value of the last completing PPUDATA ($2007) read.
    /// A second read landing inside the PPUDATA ignore window (back-to-back
    /// dummy+real reads of page-crossing `lda abs,X`) is fully ignored on
    /// hardware (Mesen2) and re-returns this value. Ignored reads do not
    /// stamp the cycle.
    ppudata_last_read: Option<(u64, u8)>,
    /// Flat palette-index buffer for the current frame (ticket #62, batch
    /// color): one 6-bit palette entry per visible beam position, indexed
    /// by `y * 256 + x`. Allocated once in [`Self::new`] and reused every
    /// frame — no per-frame 61,440-byte allocation, no per-pixel
    /// `(x, y, palette_index)` tuples. The live per-dot path writes only
    /// the resolved palette index here; [`Self::flush_palette_index_buffer`]
    /// resolves indices to RGBA once at the VBL set dot before `finish()`.
    ///
    /// This retires the former two-dot `PixelPipeline` output stage as a
    /// per-pixel delay: raster order is preserved by the flush's linear
    /// scan, and any dot-level color landing offset becomes the stored
    /// split position rather than pipeline slots (see #63). Palette RAM is
    /// still read live at per-dot resolve time, so a palette write takes
    /// effect from its beam position without flushing geometry.
    palette_index_buffer: Box<[u8]>,
    /// Mid-frame `$2001` color changes for the frame being buffered
    /// (ticket #63), in beam order. Consumed and cleared by
    /// [`Self::flush_palette_index_buffer`]; splits recorded after the
    /// flush (during vblank/pre-render) are discarded at the next frame
    /// start, since the live mask already carries them into that frame.
    color_splits: Vec<ColorSplit>,
    /// Number of leading buffer entries written this frame. The live
    /// per-dot path writes strictly in raster order, so a single count
    /// tracks which beam positions have been buffered: indices below it
    /// hold this frame's palette indices, indices at/after it are stale
    /// and have not been scanned yet. [`Self::pixel_brightness`] answers
    /// black for the latter — matching the materialized framebuffer,
    /// which is also black until the beam passes.
    buffered_pixel_count: usize,
    /// First beam position holding this frame's index (ticket #67).
    /// Normally 0; a mid-frame reset moves it to the next beam position,
    /// so positions scanned before the reset flush and report black
    /// instead of their stale pre-reset indices. The valid range is
    /// `buffered_pixel_from..buffered_pixel_count`.
    buffered_pixel_from: usize,
    /// Set when the PPU timing enters vblank (scanline 241, dot 1).
    /// This is a pure timing event, independent of the $2002 VBL flag
    /// (which `$2002` reads clear and the dot-0 race can suppress).
    /// `NesMachine::process_frame` consumes it via
    /// [`Self::take_vblank_entered`].
    vblank_entered: bool,
}

/// PPU registers are mirrored every 8 bytes in range $2000-$3FFF;
/// this function normalized $2000, $2008, .. $3ff8 to $2000
fn normalize_ppu_addr(addr: u16) -> u16 {
    addr & 0x2007
}

/// Wrap a PPU address into the 16 KiB bus space.
///
/// `vram_addr` is 15 bits, so the `$4000` wrap decides palette vs cartridge:
/// after wrapping, `$3F00-$3FFF` is palette and everything below is cartridge
/// space. The `$3000-$3EFF` nametable mirror is deliberately not folded here:
/// the cartridge PPU window maps its alias slots to the same nametable banks
/// as `$2000-$2EFF`, so cartridge reads and writes agree either way.
///
/// `$4000` is a power of two, so the mask is exactly the modulo.
fn normalize_vram_addr(address: u16) -> u16 {
    address & 0x3FFF
}

impl<R: Render> Ppu<R> {
    pub fn new(renderer: R) -> Self {
        Ppu {
            registers: Registers::new(),
            palette: Palette::default(),
            oam: Oam::default(),
            oam_addr: 0,
            color_theme: ColorTheme::default(),
            renderer,
            timing: Timing::new(),
            background_anchor: None,
            pending_background_activation: None,
            tile_cache: TileCache::default(),
            sprite: SpriteManager::new(),
            suppress_vblank_pending: false,
            ren_latched_at_338: false,
            cycle: 0,
            ppudata_last_read: None,
            palette_index_buffer: vec![0; VISIBLE_WIDTH * VISIBLE_HEIGHT].into_boxed_slice(),
            color_splits: Vec::new(),
            buffered_pixel_count: 0,
            buffered_pixel_from: 0,
            vblank_entered: false,
        }
    }

    pub fn set_color_theme(&mut self, theme: ColorTheme) {
        self.color_theme = theme;
    }

    pub fn reset(&mut self) {
        // https://www.nesdev.org/wiki/PPU_power_up_state
        self.registers.reset();
        self.background_anchor = None;
        self.pending_background_activation = None;
        self.tile_cache = TileCache::default();
        self.timing.reset();
        self.sprite.reset();
        self.registers.write_scroll(0);
        self.ren_latched_at_338 = false;
        self.vblank_entered = false;
        // A reset landing mid-frame must not let already-buffered palette
        // indices of the old frame flush into the fresh frame: positions
        // not yet rewritten since the reset flush black (matching the
        // cleared pre-change framebuffer) and report black to live
        // brightness queries. No buffer fill is needed: the valid range
        // (`buffered_pixel_from..buffered_pixel_count`) simply never
        // reads stale entries.
        self.color_splits.clear();
        self.buffered_pixel_from = self.next_beam_index();
        self.buffered_pixel_count = 0;
    }

    pub fn timing(&self) -> &Timing {
        &self.timing
    }

    pub fn renderer(&self) -> &R {
        &self.renderer
    }

    pub fn renderer_mut(&mut self) -> &mut R {
        &mut self.renderer
    }

    fn schedule_background_activation_if_visible(&mut self) {
        if self.rendering_enabled()
            && let Some(screen_x) = self.timing.next_visible_x()
        {
            let screen_x = screen_x.saturating_add(16);
            self.pending_background_activation =
                Some(BackgroundActivation::snapshot(self, screen_x));
        }
    }

    fn apply_pending_background_activation(&mut self, screen_x: u8) {
        if let Some(pending) = self.pending_background_activation
            && screen_x >= pending.screen_x
        {
            self.background_anchor = Some(pending);
            self.pending_background_activation = None;
        }
    }

    /// Increment fine Y in vram_addr. Called at dot 256 of each visible scanline
    /// when rendering is enabled. Wraps fine Y → coarse Y → nametable Y toggle.
    fn increment_vram_y(&mut self) {
        let fine_y = (self.registers.vram_addr >> 12) & 0x07;
        if fine_y < 7 {
            self.registers.vram_addr += 0x1000; // increment fine Y
        } else {
            self.registers.vram_addr &= !0x7000; // clear fine Y
            let mut coarse_y = (self.registers.vram_addr >> 5) & 0x1F;
            if coarse_y == 29 {
                coarse_y = 0;
                self.registers.vram_addr ^= 0x0800; // toggle nametable Y
            } else if coarse_y == 31 {
                coarse_y = 0; // wrap without toggling nametable
            } else {
                coarse_y += 1;
            }
            self.registers.vram_addr = (self.registers.vram_addr & !0x03E0) | (coarse_y << 5);
        }
    }

    /// Copy horizontal scroll bits (coarse X + nametable X) from temp to vram.
    /// Called at dot 257 of each visible/pre-render scanline when rendering is enabled.
    fn reload_horizontal_from_temp(&mut self) {
        // Horizontal bits: coarse X (bits 0-4) and nametable X (bit 10)
        self.registers.vram_addr =
            (self.registers.vram_addr & !0x041F) | (self.registers.temp_vram_addr & 0x041F);
    }

    /// Copy vertical scroll bits (coarse Y + nametable Y + fine Y) from temp to vram.
    /// Called at dots 280-304 of the pre-render scanline when rendering is enabled.
    fn reload_vertical_from_temp(&mut self) {
        // Vertical bits: coarse Y (bits 5-9), nametable Y (bit 11), fine Y (bits 12-14)
        self.registers.vram_addr =
            (self.registers.vram_addr & !0x7BE0) | (self.registers.temp_vram_addr & 0x7BE0);
    }

    pub fn rendering_enabled(&self) -> bool {
        self.registers.mask.background_enabled() || self.registers.mask.sprite_enabled()
    }

    /// Render-state snapshot for the mapper PPU-read seam (ticket #75):
    /// rendering from the PPUMASK enable bits, sprite size from PPUCTRL
    /// `$2000`, and the fetch phase as `dot % 8`. The mapper must not read
    /// these registers itself — the PPU owns the render gate.
    fn ppu_render_state(&self) -> PpuRenderState {
        PpuRenderState {
            rendering_enabled: self.rendering_enabled(),
            sprite_8x16: self.registers.ctrl.sprite_size_16(),
            fetch_phase: (self.timing.dot % 8) as u8,
        }
    }

    pub fn in_vblank(&self) -> bool {
        self.registers.status.v_blank()
    }

    /// Consume the vblank-entry timing event.
    ///
    /// Returns `true` once per entry into vblank (scanline 241, dot 1)
    /// and resets the internal flag, so `$2002` reads by the emulated
    /// program cannot interfere with frame stepping.
    pub fn take_vblank_entered(&mut self) -> bool {
        std::mem::take(&mut self.vblank_entered)
    }

    pub fn oam_data(&self) -> &[u8; 256] {
        self.oam.as_bytes()
    }

    pub fn write_oam_data(&mut self, value: u8) {
        fn normalize_oam_byte(addr: u8, value: u8) -> u8 {
            if addr & 0x03 == 0x02 {
                value & 0xE3
            } else {
                value
            }
        }

        if self.oam_data_writes_ignored() {
            // Hardware: with background or sprite rendering enabled, $2004 writes
            // on visible lines 0-239 and the pre-render line (261) do not touch
            // OAM. OAMADDR instead does a glitchy +4 that bumps only its high
            // 6 bits, leaving the low 2 bits unchanged (nesdev PPU registers
            // §OAMDATA). `wrapping_add(4)` lands exactly on that mod-64 carry:
            // the low two bits of addr+4 equal the original low two bits. OAM
            // DMA rides this same path via the bus.
            self.oam_addr = self.oam_addr.wrapping_add(4);
            return;
        }

        let addr = self.oam_addr;
        self.oam.set_byte(addr, normalize_oam_byte(addr, value));
        self.oam_addr = addr.wrapping_add(1);
    }

    /// True while `$2004` writes are ignored on hardware: the pre-render
    /// line (261) or visible lines 0-239, provided either background or
    /// sprite rendering is enabled. Outside these windows writes take
    /// normal effect (readable during vblank, for example).
    fn oam_data_writes_ignored(&self) -> bool {
        self.rendering_enabled() && self.timing.in_ppu_active_line()
    }

    #[hotpath::measure(impl_type = "Ppu")]
    pub fn tick(&mut self, cartridge: &mut Cartridge) {
        crate::hotpath_cpu::hotpath_cpu_alias!(
            Self,
            concat!(module_path!(), "::Ppu::tick"),
            "tick"
        );
        self.cycle += 1;

        let dot = self.timing.dot;
        let scanline = self.timing.scanline;
        let rendering = self.rendering_enabled();

        if scanline == 261 && dot == 338 {
            self.ren_latched_at_338 = rendering;
        }
        if dot == 0 {
            if scanline == 0 {
                // New frame: discard splits recorded after the previous
                // flush (vblank/pre-render writes). The live mask already
                // carries those color changes into this frame, and any
                // mid-frame split from here on is recorded fresh. The
                // buffered-pixel cursor likewise restarts, so unscanned
                // beam positions of the new frame report black until the
                // beam writes them.
                self.color_splits.clear();
                self.buffered_pixel_from = self.next_beam_index();
                self.buffered_pixel_count = 0;
            }
            // The secondary-OAM swap runs on every line, including the
            // pre-render line (261). Sprite evaluation itself only runs on
            // visible scanlines 0-239, so scanline 0's swap brings in the
            // empty buffer: no freshly evaluated sprites on line 0.
            self.sprite.swap_secondary_oam();
            if self.timing.in_visible_scanline() {
                // compute background anchor at start of visible scanline
                self.background_anchor = Some(BackgroundActivation::snapshot(self, 0));
                self.pending_background_activation = None;
                let render = self.ppu_render_state();
                self.fill_tile_cache(0, cartridge, &render);
            }
        }
        if self.sprite.status_dirty() {
            self.sprite.update_ctrl_status(&mut self.registers.status);
        }

        if rendering {
            // MMC3 scanline IRQs are driven by filtered PPU A12 rises.
            //
            // On the real NES the PPU address bus toggles A12 on each tile
            // fetch.  The MMC3 filters the rapid bouncing and effectively
            // clocks its IRQ counter once per scanline at the point where A12
            // rises from a sustained low period:
            //
            //   * When BG uses $0xxx and sprites use $1xxx: the rise occurs at
            //     the first sprite pattern fetch (~dot 260).
            //   * When BG uses $1xxx and sprites use $0xxx: the rise occurs at
            //     the first BG pattern fetch (~dot 4), with a second rise at
            //     the BG-prefetch after sprite fetches (~dot 324).
            //
            // We model this by emitting only pattern-table addresses (skipping
            // the nametable/attribute-table fetches that cause the filtered
            // bouncing).  BG pattern addresses are emitted on odd dots and
            // sprite pattern addresses on even dots, matching the real PPU's
            // memory-access phasing.
            //
            // Manual $2006/$2007 writes still reach the mapper directly.
            if self.timing.in_ppu_active_line() {
                // During sprite tile loading (dots 257-320 of visible scanlines and
                // the pre-render line) hardware drives OAMADDR to 0; software
                // polling $2003 mid-frame observes that.
                if (257..=320).contains(&dot) {
                    self.oam_addr = 0;
                }

                // Horizontal bits reload from temp at dot 257
                // (visible scanlines and pre-render scanline)
                if dot == 257 {
                    self.reload_horizontal_from_temp();
                }

                if cartridge.wants_vram_notify() && (dot & 1) == 1 {
                    match dot {
                        3..=256 | 323..=336 => {
                            // Notifications are shifted +2 dots (base 3/323 instead of 1/321)
                            // so that the first BG pattern appears at dot 7 rather than dot 5.
                            // Together with the sprite base (first pattern at dot 263), this
                            // gives the real hardware's 256-dot gap between mode $10 and $08.
                            let base_dot = if dot <= 256 { 3 } else { 323 };
                            // `/ 2 % 4` on an odd dot with a constant base: the
                            // shift-and-mask is the same value without the divide.
                            let fetch_type = (((dot - base_dot) >> 1) & 3) as usize;
                            if fetch_type >= 2 {
                                cartridge.notify_vram_address(
                                    if self.registers.ctrl.background_pattern_table() {
                                        0x1000
                                    } else {
                                        0x0000
                                    },
                                );
                            }
                        }
                        259..=320 => {
                            // Sprite tile fetch: only pattern-table accesses (fetch types 2-3).
                            // Emitted on odd dots to align the MMC3 counter clock with the
                            // observed ~dot-260 hardware timing.
                            let fetch_type = (((dot - 259) >> 1) & 3) as usize;
                            if fetch_type >= 2 {
                                cartridge.notify_vram_address(
                                    if self.registers.ctrl.sprite_size_16()
                                        || self.registers.ctrl.sprite_pattern_table()
                                    {
                                        0x1000
                                    } else {
                                        0x0000
                                    },
                                );
                            }
                        }
                        _ => {}
                    }
                }
            }

            if self.timing.in_visible_area() {
                if dot == 65 {
                    self.sprite.begin_sprite_overflow_eval(self.oam_addr);
                }

                if (dot & 1) == 1 {
                    self.sprite
                        .step_sprite_overflow_eval(scanline, self.registers.ctrl, &self.oam);
                }

                // --- vram_addr management (rendering-enabled only) ---
                // These model the real NES PPU's internal v-register updates:
                // Fine Y increment at dot 256 of each visible scanline
                if dot == 256 {
                    self.increment_vram_y();
                }
            }

            // Vertical bits reload from temp at dots 280-304 of pre-render scanline
            if self.timing.is_prepare_line() && (280..=304).contains(&dot) {
                self.reload_vertical_from_temp();
            }
        }

        // Visible pixels are output on dots 1-256; dot 0 is the idle fetch
        // slot. Batch color (ticket #62): the live per-dot path resolves
        // geometry (TileCache background fetch, sprite evaluation, sprite-0
        // hit) into a palette index only and stores it in the flat buffer
        // at the beam position — no RGB work here. Color (grayscale and
        // emphasis) is applied once, at the flush in `enter_vblank` below.
        // Mid-frame `$2001` color changes are recorded as split points
        // in `write_ppureg` and applied per segment by the flush below;
        // enable bits, by contrast, take effect here immediately.
        if self.timing.is_visible() {
            let x = (dot - 1) as u8;
            let pixel_idx = if rendering {
                self.render_pixel(x, cartridge)
            } else {
                self.palette.disabled_color_index(self.registers.vram_addr)
            };
            let index = scanline as usize * VISIBLE_WIDTH + x as usize;
            self.palette_index_buffer[index] = pixel_idx;
            // Monotonic per frame (ticket #67): a backward timing jump
            // must never lower the cursor, or already-buffered beam
            // positions would wrongly report black ("indices below the
            // count hold this frame's indices"). reset only at frame
            // start / reset above.
            self.buffered_pixel_count = self.buffered_pixel_count.max(index + 1);
        }

        if self.timing.enter_vblank() {
            // Flush the whole frame's buffered palette indices through the
            // Render seam (raster order) before `finish()`, exactly once
            // per frame, at the VBL set dot.
            self.flush_palette_index_buffer();
            self.vblank_entered = true;
            self.renderer.finish();
            if !std::mem::take(&mut self.suppress_vblank_pending) {
                self.registers.status.set_v_blank(true);
            }
        }

        if self.timing.leave_vblank() {
            self.registers.status = PpuStatus::EMPTY;
            self.sprite.clear_pending();
        }

        self.timing.advance(self.ren_latched_at_338);
        cartridge.on_ppu_tick(self.timing.scanline);
    }

    /// Beam index of the next visible pixel to be buffered, for recording
    /// a `$2001` color split. `self.timing` points at the next dot to be
    /// processed (a CPU write lands between ticks, after the previous
    /// dot has advanced), so the next visible pixel is:
    ///
    /// - dot 0 of a visible scanline: `scanline * 256` (its first pixel)
    /// - dots 1..=256 of a visible scanline: `scanline * 256 + (dot - 1)`
    /// - dots 257..=339 of a visible scanline: the first pixel of the
    ///   next scanline, clamped to the end of the frame on scanline 239
    /// - non-visible scanlines (240-261): the end of the frame, so a write
    ///   there affects no buffered pixel of the flushing frame
    ///
    /// For dots 1..=256 this is `scanline*256 + (dot-1)`, exactly the
    /// ticket #63 split formula `scanline*256 + dot` applied to the
    /// *accounted* dot (dot 0 is the idle fetch slot; the buffered index
    /// of the current dot is `dot - 1`). Non-visible scanlines clamp to
    /// the end of the frame: a `$2001` write there cannot colour any
    /// pixel of the frame being flushed, and the next frame starts with
    /// a fresh split list (cleared at scanline 0 dot 0).
    fn next_beam_index(&self) -> usize {
        const FRAME_PIXELS: usize = VISIBLE_WIDTH * VISIBLE_HEIGHT;
        if self.timing.scanline >= 240 {
            return FRAME_PIXELS;
        }
        match self.timing.dot {
            0 => self.timing.scanline as usize * VISIBLE_WIDTH,
            1..=256 => {
                self.timing.scanline as usize * VISIBLE_WIDTH + (self.timing.dot - 1) as usize
            }
            _ => ((self.timing.scanline as usize + 1) * VISIBLE_WIDTH).min(FRAME_PIXELS),
        }
    }

    /// Flush the frame's buffered palette indices through the Render seam
    /// in raster (linear buffer) order, segment-wise by recorded color
    /// splits (ticket #63). Runs once per frame at the VBL set dot, before
    /// `finish()`. The linear scan preserves the raster order the retired
    /// two-dot output pipeline used to provide.
    ///
    /// Each [`ColorSplit`] closes a segment: indices below its `index`
    /// use its `prior` configuration, and the segment starting at `index`
    /// uses the next split's `prior` — or the live mask at flush time for
    /// the final segment. Applying only the final mask to the whole frame
    /// would mis-color every pixel before a mid-frame write. Splits are
    /// consumed here and cleared, so they never leak into the next frame.
    ///
    /// Only beam positions actually scanned this frame
    /// (`buffered_pixel_from..buffered_pixel_count`) hold this frame's
    /// indices. Every other position flushes black, matching the cleared
    /// framebuffer an incomplete frame (e.g. after a mid-frame reset)
    /// would have produced per-dot.
    ///
    fn flush_palette_index_buffer(&mut self) {
        let live = ColorConfig::from_mask(self.registers.mask);
        let frame_len = self.palette_index_buffer.len();
        let valid_start = self.buffered_pixel_from.min(frame_len);
        let valid_end = self.buffered_pixel_count.clamp(valid_start, frame_len);
        // Black here is the zeroed framebuffer pixel `[0, 0, 0, 0]`
        // (not an opaque palette color): that is what a position the
        // beam never reached held in the pre-change per-dot framebuffer.
        for index in 0..valid_start {
            self.emit_pixel(index, Pixel::from_u32(0));
        }
        let split_count = self.color_splits.len();
        let mut start = 0usize;
        for segment in 0..=split_count {
            let (end, config) = if segment < split_count {
                let split = self.color_splits[segment];
                (split.index.min(frame_len), split.prior)
            } else {
                (frame_len, live)
            };
            let seg_start = start;
            // Every flush segment goes through the per-segment 64-entry
            // LUT + batched gather (scalar or SSSE3, identical output).
            // Ticket #67 removed the small-segment bypass: one path, no
            // extra branch, and `perf` shows the flush is not the Hot Path.
            let mut index = seg_start.max(valid_start);
            let seg_end = end.min(valid_end);
            if index < seg_end {
                let lut = batch_convert::build_final_color_lut(&self.color_theme, config);
                let mut chunk = [0u32; 16];
                while index < seg_end {
                    let chunk_len = (seg_end - index).min(chunk.len());
                    batch_convert::convert(
                        &self.palette_index_buffer[index..index + chunk_len],
                        &lut,
                        &mut chunk[..chunk_len],
                    );
                    for (offset, &word) in chunk[..chunk_len].iter().enumerate() {
                        self.emit_pixel(index + offset, Pixel::from_u32(word));
                    }
                    index += chunk_len;
                }
            }
            // Monotonic start: duplicate or out-of-order split indices
            // close empty segments without re-emitting earlier pixels.
            start = start.max(end);
        }
        for index in valid_end..frame_len {
            self.emit_pixel(index, Pixel::from_u32(0));
        }
        self.color_splits.clear();
    }

    /// Emit one flushed pixel: beam index -> `(x, y, color)` through the
    /// Render seam. Single per-pixel emit shared by the LUT path, the
    /// black-fill for unscanned positions, and the segment loop direct
    /// callers (ticket #67).
    fn emit_pixel(&mut self, index: usize, pixel: Pixel) {
        let x = (index % VISIBLE_WIDTH) as u32;
        let y = (index / VISIBLE_WIDTH) as u32;
        self.renderer.set_pixel(x, y, pixel.0);
    }

    /// Resolve one buffered palette index to its final pixel under a
    /// given stored color config. Hardware grayscale ANDs the 6-bit entry
    /// with $30 before the color lookup ($3F black renders as $30 white);
    /// emphasis tints are analog effects on the looked-up RGB. The flush
    /// passes each segment's stored [`ColorConfig`] and the live
    /// brightness path ([`Self::pixel_brightness`]) passes the current
    /// mask's projection, so the two share one resolution point and stay
    /// segment-compatible (ticket #67: no `ColorConfig` -> mask round
    /// trip).
    fn resolve_pixel(&self, palette_index: u8, config: ColorConfig) -> Pixel {
        config.apply_effects(
            self.color_theme
                .color(config.grayscale_index(palette_index)),
        )
    }

    /// R+G+B luminance of the pixel at (`x`, `y`) for Zapper light
    /// sense, served live from the palette-index buffer: the buffered
    /// index is resolved on demand through the palette plus the current
    /// grayscale/emphasis logic, so light is detectable mid-frame,
    /// before the flush materializes the framebuffer. Beam positions
    /// not yet written this frame report black, matching the
    /// framebuffer's behaviour until the flush. Out-of-bounds
    /// coordinates also report black (same as `Render::pixel_brightness`).
    pub fn pixel_brightness(&self, x: u32, y: u32) -> u32 {
        if x >= VISIBLE_WIDTH as u32 || y >= VISIBLE_HEIGHT as u32 {
            return 0;
        }
        let index = y as usize * VISIBLE_WIDTH + x as usize;
        if !(self.buffered_pixel_from..self.buffered_pixel_count).contains(&index) {
            return 0;
        }
        let pixel = self.resolve_pixel(
            self.palette_index_buffer[index],
            ColorConfig::from_mask(self.registers.mask),
        );
        let (r, g, b) = pixel.to_rgb();
        u32::from(r) + u32::from(g) + u32::from(b)
    }

    /// Atomically sampled NMI line for this dot.
    ///
    /// `v_blank && nmi_enable`. A `$2002` read (clears vblank) or a
    /// `$2000` write that disables NMI drops the level on the next dot;
    /// same-dot suppression falls out of the CPU sampler's phase (a 1-dot
    /// assertion at the set dot is never sampled — see `NmiDetector`).
    pub fn nmi_line(&self) -> bool {
        self.registers.status.v_blank() & self.registers.ctrl.nmi_enable()
    }

    pub(crate) fn read_ppureg(&mut self, address: u16, cartridge: &mut Cartridge) -> u8 {
        let reg = normalize_ppu_addr(address);
        match reg {
            0x2002 => {
                let status = self.read_status();
                let status_bits = status.into_bits();
                let result = (status_bits & 0xE0) | (self.current_bus_latch() & 0x1F);
                self.refresh_bus_latch_bits(0xE0, status_bits);
                result
            }
            0x2004 => {
                let result = self.read_oam_data();
                self.refresh_bus_latch(result);
                result
            }
            0x2007 => self.read_vram_and_inc(cartridge),
            _ => self.current_bus_latch(),
        }
    }

    /// Side-effect-free peek of the PPU address space.
    ///
    /// Only `$2000-$3FFF` is valid: PPU registers are mirrored every 8 bytes
    /// (`addr & 0x2007`), so `$2008-$3FFF` resolve to `$2000-$2007`. Every
    /// other range belongs to another bus device, and
    /// [`crate::nes::NesMcu`] routes those itself in
    /// [`Mcu::peek`](crate::mcu::Mcu::peek), so an out-of-range address here
    /// is a caller bug: it asserts in debug builds and reads `0` in release
    /// rather than wrapping into an unrelated register.
    ///
    /// The result approximates what a hardware read would see, minus the side
    /// effects — exact behaviour lives in [`Self::read_ppureg`]:
    ///
    /// - `$2002` returns the status bits without clearing vblank and without
    ///   latching the open-bus low bits.
    /// - `$2007` reads VRAM at the current `vram_addr` directly: no read
    ///   buffer refill, no `PPUCTRL` address increment, no back-to-back ignore
    ///   window, no palette-grayscale override.
    /// - `$2004` goes through the rendering-aware [`Self::read_oam_data`].
    /// - Write-only registers (`$2000`, `$2001`, `$2003`, `$2005`, `$2006`) are
    ///   not readable on hardware; they read `0` here rather than the CPU bus
    ///   latch [`Self::read_ppureg`] returns.
    pub(crate) fn peek(&self, address: u16, cartridge: &Cartridge) -> u8 {
        if !(0x2000..=0x3fff).contains(&address) {
            debug_assert!(
                false,
                "Ppu::peek address {address:#06x} outside $2000-$3FFF"
            );
            return 0;
        }

        let reg = normalize_ppu_addr(address);
        match reg {
            0x2002 => self.registers.status.into_bits(),
            0x2004 => self.read_oam_data(),
            0x2007 => self.read_vram(self.registers.vram_addr, cartridge),
            _ => 0,
        }
    }

    pub(crate) fn write_ppureg(&mut self, address: u16, value: u8, cartridge: &mut Cartridge) {
        self.refresh_bus_latch(value);
        let reg = normalize_ppu_addr(address);
        match reg {
            // PPUCTRL
            0x2000 => {
                let ctrl = PpuCtrl::from_bits(value);
                self.set_control_flags(ctrl);
                // Clearing the NMI enable drops the /NMI line from the
                // next dot's `nmi_line` sample.
                self.schedule_background_activation_if_visible();
            }
            0x2001 => {
                let prior = ColorConfig::from_mask(self.registers.mask);
                let mask = PpuMask::from_bits(value);
                self.registers.mask = mask;
                // Only the color bits are deferred: a write that changes
                // grayscale/emphasis becomes a split point carrying the
                // prior color configuration. The split is the next beam
                // position minus the pixels still in the retired
                // two-dot output stage: the old pipeline colored each
                // pixel with the mask at commit time (two ticks after
                // render), so rendered-but-uncommitted pixels take the
                // new color once the write lands. Two pixels are in
                // flight only while the beam is mid-render (next dots
                // 3..=257); at the line start (0..=1) the previous
                // line is fully committed, single pixels are in flight
                // at dots 2 and 258, and hblank/vblank writes have
                // nothing pending (blargg nmi_sync / dpcmletterbox draw
                // their lines with exactly this landing).
                if ColorConfig::from_mask(mask) != prior {
                    let next = self.next_beam_index();
                    let pending = if self.timing.scanline >= 240 {
                        0
                    } else {
                        match self.timing.dot {
                            0 | 1 => 0,
                            2 | 258 => 1,
                            3..=257 => 2,
                            _ => 0,
                        }
                    };
                    let index = next.saturating_sub(pending);
                    self.color_splits.push(ColorSplit { index, prior });
                }
            }
            // OAMADDR
            0x2003 => {
                self.oam_addr = value;
            }
            // OAMDATA
            0x2004 => {
                self.write_oam_data(value);
            }
            // PPUSCROLL
            0x2005 => self.registers.write_scroll(value),
            // PPUADDR
            0x2006 => {
                self.write_vram_addr(value);
                cartridge.notify_vram_address(self.registers.vram_addr);
            }
            // PPUDATA
            0x2007 => {
                let write_addr = self.registers.vram_addr;
                self.write_vram(write_addr, value, cartridge);
                self.registers
                    .ctrl
                    .inc_ppu_addr(&mut self.registers.vram_addr);
                cartridge.notify_vram_address(self.registers.vram_addr);
                // A pattern-table write changes Sprite plane bytes; drop the
                // cached rows so the next pixel refetches.
                if write_addr < 0x2000 {
                    self.sprite.invalidate_rows();
                }
            }
            _ => {}
        }
    }

    pub fn read_vram(&self, address: u16, cartridge: &Cartridge) -> u8 {
        let addr = normalize_vram_addr(address);
        if addr < 0x3f00 {
            cartridge.read_vram(addr, &self.ppu_render_state())
        } else {
            self.palette.read(addr)
        }
    }

    fn write_vram(&mut self, address: u16, value: u8, cartridge: &mut Cartridge) {
        let addr = normalize_vram_addr(address);
        if addr < 0x3f00 {
            cartridge.write_vram(addr, value);
        } else {
            self.palette.write(addr, value);
        }
    }

    fn write_vram_addr(&mut self, value: u8) {
        if self.registers.write_vram_addr(value) {
            self.schedule_background_activation_if_visible();
        }
    }

    fn get_background_pixel(
        &mut self,
        screen_x: u8,
        cartridge: &Cartridge,
        render: &PpuRenderState,
    ) -> (u8, u8) {
        self.apply_pending_background_activation(screen_x);

        if !self.registers.mask.background_left_enabled() && screen_x < 8 {
            return (0, 0);
        }

        // Refill cache when empty or exhausted
        if self.tile_cache.remaining == 0 {
            self.fill_tile_cache(screen_x, cartridge, render);
        }

        self.tile_cache.consume()
    }

    fn fill_tile_cache(&mut self, screen_x: u8, cartridge: &Cartridge, render: &PpuRenderState) {
        let background = self
            .background_anchor
            .unwrap_or_else(|| BackgroundActivation::snapshot(self, 0));

        let (palette_idx, tile_fine_x, bitplane_low, bitplane_high) =
            fetch_tile_data(&background, screen_x, cartridge, render);

        self.tile_cache = TileCache {
            bitplane_low,
            bitplane_high,
            palette_idx,
            remaining: 8 - tile_fine_x,
        };
    }

    fn read_vram_and_inc(&mut self, cartridge: &mut Cartridge) -> u8 {
        // Two $2007 reads in quick succession (within the PPUDATA ignore
        // window, ~2 CPU cycles — the page-crossing dummy read of
        // `lda abs,X` followed by the real read) make the second read fully
        // ignored on hardware (Mesen2 `NesPpu::Read`): it re-returns the
        // previous read's value with no VRAM access, no address increment,
        // no read-buffer refill and no mapper notification. Ignored reads
        // do not re-arm the window — only a completing read does.
        if let Some((at, value)) = self.ppudata_last_read
            && self.cycle <= at + PPUDATA_IGNORE_WINDOW_DOTS
        {
            return value;
        }

        let vram_addr = self.registers.vram_addr;
        let current = self.read_vram(vram_addr, cartridge);
        self.registers
            .ctrl
            .inc_ppu_addr(&mut self.registers.vram_addr);
        cartridge.notify_vram_address(self.registers.vram_addr);

        // Non-palette addresses use a read buffer (delayed by one read).
        // Palette addresses ($3F00-$3FFF) return immediately, but still
        // update the buffer with the nametable byte "underneath".
        let addr = normalize_vram_addr(vram_addr);
        let result = if addr >= 0x3F00 {
            // Palette: fill buffer with the nametable data underneath
            // (mirrored from $2F00-$2FFF). The returned value carries the
            // grayscale mask ($30) when grayscale is enabled, exactly like
            // the rendering path (Mesen2 `_paletteRamMask`).
            self.registers.ppudata_buffer = self.read_vram(addr - 0x1000, cartridge);
            let masked = self.registers.mask.grayscale_index(current);
            let result = (masked & 0x3F) | (self.current_bus_latch() & 0xC0);
            self.refresh_bus_latch_bits(0x3F, result);
            result
        } else {
            let buffered = self.registers.ppudata_buffer;
            self.registers.ppudata_buffer = current;
            self.refresh_bus_latch(buffered);
            buffered
        };
        self.ppudata_last_read = Some((self.cycle, result));
        result
    }

    fn current_bus_latch(&mut self) -> u8 {
        self.registers.current_bus_latch(self.cycle)
    }

    fn refresh_bus_latch(&mut self, value: u8) {
        self.registers.refresh_bus_latch(value, self.cycle);
    }

    fn refresh_bus_latch_bits(&mut self, mask: u8, value: u8) {
        self.registers
            .refresh_bus_latch_bits(mask, value, self.cycle);
    }

    /// Read OAM data at current OAM address.
    ///
    /// With rendering enabled on visible lines (0-239) the PPU is
    /// mid-evaluation/fetch and $2004 exposes internal traffic instead of
    /// primary OAM (nesdev PPU registers §OAMDATA; Micro Machines relies on
    /// this): dots 1-64 return $FF (secondary-OAM clear phase), dots 65-256
    /// return the evaluation copy buffer, dots 257-320 return secondary OAM
    /// at the fetch position `(dot-257)/8*4 + min((dot-257)%8, 3)`. All other
    /// states (rendering disabled, blanking incl. the pre-render line) keep
    /// the primary-OAM-at-OAMADDR behavior. The eval machine is observed
    /// through a read-only projection (`copy_buffer`/`secondary_byte` take
    /// `&self`); this layer never mutates eval internals.
    fn read_oam_data(&self) -> u8 {
        if self.rendering_enabled() && self.timing.in_visible_scanline() {
            let dot = self.timing.dot;
            if (1..=64).contains(&dot) {
                return 0xFF;
            }
            if self.timing.in_visible_dots() {
                return self.sprite.copy_buffer();
            }
            if (257..=320).contains(&dot) {
                let rel = dot - 257;
                let step = (rel % 8).min(3);
                let index = (rel / 8 * 4 + step) as usize;
                return self.sprite.secondary_byte(index);
            }
        }
        self.oam.get_byte(self.oam_addr)
    }

    /// Set control flags (for testing)
    fn set_control_flags(&mut self, flags: PpuCtrl) {
        self.registers.set_control_flags(flags);
        // The sprite pattern table / sprite size selection lives in ctrl, so
        // the cached Sprite rows no longer match.
        self.sprite.invalidate_rows();
    }

    /// Read status register (for testing) - behaves like reading from 0x2002
    /// Returns the current status and clears the v_blank flag
    fn read_status(&mut self) -> PpuStatus {
        let r = self.registers.status;
        // One PPU clock before the VBL flag would set — hardware dot 0 of
        // scanline 241 — a $2002 read there reads 0 and flag never rises
        // this frame (PPU_frame_timing). At read time `timing` already
        // reflects the *next* dot (PPU tick runs before the CPU read in
        // `NesMachine::tick`), so hardware dot 0 appears as dot==1 here.
        // No edge to retract — the line never asserted.
        if self.timing.scanline == 241 && self.timing.dot == 1 {
            self.suppress_vblank_pending = true;
        }

        // Clear v_blank flag on read; that drops the /NMI line from the
        // next dot's `nmi_line` sample.
        self.registers.status.set_v_blank(false);
        self.registers.write_toggle = false; // Also reset write toggle on status read
        r
    }

    fn render_pixel(&mut self, x: u8, cartridge: &Cartridge) -> u8 {
        let screen_y = self.timing.scanline as u8;
        let render = self.ppu_render_state();
        let (bg_palette_idx, bg_color_idx) = if self.registers.mask.background_enabled() {
            self.get_background_pixel(x, cartridge, &render)
        } else {
            (0, 0)
        };

        let sprite_pixel = if self.registers.mask.sprite_enabled()
            && (x >= 8 || self.registers.mask.sprite_left_enabled())
        {
            self.sprite
                .find_sprite_pixel(self.registers.ctrl, cartridge, x, screen_y, &render)
        } else {
            None
        };

        // All pure reads, so the cheapest discriminators go first: a
        // transparent background pixel or the rightmost column can never
        // set the hit, and the row-cache lookup is skipped for them.
        if bg_color_idx != 0
            && x != 255
            && self.registers.mask.background_enabled()
            && self.registers.mask.sprite_enabled()
            && (x >= 8
                || (self.registers.mask.background_left_enabled()
                    && self.registers.mask.sprite_left_enabled()))
            && !self.registers.status.sprite_zero_hit()
            && self.sprite.sprite_zero_pixel_opaque(x, screen_y)
        {
            self.sprite.set_zero_hit_pending();
        }

        match sprite_pixel {
            Some(sprite_pixel) if bg_color_idx == 0 || !sprite_pixel.behind_bg => self
                .palette
                .get_sprite_color_index(sprite_pixel.palette_idx, sprite_pixel.color_idx),
            _ => self
                .palette
                .get_background_color_index(bg_palette_idx, bg_color_idx),
        }
    }
}

/// Fetch all four tile bytes for background rendering at a given screen pixel position.
///
/// Returns the tile's palette index, the pixel's horizontal offset within the
/// tile, and the two pattern bitplanes.
///
/// The divisions are written as shifts, masks and one range fold: `world_x`
/// stays below 512 so `/ 256` is the top bit and `% 256` the low byte, and
/// `world_y` folds at 240 (a multiple of 8, so the fine-Y low bits survive
/// the fold untouched). All values match the naive arithmetic exactly.
fn fetch_tile_data(
    background: &BackgroundActivation,
    screen_x: u8,
    cartridge: &Cartridge,
    render: &PpuRenderState,
) -> (u8, u8, u8, u8) {
    let coarse_x = background.vram_addr & 0x001F;
    let coarse_y = (background.vram_addr >> 5) & 0x001F;
    let fine_y = (background.vram_addr >> 12) & 0x0007;
    let nt_select = ((background.vram_addr >> 10) & 0x03) as u8;

    let world_x = coarse_x * 8 + background.fine_x as u16 + screen_x as u16;
    let world_y = coarse_y * 8 + fine_y;
    let wrapped_y = if world_y >= 240 {
        world_y - 240
    } else {
        world_y
    };

    let nt_select_x = (((nt_select & 0x01) as u16 + (world_x >> 8)) & 0x01) as u8;
    let nt_select_y = (((nt_select >> 1) as u16 + u16::from(world_y >= 240)) & 0x01) as u8;
    let nt_idx = (nt_select_y << 1) | nt_select_x;

    let nt_x = ((world_x & 0xFF) >> 3) as u8;
    let nt_y = (wrapped_y >> 3) as u8;

    let nt_base = 0x2000 + nt_idx as u16 * 0x0400;
    let nt_addr = nt_base + nt_y as u16 * 32 + nt_x as u16;
    let nt_byte = cartridge.read_vram(nt_addr, render);
    let attr_addr = nt_base + 0x03c0 + (nt_y as u16 >> 2) * 8 + (nt_x as u16 >> 2);
    let attr_byte = cartridge.read_vram(attr_addr, render);

    let shift = (((nt_y >> 1) & 0x01) << 2) | (((nt_x >> 1) & 0x01) << 1);

    let tile_position = background.ctrl.background_tile_position(nt_byte);
    let tile_fine_y = (world_y & 7) as u8;
    let low_addr = tile_position.resolve_pixel_addr(tile_fine_y);
    let bitplane_low = cartridge.read_vram(low_addr.0, render);
    let bitplane_high = cartridge.read_vram(low_addr.second_plane_addr(), render);
    let palette_idx = (attr_byte >> shift) & 0x03;

    (
        palette_idx,
        (world_x & 7) as u8,
        bitplane_low,
        bitplane_high,
    )
}

#[cfg(test)]
mod tests;
