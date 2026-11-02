mod oam;
pub mod palette;
mod registers;
mod sprite;

use crate::{
    interrupt::NmiLines,
    nes::{
        mapper::Cartridge,
        ppu::{palette::ColorTheme, sprite::SpriteManager},
    },
    render::Render,
};
use arraydeque::ArrayDeque;
use oam::Oam;
use palette::{Palette, Pixel};
use registers::{PpuCtrl, PpuMask, PpuStatus, Registers};

// Pixel, palette data and COLORS are defined in the submodule `palette`.

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
    /// Set when a racing access must retract an NMI edge the machine latched
    /// earlier in the same tick (see `NesMachine::tick`).
    nmi_race_cancel: bool,

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
    /// Two-dot pixel output pipeline: pixels rendered on visible dots wait
    /// here (due cycle, target coordinates, palette index) before committing
    /// to the renderer, so CPU register writes landing in between color them.
    /// Commit is due-cycle driven: a scanline's final two pixels commit on
    /// that scanline's own dots +2 — not on the next line's first pushes —
    /// so a $2001 write in the fetch/idle phase cannot retro-recolor them
    /// (DPCM Letterbox depends on this).
    ///
    /// A fixed 8-slot `ArrayDeque` (no heap): pushes happen at most once
    /// per dot and dues rise strictly with `cycle`, so at most two pixels
    /// are ever in flight and the buffer can never fill.
    pixel_pipeline: ArrayDeque<(u64, u8, u16, u8), 8>,
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
            nmi_race_cancel: false,
            pixel_pipeline: ArrayDeque::new(),
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
        // A reset landing mid-frame must not let already-rendered pixels of
        // the old frame commit into the fresh frame's framebuffer.
        self.pixel_pipeline.clear();
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
            // The secondary-OAM swap runs on every line, including the
            // pre-render line (261). Sprite evaluation itself only runs on
            // visible scanlines 0-239, so scanline 0's swap brings in the
            // empty buffer: no freshly evaluated sprites on line 0.
            self.sprite.swap_secondary_oam();
            if self.timing.in_visible_scanline() {
                // compute background anchor at start of visible scanline
                self.background_anchor = Some(BackgroundActivation::snapshot(self, 0));
                self.pending_background_activation = None;
                self.fill_tile_cache(0, cartridge);
            }
        }
        if self.sprite.status_dirty() {
            self.sprite.update_ctrl_status(&mut self.registers.status);
        }

        if rendering {
            let in_active = self.timing.in_ppu_active_line();
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
            if in_active && cartridge.wants_vram_notify() && (dot & 1) == 1 {
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

            if self.timing.in_visible_area() {
                if dot == 65 {
                    self.sprite.begin_sprite_overflow_eval(self.oam_addr);
                }

                if (dot & 1) == 1 {
                    self.sprite
                        .step_sprite_overflow_eval(scanline, self.registers.ctrl, &self.oam);
                }
            }

            // During sprite tile loading (dots 257-320 of visible scanlines and
            // the pre-render line) hardware drives OAMADDR to 0; software
            // polling $2003 mid-frame observes that.
            if in_active && (257..=320).contains(&dot) {
                self.oam_addr = 0;
            }

            // --- vram_addr management (rendering-enabled only) ---
            // These model the real NES PPU's internal v-register updates:
            // Fine Y increment at dot 256 of each visible scanline
            if dot == 256 && self.timing.in_visible_scanline() {
                self.increment_vram_y();
            }
            // Horizontal bits reload from temp at dot 257
            // (visible scanlines and pre-render scanline)
            if dot == 257 && in_active {
                self.reload_horizontal_from_temp();
            }
            // Vertical bits reload from temp at dots 280-304 of pre-render scanline
            if self.timing.is_prepare_line() && (280..=304).contains(&dot) {
                self.reload_vertical_from_temp();
            }
        }

        // Visible pixels are output on dots 1-256; dot 0 is the idle fetch
        // slot. Pixels leave a two-dot output pipeline: a pixel rendered on
        // dot X commits to the framebuffer on dot X+2, after any CPU
        // register write that landed in between. This is the hardware
        // output-stage latency — grayscale and emphasis tint are applied at
        // commit time (the color-modifying stage), so a mid-scanline $2001
        // write takes effect two dots earlier than the write cycle itself
        // (blargg nmi_sync draws its line with exactly this landing).
        if self.timing.is_visible() {
            let x = (dot - 1) as u8;
            let pixel_idx = if rendering {
                self.render_pixel(x, cartridge)
            } else {
                self.palette.disabled_color_index(self.registers.vram_addr)
            };
            self.pixel_pipeline
                .push_back((self.cycle + 2, x, scanline, pixel_idx))
                .ok();
        }
        while self
            .pixel_pipeline
            .front()
            .is_some_and(|(due, ..)| *due <= self.cycle)
        {
            if let Some((_, x, y, pixel_idx)) = self.pixel_pipeline.pop_front() {
                // Hardware grayscale ANDs the 6-bit palette entry with $30
                // before the color lookup ($3F black renders as $30 white);
                // emphasis tints are analog effects on the looked-up RGB.
                let pixel_idx = self.registers.mask.grayscale_index(pixel_idx);
                let pixel = self
                    .registers
                    .mask
                    .apply_effects(self.color_theme.color(pixel_idx));
                self.renderer.set_pixel(x as u32, y as u32, pixel.0);
            }
        }

        if self.timing.enter_vblank() {
            // Timing event for frame stepping: reported even when the $2002
            // VBL flag set itself is suppressed by the dot-0 race, so
            // `process_frame` always terminates at the frame boundary.
            self.vblank_entered = true;
            self.renderer.finish();
            if !std::mem::take(&mut self.suppress_vblank_pending) {
                self.registers.status.set_v_blank(true);
            }
        }

        if self.timing.leave_vblank() {
            self.registers.status.set_v_blank(false);
            self.registers.status.set_sprite_overflow(false);
            self.registers.status.set_sprite_zero_hit(false);
            self.sprite.clear_pending();
        }

        self.timing.advance(self.ren_latched_at_338);
        cartridge.on_ppu_tick(self.timing.scanline);
    }

    /// Atomically sampled NMI line bundle for this dot.
    ///
    /// `level = v_blank && nmi_enable`; `race_cancel` retracts an edge the
    /// machine latched earlier in the same tick, because a `$2002` read
    /// (clears vblank) or a `$2000` write that disables NMI dropped the
    /// line after that edge was sampled.
    /// The two are returned together so a racing site cannot emit
    /// `race_cancel` without its `level` — the flag is never exposed alone.
    pub fn nmi_lines(&mut self) -> NmiLines {
        let level = self.registers.status.v_blank() && self.registers.ctrl.nmi_enable();
        let race_cancel = std::mem::take(&mut self.nmi_race_cancel);
        NmiLines { level, race_cancel }
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
    pub fn peek(&self, address: u16, cartridge: &Cartridge) -> u8 {
        match address {
            0x2000..=0x3fff => {
                let reg = normalize_ppu_addr(address);
                match reg {
                    0x2002 => self.registers.status.into_bits(),
                    0x2004 => self.read_oam_data(),
                    0x2007 => self.read_vram(self.registers.vram_addr, cartridge),
                    _ => 0,
                }
            }
            0x4100..=0xffff => cartridge.read(address),
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
                // Clearing the NMI enable drops the /NMI line for this dot.
                // If the machine already latched an edge earlier in this
                // tick, retract it.
                if !ctrl.nmi_enable() {
                    self.nmi_race_cancel = true;
                }
                self.schedule_background_activation_if_visible();
            }
            0x2001 => {
                self.registers.mask = PpuMask::from_bits(value);
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
            cartridge.read_vram(addr)
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

    fn get_background_pixel(&mut self, screen_x: u8, cartridge: &Cartridge) -> (u8, u8) {
        self.apply_pending_background_activation(screen_x);

        if !self.registers.mask.background_left_enabled() && screen_x < 8 {
            return (0, 0);
        }

        // Refill cache when empty or exhausted
        if self.tile_cache.remaining == 0 {
            self.fill_tile_cache(screen_x, cartridge);
        }

        self.tile_cache.consume()
    }

    fn fill_tile_cache(&mut self, screen_x: u8, cartridge: &Cartridge) {
        let background = self
            .background_anchor
            .unwrap_or_else(|| BackgroundActivation::snapshot(self, 0));

        let (palette_idx, tile_fine_x, bitplane_low, bitplane_high) =
            fetch_tile_data(&background, screen_x, cartridge);

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

        // Clear v_blank flag on read. That also drops the /NMI line for this
        // dot, so retract any edge the machine latched earlier in this tick.
        self.registers.status.set_v_blank(false);
        self.nmi_race_cancel = true;
        self.registers.write_toggle = false; // Also reset write toggle on status read
        r
    }

    fn render_pixel(&mut self, x: u8, cartridge: &Cartridge) -> u8 {
        let screen_y = self.timing.scanline as u8;
        let (bg_palette_idx, bg_color_idx) = if self.registers.mask.background_enabled() {
            self.get_background_pixel(x, cartridge)
        } else {
            (0, 0)
        };

        let sprite_pixel = if self.registers.mask.sprite_enabled()
            && (x >= 8 || self.registers.mask.sprite_left_enabled())
        {
            self.sprite
                .find_sprite_pixel(self.registers.ctrl, cartridge, x, screen_y)
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
    let tile_fine_y = (world_y & 7) as u8;

    let nt_base = 0x2000 + nt_idx as u16 * 0x0400;
    let nt_addr = nt_base + nt_y as u16 * 32 + nt_x as u16;
    let nt_byte = cartridge.read_vram(nt_addr);
    let attr_addr = nt_base + 0x03c0 + (nt_y as u16 >> 2) * 8 + (nt_x as u16 >> 2);
    let attr_byte = cartridge.read_vram(attr_addr);

    let shift = (((nt_y >> 1) & 0x01) << 2) | (((nt_x >> 1) & 0x01) << 1);
    let palette_idx = (attr_byte >> shift) & 0x03;

    let tile_position = background.ctrl.background_tile_position(nt_byte);
    let low_addr = tile_position.resolve_pixel_addr(tile_fine_y);
    let bitplane_low = cartridge.read_vram(low_addr.0);
    let bitplane_high = cartridge.read_vram(low_addr.second_plane_addr());

    (
        palette_idx,
        (world_x & 7) as u8,
        bitplane_low,
        bitplane_high,
    )
}

#[cfg(test)]
mod tests;
