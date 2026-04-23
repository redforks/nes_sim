use crate::nes::mapper::Cartridge;
use crate::render::Render;
use bitfield_struct::bitfield;
use image::Rgba;
use std::fmt::Write;

pub mod pattern;
pub use pattern::{PatternBand, draw_pattern};

type Pixel = Rgba<u8>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PatternAccess {
    Cpu,
    Background,
    Sprite,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct BackgroundTileOverride {
    pub palette_idx: u8,
    pub color_idx: u8,
}

impl PpuMask {
    /// Apply grayscale and emphasis effects to a pixel color using this mask's flags.
    /// - Grayscale: Converts color to grayscale by averaging RGB channels
    /// - Emphasis: Attenuates non-emphasized color channels (e.g., red emphasis darkens G and B)
    fn apply_effects(&self, pixel: Pixel) -> Pixel {
        let Rgba([r, g, b, a]) = pixel;

        // Apply emphasis (color emphasis darkens the non-emphasized channels)
        // Using ~0.75 attenuation factor (192/256)
        let r = if self.red_tint() || (!self.green_tint() && !self.blue_tint()) {
            r
        } else {
            (r as u16 * 192 / 256) as u8
        };
        let g = if self.green_tint() || (!self.red_tint() && !self.blue_tint()) {
            g
        } else {
            (g as u16 * 192 / 256) as u8
        };
        let b = if self.blue_tint() || (!self.red_tint() && !self.green_tint()) {
            b
        } else {
            (b as u16 * 192 / 256) as u8
        };

        // Apply grayscale by averaging RGB channels
        let (r, g, b) = if self.grayscale() {
            // Use luminance weights (ITU-R BT.601): 0.299R + 0.587G + 0.114B
            let gray = (r as u16 * 77 + g as u16 * 150 + b as u16 * 29) / 256;
            (gray as u8, gray as u8, gray as u8)
        } else {
            (r, g, b)
        };

        Rgba([r, g, b, a])
    }
}

#[bitfield(u8)]
struct PpuCtrl {
    // Field declaration is LSB-first (first declared field maps to bit 0).
    // Arrange fields so they match the hardware PPUCTRL layout (bits 0..7):
    // bits 0-1: name_table_select
    // bit 2: increment_mode
    // bit 3: sprite_pattern_table
    // bit 4: background_pattern_table
    // bit 5: sprite_size
    // bit 6: ppu_master
    // bit 7: nmi_enable
    #[bits(2)]
    name_table_select: u8,
    increment_mode: bool,
    sprite_pattern_table: bool,
    background_pattern_table: bool,
    sprite_size: bool,
    ppu_master: bool, // not used in NES, can be ignored
    nmi_enable: bool,
}

impl PpuCtrl {
    fn inc_ppu_addr(self, ppu_addr: &mut u16) {
        *ppu_addr = ppu_addr.wrapping_add(if self.increment_mode() { 32 } else { 1 });
    }
}

#[bitfield(u8)]
struct PpuMask {
    grayscale: bool,
    background_left_enabled: bool,
    sprite_left_enabled: bool,
    background_enabled: bool,
    sprite_enabled: bool,
    red_tint: bool,
    green_tint: bool,
    blue_tint: bool,
}

#[bitfield(u8)]
struct PpuStatus {
    #[bits(5)]
    __: u8,
    sprite_overflow: bool,
    sprite_zero_hit: bool,
    v_blank: bool,
}

const TILES_PER_ROW: u8 = 32;

// PPU Timing Constants
const SCANLINES_PER_FRAME: u16 = 262;
const DOTS_PER_SCANLINE: u16 = 341;
const PPU_OPEN_BUS_DECAY_TICKS: u64 = 3_221_591;
pub const VBLANK_SET_SCANLINE: u16 = 241;
pub const VBLANK_SET_DOT: u16 = 1;
const VBLANK_CLEAR_SCANLINE: u16 = 261;
const VBLANK_CLEAR_DOT: u16 = 1;
const MAX_SPRITES_PER_SCANLINE: usize = 8;

#[derive(Copy, Clone)]
struct SpritePixel {
    palette_idx: u8,
    color_idx: u8,
    behind_bg: bool,
}

#[derive(Copy, Clone)]
struct CachedSprite {
    x: u8,
    palette_idx: u8,
    behind_bg: bool,
    pixels: [u8; 8],
}

impl CachedSprite {
    fn opaque_pixel(&self, screen_x: u8) -> Option<SpritePixel> {
        let sprite_x = screen_x as i16 - self.x as i16;
        if !(0..8).contains(&sprite_x) {
            return None;
        }

        let color_idx = self.pixels[sprite_x as usize];
        if color_idx == 0 {
            return None;
        }

        Some(SpritePixel {
            palette_idx: self.palette_idx,
            color_idx,
            behind_bg: self.behind_bg,
        })
    }
}

fn normalize_oam_byte(addr: u8, value: u8) -> u8 {
    if addr & 0x03 == 0x02 {
        value & 0xE3
    } else {
        value
    }
}

// Scanline cache removed. Sprite pixels and sprite-zero checks are computed
// on-the-fly to simplify state and remove cached arrays.

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
            vram_addr: ppu.vram_addr,
            fine_x: ppu.fine_x,
            ctrl: ppu.ctrl,
        }
    }
}

const fn rgb(v: [u8; 3]) -> Pixel {
    let [r, g, b] = v;
    Rgba::<u8>([r, g, b, 0xff])
}

#[rustfmt::skip]
const COLORS: [Pixel; 64] = [
    // 2C02
    rgb([102, 102, 102]), rgb([0, 42, 136]), rgb([20, 18, 167]), rgb([59, 0, 164]),
    rgb([92, 0, 126]), rgb([110, 0, 64]), rgb([108, 6, 0]), rgb([86, 29, 0]),
    rgb([51, 53, 0]), rgb([11, 72, 0]), rgb([0, 82, 0]), rgb([0, 79, 8]),
    rgb([0, 64, 77]), rgb([0, 0, 0]), rgb([0, 0, 0]), rgb([0, 0, 0]),

    rgb([173, 173, 173]), rgb([21, 95, 217]), rgb([66, 64, 255]), rgb([117, 39, 254]),
    rgb([160, 26, 204]), rgb([183, 30, 123]), rgb([181, 49, 32]), rgb([153, 78, 0]),
    rgb([107, 109, 0]), rgb([56, 135, 0]), rgb([12, 147, 0]), rgb([0, 143, 50]),
    rgb([0, 124, 141]), rgb([0, 0, 0]), rgb([0, 0, 0]), rgb([0, 0, 0]),

    rgb([255, 254, 255]), rgb([100, 176, 255]), rgb([146, 144, 255]), rgb([198, 118, 255]),
    rgb([243, 106, 255]), rgb([254, 110, 204]), rgb([254, 129, 112]), rgb([234, 158, 34]),
    rgb([188, 190, 0]), rgb([136, 216, 0]), rgb([92, 228, 48]), rgb([69, 224, 130]),
    rgb([72, 205, 222]), rgb([79, 79, 79]), rgb([0, 0, 0]), rgb([0, 0, 0]),

    rgb([255, 254, 255]), rgb([192, 223, 255]), rgb([211, 210, 255]), rgb([232, 200, 255]),
    rgb([251, 194, 255]), rgb([254, 196, 234]), rgb([254, 204, 197]), rgb([247, 216, 165]),
    rgb([228, 229, 148]), rgb([207, 239, 150]), rgb([189, 244, 171]), rgb([179, 243, 204]),
    rgb([179, 235, 242]), rgb([184, 184, 184]), rgb([0, 0, 0]), rgb([0, 0, 0]),
];

#[derive(Default)]
struct PaletteRam {
    data: [u8; 0x20],
}

impl PaletteRam {
    fn read(&self, address: u16) -> u8 {
        self.data[Self::index(address)]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.data[Self::index(address)] = value;
    }

    fn index(addr: u16) -> usize {
        let addr = addr & 0x1f;
        if let 0x10 | 0x14 | 0x18 | 0x1C = addr {
            (addr & 0xf) as usize
        } else {
            addr as usize
        }
    }

    fn get_color(&self, start: u8, palette_idx: u8, idx: u8) -> Pixel {
        let offset = if idx == 0 {
            0
        } else {
            (start | idx | (palette_idx << 2)) as usize
        };
        COLORS[self.data[offset] as usize]
    }

    fn get_background_color(&self, palette_idx: u8, idx: u8) -> Pixel {
        self.get_color(0, palette_idx, idx)
    }

    fn get_sprit_color(&self, palette_idx: u8, idx: u8) -> Pixel {
        self.get_color(0x10, palette_idx, idx)
    }
}

pub struct Ppu<R: Render = ()> {
    ctrl: PpuCtrl,
    status: PpuStatus,
    palette: PaletteRam, // palette memory

    suppress_vblank_for_current_frame: bool,
    suppress_nmi_for_current_frame: bool,

    mask: PpuMask,

    oam_addr: u8,
    oam_data: [u8; 0x100], // object attribute memory

    // VRAM address registers per NES PPU: v (current), t (temporary), x (fine X), w (write toggle)
    vram_addr: u16,      // v - current VRAM address
    temp_vram_addr: u16, // t - temporary VRAM address
    fine_x: u8,          // x - fine X scroll (3 bits)
    write_toggle: bool,  // w - first/second write toggle
    renderer: R,

    // PPUDATA read buffer: non-palette VRAM reads are delayed by one read
    ppudata_buffer: u8,

    // PPU open bus / data latch: last value placed on the PPU data bus.
    // Reading write-only registers returns this value.
    bus_latch: u8,
    bus_latch_decay_deadlines: [u64; 8],

    // PPU timing
    scanline: u16, // 0-261
    dot: u16,      // 0-340
    background_anchor: Option<BackgroundActivation>,
    pending_background_activation: Option<BackgroundActivation>,
    odd_frame: bool,
    sprite_zero_hit_pending: bool,
    sprite_overflow_pending: bool,
    sprite_overflow_eval: SpriteOverflowEval,

    /// PPU mask register changes are not visible until the next PPU tick;
    /// this delayed copy models the 1-dot internal pipeline delay.
    effective_mask: PpuMask,
    rendering_enabled_at_scanline_start: bool,

    frame_no: usize,
    ppu_ticks: u64,
}

/// PPU registers are mirrored every 8 bytes in range $2000-$3FFF
/// this function normalized $2000, $2008, .. $3ff8 to $2000
fn normalize_ppu_addr(addr: u16) -> u16 {
    addr & 0x2007
}

impl<R: Render> Ppu<R> {
    pub fn new(renderer: R) -> Self {
        Ppu {
            ctrl: PpuCtrl::new(),
            mask: PpuMask::new(),
            status: PpuStatus::new(),
            oam_addr: 0,
            oam_data: [0; 0x100],

            palette: PaletteRam::default(),

            vram_addr: 0,
            temp_vram_addr: 0,
            fine_x: 0,
            write_toggle: false,
            renderer,
            ppudata_buffer: 0,
            bus_latch: 0,
            bus_latch_decay_deadlines: [0; 8],
            scanline: 0,
            dot: 0,
            background_anchor: None,
            pending_background_activation: None,
            odd_frame: false,
            sprite_zero_hit_pending: false,
            sprite_overflow_pending: false,
            sprite_overflow_eval: SpriteOverflowEval::default(),
            suppress_vblank_for_current_frame: false,
            suppress_nmi_for_current_frame: false,
            // no scanline cache
            frame_no: 0,
            ppu_ticks: 0,
            effective_mask: PpuMask::new(),
            rendering_enabled_at_scanline_start: false,
        }
    }

    pub fn reset(&mut self) {
        // https://www.nesdev.org/wiki/PPU_power_up_state
        self.ctrl = PpuCtrl::new();
        self.status = PpuStatus::new();
        self.mask = PpuMask::new();
        self.effective_mask = PpuMask::new();

        self.vram_addr = 0;
        self.temp_vram_addr = 0;
        self.fine_x = 0;
        self.write_toggle = false;
        self.ppudata_buffer = 0;
        self.bus_latch = 0;
        self.bus_latch_decay_deadlines = [0; 8];
        self.background_anchor = None;
        self.pending_background_activation = None;
        self.odd_frame = false;
        self.sprite_zero_hit_pending = false;
        self.sprite_overflow_pending = false;
        self.sprite_overflow_eval = SpriteOverflowEval::default();
        self.ppu_ticks = 0;
        // cache-free: nothing to mark
        self.write_scroll(0);
        self.rendering_enabled_at_scanline_start = false;
    }

    pub fn timing(&self) -> (u16, u16) {
        (self.scanline, self.dot)
    }

    pub fn frame_no(&self) -> usize {
        self.frame_no
    }

    pub fn renderer(&self) -> &R {
        &self.renderer
    }

    fn scroll_xy(&self) -> (u16, u16) {
        let scroll_addr = self.temp_vram_addr;
        let scroll_x = ((scroll_addr & 0x001f) << 3) | self.fine_x as u16;
        let scroll_y = (((scroll_addr >> 5) & 0x001f) << 3) | ((scroll_addr >> 12) & 0x0007);
        (scroll_x, scroll_y)
    }

    fn schedule_background_activation_if_visible(&mut self) {
        if self.rendering_enabled()
            && let Some(screen_x) = self.next_visible_x()
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

    fn next_visible_x(&self) -> Option<u8> {
        if self.scanline < 240 && (1..=256).contains(&self.dot) {
            Some((self.dot - 1) as u8)
        } else {
            None
        }
    }

    /// Increment fine Y in vram_addr. Called at dot 256 of each visible scanline
    /// when rendering is enabled. Wraps fine Y → coarse Y → nametable Y toggle.
    fn increment_vram_y(&mut self) {
        let fine_y = (self.vram_addr >> 12) & 0x07;
        if fine_y < 7 {
            self.vram_addr += 0x1000; // increment fine Y
        } else {
            self.vram_addr &= !0x7000; // clear fine Y
            let mut coarse_y = (self.vram_addr >> 5) & 0x1F;
            if coarse_y == 29 {
                coarse_y = 0;
                self.vram_addr ^= 0x0800; // toggle nametable Y
            } else if coarse_y == 31 {
                coarse_y = 0; // wrap without toggling nametable
            } else {
                coarse_y += 1;
            }
            self.vram_addr = (self.vram_addr & !0x03E0) | (coarse_y << 5);
        }
    }

    /// Copy horizontal scroll bits (coarse X + nametable X) from temp to vram.
    /// Called at dot 257 of each visible/pre-render scanline when rendering is enabled.
    fn reload_horizontal_from_temp(&mut self) {
        // Horizontal bits: coarse X (bits 0-4) and nametable X (bit 10)
        self.vram_addr = (self.vram_addr & !0x041F) | (self.temp_vram_addr & 0x041F);
    }

    /// Copy vertical scroll bits (coarse Y + nametable Y + fine Y) from temp to vram.
    /// Called at dots 280-304 of the pre-render scanline when rendering is enabled.
    fn reload_vertical_from_temp(&mut self) {
        // Vertical bits: coarse Y (bits 5-9), nametable Y (bit 11), fine Y (bits 12-14)
        self.vram_addr = (self.vram_addr & !0x7BE0) | (self.temp_vram_addr & 0x7BE0);
    }

    pub fn dump_state(&self, cartridge: &Cartridge) -> String {
        let mut out = String::new();

        let _ = writeln!(out, "# PPU State");
        let _ = writeln!(out, "- Frame: {}", self.frame_no);
        let _ = writeln!(
            out,
            "- Timing: scanline {}, dot {}",
            self.scanline, self.dot
        );
        let cur_name_table_addr = 0x2000 + (self.ctrl.name_table_select() as u16 * 0x400);
        let _ = writeln!(
            out,
            "- Current nametable address: 0x{:04X}",
            cur_name_table_addr
        );
        let _ = writeln!(out, "- PPUCTRL: 0x{:02X}", self.ctrl.into_bits());
        let _ = writeln!(out, "- PPUMASK: 0x{:02X}", self.mask.into_bits());
        let _ = writeln!(out, "- PPUSTATUS: 0x{:02X}", self.status.into_bits());
        let _ = writeln!(out, "- OAMADDR: 0x{:02X}", self.oam_addr);
        let _ = writeln!(out, "- VRAM address: 0x{:04X}", self.vram_addr);
        let _ = writeln!(
            out,
            "- Temporary VRAM address: 0x{:04X}",
            self.temp_vram_addr
        );
        let _ = writeln!(out, "- Fine X: 0x{:02X}", self.fine_x);
        let _ = writeln!(out, "- Write toggle: {}", self.write_toggle);
        let _ = writeln!(out, "- Scroll X/Y: {:?}", self.scroll_xy());

        let _ = writeln!(out);

        // Pattern tables (CHR data) - both 0x0000 and 0x1000 banks
        let _ = writeln!(out, "## Pattern Tables");
        for base in [0x0000u16, 0x1000u16] {
            let _ = writeln!(out, "### 0x{:04X}", base);
            // Print 16 bytes per row (0x1000 / 16 = 256 rows)
            for row in 0..0x1000u16 / 16 {
                let addr = base + row * 16;
                let _ = write!(out, "\n0x{:04X}: ", addr);
                for col in 0..16u16 {
                    let value = cartridge.read_chr(addr + col, PatternAccess::Cpu);
                    let _ = write!(out, "{:02X} ", value);
                }
                let _ = writeln!(out);
                let _ = writeln!(out);
            }
            let _ = writeln!(out);
        }

        let _ = writeln!(out, "## Nametable");
        for base in [0x2000u16, 0x2400, 0x2800, 0x2C00] {
            let _ = writeln!(out, "### 0x{:04X}", base);
            for row in 0..0x0400u16 / 16 {
                let addr = base + row * 16;
                let _ = write!(out, "\n0x{:04X}: ", addr);
                for col in 0..16u16 {
                    let value = cartridge.read_nametable(addr + col);
                    let _ = write!(out, "{:02X} ", value);
                }
                let _ = writeln!(out);
                let _ = writeln!(out);
            }
            let _ = writeln!(out);
        }

        let _ = writeln!(out, "## Palette RAM");
        for row in 0..2u16 {
            let addr = 0x3F00 + row * 16;
            let _ = write!(out, "0x{:04X}: ", addr);
            for col in 0..16u16 {
                let value = self.palette.data[(row * 16 + col) as usize];
                let _ = write!(out, "{:02X} ", value);
            }
            let _ = writeln!(out);
        }

        let _ = writeln!(out);
        let _ = writeln!(out, "## OAM");
        let _ = writeln!(out, "- OAMADDR: 0x{:02X}", self.oam_addr);
        for row in 0..16u16 {
            let addr = row * 16;
            let _ = write!(out, "0x{:04X}: ", addr);
            for col in 0..16u16 {
                let value = self.oam_data[(row * 16 + col) as usize];
                let _ = write!(out, "{:02X} ", value);
            }
            let _ = writeln!(out);
        }

        out
    }

    pub fn rendering_enabled(&self) -> bool {
        self.effective_mask.background_enabled() || self.effective_mask.sprite_enabled()
    }

    pub fn in_vblank(&self) -> bool {
        self.status.v_blank()
    }

    pub fn oam_dma(&mut self, vals: &[u8; 256]) {
        for (addr, value) in vals.iter().copied().enumerate() {
            self.oam_data[addr] = normalize_oam_byte(addr as u8, value);
        }
        // cache-free: nothing to mark
    }

    pub fn tick(&mut self, cartridge: &mut Cartridge) {
        self.ppu_ticks = self.ppu_ticks.wrapping_add(1);
        let rendering_enabled = self.rendering_enabled();

        if self.dot == 0 {
            self.rendering_enabled_at_scanline_start = rendering_enabled;
        }

        if self.sprite_zero_hit_pending {
            self.status.set_sprite_zero_hit(true);
            self.sprite_zero_hit_pending = false;
        }

        if self.sprite_overflow_pending {
            self.status.set_sprite_overflow(true);
            self.sprite_overflow_pending = false;
        }

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
        if rendering_enabled
            && (self.scanline < 240
                || (self.scanline == 261 && self.rendering_enabled_at_scanline_start))
        {
            let fetch_addr = if ((1..=256).contains(&self.dot) || (321..=336).contains(&self.dot))
                && self.dot % 2 == 1
            {
                // BG tile fetch: only pattern-table accesses (fetch types 2-3).
                // Notifications are shifted +2 dots (base 3/323 instead of 1/321)
                // so that the first BG pattern appears at dot 7 rather than dot 5.
                // Together with the sprite base (first pattern at dot 263), this
                // gives the real hardware's 256-dot gap between mode $10 and $08.
                let base_dot = if self.dot <= 256 { 3 } else { 323 };
                if self.dot < base_dot {
                    None
                } else {
                    let fetch_type = (((self.dot - base_dot) / 2) % 4) as usize;
                    if fetch_type >= 2 {
                        Some(if self.ctrl.background_pattern_table() {
                            0x1000
                        } else {
                            0x0000
                        })
                    } else {
                        None
                    }
                }
            } else if (259..=320).contains(&self.dot) && self.dot % 2 == 1 {
                // Sprite tile fetch: only pattern-table accesses (fetch types 2-3).
                // Emitted on odd dots to align the MMC3 counter clock with the
                // observed ~dot-260 hardware timing.
                let fetch_type = (((self.dot - 259) / 2) % 4) as usize;
                if fetch_type >= 2 {
                    Some(
                        if self.ctrl.sprite_size() || self.ctrl.sprite_pattern_table() {
                            0x1000
                        } else {
                            0x0000
                        },
                    )
                } else {
                    None
                }
            } else {
                None
            };

            if let Some(fetch_addr) = fetch_addr {
                cartridge.notify_vram_address(fetch_addr);
            }
        }

        if self.scanline < 240 && self.dot == 0 {
            // compute background anchor at start of visible scanline
            self.background_anchor = Some(BackgroundActivation::snapshot(self, 0));
            self.pending_background_activation = None;
        }

        if self.scanline < 240 && self.dot == 65 {
            self.begin_sprite_overflow_eval();
        }

        if rendering_enabled
            && self.scanline < 240
            && (65..=256).contains(&self.dot)
            && self.dot % 2 == 1
        {
            self.step_sprite_overflow_eval();
        }

        // Visible pixels are output on dots 1-256; dot 0 is the idle fetch slot.
        if self.scanline < 240 && (1..=256).contains(&self.dot) {
            let x = (self.dot - 1) as u8;
            let pixel = if rendering_enabled {
                self.render_pixel(x, cartridge)
            } else {
                let vram_addr = self.vram_addr % 0x4000;
                if (0x3f00..0x4000).contains(&vram_addr) {
                    COLORS[self.palette.read(vram_addr) as usize]
                } else {
                    COLORS[self.palette.read(0x3f00) as usize]
                }
            };
            let pixel = self.effective_mask.apply_effects(pixel);
            self.renderer
                .set_pixel(x as u32, self.scanline as u32, pixel.0);
        }

        // --- vram_addr management (rendering-enabled only) ---
        // These model the real NES PPU's internal v-register updates:
        if rendering_enabled {
            // Fine Y increment at dot 256 of each visible scanline
            if self.scanline < 240 && self.dot == 256 {
                self.increment_vram_y();
            }
            // Horizontal bits reload from temp at dot 257
            // (visible scanlines and pre-render scanline)
            if (self.scanline < 240 || self.scanline == VBLANK_CLEAR_SCANLINE) && self.dot == 257 {
                self.reload_horizontal_from_temp();
            }
            // Vertical bits reload from temp at dots 280-304 of pre-render scanline
            if self.scanline == VBLANK_CLEAR_SCANLINE && (280..=304).contains(&self.dot) {
                self.reload_vertical_from_temp();
            }
        }

        // At end of pre-render scanline (261), clear screen for next frame
        if rendering_enabled && self.scanline == VBLANK_CLEAR_SCANLINE && self.dot == 0 {
            let bg_color = COLORS[self.palette.data[0] as usize];
            self.renderer.clear(bg_color.0);
        }

        if self.scanline == VBLANK_SET_SCANLINE && self.dot == VBLANK_SET_DOT {
            self.renderer.finish();
            if !self.suppress_vblank_for_current_frame {
                self.status.set_v_blank(true);
            }
        }

        // VBlank clear: pre-render scanline 261, dot 1
        if self.scanline == VBLANK_CLEAR_SCANLINE && self.dot == VBLANK_CLEAR_DOT {
            self.status.set_v_blank(false);
            self.status.set_sprite_overflow(false);
            self.status.set_sprite_zero_hit(false);
            self.sprite_zero_hit_pending = false;
            self.sprite_overflow_pending = false;
        }

        // On odd frames with rendering enabled, the pre-render scanline skips the
        // final idle dot and wraps directly to scanline 0 dot 0.
        if self.scanline == VBLANK_CLEAR_SCANLINE
            && self.dot == DOTS_PER_SCANLINE - 2
            && self.odd_frame
            && rendering_enabled
        {
            self.dot = 0;
            self.scanline = 0;
            self.odd_frame = !self.odd_frame;
            self.suppress_vblank_for_current_frame = false;
            self.suppress_nmi_for_current_frame = false;
            self.frame_no += 1;
            return;
        }

        // Advance dot/scanline counters
        self.dot += 1;
        if self.dot >= DOTS_PER_SCANLINE {
            self.dot = 0;
            self.scanline += 1;
            if self.scanline >= SCANLINES_PER_FRAME {
                self.scanline = 0;
                self.odd_frame = !self.odd_frame;
                self.suppress_vblank_for_current_frame = false;
                self.suppress_nmi_for_current_frame = false;
                self.frame_no += 1;
            }
        }

        self.effective_mask = self.mask;
    }

    /// Return ppu nmi signal, it connect to Cpu nmi input line
    pub fn nmi_line_out(&self) -> bool {
        self.status.v_blank() && self.ctrl.nmi_enable() && !self.suppress_nmi_for_current_frame
    }

    /// Read by cpu memory bus. Uses Cartridge for CHR/name-table access.
    pub fn read(&mut self, address: u16, cartridge: &mut Cartridge) -> u8 {
        // PPU registers are mirrored every 8 bytes in range $2000-$3FFF
        let reg = normalize_ppu_addr(address);
        match reg {
            // PPUSTATUS: bits 7-5 from status, bits 4-0 from bus latch
            0x2002 => {
                let status = self.read_status();
                self.write_toggle = false;
                let status_bits = status.into_bits();
                let result = (status_bits & 0xE0) | (self.current_bus_latch() & 0x1F);
                self.refresh_bus_latch_bits(0xE0, status_bits);
                result
            }
            // OAMDATA
            0x2004 => {
                let result = self.read_oam_data();
                self.refresh_bus_latch(result);
                result
            }
            // PPUDATA - read from VRAM or palette
            0x2007 => self.read_vram_and_inc(cartridge),
            // Other registers are write-only: return open bus (latch)
            _ => self.current_bus_latch(),
        }
    }

    pub fn peek(&self, address: u16, cartridge: &Cartridge) -> u8 {
        let reg = normalize_ppu_addr(address);
        match reg {
            0x2002 => self.status.into_bits(),
            0x2004 => self.read_oam_data(),
            0x2007 => self.read_vram(self.vram_addr, cartridge),
            _ => 0,
        }
    }

    /// Write by cpu memory bus. Uses Cartridge for CHR/name-table writes.
    pub fn write(&mut self, address: u16, value: u8, cartridge: &mut Cartridge) {
        self.refresh_bus_latch(value);
        let reg = normalize_ppu_addr(address);
        match reg {
            // PPUCTRL
            0x2000 => {
                cartridge.on_ppu_ctrl_write(value);
                self.set_control_flags(PpuCtrl::from_bits(value));
                self.schedule_background_activation_if_visible();
                // Update name table selection
            }
            // PPUMASK
            0x2001 => {
                cartridge.on_ppu_mask_write(value);
                self.mask = PpuMask::from_bits(value);
                // mask changed; no cache to mark
            }
            // OAMADDR
            0x2003 => {
                self.oam_addr = value;
            }
            // OAMDATA
            0x2004 => {
                let addr = self.oam_addr as usize;
                self.oam_data[addr] = normalize_oam_byte(self.oam_addr, value);
                self.oam_addr = self.oam_addr.wrapping_add(1);
                // OAM mutated; no cache to mark
            }
            // PPUSCROLL
            0x2005 => {
                cartridge.on_ppu_scroll_write(value);
                self.write_scroll(value);
                // scroll changed; no cache to mark
            }
            // PPUADDR
            0x2006 => {
                self.write_vram_addr(value);
                // vram addr updated; no cache to mark
                cartridge.notify_vram_address(self.vram_addr);
            }
            // PPUDATA
            0x2007 => {
                self.write_vram(self.vram_addr, value, cartridge);
                self.ctrl.inc_ppu_addr(&mut self.vram_addr);
                cartridge.notify_vram_address(self.vram_addr);
                // VRAM written; no cache to mark
            }
            _ => {} // Ignore other addresses
        }
    }

    fn read_vram(&self, address: u16, cartridge: &Cartridge) -> u8 {
        let mut addr = address % 0x4000;
        if (0x3000..0x3f00).contains(&addr) {
            addr -= 0x1000;
        }
        if addr < 0x3f00 {
            // Read from pattern table or name table
            if addr < 0x2000 {
                // Pattern table (CHR ROM/RAM)
                cartridge.read_chr(addr, PatternAccess::Cpu)
            } else {
                // Name table
                self.read_name_table_byte(addr, cartridge)
            }
        } else {
            // Palette
            self.palette.read(addr)
        }
    }

    fn write_vram(&mut self, address: u16, value: u8, cartridge: &mut Cartridge) {
        let mut addr = address % 0x4000;
        if (0x3000..0x3f00).contains(&addr) {
            addr -= 0x1000;
        }
        if addr < 0x3f00 {
            if addr >= 0x2000 {
                // Name table (pattern table 0x0000-0x1FFF is read-only for CHR ROM)
                cartridge.write_nametable(addr, value);
            } else {
                cartridge.write_pattern(addr, value);
            }
        } else {
            self.palette.write(addr, value);
        }
    }

    fn write_scroll(&mut self, value: u8) {
        if !self.write_toggle {
            // First write - X scroll
            self.fine_x = value & 0x07;
            self.temp_vram_addr = (self.temp_vram_addr & 0xFFE0) | (value as u16 >> 3);
            self.write_toggle = true;
        } else {
            // Second write - Y scroll
            self.temp_vram_addr = (self.temp_vram_addr & !0x73E0)
                | (((value as u16 & 0x07) << 12) & 0x7000)
                | ((value as u16 >> 3) << 5);
            self.write_toggle = false;
        }
    }

    fn write_vram_addr(&mut self, value: u8) {
        if !self.write_toggle {
            // First write - high byte
            self.temp_vram_addr = (self.temp_vram_addr & 0x00FF) | ((value as u16 & 0x3F) << 8);
            self.write_toggle = true;
        } else {
            // Second write - low byte
            self.temp_vram_addr = (self.temp_vram_addr & 0x7F00) | (value as u16 & 0x00FF);
            self.vram_addr = self.temp_vram_addr;
            self.schedule_background_activation_if_visible();
            self.write_toggle = false;
        }
    }

    fn read_name_table_byte(&self, address: u16, cartridge: &Cartridge) -> u8 {
        cartridge.read_nametable(address)
    }

    fn read_pattern_pixel(
        cartridge: &Cartridge,
        base_addr: u16,
        tile_idx: u8,
        tile_x: usize,
        tile_y: usize,
        access: PatternAccess,
    ) -> u8 {
        let tile_addr = base_addr + tile_idx as u16 * 16;
        let low = cartridge.read_chr(tile_addr + tile_y as u16, access);
        let high = cartridge.read_chr(tile_addr + tile_y as u16 + 8, access);
        let bit = 7 - tile_x;
        ((low >> bit) & 1) | (((high >> bit) & 1) << 1)
    }

    fn get_background_pixel(&mut self, cartridge: &Cartridge, screen_x: u8) -> (u8, u8) {
        self.apply_pending_background_activation(screen_x);

        // Check left-column clipping
        if !self.effective_mask.background_left_enabled() && screen_x < 8 {
            return (0, 0);
        }

        // On real hardware, the PPU fetches tiles from the position encoded in
        // vram_addr (the "v" register), which is maintained by fine-Y increments
        // and horizontal/vertical reloads from temp_vram_addr (the "t" register).
        let background = self
            .background_anchor
            .unwrap_or_else(|| BackgroundActivation::snapshot(self, 0));

        let coarse_x = (background.vram_addr & 0x001F) as u16;
        let coarse_y = ((background.vram_addr >> 5) & 0x001F) as u16;
        let fine_y = ((background.vram_addr >> 12) & 0x0007) as usize;
        let nt_select = ((background.vram_addr >> 10) & 0x03) as u8;

        // Background fetches are pipelined ahead of the pixel currently being output.
        let world_x = coarse_x * 8
            + background.fine_x as u16
            + screen_x.saturating_sub(background.screen_x) as u16;
        // Vertical: directly from vram_addr (no screen_y addition; vram_addr is
        // incremented each scanline by the fine-Y increment at dot 256)
        let world_y = coarse_y * 8 + fine_y as u16;

        let nt_select_x = (((nt_select & 0x01) as u16) + (world_x / 256)) & 0x01;
        let nt_select_y = (((nt_select >> 1) as u16) + (world_y / 240)) & 0x01;
        let nt_idx = ((nt_select_y << 1) | nt_select_x) as u8;

        let nt_x = ((world_x % 256) / 8) as u8;
        let nt_y = ((world_y % 240) / 8) as u8;
        let tile_fine_x = (world_x % 8) as usize;
        let tile_fine_y = (world_y % 8) as usize;

        let nt_base = 0x2000 + nt_idx as u16 * 0x0400;
        let nt_addr = nt_base + nt_y as u16 * TILES_PER_ROW as u16 + nt_x as u16;
        let tile_idx = self.read_name_table_byte(nt_addr, cartridge);
        let attr_addr = nt_base + 0x03c0 + (nt_y as u16 / 4) * 8 + (nt_x as u16 / 4);
        let attr_byte = self.read_name_table_byte(attr_addr, cartridge);
        let shift = (((nt_y >> 1) & 0x01) << 2) | (((nt_x >> 1) & 0x01) << 1);
        let palette_idx = (attr_byte >> shift) & 0x03;

        let screen_y = self.scanline as u8;
        if let Some(override_pixel) = cartridge.background_override(
            screen_x,
            screen_y,
            nt_addr,
            tile_idx,
            palette_idx,
            tile_fine_x,
            tile_fine_y,
        ) {
            return (override_pixel.palette_idx, override_pixel.color_idx);
        }

        let base_addr = if background.ctrl.background_pattern_table() {
            0x1000
        } else {
            0x0000
        };
        let color_idx = Self::read_pattern_pixel(
            cartridge,
            base_addr,
            tile_idx,
            tile_fine_x,
            tile_fine_y,
            PatternAccess::Background,
        );

        (palette_idx, color_idx)
    }

    fn sprite_height(&self) -> i16 {
        if self.ctrl.sprite_size() { 16 } else { 8 }
    }

    fn sprite_in_range(&self, y_byte: u8, target_scanline: u16) -> bool {
        let top = y_byte as i16 + 1;
        let sprite_y = target_scanline as i16 - top;
        (0..self.sprite_height()).contains(&sprite_y)
    }

    fn sprite_covers_scanline(&self, sprite_idx: usize, screen_y: u8) -> bool {
        let byte_idx = sprite_idx * 4;
        self.sprite_in_range(self.oam_data[byte_idx], screen_y as u16)
    }

    fn begin_sprite_overflow_eval(&mut self) {
        self.sprite_overflow_eval = SpriteOverflowEval {
            scanline: self.scanline,
            target_scanline: self.scanline + 1,
            oam_index: 0,
            byte_index: 0,
            visible_sprites: 0,
            mode: SpriteOverflowEvalMode::ScanY,
        };
    }

    fn step_sprite_overflow_eval(&mut self) {
        if self.sprite_overflow_eval.scanline != self.scanline {
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
                let y_byte = self.oam_data[oam_index * 4];
                if self.sprite_in_range(y_byte, target_scanline) {
                    self.sprite_overflow_eval.visible_sprites += 1;
                    if self.sprite_overflow_eval.visible_sprites > MAX_SPRITES_PER_SCANLINE {
                        self.sprite_overflow_pending = true;
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
                let y_byte = self.oam_data[byte_idx];
                if self.sprite_in_range(y_byte, target_scanline) {
                    self.sprite_overflow_pending = true;
                    self.sprite_overflow_eval.mode = SpriteOverflowEvalMode::Done;
                } else {
                    self.sprite_overflow_eval.oam_index += 1;
                    self.sprite_overflow_eval.byte_index = (byte_index + 1) & 0x03;
                }
            }
        }
    }

    /// Find the top-most opaque sprite pixel at (screen_x, screen_y) if any.
    fn find_sprite_pixel(
        &self,
        cartridge: &Cartridge,
        screen_x: u8,
        screen_y: u8,
    ) -> Option<SpritePixel> {
        if !self.effective_mask.sprite_enabled() {
            return None;
        }

        if !self.effective_mask.sprite_left_enabled() && screen_x < 8 {
            return None;
        }

        let sprite_height = self.sprite_height();
        let mut visible_sprites: [CachedSprite; MAX_SPRITES_PER_SCANLINE] = [CachedSprite {
            x: 0,
            palette_idx: 0,
            behind_bg: false,
            pixels: [0; 8],
        };
            MAX_SPRITES_PER_SCANLINE];
        let mut visible_count = 0usize;

        for sprite_idx in 0..64 {
            if !self.sprite_covers_scanline(sprite_idx, screen_y) {
                continue;
            }

            let byte_idx = sprite_idx * 4;
            let y = self.oam_data[byte_idx].wrapping_add(1);
            let tile_idx = self.oam_data[byte_idx + 1];
            let attributes = self.oam_data[byte_idx + 2];
            let x = self.oam_data[byte_idx + 3];
            let sprite_y = screen_y as i16 - y as i16;
            let flip_vertical = (attributes & 0x80) != 0;
            let flip_horizontal = (attributes & 0x40) != 0;
            let palette_idx = attributes & 0x03;
            let behind_bg = (attributes & 0x20) != 0;
            let mut pixels = [0u8; 8];

            for pixel_x in 0..8i16 {
                let src_x = if flip_horizontal {
                    7 - pixel_x
                } else {
                    pixel_x
                } as usize;
                let src_y = if flip_vertical {
                    sprite_height - 1 - sprite_y
                } else {
                    sprite_y
                } as u8;

                let color_idx = if self.ctrl.sprite_size() {
                    let pattern_table_idx = (tile_idx & 0x01) as u16;
                    let tile_base = tile_idx & 0xFE;
                    let tile_offset = src_y / 8;
                    let tile_row = (src_y % 8) as usize;
                    Self::read_pattern_pixel(
                        cartridge,
                        pattern_table_idx * 0x1000,
                        tile_base.wrapping_add(tile_offset),
                        src_x,
                        tile_row,
                        PatternAccess::Sprite,
                    )
                } else {
                    let pattern_table_idx = if self.ctrl.sprite_pattern_table() {
                        0x1000
                    } else {
                        0
                    };
                    Self::read_pattern_pixel(
                        cartridge,
                        pattern_table_idx,
                        tile_idx,
                        src_x,
                        src_y as usize,
                        PatternAccess::Sprite,
                    )
                };

                pixels[pixel_x as usize] = color_idx;
            }

            let sprite = CachedSprite {
                x,
                palette_idx,
                behind_bg,
                pixels,
            };

            if sprite_idx == 0 {
                // if sprite 0 covers this x, and is opaque, it should be detected by opaque_pixel below
            }

            if visible_count < MAX_SPRITES_PER_SCANLINE {
                visible_sprites[visible_count] = sprite;
                visible_count += 1;
            }
        }

        // Debug: optionally print visible sprite info
        if std::env::var("NES_SIM_DEBUG").is_ok() {
            eprintln!(
                "find_sprite_pixel: screen=({},{}), visible_count={}",
                screen_x, screen_y, visible_count
            );
        }

        for sprite in visible_sprites.iter().take(visible_count) {
            if let Some(pixel) = sprite.opaque_pixel(screen_x) {
                if std::env::var("NES_SIM_DEBUG").is_ok() {
                    eprintln!(
                        " -> found sprite pixel pal={} col={} behind={}",
                        sprite.palette_idx,
                        sprite.pixels[(screen_x as i16 - sprite.x as i16) as usize],
                        sprite.behind_bg
                    );
                }
                return Some(pixel);
            }
        }

        None
    }

    /// Return whether sprite 0 is opaque at given screen position.
    fn sprite_zero_opaque_at(&self, cartridge: &Cartridge, screen_x: u8, screen_y: u8) -> bool {
        // Find sprite 0's properties and check if it covers and is opaque at x
        let byte_idx = 0usize * 4;
        let y_byte = self.oam_data[byte_idx];
        if !self.sprite_in_range(y_byte, screen_y as u16) {
            return false;
        }

        let tile_idx = self.oam_data[byte_idx + 1];
        let attributes = self.oam_data[byte_idx + 2];
        let x = self.oam_data[byte_idx + 3];
        let y = y_byte.wrapping_add(1);
        let sprite_y = screen_y as i16 - y as i16;
        let flip_vertical = (attributes & 0x80) != 0;
        let flip_horizontal = (attributes & 0x40) != 0;

        // Compute color index for this sprite 0 pixel
        let src_x_i16 = screen_x as i16 - x as i16;
        if !(0..8).contains(&src_x_i16) {
            return false;
        }
        let src_x = if flip_horizontal {
            (7 - src_x_i16) as usize
        } else {
            src_x_i16 as usize
        };
        let src_y = if flip_vertical {
            self.sprite_height() - 1 - sprite_y
        } else {
            sprite_y
        } as u8;

        let color_idx = if self.ctrl.sprite_size() {
            let pattern_table_idx = (tile_idx & 0x01) as u16;
            let tile_base = tile_idx & 0xFE;
            let tile_offset = src_y / 8;
            let tile_row = (src_y % 8) as usize;
            Self::read_pattern_pixel(
                cartridge,
                pattern_table_idx * 0x1000,
                tile_base.wrapping_add(tile_offset),
                src_x,
                tile_row,
                PatternAccess::Sprite,
            )
        } else {
            let pattern_table_idx = if self.ctrl.sprite_pattern_table() {
                0x1000
            } else {
                0
            };
            Self::read_pattern_pixel(
                cartridge,
                pattern_table_idx,
                tile_idx,
                src_x,
                src_y as usize,
                PatternAccess::Sprite,
            )
        };

        color_idx != 0
    }

    fn read_vram_and_inc(&mut self, cartridge: &mut Cartridge) -> u8 {
        let vram_addr = self.vram_addr;
        let current = self.read_vram(vram_addr, cartridge);
        self.ctrl.inc_ppu_addr(&mut self.vram_addr);
        cartridge.notify_vram_address(self.vram_addr);

        // Non-palette addresses use a read buffer (delayed by one read).
        // Palette addresses ($3F00-$3FFF) return immediately, but still
        // update the buffer with the nametable byte "underneath".
        let addr = vram_addr % 0x4000;
        if addr >= 0x3F00 {
            // Palette: fill buffer with the nametable data underneath
            // (mirrored from $2F00-$2FFF)
            self.ppudata_buffer = self.read_vram(vram_addr - 0x1000, cartridge);
            let result = (current & 0x3F) | (self.current_bus_latch() & 0xC0);
            self.refresh_bus_latch_bits(0x3F, result);
            result
        } else {
            let buffered = self.ppudata_buffer;
            self.ppudata_buffer = current;
            self.refresh_bus_latch(buffered);
            buffered
        }
    }

    fn current_bus_latch(&mut self) -> u8 {
        self.apply_bus_decay();
        self.bus_latch
    }

    fn apply_bus_decay(&mut self) {
        for bit in 0..8 {
            let mask = 1 << bit;
            let deadline = self.bus_latch_decay_deadlines[bit];
            if (self.bus_latch & mask) != 0 && deadline != 0 && self.ppu_ticks >= deadline {
                self.bus_latch &= !mask;
                self.bus_latch_decay_deadlines[bit] = 0;
            }
        }
    }

    fn refresh_bus_latch(&mut self, value: u8) {
        self.apply_bus_decay();
        self.bus_latch = value;
        for bit in 0..8 {
            let mask = 1 << bit;
            self.bus_latch_decay_deadlines[bit] = if (value & mask) != 0 {
                self.ppu_ticks.wrapping_add(PPU_OPEN_BUS_DECAY_TICKS)
            } else {
                0
            };
        }
    }

    fn refresh_bus_latch_bits(&mut self, mask: u8, value: u8) {
        self.apply_bus_decay();
        for bit in 0..8 {
            let bit_mask = 1 << bit;
            if (mask & bit_mask) == 0 {
                continue;
            }

            if (value & bit_mask) != 0 {
                self.bus_latch |= bit_mask;
                self.bus_latch_decay_deadlines[bit] =
                    self.ppu_ticks.wrapping_add(PPU_OPEN_BUS_DECAY_TICKS);
            } else {
                self.bus_latch &= !bit_mask;
                self.bus_latch_decay_deadlines[bit] = 0;
            }
        }
    }

    /// Read OAM data at current OAM address (for testing)
    fn read_oam_data(&self) -> u8 {
        self.oam_data[self.oam_addr as usize]
    }

    /// Set control flags (for testing)
    fn set_control_flags(&mut self, flags: PpuCtrl) {
        self.ctrl = flags;
        self.temp_vram_addr =
            (self.temp_vram_addr & !0x0C00) | ((self.ctrl.name_table_select() as u16) << 10);
        // control flags changed; no cache to mark
    }

    /// Read status register (for testing) - behaves like reading from 0x2002
    /// Returns the current status and clears the v_blank flag
    fn read_status(&mut self) -> PpuStatus {
        let r = self.status;
        if self.scanline == VBLANK_SET_SCANLINE {
            match self.dot {
                1 => {
                    self.suppress_vblank_for_current_frame = true;
                    self.suppress_nmi_for_current_frame = true;
                }
                2..=4 => {
                    self.suppress_nmi_for_current_frame = true;
                }
                _ => {}
            }
        }
        // Clear v_blank flag on read
        self.status.set_v_blank(false);
        self.write_toggle = false; // Also reset write toggle on status read
        r
    }

    fn render_pixel(&mut self, x: u8, cartridge: &mut Cartridge) -> Pixel {
        // Compute background pixel on-the-fly (no cache) so that mid-scanline
        // register writes ($2000, $2001, $2005, $2006) take effect immediately.
        let (bg_palette_idx, bg_color_idx) = if self.effective_mask.background_enabled() {
            self.get_background_pixel(cartridge, x)
        } else {
            (0, 0) // transparent
        };

        let sprite_pixel = if self.effective_mask.sprite_enabled() {
            // compute sprite pixel on-the-fly for this x and current scanline
            self.find_sprite_pixel(cartridge, x, self.scanline as u8)
        } else {
            None
        };

        if self.effective_mask.background_enabled()
            && self.effective_mask.sprite_enabled()
            && bg_color_idx != 0
            && x != 255
            && (x >= 8 || self.effective_mask.background_left_enabled())
            && (x >= 8 || self.effective_mask.sprite_left_enabled())
            && self.sprite_zero_opaque_at(cartridge, x, self.scanline as u8)
        {
            self.sprite_zero_hit_pending = true;
        }

        match sprite_pixel {
            Some(sprite_pixel) => {
                if bg_color_idx == 0 || !sprite_pixel.behind_bg {
                    // Background is transparent OR sprite has priority
                    self.palette
                        .get_sprit_color(sprite_pixel.palette_idx, sprite_pixel.color_idx)
                } else {
                    // Background has priority and is opaque
                    self.palette
                        .get_background_color(bg_palette_idx, bg_color_idx)
                }
            }
            None => {
                // No sprite, just show background
                self.palette
                    .get_background_color(bg_palette_idx, bg_color_idx)
            }
        }
    }
}

#[cfg(test)]
mod tests;
