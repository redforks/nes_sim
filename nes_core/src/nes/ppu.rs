use crate::ines::NametableArrangement;
use crate::nes::mapper::Cartridge;
use crate::render::Render;
use bitfield_struct::bitfield;
use image::Rgba;

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

/// Apply grayscale and emphasis effects to a pixel color.
/// - Grayscale: Converts color to grayscale by averaging RGB channels
/// - Emphasis: Attenuates non-emphasized color channels (e.g., red emphasis darkens G and B)
fn apply_effects(
    pixel: Pixel,
    grayscale: bool,
    red_tint: bool,
    green_tint: bool,
    blue_tint: bool,
) -> Pixel {
    let Rgba([r, g, b, a]) = pixel;

    // Apply emphasis (color emphasis darkens the non-emphasized channels)
    // Using ~0.75 attenuation factor (192/256)
    let r = if red_tint || (!green_tint && !blue_tint) {
        r
    } else {
        (r as u16 * 192 / 256) as u8
    };
    let g = if green_tint || (!red_tint && !blue_tint) {
        g
    } else {
        (g as u16 * 192 / 256) as u8
    };
    let b = if blue_tint || (!red_tint && !green_tint) {
        b
    } else {
        (b as u16 * 192 / 256) as u8
    };

    // Apply grayscale by averaging RGB channels
    let (r, g, b) = if grayscale {
        // Use luminance weights (ITU-R BT.601): 0.299R + 0.587G + 0.114B
        let gray = (r as u16 * 77 + g as u16 * 150 + b as u16 * 29) / 256;
        (gray as u8, gray as u8, gray as u8)
    } else {
        (r, g, b)
    };

    Rgba([r, g, b, a])
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

const PALETTE_MEM_START: u16 = 0x3f00;
const NAME_TABLE_MEM_START: u16 = 0x2000;
const TILES_PER_ROW: u8 = 32;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Mirroring {
    LowerBank, // single screen use lower bank
    UpperBank, // single screen use upper bank
    Horizontal,
    Vertical,
    Four,
}

impl From<NametableArrangement> for Mirroring {
    fn from(value: NametableArrangement) -> Self {
        match value {
            // Vertical arrangement requires Horizontal mirrored
            NametableArrangement::Vertical => Self::Horizontal,
            NametableArrangement::Horizontal => Self::Vertical,
        }
    }
}

struct NameTableControl {
    mem: [u8; 4096],
    band_start_offset: [u16; 4],
    mirroring: Mirroring,
}

impl Default for NameTableControl {
    fn default() -> Self {
        Self {
            mem: [0; 4096],
            band_start_offset: [0, 1024, 2048, 3072],
            mirroring: Mirroring::Four,
        }
    }
}

impl NameTableControl {
    fn mirroring(&self) -> Mirroring {
        self.mirroring
    }

    fn set_mirroring(&mut self, mirroring: Mirroring) {
        if mirroring == self.mirroring {
            return;
        }

        self.mirroring = mirroring;
        self.band_start_offset = match mirroring {
            Mirroring::LowerBank => [0, 0, 0, 0],
            Mirroring::UpperBank => [1024, 1024, 1024, 1024],
            Mirroring::Vertical => [0, 1024, 0, 1024],
            Mirroring::Horizontal => [0, 0, 1024, 1024],
            Mirroring::Four => [0, 1024, 2048, 3072],
        };
    }

    fn offset(&self, addr: u16) -> usize {
        let r = addr - NAME_TABLE_MEM_START;
        (self.band_start_offset[r as usize / 1024] + r % 1024) as usize
    }

    fn read(&mut self, address: u16) -> u8 {
        self.mem[self.offset(address)]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.mem[self.offset(address)] = value;
    }
}

// PPU Timing Constants
const SCANLINES_PER_FRAME: u16 = 262;
const DOTS_PER_SCANLINE: u16 = 341;
const VBLANK_SET_SCANLINE: u16 = 241;
const VBLANK_CLEAR_SCANLINE: u16 = 261;
const VBLANK_CLEAR_DOT: u16 = 1;
const MAX_SPRITES_PER_SCANLINE: usize = 8;

#[derive(Copy, Clone)]
struct SpritePixel {
    palette_idx: u8,
    color_idx: u8,
    behind_bg: bool,
}

#[derive(Copy, Clone, Default)]
struct CachedBackgroundPixel {
    palette_idx: u8,
    color_idx: u8,
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

struct ScanlineCache {
    scanline: u8,
    dirty: bool,
    background: [CachedBackgroundPixel; 256],
    sprite_pixels: [Option<SpritePixel>; 256],
    sprite_zero_pixels: [bool; 256],
}

impl Default for ScanlineCache {
    fn default() -> Self {
        Self {
            scanline: 0,
            dirty: true,
            background: [CachedBackgroundPixel::default(); 256],
            sprite_pixels: [None; 256],
            sprite_zero_pixels: [false; 256],
        }
    }
}

const fn rgb(v: [u8; 3]) -> Pixel {
    let [r, g, b] = v;
    Rgba::<u8>([r, g, b, 0xff])
}

#[rustfmt::skip]
const COLORS: [Pixel; 64] = [
    rgb([117, 117, 117]), rgb([39, 27, 143]), rgb([0, 0, 171]), rgb([71, 0, 159]),
    rgb([143, 0, 119]), rgb([171, 0, 19]), rgb([167, 0, 0]), rgb([127, 11, 0]),
    rgb([67, 47, 0]), rgb([0, 71, 0]), rgb([0, 81, 0]), rgb([0, 63, 23]),
    rgb([27, 63, 95]), rgb([0, 0, 0]), rgb([0, 0, 0]), rgb([0, 0, 0]),

    rgb([188, 188, 188]), rgb([0, 115, 239]), rgb([35, 59, 239]), rgb([131, 0, 243]),
    rgb([191, 0, 191]), rgb([231, 0, 91]), rgb([219, 43, 0]), rgb([203, 79, 15]),
    rgb([139, 115, 0]), rgb([0, 151, 0]), rgb([0, 171, 0]), rgb([0, 147, 59]),
    rgb([0, 131, 139]), rgb([0, 0, 0]), rgb([0, 0, 0]), rgb([0, 0, 0]),

    rgb([255, 255, 255]), rgb([63, 191, 255]), rgb([95, 151, 255]), rgb([167, 139, 253]),
    rgb([247, 123, 255]), rgb([255, 119, 183]), rgb([255, 119, 99]), rgb([255, 155, 59]),
    rgb([243, 191, 63]), rgb([131, 211, 19]), rgb([79, 223, 75]), rgb([88, 248, 152]),
    rgb([0, 235, 219]), rgb([0, 0, 0]), rgb([0, 0, 0]), rgb([0, 0, 0]),

    rgb([255, 255, 255]), rgb([171, 231, 255]), rgb([199, 215, 255]), rgb([215, 203, 255]),
    rgb([255, 199, 255]), rgb([255, 199, 219]), rgb([255, 191, 179]), rgb([255, 219, 171]),
    rgb([255, 231, 163]), rgb([227, 255, 163]), rgb([171, 243, 191]), rgb([179, 255, 207]),
    rgb([159, 255, 243]), rgb([0, 0, 0]), rgb([0, 0, 0]), rgb([0, 0, 0]),
];

#[derive(Default)]
struct PaletteRam {
    data: [u8; 0x20],
}

impl PaletteRam {
    fn read(&mut self, address: u16) -> u8 {
        self.data[self.get_addr(address)]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.data[self.get_addr(address)] = value;
    }

    fn get_addr(&self, addr: u16) -> usize {
        // NES palette mirroring:
        // 0x3f00-0x3f0f: background palette
        // 0x3f10-0x3f1f: sprite palette, mirrors to 0x3f00-0x3f0f
        // 0x3f20-0x3fff: mirror down to 0x3f00-0x3f1f

        // First, mirror addresses >= 0x3f20
        let addr = if addr >= 0x3f20 {
            ((addr - 0x3f20) % 32) + 0x3f00
        } else {
            addr
        };

        // Then, handle sprite palette mirroring (0x3f10-0x3f1f -> 0x3f00-0x3f0f)
        // NOTE: Only addresses 0x3f10, 0x3f14, 0x3f18, 0x3f1c actually mirror
        // Other addresses in 0x3f10-0x3f1f range are not used on real hardware
        let addr = match addr {
            0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => addr - 0x10,
            _ => addr,
        };

        // Convert to index in the 32-byte palette array
        let addr = addr - PALETTE_MEM_START;

        // Clamp to valid range [0, 31]
        let addr = addr as usize;
        if addr >= 32 { 31 } else { addr }
    }

    fn get_color_idx(&self, start: usize, palette_idx: u8, idx: u8) -> Pixel {
        let offset = if idx == 0 {
            0
        } else {
            start
                + idx as usize
                + match palette_idx {
                    0 => 0x00,
                    1 => 0x04,
                    2 => 0x08,
                    3 => 0x0c,
                    _ => unreachable!(),
                }
        };
        COLORS[self.data[offset] as usize]
    }

    fn get_background_color(&self, palette_idx: u8, idx: u8) -> Pixel {
        self.get_color_idx(0, palette_idx, idx)
    }

    fn get_sprit_color(&self, palette_idx: u8, idx: u8) -> Pixel {
        self.get_color_idx(0x10, palette_idx, idx)
    }
}

pub struct Ppu<R: Render = ()> {
    ctrl: PpuCtrl,
    status: PpuStatus,
    name_table: NameTableControl, // name table
    cur_name_table_addr: u16,     // current active name table start address
    palette: PaletteRam,          // palette memory
    cur_pattern_table_idx: u8,    // index of current active pattern table, 0 or 1

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

    // PPU timing
    scanline: u16, // 0-261
    dot: u16,      // 0-340
    odd_frame: bool,

    scanline_cache: ScanlineCache,
    /// set to PPUMask register changes render_enabled state, but ppu see it at the next tick,
    /// so can not defineds on `self.mask`
    effective_render_enabled: bool,

    frame_no: usize,
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

            name_table: NameTableControl::default(),
            cur_name_table_addr: 0x2000,
            palette: PaletteRam::default(),
            cur_pattern_table_idx: 0,
            vram_addr: 0,
            temp_vram_addr: 0,
            fine_x: 0,
            write_toggle: false,
            renderer,
            scanline: 0,
            dot: 0,
            odd_frame: false,
            suppress_vblank_for_current_frame: false,
            suppress_nmi_for_current_frame: false,
            scanline_cache: ScanlineCache::default(),
            frame_no: 0,
            effective_render_enabled: false,
        }
    }

    pub fn reset(&mut self) {
        // https://www.nesdev.org/wiki/PPU_power_up_state
        self.ctrl = PpuCtrl::new();
        self.status = PpuStatus::new();
        self.mask = PpuMask::new();
        self.cur_pattern_table_idx = 0;
        self.vram_addr = 0;
        self.temp_vram_addr = 0;
        self.fine_x = 0;
        self.write_toggle = false;
        self.odd_frame = false;
        self.scanline_cache.dirty = true;
        self.write_scroll(0);
    }

    pub fn timing(&self) -> (u16, u16) {
        (self.scanline, self.dot)
    }

    pub fn rendering_enabled(&self) -> bool {
        self.effective_render_enabled
    }

    pub fn oam_dma(&mut self, vals: &[u8; 256]) {
        self.oam_data.copy_from_slice(vals);
        self.scanline_cache.dirty = true;
    }

    pub fn tick(&mut self, cartridge: &Cartridge) {
        let rendering_enabled = self.rendering_enabled();

        if self.scanline < 240 && self.dot == 0 {
            self.prepare_scanline_cache(cartridge, self.scanline as u8);
        }

        // Render pixel during visible scanlines (0-239) and visible dots (0-255)
        if rendering_enabled && self.scanline < 240 && self.dot < 256 {
            let pixel = self.render_pixel(cartridge, self.dot as u8, self.scanline as u8);
            self.renderer
                .set_pixel(self.dot as u32, self.scanline as u32, pixel.0);
        }

        // At end of pre-render scanline (261), clear screen for next frame
        if rendering_enabled && self.scanline == VBLANK_CLEAR_SCANLINE && self.dot == 0 {
            let bg_color = COLORS[self.palette.data[0] as usize];
            self.renderer.clear(bg_color.0);
        }

        // Call finish() at end of visible area (scanline 240, dot 0)
        if rendering_enabled && self.scanline == 240 && self.dot == 0 {
            self.renderer.finish();
        }

        if self.scanline == VBLANK_SET_SCANLINE
            && self.dot == 1
            && !self.suppress_vblank_for_current_frame
        {
            self.status.set_v_blank(true);
        }

        // VBlank clear: pre-render scanline 261, dot 1
        if self.scanline == VBLANK_CLEAR_SCANLINE && self.dot == VBLANK_CLEAR_DOT {
            self.status.set_v_blank(false);
            self.status.set_sprite_overflow(false);
            self.status.set_sprite_zero_hit(false);
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

        self.effective_render_enabled =
            self.mask.background_enabled() || self.mask.sprite_enabled();
    }

    /// Return ppu nmi signal, it connect to Cpu nmi input line
    pub fn nmi_line_out(&self) -> bool {
        self.status.v_blank() && self.ctrl.nmi_enable() && !self.suppress_nmi_for_current_frame
    }

    /// Return ppu vblank status
    pub fn vblank(&self) -> bool {
        self.status.v_blank()
    }

    pub fn set_mirroring(&mut self, mirroring: Mirroring) {
        self.name_table.set_mirroring(mirroring);
    }

    pub fn mirroring(&self) -> Mirroring {
        self.name_table.mirroring()
    }

    /// Read by cpu memory bus. Uses Cartridge for CHR/name-table access.
    pub fn read(&mut self, address: u16, cartridge: &mut Cartridge) -> u8 {
        // PPU registers are mirrored every 8 bytes in range $2000-$3FFF
        let reg = normalize_ppu_addr(address);
        match reg {
            // PPUSTATUS
            0x2002 => {
                let status = self.read_status();
                self.write_toggle = false;
                status.into_bits()
            }
            // OAMDATA
            0x2004 => self.read_oam_data(),
            // PPUDATA - read from VRAM or palette
            0x2007 => {
                // create closures that forward to cartridge
                self.read_vram_and_inc(cartridge)
            }
            _ => 0, // Other registers are write-only
        }
    }

    /// Write by cpu memory bus. Uses Cartridge for CHR/name-table writes.
    pub fn write(&mut self, address: u16, value: u8, cartridge: &mut Cartridge) {
        let reg = normalize_ppu_addr(address);
        match reg {
            // PPUCTRL
            0x2000 => {
                cartridge.on_ppu_ctrl_write(value);
                self.set_control_flags(PpuCtrl::from_bits(value));
                // Update name table address from control bits
                self.cur_name_table_addr = 0x2000 + (self.ctrl.name_table_select() as u16 * 0x400);
                self.scanline_cache.dirty = true;
            }
            // PPUMASK
            0x2001 => {
                cartridge.on_ppu_mask_write(value);
                self.mask = PpuMask::from_bits(value);
                self.scanline_cache.dirty = true;
            }
            // OAMADDR
            0x2003 => {
                self.oam_addr = value;
            }
            // OAMDATA
            0x2004 => {
                let addr = self.oam_addr as usize;
                self.oam_data[addr] = value;
                self.oam_addr = self.oam_addr.wrapping_add(1);
                self.scanline_cache.dirty = true;
            }
            // PPUSCROLL
            0x2005 => {
                cartridge.on_ppu_scroll_write(value);
                self.write_scroll(value);
                self.scanline_cache.dirty = true;
            }
            // PPUADDR
            0x2006 => {
                self.write_vram_addr(value);
                self.scanline_cache.dirty = true;
            }
            // PPUDATA
            0x2007 => {
                let vram_addr = self.vram_addr;
                // closures forwarding to cartridge
                self.write_vram(vram_addr, value, cartridge);
                self.ctrl.inc_ppu_addr(&mut self.vram_addr);
                self.scanline_cache.dirty = true;
            }
            _ => {} // Ignore other addresses
        }
    }

    fn read_vram(&mut self, address: u16, cartridge: &mut Cartridge) -> u8 {
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
                if !cartridge.write_nametable(addr, value) {
                    self.name_table.write(addr, value);
                }
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
            self.temp_vram_addr = (self.temp_vram_addr & 0xFFE0) | ((value as u16 >> 3) & 0x001F);
            self.write_toggle = true;
        } else {
            // Second write - Y scroll
            self.temp_vram_addr = (self.temp_vram_addr & !0x73E0)
                | (((value as u16 & 0x07) << 12) & 0x7000)
                | (((value as u16 >> 3) & 0x001F) << 5);
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
            self.write_toggle = false;
        }
    }

    fn read_name_table_byte(&mut self, address: u16, cartridge: &Cartridge) -> u8 {
        cartridge
            .read_nametable(address)
            .unwrap_or_else(|| self.name_table.read(address))
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

    fn get_background_pixel(
        &mut self,
        cartridge: &Cartridge,
        screen_x: u8,
        screen_y: u8,
    ) -> (u8, u8) {
        // Check left-column clipping
        if !self.mask.background_left_enabled() && screen_x < 8 {
            return (0, 0);
        }

        let scroll_addr = self.temp_vram_addr;
        let base_nametable = ((scroll_addr >> 10) & 0x0003) as u8;
        let scroll_x = ((scroll_addr & 0x001f) << 3) | self.fine_x as u16;
        let scroll_y = (((scroll_addr >> 5) & 0x001f) << 3) | ((scroll_addr >> 12) & 0x0007);

        let world_x = scroll_x + screen_x as u16;
        let world_y = scroll_y + screen_y as u16;

        let nt_select_x = (((base_nametable & 0x01) as u16) + (world_x / 256)) & 0x01;
        let nt_select_y = ((((base_nametable >> 1) & 0x01) as u16) + (world_y / 240)) & 0x01;
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

        let base_addr = self.cur_pattern_table_idx as u16 * 0x1000;
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

    fn sprite_covers_scanline(&self, sprite_idx: usize, screen_y: u8) -> bool {
        let byte_idx = sprite_idx * 4;
        let y = self.oam_data[byte_idx] as i16;
        let sprite_y = screen_y as i16 - y;
        (0..self.sprite_height()).contains(&sprite_y)
    }

    fn update_sprite_overflow(&mut self, screen_y: u8) {
        let count = (0..64)
            .filter(|&sprite_idx| self.sprite_covers_scanline(sprite_idx, screen_y))
            .take(MAX_SPRITES_PER_SCANLINE + 1)
            .count();

        if count > MAX_SPRITES_PER_SCANLINE {
            self.status.set_sprite_overflow(true);
        }
    }

    fn prepare_scanline_cache(&mut self, cartridge: &Cartridge, screen_y: u8) {
        if !self.scanline_cache.dirty && self.scanline_cache.scanline == screen_y {
            return;
        }

        self.scanline_cache.scanline = screen_y;
        self.scanline_cache.dirty = false;
        self.scanline_cache.background = [CachedBackgroundPixel::default(); 256];
        self.scanline_cache.sprite_pixels = [None; 256];
        self.scanline_cache.sprite_zero_pixels = [false; 256];

        self.update_sprite_overflow(screen_y);

        if self.mask.background_enabled() {
            for screen_x in 0..256u16 {
                let (palette_idx, color_idx) =
                    self.get_background_pixel(cartridge, screen_x as u8, screen_y);
                self.scanline_cache.background[screen_x as usize] = CachedBackgroundPixel {
                    palette_idx,
                    color_idx,
                };
            }
        }

        if !self.mask.sprite_enabled() {
            return;
        }

        let sprite_height = self.sprite_height();
        let mut visible_count = 0;
        let mut visible_sprites = [CachedSprite {
            x: 0,
            palette_idx: 0,
            behind_bg: false,
            pixels: [0; 8],
        }; MAX_SPRITES_PER_SCANLINE];

        for sprite_idx in 0..64 {
            if !self.sprite_covers_scanline(sprite_idx, screen_y) {
                continue;
            }

            let byte_idx = sprite_idx * 4;
            let y = self.oam_data[byte_idx];
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
                for screen_x in 0..256u16 {
                    self.scanline_cache.sprite_zero_pixels[screen_x as usize] =
                        sprite.opaque_pixel(screen_x as u8).is_some();
                }
            }

            if visible_count < MAX_SPRITES_PER_SCANLINE {
                visible_sprites[visible_count] = sprite;
                visible_count += 1;
            }
        }

        for screen_x in 0..256u16 {
            let x = screen_x as u8;
            if !self.mask.sprite_left_enabled() && x < 8 {
                continue;
            }

            for sprite in visible_sprites.iter().take(visible_count) {
                if let Some(pixel) = sprite.opaque_pixel(x) {
                    self.scanline_cache.sprite_pixels[screen_x as usize] = Some(pixel);
                    break;
                }
            }
        }
    }

    fn read_vram_and_inc(&mut self, cartridge: &mut Cartridge) -> u8 {
        let vram_addr = self.vram_addr;
        let value = self.read_vram(vram_addr, cartridge);
        self.ctrl.inc_ppu_addr(&mut self.vram_addr);
        value
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
        self.cur_name_table_addr = 0x2000 + (self.ctrl.name_table_select() as u16 * 0x400);
        self.scanline_cache.dirty = true;
        // Update pattern table index based on background_pattern_table flag
        self.cur_pattern_table_idx = if self.ctrl.background_pattern_table() {
            1
        } else {
            0
        };
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

    fn render_pixel(&mut self, cartridge: &Cartridge, x: u8, y: u8) -> Pixel {
        self.prepare_scanline_cache(cartridge, y);

        let bg_pixel = if self.mask.background_enabled() {
            let cached = self.scanline_cache.background[x as usize];
            (cached.palette_idx, cached.color_idx)
        } else {
            (0, 0) // transparent
        };

        let sprite_pixel = if self.mask.sprite_enabled() {
            self.scanline_cache.sprite_pixels[x as usize]
        } else {
            None
        };

        let (bg_palette_idx, bg_color_idx) = bg_pixel;

        if self.mask.background_enabled()
            && self.mask.sprite_enabled()
            && bg_color_idx != 0
            && x != 255
            && (x >= 8 || self.mask.background_left_enabled())
            && (x >= 8 || self.mask.sprite_left_enabled())
            && self.scanline_cache.sprite_zero_pixels[x as usize]
        {
            self.status.set_sprite_zero_hit(true);
        }

        let color = match sprite_pixel {
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
        };

        // Apply grayscale and emphasis effects from PPUMASK
        apply_effects(
            color,
            self.mask.grayscale(),
            self.mask.red_tint(),
            self.mask.green_tint(),
            self.mask.blue_tint(),
        )
    }
}

#[cfg(test)]
mod tests;
