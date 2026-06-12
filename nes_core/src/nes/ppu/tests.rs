use super::palette::color;
use super::*;
use crate::nes::mapper::{Cartridge, Mirroring, TestCartridge};
use crate::render::ImageRender;
use crate::set_system_cycles;
use test_case::test_case;

fn new_test_ppu_and_pattern() -> (Ppu, [u8; 8192]) {
    (Ppu::new((), Mirroring::Horizontal), [0; 8192])
}

#[test]
fn repr_ppu_status() {
    let status: PpuStatus = 0b1010_0000.into();
    assert!(status.v_blank());
    assert!(!status.sprite_zero_hit());
    assert!(status.sprite_overflow());
}

#[test]
fn test_read_status_clears_vblank() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();
    ppu.registers.status.set_v_blank(true);

    assert!(ppu.registers.status.v_blank());

    // read_status should clear vblank
    let status = ppu.read_status();
    assert!(status.v_blank());

    // v_blank should now be false
    assert!(!ppu.registers.status.v_blank());
}

#[test]
fn test_peek_status_does_not_clear_vblank() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();
    let cartridge = Cartridge::Test(Box::new(TestCartridge::new()));
    ppu.registers.status.set_v_blank(true);

    let status = ppu.peek(0x2002, &cartridge);
    assert_eq!(status & 0x80, 0x80);
    assert!(ppu.registers.status.v_blank());
}

#[test]
fn test_open_bus_bits_decay_to_zero() {
    let mut ppu = Ppu::new((), Mirroring::Horizontal);
    let mut cartridge = new_test_cartridge();

    set_system_cycles(0);
    ppu.write(0x2002, 0xFF, &mut cartridge);
    set_system_cycles(PPU_OPEN_BUS_DECAY_TICKS);

    assert_eq!(ppu.read(0x2000, &mut cartridge), 0x00);
}

#[test]
fn test_status_read_only_refreshes_high_bits() {
    let mut ppu = Ppu::new((), Mirroring::Horizontal);
    let mut cartridge = new_test_cartridge();

    set_system_cycles(0);
    ppu.write(0x2002, 0xFF, &mut cartridge);
    set_system_cycles(PPU_OPEN_BUS_DECAY_TICKS);
    ppu.registers.status.set_v_blank(true);
    assert_eq!(ppu.read(0x2002, &mut cartridge), 0x80);

    assert_eq!(ppu.read(0x2000, &mut cartridge), 0x80);
}

fn create_test_ppu_with_mask(mask: PpuMask) -> Ppu {
    // Initialize PPU with the provided mask to avoid field reassignment
    let mut ppu = Ppu {
        effective_mask: mask,
        ..Ppu::new((), Mirroring::Horizontal)
    };
    ppu.registers.mask = mask;
    // Clear OAM
    for i in 0..64 {
        ppu.registers.oam_data[i * 4] = 0x20;
        ppu.registers.oam_data[i * 4 + 3] = 0xFF;
    }
    // Clear palette RAM
    ppu.palette.data = [0; 0x20];
    ppu
}

fn create_pattern() -> [u8; 8192] {
    [0; 8192]
}

fn set_tile_solid(pattern: &mut [u8; 8192], table: usize, tile: u8, color_idx: u8) {
    let offset = table * 4096 + tile as usize * 16;
    let low = if color_idx & 0x01 != 0 { 0xFF } else { 0x00 };
    let high = if color_idx & 0x02 != 0 { 0xFF } else { 0x00 };
    for row in 0..8 {
        pattern[offset + row] = low;
        pattern[offset + row + 8] = high;
    }
}

fn set_tile_pixel(
    pattern: &mut [u8; 8192],
    table: usize,
    tile: u8,
    x: usize,
    y: usize,
    color_idx: u8,
) {
    let offset = table * 4096 + tile as usize * 16;
    let bit = 7 - x;
    let low_mask = 1 << bit;
    let high_mask = 1 << bit;

    pattern[offset + y] &= !low_mask;
    pattern[offset + y + 8] &= !high_mask;

    if color_idx & 0x01 != 0 {
        pattern[offset + y] |= low_mask;
    }
    if color_idx & 0x02 != 0 {
        pattern[offset + y + 8] |= high_mask;
    }
}

fn setup_sprite(ppu: &mut Ppu, index: usize, y: u8, tile: u8, attr: u8, x: u8) {
    let oam = &mut ppu.registers.oam_data;
    oam[index * 4] = y;
    oam[index * 4 + 1] = tile;
    oam[index * 4 + 2] = attr;
    oam[index * 4 + 3] = x;
}

fn new_test_cartridge() -> Cartridge {
    Cartridge::Test(Box::new(TestCartridge::new()))
}

fn set_bg_tile(ppu: &mut Ppu, _cartridge: &Cartridge, tile: u8, palette_idx: u8) {
    ppu.write_nametable(0x2000, tile);
    ppu.write_nametable(0x23c0, palette_idx & 0x03);
}

fn set_bg_palette_color(ppu: &mut Ppu, palette_idx: u8, color_idx: u8, color: u8) {
    let addr = 0x3f00 + palette_idx as u16 * 4 + color_idx as u16;
    ppu.palette.write(addr, color);
}

fn set_sprite_palette_color(ppu: &mut Ppu, palette_idx: u8, color_idx: u8, color: u8) {
    let addr = 0x3f10 + palette_idx as u16 * 4 + color_idx as u16;
    ppu.palette.write(addr, color);
}

fn set_universal_bg_color(ppu: &mut Ppu, color: u8) {
    ppu.palette.write(0x3f00, color);
}

/// Render a single pixel (for testing)
fn render_pixel(ppu: &mut Ppu, pattern: &[u8], x: u8, y: u8) -> u8 {
    let mut cart = Cartridge::Test(Box::new(TestCartridge::new()));
    if !pattern.is_empty()
        && let Cartridge::Test(tc) = &mut cart
    {
        for i in 0..tc.chr_rom.len() {
            tc.chr_rom[i] = pattern[i % pattern.len()];
        }
    }
    ppu.timing.scanline = y as u16;
    ppu.render_pixel(x, &mut cart)
}

fn run_scanline(ppu: &mut Ppu, pattern: &[u8], scanline: u16) {
    let mut cart = Cartridge::Test(Box::new(TestCartridge::new()));
    if !pattern.is_empty()
        && let Cartridge::Test(tc) = &mut cart
    {
        for i in 0..tc.chr_rom.len() {
            tc.chr_rom[i] = pattern[i % pattern.len()];
        }
    }

    ppu.timing.scanline = scanline;
    ppu.timing.dot = 0;
    for _ in 0..341 {
        ppu.tick(&mut cart);
    }
}

fn render_pixel_with_setup<F>(ppu: &mut Ppu, pattern: &[u8], setup: F, x: u8, y: u8) -> Pixel
where
    F: FnOnce(&mut Ppu, &mut Cartridge),
{
    let mut cart = new_test_cartridge();
    if !pattern.is_empty()
        && let Cartridge::Test(tc) = &mut cart
    {
        for i in 0..tc.chr_rom.len() {
            tc.chr_rom[i] = pattern[i % pattern.len()];
        }
    }
    setup(ppu, &mut cart);
    ppu.timing.scanline = y as u16;
    let pixel_idx = ppu.render_pixel(x, &mut cart);
    ppu.registers.mask.apply_effects(color(pixel_idx))
}

#[test]
fn test_render_pixel_returns_background_color() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        0,
    );
    assert_eq!(pixel, color(0x16));
}

#[test]
fn test_render_pixel_both_disabled() {
    let mut ppu = create_test_ppu_with_mask(PpuMask::new());
    let pattern = create_pattern();
    set_universal_bg_color(&mut ppu, 0x21);

    let pixel = render_pixel(&mut ppu, &pattern, 10, 10);
    assert_eq!(pixel, 0x21);
}

#[test]
fn test_tick_renders_palette_color_when_rendering_disabled_and_vram_points_to_palette() {
    let mut ppu = Ppu {
        ..Ppu::new(ImageRender::default_dimension(), Mirroring::Horizontal)
    };
    ppu.palette.write(0x3f00, 0x21);
    ppu.registers.vram_addr = 0x3f10;
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;

    let mut cart = new_test_cartridge();
    ppu.tick(&mut cart);
    ppu.tick(&mut cart);

    let image = ppu.renderer.borrow_image();
    assert_eq!(image.get_pixel(0, 0), &image::Rgba(color(0x21).0));
}

#[test]
fn test_tick_renders_background_color_when_rendering_disabled_and_vram_not_palette() {
    let mut ppu = Ppu {
        ..Ppu::new(ImageRender::default_dimension(), Mirroring::Horizontal)
    };
    ppu.palette.write(0x3f00, 0x16);
    ppu.registers.vram_addr = 0x2000;
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;

    let mut cart = new_test_cartridge();
    ppu.tick(&mut cart);
    ppu.tick(&mut cart);

    let image = ppu.renderer.borrow_image();
    assert_eq!(image.get_pixel(0, 0), &image::Rgba(color(0x16).0));
}

#[test]
fn test_render_pixel_transparent_bg_and_sprite_use_backdrop_color() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true)
            .with_background_left_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let pattern = create_pattern();
    set_universal_bg_color(&mut ppu, 0x2c);

    let pixel = render_pixel(&mut ppu, &pattern, 12, 34);
    assert_eq!(pixel, 0x2c);
}

#[test]
fn test_render_pixel_returns_sprite_color_when_background_disabled() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 1, 1);
    set_sprite_palette_color(&mut ppu, 0, 1, 0x27);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    let pixel = render_pixel(&mut ppu, &pattern, 0, 1);
    assert_eq!(pixel, 0x27);
}

#[test]
fn test_render_pixel_transparent_sprite_falls_back_to_background() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true)
            .with_background_left_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_bg_palette_color(&mut ppu, 0, 1, 0x12);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
    assert_eq!(pixel, color(0x12));
}

#[test]
fn test_render_pixel_sprite_in_front_of_background() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true)
            .with_background_left_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_tile_solid(&mut pattern, 0, 1, 2);
    set_bg_palette_color(&mut ppu, 0, 1, 0x11);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x22);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
    assert_eq!(pixel, color(0x22));
}

#[test]
fn test_render_pixel_background_priority_when_sprite_is_behind() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true)
            .with_background_left_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_tile_solid(&mut pattern, 0, 1, 2);
    set_bg_palette_color(&mut ppu, 0, 1, 0x14);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x24);
    setup_sprite(&mut ppu, 0, 0, 1, 0x20, 0);

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
    assert_eq!(pixel, color(0x14));
}

#[test]
fn test_render_pixel_sprite_behind_transparent_background() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true)
            .with_background_left_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 1, 2);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x25);
    setup_sprite(&mut ppu, 0, 0, 1, 0x20, 0);

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
    assert_eq!(pixel, color(0x25));
}

#[test]
fn test_render_pixel_applies_left_column_clipping() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_tile_solid(&mut pattern, 0, 1, 2);
    set_universal_bg_color(&mut ppu, 0x20);
    set_bg_palette_color(&mut ppu, 0, 1, 0x10);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x30);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
    assert_eq!(pixel, color(0x20));
}

#[test]
fn test_render_pixel_uses_highest_priority_opaque_sprite() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 1, 1);
    set_tile_solid(&mut pattern, 0, 2, 0); // transparent
    set_sprite_palette_color(&mut ppu, 0, 1, 0x26);
    set_sprite_palette_color(&mut ppu, 1, 2, 0x36);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);
    setup_sprite(&mut ppu, 1, 0, 2, 1, 0);

    let pixel = render_pixel(&mut ppu, &pattern, 0, 1);
    assert_eq!(pixel, 0x26);
}

#[test]
fn test_render_pixel_applies_sprite_priority_before_background_priority() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true)
            .with_background_left_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_tile_solid(&mut pattern, 0, 1, 2);
    set_bg_palette_color(&mut ppu, 0, 1, 0x14);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x24);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0); // sprite in front (not behind)

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
    // Sprite is in front and opaque, so sprite color should show
    assert_eq!(pixel, color(0x24));
}

#[test]
fn test_render_pixel_skips_transparent_higher_priority_sprite() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 2, 2);
    set_sprite_palette_color(&mut ppu, 1, 2, 0x37);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);
    setup_sprite(&mut ppu, 1, 0, 2, 1, 0);

    let pixel = render_pixel(&mut ppu, &pattern, 0, 1);
    assert_eq!(pixel, 0x37);
}

#[test]
fn test_render_pixel_respects_background_pattern_table_selection() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_tile_solid(&mut pattern, 1, 0, 2);
    set_bg_palette_color(&mut ppu, 0, 1, 0x18);
    set_bg_palette_color(&mut ppu, 0, 2, 0x28);
    ppu.set_control_flags(PpuCtrl::new().with_background_pattern_table(true));

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        0,
    );
    assert_eq!(pixel, color(0x28));
}

#[test]
fn test_render_pixel_respects_sprite_pattern_table_selection() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 1, 1);
    set_tile_solid(&mut pattern, 1, 1, 2);
    set_sprite_palette_color(&mut ppu, 0, 1, 0x19);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x29);
    ppu.set_control_flags(PpuCtrl::new().with_sprite_pattern_table(true));
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    let pixel = render_pixel(&mut ppu, &pattern, 0, 1);
    assert_eq!(pixel, 0x29);
}

#[test]
fn test_render_pixel_respects_sprite_flipping() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_pixel(&mut pattern, 0, 1, 0, 0, 1);
    set_sprite_palette_color(&mut ppu, 0, 1, 0x2a);
    setup_sprite(&mut ppu, 0, 10, 1, 0xC0, 0); // Both flips enabled

    // With both flips, pixel (0,0) in tile becomes (7,7) after flipping
    // So we need to render at a position where sprite_offset becomes (7,7)
    // sprite_offset = screen - sprite_pos, so screen = sprite_offset + sprite_pos
    // If sprite_pos.x = 0, then screen_x = 7
    // If sprite_pos.y = 10, then screen_y = 10 + 7 = 17
    let pixel = render_pixel(&mut ppu, &pattern, 7, 18);
    assert_eq!(pixel, 0x2a);
}

#[test]
fn test_render_pixel_uses_second_tile_for_8x16_sprites() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 1, 3);
    set_sprite_palette_color(&mut ppu, 0, 3, 0x2b);
    ppu.set_control_flags(PpuCtrl::new().with_sprite_size(true));
    setup_sprite(&mut ppu, 0, 0, 0, 0, 0);

    let pixel = render_pixel(&mut ppu, &pattern, 0, 9);
    assert_eq!(pixel, 0x2b);
}

#[test]
fn test_render_pixel_uses_odd_tile_bank_for_8x16_sprites() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 1, 2, 2);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x2c);
    ppu.set_control_flags(PpuCtrl::new().with_sprite_size(true));
    setup_sprite(&mut ppu, 0, 0, 3, 0, 0);

    let pixel = render_pixel(&mut ppu, &pattern, 0, 1);
    assert_eq!(pixel, 0x2c);
}

#[test]
fn test_render_pixel_uses_vertical_flip_for_8x16_sprites() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_sprite_palette_color(&mut ppu, 0, 1, 0x2d);
    ppu.set_control_flags(PpuCtrl::new().with_sprite_size(true));
    setup_sprite(&mut ppu, 0, 0, 0, 0x80, 0);

    let pixel = render_pixel(&mut ppu, &pattern, 0, 15);
    assert_eq!(pixel, 0x2d);
}

#[test]
fn test_render_pixel_sets_sprite_overflow_with_nine_sprites_on_scanline() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true),
    );
    let pattern = create_pattern();

    for idx in 0..9 {
        setup_sprite(&mut ppu, idx, 20, 0, 0, (idx * 8) as u8);
    }

    assert!(!ppu.registers.status.sprite_overflow());
    run_scanline(&mut ppu, &pattern, 20);
    assert!(ppu.registers.status.sprite_overflow());
}

#[test]
fn test_render_pixel_does_not_set_sprite_overflow_with_eight_sprites_on_scanline() {
    let mut ppu = create_test_ppu_with_mask(PpuMask::new());
    let pattern = create_pattern();

    for idx in 0..8 {
        setup_sprite(&mut ppu, idx, 20, 0, 0, (idx * 8) as u8);
    }

    render_pixel(&mut ppu, &pattern, 0, 20);
    assert!(!ppu.registers.status.sprite_overflow());
}

#[test]
fn test_render_pixel_does_not_set_sprite_overflow_when_rendering_disabled() {
    let mut ppu = create_test_ppu_with_mask(PpuMask::new());
    let pattern = create_pattern();

    for idx in 0..9 {
        setup_sprite(&mut ppu, idx, 20, 0, 0, (idx * 8) as u8);
    }

    render_pixel(&mut ppu, &pattern, 0, 20);
    assert!(!ppu.registers.status.sprite_overflow());
}

#[test]
fn test_sprite_overflow_sets_for_next_scanline_at_y239() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true),
    );
    let pattern = create_pattern();

    for idx in 0..9 {
        setup_sprite(&mut ppu, idx, 239, 0, 0, (idx * 8) as u8);
    }

    run_scanline(&mut ppu, &pattern, 239);
    assert!(ppu.registers.status.sprite_overflow());
}

#[test]
fn test_sprite_overflow_does_not_set_for_next_scanline_at_y240() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true),
    );
    let pattern = create_pattern();

    for idx in 0..9 {
        setup_sprite(&mut ppu, idx, 240, 0, 0, (idx * 8) as u8);
    }

    run_scanline(&mut ppu, &pattern, 239);
    assert!(!ppu.registers.status.sprite_overflow());
}

#[test]
fn test_render_pixel_sprite_zero_hit() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true)
            .with_background_left_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_tile_solid(&mut pattern, 0, 1, 2);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 8);

    assert!(!ppu.registers.status.sprite_zero_hit());
    render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, _cart| ppu.write_nametable(0x2001, 0),
        8,
        1,
    );
    ppu.sprite.update_ctrl_status(&mut ppu.registers);
    assert!(ppu.registers.status.sprite_zero_hit());
}

#[test]
fn test_render_pixel_sprite_zero_hit_requires_opaque_background() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true)
            .with_background_left_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 1, 2);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
    assert!(!ppu.registers.status.sprite_zero_hit());
}

#[test]
fn test_render_pixel_sprite_zero_hit_respects_background_left_mask() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_tile_solid(&mut pattern, 0, 1, 2);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
    assert!(!ppu.registers.status.sprite_zero_hit());
}

#[test]
fn test_render_pixel_sprite_zero_hit_respects_sprite_left_mask() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true)
            .with_background_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_tile_solid(&mut pattern, 0, 1, 2);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
    assert!(!ppu.registers.status.sprite_zero_hit());
}

#[test]
fn test_render_pixel_sprite_zero_hit_requires_sprite_zero() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true)
            .with_background_left_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_tile_solid(&mut pattern, 0, 1, 2);
    setup_sprite(&mut ppu, 0, 20, 1, 0, 20);
    setup_sprite(&mut ppu, 1, 0, 1, 0, 0);

    render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
    assert!(!ppu.registers.status.sprite_zero_hit());
}

#[test]
fn test_render_pixel_sprite_zero_not_at_x255() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_tile_solid(&mut pattern, 0, 1, 2);
    setup_sprite(&mut ppu, 0, 9, 0, 0, 248);

    render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, _cart| ppu.write_nametable(0x201f, 0),
        255,
        10,
    );
    assert!(!ppu.registers.status.sprite_zero_hit());
}

fn pixel_to_rgb(pixel: Pixel) -> (u8, u8, u8) {
    pixel.to_rgb()
}

fn render_bg_pixel(mask: PpuMask) -> Pixel {
    let mut ppu = create_test_ppu_with_mask(mask);
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);
    render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        0,
    )
}

#[test_case(0b00000000, true, true, true ; "no effects")]
#[test_case(0b00100000, true, false, false ; "red emphasis")]
#[test_case(0b01000000, false, true, false ; "green emphasis")]
#[test_case(0b10000000, false, false, true ; "blue emphasis")]
#[test_case(0b01100000, true, true, false ; "red+green emphasis")]
#[test_case(0b10100000, true, false, true ; "red+blue emphasis")]
#[test_case(0b11000000, false, true, true ; "green+blue emphasis")]
#[test_case(0b11100000, true, true, true ; "all emphasis bits")]
fn test_render_pixel_emphasis(
    tint_bits: u8,
    r_preserved: bool,
    g_preserved: bool,
    b_preserved: bool,
) {
    let with = render_bg_pixel(
        PpuMask::from(tint_bits)
            .with_background_enabled(true)
            .with_background_left_enabled(true),
    );
    let without = render_bg_pixel(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true),
    );

    let (r_with, g_with, b_with) = pixel_to_rgb(with);
    let (r_without, g_without, b_without) = pixel_to_rgb(without);

    fn check(name: &str, channel: u8, reference: u8, preserved: bool) {
        if preserved {
            assert_eq!(channel, reference, "{name} should not change");
        } else {
            assert!(
                channel < reference || reference == 0,
                "{name} should be attenuated",
            );
        }
    }

    check("Red", r_with, r_without, r_preserved);
    check("Green", g_with, g_without, g_preserved);
    check("Blue", b_with, b_without, b_preserved);
}

#[test_case(false ; "background grayscale")]
#[test_case(true ; "background grayscale with red emphasis")]
fn test_render_pixel_grayscale_background(with_red_tint: bool) {
    let pixel = render_bg_pixel(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true)
            .with_grayscale(true)
            .with_red_tint(with_red_tint),
    );
    let (r, g, b) = pixel_to_rgb(pixel);
    assert_eq!(r, g, "Red and Green should be equal in grayscale mode");
    assert_eq!(g, b, "Green and Blue should be equal in grayscale mode");
}

#[test]
fn test_render_pixel_grayscale_mode_with_sprite() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true)
            .with_grayscale(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 1, 1);
    set_sprite_palette_color(&mut ppu, 0, 1, 0x27);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    let pixel = color(render_pixel(&mut ppu, &pattern, 0, 1));
    let pixel = ppu.registers.mask.apply_effects(pixel);
    let (r, g, b) = pixel_to_rgb(pixel);

    // In grayscale mode, R, G, and B should all be equal
    assert_eq!(r, g, "Red and Green should be equal in grayscale mode");
    assert_eq!(g, b, "Green and Blue should be equal in grayscale mode");
}

#[test]
fn test_render_pixel_no_emphasis_no_change() {
    let pixel = render_bg_pixel(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true),
    );
    assert_eq!(pixel, color(0x16));
}

#[test_case(0, 340, false, false => (1, 0, false, 0); "dot wraps at scanline end")]
#[test_case(261, 340, false, false => (0, 0, true, 1); "frame wraps")]
#[test_case(261, 339, true, true => (0, 0, false, 1); "odd frame skip")]
#[test_case(261, 339, true, false => (261, 340, true, 0); "skip requires rendering")]
#[test_case(261, 339, false, true => (261, 340, false, 0); "skip requires odd_frame")]
#[test_case(261, 338, true, true => (261, 339, true, 0); "skip requires correct dot")]
#[test_case(260, 339, true, true => (260, 340, true, 0); "skip requires correct scanline")]
#[test_case(261, 340, true, true => (0, 0, false, 1); "normal frame wrap from last dot")]
fn timing_advance_state(
    scanline: u16,
    dot: u16,
    odd_frame: bool,
    rendering_enabled: bool,
) -> (u16, u16, bool, usize) {
    let mut t = Timing {
        scanline,
        dot,
        odd_frame,
        frame_no: 0,
    };

    t.advance(rendering_enabled);
    (t.scanline, t.dot, t.odd_frame, t.frame_no)
}

#[test_case(0, 1, true; "scanline 0 dot 1 is visible")]
#[test_case(0, 256, true; "scanline 0 dot 256 is visible")]
#[test_case(239, 128, true; "scanline 239 dot 128 is visible")]
#[test_case(0, 0, false; "dot 0 is not visible")]
#[test_case(0, 257, false; "dot 257 is not visible")]
#[test_case(240, 1, false; "scanline 240 dot 1 is not visible")]
#[test_case(261, 1, false; "pre-render scanline is not visible")]
#[test_case(239, 340, false; "hblank dot is not visible")]
fn timing_is_visible(scanline: u16, dot: u16, expected: bool) {
    let t = Timing {
        scanline,
        dot,
        odd_frame: false,
        frame_no: 0,
    };
    assert_eq!(t.is_visible(), expected);
}

#[test_case(241, 1, true; "scanline 241 dot 1 enters vblank")]
#[test_case(241, 0, false; "one dot before vblank")]
#[test_case(241, 2, false; "one dot after vblank")]
#[test_case(240, 1, false; "one scanline before vblank")]
#[test_case(242, 1, false; "one scanline after vblank")]
#[test_case(0, 0, false; "arbitrary timing not vblank")]
#[test_case(261, 1, false; "pre-render scanline not vblank")]
fn timing_enter_vblank(scanline: u16, dot: u16, expected: bool) {
    let t = Timing {
        scanline,
        dot,
        odd_frame: false,
        frame_no: 0,
    };
    assert_eq!(t.enter_vblank(), expected);
}

// ============================================================================
// leave_vblank() Tests
// ============================================================================

#[test_case(261, 1, true; "scanline 261 dot 1 leaves vblank")]
#[test_case(261, 0, false; "one dot before leave vblank")]
#[test_case(261, 2, false; "one dot after leave vblank")]
#[test_case(260, 1, false; "one scanline before leave vblank")]
#[test_case(0, 1, false; "frame start not leave vblank")]
#[test_case(0, 0, false; "arbitrary timing not leave vblank")]
#[test_case(241, 1, false; "vblank set not leave vblank")]
fn timing_leave_vblank(scanline: u16, dot: u16, expected: bool) {
    let t = Timing {
        scanline,
        dot,
        odd_frame: false,
        frame_no: 0,
    };
    assert_eq!(t.leave_vblank(), expected);
}
