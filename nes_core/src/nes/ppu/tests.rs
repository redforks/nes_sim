use super::*;
use crate::nes::mapper::{Mirroring, TestCartridge};
use crate::render::ImageRender;

use test_case::test_case;

fn new_test_ppu_and_pattern() -> (Ppu, [u8; 8192]) {
    (
        Ppu::new((), Mirroring::Horizontal, Box::new(TestCartridge::new())),
        [0; 8192],
    )
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

    let status = ppu.read_status();
    assert!(status.v_blank());

    assert!(!ppu.registers.status.v_blank());
}

#[test]
fn test_peek_status_does_not_clear_vblank() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();
    ppu.registers.status.set_v_blank(true);

    let status = ppu.peek(0x2002);
    assert_eq!(status & 0x80, 0x80);
    assert!(ppu.registers.status.v_blank());
}

#[test]
fn test_open_bus_bits_decay_to_zero() {
    let mut ppu = Ppu::new((), Mirroring::Horizontal, Box::new(TestCartridge::new()));

    ppu.write(0x2002, 0xFF);
    ppu.cycle = PPU_OPEN_BUS_DECAY_TICKS;

    assert_eq!(ppu.read(0x2000), 0x00);
}

#[test]
fn test_status_read_only_refreshes_high_bits() {
    let mut ppu = Ppu::new((), Mirroring::Horizontal, Box::new(TestCartridge::new()));

    ppu.write(0x2002, 0xFF);
    ppu.cycle = PPU_OPEN_BUS_DECAY_TICKS;
    ppu.registers.status.set_v_blank(true);
    let result = ppu.read(0x2002);
    // Only high bit should remain; the 0xFF bus latch written to $2002 had 0x80 masked.
    assert_eq!(result, 0x80);

    let result2 = ppu.read(0x2000);
    assert_eq!(result2, 0x80);
}

fn create_test_ppu_with_mask(mask: PpuMask) -> Ppu {
    let mut ppu = Ppu {
        ..Ppu::new((), Mirroring::Horizontal, Box::new(TestCartridge::new()))
    };
    ppu.registers.mask = mask;

    for i in 0..64 {
        ppu.oam.set_byte(i * 4, 0x20);
        ppu.oam.set_byte(i * 4 + 3, 0xff);
    }

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

fn setup_sprite(ppu: &mut Ppu, index: u8, y: u8, tile: u8, attr: u8, x: u8) {
    ppu.oam.set_byte(index * 4, y);
    ppu.oam.set_byte(index * 4 + 1, tile);
    ppu.oam.set_byte(index * 4 + 2, attr);
    ppu.oam.set_byte(index * 4 + 3, x);
}

fn set_bg_tile(ppu: &mut Ppu, tile: u8, palette_idx: u8) {
    ppu.write_vram(0x2000, tile);
    ppu.write_vram(0x23c0, palette_idx & 0x03);
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

fn fill_chr(ppu: &mut Ppu, pattern: &[u8]) {
    for i in 0..0x2000 {
        ppu.write_vram(i as u16, pattern[i % pattern.len()]);
    }
}

fn populate_sprite_secondary_oam(ppu: &mut Ppu, target_scanline: u16) {
    let eval_scanline = if target_scanline == 0 {
        261
    } else {
        target_scanline - 1
    };
    ppu.sprite.begin_sprite_overflow_eval();
    for dot in (65..=256).filter(|d| d % 2 == 1) {
        ppu.timing.dot = dot;
        ppu.timing.scanline = eval_scanline;
        ppu.sprite
            .step_sprite_overflow_eval(eval_scanline, ppu.registers.ctrl, &ppu.oam);
    }
    ppu.sprite.swap_secondary_oam();
}

fn render_pixel(ppu: &mut Ppu, pattern: &[u8], x: u8, y: u8) -> u8 {
    if !pattern.is_empty() {
        fill_chr(ppu, pattern);
    }
    if ppu.registers.mask.sprite_enabled() {
        populate_sprite_secondary_oam(ppu, y as u16);
    }
    ppu.timing.scanline = y as u16;
    ppu.render_pixel(x)
}

fn run_scanline(ppu: &mut Ppu, pattern: &[u8], scanline: u16) {
    if !pattern.is_empty() {
        fill_chr(ppu, pattern);
    }

    ppu.timing.scanline = scanline;
    ppu.timing.dot = 0;
    for _ in 0..341 {
        ppu.tick();
    }
}

fn render_pixel_with_setup<F>(ppu: &mut Ppu, pattern: &[u8], setup: F, x: u8, y: u8) -> u8
where
    F: FnOnce(&mut Ppu),
{
    if !pattern.is_empty() {
        fill_chr(ppu, pattern);
    }
    setup(ppu);
    if ppu.registers.mask.sprite_enabled() {
        populate_sprite_secondary_oam(ppu, y as u16);
    }
    ppu.timing.scanline = y as u16;
    ppu.render_pixel(x)
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |ppu| set_bg_tile(ppu, 0, 0), 0, 0);
    assert_eq!(pixel, 0x16);
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
        ..Ppu::new(
            ImageRender::default_dimension(),
            Mirroring::Horizontal,
            Box::new(TestCartridge::new()),
        )
    };
    ppu.palette.write(0x3f00, 0x21);
    ppu.registers.vram_addr = 0x3f10;
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;

    ppu.tick();
    ppu.tick();

    let image = ppu.renderer.borrow_image();
    assert_eq!(
        image.get_pixel(0, 0),
        &image::Rgba(ppu.color_theme.color(0x21).0)
    );
}

#[test]
fn test_tick_renders_background_color_when_rendering_disabled_and_vram_not_palette() {
    let mut ppu = Ppu {
        ..Ppu::new(
            ImageRender::default_dimension(),
            Mirroring::Horizontal,
            Box::new(TestCartridge::new()),
        )
    };
    ppu.palette.write(0x3f00, 0x16);
    ppu.registers.vram_addr = 0x2000;
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;

    ppu.tick();
    ppu.tick();

    let image = ppu.renderer.borrow_image();
    assert_eq!(
        image.get_pixel(0, 0),
        &image::Rgba(ppu.color_theme.color(0x16).0)
    );
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |ppu| set_bg_tile(ppu, 0, 0), 0, 1);
    assert_eq!(pixel, 0x12);
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |ppu| set_bg_tile(ppu, 0, 0), 0, 1);
    assert_eq!(pixel, 0x22);
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |ppu| set_bg_tile(ppu, 0, 0), 0, 1);
    assert_eq!(pixel, 0x14);
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |ppu| set_bg_tile(ppu, 0, 0), 0, 1);
    assert_eq!(pixel, 0x25);
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |ppu| set_bg_tile(ppu, 0, 0), 0, 1);
    assert_eq!(pixel, 0x20);
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
    set_tile_solid(&mut pattern, 0, 2, 0);
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
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |ppu| set_bg_tile(ppu, 0, 0), 0, 1);
    assert_eq!(pixel, 0x24);
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |ppu| set_bg_tile(ppu, 0, 0), 0, 0);
    assert_eq!(pixel, 0x28);
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
    setup_sprite(&mut ppu, 0, 10, 1, 0xC0, 0);

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
    ppu.set_control_flags(PpuCtrl::new().with_sprite_size_16(true));
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
    ppu.set_control_flags(PpuCtrl::new().with_sprite_size_16(true));
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
    ppu.set_control_flags(PpuCtrl::new().with_sprite_size_16(true));
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
        setup_sprite(&mut ppu, idx, 20, 0, 0, idx * 8);
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
        setup_sprite(&mut ppu, idx, 20, 0, 0, idx * 8);
    }

    render_pixel(&mut ppu, &pattern, 0, 20);
    assert!(!ppu.registers.status.sprite_overflow());
}

#[test]
fn test_render_pixel_does_not_set_sprite_overflow_when_rendering_disabled() {
    let mut ppu = create_test_ppu_with_mask(PpuMask::new());
    let pattern = create_pattern();

    for idx in 0..9 {
        setup_sprite(&mut ppu, idx, 20, 0, 0, idx * 8);
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
        setup_sprite(&mut ppu, idx, 239, 0, 0, idx * 8);
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
        setup_sprite(&mut ppu, idx, 240, 0, 0, idx * 8);
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
    render_pixel_with_setup(&mut ppu, &pattern, |ppu| ppu.write_vram(0x2001, 0), 8, 1);
    ppu.sprite.update_ctrl_status(&mut ppu.registers.status);
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

    render_pixel_with_setup(&mut ppu, &pattern, |ppu| set_bg_tile(ppu, 0, 0), 0, 1);
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

    render_pixel_with_setup(&mut ppu, &pattern, |ppu| set_bg_tile(ppu, 0, 0), 0, 1);
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

    render_pixel_with_setup(&mut ppu, &pattern, |ppu| set_bg_tile(ppu, 0, 0), 0, 1);
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

    render_pixel_with_setup(&mut ppu, &pattern, |ppu| set_bg_tile(ppu, 0, 0), 0, 1);
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

    render_pixel_with_setup(&mut ppu, &pattern, |ppu| ppu.write_vram(0x201f, 0), 255, 10);
    assert!(!ppu.registers.status.sprite_zero_hit());
}

fn render_bg_pixel(mask: PpuMask) -> u8 {
    let mut ppu = create_test_ppu_with_mask(mask);
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);
    render_pixel_with_setup(&mut ppu, &pattern, |ppu| set_bg_tile(ppu, 0, 0), 0, 0)
}

#[test]
fn test_render_pixel_no_emphasis_no_change() {
    let pixel = render_bg_pixel(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true),
    );
    assert_eq!(pixel, 0x16);
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
