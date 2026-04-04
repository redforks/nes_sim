use super::*;
use crate::nes::mapper::{Cartridge, TestCartridge};
use crate::render::ImageRender;

fn new_test_ppu_and_pattern() -> (Ppu, [u8; 8192]) {
    (Ppu::new(()), [0; 8192])
}

#[test]
fn repr_ppu_status() {
    let status: PpuStatus = 0b1010_0000.into();
    assert!(status.v_blank());
    assert!(!status.sprite_zero_hit());
    assert!(status.sprite_overflow());
}

#[test]
fn repr_ppu_ctrl() {
    // Test with all bits set to 1 (0b1111_1111)
    // After reversing bit order in the struct, fields map from MSB->LSB.
    let ctrl: PpuCtrl = 0b1111_1111.into();
    assert!(ctrl.name_table_select() == 0x3); // bits 7-6
    assert!(ctrl.increment_mode()); // bit 5
    assert!(ctrl.sprite_pattern_table()); // bit 4
    assert!(ctrl.background_pattern_table()); // bit 3
    assert!(ctrl.sprite_size()); // bit 2
    assert!(ctrl.nmi_enable()); // bit 0

    // Test with pattern 0b0101_1100 (0x5C)
    // Bits (from MSB to LSB): 0 1 0 1 1 1 0 0
    // According to PPUCTRL (bit7..bit0):
    // bit7=nmi_enable=0, bit6=unused master/slave=1, bit5=sprite_size=0,
    // bit4=background_pattern_table=1, bit3=sprite_pattern_table=1,
    // bit2=increment_mode=1, bits1-0=name_table_select=0
    let ctrl: PpuCtrl = 0b0101_1100.into();
    assert_eq!(ctrl.name_table_select(), 0x0);
    assert!(ctrl.increment_mode());
    assert!(ctrl.sprite_pattern_table());
    assert!(ctrl.background_pattern_table());
    assert!(!ctrl.sprite_size());
    assert!(!ctrl.nmi_enable());

    // Test with all bits cleared
    let ctrl: PpuCtrl = 0b0000_0000.into();
    assert!(!ctrl.nmi_enable());
    assert!(!ctrl.sprite_size());
    assert!(!ctrl.background_pattern_table());
    assert!(!ctrl.sprite_pattern_table());
    assert!(!ctrl.increment_mode());
    assert_eq!(ctrl.name_table_select(), 0x0);
}

#[test]
fn repr_ppu_mask() {
    // Test with all bits set to 1
    let mask: PpuMask = 0b1111_1111.into();
    assert!(mask.grayscale());
    assert!(mask.background_left_enabled());
    assert!(mask.sprite_left_enabled());
    assert!(mask.background_enabled());
    assert!(mask.sprite_enabled());
    assert!(mask.red_tint());
    assert!(mask.green_tint());
    assert!(mask.blue_tint());

    // Test with specific bits set (0b1010_1010)
    let mask: PpuMask = 0b1010_1010.into();
    assert!(!mask.grayscale()); // bit 0
    assert!(mask.background_left_enabled()); // bit 1
    assert!(!mask.sprite_left_enabled()); // bit 2
    assert!(mask.background_enabled()); // bit 3
    assert!(!mask.sprite_enabled()); // bit 4
    assert!(mask.red_tint()); // bit 5
    assert!(!mask.green_tint()); // bit 6
    assert!(mask.blue_tint()); // bit 7

    // Test with all bits cleared
    let mask: PpuMask = 0b0000_0000.into();
    assert!(!mask.grayscale());
    assert!(!mask.background_left_enabled());
    assert!(!mask.sprite_left_enabled());
    assert!(!mask.background_enabled());
    assert!(!mask.sprite_enabled());
    assert!(!mask.red_tint());
    assert!(!mask.green_tint());
    assert!(!mask.blue_tint());
}

#[test]
fn palette_ram_read_write() {
    let mut p = PaletteRam::default();
    for i in 0..0x20 {
        p.write(0x3f00 + i, i as u8);
        assert_eq!(i as u8, p.read(0x3f00 + i));
    }
    for i in 0..0x20 {
        if i % 4 != 0 {
            // exclude mirrored address
            assert_eq!(i as u8, p.read(0x3f00 + i));
        }
    }
}

#[test]
fn palette_ram_get_color() {
    let mut p = PaletteRam::default();
    p.write(0x3f00, 15);
    p.write(0x3f01, 16);
    p.write(0x3f02, 17);
    p.write(0x3f03, 18);
    p.write(0x3f05, 19);
    assert_eq!(COLORS[15], p.get_background_color(0, 0));
    assert_eq!(COLORS[16], p.get_background_color(0, 1));
    assert_eq!(COLORS[17], p.get_background_color(0, 2));
    assert_eq!(COLORS[18], p.get_background_color(0, 3));
    assert_eq!(COLORS[19], p.get_background_color(1, 1));
}

#[test]
fn test_ppu_ctrl_to_from_u8() {
    let ctrl = PpuCtrl::new()
        .with_nmi_enable(true)
        .with_sprite_size(true)
        .with_background_pattern_table(true)
        .with_sprite_pattern_table(true)
        .with_increment_mode(true)
        .with_name_table_select(3);

    let byte: u8 = ctrl.into();
    let ctrl2: PpuCtrl = byte.into();

    assert!(ctrl2.nmi_enable());
    assert!(ctrl2.sprite_size());
    assert!(ctrl2.background_pattern_table());
    assert!(ctrl2.sprite_pattern_table());
    assert!(ctrl2.increment_mode());
    assert_eq!(ctrl2.name_table_select(), 3);
}

#[test]
fn test_ppu_status_to_from_u8() {
    let status = PpuStatus::new()
        .with_sprite_overflow(true)
        .with_sprite_zero_hit(true)
        .with_v_blank(true);

    let byte: u8 = status.into();
    let status2: PpuStatus = byte.into();

    assert!(status2.sprite_overflow());
    assert!(status2.sprite_zero_hit());
    assert!(status2.v_blank());
}

#[test]
fn test_oam_dma() {
    let mut ppu = Ppu::new(ImageRender::default_dimension());
    let data: [u8; 256] = [0x42; 256];

    ppu.oam_dma(&data);

    assert_eq!(ppu.oam_data[0], 0x42);
    assert_eq!(ppu.oam_data[128], 0x42);
    assert_eq!(ppu.oam_data[255], 0x42);
}

#[test]
fn test_read_status_clears_vblank() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();
    ppu.status.set_v_blank(true);

    assert!(ppu.status.v_blank());

    // read_status should clear vblank
    let status = ppu.read_status();
    assert!(status.v_blank());

    // v_blank should now be false
    assert!(!ppu.status.v_blank());
}

// ============================================================================
// render_pixel() Tests
// ============================================================================

fn create_test_ppu_with_mask(mask: PpuMask) -> Ppu {
    // Initialize PPU with the provided mask to avoid field reassignment
    let mut ppu = Ppu {
        mask,
        ..Ppu::new(())
    };
    // Clear OAM
    for i in 0..64 {
        ppu.oam_data[i * 4] = 0x20;
        ppu.oam_data[i * 4 + 3] = 0xFF;
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
    let oam = &mut ppu.oam_data;
    oam[index * 4] = y;
    oam[index * 4 + 1] = tile;
    oam[index * 4 + 2] = attr;
    oam[index * 4 + 3] = x;
}

fn new_test_cartridge() -> Cartridge {
    Cartridge::Test(Box::new(TestCartridge::new()))
}

fn set_bg_tile(cartridge: &mut Cartridge, tile: u8, palette_idx: u8) {
    cartridge.write_nametable(0x2000, tile);
    cartridge.write_nametable(0x23c0, palette_idx & 0x03);
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
fn render_pixel(ppu: &mut Ppu, pattern: &[u8], x: u8, y: u8) -> Pixel {
    // Build a temporary Cartridge::Test and fill its CHR with pattern bytes
    let mut cart = Cartridge::Test(Box::new(TestCartridge::new()));
    if !pattern.is_empty()
        && let Cartridge::Test(tc) = &mut cart
    {
        for i in 0..tc.chr_rom.len() {
            tc.chr_rom[i] = pattern[i % pattern.len()];
        }
    }
    ppu.render_pixel(&cart, x, y)
}

fn render_pixel_with_setup<F>(ppu: &mut Ppu, pattern: &[u8], setup: F, x: u8, y: u8) -> Pixel
where
    F: FnOnce(&mut Cartridge),
{
    let mut cart = new_test_cartridge();
    if !pattern.is_empty()
        && let Cartridge::Test(tc) = &mut cart
    {
        for i in 0..tc.chr_rom.len() {
            tc.chr_rom[i] = pattern[i % pattern.len()];
        }
    }
    setup(&mut cart);
    let pixel = ppu.render_pixel(&cart, x, y);
    ppu.mask.apply_effects(pixel)
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);
    assert_eq!(pixel, COLORS[0x16]);
}

#[test]
fn test_render_pixel_both_disabled() {
    let mut ppu = create_test_ppu_with_mask(PpuMask::new());
    let pattern = create_pattern();
    set_universal_bg_color(&mut ppu, 0x21);

    let pixel = render_pixel(&mut ppu, &pattern, 10, 10);
    assert_eq!(pixel, COLORS[0x21]);
}

#[test]
fn test_tick_renders_palette_color_when_rendering_disabled_and_vram_points_to_palette() {
    let mut ppu = Ppu {
        mask: PpuMask::new(),
        ..Ppu::new(ImageRender::default_dimension())
    };
    ppu.palette.write(0x3f00, 0x21);
    ppu.vram_addr = 0x3f10;
    ppu.scanline = 0;
    ppu.dot = 0;

    ppu.tick(&new_test_cartridge());

    let image = ppu.renderer.borrow_image();
    assert_eq!(image.get_pixel(0, 0), &image::Rgba(COLORS[0x21].0));
}

#[test]
fn test_tick_renders_background_color_when_rendering_disabled_and_vram_not_palette() {
    let mut ppu = Ppu {
        mask: PpuMask::new(),
        ..Ppu::new(ImageRender::default_dimension())
    };
    ppu.palette.write(0x3f00, 0x16);
    ppu.vram_addr = 0x2000;
    ppu.scanline = 0;
    ppu.dot = 0;

    ppu.tick(&new_test_cartridge());

    let image = ppu.renderer.borrow_image();
    assert_eq!(image.get_pixel(0, 0), &image::Rgba(COLORS[0x16].0));
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
    assert_eq!(pixel, COLORS[0x2c]);
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
    assert_eq!(pixel, COLORS[0x27]);
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 1);
    assert_eq!(pixel, COLORS[0x12]);
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 1);
    assert_eq!(pixel, COLORS[0x22]);
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 1);
    assert_eq!(pixel, COLORS[0x14]);
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 1);
    assert_eq!(pixel, COLORS[0x25]);
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 1);
    assert_eq!(pixel, COLORS[0x20]);
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
    assert_eq!(pixel, COLORS[0x26]);
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 1);
    // Sprite is in front and opaque, so sprite color should show
    assert_eq!(pixel, COLORS[0x24]);
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
    assert_eq!(pixel, COLORS[0x37]);
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

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);
    assert_eq!(pixel, COLORS[0x28]);
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
    assert_eq!(pixel, COLORS[0x29]);
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
    let pixel = render_pixel(&mut ppu, &pattern, 7, 17);
    assert_eq!(pixel, COLORS[0x2a]);
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

    let pixel = render_pixel(&mut ppu, &pattern, 0, 8);
    assert_eq!(pixel, COLORS[0x2b]);
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

    let pixel = render_pixel(&mut ppu, &pattern, 0, 0);
    assert_eq!(pixel, COLORS[0x2c]);
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
    assert_eq!(pixel, COLORS[0x2d]);
}

#[test]
fn test_render_pixel_sets_sprite_overflow_with_nine_sprites_on_scanline() {
    let mut ppu = create_test_ppu_with_mask(PpuMask::new());
    let pattern = create_pattern();

    for idx in 0..9 {
        setup_sprite(&mut ppu, idx, 20, 0, 0, (idx * 8) as u8);
    }

    assert!(!ppu.status.sprite_overflow());
    render_pixel(&mut ppu, &pattern, 0, 20);
    assert!(ppu.status.sprite_overflow());
}

#[test]
fn test_render_pixel_does_not_set_sprite_overflow_with_eight_sprites_on_scanline() {
    let mut ppu = create_test_ppu_with_mask(PpuMask::new());
    let pattern = create_pattern();

    for idx in 0..8 {
        setup_sprite(&mut ppu, idx, 20, 0, 0, (idx * 8) as u8);
    }

    render_pixel(&mut ppu, &pattern, 0, 20);
    assert!(!ppu.status.sprite_overflow());
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

    assert!(!ppu.status.sprite_zero_hit());
    render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |cart| cart.write_nametable(0x2001, 0),
        8,
        1,
    );
    assert!(ppu.status.sprite_zero_hit());
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

    render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 1);
    assert!(!ppu.status.sprite_zero_hit());
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

    render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 1);
    assert!(!ppu.status.sprite_zero_hit());
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

    render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 1);
    assert!(!ppu.status.sprite_zero_hit());
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

    render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 1);
    assert!(!ppu.status.sprite_zero_hit());
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
        |cart| cart.write_nametable(0x201f, 0),
        255,
        10,
    );
    assert!(!ppu.status.sprite_zero_hit());
}

// ============================================================================
// Grayscale and Emphasis Tests
// ============================================================================

/// Helper to extract RGB components from a pixel
fn pixel_to_rgb(pixel: Pixel) -> (u8, u8, u8) {
    let Rgba([r, g, b, _]) = pixel;
    (r, g, b)
}

#[test]
fn test_render_pixel_grayscale_mode() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true)
            .with_grayscale(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16); // Color 0x16 is blue-ish

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);
    let (r, g, b) = pixel_to_rgb(pixel);

    // In grayscale mode, R, G, and B should all be equal
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

    let pixel = render_pixel(&mut ppu, &pattern, 0, 1);
    let pixel = ppu.mask.apply_effects(pixel);
    let (r, g, b) = pixel_to_rgb(pixel);

    // In grayscale mode, R, G, and B should all be equal
    assert_eq!(r, g, "Red and Green should be equal in grayscale mode");
    assert_eq!(g, b, "Green and Blue should be equal in grayscale mode");
}

#[test]
fn test_render_pixel_red_emphasis() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true)
            .with_red_tint(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel_with_emphasis =
        render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);

    // Compare with no emphasis
    {
        let this = &mut ppu;
        let mask = PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true);
        this.mask = mask;
    };
    let pixel_without_emphasis =
        render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);

    let (r_with, g_with, b_with) = pixel_to_rgb(pixel_with_emphasis);
    let (r_without, g_without, b_without) = pixel_to_rgb(pixel_without_emphasis);

    // Red should stay the same, green and blue should be attenuated
    assert_eq!(r_with, r_without, "Red should not change with red emphasis");
    assert!(
        g_with < g_without || g_without == 0,
        "Green should be attenuated with red emphasis"
    );
    assert!(
        b_with < b_without || b_without == 0,
        "Blue should be attenuated with red emphasis"
    );
}

#[test]
fn test_render_pixel_green_emphasis() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true)
            .with_green_tint(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel_with_emphasis =
        render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);

    // Compare with no emphasis
    {
        let this = &mut ppu;
        let mask = PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true);
        this.mask = mask;
    };
    let pixel_without_emphasis =
        render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);

    let (r_with, g_with, b_with) = pixel_to_rgb(pixel_with_emphasis);
    let (r_without, g_without, b_without) = pixel_to_rgb(pixel_without_emphasis);

    // Green should stay the same, red and blue should be attenuated
    assert!(
        r_with < r_without || r_without == 0,
        "Red should be attenuated with green emphasis"
    );
    assert_eq!(
        g_with, g_without,
        "Green should not change with green emphasis"
    );
    assert!(
        b_with < b_without || b_without == 0,
        "Blue should be attenuated with green emphasis"
    );
}

#[test]
fn test_render_pixel_blue_emphasis() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true)
            .with_blue_tint(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel_with_emphasis =
        render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);

    // Compare with no emphasis
    {
        let this = &mut ppu;
        let mask = PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true);
        this.mask = mask;
    };
    let pixel_without_emphasis =
        render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);

    let (r_with, g_with, b_with) = pixel_to_rgb(pixel_with_emphasis);
    let (r_without, g_without, b_without) = pixel_to_rgb(pixel_without_emphasis);

    // Blue should stay the same, red and green should be attenuated
    assert!(
        r_with < r_without || r_without == 0,
        "Red should be attenuated with blue emphasis"
    );
    assert!(
        g_with < g_without || g_without == 0,
        "Green should be attenuated with blue emphasis"
    );
    assert_eq!(
        b_with, b_without,
        "Blue should not change with blue emphasis"
    );
}

#[test]
fn test_render_pixel_multiple_emphasis_bits() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true)
            .with_red_tint(true)
            .with_green_tint(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel_with_emphasis =
        render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);

    // Compare with no emphasis
    {
        let this = &mut ppu;
        let mask = PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true);
        this.mask = mask;
    };
    let pixel_without_emphasis =
        render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);

    let (r_with, g_with, b_with) = pixel_to_rgb(pixel_with_emphasis);
    let (r_without, g_without, b_without) = pixel_to_rgb(pixel_without_emphasis);

    // With red + green emphasis, only blue should be attenuated
    assert_eq!(
        r_with, r_without,
        "Red should not change with red+green emphasis"
    );
    assert_eq!(
        g_with, g_without,
        "Green should not change with red+green emphasis"
    );
    assert!(
        b_with < b_without || b_without == 0,
        "Blue should be attenuated with red+green emphasis"
    );
}

#[test]
fn test_render_pixel_all_emphasis_bits() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true)
            .with_red_tint(true)
            .with_green_tint(true)
            .with_blue_tint(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel_with_emphasis =
        render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);

    // Compare with no emphasis - all channels should remain the same
    {
        let this = &mut ppu;
        let mask = PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true);
        this.mask = mask;
    };
    let pixel_without_emphasis =
        render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);

    let (r_with, g_with, b_with) = pixel_to_rgb(pixel_with_emphasis);
    let (r_without, g_without, b_without) = pixel_to_rgb(pixel_without_emphasis);

    // With all emphasis bits set, nothing should be attenuated
    assert_eq!(
        r_with, r_without,
        "Red should not change with all emphasis bits"
    );
    assert_eq!(
        g_with, g_without,
        "Green should not change with all emphasis bits"
    );
    assert_eq!(
        b_with, b_without,
        "Blue should not change with all emphasis bits"
    );
}

#[test]
fn test_render_pixel_grayscale_and_emphasis_combined() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true)
            .with_grayscale(true)
            .with_red_tint(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);
    let (r, g, b) = pixel_to_rgb(pixel);

    // In grayscale mode with emphasis, result should still be grayscale
    assert_eq!(r, g, "Red and Green should be equal in grayscale mode");
    assert_eq!(g, b, "Green and Blue should be equal in grayscale mode");
}

#[test]
fn test_render_pixel_no_emphasis_no_change() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel = render_pixel_with_setup(&mut ppu, &pattern, |cart| set_bg_tile(cart, 0, 0), 0, 0);

    // Without any effects, pixel should match COLORS lookup directly
    assert_eq!(pixel, COLORS[0x16]);
}
