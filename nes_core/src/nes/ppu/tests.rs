use super::*;

fn new_test_ppu_and_pattern() -> (Ppu, [u8; 8192]) {
    (Ppu::default(), [0; 8192])
}

#[test]
fn palette_read_write() {
    let mut p = Palette::default();
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
fn palette_get_color() {
    let mut p = Palette::default();
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
fn read_write_v_ram() {
    let (mut ppu, mut pattern) = new_test_ppu_and_pattern();
    // data_addr default to 0
    assert_eq!(ppu.read_vram_and_inc(&pattern), 0);

    ppu.read(&pattern, 0x2000); // reset addr
    ppu.write(0x2006, 0x21);
    ppu.write(0x2006, 0x08);
    ppu.write(0x2007, 0x12);
    ppu.write(0x2007, 0x34);
    assert_eq!(ppu.read_vram(&pattern, 0x2108), 0x12);
    assert_eq!(ppu.read_vram(&pattern, 0x2109), 0x34);

    ppu.read(&pattern, 0x2000); // reset addr
    ppu.write(0x2006, 0x21);
    ppu.write(0x2006, 0x08);
    assert_eq!(ppu.read(&pattern, 0x2007), 0x12);
    assert_eq!(ppu.read(&pattern, 0x2007), 0x34);

    // set increase mode to 32
    ppu.write(0x2000, PpuCtrl::new().with_increment_mode(true).into());
    ppu.read(&pattern, 0x2000); // reset addr
    ppu.write(0x2006, 0x21);
    ppu.write(0x2006, 0x08);
    ppu.write(0x2007, 0x56);
    ppu.write(0x2007, 0x78);
    assert_eq!(ppu.read_vram(&pattern, 0x2108,), 0x56);
    assert_eq!(ppu.read_vram(&pattern, 0x2128,), 0x78);

    ppu.read(&pattern, 0x2000); // reset addr
    ppu.write(0x2006, 0x21);
    ppu.write(0x2006, 0x08);
    assert_eq!(ppu.read(&pattern, 0x2007), 0x56);
    assert_eq!(ppu.read(&pattern, 0x2007), 0x78);

    fn rw(ppu: &mut Ppu, pattern: &mut [u8], addr: u16, w_value: u8, r_value: u8) {
        ppu.read(pattern, 0x2000); // reset addr
        ppu.write(0x2006, (addr >> 8) as u8);
        ppu.write(0x2006, addr as u8);
        ppu.write(0x2007, w_value);
        ppu.read(pattern, 0x2000); // reset addr
        ppu.write(0x2006, (addr >> 8) as u8);
        ppu.write(0x2006, addr as u8);
        assert_eq!(ppu.read(pattern, 0x2007), r_value);
    }

    fn rw_round_trip(ppu: &mut Ppu, pattern: &mut [u8], addr: u16, value: u8) {
        rw(ppu, pattern, addr, value, value);
    }

    // ignore write to pattern/chr-rom
    rw(&mut ppu, &mut pattern, 0x0001, 13, 0);

    // read-write palette ram index
    rw_round_trip(&mut ppu, &mut pattern, 0x3f00, 14);
    // read-write mirror of name-table
    rw_round_trip(&mut ppu, &mut pattern, 0x2400, 15);
    // read-write mirror of palette ram index
    rw_round_trip(&mut ppu, &mut pattern, 0x3f20, 16);
}

#[test]
fn read_write_oam() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();
    // oam addr default to 0
    assert_eq!(ppu.oam_addr, 0);

    ppu.write(0x2003, 0x12);
    // write oma data, auto inc oma addr
    ppu.write(0x2004, 0x34);
    assert_eq!(0x13, ppu.oam_addr);
    ppu.write(0x2004, 0x56);
    assert_eq!(0x14, ppu.oam_addr);

    ppu.write(0x2003, 0x12);
    assert_eq!(ppu.read_oam_data(), 0x34);
    assert_eq!(0x12, ppu.oam_addr);
    // read oma data, won't auto inc oma addr
    assert_eq!(ppu.read_oam_data(), 0x34);
}

#[test]
fn nmi_occurred() {
    let (mut ppu, _) = new_test_ppu_and_pattern();

    let flag = PpuCtrl::new().with_nmi_enable(false);
    assert!(!ppu.status.borrow().v_blank());

    // disable nmi
    ppu.set_control_flags(flag);
    ppu.set_v_blank(true);
    assert!(ppu.status.borrow().v_blank());
    // should_nmi is false because nmi is disabled
    assert!(!(ppu.ctrl_flags.nmi_enable() && ppu.status.borrow().v_blank()));
    ppu.set_v_blank(false);
    // v_blank is false on v_blank end
    assert!(!ppu.status.borrow().v_blank());

    // read status register will reset v_blank flag
    ppu.set_v_blank(true);
    assert!(ppu.read_status().v_blank());
    assert!(!ppu.status.borrow().v_blank());

    // enable nmi
    ppu.set_control_flags(flag.with_nmi_enable(true));
    assert!(!(ppu.ctrl_flags.nmi_enable() && ppu.status.borrow().v_blank()));
    ppu.set_v_blank(true);
    assert!(ppu.status.borrow().v_blank());
    // should_nmi is true because nmi is enabled
    assert!(ppu.ctrl_flags.nmi_enable() && ppu.status.borrow().v_blank());
}

#[test]
fn ppu_tick_timing() {
    let (mut ppu, pattern) = new_test_ppu_and_pattern();

    // Enable NMI
    ppu.set_control_flags(PpuCtrl::new().with_nmi_enable(true));

    // Initial state
    assert_eq!(ppu.scanline, 0);
    assert_eq!(ppu.dot, 0);
    assert!(!ppu.status.borrow().v_blank());

    // Advance to scanline 241, dot 0
    ppu.scanline = VBLANK_SET_SCANLINE;
    ppu.dot = 0;

    // Tick once - should set VBlank and trigger NMI
    let result = ppu.tick(&pattern);
    assert!(result.nmi, "NMI should be triggered at scanline 241, dot 1");
    assert!(
        result.vblank_started,
        "VBlank should start at scanline 241, dot 1"
    );
    assert!(ppu.status.borrow().v_blank());

    // Advance to scanline 261, dot 0
    ppu.scanline = VBLANK_CLEAR_SCANLINE;
    ppu.dot = 0;

    // Tick once - should clear VBlank
    let result = ppu.tick(&pattern);
    assert!(
        !result.nmi,
        "NMI should not be triggered when clearing VBlank"
    );
    assert!(
        !result.vblank_started,
        "VBlank should not start at scanline 261"
    );
    assert!(!ppu.status.borrow().v_blank());
}

#[test]
fn ppu_tick_scanline_wrap() {
    let (mut ppu, pattern) = new_test_ppu_and_pattern();

    // Start at last scanline, last dot
    ppu.scanline = SCANLINES_PER_FRAME - 1; // 261
    ppu.dot = DOTS_PER_SCANLINE - 1; // 340

    // Tick once - should wrap to scanline 0, dot 0
    let _ = ppu.tick(&pattern);
    assert_eq!(ppu.scanline, 0);
    assert_eq!(ppu.dot, 0);
}

#[test]
fn test_ppu_ctrl_to_from_u8() {
    let ctrl = PpuCtrl::new()
        .with_nmi_enable(true)
        .with_ppu_master(true)
        .with_sprite_size(true)
        .with_background_pattern_table(true)
        .with_sprite_pattern_table(true)
        .with_increment_mode(true)
        .with_name_table_select(3);

    let byte: u8 = ctrl.into();
    let ctrl2: PpuCtrl = byte.into();

    assert!(ctrl2.nmi_enable());
    assert!(ctrl2.ppu_master());
    assert!(ctrl2.sprite_size());
    assert!(ctrl2.background_pattern_table());
    assert!(ctrl2.sprite_pattern_table());
    assert!(ctrl2.increment_mode());
    assert_eq!(ctrl2.name_table_select(), 3);
}

#[test]
fn test_ppu_mask_to_from_u8() {
    let mask = PpuMask::new()
        .with_blue_tint(true)
        .with_green_tint(true)
        .with_red_tint(true)
        .with_sprite_enabled(true)
        .with_background_enabled(true)
        .with_sprite_left_enabled(true)
        .with_background_left_enabled(true)
        .with_grayscale(true);

    let byte: u8 = mask.into();
    let mask2: PpuMask = byte.into();

    assert!(mask2.blue_tint());
    assert!(mask2.green_tint());
    assert!(mask2.red_tint());
    assert!(mask2.sprite_enabled());
    assert!(mask2.background_enabled());
    assert!(mask2.sprite_left_enabled());
    assert!(mask2.background_left_enabled());
    assert!(mask2.grayscale());
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
    let mut ppu = Ppu::default();
    let data: [u8; 256] = [0x42; 256];

    ppu.oam_dma(&data);

    assert_eq!(ppu.oam[0], 0x42);
    assert_eq!(ppu.oam[128], 0x42);
    assert_eq!(ppu.oam[255], 0x42);
}

#[test]
fn test_set_mirroring() {
    let mut ppu = Ppu::default();

    ppu.set_mirroring(Mirroring::Horizontal);
    ppu.set_mirroring(Mirroring::Vertical);

    // Just verify it doesn't panic - actual mirroring behavior is tested in name_table tests
}

#[test]
fn test_read_status_clears_vblank() {
    let (ppu, _pattern) = new_test_ppu_and_pattern();
    ppu.set_v_blank(true);

    assert!(ppu.status.borrow().v_blank());

    // read_status should clear vblank
    let status = ppu.read_status();
    assert!(status.v_blank());

    // v_blank should now be false
    assert!(!ppu.status.borrow().v_blank());
}

#[test]
fn test_write_0x2006_set_data_addr() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();

    // First write to 0x2006 sets high byte of VRAM address
    ppu.write(0x2006, 0x3f);

    // Second write to 0x2006 sets low byte and commits to vram_addr
    ppu.write(0x2006, 0x00);

    // Verify the VRAM address was set
    assert_eq!(*ppu.vram_addr.borrow(), 0x3f00);
}

#[test]
fn test_read_0x3000_reads_palette() {
    let (mut ppu, pattern) = new_test_ppu_and_pattern();

    // Write to palette memory at 0x3f00 using VRAM write
    ppu.write(0x2006, 0x3f);
    ppu.write(0x2006, 0x00);
    ppu.write(0x2007, 0xAB);

    // Reset VRAM address to read from 0x3f20 (mirrors to 0x3f00)
    ppu.write(0x2006, 0x3f);
    ppu.write(0x2006, 0x20);

    // Read from 0x3f20 (should mirror to 0x3f00)
    let val = ppu.read(&pattern, 0x2007);
    assert_eq!(val, 0xAB);
}

#[test]
fn test_pattern_table_selection() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();

    // Set pattern table index via control flag
    ppu.write(
        0x2000,
        PpuCtrl::new().with_background_pattern_table(true).into(),
    );

    assert_eq!(ppu.cur_pattern_table_idx, 1);
}

#[test]
fn test_mirroring() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();

    // Test setting vertical mirroring
    ppu.set_mirroring(Mirroring::Vertical);
    assert_eq!(ppu.mirroring(), Mirroring::Vertical);

    // Test setting horizontal mirroring
    ppu.set_mirroring(Mirroring::Horizontal);
    assert_eq!(ppu.mirroring(), Mirroring::Horizontal);

    // Test four screen mirroring
    ppu.set_mirroring(Mirroring::Four);
    assert_eq!(ppu.mirroring(), Mirroring::Four);

    // Test one screen mirroring modes
    ppu.set_mirroring(Mirroring::LowerBank);
    assert_eq!(ppu.mirroring(), Mirroring::LowerBank);

    ppu.set_mirroring(Mirroring::UpperBank);
    assert_eq!(ppu.mirroring(), Mirroring::UpperBank);
}

#[test]
fn test_scroll_register_first_write() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();

    // First write to 0x2005 sets coarse X and fine X
    ppu.write(0x2005, 0x3A); // coarse X = 0x07, fine X = 0x02

    // Verify the temp_vram_addr was updated with coarse X
    assert_eq!(ppu.temp_vram_addr & 0x1F, 0x07);
    assert_eq!(ppu.fine_x, 0x02);
    // write_toggle should be true
    assert!(*ppu.write_toggle.borrow());
}

#[test]
fn test_scroll_register_second_write() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();

    // First write
    ppu.write(0x2005, 0x00);

    // Second write to 0x2005 sets coarse Y and fine Y
    ppu.write(0x2005, 0x7B); // coarse Y = 0x0F, fine Y = 0x03

    // Verify the temp_vram_addr was updated
    assert_eq!((ppu.temp_vram_addr >> 5) & 0x1F, 0x0F);
    assert_eq!((ppu.temp_vram_addr >> 12) & 0x07, 0x03);
    // write_toggle should be false again
    assert!(!*ppu.write_toggle.borrow());
}

#[test]
fn test_vram_address_high_byte_only() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();

    // Write only the high byte to 0x2006
    ppu.write(0x2006, 0x3F);

    // write_toggle should be true, but vram_addr should not be committed yet
    assert!(*ppu.write_toggle.borrow());
    // The temp_vram_addr should have been set
    assert_eq!(ppu.temp_vram_addr, 0x3F00);
}

#[test]
fn test_read_vram_pattern_table() {
    let (ppu, pattern) = new_test_ppu_and_pattern();

    // Write some data to pattern table at 0x1000
    // Note: We can't actually write to pattern table (it's ROM in real NES)
    // But we can test reading from it
    let val = ppu.read_vram(&pattern, 0x1000);
    // Pattern table data defaults to 0
    assert_eq!(val, 0);
}

#[test]
fn test_write_vram_pattern_ignored() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();

    // Writing to pattern table space should be ignored (it's ROM)
    // This test just verifies it doesn't panic
    ppu.write_vram(0x0000, 0xFF);
    ppu.write_vram(0x1000, 0xFF);
}

#[test]
fn test_write_0x2000_updates_name_table_addr() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();

    // Set name table select to 1 (0x2400)
    ppu.write(0x2000, PpuCtrl::new().with_name_table_select(1).into());

    assert_eq!(ppu.cur_name_table_addr, 0x2400);

    // Set name table select to 2 (0x2800)
    ppu.write(0x2000, PpuCtrl::new().with_name_table_select(2).into());

    assert_eq!(ppu.cur_name_table_addr, 0x2800);

    // Set name table select to 3 (0x2C00)
    ppu.write(0x2000, PpuCtrl::new().with_name_table_select(3).into());

    assert_eq!(ppu.cur_name_table_addr, 0x2C00);
}

#[test]
fn test_ppu_mask_register() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();

    // Write mask register
    ppu.write(
        0x2001,
        PpuMask::new()
            .with_blue_tint(true)
            .with_sprite_enabled(true)
            .with_background_enabled(true)
            .into(),
    );

    assert!(ppu.mask.blue_tint());
    assert!(ppu.mask.sprite_enabled());
    assert!(ppu.mask.background_enabled());
}

#[test]
fn test_read_status_register() {
    let (ppu, pattern) = new_test_ppu_and_pattern();

    // Set some status flags
    *ppu.status.borrow_mut() = PpuStatus::new()
        .with_sprite_overflow(true)
        .with_sprite_zero_hit(true)
        .with_v_blank(true);

    // Read status register (0x2002)
    let val = ppu.read(&pattern, 0x2002);

    // Check that status flags are in the correct bits
    let status: PpuStatus = val.into();
    assert!(status.sprite_overflow());
    assert!(status.sprite_zero_hit());
    assert!(status.v_blank());

    // VBlank should be cleared after reading
    assert!(!ppu.status.borrow().v_blank());
    // write_toggle should be reset
    assert!(!*ppu.write_toggle.borrow());
}

#[test]
fn test_read_buffer() {
    let (mut ppu, pattern) = new_test_ppu_and_pattern();

    // First read from VRAM (0x2007) returns read_buffer (default 0)
    let val1 = ppu.read(&pattern, 0x2007);
    assert_eq!(val1, 0);

    // Set up pattern data
    let pattern_data = [0u8; 8192];

    // Set VRAM address
    ppu.write(0x2006, 0x00);
    ppu.write(0x2006, 0x00);

    // Reading from pattern table - first read returns buffer
    let _val2 = ppu.read(&pattern_data, 0x2007);
    // Actual value is loaded into buffer for next read
}

// ============================================================================
// render_pixel() Tests
// ============================================================================

fn create_test_ppu_with_mask(mask: PpuMask) -> Ppu {
    let mut ppu = Ppu::default();
    ppu.mask = mask;
    for i in 0..64 {
        ppu.oam[i * 4] = 0x20;
        ppu.oam[i * 4 + 3] = 0xFF;
    }
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
    ppu.oam[index * 4] = y;
    ppu.oam[index * 4 + 1] = tile;
    ppu.oam[index * 4 + 2] = attr;
    ppu.oam[index * 4 + 3] = x;
}

fn set_bg_tile(ppu: &mut Ppu, tile: u8, palette_idx: u8) {
    ppu.name_table.write(0x2000, tile);
    ppu.name_table.write(0x23c0, palette_idx & 0x03);
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

#[test]
fn test_render_pixel_returns_background_color() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel = ppu.render_pixel(&pattern, 0, 0);
    assert_eq!(pixel, COLORS[0x16]);
}

#[test]
fn test_render_pixel_both_disabled() {
    let mut ppu = create_test_ppu_with_mask(PpuMask::new());
    let pattern = create_pattern();
    set_universal_bg_color(&mut ppu, 0x21);

    let pixel = ppu.render_pixel(&pattern, 10, 10);
    assert_eq!(pixel, COLORS[0x21]);
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

    let pixel = ppu.render_pixel(&pattern, 12, 34);
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

    let pixel = ppu.render_pixel(&pattern, 0, 1);
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
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x12);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    let pixel = ppu.render_pixel(&pattern, 0, 1);
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
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x11);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x22);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    let pixel = ppu.render_pixel(&pattern, 0, 1);
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
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x14);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x24);
    setup_sprite(&mut ppu, 0, 0, 1, 0x20, 0);

    let pixel = ppu.render_pixel(&pattern, 0, 1);
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
    set_bg_tile(&mut ppu, 0, 0);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x25);
    setup_sprite(&mut ppu, 0, 0, 1, 0x20, 0);

    let pixel = ppu.render_pixel(&pattern, 0, 1);
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
    set_bg_tile(&mut ppu, 0, 0);
    set_universal_bg_color(&mut ppu, 0x20);
    set_bg_palette_color(&mut ppu, 0, 1, 0x10);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x30);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    let pixel = ppu.render_pixel(&pattern, 0, 1);
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
    set_tile_solid(&mut pattern, 0, 2, 2);
    set_sprite_palette_color(&mut ppu, 0, 1, 0x26);
    set_sprite_palette_color(&mut ppu, 1, 2, 0x36);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);
    setup_sprite(&mut ppu, 1, 0, 2, 1, 0);

    let pixel = ppu.render_pixel(&pattern, 0, 1);
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
    set_tile_solid(&mut pattern, 0, 2, 3);
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x14);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x24);
    set_sprite_palette_color(&mut ppu, 1, 3, 0x34);
    setup_sprite(&mut ppu, 0, 0, 1, 0x20, 0);
    setup_sprite(&mut ppu, 1, 0, 2, 1, 0);

    let pixel = ppu.render_pixel(&pattern, 0, 1);
    assert_eq!(pixel, COLORS[0x14]);
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

    let pixel = ppu.render_pixel(&pattern, 0, 1);
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
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x18);
    set_bg_palette_color(&mut ppu, 0, 2, 0x28);
    ppu.set_control_flags(PpuCtrl::new().with_background_pattern_table(true));

    let pixel = ppu.render_pixel(&pattern, 0, 0);
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

    let pixel = ppu.render_pixel(&pattern, 0, 1);
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
    setup_sprite(&mut ppu, 0, 10, 1, 0xC0, 0);

    let pixel = ppu.render_pixel(&pattern, 7, 18);
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
    setup_sprite(&mut ppu, 0, 10, 0, 0, 0);

    let pixel = ppu.render_pixel(&pattern, 0, 19);
    assert_eq!(pixel, COLORS[0x2b]);
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
    ppu.name_table.write(0x2000 + 1, 0);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 8);

    assert!(!ppu.status.borrow().sprite_zero_hit());
    ppu.render_pixel(&pattern, 8, 1);
    assert!(ppu.status.borrow().sprite_zero_hit());
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
    set_bg_tile(&mut ppu, 0, 0);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    ppu.render_pixel(&pattern, 0, 1);
    assert!(!ppu.status.borrow().sprite_zero_hit());
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
    set_bg_tile(&mut ppu, 0, 0);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    ppu.render_pixel(&pattern, 0, 1);
    assert!(!ppu.status.borrow().sprite_zero_hit());
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
    set_bg_tile(&mut ppu, 0, 0);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);

    ppu.render_pixel(&pattern, 0, 1);
    assert!(!ppu.status.borrow().sprite_zero_hit());
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
    set_bg_tile(&mut ppu, 0, 0);
    setup_sprite(&mut ppu, 0, 20, 1, 0, 20);
    setup_sprite(&mut ppu, 1, 0, 1, 0, 0);

    ppu.render_pixel(&pattern, 0, 1);
    assert!(!ppu.status.borrow().sprite_zero_hit());
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
    ppu.name_table.write(0x2000 + 31, 0);
    setup_sprite(&mut ppu, 0, 9, 0, 0, 248);

    ppu.render_pixel(&pattern, 255, 10);
    assert!(!ppu.status.borrow().sprite_zero_hit());
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
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16); // Color 0x16 is blue-ish

    let pixel = ppu.render_pixel(&pattern, 0, 0);
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

    let pixel = ppu.render_pixel(&pattern, 0, 1);
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
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel_with_emphasis = ppu.render_pixel(&pattern, 0, 0);

    // Compare with no emphasis
    ppu.mask = PpuMask::new()
        .with_background_enabled(true)
        .with_background_left_enabled(true);
    let pixel_without_emphasis = ppu.render_pixel(&pattern, 0, 0);

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
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel_with_emphasis = ppu.render_pixel(&pattern, 0, 0);

    // Compare with no emphasis
    ppu.mask = PpuMask::new()
        .with_background_enabled(true)
        .with_background_left_enabled(true);
    let pixel_without_emphasis = ppu.render_pixel(&pattern, 0, 0);

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
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel_with_emphasis = ppu.render_pixel(&pattern, 0, 0);

    // Compare with no emphasis
    ppu.mask = PpuMask::new()
        .with_background_enabled(true)
        .with_background_left_enabled(true);
    let pixel_without_emphasis = ppu.render_pixel(&pattern, 0, 0);

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
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel_with_emphasis = ppu.render_pixel(&pattern, 0, 0);

    // Compare with no emphasis
    ppu.mask = PpuMask::new()
        .with_background_enabled(true)
        .with_background_left_enabled(true);
    let pixel_without_emphasis = ppu.render_pixel(&pattern, 0, 0);

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
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel_with_emphasis = ppu.render_pixel(&pattern, 0, 0);

    // Compare with no emphasis - all channels should remain the same
    ppu.mask = PpuMask::new()
        .with_background_enabled(true)
        .with_background_left_enabled(true);
    let pixel_without_emphasis = ppu.render_pixel(&pattern, 0, 0);

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
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel = ppu.render_pixel(&pattern, 0, 0);
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
    set_bg_tile(&mut ppu, 0, 0);
    set_bg_palette_color(&mut ppu, 0, 1, 0x16);

    let pixel = ppu.render_pixel(&pattern, 0, 0);

    // Without any effects, pixel should match COLORS lookup directly
    assert_eq!(pixel, COLORS[0x16]);
}
