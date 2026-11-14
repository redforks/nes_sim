use super::*;
use crate::nes::mapper::{Cartridge, PpuRenderState};
use crate::render::{ImageRender, Render};

use test_case::test_case;

fn new_test_ppu_and_pattern() -> (Ppu, [u8; 8192]) {
    (Ppu::new(()), [0; 8192])
}

#[test]
fn repr_ppu_status() {
    let status: PpuStatus = PpuStatus::from_bytes([0b1010_0000]);
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
    let cart = Cartridge::test();

    let status = ppu.peek(0x2002, &cart);
    assert_eq!(status & 0x80, 0x80);
    assert!(ppu.registers.status.v_blank());
}

/// Registers are mirrored every 8 bytes through `$3FFF`, so peeking a mirror
/// address must read the same register as its `$2000-$2007` home.
#[test]
fn peek_normalizes_mirrored_register_addresses() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();
    let cart = Cartridge::test();
    ppu.registers.status.set_v_blank(true);
    ppu.registers.status.set_sprite_overflow(true);

    let home = ppu.peek(0x2002, &cart);
    assert_eq!(home, 0xA0);
    assert_eq!(ppu.peek(0x200A, &cart), home);
    assert_eq!(ppu.peek(0x2002 + 8 * 0x100, &cart), home);
    assert_eq!(ppu.peek(0x2FFA, &cart), home);
    assert_eq!(ppu.peek(0x3E02, &cart), home);
}

/// Write-only registers are not readable on hardware; peek reports `0` instead
/// of the CPU bus latch that the mutating read path would return.
#[test]
fn peek_write_only_registers_return_zero() {
    let mut ppu = Ppu::new(());
    let mut cart = Cartridge::test();

    // Leave a bus latch behind so a latch-returning peek would be visible.
    ppu.write_ppureg(0x2002, 0xFF, &mut cart);
    ppu.registers.ctrl = PpuCtrl::from_bytes([0xFF]);

    for address in [0x2000u16, 0x2001, 0x2003, 0x2005, 0x2006] {
        assert_eq!(ppu.peek(address, &cart), 0, "{address:#06x}");
        assert_eq!(ppu.peek(address + 8, &cart), 0, "{address:#06x} mirror");
    }
}

/// `$2007` peek reads VRAM at `vram_addr` without the read path's buffer
/// refill or `PPUCTRL` increment, and without the back-to-back ignore window.
#[test]
fn peek_vram_does_not_increment_or_touch_read_buffer() {
    let mut ppu = Ppu::new(());
    let mut cart = Cartridge::test();

    ppu.write_ppureg(0x2006, 0x00, &mut cart);
    ppu.write_ppureg(0x2006, 0x10, &mut cart);
    ppu.write_ppureg(0x2007, 0x5A, &mut cart);

    ppu.write_ppureg(0x2006, 0x00, &mut cart);
    ppu.write_ppureg(0x2006, 0x10, &mut cart);
    ppu.registers.ctrl = PpuCtrl::from_bytes([0x04]); // increment by 32

    assert_eq!(ppu.peek(0x2007, &cart), 0x5A);
    assert_eq!(ppu.peek(0x2007, &cart), 0x5A);
    assert_eq!(ppu.registers.vram_addr, 0x0010);
    assert_eq!(ppu.registers.ppudata_buffer, 0x00);
}

/// `$2004` peek is rendering-aware but does not advance `oam_addr`.
#[test]
fn peek_oam_data_does_not_advance_oam_addr() {
    let mut ppu = Ppu::new(());
    let mut cart = Cartridge::test();

    ppu.write_ppureg(0x2003, 0x05, &mut cart);
    ppu.write_oam_data(0xAB);
    assert_eq!(ppu.oam_addr, 0x06);

    ppu.write_ppureg(0x2003, 0x05, &mut cart);
    assert_eq!(ppu.peek(0x2004, &cart), 0xAB);
    assert_eq!(ppu.oam_addr, 0x05);
}

/// Out-of-range peeks are a caller bug: debug builds assert (covered by the
/// range contract), release builds read `0` instead of wrapping into a
/// register or reaching into the cartridge.
#[cfg(not(debug_assertions))]
#[test]
fn peek_out_of_range_returns_zero() {
    let (mut ppu, _pattern) = new_test_ppu_and_pattern();
    let cart = Cartridge::test();
    ppu.registers.status.set_v_blank(true);

    for address in [
        0x0000u16, 0x1FFF, 0x4000, 0x4015, 0x4100, 0x6000, 0x8000, 0xFFFF,
    ] {
        assert_eq!(ppu.peek(address, &cart), 0, "{address:#06x}");
    }
}
#[test]
fn test_open_bus_bits_decay_to_zero() {
    let mut ppu = Ppu::new(());
    let mut cart = Cartridge::test();

    ppu.write_ppureg(0x2002, 0xFF, &mut cart);
    ppu.cycle = PPU_OPEN_BUS_DECAY_TICKS;

    assert_eq!(ppu.read_ppureg(0x2000, &mut cart), 0x00);
}

#[test]
fn test_status_read_only_refreshes_high_bits() {
    let mut ppu = Ppu::new(());
    let mut cart = Cartridge::test();

    ppu.write_ppureg(0x2002, 0xFF, &mut cart);
    ppu.cycle = PPU_OPEN_BUS_DECAY_TICKS;
    ppu.registers.status.set_v_blank(true);
    let result = ppu.read_ppureg(0x2002, &mut cart);
    // Only high bit should remain; the 0xFF bus latch written to $2002 had 0x80 masked.
    assert_eq!(result, 0x80);

    let result2 = ppu.read_ppureg(0x2000, &mut cart);
    assert_eq!(result2, 0x80);
}

/// The back-to-back $2007 ignore window is 6 PPU dots wide (Mesen2's
/// `_ignoreVramRead = 6` decremented once per PPU cycle): a read landing
/// up to 5 dots after a completing read is ignored, one 6 dots later is
/// normal, and the ignored read neither advances the address nor refills
/// the read buffer.
#[test]
fn vram_read_ignore_window_is_six_dots() {
    let mut ppu = Ppu::new(());
    let mut cart = Cartridge::test();

    // VRAM $0000-$0002 = 11 22 33.
    ppu.write_ppureg(0x2006, 0x00, &mut cart);
    ppu.write_ppureg(0x2006, 0x00, &mut cart);
    for v in [0x11u8, 0x22, 0x33] {
        ppu.write_ppureg(0x2007, v, &mut cart);
    }

    // Rewind to $0000; prime the buffer at cycle 0 (returns the initial
    // buffer, loads VRAM[0], increments to $0001).
    ppu.write_ppureg(0x2006, 0x00, &mut cart);
    ppu.write_ppureg(0x2006, 0x00, &mut cart);
    assert_eq!(ppu.read_ppureg(0x2007, &mut cart), 0x00);
    assert_eq!(ppu.registers.vram_addr, 0x0001);

    // At window edge (5 dots later): still ignored — stale return, no
    // increment, and the buffer still holds VRAM[0] (a refill would have
    // replaced it with VRAM[1]).
    ppu.cycle = PPUDATA_IGNORE_WINDOW_DOTS;
    assert_eq!(ppu.read_ppureg(0x2007, &mut cart), 0x00);
    assert_eq!(ppu.registers.vram_addr, 0x0001);

    // 6 dots later: outside the window, so the normal read path runs.
    ppu.cycle = PPUDATA_IGNORE_WINDOW_DOTS + 1;
    assert_eq!(ppu.read_ppureg(0x2007, &mut cart), 0x11);
    assert_eq!(ppu.registers.vram_addr, 0x0002);
}

/// An ignored back-to-back read must not reach the mapper's A12 watcher —
/// otherwise double reads would clock MMC3's IRQ counter (Mesen2 skips the
/// whole access, not just the returned value).
#[test]
fn vram_read_ignored_within_window_does_not_notify_mapper() {
    let mut ppu = Ppu::new(());
    let (mut cart, notified) = Cartridge::test_recording();

    ppu.write_ppureg(0x2006, 0x00, &mut cart);
    ppu.write_ppureg(0x2006, 0x00, &mut cart);
    let after_setup = notified.borrow().len();

    // Completing read: increments to $0001 and notifies the mapper.
    assert_eq!(ppu.read_ppureg(0x2007, &mut cart), 0x00);
    assert_eq!(notified.borrow().len(), after_setup + 1);
    assert_eq!(notified.borrow().last().copied(), Some(0x0001));

    // Same-cycle read: fully ignored — no second notification.
    assert_eq!(ppu.read_ppureg(0x2007, &mut cart), 0x00);
    assert_eq!(notified.borrow().len(), after_setup + 1);

    // Once outside the window the mapper is notified again ($0002).
    ppu.cycle = PPUDATA_IGNORE_WINDOW_DOTS + 1;
    assert_eq!(ppu.read_ppureg(0x2007, &mut cart), 0x00);
    assert_eq!(notified.borrow().len(), after_setup + 2);
    assert_eq!(notified.borrow().last().copied(), Some(0x0002));
}

/// The ignore branch sits above the palette/non-palette split, so an
/// ignored back-to-back read of a palette address must behave the same:
/// stale value, no increment, no mapper notification.
#[test]
fn vram_read_ignored_palette_read_returns_stale_value() {
    let mut ppu = Ppu::new(());
    let (mut cart, notified) = Cartridge::test_recording();

    ppu.palette.write(0x3F01, 0x2C);
    ppu.write_ppureg(0x2006, 0x3F, &mut cart);
    ppu.write_ppureg(0x2006, 0x01, &mut cart);
    let after_setup = notified.borrow().len();

    // Completing palette read: returns $2C, increments to $3F02, notifies.
    assert_eq!(ppu.read_ppureg(0x2007, &mut cart), 0x2C);
    assert_eq!(ppu.registers.vram_addr, 0x3F02);
    assert_eq!(notified.borrow().len(), after_setup + 1);

    // Same-cycle read: ignored — stale $2C, address stays put, no notify.
    assert_eq!(ppu.read_ppureg(0x2007, &mut cart), 0x2C);
    assert_eq!(ppu.registers.vram_addr, 0x3F02);
    assert_eq!(notified.borrow().len(), after_setup + 1);
}

/// $2007 reads of the palette return the value with the grayscale mask
/// ($30) applied when grayscale is enabled, while the read buffer is still
/// refilled from the nametable underneath (Mesen2 `_paletteRamMask`).
#[test_case(false, 0x2C ; "no grayscale keeps the full 6-bit entry")]
#[test_case(true, 0x20 ; "grayscale masks the entry to its $3x row")]
fn vram_read_palette_applies_grayscale_mask(grayscale: bool, expected: u8) {
    let mut ppu = Ppu::new(());
    ppu.registers.mask = PpuMask::new().with_grayscale(grayscale);
    let mut cart = Cartridge::test();

    // Palette $3F01 = $2C; the nametable byte underneath ($2F01) = $81.
    ppu.palette.write(0x3F01, 0x2C);
    cart.write_vram(0x2F01, 0x81);

    ppu.write_ppureg(0x2006, 0x3F, &mut cart);
    ppu.write_ppureg(0x2006, 0x01, &mut cart);
    // $2C AND $30 = $20 with grayscale, plus the open-bus bits 6-7
    // (latch cleared by the $2006 writes above).
    assert_eq!(ppu.read_ppureg(0x2007, &mut cart), expected);

    // The buffer was filled from the nametable underneath, not masked.
    ppu.cycle = PPUDATA_IGNORE_WINDOW_DOTS + 1;
    ppu.write_ppureg(0x2006, 0x2F, &mut cart);
    ppu.write_ppureg(0x2006, 0x01, &mut cart);
    assert_eq!(ppu.read_ppureg(0x2007, &mut cart), 0x81);
}

fn create_test_ppu_with_mask(mask: PpuMask) -> Ppu {
    let mut ppu = Ppu { ..Ppu::new(()) };
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

fn setup_sprite<R: Render>(ppu: &mut Ppu<R>, index: u8, y: u8, tile: u8, attr: u8, x: u8) {
    ppu.oam.set_byte(index * 4, y);
    ppu.oam.set_byte(index * 4 + 1, tile);
    ppu.oam.set_byte(index * 4 + 2, attr);
    ppu.oam.set_byte(index * 4 + 3, x);
}

/// Fill OAM with a ramp (`oam[i] == i`) and apply `mask` unchanged.
/// Reads of $2004 then identify the *address* by the byte value alone.
/// Seeding OAM directly is arrange-time only (no register writes 256
/// arbitrary bytes), but every assertion built on it reads back through
/// the registers, so OAMADDR movement stays behaviorally observable.
fn ppu_with_ramp_oam(mask: PpuMask) -> Ppu {
    let mut ppu = create_test_ppu_with_mask(mask);
    for i in 0..=255u8 {
        ppu.oam.set_byte(i, i);
    }
    ppu
}

/// Behavioral OAM probe: anchor `$2003` to `addr`, then read the byte
/// back through `$2004`. Reads do not move OAMADDR.
fn read_oam_at(ppu: &mut Ppu, addr: u8) -> u8 {
    let mut cart = Cartridge::test();
    ppu.write_ppureg(0x2003, addr, &mut cart);
    ppu.read_ppureg(0x2004, &mut cart)
}

fn set_bg_tile<R: Render>(ppu: &mut Ppu<R>, cartridge: &mut Cartridge, tile: u8, palette_idx: u8) {
    ppu.write_vram(0x2000, tile, cartridge);
    ppu.write_vram(0x23c0, palette_idx & 0x03, cartridge);
}

fn set_bg_palette_color<R: Render>(ppu: &mut Ppu<R>, palette_idx: u8, color_idx: u8, color: u8) {
    let addr = 0x3f00 + palette_idx as u16 * 4 + color_idx as u16;
    ppu.palette.write(addr, color);
}

fn set_sprite_palette_color<R: Render>(
    ppu: &mut Ppu<R>,
    palette_idx: u8,
    color_idx: u8,
    color: u8,
) {
    let addr = 0x3f10 + palette_idx as u16 * 4 + color_idx as u16;
    ppu.palette.write(addr, color);
}

fn set_universal_bg_color<R: Render>(ppu: &mut Ppu<R>, color: u8) {
    ppu.palette.write(0x3f00, color);
}
fn fill_chr<R: Render>(ppu: &mut Ppu<R>, pattern: &[u8], cartridge: &mut Cartridge) {
    for i in 0..0x2000 {
        ppu.write_vram(i as u16, pattern[i % pattern.len()], cartridge);
    }
}

fn populate_sprite_secondary_oam<R: Render>(ppu: &mut Ppu<R>, target_scanline: u16) {
    // Hardware evaluates sprites for the *next* scanline during dots 65-256 of
    // each visible scanline only. The pre-render line (261) evaluates nothing
    // (nesdev wiki: "Sprite evaluation does not happen on the pre-render
    // scanline"), so scanline 0's secondary OAM is empty: no eval to simulate.
    if target_scanline == 0 {
        return;
    }
    let eval_scanline = target_scanline - 1;
    ppu.sprite.begin_sprite_overflow_eval(ppu.oam_addr);
    for dot in (65..=256).filter(|d| d % 2 == 1) {
        ppu.timing.dot = dot;
        ppu.timing.scanline = eval_scanline;
        ppu.sprite
            .step_sprite_overflow_eval(eval_scanline, ppu.registers.ctrl, &ppu.oam);
    }
    ppu.sprite.swap_secondary_oam();
}

fn render_pixel<R: Render>(ppu: &mut Ppu<R>, pattern: &[u8], x: u8, y: u8) -> u8 {
    let mut cart = Cartridge::test();
    if !pattern.is_empty() {
        fill_chr(ppu, pattern, &mut cart);
    }
    if ppu.registers.mask.sprite_enabled() {
        populate_sprite_secondary_oam(ppu, y as u16);
    }
    ppu.timing.scanline = y as u16;
    ppu.render_pixel(x, &cart)
}

fn run_scanline<R: Render>(ppu: &mut Ppu<R>, pattern: &[u8], scanline: u16) {
    let mut cart = Cartridge::test();
    if !pattern.is_empty() {
        fill_chr(ppu, pattern, &mut cart);
    }

    ppu.timing.scanline = scanline;
    ppu.timing.dot = 0;
    for _ in 0..341 {
        ppu.tick(&mut cart);
    }
}

/// Like create_test_ppu_with_mask, but with a readable 1x framebuffer so a
/// test can assert the pixels a ticked scanline committed.
fn create_test_ppu_render(mask: PpuMask) -> Ppu<ImageRender<1>> {
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    ppu.registers.mask = mask;
    for i in 0..64 {
        ppu.oam.set_byte(i * 4, 0x20);
        ppu.oam.set_byte(i * 4 + 3, 0xff);
    }
    ppu.palette.data = [0; 0x20];
    ppu
}

/// Populate secondary OAM for the target scanline, then run the whole visible
/// line dot by dot so its pixels reach the (deferred) buffer. Starts at
/// dot 1: the dot-0 swap belongs to the line boundary and must not discard
/// the freshly evaluated Sprites. Afterwards the timing fast-forwards to
/// the VBL set dot, which flushs the frame through the Render seam.
fn run_visible_scanline<R: Render>(ppu: &mut Ppu<R>, cart: &mut Cartridge, scanline: u16) {
    populate_sprite_secondary_oam(ppu, scanline);
    ppu.timing.scanline = scanline;
    ppu.timing.dot = 1;
    for _ in 1..=340 {
        ppu.tick(cart);
    }
    flush_frame_at_vblank(ppu, cart);
}

/// Fast-forward timing to the VBL set dot (scanline 241, dots 0-1) so the
/// buffered frame flushs through the Render seam. Tests asserting one
/// scanline use this instead of ticking the intervening lines.
fn flush_frame_at_vblank<R: Render>(ppu: &mut Ppu<R>, cart: &mut Cartridge) {
    ppu.timing.scanline = 241;
    ppu.timing.dot = 0;
    ppu.tick(cart);
    ppu.tick(cart);
}

fn render_pixel_with_setup<R: Render, F>(
    ppu: &mut Ppu<R>,
    pattern: &[u8],
    setup: F,
    x: u8,
    y: u8,
) -> u8
where
    F: FnOnce(&mut Ppu<R>, &mut Cartridge),
{
    let mut cart = Cartridge::test();
    if !pattern.is_empty() {
        fill_chr(ppu, pattern, &mut cart);
    }
    setup(ppu, &mut cart);
    if ppu.registers.mask.sprite_enabled() {
        populate_sprite_secondary_oam(ppu, y as u16);
    }
    ppu.timing.scanline = y as u16;
    ppu.render_pixel(x, &cart)
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
    assert_eq!(pixel, 0x16);
}

/// The PPU reaches the cartridge for pattern bytes through
/// `Cartridge::read_vram`; the test recorder makes that traffic visible from a
/// PPU test, in read order. Ticket #44 builds its sprite pattern-read
/// assertion on this seam.
#[test]
fn ppu_background_fetch_logs_cartridge_vram_reads_in_order() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_background_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    let (mut cart, recorder) = Cartridge::test_recording_all();
    fill_chr(&mut ppu, &pattern, &mut cart);
    set_bg_tile(&mut ppu, &mut cart, 0, 0);
    recorder.vram_reads.clear();

    ppu.timing.scanline = 0;
    ppu.render_pixel(0, &cart);

    assert_eq!(
        vec![0x2000, 0x23c0, 0x0000, 0x0008],
        recorder.vram_reads.addresses(),
        "nametable, attribute, then the tile's two pattern planes"
    );
    assert_eq!(4, recorder.vram_reads.count());
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
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    let mut cart = Cartridge::test();
    ppu.registers.vram_addr = 0x3f10;
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;

    ppu.tick(&mut cart);
    ppu.tick(&mut cart);
    ppu.tick(&mut cart);
    // Visible dots only buffer palette indices now; the frame flushs at
    // the VBL set dot.
    flush_frame_at_vblank(&mut ppu, &mut cart);
    // $3F10 mirrors $3F00, whose power-up entry is $09.
    assert_eq!(ppu.renderer.get_pixel(0, 0), ppu.color_theme.color(0x09).0);
}

#[test]
fn test_tick_renders_background_color_when_rendering_disabled_and_vram_not_palette() {
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    let mut cart = Cartridge::test();
    ppu.palette.write(0x3f00, 0x16);
    ppu.registers.vram_addr = 0x2000;
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;

    ppu.tick(&mut cart);
    ppu.tick(&mut cart);
    ppu.tick(&mut cart);
    // The pixel is buffered on its dot and flushs at the VBL set dot.
    flush_frame_at_vblank(&mut ppu, &mut cart);
    assert_eq!(ppu.renderer.get_pixel(0, 0), ppu.color_theme.color(0x16).0);
}

#[test_case(0x3f, 0x30 ; "black mirror entry renders white")]
#[test_case(0x0f, 0x00 ; "black entry renders gray")]
fn test_tick_grayscale_masks_palette_index_before_lookup(entry: u8, expected: u8) {
    // Hardware grayscale ANDs the 6-bit palette entry with $30 before the
    // color lookup; it is not a luminance filter on the output RGB. Entry
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    let mut cart = Cartridge::test();
    ppu.palette.write(0x3f00, entry);
    ppu.registers.vram_addr = 0x2000;
    ppu.registers.mask = PpuMask::new().with_grayscale(true);
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;

    ppu.tick(&mut cart);
    ppu.tick(&mut cart);
    ppu.tick(&mut cart);
    // The pixel is buffered on its dot and flushs at the VBL set dot, where
    // the grayscale mask applies to the indexed entry.
    flush_frame_at_vblank(&mut ppu, &mut cart);
    assert_eq!(
        ppu.renderer.get_pixel(0, 0),
        ppu.color_theme.color(expected).0
    );
}

#[test]
fn test_reset_clears_buffered_palette_indices() {
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    let mut cart = Cartridge::test();
    ppu.palette.write(0x3f00, 0x21);
    ppu.registers.vram_addr = 0x3f10;

    // Land mid-frame on a visible scanline and buffer the first three
    // visible dots' pixels (x = 0, 1, 2). With deferred color nothing has
    // reached the framebuffer yet.
    ppu.timing.scanline = 100;
    ppu.timing.dot = 0;
    ppu.tick(&mut cart);
    ppu.tick(&mut cart);
    ppu.tick(&mut cart);
    assert_eq!(ppu.renderer.get_pixel(0, 100), [0, 0, 0, 0]);
    assert_eq!(ppu.renderer.get_pixel(1, 100), [0, 0, 0, 0]);

    ppu.reset();

    // Post-reset rendering uses a different backdrop color so stale and
    // fresh output are distinguishable. Buffer a few more dots (x = 2, 3).
    ppu.palette.write(0x3f00, 0x16);
    ppu.tick(&mut cart);
    ppu.tick(&mut cart);

    flush_frame_at_vblank(&mut ppu, &mut cart);

    // The pre-reset buffered indices must never flush into the fresh
    // frame: positions not rewritten since the reset flush black
    // (matching the cleared pre-change framebuffer)...
    assert_eq!(ppu.renderer.get_pixel(0, 100), [0, 0, 0, 0]);
    assert_eq!(ppu.renderer.get_pixel(1, 100), [0, 0, 0, 0]);
    // x=2 was rewritten after the reset, so it flushes normally.
    assert_eq!(
        ppu.renderer.get_pixel(2, 100),
        ppu.color_theme.color(0x16).0
    );
    assert_ne!(
        ppu.renderer.get_pixel(1, 100),
        ppu.color_theme.color(0x21).0
    );
    // ...while positions written after the reset flush normally.
    assert_eq!(
        ppu.renderer.get_pixel(3, 100),
        ppu.color_theme.color(0x16).0
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

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
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

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
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

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
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

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
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

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
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

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        1,
    );
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

    let pixel = render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        0,
    );
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

/// Ticket #44 acceptance: a fully populated scanline fetches each evaluated
/// Sprite's two pattern planes exactly once. The row cache makes 8 x 2
/// pattern-table reads, and the recorded bytes still decode to the pixels the
/// framebuffer holds.
#[test]
fn sprite_row_cache_reads_each_evaluated_sprite_once_per_scanline() {
    let mut ppu = create_test_ppu_render(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_universal_bg_color(&mut ppu, 0x0f);
    for color_idx in 1..=3u8 {
        set_sprite_palette_color(&mut ppu, 0, color_idx, 0x20 + color_idx);
    }
    // Eight Sprites side by side, each with its own tile and a distinct
    // row-0 bit pair, so each Sprite owns exactly its 8-pixel window.
    for i in 0..8u8 {
        let tile = i + 1;
        let low = 0b1010_1010 ^ i.wrapping_mul(0x11);
        let high = 0b1100_0011 ^ (i << 1);
        pattern[tile as usize * 16] = low;
        pattern[tile as usize * 16 + 8] = high;
        setup_sprite(&mut ppu, i, 0, tile, 0, i * 8);
    }

    let (mut cart, recorder) = Cartridge::test_recording_all();
    fill_chr(&mut ppu, &pattern, &mut cart);
    recorder.vram_reads.clear();

    run_visible_scanline(&mut ppu, &mut cart, 1);

    let pattern_reads: Vec<u16> = recorder
        .vram_reads
        .addresses()
        .into_iter()
        .filter(|addr| *addr < 0x2000)
        .collect();
    let expected_addresses: Vec<u16> = (0..8u16)
        .flat_map(|i| {
            let base = (i + 1) * 16;
            [base, base + 8]
        })
        .collect();
    assert_eq!(
        expected_addresses, pattern_reads,
        "one low+high pair per evaluated Sprite, in Secondary OAM order"
    );

    // The addresses the cache read still decode to the framebuffer pixels.
    let backdrop = ppu.color_theme.color(0x0f).0;
    for i in 0..8usize {
        let low = cart.read_vram(pattern_reads[i * 2], &PpuRenderState::disabled());
        let high = cart.read_vram(pattern_reads[i * 2 + 1], &PpuRenderState::disabled());
        for dx in 0..8u8 {
            let bit = 7 - dx;
            let color_idx = ((low >> bit) & 1) | (((high >> bit) & 1) << 1);
            let expected = if color_idx == 0 {
                backdrop
            } else {
                ppu.color_theme.color(0x20 + color_idx).0
            };
            let x = i as u32 * 8 + dx as u32;
            assert_eq!(
                expected,
                ppu.renderer.get_pixel(x, 1),
                "sprite {i} pixel dx={dx} must decode from the recorded bytes"
            );
        }
    }
    assert_eq!(backdrop, ppu.renderer.get_pixel(255, 1));
}

#[test]
fn scanline_renders_plain_sprite_from_cached_row() {
    let mut ppu = create_test_ppu_render(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_universal_bg_color(&mut ppu, 0x0f);
    set_tile_solid(&mut pattern, 0, 1, 2);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x22);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 4);
    let mut cart = Cartridge::test();
    fill_chr(&mut ppu, &pattern, &mut cart);

    run_visible_scanline(&mut ppu, &mut cart, 1);

    let sprite_color = ppu.color_theme.color(0x22).0;
    let backdrop = ppu.color_theme.color(0x0f).0;
    for x in 4..12u32 {
        assert_eq!(sprite_color, ppu.renderer.get_pixel(x, 1), "x={x}");
    }
    assert_eq!(backdrop, ppu.renderer.get_pixel(3, 1));
    assert_eq!(backdrop, ppu.renderer.get_pixel(12, 1));
}

#[test]
fn scanline_renders_horizontally_flipped_sprite() {
    let mut ppu = create_test_ppu_render(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_universal_bg_color(&mut ppu, 0x0f);
    // Only the leftmost pixel of the tile row is opaque; h-flip must move it
    // to the Sprite's rightmost screen pixel.
    set_tile_pixel(&mut pattern, 0, 1, 0, 0, 2);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x22);
    setup_sprite(&mut ppu, 0, 0, 1, 0x40, 4);
    let mut cart = Cartridge::test();
    fill_chr(&mut ppu, &pattern, &mut cart);

    run_visible_scanline(&mut ppu, &mut cart, 1);

    let sprite_color = ppu.color_theme.color(0x22).0;
    let backdrop = ppu.color_theme.color(0x0f).0;
    assert_eq!(backdrop, ppu.renderer.get_pixel(4, 1));
    assert_eq!(sprite_color, ppu.renderer.get_pixel(11, 1));
}

#[test]
fn scanline_renders_vertically_flipped_sprite() {
    let mut ppu = create_test_ppu_render(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_universal_bg_color(&mut ppu, 0x0f);
    // Only the bottom tile row (row 7) is opaque; v-flip must select it for
    // the Sprite's top screen row (screen row 1 -> src row 7).
    set_universal_bg_color(&mut ppu, 0x21);
    set_tile_pixel(&mut pattern, 0, 1, 0, 7, 2);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x22);
    setup_sprite(&mut ppu, 0, 0, 1, 0x80, 4);
    let mut cart = Cartridge::test();
    fill_chr(&mut ppu, &pattern, &mut cart);

    run_visible_scanline(&mut ppu, &mut cart, 1);
    run_visible_scanline(&mut ppu, &mut cart, 2);

    let sprite_color = ppu.color_theme.color(0x22).0;
    let backdrop = ppu.color_theme.color(0x21).0;
    assert_eq!(sprite_color, ppu.renderer.get_pixel(4, 1));
    assert_eq!(backdrop, ppu.renderer.get_pixel(4, 2));
}

#[test]
fn scanline_renders_doubly_flipped_sprite() {
    let mut ppu = create_test_ppu_render(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_universal_bg_color(&mut ppu, 0x0f);
    set_tile_pixel(&mut pattern, 0, 1, 0, 7, 2);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x22);
    setup_sprite(&mut ppu, 0, 0, 1, 0xc0, 4);
    let mut cart = Cartridge::test();
    fill_chr(&mut ppu, &pattern, &mut cart);

    run_visible_scanline(&mut ppu, &mut cart, 1);

    let sprite_color = ppu.color_theme.color(0x22).0;
    let backdrop = ppu.color_theme.color(0x0f).0;
    assert_eq!(sprite_color, ppu.renderer.get_pixel(11, 1));
    assert_eq!(backdrop, ppu.renderer.get_pixel(4, 1));
}

#[test]
fn scanline_renders_8x16_sprite_second_tile() {
    let mut ppu = create_test_ppu_render(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_universal_bg_color(&mut ppu, 0x0f);
    set_tile_solid(&mut pattern, 0, 1, 3);
    set_sprite_palette_color(&mut ppu, 0, 3, 0x2b);
    ppu.set_control_flags(PpuCtrl::new().with_sprite_size_16(true));
    setup_sprite(&mut ppu, 0, 0, 0, 0, 4);
    let mut cart = Cartridge::test();
    fill_chr(&mut ppu, &pattern, &mut cart);

    // Sprite Y=0 covers screen rows 1..16; row 9 uses the second tile.
    run_visible_scanline(&mut ppu, &mut cart, 9);

    let sprite_color = ppu.color_theme.color(0x2b).0;
    for x in 4..12u32 {
        assert_eq!(sprite_color, ppu.renderer.get_pixel(x, 9), "x={x}");
    }
}

#[test]
fn scanline_renders_sprite_behind_background() {
    let mut ppu = create_test_ppu_render(
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
    setup_sprite(&mut ppu, 0, 0, 1, 0x20, 4);
    let mut cart = Cartridge::test();
    fill_chr(&mut ppu, &pattern, &mut cart);
    set_bg_tile(&mut ppu, &mut cart, 0, 0);

    run_visible_scanline(&mut ppu, &mut cart, 1);

    let bg_color = ppu.color_theme.color(0x14).0;
    for x in 4..12u32 {
        assert_eq!(bg_color, ppu.renderer.get_pixel(x, 1), "x={x}");
    }
}

#[test]
fn scanline_clips_sprite_left_columns() {
    let mut ppu = create_test_ppu_render(PpuMask::new().with_sprite_enabled(true));
    let mut pattern = create_pattern();
    set_universal_bg_color(&mut ppu, 0x0f);
    set_tile_solid(&mut pattern, 0, 1, 2);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x30);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 4);
    let mut cart = Cartridge::test();
    fill_chr(&mut ppu, &pattern, &mut cart);

    run_visible_scanline(&mut ppu, &mut cart, 1);

    let sprite_color = ppu.color_theme.color(0x30).0;
    let backdrop = ppu.color_theme.color(0x0f).0;
    // Sprite covers x=4..11; without sprite_left_enabled columns 0..7 clip.
    for x in 4..8u32 {
        assert_eq!(backdrop, ppu.renderer.get_pixel(x, 1), "clipped x={x}");
    }
    for x in 8..12u32 {
        assert_eq!(sprite_color, ppu.renderer.get_pixel(x, 1), "drawn x={x}");
    }
}

#[test]
fn scanline_first_opaque_sprite_wins() {
    let mut ppu = create_test_ppu_render(
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
    let mut cart = Cartridge::test();
    fill_chr(&mut ppu, &pattern, &mut cart);

    run_visible_scanline(&mut ppu, &mut cart, 1);

    assert_eq!(
        ppu.color_theme.color(0x26).0,
        ppu.renderer.get_pixel(0, 1),
        "the earlier opaque Sprite in Secondary OAM order wins"
    );
}

#[test]
fn scanline_sets_sprite_zero_hit_from_cached_row() {
    let mut ppu = create_test_ppu_render(
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
    let mut cart = Cartridge::test();
    fill_chr(&mut ppu, &pattern, &mut cart);
    set_bg_tile(&mut ppu, &mut cart, 0, 0);

    run_visible_scanline(&mut ppu, &mut cart, 1);

    assert!(
        ppu.registers.status.sprite_zero_hit(),
        "opaque OAM entry 0 over opaque background must set the hit"
    );
    assert_eq!(
        ppu.color_theme.color(0x24).0,
        ppu.renderer.get_pixel(0, 1),
        "the in-front Sprite shows through"
    );
}

#[test]
fn ctrl_write_invalidates_sprite_row_cache() {
    let mut ppu = create_test_ppu_render(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 1, 1);
    set_tile_solid(&mut pattern, 1, 1, 2);
    set_sprite_palette_color(&mut ppu, 0, 1, 0x2a);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x2b);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);
    let mut cart = Cartridge::test();
    fill_chr(&mut ppu, &pattern, &mut cart);
    populate_sprite_secondary_oam(&mut ppu, 1);
    ppu.timing.scanline = 1;

    // First pixel fills the row cache from sprite pattern table 0.
    assert_eq!(0x2a, ppu.render_pixel(0, &cart));
    // A $2000 write must drop the cache so the same scanline reads the new
    // sprite pattern table.
    ppu.write_ppureg(0x2000, 0x08, &mut cart);
    assert_eq!(
        0x2b,
        ppu.render_pixel(1, &cart),
        "a $2000 write must invalidate the cached Sprite row"
    );
}

#[test]
fn ppudata_pattern_write_invalidates_sprite_row_cache() {
    let mut ppu = create_test_ppu_render(
        PpuMask::new()
            .with_sprite_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 1, 1);
    set_sprite_palette_color(&mut ppu, 0, 1, 0x2a);
    set_sprite_palette_color(&mut ppu, 0, 3, 0x2c);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);
    let mut cart = Cartridge::test();
    fill_chr(&mut ppu, &pattern, &mut cart);
    populate_sprite_secondary_oam(&mut ppu, 1);
    ppu.timing.scanline = 1;

    assert_eq!(0x2a, ppu.render_pixel(0, &cart));
    // Rewrite tile 1 row 0 through PPUDATA: both planes all ones => color 3.
    for (addr, value) in [(0x0010u16, 0xffu8), (0x0018, 0xff)] {
        ppu.write_ppureg(0x2006, (addr >> 8) as u8, &mut cart);
        ppu.write_ppureg(0x2006, addr as u8, &mut cart);
        ppu.write_ppureg(0x2007, value, &mut cart);
    }
    assert_eq!(
        0x2c,
        ppu.render_pixel(1, &cart),
        "a PPUDATA pattern write must invalidate the cached Sprite row"
    );
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
fn ppu_tick_detects_sprite_overflow_on_odd_dots() {
    // Hardware reads OAM during sprite evaluation on odd dots; the overflow
    // flag therefore first becomes visible right after an odd evaluation
    // step. Sprite overflow timing ROMs pin this phase (3.Timing #13/#14).
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true),
    );
    let mut cart = Cartridge::test();

    for idx in 0..9 {
        setup_sprite(&mut ppu, idx, 20, 0, 0, idx * 8);
    }

    ppu.timing.scanline = 20;
    ppu.timing.dot = 64;
    let mut saw_overflow = false;
    for _ in 65..=256 {
        let processed_dot = ppu.timing.dot;
        ppu.tick(&mut cart);
        if !saw_overflow && ppu.registers.status.sprite_overflow() {
            saw_overflow = true;
            // Pending overflow applies at the top of the tick following the
            // evaluation step, so the detecting tick is processed_dot - 1.
            let detection_dot = processed_dot - 1;
            assert_eq!(
                detection_dot % 2,
                1,
                "overflow must be detected on an odd eval dot"
            );
            break;
        }
    }
    assert!(saw_overflow, "nine in-range sprites must set overflow");
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
    render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| {
            ppu.write_vram(0x2001, 0, cart);
        },
        8,
        1,
    );
    // No latch step: the hit raises PPUSTATUS on the dot that outputs the
    // hit pixel (blargg ppu_sprite_hit 09-timing sub-tests 3/4).
    assert!(ppu.registers.status.sprite_zero_hit());
}

#[test]
fn sprite_status_clears_one_dot_before_vblank() {
    let mut ppu = create_test_ppu_with_mask(PpuMask::new());
    let mut cart = Cartridge::test();

    ppu.registers.status.set_sprite_zero_hit(true);
    ppu.registers.status.set_sprite_overflow(true);
    ppu.registers.status.set_v_blank(true);

    // Pre-render dot 0 drops the sprite status bits ...
    ppu.timing.scanline = 261;
    ppu.timing.dot = 0;
    ppu.tick(&mut cart);
    assert!(!ppu.registers.status.sprite_zero_hit());
    assert!(!ppu.registers.status.sprite_overflow());
    assert!(
        ppu.registers.status.v_blank(),
        "vblank clears one dot after the sprite bits"
    );

    // ... and dot 1 clears vblank (blargg vbl_nmi_timing 4.vbl_clear_timing).
    ppu.tick(&mut cart);
    assert!(!ppu.registers.status.v_blank());
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
fn test_render_pixel_sprite_zero_hit_requires_evaluation() {
    // Hardware: the hit only comes from a sprite 0 that evaluation picked.
    // With OAMADDR=1 and sprites 1..=9 in range, sprite 0 is never
    // evaluated (8-sprite limit trips first) — no hit despite opaque
    // sprite-0/BG overlap.
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
    setup_sprite(&mut ppu, 0, 20, 1, 0, 0);
    for i in 1..=9u8 {
        setup_sprite(&mut ppu, i, 20, 1, 0, 0);
    }
    ppu.oam_addr = 1;

    render_pixel_with_setup(
        &mut ppu,
        &pattern,
        |ppu, cart| set_bg_tile(ppu, cart, 0, 0),
        0,
        21,
    );
    assert!(!ppu.registers.status.sprite_zero_hit());
}

#[test]
fn test_evaluation_starts_at_oamaddr_and_wraps() {
    let mut ppu = create_test_ppu_with_mask(PpuMask::new());
    for i in 0..=9u8 {
        setup_sprite(&mut ppu, i, 20, 0, 0, 0);
    }
    ppu.oam_addr = 1;
    populate_sprite_secondary_oam(&mut ppu, 21);

    // Sprites 1..=8 fill the secondary OAM; the 9th in-range sprite trips
    // overflow before the wrap ever reaches OAM entry 0.
    assert_eq!(ppu.sprite.secondary_oam_len(), 8);
    assert!(ppu.sprite.current_zero_sprite().is_none());
}

#[test_case(0; "visible scanline")]
#[test_case(261; "pre-render scanline")]
fn test_oamaddr_is_zeroed_during_sprite_fetches(scanline: u16) {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true),
    );
    let mut cart = Cartridge::test();
    ppu.timing.scanline = scanline;
    ppu.timing.dot = 256;
    ppu.write_ppureg(0x2003, 0x50, &mut cart);
    assert_eq!(ppu.oam_addr, 0x50);
    // Tick 1 processes dot 256 (last background fetch) and leaves
    // OAMADDR alone; the tick that processes dot 257 — the start of the
    // sprite tile-fetch window — drives it to 0, on visible lines and
    // the pre-render line alike.
    ppu.tick(&mut cart);
    assert_eq!(ppu.oam_addr, 0x50);
    ppu.tick(&mut cart);
    assert_eq!(ppu.oam_addr, 0);
}

/// Hardware (nesdev PPU registers §OAMDATA): with background or sprite
/// rendering enabled, $2004 writes on the pre-render line and visible
/// lines 0-239 are ignored — OAM is untouched — while OAMADDR does a
/// glitchy +4 that bumps only its high 6 bits, leaving the low 2 bits
/// unchanged. The ramp makes the observed byte equal to the address, so an
/// unanchored $2004 read pins OAMADDR's new value exactly — parked in
/// hblank (dot 340), since mid-render reads expose eval/fetch traffic,
/// not primary OAM.
#[test_case(0,   0x02, 0x06; "visible scanline, addr 2 -> 6")]
#[test_case(0,   0x03, 0x07; "visible scanline, low 2 bits preserved")]
#[test_case(0,   0xFC, 0x00; "visible scanline, high 6 bits wrap")]
#[test_case(261, 0x02, 0x06; "pre-render line, addr 2 -> 6")]
#[test_case(261, 0xFF, 0x03; "pre-render line, addr FF -> 03")]
fn test_oam_data_write_during_rendering_ignored_with_glitchy_oamaddr(
    scanline: u16,
    oamaddr: u8,
    glitched: u8,
) {
    let mut ppu = ppu_with_ramp_oam(PpuMask::new().with_background_enabled(true));
    ppu.timing.scanline = scanline;
    ppu.timing.dot = 100;
    let mut cart = Cartridge::test();

    ppu.write_ppureg(0x2003, oamaddr, &mut cart);
    ppu.write_ppureg(0x2004, 0xEE, &mut cart);
    // Park in hblank for the probe reads: mid-render $2004 reads expose
    // eval/fetch traffic, not primary OAM, so OAMADDR is pinned from dot 340.
    ppu.timing.dot = 340;
    // OAMADDR glitched to the +4 address: an unanchored $2004 read
    // (reads do not move OAMADDR) returns the ramp byte there.
    assert_eq!(ppu.read_ppureg(0x2004, &mut cart), glitched);
    // The write itself was dropped: anchoring back to the original
    // address reads the intact ramp byte, not 0xEE.
    assert_eq!(read_oam_at(&mut ppu, oamaddr), oamaddr);
}

#[test]
fn test_oam_data_write_during_sprite_only_rendering_also_glitches() {
    let mut ppu = ppu_with_ramp_oam(PpuMask::new().with_sprite_enabled(true));
    ppu.timing.scanline = 100;
    ppu.timing.dot = 100;
    let mut cart = Cartridge::test();

    ppu.write_ppureg(0x2003, 0x02, &mut cart);
    ppu.write_ppureg(0x2004, 0xEE, &mut cart);
    // Park in hblank: mid-render reads expose traffic, not primary OAM.
    ppu.timing.dot = 340;
    assert_eq!(ppu.read_ppureg(0x2004, &mut cart), 0x06);
    assert_eq!(read_oam_at(&mut ppu, 0x02), 0x02);
}

#[test]
fn test_oam_data_write_outside_render_windows_takes_effect() {
    // Writes land normally during vblank even with rendering enabled, and
    // on active lines once rendering is disabled: the ignore only applies
    // on the pre-render/visible lines while a renderer is enabled.
    let mut cart = Cartridge::test();

    // Rendering enabled but in vblank: normal write.
    let mut ppu = ppu_with_ramp_oam(PpuMask::new().with_background_enabled(true));
    ppu.timing.scanline = 241;
    ppu.timing.dot = 10;
    ppu.write_ppureg(0x2003, 0x04, &mut cart);
    ppu.write_ppureg(0x2004, 0xEE, &mut cart);
    assert_eq!(ppu.read_ppureg(0x2004, &mut cart), 0x05);
    assert_eq!(read_oam_at(&mut ppu, 0x04), 0xEE);

    // Rendering disabled on a visible line: normal write.
    let mut ppu = ppu_with_ramp_oam(PpuMask::new());
    ppu.timing.scanline = 100;
    ppu.timing.dot = 100;
    ppu.write_ppureg(0x2003, 0x08, &mut cart);
    ppu.write_ppureg(0x2004, 0xEE, &mut cart);
    assert_eq!(read_oam_at(&mut ppu, 0x08), 0xEE);
    // Attribute-byte normalization (addr & 3 == 2) still applies.
    ppu.write_ppureg(0x2003, 0x0A, &mut cart);
    ppu.write_ppureg(0x2004, 0xFF, &mut cart);
    // 0xFF & 0xE3 == 0xE3
    assert_eq!(read_oam_at(&mut ppu, 0x0A), 0xE3);
    // Non-attribute addresses stay unmasked.
    ppu.write_ppureg(0x2003, 0x0C, &mut cart);
    ppu.write_ppureg(0x2004, 0xFF, &mut cart);
    assert_eq!(read_oam_at(&mut ppu, 0x0C), 0xFF);
}

/// Drive the PPU from dot 0 of `scanline` to `dot`: after the call
/// `timing.dot == dot` and dots 0..dot have been processed (evaluation,
/// fetches, OAMADDR forcing — the full per-dot pipeline).
fn tick_to_dot(ppu: &mut Ppu, scanline: u16, dot: u16) {
    let mut cart = Cartridge::test();
    ppu.timing.scanline = scanline;
    ppu.timing.dot = 0;
    for _ in 0..dot {
        ppu.tick(&mut cart);
    }
}

/// Read $2004 without advancing the PPU clock (register reads never move
/// timing; the dot under test is whatever the driver parked beforehand).
fn read_oamdata_now(ppu: &mut Ppu) -> u8 {
    let mut cart = Cartridge::test();
    ppu.read_ppureg(0x2004, &mut cart)
}

/// Seed all 64 sprites with one in-range sprite 0 (Y=20 covers line 21)
/// and every other Y parked at $FF (out of range for line 21).
fn seed_single_sprite_for_line_21(ppu: &mut Ppu) {
    for i in 0..64u8 {
        setup_sprite(ppu, i, 0xFF, 0x00, 0x00, 0x00);
    }
    setup_sprite(ppu, 0, 20, 0xAA, 0x55, 0x33);
}

/// Dots 1-64 with rendering on expose the secondary-OAM clear ($FF), not
/// primary OAM — even with a ramp behind OAMADDR.
#[test]
fn test_oamdata_read_clear_phase_returns_ff() {
    let mut ppu = ppu_with_ramp_oam(PpuMask::new().with_background_enabled(true));
    for dot in [1u16, 32, 64] {
        tick_to_dot(&mut ppu, 20, dot);
        assert_eq!(read_oamdata_now(&mut ppu), 0xFF, "dot {dot}");
    }
}

/// Dots 65-256 with rendering on expose the evaluation copy buffer: the
/// last primary-OAM byte latched, walking Y → tile → attr → X as sprite 0
/// is copied, then the next sprite's Y once it is done.
#[test]
fn test_oamdata_read_eval_returns_copy_buffer() {
    let mut ppu = create_test_ppu_with_mask(PpuMask::new().with_sprite_enabled(true));
    seed_single_sprite_for_line_21(&mut ppu);

    // Dot 65 scans sprite 0's Y (in range for line 21) into the buffer.
    tick_to_dot(&mut ppu, 20, 66);
    assert_eq!(read_oamdata_now(&mut ppu), 20);
    // Dots 67/69/71 copy tile, attributes, X.
    tick_to_dot(&mut ppu, 20, 68);
    assert_eq!(read_oamdata_now(&mut ppu), 0xAA);
    tick_to_dot(&mut ppu, 20, 70);
    assert_eq!(read_oamdata_now(&mut ppu), 0x55);
    tick_to_dot(&mut ppu, 20, 72);
    assert_eq!(read_oamdata_now(&mut ppu), 0x33);
    // Sprite 0 is fully copied: the next odd dot scans sprite 1's Y ($FF).
    tick_to_dot(&mut ppu, 20, 74);
    assert_eq!(read_oamdata_now(&mut ppu), 0xFF);
}

/// Dots 257-320 with rendering on expose secondary OAM at the fetch
/// position `(dot-257)/8*4 + min((dot-257)%8, 3)`: bytes 0-3 walk Y, tile,
/// attr, X and dots 4-7 of each slot repeat X; unwritten slots read $FF.
/// Reads are side-effect free — the buffer is intact afterwards.
#[test]
fn test_oamdata_read_fetch_returns_secondary_at_position() {
    let mut ppu = create_test_ppu_with_mask(PpuMask::new().with_sprite_enabled(true));
    seed_single_sprite_for_line_21(&mut ppu);
    tick_to_dot(&mut ppu, 20, 321);
    assert_eq!(ppu.sprite.next_scanline_oam_len(), 1);
    for (dot, expected) in [
        (257u16, 20u8),
        (258, 0xAA),
        (259, 0x55),
        (260, 0x33),
        (261, 0x33),
        (264, 0x33),
        (265, 0xFF),
        (320, 0xFF),
    ] {
        ppu.timing.dot = dot;
        assert_eq!(read_oamdata_now(&mut ppu), expected, "dot {dot}");
    }
    // The probe reads mutated nothing: same bytes, same buffer length.
    ppu.timing.dot = 257;
    assert_eq!(read_oamdata_now(&mut ppu), 20);
    assert_eq!(ppu.sprite.next_scanline_oam_len(), 1);
}

/// Scanline 0 builds from an empty buffer (no evaluation ran on the
/// pre-render line per #33): with every Y parked out of range for line 1,
/// the clear, eval and fetch windows all read $FF.
#[test]
fn test_oamdata_read_scanline_zero_empty_copy_buffer() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true),
    );
    for i in 0..64u8 {
        setup_sprite(&mut ppu, i, 0xFF, 0x11, 0x22, 0x33);
    }
    tick_to_dot(&mut ppu, 0, 30);
    assert_eq!(read_oamdata_now(&mut ppu), 0xFF);
    tick_to_dot(&mut ppu, 0, 100);
    assert_eq!(read_oamdata_now(&mut ppu), 0xFF);
    tick_to_dot(&mut ppu, 0, 321);
    for dot in [257u16, 260, 280, 320] {
        ppu.timing.dot = dot;
        assert_eq!(read_oamdata_now(&mut ppu), 0xFF, "dot {dot}");
    }
}

/// Rendering disabled — or blanking lines (vblank, hblank dots, the
/// pre-render line) even with rendering enabled — keeps the
/// primary-OAM-at-OAMADDR behavior on every dot.
#[test]
fn test_oamdata_read_outside_rendering_returns_primary_oam() {
    let mut ppu = ppu_with_ramp_oam(PpuMask::new());
    for dot in [0u16, 10, 100, 260, 321, 340] {
        ppu.timing.scanline = 20;
        ppu.timing.dot = dot;
        assert_eq!(read_oam_at(&mut ppu, 0x42), 0x42, "dot {dot}");
    }
    let mut ppu = ppu_with_ramp_oam(PpuMask::new().with_background_enabled(true));
    for (scanline, dot) in [(241u16, 10u16), (240, 100), (261, 100), (20, 0), (20, 340)] {
        ppu.timing.scanline = scanline;
        ppu.timing.dot = dot;
        assert_eq!(
            read_oam_at(&mut ppu, 0x42),
            0x42,
            "line {scanline} dot {dot}"
        );
    }
}

#[test]
fn test_pre_render_line_skips_sprite_evaluation() {
    // Hardware performs no sprite evaluation on the pre-render scanline
    // (nesdev wiki). The +1 Y offset means target-line 0 can never be
    // populated, so this pins the gate contract itself rather than a
    // pixel-visible effect: neither buffer may gain entries from the
    // 261 eval window (a future change to target math or eval side
    // effects would surface here).
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true),
    );
    let mut cart = Cartridge::test();
    for idx in 0..=9u8 {
        setup_sprite(&mut ppu, idx, 0, 0, 0, idx * 8);
    }

    ppu.timing.scanline = 261;
    ppu.timing.dot = 64;
    // Process dots 65..=256 of scanline 261: the next-scanline eval
    // window must not run here.
    for _ in 65..=256 {
        ppu.tick(&mut cart);
    }
    // Nothing was copied into either buffer: the dot-0 swap crossed an
    // already-empty next-buffer, so the live line is empty too.
    assert_eq!(ppu.sprite.next_scanline_oam_len(), 0);
    assert_eq!(ppu.sprite.secondary_oam_len(), 0);
}

#[test]
fn test_scanline_zero_renders_no_freshly_evaluated_sprites() {
    let mut ppu = create_test_ppu_with_mask(
        PpuMask::new()
            .with_background_enabled(true)
            .with_sprite_enabled(true)
            .with_background_left_enabled(true)
            .with_sprite_left_enabled(true),
    );
    let mut cart = Cartridge::test();
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);
    setup_sprite(&mut ppu, 1, 0, 1, 0, 8);

    // A sprite at Y=0 covers scanline 1 (the +1 offset), and is in range
    // for line-1 evaluation only. With no evaluation on the pre-render
    // line, scanline 0 must render nothing even though evaluation of the
    // very next line picks both sprites.
    ppu.timing.scanline = 261;
    ppu.timing.dot = 339;
    // Process dots 339-340 of 261, then render all of scanline 0 (dots
    // 0..340): its eval window (65-256) targets line 1, unused here.
    for _ in 0..343 {
        ppu.tick(&mut cart);
    }
    // Scanline 0's dot-0 swap brought in an empty next-buffer: line 0
    // has no sprites.
    assert_eq!(ppu.sprite.secondary_oam_len(), 0);
    assert!(ppu.sprite.current_zero_sprite().is_none());
    assert!(
        ppu.sprite
            .find_sprite_pixel(ppu.registers.ctrl, &cart, 0, 0, &PpuRenderState::disabled(),)
            .is_none(),
        "scanline 0 must render no sprites (empty secondary OAM)"
    );
    // Now cross scanline 0's wrap: line 1's dot-0 swap brings in the sprites
    // evaluation selected during scanline 0 — both Y=0 sprites.
    ppu.tick(&mut cart);
    assert_eq!(ppu.sprite.secondary_oam_len(), 2);
    assert!(ppu.sprite.current_zero_sprite().is_some());
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
        |ppu, cart| {
            ppu.write_vram(0x201f, 0, cart);
        },
        255,
        10,
    );
    assert!(!ppu.registers.status.sprite_zero_hit());
}

fn render_bg_pixel(mask: PpuMask) -> u8 {
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

    t.advance(t.status(), rendering_enabled);
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
fn timing_next_visible_x(scanline: u16, dot: u16, expected: bool) {
    let t = Timing {
        scanline,
        dot,
        odd_frame: false,
        frame_no: 0,
    };
    assert_eq!(t.next_visible_x().is_some(), expected);
}

#[test_case(64, false; "dot 64 is before sprite-eval dots")]
#[test_case(65, true; "dot 65 starts sprite-eval dots")]
#[test_case(256, true; "dot 256 is last sprite-eval dot")]
#[test_case(257, false; "dot 257 is outside sprite-eval dots")]
fn timing_in_sprite_eval_dots(dot: u16, expected: bool) {
    let t = Timing {
        scanline: 0,
        dot,
        odd_frame: false,
        frame_no: 0,
    };
    assert_eq!(t.in_sprite_eval_dots(), expected);
}

// --- Batch palette-index flush (ticket #62) ---------------------------------
//
// These tests observe only the `Render` seam: the sequence of `set_pixel`
// calls and the `finish` invocation per frame. They do not inspect the
// internal palette-index buffer layout or flush internals.

/// Records every `set_pixel` call in order plus the number of `finish`
/// calls, so a test can assert flush timing, raster order, and colors
/// without reaching into PPU internals.
#[derive(Debug, Default)]
struct RecordingRender {
    pixels: Vec<(u32, u32, [u8; 4])>,
    finishes: usize,
}

impl Render for RecordingRender {
    fn set_pixel(&mut self, x: u32, y: u32, color: [u8; 4]) {
        self.pixels.push((x, y, color));
    }

    fn finish(&mut self) {
        self.finishes += 1;
    }
}

/// Tick until the VBL set dot (scanline 241, dot 1) has flushed one frame.
fn tick_until_vblank(ppu: &mut Ppu<RecordingRender>, cart: &mut Cartridge) {
    for _ in 0..200_000 {
        if ppu.renderer.finishes == 1 {
            return;
        }
        ppu.tick(cart);
    }
    panic!("vblank flush did not happen within the tick budget");
}

/// Deferred color: while visible dots are still scanning, no pixel has
/// been emitted and `finish` has not fired. Exactly once per frame, at the
/// VBL set dot, the whole frame is emitted linearly in raster order
/// (x fastest, then y), each pixel resolved through the backdrop entry —
/// and a second identical frame flushs byte-identically.
#[test]
fn flush_emits_full_frame_once_at_vblank_in_raster_order() {
    let mut ppu = Ppu::new(RecordingRender::default());
    let mut cart = Cartridge::test();
    // Rendering disabled: every visible dot resolves to the backdrop entry
    // at $3F00 (vram_addr below $3F00, so `disabled_color_index` reads it).
    ppu.palette.write(0x3f00, 0x21);
    ppu.registers.vram_addr = 0x2000;

    // Scan well into the visible area: still nothing emitted.
    for _ in 0..2_000 {
        ppu.tick(&mut cart);
    }
    assert!(ppu.renderer.pixels.is_empty(), "no pixel before the flush");
    assert_eq!(ppu.renderer.finishes, 0, "no finish before the flush");

    tick_until_vblank(&mut ppu, &mut cart);

    let expected_color = ppu.color_theme.color(0x21).0;
    assert_eq!(ppu.renderer.finishes, 1, "exactly one finish per frame");
    assert_eq!(ppu.renderer.pixels.len(), 256 * 240);
    for (i, &(x, y, color)) in ppu.renderer.pixels.iter().enumerate() {
        assert_eq!(
            (x, y),
            ((i % 256) as u32, (i / 256) as u32),
            "raster order at {i}"
        );
        assert_eq!(color, expected_color, "backdrop color at {i}");
    }

    // A second static frame flushs byte-identically (same seam, same bytes).
    ppu.renderer.finishes = 0;
    ppu.renderer.pixels.clear();
    tick_until_vblank(&mut ppu, &mut cart);
    assert_eq!(ppu.renderer.finishes, 1);
    assert_eq!(ppu.renderer.pixels.len(), 256 * 240);
    for (i, &(x, y, color)) in ppu.renderer.pixels.iter().enumerate() {
        assert_eq!(
            (x, y),
            ((i % 256) as u32, (i / 256) as u32),
            "raster order at {i}"
        );
        assert_eq!(color, expected_color, "backdrop color at {i}");
    }
}

/// Static background image: the flushed frame is the per-dot geometry
/// (TileCache fetch of a solid tile per 8px column) resolved through the
/// palette, emitted in raster order. Expected colors come from the
/// literal palette entries written here, not from the PPU's own lookup.
#[test]
fn flush_emits_static_background_frame_in_raster_order() {
    let mut ppu = Ppu::new(RecordingRender::default());
    let mut cart = Cartridge::test();
    ppu.registers.mask = PpuMask::new()
        .with_background_enabled(true)
        .with_background_left_enabled(true);
    // Palette entries for bg color indices 1..=3 (palette index 0).
    ppu.palette.write(0x3f01, 0x10);
    ppu.palette.write(0x3f02, 0x20);
    ppu.palette.write(0x3f03, 0x30);
    // Tiles 0..=31 solid with color index 1, 2, 3 cycling.
    let mut pattern = create_pattern();
    for tile in 0..32u8 {
        set_tile_solid(&mut pattern, 0, tile, tile % 3 + 1);
    }
    fill_chr(&mut ppu, &pattern, &mut cart);
    // Nametable row 0: tile `i` at column `i`.
    for i in 0..32u16 {
        ppu.write_vram(0x2000 + i, i as u8, &mut cart);
    }

    tick_until_vblank(&mut ppu, &mut cart);

    let entry_for = |x: u32| -> u8 { [0x10, 0x20, 0x30][((x / 8) % 3) as usize] };
    assert_eq!(ppu.renderer.finishes, 1);
    assert_eq!(ppu.renderer.pixels.len(), 256 * 240);
    for (i, &(x, y, color)) in ppu.renderer.pixels.iter().enumerate() {
        assert_eq!(
            (x, y),
            ((i % 256) as u32, (i / 256) as u32),
            "raster order at {i}"
        );
        // Rows 0..8 vary by column (nametable row 0); every later row uses
        // nametable tile 0 (color index 1 -> entry $10) since the rest of
        // VRAM is zero.
        let want = if y < 8 { entry_for(x) } else { 0x10 };
        assert_eq!(color, ppu.color_theme.color(want).0, "color at {x},{y}");
    }
}

/// Single mid-frame grayscale toggle (ticket #63): a `$2001` write after
/// 100 pixels of scanline 0 have been buffered splits the frame at pixel
/// index 98 — the retired two-dot output stage (ticket #62) meant the
/// two in-flight pixels (98, 99) committed after the write and take the
/// new color, i.e. the write lands two pixels before its beam position.
/// Pixels below the split keep the prior (color) configuration
/// and pixels at/after it use grayscale. Entry $3F under grayscale renders
/// as $30 (hardware ANDs the 6-bit entry with $30 before lookup), so the
/// two segments are distinguishable through the Render seam alone.
#[test]
fn flush_applies_prior_color_below_midframe_grayscale_split() {
    let mut ppu = Ppu::new(RecordingRender::default());
    let mut cart = Cartridge::test();
    // Rendering disabled: every visible dot resolves to the backdrop entry
    // at $3F00 (vram_addr below $3F00, so `disabled_color_index` reads it).
    ppu.palette.write(0x3f00, 0x3f);
    ppu.registers.vram_addr = 0x2000;
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;

    // Tick dot 0 (idle) plus dots 1..=100, buffering x = 0..=99 of
    // scanline 0. Timing now points at dot 101, the next pixel (index 100).
    for _ in 0..101 {
        ppu.tick(&mut cart);
    }
    assert!(ppu.renderer.pixels.is_empty(), "no pixel before the flush");

    // Mid-frame $2001 write: grayscale on (bit 0). Enable bits stay off,
    // so the per-dot geometry path (disabled backdrop) is unchanged.
    ppu.write_ppureg(0x2001, 0x01, &mut cart);

    tick_until_vblank(&mut ppu, &mut cart);

    let color = ppu.color_theme.color(0x3f).0;
    let grayscale = ppu.color_theme.color(0x30).0;
    assert_ne!(color, grayscale, "segments must be distinguishable");
    assert_eq!(ppu.renderer.finishes, 1, "exactly one finish per frame");
    assert_eq!(ppu.renderer.pixels.len(), 256 * 240);
    for (i, &(x, y, color)) in ppu.renderer.pixels.iter().enumerate() {
        assert_eq!(
            (x, y),
            ((i % 256) as u32, (i / 256) as u32),
            "raster order at {i}"
        );
        let want = if i < 98 { color } else { grayscale };
        assert_eq!(color, want, "color at pixel index {i}");
    }
}

/// Multiple mid-frame grayscale toggles (ticket #63): grayscale on at
/// pixel index 98, off again at the top of scanline 120 (index
/// 120 * 256 — a write at the line start has nothing in flight, so it
/// lands exactly at the beam position). Each of the
/// three segments must use its own configuration — color, grayscale,
/// color — so neither the first write nor the final register state may
/// color the whole frame.
#[test]
fn flush_applies_each_segment_for_multiple_midframe_grayscale_toggles() {
    let mut ppu = Ppu::new(RecordingRender::default());
    let mut cart = Cartridge::test();
    ppu.palette.write(0x3f00, 0x3f);
    ppu.registers.vram_addr = 0x2000;
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;

    // Buffer x = 0..=99 of scanline 0, then toggle grayscale on.
    for _ in 0..101 {
        ppu.tick(&mut cart);
    }
    ppu.write_ppureg(0x2001, 0x01, &mut cart);

    // Scan until timing points at scanline 120, dot 1 (its first pixel,
    // index 120 * 256), buffering every intervening visible dot.
    for _ in 0..200_000 {
        if ppu.timing.scanline() == 120 && ppu.timing.dot() == 1 {
            break;
        }
        ppu.tick(&mut cart);
    }
    assert_eq!((ppu.timing.scanline(), ppu.timing.dot()), (120, 1));
    assert!(ppu.renderer.pixels.is_empty(), "no pixel before the flush");

    // Toggle grayscale back off at the scanline-120 boundary.
    ppu.write_ppureg(0x2001, 0x00, &mut cart);

    tick_until_vblank(&mut ppu, &mut cart);

    let color = ppu.color_theme.color(0x3f).0;
    let grayscale = ppu.color_theme.color(0x30).0;
    assert_ne!(color, grayscale, "segments must be distinguishable");
    assert_eq!(ppu.renderer.finishes, 1, "exactly one finish per frame");
    assert_eq!(ppu.renderer.pixels.len(), 256 * 240);
    for (i, &(x, y, color)) in ppu.renderer.pixels.iter().enumerate() {
        assert_eq!(
            (x, y),
            ((i % 256) as u32, (i / 256) as u32),
            "raster order at {i}"
        );
        let want = if !(98..120 * 256).contains(&i) {
            color
        } else {
            grayscale
        };
        assert_eq!(color, want, "color at pixel index {i}");
    }
}

/// Mid-frame emphasis toggle (ticket #63): a `$2001` write enabling the
/// red tint after 100 pixels of scanline 0 have been buffered splits the
/// frame at pixel index 98 (same two-dot landing as the grayscale test).
/// Pixels below the split keep the untinted
/// color; pixels at/after it have red-only emphasis applied (green and
/// blue scaled by 192/256, red unchanged). Expected colors are derived
/// from the literal backdrop entry's theme color, not from the PPU's
/// own emphasis code.
#[test]
fn flush_applies_prior_color_below_midframe_emphasis_split() {
    let mut ppu = Ppu::new(RecordingRender::default());
    let mut cart = Cartridge::test();
    ppu.palette.write(0x3f00, 0x2c);
    ppu.registers.vram_addr = 0x2000;
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;

    // Buffer x = 0..=99 of scanline 0. Timing now points at dot 101.
    for _ in 0..101 {
        ppu.tick(&mut cart);
    }
    assert!(ppu.renderer.pixels.is_empty(), "no pixel before the flush");

    // Mid-frame $2001 write: red emphasis on (bit 5 = $20). Enable bits
    // stay off, so the per-dot geometry path is unchanged.
    ppu.write_ppureg(0x2001, 0x20, &mut cart);

    tick_until_vblank(&mut ppu, &mut cart);

    let color = ppu.color_theme.color(0x2c).0;
    let emphasised = [
        color[0],
        (color[1] as u16 * 192 / 256) as u8,
        (color[2] as u16 * 192 / 256) as u8,
        color[3],
    ];
    assert_ne!(color, emphasised, "segments must be distinguishable");
    assert_eq!(ppu.renderer.finishes, 1, "exactly one finish per frame");
    assert_eq!(ppu.renderer.pixels.len(), 256 * 240);
    for (i, &(x, y, color)) in ppu.renderer.pixels.iter().enumerate() {
        assert_eq!(
            (x, y),
            ((i % 256) as u32, (i / 256) as u32),
            "raster order at {i}"
        );
        let want = if i < 98 { color } else { emphasised };
        assert_eq!(color, want, "color at pixel index {i}");
    }
}

/// Ticket #64: palette RAM writes mid-frame take effect from their beam
/// position without flushing geometry. Pixels buffered before the write
/// keep the old entry; pixels at/after it use the new one. Observed only
/// through the `Render` seam (flushed `set_pixel` colors).
///
/// #62 already reads palette RAM live at per-dot resolve time and the
/// buffer stores the resolved 6-bit entry value, not a palette address,
/// so this slice proves the position semantics rather than adding
/// segment bookkeeping to the flush.
#[test]
fn palette_write_mid_frame_takes_effect_from_beam_position() {
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    let mut cart = Cartridge::test();
    // Rendering disabled: every visible dot resolves to the backdrop
    // entry at $3F00 (vram_addr below $3F00).
    ppu.palette.write(0x3f00, 0x21);
    ppu.registers.vram_addr = 0x2000;

    // Scan ten pixels of line 0 (x = 0..=9) under the old entry.
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;
    for _ in 0..11 {
        ppu.tick(&mut cart);
    }
    // Write the backdrop entry mid-frame; continue the line and flush.
    ppu.palette.write(0x3f00, 0x16);
    for _ in 0..250 {
        ppu.tick(&mut cart);
    }
    flush_frame_at_vblank(&mut ppu, &mut cart);

    let old_color = ppu.color_theme.color(0x21).0;
    let new_color = ppu.color_theme.color(0x16).0;
    assert_ne!(old_color, new_color, "entries must be distinguishable");
    for x in 0..10u32 {
        assert_eq!(
            ppu.renderer.get_pixel(x, 0),
            old_color,
            "x={x} buffered before the write keeps the old entry"
        );
    }
    for x in 10..256u32 {
        assert_eq!(
            ppu.renderer.get_pixel(x, 0),
            new_color,
            "x={x} buffered at/after the write uses the new entry"
        );
    }
}

/// Ticket #64 (Zapper): `pixel_brightness` is served mid-frame from the
/// buffered palette index resolved through the current palette/
/// grayscale/emphasis logic — not from the materialized framebuffer.
/// Beam positions not yet written this frame report black. Observed
/// only through the PPU's brightness seam before any flush.
#[test]
fn live_pixel_brightness_resolves_buffered_index_mid_frame() {
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    let mut cart = Cartridge::test();
    // Rendering disabled: backdrop entry $3F00 = $30 (white, per
    // default.pal: (255, 254, 255), luminance 764).
    ppu.palette.write(0x3f00, 0x30);
    ppu.registers.vram_addr = 0x2000;

    // Buffer ten pixels of line 0; the framebuffer must stay black until
    // the flush, so a non-zero answer can only come from the live buffer.
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;
    for _ in 0..11 {
        ppu.tick(&mut cart);
    }
    assert_eq!(
        ppu.renderer.get_pixel(0, 0),
        [0, 0, 0, 0],
        "no flush yet: framebuffer still black"
    );

    assert_eq!(
        ppu.pixel_brightness(0, 0),
        255 + 254 + 255,
        "buffered pixel resolves to its live luminance"
    );
    assert_eq!(
        ppu.pixel_brightness(9, 0),
        255 + 254 + 255,
        "last buffered pixel is already live"
    );
    assert_eq!(
        ppu.pixel_brightness(10, 0),
        0,
        "not-yet-written beam position reports black"
    );
    assert_eq!(
        ppu.pixel_brightness(0, 1),
        0,
        "unscanned line reports black"
    );
    assert_eq!(
        ppu.pixel_brightness(256, 0),
        0,
        "out-of-bounds x reports black"
    );
    assert_eq!(
        ppu.pixel_brightness(0, 240),
        0,
        "out-of-bounds y reports black"
    );
}

/// Ticket #64 (Zapper): the live brightness goes through the same
/// grayscale mapping as the flush — entry $3F (black) with grayscale
/// on resolves to its $3x row $30 (white, default.pal (255, 254, 255)).
#[test]
fn live_pixel_brightness_applies_current_grayscale() {
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    let mut cart = Cartridge::test();
    ppu.palette.write(0x3f00, 0x3f);
    ppu.registers.vram_addr = 0x2000;
    ppu.registers.mask = PpuMask::new().with_grayscale(true);

    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;
    for _ in 0..3 {
        ppu.tick(&mut cart);
    }

    assert_eq!(
        ppu.pixel_brightness(0, 0),
        255 + 254 + 255,
        "grayscale maps $3F to $30 (white) on the live path"
    );
    assert_eq!(ppu.pixel_brightness(1, 0), 255 + 254 + 255);
    assert_eq!(
        ppu.pixel_brightness(2, 0),
        0,
        "past the beam is still black"
    );
}

// --- Code-review follow-ups (ticket #67) ------------------------------------
//
// Render-seam only: flushed frame pixels / finishes, and the public
// `pixel_brightness` seam. No palette-index buffer layout assertions.

/// Scroll-tile setup shared by the reload-dot tests: background rendering
/// on, tiles 0..=31 solid with color indices 1,2,3 cycling, and the same
/// tile row in nametables 0 and 1, so a one-tile scroll shift is visible
/// as a color change at x = 0. Palette entries $10/$20/$30.
fn setup_scroll_tiles(ppu: &mut Ppu<ImageRender<1>>, cart: &mut Cartridge) {
    ppu.registers.mask = PpuMask::new()
        .with_background_enabled(true)
        .with_background_left_enabled(true);
    ppu.palette.write(0x3f01, 0x10);
    ppu.palette.write(0x3f02, 0x20);
    ppu.palette.write(0x3f03, 0x30);
    let mut pattern = create_pattern();
    for tile in 0..32u8 {
        set_tile_solid(&mut pattern, 0, tile, tile % 3 + 1);
    }
    fill_chr(ppu, &pattern, cart);
    for i in 0..32u16 {
        ppu.write_vram(0x2000 + i, i as u8, cart);
        ppu.write_vram(0x2020 + i, i as u8, cart);
        ppu.write_vram(0x2400 + i, i as u8, cart);
        ppu.write_vram(0x2420 + i, i as u8, cart);
    }
}

/// Incomplete frame after a mid-frame reset (ticket #67): positions not
/// scanned since the reset must flush as black, matching the cleared
/// pre-change framebuffer — not as entry 0 or stale previous-frame
/// palette indices.
#[test]
fn flush_emits_black_for_unwritten_positions_after_mid_frame_reset() {
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    let mut cart = Cartridge::test();
    ppu.palette.write(0x3f00, 0x21);
    ppu.registers.vram_addr = 0x2000;

    // Buffer the first pixels of line 0, then reset mid-frame.
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;
    for _ in 0..11 {
        ppu.tick(&mut cart);
    }
    ppu.reset();

    // No further scan before the VBL set dot flushes the frame.
    flush_frame_at_vblank(&mut ppu, &mut cart);

    for y in 0..240u32 {
        for x in 0..256u32 {
            assert_eq!(
                ppu.renderer.get_pixel(x, y),
                [0, 0, 0, 0],
                "unwritten position {x},{y} must flush black"
            );
        }
    }
    assert_eq!(
        ppu.pixel_brightness(0, 0),
        0,
        "live brightness also reports black after reset"
    );
}

/// The buffered-pixel cursor is monotonic per frame (ticket #67): a
/// backward timing jump must not lower it, otherwise already-buffered
/// beam positions would wrongly report black to the Zapper seam.
#[test]
fn buffered_brightness_survives_backward_timing_jump() {
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    let mut cart = Cartridge::test();
    ppu.palette.write(0x3f00, 0x30);
    ppu.registers.vram_addr = 0x2000;

    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;
    for _ in 0..11 {
        ppu.tick(&mut cart);
    }
    let live = ppu.pixel_brightness(9, 0);
    assert!(live > 0, "buffered pixel must be live before the jump");

    // Timing jumps backward within the scanline (a frame-start jump to
    // dot 0 is a new frame and legitimately restarts the cursor); two
    // more visible dots are scanned.
    ppu.timing.scanline = 0;
    ppu.timing.dot = 1;
    for _ in 0..3 {
        ppu.tick(&mut cart);
    }

    assert_eq!(
        ppu.pixel_brightness(9, 0),
        live,
        "already-buffered position must stay live after a backward jump"
    );
}

/// Sprite-0 hit and sprite overflow stay precise with deferred color
/// (ticket #67): flags are set live per-dot during the scan, and the
/// flushed frame shows the sprite color at the hit pixel.
#[test]
fn sprite_zero_hit_and_overflow_unchanged_with_deferred_color() {
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    ppu.registers.mask = PpuMask::new()
        .with_background_enabled(true)
        .with_sprite_enabled(true)
        .with_background_left_enabled(true)
        .with_sprite_left_enabled(true);
    let mut pattern = create_pattern();
    set_tile_solid(&mut pattern, 0, 0, 1);
    set_tile_solid(&mut pattern, 0, 1, 2);
    set_bg_palette_color(&mut ppu, 0, 1, 0x14);
    set_sprite_palette_color(&mut ppu, 0, 2, 0x24);
    setup_sprite(&mut ppu, 0, 0, 1, 0, 0);
    // Nine in-range sprites on line 21 (y = 20) force overflow.
    for idx in 1..=9u8 {
        setup_sprite(&mut ppu, idx, 20, 0, 0, idx * 8);
    }
    let mut cart = Cartridge::test();
    fill_chr(&mut ppu, &pattern, &mut cart);
    set_bg_tile(&mut ppu, &mut cart, 0, 0);

    // Sprite evaluation for line 1 normally happens across line 0;
    // prime it directly, then start after the dot-0 OAM swap.
    populate_sprite_secondary_oam(&mut ppu, 1);
    ppu.timing.scanline = 1;
    ppu.timing.dot = 1;
    for _ in 0..200_000 {
        if ppu.timing.scanline() == 30 {
            break;
        }
        ppu.tick(&mut cart);
    }
    assert_eq!(
        ppu.renderer.get_pixel(0, 1),
        [0, 0, 0, 0],
        "no pixel reaches the framebuffer before the flush"
    );
    assert!(
        ppu.registers.status.sprite_zero_hit(),
        "opaque sprite 0 over opaque background must set the hit"
    );
    assert!(
        ppu.registers.status.sprite_overflow(),
        "nine in-range sprites must set overflow"
    );

    flush_frame_at_vblank(&mut ppu, &mut cart);
    assert_eq!(
        ppu.renderer.get_pixel(0, 1),
        ppu.color_theme.color(0x24).0,
        "flushed frame shows the in-front sprite color at the hit pixel"
    );
    assert_eq!(
        ppu.renderer.get_pixel(8, 1),
        ppu.color_theme.color(0x14).0,
        "flushed frame shows the background color off the sprite"
    );
}

/// Reload dots (ticket #67): a mid-line `$2005` write lands at dot 257 —
/// the rest of the current line keeps its geometry, the next line starts
/// one tile further right.
#[test]
fn scroll_write_mid_line_takes_effect_at_dot_257() {
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    let mut cart = Cartridge::test();
    setup_scroll_tiles(&mut ppu, &mut cart);

    // Scan 100 pixels of line 0, then a $2005 X write (coarse X = 1).
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;
    for _ in 0..101 {
        ppu.tick(&mut cart);
    }
    ppu.write_ppureg(0x2005, 8, &mut cart);

    // Scan to line 1, dot 257 (reload has landed), then flush.
    for _ in 0..200_000 {
        if ppu.timing.scanline() == 1 && ppu.timing.dot() == 257 {
            break;
        }
        ppu.tick(&mut cart);
    }
    assert_eq!((ppu.timing.scanline(), ppu.timing.dot()), (1, 257));
    flush_frame_at_vblank(&mut ppu, &mut cart);

    // Line 0 is entirely pre-write geometry.
    let c = |entry: u8| ppu.color_theme.color(entry).0;
    assert_eq!(ppu.renderer.get_pixel(0, 0), c(0x10));
    assert_eq!(ppu.renderer.get_pixel(8, 0), c(0x20));
    // Line 1 starts one tile (8px) to the right.
    assert_eq!(ppu.renderer.get_pixel(0, 1), c(0x20));
    assert_eq!(ppu.renderer.get_pixel(8, 1), c(0x30));
    assert_eq!(ppu.renderer.get_pixel(16, 1), c(0x10));
}

/// Reload dots (ticket #67): a mid-line `$2006` second write performs the
/// full reload via the dot-257 reload — the current line is unchanged,
/// the next line starts at the new temporary address (coarse X = 7).
#[test]
fn ppuaddr_write_mid_line_full_reload_at_dot_257() {
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    let mut cart = Cartridge::test();
    setup_scroll_tiles(&mut ppu, &mut cart);

    // Scan to dot 250 of line 0 — late enough that the scheduled
    // background activation (screen_x + 16) falls past the line end and
    // is discarded at the next line boundary.
    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;
    for _ in 0..251 {
        ppu.tick(&mut cart);
    }
    ppu.write_ppureg(0x2006, 0x24, &mut cart);
    ppu.write_ppureg(0x2006, 0x07, &mut cart);

    // Scan to line 1, dot 257, then flush.
    for _ in 0..200_000 {
        if ppu.timing.scanline() == 1 && ppu.timing.dot() == 257 {
            break;
        }
        ppu.tick(&mut cart);
    }
    assert_eq!((ppu.timing.scanline(), ppu.timing.dot()), (1, 257));
    flush_frame_at_vblank(&mut ppu, &mut cart);

    let c = |entry: u8| ppu.color_theme.color(entry).0;
    // Current line kept its geometry throughout.
    assert_eq!(ppu.renderer.get_pixel(0, 0), c(0x10));
    // Next line starts at tile 7 of nametable 1 (0x2407).
    assert_eq!(ppu.renderer.get_pixel(0, 1), c(0x20));
    assert_eq!(ppu.renderer.get_pixel(8, 1), c(0x30));
}

/// Rendering disabled / PPUMASK all-off (ticket #67): color semantics
/// still hold for the flush — grayscale masks the backdrop entry before
/// the lookup even with every enable bit off.
#[test]
fn flush_applies_grayscale_with_rendering_disabled() {
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    let mut cart = Cartridge::test();
    ppu.palette.write(0x3f00, 0x3f);
    ppu.registers.vram_addr = 0x2000;

    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;
    // Grayscale only; all enable bits off (rendering stays disabled).
    ppu.write_ppureg(0x2001, 0x01, &mut cart);

    // Scan two full lines so every asserted pixel was buffered.
    for _ in 0..200_000 {
        if ppu.timing.scanline() == 1 && ppu.timing.dot() == 257 {
            break;
        }
        ppu.tick(&mut cart);
    }
    flush_frame_at_vblank(&mut ppu, &mut cart);

    let grayscale = ppu.color_theme.color(0x30).0;
    let color = ppu.color_theme.color(0x3f).0;
    assert_ne!(grayscale, color, "segments must be distinguishable");
    // The write lands before the first pixel, so every buffered pixel
    // flushes under grayscale.
    for y in 0..2u32 {
        for x in 0..256u32 {
            assert_eq!(
                ppu.renderer.get_pixel(x, y),
                grayscale,
                "backdrop {x},{y} must flush grayscale-masked"
            );
        }
    }
}

/// Rendering disabled / PPUMASK all-off (ticket #67): red emphasis
/// applies to the backdrop color at flush time with all enable bits off.
#[test]
fn flush_applies_emphasis_with_rendering_disabled() {
    let mut ppu = Ppu::new(ImageRender::<1>::default_dimension());
    let mut cart = Cartridge::test();
    ppu.palette.write(0x3f00, 0x2c);
    ppu.registers.vram_addr = 0x2000;

    ppu.timing.scanline = 0;
    ppu.timing.dot = 0;
    // Red emphasis only; all enable bits off.
    ppu.write_ppureg(0x2001, 0x20, &mut cart);

    // Scan two full lines so every asserted pixel was buffered.
    for _ in 0..200_000 {
        if ppu.timing.scanline() == 1 && ppu.timing.dot() == 257 {
            break;
        }
        ppu.tick(&mut cart);
    }
    flush_frame_at_vblank(&mut ppu, &mut cart);

    let color = ppu.color_theme.color(0x2c).0;
    let emphasized = [
        color[0],
        (color[1] as u16 * 192 / 256) as u8,
        (color[2] as u16 * 192 / 256) as u8,
        color[3],
    ];
    assert_ne!(color, emphasized, "emphasis must change the color");
    for y in 0..2u32 {
        for x in 0..256u32 {
            assert_eq!(
                ppu.renderer.get_pixel(x, y),
                emphasized,
                "backdrop {x},{y} must flush red-emphasised"
            );
        }
    }
}
