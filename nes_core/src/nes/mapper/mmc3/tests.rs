use super::*;
use crate::nes::mapper::Mirroring;

fn create_prg() -> [u8; PRG_ROM_BANK_SIZE * 8] {
    let mut prg = [0u8; PRG_ROM_BANK_SIZE * 8];
    for bank in 0..8 {
        prg[bank * PRG_ROM_BANK_SIZE] = bank as u8;
    }
    prg[(8 * PRG_ROM_BANK_SIZE) - 1] = 0xfe;
    prg
}

#[test]
fn switches_prg_bank_in_normal_mode() {
    let prg = create_prg();
    let mut mapper = MMC3::new(&prg, false, false);

    mapper.write(0x8000, 0x06);
    mapper.write(0x8001, 0x03);
    mapper.write(0x8000, 0x07);
    mapper.write(0x8001, 0x04);

    assert_eq!(mapper.read(0x8000), 0x03);
    assert_eq!(mapper.read(0xa000), 0x04);
    assert_eq!(mapper.read(0xc000), 0x06);
    assert_eq!(mapper.read(0xe000), 0x07);
    assert_eq!(mapper.read(0xffff), 0xfe);
}

#[test]
fn swaps_fixed_and_switchable_prg_regions_in_prg_mode_1() {
    let prg = create_prg();
    let mut mapper = MMC3::new(&prg, false, false);

    mapper.write(0x8000, 0x46);
    mapper.write(0x8001, 0x03);
    mapper.write(0x8000, 0x47);
    mapper.write(0x8001, 0x04);

    assert_eq!(mapper.read(0x8000), 0x06);
    assert_eq!(mapper.read(0xa000), 0x04);
    assert_eq!(mapper.read(0xc000), 0x03);
    assert_eq!(mapper.read(0xe000), 0x07);
}

#[test]
fn changes_mirroring_unless_four_screen_is_forced() {
    let mut mapper = MMC3::new(&create_prg(), false, false);

    assert_eq!(
        mapper.write(0xa000, 0x00),
        CartridgeOperation::UpdateNametableMirroring(Mirroring::Vertical)
    );

    assert_eq!(
        mapper.write(0xa000, 0x01),
        CartridgeOperation::UpdateNametableMirroring(Mirroring::Horizontal)
    );

    let mut locked_mapper = MMC3::new(&create_prg(), true, false);
    assert_eq!(locked_mapper.write(0xa000, 0x01), CartridgeOperation::None);
}

#[test]
fn triggers_irq_after_scanline_clocks() {
    let mut mapper = MMC3::new(&create_prg(), false, false);

    mapper.write(0xc000, 0x01);
    mapper.write(0xc001, 0x00);
    mapper.write(0xe001, 0x00);

    for _ in 0..=MMC3_A12_LOW_FILTER_TICKS {
        mapper.on_ppu_tick(0, 0, true);
    }
    mapper.notify_vram_address(0x1000);

    mapper.notify_vram_address(0x0000);
    for _ in 0..=MMC3_A12_LOW_FILTER_TICKS {
        mapper.on_ppu_tick(0, 0, true);
    }
    mapper.notify_vram_address(0x1000);

    assert!(mapper.irq_pending());

    mapper.write(0xe000, 0x00);
    assert!(!mapper.irq_pending());
}
