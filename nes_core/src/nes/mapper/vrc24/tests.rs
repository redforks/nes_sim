use super::*;

fn create_prg(size_kb: usize) -> Vec<u8> {
    let bank_count = size_kb / 8;
    let mut prg = vec![0u8; size_kb * 1024];
    for bank in 0..bank_count {
        prg[bank * PRG_BANK_SIZE] = bank as u8;
    }
    if bank_count > 0 {
        prg[bank_count * PRG_BANK_SIZE - 1] = 0xfe;
    }
    prg
}

#[test]
fn vrc4f_basic_prg_banking() {
    let prg = create_prg(32);
    let mut mapper = Vrc24::new(&prg, VrcVariant::Vrc4f);

    assert_eq!(mapper.read(0x8000), 0);
    assert_eq!(mapper.read(0xc000), 2);
    assert_eq!(mapper.read(0xe000), 3);
    assert_eq!(mapper.read(0xffff), 0xfe);

    mapper.write(0x8000, 1);
    assert_eq!(mapper.read(0x8000), 1);
}

#[test]
fn vrc4f_prg_swap_mode() {
    let prg = create_prg(32);
    let mut mapper = Vrc24::new(&prg, VrcVariant::Vrc4f);

    assert_eq!(mapper.read(0x8000), 0);
    assert_eq!(mapper.read(0xc000), 2);

    mapper.write(0x9002, 3);

    assert_eq!(mapper.read(0x8000), 2);
    assert_eq!(mapper.read(0xc000), 0);
}

#[test]
fn vrc4_irq_cycle_mode() {
    let prg = create_prg(32);
    let mut mapper = Vrc24::new(&prg, VrcVariant::Vrc4f);

    mapper.write(0xf000, 240 & 0x0f);
    mapper.write(0xf001, (240 >> 4) & 0x0f);

    mapper.write(0xf002, 4);

    assert!(!mapper.irq_pending());

    for _ in 0..48 {
        mapper.on_ppu_tick(0);
    }

    assert!(mapper.irq_pending());
}

#[test]
fn vrc2_microwire_latch() {
    let prg = create_prg(32);
    let mut mapper = Vrc24::new(&prg, VrcVariant::Vrc2b);

    mapper.write(0x6000, 0x01);
    assert_eq!(mapper.read(0x6000) & 0x01, 0x01);
    assert_eq!(mapper.read(0x6100) & 0x01, 0x01);

    mapper.write(0x6000, 0x00);
    assert_eq!(mapper.read(0x6000) & 0x01, 0x00);
}
