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

fn create_chr(size_kb: usize) -> Vec<u8> {
    let bank_count = size_kb;
    let mut chr = vec![0u8; size_kb * 1024];
    for bank in 0..bank_count {
        chr[bank * CHR_BANK_SIZE] = bank as u8;
    }
    chr
}

#[test]
fn vrc4f_basic_prg_banking() {
    let prg = create_prg(32);
    let chr = create_chr(8);
    let mut mapper = Vrc24::new(&prg, &chr, VrcVariant::Vrc4f);

    // Default: $8000 = bank 0, $A000 = bank 0, $C000 = second-last, $E000 = last
    assert_eq!(mapper.read(0x8000), 0);
    assert_eq!(mapper.read(0xc000), 2);
    assert_eq!(mapper.read(0xe000), 3);
    assert_eq!(mapper.read(0xffff), 0xfe);

    // Write PRG Select 0
    mapper.write(0x8000, 1);
    assert_eq!(mapper.read(0x8000), 1);
}

#[test]
fn vrc4f_prg_swap_mode() {
    let prg = create_prg(32);
    let chr = create_chr(8);
    let mut mapper = Vrc24::new(&prg, &chr, VrcVariant::Vrc4f);

    // Normal mode: $8000 switchable, $C000 fixed
    assert_eq!(mapper.read(0x8000), 0);
    assert_eq!(mapper.read(0xc000), 2);

    // Enable swap mode and WRAM
    mapper.write(0x9002, 3); // WRAM enable + swap mode

    // $8000 now fixed to second-last bank, $C000 switchable
    assert_eq!(mapper.read(0x8000), 2);
    assert_eq!(mapper.read(0xc000), 0);
}

#[test]
fn vrc4_irq_cycle_mode() {
    let prg = create_prg(32);
    let chr = create_chr(8);
    let mut mapper = Vrc24::new(&prg, &chr, VrcVariant::Vrc4f);

    // latch = 240: counter = 256-240 = 16
    // IRQ fires after 16 decrements = 16 CPU cycles = 48 PPU ticks
    mapper.write(0xf000, 240 & 0x0f); // low nibble
    mapper.write(0xf001, (240 >> 4) & 0x0f); // high nibble

    // IRQ control: bit 2=1 (cycle prescaler = 3 PPU ticks)
    mapper.write(0xf002, 4);

    assert!(!mapper.irq_pending());

    // 16 CPU cycles = 48 PPU ticks, prescaler wraps every 3 ticks
    for _ in 0..48 {
        mapper.on_ppu_tick(0, 0, true);
    }

    // After 16 decrements, counter reaches 0, IRQ pending
    assert!(mapper.irq_pending());
}

#[test]
fn vrc2_microwire_latch() {
    let prg = create_prg(32);
    let chr = create_chr(8);
    let mut mapper = Vrc24::new(&prg, &chr, VrcVariant::Vrc2b);

    // Write to Microwire area
    mapper.write(0x6000, 0x01);
    assert_eq!(mapper.read(0x6000) & 0x01, 0x01);
    assert_eq!(mapper.read(0x6100) & 0x01, 0x01);

    mapper.write(0x6000, 0x00);
    assert_eq!(mapper.read(0x6000) & 0x01, 0x00);
}
