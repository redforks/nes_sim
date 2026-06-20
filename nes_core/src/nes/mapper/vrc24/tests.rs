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
fn vrc4f_chr_banking() {
    let chr = create_chr(64);
    let prg = create_prg(32);
    let mut mapper = Vrc24::new(&prg, &chr, VrcVariant::Vrc4f);

    // VRC4f uses A0/A1, so register offsets are $x000, $x001, $x002, $x003
    // CHR0 low at $B000, CHR0 high at $B001
    mapper.write(0xb000, 1); // low nibble
    mapper.write(0xb001, 0); // high nibble (bank = 1)

    assert_eq!(mapper.pattern_ref()[0x0000], 1);
}

#[test]
fn vrc4a_register_offsets() {
    // VRC4a: A1/A2, offsets $x000, $x002, $x004, $x006
    let prg = create_prg(32);
    let chr = create_chr(64);
    let mut mapper = Vrc24::new(&prg, &chr, VrcVariant::Vrc4a);

    // Write to $8002 (should be same register as $8000 for PRG0, but different
    // register index for CHR)
    mapper.write(0x8000, 2); // PRG Select 0
    assert_eq!(mapper.read(0x8000), 2);

    // Write CHR via $B004 (index 2 = CHR1 low byte in $B000 group)
    mapper.write(0xb004, 5); // CHR1 low
    mapper.write(0xb006, 0); // CHR1 high
    assert_eq!(mapper.pattern_ref()[0x0400], 5);
}

#[test]
fn vrc2a_chr_shift() {
    // VRC2a (mapper 22): ignores low bit, right-shifts by 1
    let prg = create_prg(32);
    let chr = create_chr(64);
    let mut mapper = Vrc24::new(&prg, &chr, VrcVariant::Vrc2a);

    // Bank value 2 on VRC2a becomes bank 1 (shifted right)
    mapper.write(0xb000, 2); // low nibble = 2
    mapper.write(0xb002, 0); // high nibble = 0, total = 2, shifted = 1

    assert_eq!(mapper.pattern_ref()[0x0000], 1);
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
