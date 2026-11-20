//! Shared iNES ROM images for wait/freeze tests.
//!
//! This module is pure test data: no emulator types and no behavior. Unit
//! tests and integration tests both include this source file, so a wait
//! program has one definition no matter which test target runs it.

const PRG_LEN: usize = 16 * 1024;
const CHR_LEN: usize = 8 * 1024;

fn nrom_image(prg: Vec<u8>) -> Vec<u8> {
    debug_assert_eq!(prg.len(), PRG_LEN);
    let mut rom = b"NES\x1a".to_vec();
    rom.extend_from_slice(&[1, 1, 0, 0]); // one PRG page, one CHR page, mapper 0
    rom.extend_from_slice(&[0; 8]);
    rom.extend_from_slice(&prg);
    rom.extend(std::iter::repeat_n(0, CHR_LEN));
    rom
}

/// Bare `JMP $8000` self-loop with the original all-zero vectors: tests
/// that set the PC explicitly rely on that reset state.
pub fn bare_spin_image() -> Vec<u8> {
    let mut prg = vec![0u8; PRG_LEN];
    prg[..3].copy_from_slice(&[0x4C, 0x00, 0x80]);
    nrom_image(prg)
}

/// `LDA $0010 / BNE $8000` RAM flag wait with an NMI handler at $8600
/// (PRG offset $600) that clears the flag. Both vectors point at $8000.
pub fn ram_flag_wait_image() -> Vec<u8> {
    let mut prg = vec![0u8; PRG_LEN];
    prg[..8].copy_from_slice(&[
        0xAD, 0x10, 0x00, // $8000 LDA $0010
        0xD0, 0xFB, // $8003 BNE $8000
        0x4C, 0x08, 0x80, // $8005 JMP $8008 (park)
    ]);
    prg[8..11].copy_from_slice(&[0x4C, 0x08, 0x80]);
    prg[0x600..0x606].copy_from_slice(&[
        0xA9, 0x00, // LDA #$00
        0x8D, 0x10, 0x00, // STA $0010
        0x40, // RTI
    ]);
    prg[PRG_LEN - 4..].copy_from_slice(&[0x00, 0x80, 0x00, 0x80]);
    nrom_image(prg)
}

/// `BIT $2002 / BPL $800A` VBL status wait. NMI is left disabled so the
/// only exit is a poll read observing VBL; the write to $3F01 mirrors
/// $2001 (PPUMASK) and enables background rendering, so the framebuffer
/// has signal. All vectors point at the wait, and unused bytes are zero.
pub fn vbl_status_wait_image() -> Vec<u8> {
    let mut prg = vec![0u8; PRG_LEN];
    prg[..18].copy_from_slice(&[
        0xA9, 0x00, // $8000 LDA #$00
        0x8D, 0x00, 0x20, // $8002 STA $2000 (disable NMI)
        0xA9, 0x2C, // $8005 LDA #$2C
        0x8D, 0x01, 0x3F, // $8007 STA $3F01 (mirror of $2001: enable bg)
        0x2C, 0x02, 0x20, // $800A BIT $2002
        0x10, 0xFB, // $800D BPL $800A
        0x4C, 0x10, 0x80, // $800F JMP $8010 (park)
    ]);
    prg[PRG_LEN - 4..].copy_from_slice(&[0x0A, 0x80, 0x0A, 0x80]);
    nrom_image(prg)
}

/// DMC setup followed by a `LDA $0010 / BNE` RAM flag spin at $8014.
/// The filler is the DMC sample byte; both vectors point at $8000.
pub fn dmc_ram_spin_image() -> Vec<u8> {
    let mut prg = vec![0x80u8; PRG_LEN];
    prg[..28].copy_from_slice(&[
        0xA9, 0x0F, 0x8D, 0x10, 0x40, // $8000 LDA #$0F / STA $4010 (rate)
        0xA9, 0x00, 0x8D, 0x12, 0x40, // $8005 LDA #$00 / STA $4012 (address $C000)
        0xA9, 0x00, 0x8D, 0x13, 0x40, // $800A LDA #$00 / STA $4013 (length)
        0xA9, 0x10, 0x8D, 0x15, 0x40, // $800F LDA #$10 / STA $4015 (enable DMC)
        0xAD, 0x10, 0x00, // $8014 LDA $0010
        0xD0, 0xFB, // $8017 BNE $8014
        0x4C, 0x19, 0x80, // $8019 JMP $8019
    ]);
    prg[PRG_LEN - 4..].copy_from_slice(&[0x00, 0x80, 0x00, 0x80]);
    nrom_image(prg)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn header(image: &[u8]) -> &[u8] {
        &image[..16]
    }

    #[test]
    fn bare_spin_image_keeps_the_jmp_and_zero_vectors() {
        let image = bare_spin_image();
        assert_eq!(
            header(&image),
            &[0x4E, 0x45, 0x53, 0x1A, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        );
        assert_eq!(&image[16..19], &[0x4C, 0x00, 0x80]);
        assert_eq!(image.len(), 16 + PRG_LEN + CHR_LEN);
        assert_eq!(&image[16 + PRG_LEN - 4..16 + PRG_LEN], &[0; 4]);
    }

    #[test]
    fn ram_flag_wait_image_keeps_the_poll_handler_and_vectors() {
        let image = ram_flag_wait_image();
        let prg = &image[16..16 + PRG_LEN];
        assert_eq!(
            &prg[..11],
            &[
                0xAD, 0x10, 0x00, 0xD0, 0xFB, 0x4C, 0x08, 0x80, 0x4C, 0x08, 0x80
            ]
        );
        assert_eq!(&prg[0x600..0x606], &[0xA9, 0x00, 0x8D, 0x10, 0x00, 0x40]);
        assert_eq!(&prg[PRG_LEN - 4..], &[0x00, 0x80, 0x00, 0x80]);
    }

    #[test]
    fn vbl_status_wait_image_keeps_the_poll_setup_and_vectors() {
        let image = vbl_status_wait_image();
        let prg = &image[16..16 + PRG_LEN];
        assert_eq!(
            &prg[..18],
            &[
                0xA9, 0x00, 0x8D, 0x00, 0x20, 0xA9, 0x2C, 0x8D, 0x01, 0x3F, 0x2C, 0x02, 0x20, 0x10,
                0xFB, 0x4C, 0x10, 0x80,
            ]
        );
        assert_eq!(&prg[PRG_LEN - 4..], &[0x0A, 0x80, 0x0A, 0x80]);
        assert_eq!(image.len(), 16 + PRG_LEN + CHR_LEN);
    }

    #[test]
    fn dmc_ram_spin_image_keeps_the_dmc_setup_and_spin() {
        let image = dmc_ram_spin_image();
        let prg = &image[16..16 + PRG_LEN];
        assert_eq!(
            &prg[..28],
            &[
                0xA9, 0x0F, 0x8D, 0x10, 0x40, 0xA9, 0x00, 0x8D, 0x12, 0x40, 0xA9, 0x00, 0x8D, 0x13,
                0x40, 0xA9, 0x10, 0x8D, 0x15, 0x40, 0xAD, 0x10, 0x00, 0xD0, 0xFB, 0x4C, 0x19, 0x80,
            ]
        );
        assert_eq!(prg[28], 0x80, "the sample filler must stay intact");
        assert_eq!(&prg[PRG_LEN - 4..], &[0x00, 0x80, 0x00, 0x80]);
    }
}
