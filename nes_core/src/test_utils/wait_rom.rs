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

/// `LDA #$FF / loop: LDA $2007 / BPL loop`: a pure PPUDATA poll whose
/// reads refill the read buffer and increment v. All seeded values have
/// bit 7 clear, so BPL keeps looping. Both vectors point at $8000.
pub fn ppudata_poll_image() -> Vec<u8> {
    let mut prg = vec![0u8; PRG_LEN];
    prg[..7].copy_from_slice(&[
        0xA9, 0xFF, // $8000 LDA #$FF
        0xAD, 0x07, 0x20, // $8002 LDA $2007 (loop top)
        0x10, 0xFB, // $8005 BPL $8002
    ]);
    prg[PRG_LEN - 4..].copy_from_slice(&[0x00, 0x80, 0x00, 0x80]);
    nrom_image(prg)
}

/// `LDA #$01 / STA $4016` holds the controller strobe, then
/// `loop: LDA $4016 / BNE loop` polls the pressed A bit forever. The frozen
/// and control runs both keep A at the constant strobe value, so frame
/// equivalence pins the device read effects instead of loop-top progress.
/// Both vectors point at $8000.
pub fn controller_poll_image() -> Vec<u8> {
    let mut prg = vec![0u8; PRG_LEN];
    prg[..10].copy_from_slice(&[
        0xA9, 0x01, // $8000 LDA #$01
        0x8D, 0x16, 0x40, // $8002 STA $4016 (strobe on)
        0xAD, 0x16, 0x40, // $8005 LDA $4016 (loop top)
        0xD0, 0xFB, // $8008 BNE $8005
    ]);
    prg[PRG_LEN - 4..].copy_from_slice(&[0x00, 0x80, 0x00, 0x80]);
    nrom_image(prg)
}

/// Common DMC arm sequence: rate $0F, sample at $C000, length $01, then
/// DMC enable via $4015. The rest of the PRG is the sample filler.
fn dmc_setup(prg: &mut [u8]) {
    prg[..20].copy_from_slice(&[
        0xA9, 0x0F, 0x8D, 0x10, 0x40, // $8000 LDA #$0F / STA $4010 (rate)
        0xA9, 0x00, 0x8D, 0x12, 0x40, // $8005 LDA #$00 / STA $4012 (address $C000)
        0xA9, 0x00, 0x8D, 0x13, 0x40, // $800A LDA #$00 / STA $4013 (length 1)
        0xA9, 0x10, 0x8D, 0x15, 0x40, // $800F LDA #$10 / STA $4015 (enable DMC)
    ]);
    prg[20..].fill(0x80);
}

/// DMC setup followed by a `LDA $0010 / BNE` RAM flag spin at $8014.
pub fn dmc_ram_spin_image() -> Vec<u8> {
    let mut prg = vec![0u8; PRG_LEN];
    dmc_setup(&mut prg);
    prg[20..28].copy_from_slice(&[
        0xAD, 0x10, 0x00, // $8014 LDA $0010
        0xD0, 0xFB, // $8017 BNE $8014
        0x4C, 0x19, 0x80, // $8019 JMP $8019
    ]);
    prg[PRG_LEN - 4..].copy_from_slice(&[0x00, 0x80, 0x00, 0x80]);
    nrom_image(prg)
}

/// DMC setup followed by a `LDA $4015 / BNE` device status poll at $8014.
/// The DMC's active bit keeps the poll asserted until DMA consumes it.
pub fn dmc_device_poll_image() -> Vec<u8> {
    let mut prg = vec![0u8; PRG_LEN];
    dmc_setup(&mut prg);
    prg[20..28].copy_from_slice(&[
        0xAD, 0x15, 0x40, // $8014 LDA $4015
        0xD0, 0xFB, // $8017 BNE $8014
        0x4C, 0x19, 0x80, // $8019 JMP $8019
    ]);
    prg[PRG_LEN - 4..].copy_from_slice(&[0x00, 0x80, 0x00, 0x80]);
    nrom_image(prg)
}

/// `LDA $4015 / BNE $8005` poll of the APU status register.
pub fn apu_status_poll_image() -> Vec<u8> {
    let mut prg = vec![0u8; PRG_LEN];
    prg[..5].copy_from_slice(&[
        0xAD, 0x15, 0x40, // $8000 LDA $4015
        0xD0, 0xFB, // $8003 BNE $8000
    ]);
    prg[PRG_LEN - 4..].copy_from_slice(&[0x00, 0x80, 0x00, 0x80]);
    nrom_image(prg)
}

/// `LDA $4017 / BNE $8005` poll of controller port B (read via $4017).
pub fn controller_b_poll_image() -> Vec<u8> {
    let mut prg = vec![0u8; PRG_LEN];
    prg[..5].copy_from_slice(&[
        0xAD, 0x17, 0x40, // $8000 LDA $4017
        0xD0, 0xFB, // $8003 BNE $8000
    ]);
    prg[PRG_LEN - 4..].copy_from_slice(&[0x00, 0x80, 0x00, 0x80]);
    nrom_image(prg)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A valid iNES image has a 16-byte header, one 16 KiB PRG page and one
    /// 8 KiB CHR page. The two CPU vectors are the final four PRG bytes.
    fn prg(image: &[u8]) -> &[u8] {
        assert_eq!(image.len(), 16 + PRG_LEN + CHR_LEN);
        assert_eq!(&image[..4], b"NES\x1a");
        assert_eq!(&image[4..6], &[1, 1]);
        &image[16..16 + PRG_LEN]
    }

    fn vectors(prg: &[u8]) -> (u16, u16) {
        let bytes = &prg[PRG_LEN - 4..];
        (
            u16::from_le_bytes([bytes[0], bytes[1]]),
            u16::from_le_bytes([bytes[2], bytes[3]]),
        )
    }

    /// Decode one instruction at `offset`, returning (opcode, operand bytes,
    /// next offset). Only the addressing modes these ROMs use are needed.
    fn decode(prg: &[u8], offset: usize) -> (u8, &[u8], usize) {
        let op = prg[offset];
        let len = match op {
            0x4C | 0xAD | 0x8D | 0x2C => 3, // JMP abs, LDA abs, STA abs, BIT abs
            0xA9 | 0xD0 | 0x10 => 2,        // LDA imm, BNE/BPL rel
            0x40 => 1,                      // RTI
            _ => panic!("unexpected opcode {op:#04X} at PRG offset {offset:#06X}"),
        };
        (op, &prg[offset + 1..offset + len], offset + len)
    }

    /// Resolve a relative branch from the byte after the branch opcode.
    fn branch_target(instruction_pc: u16, offset: u8) -> u16 {
        instruction_pc
            .wrapping_add(2)
            .wrapping_add(offset as i8 as u16)
    }

    #[test]
    fn bare_spin_program_jumps_back_to_its_entry() {
        let image = bare_spin_image();
        let prg = prg(&image);
        let (op, operand, _) = decode(prg, 0);
        assert_eq!(op, 0x4C, "expected a JMP spin");
        assert_eq!(
            u16::from_le_bytes([operand[0], operand[1]]),
            0x8000,
            "JMP must target the ROM entry"
        );
        assert_eq!(vectors(prg), (0, 0));
    }

    #[test]
    fn ram_flag_wait_loops_on_its_flag_and_its_handler_returns() {
        let image = ram_flag_wait_image();
        let prg = prg(&image);
        let (op, operand, _) = decode(prg, 0);
        assert_eq!(op, 0xAD, "loop top must read the flag");
        let flag = u16::from_le_bytes([operand[0], operand[1]]);
        let (bne, rel, _) = decode(prg, 3);
        assert_eq!(bne, 0xD0);
        assert_eq!(branch_target(0x8003, rel[0]), 0x8000);
        let (lda, flag_value, handler) = decode(prg, 0x600);
        assert_eq!(lda, 0xA9);
        assert_eq!(flag_value, &[0], "the handler must clear the RAM flag");
        let (sta, operand, handler) = decode(prg, handler);
        assert_eq!(sta, 0x8D);
        assert_eq!(u16::from_le_bytes([operand[0], operand[1]]), flag);
        let (rti, _, _) = decode(prg, handler);
        assert_eq!(rti, 0x40, "NMI handler must return with RTI");
        assert_eq!(vectors(prg), (0x8000, 0x8000));
        assert_ne!(flag, 0, "the polled flag must be a RAM address");
    }

    #[test]
    fn vbl_status_wait_reads_2002_and_branches_to_the_read() {
        let image = vbl_status_wait_image();
        let prg = prg(&image);
        let mut pc = 0usize;
        let mut reads_2002 = None;
        while pc < 12 {
            let (op, operand, next) = decode(prg, pc);
            let addr = if operand.len() >= 2 {
                u16::from_le_bytes([operand[0], operand[1]])
            } else {
                0
            };
            if op == 0x2C && addr == 0x2002 {
                reads_2002 = Some(0x8000 + pc as u16);
            }
            if op == 0x10 {
                assert_eq!(
                    branch_target(0x8000 + pc as u16, operand[0]),
                    reads_2002.expect("BPL must follow the $2002 read")
                );
            }
            pc = next;
        }
        assert!(reads_2002.is_some(), "the wait must poll $2002");
        assert_eq!(vectors(prg), (0x800A, 0x800A));
    }

    /// Assert a `LDA $addr / B<cond> loop` device poll from decoded
    /// instructions. `branch` is BNE ($D0) or BPL ($10).
    fn assert_device_poll(prg: &[u8], offset: usize, addr: u16, loop_pc: u16, branch: u8) {
        let (lda, operand, next) = decode(prg, offset);
        assert_eq!(lda, 0xAD, "loop top must be an absolute LDA");
        assert_eq!(u16::from_le_bytes([operand[0], operand[1]]), addr);
        let (op, rel, _) = decode(prg, next);
        assert_eq!(op, branch, "the poll must branch on the read result");
        assert_eq!(
            branch_target(loop_pc.wrapping_add(3), rel[0]),
            loop_pc,
            "the branch must resolve to the loop top"
        );
    }

    #[test]
    fn ppudata_and_controller_polls_target_their_device_registers() {
        let ppudata_image = ppudata_poll_image();
        let ppudata = prg(&ppudata_image);
        let (_, _, after_init) = decode(ppudata, 0);
        assert_device_poll(ppudata, after_init, 0x2007, 0x8002, 0x10);
        assert_eq!(vectors(ppudata), (0x8000, 0x8000));

        let controller_image = controller_poll_image();
        let controller = prg(&controller_image);
        let mut pc = 0;
        let (_, _, next) = decode(controller, pc);
        pc = next;
        let (sta, operand, next) = decode(controller, pc);
        assert_eq!(sta, 0x8D, "the strobe must be a real $4016 write");
        assert_eq!(u16::from_le_bytes([operand[0], operand[1]]), 0x4016);
        assert_device_poll(controller, next, 0x4016, 0x8005, 0xD0);
        assert_eq!(vectors(controller), (0x8000, 0x8000));
    }

    /// Decode the four DMC setup writes and assert the register each one
    /// targets, then return the offset of the first instruction after them.
    fn assert_dmc_setup(prg: &[u8]) -> usize {
        let mut pc = 0;
        for expected in [0x4010, 0x4012, 0x4013, 0x4015] {
            let (lda, _, next) = decode(prg, pc);
            assert_eq!(lda, 0xA9, "DMC setup must load an immediate value");
            pc = next;
            let (sta, operand, next) = decode(prg, pc);
            assert_eq!(sta, 0x8D, "DMC setup must be absolute STA");
            assert_eq!(u16::from_le_bytes([operand[0], operand[1]]), expected);
            pc = next;
        }
        pc
    }

    #[test]
    fn dmc_images_arm_the_sample_and_poll_different_sources() {
        let ram_image = dmc_ram_spin_image();
        let ram = prg(&ram_image);
        let poll = assert_dmc_setup(ram);
        assert_device_poll(ram, poll, 0x0010, 0x8014, 0xD0);
        assert_eq!(vectors(ram), (0x8000, 0x8000));
        assert!(
            ram.contains(&0x80),
            "the DMC sample filler must remain nonzero"
        );

        let device_image = dmc_device_poll_image();
        let device = prg(&device_image);
        let poll = assert_dmc_setup(device);
        assert_device_poll(device, poll, 0x4015, 0x8014, 0xD0);
        assert_eq!(vectors(device), (0x8000, 0x8000));
    }

    #[test]
    fn apu_and_controller_b_polls_target_their_registers() {
        let apu_image = apu_status_poll_image();
        let apu = prg(&apu_image);
        assert_device_poll(apu, 0, 0x4015, 0x8000, 0xD0);
        assert_eq!(vectors(apu), (0x8000, 0x8000));

        let b_image = controller_b_poll_image();
        let b = prg(&b_image);
        assert_device_poll(b, 0, 0x4017, 0x8000, 0xD0);
        assert_eq!(vectors(b), (0x8000, 0x8000));
    }
}
