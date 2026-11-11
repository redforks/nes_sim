use super::{Mapper, Mirroring, memory::CartridgeMemory};
use crate::SystemClock;

/// Action 53 (mapper 28): four configuration registers behind a
/// select/value split — `$5000-$5FFF` writes pick the CHR / inner / mode /
/// outer register, `$8000-$FFFF` writes store the value and recompute the
/// program windows, the 8 KiB character window, and the nametable slot
/// entries. Register file: CHR / inner / mode / outer; the outer-size /
/// program-mode matrix follows the reference board contract with
/// outer-size-dependent address-line decoding.
pub struct Action53 {
    regs: [u8; 4],
    selected: usize,
    mirroring_bit: u8,
}

impl Action53 {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        let mapper = Self {
            // Outer bank $FF: through the 32 KiB recompute below this maps
            // the last 32 KiB bank across the whole program window, so the
            // top window powers on with the last 16 KiB bank.
            regs: [0, 0, 0, 0xff],
            selected: 0,
            mirroring_bit: 0,
        };
        mapper.install(memory);
        mapper
    }

    fn install(&self, memory: &mut CartridgeMemory) {
        self.sync(memory);
    }

    /// Recomputes every window from the register file: both 16 KiB program
    /// slots, the 8 KiB character window, and the nametable slot entries.
    fn sync(&self, memory: &mut CartridgeMemory) {
        self.sync_prg(memory);
        self.sync_chr(memory);
        self.sync_mirroring(memory);
    }

    /// Character window: the low two CHR-register bits pick the 8 KiB
    /// CHR-RAM bank behind the pattern tables.
    fn sync_chr(&self, memory: &mut CartridgeMemory) {
        memory.map_chr_8k((self.regs[0] & 0x03) as u16);
    }

    /// Nametable slot entries: the mode-register low bits pick vertical /
    /// horizontal directly; the single-screen variants fall back to the
    /// latch, which rides on program-area writes (D4 of the CHR / inner
    /// value, bit 0 of the mode value).
    fn sync_mirroring(&self, memory: &mut CartridgeMemory) {
        let mut mirroring = self.regs[2] & 0x03;
        if mirroring & 0x02 == 0 {
            mirroring = self.mirroring_bit;
        }
        memory.apply_mirroring(match mirroring {
            0 => Mirroring::LowerBank,
            1 => Mirroring::UpperBank,
            2 => Mirroring::Vertical,
            _ => Mirroring::Horizontal,
        });
    }

    /// Bit 3 of the mode register picks the shape: clear selects the two
    /// 32 KiB program modes (mode-register bits 3-2 are 0/1, whole window
    /// follows outer/inner), set selects the fixed 16 KiB modes — mode 2
    /// holds the bottom half of the outer bank at $8000-$BFFF with the
    /// inner selection at $C000-$FFFF, mode 3 the mirror image. The fixed
    /// bank always decodes against a 32 KiB outer size so every outer bit
    /// passes through.
    fn sync_prg(&self, memory: &mut CartridgeMemory) {
        let mode = self.regs[2];
        let game_size = ((mode >> 4) & 0x03) as usize;
        const OUTER_MASK: [u16; 4] = [0x1fe, 0x1fc, 0x1f8, 0x1f0];
        const INNER_MASK: [u16; 4] = [0x01, 0x03, 0x07, 0x0f];
        let outer = (self.regs[3] as u16) << 1;
        let inner = (self.regs[1] & 0x0f) as u16;
        if mode & 0x08 != 0 {
            let slot_select = ((mode >> 2) & 0x01) as u16;
            let switch_slot = slot_select ^ 0x01;
            memory.map_prg_16k(
                switch_slot,
                (outer & OUTER_MASK[game_size]) | (inner & INNER_MASK[game_size]),
            );
            memory.map_prg_16k(slot_select, (outer & 0x1fe) | slot_select);
            return;
        }
        let inner = inner << 1;
        memory.map_prg_16k(
            0,
            (outer & OUTER_MASK[game_size]) | (inner & INNER_MASK[game_size]),
        );
        memory.map_prg_16k(
            1,
            (outer & OUTER_MASK[game_size]) | ((inner | 0x01) & INNER_MASK[game_size]),
        );
    }
}

impl Mapper for Action53 {
    fn cpu_write(
        &mut self,
        address: u16,
        value: u8,
        _cycle: SystemClock,
        memory: &mut CartridgeMemory,
    ) {
        match address {
            // Register select: $00 -> CHR, $01 -> inner, $80 -> mode,
            // $81 -> outer; only the supervisor bit and bit 0 matter.
            0x5000..=0x5fff => {
                self.selected = (((value & 0x80) >> 6) | (value & 0x01)) as usize;
            }
            0x6000..=0x7fff => memory.write_prg_ram(address, value),
            0x8000..=0xffff => {
                if self.selected <= 1 {
                    self.mirroring_bit = (value >> 4) & 0x01;
                } else if self.selected == 2 {
                    self.mirroring_bit = value & 0x01;
                }
                self.regs[self.selected] = value;
                self.sync(memory);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nes::mapper::Mirroring;

    const BANK_16K: usize = 0x4000;

    fn cpu_write(mapper: &mut Action53, memory: &mut CartridgeMemory, address: u16, value: u8) {
        mapper.cpu_write(address, value, SystemClock::default(), memory);
    }

    /// Four 16 KiB banks with a distinct marker byte at the start of each.
    fn four_bank_prg() -> Vec<u8> {
        let mut prg = vec![0u8; BANK_16K * 4];
        prg[0] = 0x10;
        prg[BANK_16K] = 0x20;
        prg[BANK_16K * 2] = 0x30;
        prg[BANK_16K * 3] = 0x40;
        let last = prg.len() - 1;
        prg[last] = 0x4f;
        prg
    }

    #[test]
    fn power_on_maps_last_16k_bank_at_top_window() {
        let prg = four_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        Action53::new(&mut memory);

        assert_eq!(memory.read_prg(0xc000), 0x40);
        assert_eq!(memory.read_prg(0xffff), 0x4f);
    }

    #[test]
    fn program_write_without_select_leaves_prg_windows_alone() {
        let prg = four_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        // Power-on select is the CHR register: a program-area write lands
        // there, not in a program register, so the windows do not move.
        cpu_write(&mut mapper, &mut memory, 0x8000, 0x00);

        assert_eq!(memory.read_prg(0x8000), 0x30);
        assert_eq!(memory.read_prg(0xc000), 0x40);
    }

    #[test]
    fn select_outer_then_value_switches_whole_window() {
        let prg = four_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        cpu_write(&mut mapper, &mut memory, 0x5000, 0x81);
        cpu_write(&mut mapper, &mut memory, 0x8000, 0x00);

        assert_eq!(memory.read_prg(0x8000), 0x10);
        assert_eq!(memory.read_prg(0xc000), 0x20);
    }

    /// Eight 16 KiB banks with a distinct marker byte at the start of each.
    fn eight_bank_prg() -> Vec<u8> {
        let mut prg = vec![0u8; BANK_16K * 8];
        for (i, marker) in [0x10u8, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80]
            .iter()
            .enumerate()
        {
            prg[i * BANK_16K] = *marker;
        }
        prg
    }

    fn select(mapper: &mut Action53, memory: &mut CartridgeMemory, which: u8) {
        cpu_write(mapper, memory, 0x5000, which);
    }

    fn store(mapper: &mut Action53, memory: &mut CartridgeMemory, value: u8) {
        cpu_write(mapper, memory, 0x8000, value);
    }

    #[test]
    fn inner_selects_within_outer_bank() {
        let prg = eight_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        select(&mut mapper, &mut memory, 0x80);
        store(&mut mapper, &mut memory, 0x10);
        select(&mut mapper, &mut memory, 0x81);
        store(&mut mapper, &mut memory, 0x00);
        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x01);

        assert_eq!(memory.read_prg(0x8000), 0x30);
        assert_eq!(memory.read_prg(0xc000), 0x40);

        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x00);

        assert_eq!(memory.read_prg(0x8000), 0x10);
        assert_eq!(memory.read_prg(0xc000), 0x20);
    }

    #[test]
    fn both_32k_modes_map_outer_inner_across_whole_window() {
        for mode in [0x10u8, 0x14] {
            let prg = eight_bank_prg();
            let mut memory = CartridgeMemory::test(&prg, &[]);
            let mut mapper = Action53::new(&mut memory);

            select(&mut mapper, &mut memory, 0x80);
            store(&mut mapper, &mut memory, mode);
            select(&mut mapper, &mut memory, 0x81);
            store(&mut mapper, &mut memory, 0x02);
            select(&mut mapper, &mut memory, 0x01);
            store(&mut mapper, &mut memory, 0x01);

            assert_eq!(memory.read_prg(0x8000), 0x70, "mode {mode:#04x} low");
            assert_eq!(memory.read_prg(0xc000), 0x80, "mode {mode:#04x} high");
        }
    }

    #[test]
    fn fixed_bottom_holds_outer_bottom_at_low_window() {
        let prg = eight_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        // Mode 2: gameSize 0 (32 KiB outer), fixed bottom at $8000-$BFFF,
        // inner-selected bank at $C000-$FFFF.
        select(&mut mapper, &mut memory, 0x80);
        store(&mut mapper, &mut memory, 0x08);
        select(&mut mapper, &mut memory, 0x81);
        store(&mut mapper, &mut memory, 0x00);
        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x01);

        assert_eq!(memory.read_prg(0x8000), 0x10);
        assert_eq!(memory.read_prg(0xc000), 0x20);

        // Moving the inner selection moves only the top window.
        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x00);

        assert_eq!(memory.read_prg(0x8000), 0x10);
        assert_eq!(memory.read_prg(0xc000), 0x10);
    }

    #[test]
    fn fixed_top_holds_outer_top_at_high_window() {
        let prg = eight_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        // Mode 3: inner-selected bank at $8000-$BFFF, fixed top of the
        // outer bank (banks 2-3) at $C000-$FFFF.
        select(&mut mapper, &mut memory, 0x80);
        store(&mut mapper, &mut memory, 0x0c);
        select(&mut mapper, &mut memory, 0x81);
        store(&mut mapper, &mut memory, 0x01);
        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x00);

        assert_eq!(memory.read_prg(0x8000), 0x30);
        assert_eq!(memory.read_prg(0xc000), 0x40);

        // Moving the inner selection moves only the bottom window.
        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x01);

        assert_eq!(memory.read_prg(0x8000), 0x40);
        assert_eq!(memory.read_prg(0xc000), 0x40);
    }

    /// Sixteen 16 KiB banks with the bank index as the marker byte.
    fn sixteen_bank_prg() -> Vec<u8> {
        let mut prg = vec![0u8; BANK_16K * 16];
        for (i, slot) in prg.chunks_mut(BANK_16K).enumerate() {
            slot[0] = i as u8;
        }
        prg
    }

    #[test]
    fn larger_outer_size_ignores_outer_low_bit() {
        let prg = eight_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        // 64 KiB outer size: outer bit 0 is masked out, so outer bank 1
        // behaves like outer bank 0; inner spans all four 16 KiB banks.
        select(&mut mapper, &mut memory, 0x80);
        store(&mut mapper, &mut memory, 0x50);
        select(&mut mapper, &mut memory, 0x81);
        store(&mut mapper, &mut memory, 0x01);
        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x03);

        assert_eq!(memory.read_prg(0x8000), 0x30);
        assert_eq!(memory.read_prg(0xc000), 0x40);
    }

    #[test]
    fn largest_outer_size_spans_full_inner_range() {
        let prg = sixteen_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        // 256 KiB outer size in 32 KiB mode: the whole inner nibble
        // addresses banks within the outer window.
        select(&mut mapper, &mut memory, 0x80);
        store(&mut mapper, &mut memory, 0x30);
        select(&mut mapper, &mut memory, 0x81);
        store(&mut mapper, &mut memory, 0x02);
        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x0f);

        assert_eq!(memory.read_prg(0x8000), 14);
        assert_eq!(memory.read_prg(0xc000), 15);
    }

    #[test]
    fn fixed_bank_uses_full_outer_bits_regardless_of_size() {
        let prg = sixteen_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        // Fixed-top with a 256 KiB outer size: the switchable window
        // masks the outer low nibble away, but the fixed top bank keeps
        // every outer bit (bank 15, not bank 1).
        select(&mut mapper, &mut memory, 0x80);
        store(&mut mapper, &mut memory, 0x3c);
        select(&mut mapper, &mut memory, 0x81);
        store(&mut mapper, &mut memory, 0x07);
        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x00);

        assert_eq!(memory.read_prg(0x8000), 0);
        assert_eq!(memory.read_prg(0xc000), 15);
    }

    #[test]
    fn fixed_bottom_follows_size_mask_on_switchable_window() {
        let prg = sixteen_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        // Fixed-bottom with a 128 KiB outer size: fixed bottom keeps the
        // outer bits, the switchable top window masks to three inner bits.
        select(&mut mapper, &mut memory, 0x80);
        store(&mut mapper, &mut memory, 0x28);
        select(&mut mapper, &mut memory, 0x81);
        store(&mut mapper, &mut memory, 0x01);
        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x05);

        assert_eq!(memory.read_prg(0x8000), 2);
        assert_eq!(memory.read_prg(0xc000), 5);
    }

    #[test]
    fn mode_register_selects_all_four_mirroring_modes() {
        for (mode, expected) in [
            (0x10u8, Mirroring::LowerBank),
            (0x11, Mirroring::UpperBank),
            (0x12, Mirroring::Vertical),
            (0x13, Mirroring::Horizontal),
        ] {
            let prg = four_bank_prg();
            let mut memory = CartridgeMemory::test(&prg, &[]);
            let mut mapper = Action53::new(&mut memory);

            select(&mut mapper, &mut memory, 0x80);
            store(&mut mapper, &mut memory, mode);

            assert_eq!(memory.mirroring(), expected, "mode {mode:#04x}");
        }
    }

    #[test]
    fn chr_d4_drives_single_screen_latch() {
        let prg = four_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        // Mode mirroring field 0 falls back to the latch.
        select(&mut mapper, &mut memory, 0x80);
        store(&mut mapper, &mut memory, 0x10);
        assert_eq!(memory.mirroring(), Mirroring::LowerBank);

        // CHR register D4 sets the latch: one screen everywhere.
        select(&mut mapper, &mut memory, 0x00);
        store(&mut mapper, &mut memory, 0x10);
        assert_eq!(memory.mirroring(), Mirroring::UpperBank);
        memory.write_vram(0x2000, 0xab);
        assert_eq!(memory.read_vram(0x2400), 0xab);
        assert_eq!(memory.read_vram(0x2800), 0xab);
        assert_eq!(memory.read_vram(0x2c00), 0xab);

        select(&mut mapper, &mut memory, 0x00);
        store(&mut mapper, &mut memory, 0x00);
        assert_eq!(memory.mirroring(), Mirroring::LowerBank);
        memory.write_vram(0x2400, 0xcd);
        assert_eq!(memory.read_vram(0x2000), 0xcd);
    }

    #[test]
    fn inner_d4_drives_single_screen_latch() {
        let prg = four_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        select(&mut mapper, &mut memory, 0x80);
        store(&mut mapper, &mut memory, 0x10);

        // Inner register D4 sets the latch the same way.
        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x1f);
        assert_eq!(memory.mirroring(), Mirroring::UpperBank);

        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x0f);
        assert_eq!(memory.mirroring(), Mirroring::LowerBank);
    }

    #[test]
    fn vertical_and_horizontal_ignore_d4_latch() {
        let prg = four_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        select(&mut mapper, &mut memory, 0x80);
        store(&mut mapper, &mut memory, 0x12);
        assert_eq!(memory.mirroring(), Mirroring::Vertical);

        // D4 writes still update the latch underneath, but the mode
        // field wins while bit 1 is set.
        select(&mut mapper, &mut memory, 0x00);
        store(&mut mapper, &mut memory, 0x13);
        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x1f);
        assert_eq!(memory.mirroring(), Mirroring::Vertical);

        select(&mut mapper, &mut memory, 0x80);
        store(&mut mapper, &mut memory, 0x13);
        assert_eq!(memory.mirroring(), Mirroring::Horizontal);

        select(&mut mapper, &mut memory, 0x01);
        store(&mut mapper, &mut memory, 0x1f);
        assert_eq!(memory.mirroring(), Mirroring::Horizontal);
    }

    #[test]
    fn chr_window_selects_ram_bank_and_survives_switching() {
        use crate::nes::mapper::Mirroring as M;
        let prg = four_bank_prg();
        let mut memory = CartridgeMemory::from_rom(&prg, &[], Some(0x8000), M::Horizontal);
        let mut mapper = Action53::new(&mut memory);

        memory.write_vram(0x0000, 0xaa);
        assert_eq!(memory.read_vram(0x0000), 0xaa);

        // CHR register 1 shows a different 8 KiB RAM bank.
        select(&mut mapper, &mut memory, 0x00);
        store(&mut mapper, &mut memory, 0x01);
        assert_eq!(memory.read_vram(0x0000), 0x00);
        memory.write_vram(0x0000, 0xbb);

        // Switching away and back preserves both banks' contents, and
        // only the low two register bits select the window.
        select(&mut mapper, &mut memory, 0x00);
        store(&mut mapper, &mut memory, 0x04);
        assert_eq!(memory.read_vram(0x0000), 0xaa);
        select(&mut mapper, &mut memory, 0x00);
        store(&mut mapper, &mut memory, 0x05);
        assert_eq!(memory.read_vram(0x0000), 0xbb);
    }

    #[test]
    fn oversized_outer_selection_wraps_through_pool() {
        let prg = four_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        select(&mut mapper, &mut memory, 0x81);
        store(&mut mapper, &mut memory, 0x03);

        assert_eq!(memory.read_prg(0x8000), 0x30);
        assert_eq!(memory.read_prg(0xc000), 0x40);
    }

    #[test]
    fn cartridge_ram_writes_survive_bank_switches() {
        let prg = four_bank_prg();
        let mut memory = CartridgeMemory::test(&prg, &[]);
        let mut mapper = Action53::new(&mut memory);

        cpu_write(&mut mapper, &mut memory, 0x6000, 0x12);

        select(&mut mapper, &mut memory, 0x81);
        store(&mut mapper, &mut memory, 0x00);

        assert_eq!(memory.read_prg(0x8000), 0x10);
        assert_eq!(memory.read_prg_ram(0x6000), 0x12);
    }
}
