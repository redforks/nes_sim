use super::{Mapper, memory::CartridgeMemory};
use crate::SystemClock;

/// Action 53 (mapper 28): four configuration registers behind a
/// select/value split — `$5000-$5FFF` writes pick the CHR / inner / mode /
/// outer register, `$8000-$FFFF` writes store the value and recompute the
/// program windows. Ticket #90 covers the register file, the two 32 KiB
/// program modes, and power-on mapping; the fixed 16 KiB modes, mirroring,
/// and the character window follow in ticket #91.
pub struct Action53 {
    regs: [u8; 4],
    selected: usize,
}

impl Action53 {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        let mapper = Self {
            // Outer bank $FF: through the 32 KiB recompute below this maps
            // the last 32 KiB bank across the whole program window, so the
            // top window powers on with the last 16 KiB bank.
            regs: [0, 0, 0, 0xff],
            selected: 0,
        };
        mapper.install(memory);
        mapper
    }

    fn install(&self, memory: &mut CartridgeMemory) {
        self.sync_prg(memory);
    }

    /// Recomputes both 16 KiB program slots from the outer/inner combination
    /// for the two 32 KiB program modes (mode-register bits 3-2 are 0/1).
    /// The fixed-bottom / fixed-top modes (bits 3-2 are 2/3) keep their
    /// windows until ticket #91 implements them.
    fn sync_prg(&self, memory: &mut CartridgeMemory) {
        let mode = self.regs[2];
        if mode & 0x08 != 0 {
            return;
        }
        let game_size = ((mode >> 4) & 0x03) as usize;
        const OUTER_MASK: [u16; 4] = [0x1fe, 0x1fc, 0x1f8, 0x1f0];
        const INNER_MASK: [u16; 4] = [0x01, 0x03, 0x07, 0x0f];
        let outer = (self.regs[3] as u16) << 1;
        let inner = ((self.regs[1] & 0x0f) as u16) << 1;
        let base = outer & OUTER_MASK[game_size];
        memory.map_prg_16k(0, base | (inner & INNER_MASK[game_size]));
        memory.map_prg_16k(1, base | ((inner | 0x01) & INNER_MASK[game_size]));
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
                self.regs[self.selected] = value;
                self.sync_prg(memory);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
