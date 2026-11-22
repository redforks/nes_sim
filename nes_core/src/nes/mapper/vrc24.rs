use super::{CartridgeCaps, Mapper, MapperState, SnapshotError, memory::CartridgeMemory};
use crate::SystemClock;
use crate::nes::mapper::Mirroring;

#[cfg(test)]
const PRG_BANK_SIZE: usize = 0x2000;
const PPU_DOTS_PER_SCANLINE: u16 = 341;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VrcVariant {
    Vrc4a,
    Vrc4c,
    Vrc2a,
    Vrc4f,
    Vrc4e,
    Vrc2b,
    Vrc4b,
    Vrc4d,
    Vrc2c,
}

impl VrcVariant {
    pub fn is_vrc4(self) -> bool {
        matches!(
            self,
            Self::Vrc4a | Self::Vrc4c | Self::Vrc4f | Self::Vrc4e | Self::Vrc4b | Self::Vrc4d
        )
    }

    pub fn chr_shift_low_bit(self) -> bool {
        matches!(self, Self::Vrc2a)
    }

    pub fn bit_positions(self) -> (u8, u8) {
        match self {
            Self::Vrc4a => (1, 2),
            Self::Vrc4c => (6, 7),
            Self::Vrc2a => (1, 0),
            Self::Vrc4f => (0, 1),
            Self::Vrc4e => (2, 3),
            Self::Vrc2b => (0, 1),
            Self::Vrc4b => (1, 0),
            Self::Vrc4d => (3, 2),
            Self::Vrc2c => (1, 0),
        }
    }
}

pub struct Vrc24 {
    variant: VrcVariant,
    s0: u8,
    s1: u8,
    prg_ram_enabled: bool,
    prg_select_0: u8,
    prg_select_1: u8,
    prg_swap_mode: bool,
    irq_latch: u8,
    irq_counter: u8,
    irq_prescaler: u16,
    irq_mode: bool,
    irq_enable: bool,
    irq_enable_after_ack: bool,
    irq_pending: bool,
    microwire_latch: u8,
    chr_nibbles: [u8; 16],
    prg_bank_count: usize,
    chr_bank_count: usize,
}

impl Vrc24 {
    pub fn new(memory: &mut CartridgeMemory, variant: VrcVariant) -> Self {
        let (s0, s1) = variant.bit_positions();
        let mapper = Self {
            variant,
            s0,
            s1,
            prg_ram_enabled: variant.is_vrc4(),
            prg_select_0: 0,
            prg_select_1: 0,
            prg_swap_mode: false,
            irq_latch: 0,
            irq_counter: 0,
            irq_prescaler: 0,
            irq_mode: false,
            irq_enable: false,
            irq_enable_after_ack: false,
            irq_pending: false,
            microwire_latch: 0,
            chr_nibbles: [0; 16],
            prg_bank_count: memory.prg_bank_count_8k(),
            chr_bank_count: memory.chr_bank_count(),
        };
        // VRC2 boards carry no PRG-RAM, but `$6000-$7FFF` still answers with
        // the bus byte, so the cart must see those accesses.
        memory.set_prg_ram_enabled(!variant.is_vrc4() || mapper.prg_ram_enabled);
        mapper.sync_prg(memory);
        mapper.refresh_chr_banks(memory);
        mapper
    }

    fn compute_register_index(&self, address: u16) -> usize {
        let bit0 = ((address >> self.s0) & 1) as usize;
        let bit1 = ((address >> self.s1) & 1) as usize;
        bit1 << 1 | bit0
    }

    fn normalize_prg_bank(&self, bank: u8) -> u16 {
        (bank as usize % self.prg_bank_count.max(1)) as u16
    }

    fn sync_prg(&self, memory: &mut CartridgeMemory) {
        let last = self.prg_bank_count.saturating_sub(1) as u16;
        let second_last = self.prg_bank_count.saturating_sub(2) as u16;
        let switchable = self.normalize_prg_bank(self.prg_select_0);
        let (bank0, bank2) = if self.prg_swap_mode {
            (second_last, switchable)
        } else {
            (switchable, second_last)
        };
        memory.map_prg_8k(0, bank0);
        memory.map_prg_8k(1, self.normalize_prg_bank(self.prg_select_1));
        memory.map_prg_8k(2, bank2);
        memory.map_prg_8k(3, last);
    }

    fn refresh_chr_banks(&self, memory: &mut CartridgeMemory) {
        for slot in 0..8 {
            let lo = self.chr_nibbles[slot * 2] as u16;
            let hi = self.chr_nibbles[slot * 2 + 1] as u16;
            let mut bank = lo | (hi << 4);
            if self.variant.is_vrc4() {
                bank &= 0x1ff;
            } else {
                bank &= 0x0ff;
            }
            if self.variant.chr_shift_low_bit() {
                bank >>= 1;
            }
            let bank = (bank as usize % self.chr_bank_count.max(1)) as u16;
            memory.map_chr_1k(slot as u16, bank);
        }
    }

    fn decrement_irq_counter(&mut self) {
        self.irq_counter = self.irq_counter.wrapping_sub(1);
        if self.irq_counter == 0 {
            self.irq_pending = true;
            self.reload_irq_counter();
        }
    }

    fn reload_irq_counter(&mut self) {
        self.irq_counter = self.irq_latch.wrapping_neg();
    }
}

impl Mapper for Vrc24 {
    fn snapshot(&self) -> Result<MapperState, SnapshotError> {
        Ok(MapperState::Vrc24(Vrc24State {
            prg_ram_enabled: self.prg_ram_enabled,
            prg_select_0: self.prg_select_0,
            prg_select_1: self.prg_select_1,
            prg_swap_mode: self.prg_swap_mode,
            irq_latch: self.irq_latch,
            irq_counter: self.irq_counter,
            irq_prescaler: self.irq_prescaler,
            irq_mode: self.irq_mode,
            irq_enable: self.irq_enable,
            irq_enable_after_ack: self.irq_enable_after_ack,
            irq_pending: self.irq_pending,
            microwire_latch: self.microwire_latch,
            chr_nibbles: self.chr_nibbles,
        }))
    }

    fn restore(&mut self, state: &MapperState) -> Result<(), SnapshotError> {
        let MapperState::Vrc24(saved) = state else {
            return Err(SnapshotError::Mismatch);
        };
        self.prg_ram_enabled = saved.prg_ram_enabled;
        self.prg_select_0 = saved.prg_select_0;
        self.prg_select_1 = saved.prg_select_1;
        self.prg_swap_mode = saved.prg_swap_mode;
        self.irq_latch = saved.irq_latch;
        self.irq_counter = saved.irq_counter;
        self.irq_prescaler = saved.irq_prescaler;
        self.irq_mode = saved.irq_mode;
        self.irq_enable = saved.irq_enable;
        self.irq_enable_after_ack = saved.irq_enable_after_ack;
        self.irq_pending = saved.irq_pending;
        self.microwire_latch = saved.microwire_latch;
        self.chr_nibbles = saved.chr_nibbles;
        // Bank windows, mirroring, and the PRG-RAM flag live in
        // `CartridgeMemory` and are restored from its own snapshot; only
        // the registers (which steer future writes and IRQ ticks) are
        // restored here. The variant stays as the factory built it.
        Ok(())
    }

    fn cpu_write(
        &mut self,
        address: u16,
        value: u8,
        _cycle: SystemClock,
        memory: &mut CartridgeMemory,
    ) {
        match address {
            0x6000..=0x7fff => {
                if self.variant.is_vrc4() {
                    if self.prg_ram_enabled {
                        memory.write_prg_ram(address, value);
                    }
                } else {
                    self.microwire_latch = value & 0x01;
                }
            }
            0x8000..=0x8fff => {
                self.prg_select_0 = value & 0x1f;
                self.sync_prg(memory);
            }
            0x9000..=0x9fff => {
                let idx = self.compute_register_index(address);
                if self.variant.is_vrc4() && idx == 2 {
                    self.prg_ram_enabled = (value & 0x01) != 0;
                    self.prg_swap_mode = (value & 0x02) != 0;
                    memory.set_prg_ram_enabled(self.prg_ram_enabled);
                    self.sync_prg(memory);
                } else {
                    let mirroring = if self.variant.is_vrc4() {
                        match value & 0x03 {
                            0 => Mirroring::Vertical,
                            1 => Mirroring::Horizontal,
                            2 => Mirroring::LowerBank,
                            3 => Mirroring::UpperBank,
                            _ => unreachable!(),
                        }
                    } else if value & 0x01 == 0 {
                        Mirroring::Vertical
                    } else {
                        Mirroring::Horizontal
                    };
                    memory.apply_mirroring(mirroring);
                }
            }
            0xa000..=0xafff => {
                self.prg_select_1 = value & 0x1f;
                self.sync_prg(memory);
            }
            0xb000..=0xefff => {
                let group = ((address >> 12) & 0x0f) as usize;
                let idx = self.compute_register_index(address);
                let slot = (group - 0xb) * 2 + (idx >> 1);
                if idx & 1 == 0 {
                    self.chr_nibbles[slot * 2] = value & 0x0f;
                } else {
                    self.chr_nibbles[slot * 2 + 1] = value & 0x1f;
                }
                self.refresh_chr_banks(memory);
            }
            0xf000..=0xffff if self.variant.is_vrc4() => {
                let idx = self.compute_register_index(address);
                match idx {
                    0 => {
                        self.irq_latch = (self.irq_latch & 0xf0) | (value & 0x0f);
                    }
                    1 => {
                        self.irq_latch = (self.irq_latch & 0x0f) | ((value & 0x0f) << 4);
                    }
                    2 => {
                        // nesdev wiki, VRC_IRQ: "Any write to this register
                        // will acknowledge the pending IRQ and reset the
                        // prescaler. If this register is written to with 'E'
                        // set, the IRQ counter is reloaded with the latch
                        // value. If 'E' is clear, the IRQ counter remains
                        // unchanged."
                        self.irq_mode = (value & 0x04) != 0;
                        self.irq_enable_after_ack = (value & 0x01) != 0;
                        self.irq_enable = (value & 0x02) != 0;
                        self.irq_pending = false;
                        self.irq_prescaler = 0;
                        if self.irq_enable {
                            self.reload_irq_counter();
                        }
                    }
                    3 => {
                        // nesdev wiki, VRC_IRQ: "Any write to this register
                        // will acknowledge the pending IRQ. In addition, the
                        // 'A' control bit moves to the 'E' control bit...
                        // Writes to this register do not affect the current
                        // state of the IRQ counter or prescaler." 'A' itself
                        // persists until the next Control write (copy, not
                        // move), so back-to-back acknowledges are idempotent.
                        self.irq_pending = false;
                        self.irq_enable = self.irq_enable_after_ack;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    fn on_ppu_tick(&mut self, _scanline: u16) {
        // nesdev wiki, VRC_IRQ: "If IRQs are disabled ('E' bit clear),
        // neither the prescaler nor IRQ counter gets clocked." Freezing
        // mid-interval matters: an acknowledge must leave a partially
        // elapsed scanline phase intact for the next enable.
        if !self.variant.is_vrc4() || !self.irq_enable {
            return;
        }

        if self.irq_mode {
            self.irq_prescaler += 1;
            if self.irq_prescaler >= 3 {
                self.irq_prescaler -= 3;
                self.decrement_irq_counter();
            }
        } else {
            self.irq_prescaler += 1;
            if self.irq_prescaler >= PPU_DOTS_PER_SCANLINE {
                self.irq_prescaler -= PPU_DOTS_PER_SCANLINE;
                self.decrement_irq_counter();
            }
        }
    }

    fn irq_pending(&self) -> bool {
        self.irq_pending
    }

    fn caps(&self) -> CartridgeCaps {
        CartridgeCaps {
            on_ppu_tick: true,
            notify_vram_address: false,
            irq_pending: true,
            ppu_read: false,
            tick_audio: false,
            notify_prg_read: false,
            read_expansion: false,
        }
    }

    /// VRC2 boards have no PRG-RAM: `$6000-$6FFF` reads return the address
    /// byte with the microwire latch bit ORed in, `$7000-$7FFF` reads 0.
    fn read_prg(&self, address: u16) -> Option<u8> {
        if self.variant.is_vrc4() {
            return None;
        }
        match address {
            0x6000..=0x6fff => Some((address as u8) | self.microwire_latch),
            0x7000..=0x7fff => Some(0),
            _ => None,
        }
    }
}

/// Saved mutable state for [`Vrc24`] (ticket #188). The variant selector
/// (`variant`, `s0`, `s1`) is config rebuilt by the factory and never
/// stored, as are the ROM-derived bank counts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Vrc24State {
    pub prg_ram_enabled: bool,
    pub prg_select_0: u8,
    pub prg_select_1: u8,
    pub prg_swap_mode: bool,
    pub irq_latch: u8,
    pub irq_counter: u8,
    pub irq_prescaler: u16,
    pub irq_mode: bool,
    pub irq_enable: bool,
    pub irq_enable_after_ack: bool,
    pub irq_pending: bool,
    pub microwire_latch: u8,
    pub chr_nibbles: [u8; 16],
}

#[cfg(test)]
mod tests {
    use super::*;

    fn board(variant: VrcVariant, chr: &[u8]) -> (Vrc24, CartridgeMemory) {
        let prg = vec![0u8; PRG_BANK_SIZE * 2];
        let mut memory = CartridgeMemory::from_rom(&prg, chr, None, Mirroring::Horizontal);
        let mapper = Vrc24::new(&mut memory, variant);
        (mapper, memory)
    }

    fn make_vrc24_with_chr() -> (Vrc24, CartridgeMemory) {
        let bank_count = 32;
        let mut chr_data = vec![0u8; bank_count * 0x0400];
        for bank in 0..bank_count {
            chr_data[bank * 0x0400] = bank as u8;
        }
        board(VrcVariant::Vrc4f, &chr_data)
    }

    #[test]
    fn vrc4f_banking() {
        let (mut mapper, mut memory) = make_vrc24_with_chr();
        mapper.cpu_write(0xb000, 0x02, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0x0000), 0x02);
    }

    #[test]
    fn vrc4f_high_nibble() {
        let (mut mapper, mut memory) = make_vrc24_with_chr();
        mapper.cpu_write(0xb001, 0x01, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_vram(0x0000), 0x10);
    }

    #[test]
    fn writes_to_chr_ram() {
        let (_mapper, mut memory) = board(VrcVariant::Vrc4f, &[]);
        memory.write_vram(0x0000, 0xab);
        assert_eq!(memory.read_vram(0x0000), 0xab);
    }

    #[test]
    fn vrc2_prg_space_reads_the_bus_byte() {
        let (mut mapper, mut memory) = board(VrcVariant::Vrc2b, &[]);
        let read = |mapper: &Vrc24, memory: &CartridgeMemory, addr: u16| {
            mapper
                .read_prg(addr)
                .unwrap_or_else(|| memory.read_prg_ram(addr))
        };
        assert_eq!(read(&mapper, &memory, 0x6123), 0x23);
        assert_eq!(read(&mapper, &memory, 0x7000), 0x00);

        mapper.cpu_write(0x6123, 0x01, SystemClock::default(), &mut memory);
        assert_eq!(read(&mapper, &memory, 0x6123), 0x23 | 0x01);
    }

    // --- IRQ pins: black-box, driving only the Mapper surface ---

    const VARIANT: VrcVariant = VrcVariant::Vrc4f;

    fn irq_addr(idx: usize) -> u16 {
        let (s0, s1) = VARIANT.bit_positions();
        0xf000u16 | (((idx & 1) as u16) << s0) | (((idx >> 1) as u16) << s1)
    }

    fn write_irq_latch(mapper: &mut Vrc24, memory: &mut CartridgeMemory, latch: u8) {
        mapper.cpu_write(irq_addr(0), latch & 0x0f, SystemClock::default(), memory);
        mapper.cpu_write(irq_addr(1), latch >> 4, SystemClock::default(), memory);
    }

    fn write_irq_control(mapper: &mut Vrc24, memory: &mut CartridgeMemory, control: u8) {
        mapper.cpu_write(irq_addr(2), control, SystemClock::default(), memory);
    }

    /// First window-relative tick at which the IRQ level asserts, if it does.
    fn first_pending_tick(mapper: &mut Vrc24, ticks: usize) -> Option<usize> {
        for tick in 1..=ticks {
            mapper.on_ppu_tick(0);
            if mapper.irq_pending() {
                return Some(tick);
            }
        }
        None
    }

    fn acknowledge_irq(mapper: &mut Vrc24, memory: &mut CartridgeMemory) {
        mapper.cpu_write(irq_addr(3), 0, SystemClock::default(), memory);
    }

    /// Drives `ticks` PPU dots, returning the 1-based tick indices at which
    /// the IRQ line was observed asserted. Does not acknowledge.
    fn drive(mapper: &mut Vrc24, ticks: usize) -> Vec<usize> {
        let mut edges = Vec::new();
        for tick in 1..=ticks {
            mapper.on_ppu_tick(0);
            if mapper.irq_pending() {
                edges.push(tick);
            }
        }
        edges
    }

    #[test]
    fn snapshot_roundtrip_preserves_continued_behavior() {
        use crate::nes::mapper::{MapperState, NRomState, SnapshotError};

        fn fixture() -> (Vec<u8>, Vec<u8>) {
            let mut prg = vec![0u8; PRG_BANK_SIZE * 8];
            for (i, slot) in prg.chunks_mut(PRG_BANK_SIZE).enumerate() {
                slot[0] = i as u8;
            }
            let mut chr = vec![0u8; 32 * 0x0400];
            for (i, slot) in chr.chunks_mut(0x0400).enumerate() {
                slot[0] = i as u8;
            }
            (prg, chr)
        }

        fn observe(mapper: &Vrc24, memory: &CartridgeMemory) -> (u8, u8, u8, u8, bool) {
            (
                memory.read_prg(0x8000),
                memory.read_prg(0xa000),
                memory.read_vram(0x0000),
                memory.read_vram(0x0400),
                mapper.irq_pending(),
            )
        }

        let (prg, chr) = fixture();
        let mut mem_a = CartridgeMemory::from_rom(&prg, &chr, None, Mirroring::Horizontal);
        let mut a = Vrc24::new(&mut mem_a, VrcVariant::Vrc4f);
        // Non-default state: PRG selects, CHR nibbles, mirroring, and a
        // mid-interval IRQ (latch loaded, enabled, partially ticked).
        a.cpu_write(0x8000, 0x03, SystemClock::default(), &mut mem_a);
        a.cpu_write(0xa000, 0x05, SystemClock::default(), &mut mem_a);
        a.cpu_write(0xb000, 0x02, SystemClock::default(), &mut mem_a);
        a.cpu_write(0xb001, 0x01, SystemClock::default(), &mut mem_a);
        a.cpu_write(0x9000, 0x01, SystemClock::default(), &mut mem_a);
        write_irq_latch(&mut a, &mut mem_a, 0xfe);
        write_irq_control(&mut a, &mut mem_a, 0x07);
        for _ in 0..3 {
            a.on_ppu_tick(0);
        }

        let snap = a.snapshot().expect("Vrc24 snapshot");
        assert!(matches!(snap, MapperState::Vrc24(_)));

        let mut mem_b = CartridgeMemory::from_rom(&prg, &chr, None, Mirroring::Horizontal);
        let mut b = Vrc24::new(&mut mem_b, VrcVariant::Vrc4f);
        assert_eq!(
            b.restore(&MapperState::NRom(NRomState::default())),
            Err(SnapshotError::Mismatch)
        );
        b.restore(&snap).expect("Vrc24 restore");
        assert_eq!(b.snapshot().expect("re-snapshot"), snap);

        // Continued behavior matches: the pending IRQ fires on the same
        // tick and later writes land identically.
        for _ in 0..3 {
            a.on_ppu_tick(0);
            b.on_ppu_tick(0);
        }
        assert_eq!(a.irq_pending(), b.irq_pending());
        assert!(b.irq_pending());
        for (mapper, memory) in [(&mut a, &mut mem_a), (&mut b, &mut mem_b)] {
            acknowledge_irq(mapper, memory);
            mapper.cpu_write(0x8000, 0x01, SystemClock::default(), memory);
            mapper.cpu_write(0xb002, 0x04, SystemClock::default(), memory);
        }
        assert_eq!(observe(&a, &mem_a), observe(&b, &mem_b));
        assert_eq!(a.snapshot().unwrap(), b.snapshot().unwrap());
    }

    #[test]
    fn snapshot_does_not_capture_variant() {
        // The VRC variant selector is config, rebuilt by the factory: a
        // snapshot taken from a VRC4 board restores into a VRC2 board
        // without disturbing the target's variant-derived decoding.
        let (prg, chr) = (vec![0u8; PRG_BANK_SIZE * 2], vec![0u8; 0x0400]);
        let mut mem_a = CartridgeMemory::from_rom(&prg, &chr, None, Mirroring::Horizontal);
        let mut a = Vrc24::new(&mut mem_a, VrcVariant::Vrc4f);
        a.cpu_write(0x8000, 0x01, SystemClock::default(), &mut mem_a);
        a.cpu_write(0x6000, 0x55, SystemClock::default(), &mut mem_a);
        let snap = a.snapshot().expect("Vrc24 snapshot");

        let mut mem_b = CartridgeMemory::from_rom(&prg, &chr, None, Mirroring::Horizontal);
        let mut b = Vrc24::new(&mut mem_b, VrcVariant::Vrc2b);
        b.restore(&snap).expect("Vrc24 restore");
        // Shared mutable fields applied ...
        assert_eq!(b.snapshot().expect("re-snapshot"), snap);
        // ... while the target keeps its own variant config.
        assert!(!b.variant.is_vrc4());
        assert_eq!(b.variant.bit_positions(), VrcVariant::Vrc2b.bit_positions());
        // VRC2 boards answer `$6000-$6FFF` with the bus byte plus the
        // microwire latch, which proves the VRC2 decoding survived.
        assert_eq!(b.read_prg(0x6123), Some(0x23 | b.microwire_latch));
    }

    #[test]
    fn disabled_irq_never_clocks() {
        let (mut mapper, mut memory) = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, &mut memory, 0xfe);
        write_irq_control(&mut mapper, &mut memory, 0x04); // cycle mode, 'E' clear
        assert!(drive(&mut mapper, 100_000).is_empty());
    }

    #[test]
    fn reenabling_control_write_rearms_full_period() {
        let (mut mapper, mut memory) = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, &mut memory, 0xfe); // cycle period: 3*(256-254) = 6
        write_irq_control(&mut mapper, &mut memory, 0x07); // M|E|A
        drive(&mut mapper, 3); // mid-count
        write_irq_control(&mut mapper, &mut memory, 0x07); // re-sync: reload + phase reset
        assert_eq!(drive(&mut mapper, 5), Vec::<usize>::new());
        assert_eq!(
            first_pending_tick(&mut mapper, 8),
            Some(1) // full period after the write (global tick 9)
        );
    }

    #[test]
    fn acknowledge_preserves_schedule() {
        let (mut mapper, mut memory) = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, &mut memory, 0xfa); // 341*(256-250) = 2046 dots
        write_irq_control(&mut mapper, &mut memory, 0x03); // scanline mode, E|A
        drive(&mut mapper, 1000); // mid-interval
        acknowledge_irq(&mut mapper, &mut memory);
        assert_eq!(drive(&mut mapper, 1046), vec![1046]); // edge at total tick 2046
    }

    #[test]
    fn control_write_resyncs_phase_and_counter() {
        let (mut mapper, mut memory) = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, &mut memory, 0xfe); // 341*2 = 682 dots
        write_irq_control(&mut mapper, &mut memory, 0x03);
        drive(&mut mapper, 340); // one dot short of the first decrement
        write_irq_control(&mut mapper, &mut memory, 0x03); // re-sync resets phase
        assert_eq!(drive(&mut mapper, 681), Vec::<usize>::new());
        assert_eq!(drive(&mut mapper, 1), vec![1]); // global tick 1022
    }

    #[test]
    fn repeated_irq_spacing_follows_latch() {
        let (mut mapper, mut memory) = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, &mut memory, 0xfe); // interval 3*2 = 6 ticks
        write_irq_control(&mut mapper, &mut memory, 0x07); // M|E|A
        let mut edges = Vec::new();
        for tick in 1..=20 {
            mapper.on_ppu_tick(0);
            if mapper.irq_pending() {
                edges.push(tick);
                acknowledge_irq(&mut mapper, &mut memory); // 'A'=1 keeps 'E' set
            }
        }
        assert_eq!(edges, vec![6, 12, 18]); // uniform interval: 3*(256-latch)
    }

    #[test]
    fn acknowledge_with_a_clear_disables() {
        let (mut mapper, mut memory) = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, &mut memory, 0xfe);
        write_irq_control(&mut mapper, &mut memory, 0x06); // M|E, 'A' clear: one-shot
        assert_eq!(drive(&mut mapper, 6), vec![6]);
        acknowledge_irq(&mut mapper, &mut memory); // 'A'=0 moves to 'E': disabled
        assert!(drive(&mut mapper, 10_000).is_empty());
    }

    #[test]
    fn scanline_mode_uniform_period() {
        let (mut mapper, mut memory) = make_vrc24_with_chr();
        write_irq_latch(&mut mapper, &mut memory, 0xfe); // 2 decrements of 341 dots
        write_irq_control(&mut mapper, &mut memory, 0x03);
        assert_eq!(drive(&mut mapper, 681), Vec::<usize>::new());
        assert_eq!(drive(&mut mapper, 1), vec![1]); // global tick 682
    }
}
