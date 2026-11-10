//! MMC5 (ExROM, mapper 5): skeleton PRG (#76) plus split screen and
//! multiplier (ticket #82).
//!
//! Mode-3 PRG banking with power-on vectors visible, so MMC5 ROMs construct
//! and run frames without panic. The full PRG subsystem (modes 0-2, WRAM
//! windows, write-protect sequence) arrives in ticket #77; CHR, ExRAM,
//! nametables, IRQ and audio arrive in later tickets.
//!
//! Ticket #82 owns the CL-mode vertical-split registers (`$5200` enable /
//! side / threshold, `$5201` independent Y scroll, `$5202` split CHR bank)
//! and the unsigned 8x8 multiplier (`$5205`/`$5206`, instant 16-bit readback),
//! intercepted via the gated PPU-read seam (#75).
//!
//! Scope notes (spec #74; wiki "MMC5" 3.6.1-3.6.3, 3.6.6):
//! - Only CL mode is implemented; the smooth-split pin mode is excluded per
//!   the spec's Out of Scope list.
//! - The split byte substitution needs ExRAM-as-nametable (#79) and split
//!   CHR-bank routing (#78), so `ppu_read` currently counts NT-tile fetches,
//!   tracks the split region, and falls through (`None`) for the bytes.
//!   The decision helpers (`split_in_region`, `split_tile_row`) are pure
//!   and unit-tested; later tickets reuse the counter/region state.
//! - Scanline/in-frame counters belong to the scanline IRQ (#81): this
//!   ticket does NOT implement the 3-match detection or idle timeout, and
//!   the fetch counter exposes `reset_split_fetch` for #81 to wire to its
//!   scanline detection. Unit tests drive the reset directly until then.
//! - `$5203`/`$5204` (IRQ target/status) are untouched for #81; `$5101` (CHR
//!   mode) and `$5120-$512B`/`$5130` (CHR banks) are untouched for #78;
//!   `$5104` (ExRAM mode, which gates split in hardware) is untouched for
//!   #79, so split is not ExRAM-mode-gated yet.

use super::{CartridgeCaps, Mapper, PpuRenderState, memory::CartridgeMemory};
use crate::SystemClock;
use std::cell::Cell;

/// PRG bank number bits in `$5114-$5117`: 8 KB banks, up to 128 (1 MB).
const BANK_MASK: u8 = 0x7f;
/// Power-on PRG bank for `$E000-$FFFF`: bank `$7F` wraps to the last 8 KB
/// bank of any power-of-two ROM up to 1 MB, so the CPU vectors at
/// `$FFFA-$FFFF` are visible with no register setup (map #70).
const POWER_ON_E000_BANK: u8 = 0xff;

/// MMC5 (ExROM, mapper 5) skeleton: mode-3 PRG banking only.
///
/// Power-on state is PRG mode 3 with `$5117 = $FF` (map #70). `$5114-$5116`
/// bit 7 is the ROM/WRAM select (`1` = ROM); the skeleton maps ROM for every
/// window and WRAM windows arrive with the full PRG subsystem (#77).
pub struct MMC5 {
    /// Last values written to `$5114-$5117`: one 8 KB bank per mode-3 window
    /// at `$8000/$A000/$C000/$E000`.
    prg_banks: [u8; 4],
    prg_bank_count_8k: usize,
    /// Raw `$5200` split control: bit 7 enable, bit 6 side (0 = left,
    /// 1 = right), bits 4-0 threshold tile count (wiki 3.6.1, `[ES.T TTTT]`).
    split_mode: u8,
    /// `$5201`: independent vertical scroll for the split region (no X
    /// scroll; wiki 3.6.2).
    split_scroll: u8,
    /// `$5202`: 4 KB CHR bank used for both pattern tables while rendering
    /// the split region (wiki 3.6.3).
    split_bank: u8,
    /// `$5205` multiplicand / `$5206` multiplier; both power on as `$FF`
    /// (wiki 3.6.6).
    mult_a: u8,
    mult_b: u8,
    /// Cached unsigned product, readable instantly as `$5205` low /
    /// `$5206` high; power-on `$FF * $FF = $FE01` (wiki 3.6.6).
    mult_result: u16,
    /// Local NT-tile-fetch counter for locating the split threshold
    /// (wiki: 34 fetches/scanline locate the threshold by count). Counts only
    /// while split is enabled and the PPU render gate is open; #81 resets it
    /// from its scanline detection (it owns the in-frame/scanline counters).
    split_fetch: Cell<u8>,
    /// Whether the most recent counted fetch fell inside the split region;
    /// the handoff bit the ExRAM (#79) / CHR-bank (#78) overrides will read.
    split_active: Cell<bool>,
}

impl MMC5 {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        let mapper = Self {
            prg_banks: [0, 0, 0, POWER_ON_E000_BANK],
            prg_bank_count_8k: memory.prg_bank_count_8k(),
            split_mode: 0,
            split_scroll: 0,
            split_bank: 0,
            mult_a: 0xff,
            mult_b: 0xff,
            mult_result: 0xff * 0xff,
            split_fetch: Cell::new(0),
            split_active: Cell::new(false),
        };
        mapper.sync_prg_banks(memory);
        mapper
    }

    /// `$5200` bit 7: split enabled.
    fn split_enabled(&self) -> bool {
        self.split_mode & 0x80 != 0
    }

    /// `$5200` bit 6: split side (false = left, true = right).
    fn split_right_side(&self) -> bool {
        self.split_mode & 0x40 != 0
    }

    /// `$5200` bits 4-0: threshold tile count T.
    fn split_threshold(&self) -> u8 {
        self.split_mode & 0x1f
    }

    /// Whether NT-tile fetch `n` (1-based count this scanline) falls in the
    /// split region (wiki 3.6.1 Left/Right Split): left side renders fetches
    /// `1..=T` from the split nametable, right side renders fetches `> T`.
    /// `control` is the raw `$5200` byte, so the predicate stays pure and
    /// unit-testable without driving the fetch stream.
    fn split_in_region(n: u8, control: u8) -> bool {
        if control & 0x80 == 0 {
            return false;
        }
        let threshold = control & 0x1f;
        if control & 0x40 == 0 {
            // Left: tiles `0..T-1` are the split region.
            n <= threshold && n > 0
        } else {
            // Right: tiles `T..` are the split region.
            n > threshold
        }
    }

    /// Split nametable tile row (0-31) for a visible `scanline` under the
    /// independent `$5201` scroll (wiki 3.6.1 Vertical Behavior: the MMC5
    /// adds the scanline count to the scroll value). Scrolls 0-239 are
    /// valid; sums reaching rows 30-31 alias the ExRAM attribute bytes as
    /// nametable data, and sums past 255 roll over to the top of the
    /// nametable (32-row wrap, so the split never shows CIRAM attributes).
    /// In CL mode the fine row comes from the PPU's own scroll at the pins
    /// (smooth-split wiring excluded per spec Out of Scope), so only the
    /// coarse row is modelled here.
    fn split_tile_row(scanline: u16, scroll: u8) -> u8 {
        (((scanline + u16::from(scroll)) / 8) % 32) as u8
    }

    /// Resets the local split-fetch counter at a scanline boundary. Owned by
    /// the split logic; #81 (scanline IRQ, which owns the in-frame/scanline
    /// counters) wires this to its scanline detection once it lands.
    fn reset_split_fetch(&self) {
        self.split_fetch.set(0);
    }

    fn normalize_bank(&self, value: u8) -> u16 {
        ((value & BANK_MASK) as usize % self.prg_bank_count_8k) as u16
    }

    /// Mode 3: four independent 8 KB windows at `$8000/$A000/$C000/$E000`.
    fn sync_prg_banks(&self, memory: &mut CartridgeMemory) {
        for (slot, &value) in self.prg_banks.iter().enumerate() {
            memory.map_prg_8k(slot as u16, self.normalize_bank(value));
        }
    }
}

impl Mapper for MMC5 {
    fn cpu_write(
        &mut self,
        address: u16,
        value: u8,
        _cycle: SystemClock,
        memory: &mut CartridgeMemory,
    ) {
        match address {
            0x6000..=0x7fff => memory.write_prg_ram(address, value),
            0x5114..=0x5117 => {
                self.prg_banks[(address - 0x5114) as usize] = value;
                self.sync_prg_banks(memory);
            }
            // Ticket #82: CL-mode vertical split + multiplier.
            0x5200 => self.split_mode = value,
            0x5201 => self.split_scroll = value,
            0x5202 => self.split_bank = value,
            // Unsigned 8x8 multiplier with instant readback (wiki 3.6.6):
            // each write recomputes the product immediately.
            0x5205 => {
                self.mult_a = value;
                self.mult_result = u16::from(self.mult_a) * u16::from(self.mult_b);
            }
            0x5206 => {
                self.mult_b = value;
                self.mult_result = u16::from(self.mult_a) * u16::from(self.mult_b);
            }
            // Skeleton: every other MMC5 register (`$5100` PRG mode, `$5101`
            // CHR mode, `$5104` ExRAM mode, `$5105` nametables, `$5120-$512B`
            // CHR banks, `$5130` CHR high, `$5203-$5204` IRQ (#81),
            // `$5010/$5015` audio (#83-#84), `$5C00-$5FFF` ExRAM (#79/#80))
            // is accepted and ignored; later tickets implement them.
            _ => {}
        }
    }

    fn read_expansion(&self, address: u16) -> Option<u8> {
        match address {
            // Split registers read back the last-written values (tetanes
            // `m005_exrom.rs` parity; `$5203`/`$5204` belong to #81).
            0x5200 => Some(self.split_mode),
            0x5201 => Some(self.split_scroll),
            0x5202 => Some(self.split_bank),
            // Multiplier product (wiki 3.6.6 Read): low byte / high byte.
            0x5205 => Some((self.mult_result & 0xff) as u8),
            0x5206 => Some((self.mult_result >> 8) as u8),
            _ => None,
        }
    }

    fn ppu_read(&self, address: u16, render: &PpuRenderState) -> Option<u8> {
        // The render gate is PPU-owned (seam #75): CPU `$2007` reads with
        // rendering off must not advance the split counter.
        if !render.rendering_enabled || !self.split_enabled() {
            return None;
        }
        // Only nametable tile fetches locate the threshold (wiki 3.6.1:
        // 34 tile fetches per scanline); attribute (`$x3C0+`) and pattern
        // (`$0000-$1FFF`) fetches leave the counter alone. `$3000-$3EFF`
        // mirrors share the low 10 bits, so one range covers both.
        if matches!(address, 0x2000..=0x3eff) && address & 0x03ff < 0x03c0 {
            let n = self.split_fetch.get().wrapping_add(1);
            self.split_fetch.set(n);
            self.split_active
                .set(Self::split_in_region(n, self.split_mode));
        }
        // Byte substitution needs ExRAM-as-nametable (#79) and split
        // CHR-bank routing (#78): fall through to the window until then.
        None
    }

    fn caps(&self) -> CartridgeCaps {
        CartridgeCaps {
            ppu_read: true,
            ..CartridgeCaps::default()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nes::mapper::Mirroring;

    const PRG_8K: usize = 0x2000;

    /// 8 KB PRG banks with the bank index as the first byte of each bank,
    /// plus `vectors` as the last 6 bytes (NMI/reset/IRQ).
    fn board(banks_8k: usize, vectors: [u8; 6]) -> (MMC5, CartridgeMemory) {
        let mut prg = vec![0u8; PRG_8K * banks_8k];
        for (i, chunk) in prg.chunks_mut(PRG_8K).enumerate() {
            chunk[0] = i as u8;
        }
        let tail = prg.len() - vectors.len();
        prg[tail..].copy_from_slice(&vectors);
        let mut memory = CartridgeMemory::from_rom(&prg, &[], None, Mirroring::Horizontal);
        let mapper = MMC5::new(&mut memory);
        (mapper, memory)
    }

    // Power-on is mode 3 with `$5117 = $FF`: the last 8 KB bank covers
    // `$E000-$FFFF`, so the CPU vectors at `$FFFA-$FFFF` read correctly with
    // no register setup (spec #74 user story 4, map #70).
    #[test]
    fn power_on_shows_vectors_from_last_bank() {
        let vectors = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66];
        let (_, memory) = board(2, vectors);

        assert_eq!(memory.read_prg(0xfffa), 0x11);
        assert_eq!(memory.read_prg(0xfffb), 0x22);
        assert_eq!(memory.read_prg(0xfffc), 0x33);
        assert_eq!(memory.read_prg(0xfffd), 0x44);
        assert_eq!(memory.read_prg(0xfffe), 0x55);
        assert_eq!(memory.read_prg(0xffff), 0x66);
    }

    #[test]
    fn power_on_maps_bank_0_at_8000_and_last_bank_at_e000() {
        let (_, memory) = board(2, [0; 6]);

        assert_eq!(memory.read_prg(0x8000), 0);
        assert_eq!(memory.read_prg(0xa000), 0);
        assert_eq!(memory.read_prg(0xc000), 0);
        assert_eq!(memory.read_prg(0xe000), 1);
    }

    // Mode 3 (spec #74 user story 2): `$5114-$5117` select one 8 KB ROM bank
    // each for `$8000/$A000/$C000/$E000`. Bit 7 is the ROM select
    // (tetanes `m005_exrom.rs` `ROM_SELECT_MASK`); `$5117` always selects ROM.
    #[test]
    fn mode_3_registers_select_each_8k_window() {
        let (mut mapper, mut memory) = board(4, [0; 6]);

        mapper.cpu_write(0x5114, 0x80 | 3, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5115, 0x80 | 2, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5116, 0x80 | 1, SystemClock::default(), &mut memory);
        mapper.cpu_write(0x5117, 0x80, SystemClock::default(), &mut memory);

        assert_eq!(memory.read_prg(0x8000), 3);
        assert_eq!(memory.read_prg(0xa000), 2);
        assert_eq!(memory.read_prg(0xc000), 1);
        assert_eq!(memory.read_prg(0xe000), 0);
    }

    #[test]
    fn bank_number_wraps_past_end_of_rom() {
        let (mut mapper, mut memory) = board(4, [0; 6]);

        // `$7F` is the power-on `$5117` value: 127 wraps to the last of 4 banks.
        mapper.cpu_write(0x5117, 0xff, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0xe000), 3);

        mapper.cpu_write(0x5114, 0x80 | 6, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg(0x8000), 2);
    }

    // The boot code of the local MMC5 test ROMs writes CHR, nametable, IRQ
    // and audio registers (`$5101/$5104/$5105/$5127/$5200/$5204/$5015`):
    // the skeleton accepts and ignores them, leaving PRG mapping intact.
    #[test]
    fn other_mmc5_registers_leave_prg_mapping_intact() {
        let (mut mapper, mut memory) = board(4, [0; 6]);
        let before = [
            memory.read_prg(0x8000),
            memory.read_prg(0xa000),
            memory.read_prg(0xc000),
            memory.read_prg(0xe000),
        ];

        for address in [
            0x5100, 0x5101, 0x5104, 0x5105, 0x5120, 0x5127, 0x512b, 0x5130, 0x5200, 0x5204, 0x5010,
            0x5015, 0x8000, 0xc000,
        ] {
            mapper.cpu_write(address, 0x03, SystemClock::default(), &mut memory);
        }

        assert_eq!(
            [
                memory.read_prg(0x8000),
                memory.read_prg(0xa000),
                memory.read_prg(0xc000),
                memory.read_prg(0xe000),
            ],
            before
        );
    }

    #[test]
    fn prg_ram_window_round_trips() {
        let (mut mapper, mut memory) = board(2, [0; 6]);

        mapper.cpu_write(0x6000, 0x5a, SystemClock::default(), &mut memory);
        assert_eq!(memory.read_prg_ram(0x6000), 0x5a);
    }

    // ---- Ticket #82: split screen + multiplier ----

    use crate::nes::mapper::PpuRenderState;

    fn render_on() -> PpuRenderState {
        PpuRenderState {
            rendering_enabled: true,
            sprite_8x16: false,
            fetch_phase: 1,
        }
    }

    fn write_split(
        mapper: &mut MMC5,
        memory: &mut CartridgeMemory,
        mode: u8,
        scroll: u8,
        bank: u8,
    ) {
        mapper.cpu_write(0x5200, mode, SystemClock::default(), memory);
        mapper.cpu_write(0x5201, scroll, SystemClock::default(), memory);
        mapper.cpu_write(0x5202, bank, SystemClock::default(), memory);
    }

    // Power-on: split off (`$5200 = $00`: disabled, left, threshold 0),
    // scroll/bank zero; multiplier operands `$FF` so the product reads
    // `$FE01` (wiki 3.6.6 power-on values).
    #[test]
    fn split_and_multiplier_power_on_defaults() {
        let (mapper, _) = board(2, [0; 6]);

        assert!(!mapper.split_enabled());
        assert!(!mapper.split_right_side());
        assert_eq!(mapper.split_threshold(), 0);
        assert_eq!(mapper.read_expansion(0x5200), Some(0x00));
        assert_eq!(mapper.read_expansion(0x5201), Some(0x00));
        assert_eq!(mapper.read_expansion(0x5202), Some(0x00));
        assert_eq!(mapper.read_expansion(0x5205), Some(0x01));
        assert_eq!(mapper.read_expansion(0x5206), Some(0xfe));
    }

    // `$5200` bit contract `[ES.T TTTT]` (wiki 3.6.1; tetanes `VSsplit`):
    // enable, side and threshold decompose and read back last-written.
    #[test]
    fn split_control_register_contract() {
        let (mut mapper, mut memory) = board(2, [0; 6]);

        mapper.cpu_write(0x5200, 0xc5, SystemClock::default(), &mut memory);
        assert!(mapper.split_enabled());
        assert!(mapper.split_right_side());
        assert_eq!(mapper.split_threshold(), 5);
        assert_eq!(mapper.read_expansion(0x5200), Some(0xc5));

        mapper.cpu_write(0x5200, 0x9f, SystemClock::default(), &mut memory);
        assert!(mapper.split_enabled());
        assert!(!mapper.split_right_side());
        assert_eq!(mapper.split_threshold(), 31);

        // Bit 5 is unused: it neither enables the right side nor leaks into
        // the threshold.
        mapper.cpu_write(0x5200, 0x20, SystemClock::default(), &mut memory);
        assert!(!mapper.split_enabled());
        assert!(!mapper.split_right_side());
        assert_eq!(mapper.split_threshold(), 0);
    }

    // `$5201` scroll and `$5202` bank store and read back verbatim, without
    // disturbing the PRG windows.
    #[test]
    fn split_scroll_and_bank_registers() {
        let (mut mapper, mut memory) = board(4, [0; 6]);
        let before = memory.read_prg(0x8000);

        write_split(&mut mapper, &mut memory, 0x80, 0x37, 0xab);

        assert_eq!(mapper.read_expansion(0x5201), Some(0x37));
        assert_eq!(mapper.read_expansion(0x5202), Some(0xab));
        assert_eq!(memory.read_prg(0x8000), before);
    }

    // Left split (wiki 3.6.1): fetches `1..=T` are the split region;
    // threshold 0 splits nothing.
    #[test]
    fn split_region_left_vectors() {
        let control = 0x80 | 5;
        assert!(!MMC5::split_in_region(0, control));
        for n in 1..=5 {
            assert!(MMC5::split_in_region(n, control), "fetch {n} in region");
        }
        for n in 6..=34 {
            assert!(!MMC5::split_in_region(n, control), "fetch {n} normal");
        }
        // Threshold 0, left side: no split region at all.
        for n in 0..=34 {
            assert!(!MMC5::split_in_region(n, 0x80));
        }
        // Disabled: never in region, whatever the side/threshold bits say.
        for n in 0..=34 {
            assert!(!MMC5::split_in_region(n, 0x45));
        }
    }

    // Right split (wiki 3.6.1): fetches `> T` are the split region;
    // threshold 0 splits the whole scanline.
    #[test]
    fn split_region_right_vectors() {
        let control = 0x80 | 0x40 | 5;
        for n in 0..=5 {
            assert!(!MMC5::split_in_region(n, control), "fetch {n} normal");
        }
        for n in 6..=34 {
            assert!(MMC5::split_in_region(n, control), "fetch {n} in region");
        }
        for n in 1..=34 {
            assert!(MMC5::split_in_region(n, 0x80 | 0x40), "T=0: all split");
        }
    }

    // Split row = scanline + scroll with 32-row wrap (wiki 3.6.1 Vertical
    // Behavior): sums reaching rows 30-31 alias ExRAM attribute bytes as
    // tiles, sums past 255 roll over to the top.
    #[test]
    fn split_tile_row_vectors() {
        assert_eq!(MMC5::split_tile_row(0, 0), 0);
        assert_eq!(MMC5::split_tile_row(8, 0), 1);
        assert_eq!(MMC5::split_tile_row(0, 8), 1);
        assert_eq!(MMC5::split_tile_row(239, 0), 29);
        // Scan 239 + scroll 1 reaches row 30: attribute alias, not top.
        assert_eq!(MMC5::split_tile_row(239, 1), 30);
        // Scroll 240 shows attribute data on the first scanlines.
        assert_eq!(MMC5::split_tile_row(0, 240), 30);
        // Past 255 rolls over to the top of the nametable.
        assert_eq!(MMC5::split_tile_row(17, 239), 0);
        assert_eq!(MMC5::split_tile_row(100, 100), ((200 / 8) % 32) as u8);
    }

    // Multiplier spec vectors (wiki 3.6.6): instant recompute on every
    // write, low byte at `$5205`, high byte at `$5206`.
    #[test]
    fn multiplier_vectors() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        let mut product = |mapper: &mut MMC5, memory: &mut CartridgeMemory, a: u8, b: u8| {
            mapper.cpu_write(0x5205, a, SystemClock::default(), memory);
            mapper.cpu_write(0x5206, b, SystemClock::default(), memory);
            (mapper.read_expansion(0x5205), mapper.read_expansion(0x5206))
        };

        assert_eq!(
            product(&mut mapper, &mut memory, 0x00, 0x00),
            (Some(0x00), Some(0x00))
        );
        assert_eq!(
            product(&mut mapper, &mut memory, 0xff, 0xff),
            (Some(0x01), Some(0xfe))
        );
        assert_eq!(
            product(&mut mapper, &mut memory, 0x12, 0x34),
            (Some(0xa8), Some(0x03))
        );
        assert_eq!(
            product(&mut mapper, &mut memory, 0x80, 0x02),
            (Some(0x00), Some(0x01))
        );
        assert_eq!(
            product(&mut mapper, &mut memory, 0x01, 0xcd),
            (Some(0xcd), Some(0x00))
        );
        assert_eq!(
            product(&mut mapper, &mut memory, 0x07, 0x09),
            (Some(0x3f), Some(0x00))
        );
    }

    // Each multiplier write recomputes instantly from the other operand's
    // current value: no latch delay, either write order works.
    #[test]
    fn multiplier_recomputes_on_each_write() {
        let (mut mapper, mut memory) = board(2, [0; 6]);

        // Power-on `$5206` is `$FF`: writing only `$5205` multiplies by it.
        mapper.cpu_write(0x5205, 0x12, SystemClock::default(), &mut memory);
        assert_eq!(mapper.read_expansion(0x5205), Some(0xee)); // `0x12 * 0xFF` low
        assert_eq!(mapper.read_expansion(0x5206), Some(0x11)); // high

        mapper.cpu_write(0x5206, 0x34, SystemClock::default(), &mut memory);
        assert_eq!(mapper.read_expansion(0x5205), Some(0xa8));
        assert_eq!(mapper.read_expansion(0x5206), Some(0x03));
    }

    // The PPU-read hook counts NT tile fetches while rendering with split
    // enabled, tracks the region, and always falls through (`None`): byte
    // substitution needs ExRAM (#79) and CHR routing (#78).
    #[test]
    fn ppu_read_counts_nt_fetches_and_tracks_region() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        write_split(&mut mapper, &mut memory, 0x80 | 2, 0, 0);
        let render = render_on();

        // Pattern and attribute fetches neither count nor enter the region.
        assert_eq!(mapper.ppu_read(0x0345, &render), None);
        assert_eq!(mapper.ppu_read(0x23c0, &render), None);
        assert_eq!(mapper.split_fetch.get(), 0);
        assert!(!mapper.split_active.get());

        // `$3000-$3EFF` mirrors count the same as `$2000-$2FFF`.
        assert_eq!(mapper.ppu_read(0x2000, &render), None);
        assert_eq!(mapper.ppu_read(0x3001, &render), None);
        assert_eq!(mapper.split_fetch.get(), 2);
        assert!(mapper.split_active.get(), "left T=2: fetch 2 in region");

        assert_eq!(mapper.ppu_read(0x2042, &render), None);
        assert_eq!(mapper.split_fetch.get(), 3);
        assert!(!mapper.split_active.get(), "left T=2: fetch 3 normal");
    }

    // Right side: the region starts after the threshold.
    #[test]
    fn ppu_read_tracks_right_side_region() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        write_split(&mut mapper, &mut memory, 0x80 | 0x40 | 2, 0, 0);
        let render = render_on();

        mapper.ppu_read(0x2000, &render);
        mapper.ppu_read(0x2001, &render);
        assert!(!mapper.split_active.get());
        mapper.ppu_read(0x2002, &render);
        assert!(mapper.split_active.get());
    }

    // Render gate (PPU-owned, seam #75): CPU `$2007` reads with rendering
    // off, and any fetch while split is disabled, leave the counter alone.
    #[test]
    fn ppu_read_respects_render_gate_and_enable() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        write_split(&mut mapper, &mut memory, 0x80 | 2, 0, 0);

        mapper.ppu_read(0x2000, &PpuRenderState::disabled());
        assert_eq!(mapper.split_fetch.get(), 0);

        // Disabled split never counts, even with rendering on.
        mapper.cpu_write(0x5200, 0x00, SystemClock::default(), &mut memory);
        mapper.ppu_read(0x2000, &render_on());
        assert_eq!(mapper.split_fetch.get(), 0);
        assert!(!mapper.split_active.get());
    }

    // Scanline-boundary reset for #81 to wire to its scanline detection.
    #[test]
    fn split_fetch_reset_starts_next_scanline_at_zero() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        write_split(&mut mapper, &mut memory, 0x80 | 0x40 | 2, 0, 0);
        let render = render_on();

        for addr in 0x2000..=0x2002 {
            mapper.ppu_read(addr, &render);
        }
        assert_eq!(mapper.split_fetch.get(), 3);
        mapper.reset_split_fetch();
        assert_eq!(mapper.split_fetch.get(), 0);
        mapper.ppu_read(0x2000, &render);
        assert!(!mapper.split_active.get(), "right T=2 restarts normal");
    }
}
