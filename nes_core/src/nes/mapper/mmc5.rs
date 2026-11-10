//! MMC5 (ExROM, mapper 5) implementation.
//!
//! Ticket #76 built mode-3 PRG banking with power-on vectors visible, so
//! MMC5 ROMs construct and run frames without panic. Ticket #81 adds the
//! scanline IRQ (spec #74 decision 17, #72 resolutions). The full PRG
//! subsystem (#77), CHR (#78), ExRAM/nametables (#79/#80), split +
//! multiplier (#82) and audio (#83) arrive in later tickets; their registers
//! are still accepted-and-ignored.

use super::{CartridgeCaps, Mapper, PpuRenderState, memory::CartridgeMemory};
use crate::SystemClock;
use std::cell::Cell;

/// PRG bank number bits in `$5114-$5117`: 8 KB banks, up to 128 (1 MB).
const BANK_MASK: u8 = 0x7f;
/// Power-on PRG bank for `$E000-$FFFF`: bank `$7F` wraps to the last 8 KB
/// bank of any power-of-two ROM up to 1 MB, so the CPU vectors at
/// `$FFFA-$FFFF` are visible with no register setup (map #70).
const POWER_ON_E000_BANK: u8 = 0xff;
/// Ticks into a scanline at which the scanline-start is detected: the fourth
/// [`Mapper::on_ppu_tick`] carrying the new scanline number. The first three
/// ticks processed the previous line's last dot and this line's dots 0-1
/// (including the dot-0 render fetch), so the fourth lands as the PPU enters
/// dot 3 — ~PPU cycle 4, the hardware attribute-fetch detection point (spec
/// #74 decision 17, #72 resolution 1).
const DETECT_DOTS_IN_LINE: u16 = 4;

/// MMC5 (ExROM, mapper 5): mode-3 PRG banking plus the scanline IRQ.
///
/// Power-on state is PRG mode 3 with `$5117 = $FF` (map #70). `$5114-$5116`
/// bit 7 is the ROM/WRAM select (`1` = ROM); the skeleton maps ROM for every
/// window and WRAM windows arrive with the full PRG subsystem (#77).
///
/// Scanline-IRQ state lives behind [`Cell`]s: the [`Mapper::ppu_read`] fetch
/// hook and [`Mapper::irq_pending`] take `&self`, while [`Mapper::cpu_write`]
/// and [`Mapper::on_ppu_tick`] take `&mut self` — interior mutability keeps
/// both paths on one state.
pub struct MMC5 {
    /// Last values written to `$5114-$5117`: one 8 KB bank per mode-3 window
    /// at `$8000/$A000/$C000/$E000`.
    prg_banks: [u8; 4],
    prg_bank_count_8k: usize,
    /// `$5203`: scanline compare target. `$00` is the special case that
    /// never matches (#72 resolution 2): the compare below requires a
    /// nonzero target, so a cleared target cannot produce pending.
    irq_target: Cell<u8>,
    /// `$5204` write bit 7: IRQ enable. Gates only the CPU IRQ line, never
    /// the sticky pending flag — disabling drops a asserted line but keeps
    /// pending, and re-enabling with pending set asserts at once (wiki
    /// "any time that scanline IRQ becomes disabled" semantics).
    irq_enabled: Cell<bool>,
    /// Sticky `$5204` read bit 7: set by the counter compare regardless of
    /// enable, cleared by a `$5204` read (acknowledge). Writing `$5203`
    /// never acknowledges, so an ISR can re-arm a larger target and stack a
    /// second split in the same frame (#72 resolution 2).
    irq_pending: Cell<bool>,
    /// Live `$5204` read bit 6: whether the PPU is actively rendering
    /// visible scanlines. Set on the first detected scanline of a frame,
    /// cleared when a scanline boundary passes with no render-enabled reads
    /// (vblank lines, or the `$2001` mask went off).
    irq_in_frame: Cell<bool>,
    /// Internal 8-bit scanline counter: reset to 0 on the first detected
    /// scanline of a frame, incremented per detected scanline after that.
    /// Compared against [`Self::irq_target`] after each increment.
    irq_counter: Cell<u8>,
    /// Last scanline number seen by [`Mapper::on_ppu_tick`]; `u16::MAX`
    /// initially so the first tick counts as a boundary.
    last_tick_scanline: Cell<u16>,
    /// `on_ppu_tick` calls seen carrying the current scanline number.
    dots_in_line: Cell<u16>,
    /// A render-enabled [`Mapper::ppu_read`] arrived since the previous
    /// scanline boundary: the line is genuinely rendering (dot-0 fills read
    /// even with the mask off, so the [`PpuRenderState`] gate — not the bare
    /// fact of a read — is what sets this).
    render_seen: Cell<bool>,
}

impl MMC5 {
    pub fn new(memory: &mut CartridgeMemory) -> Self {
        let mapper = Self {
            prg_banks: [0, 0, 0, POWER_ON_E000_BANK],
            prg_bank_count_8k: memory.prg_bank_count_8k(),
            irq_target: Cell::new(0),
            irq_enabled: Cell::new(false),
            irq_pending: Cell::new(false),
            irq_in_frame: Cell::new(false),
            irq_counter: Cell::new(0),
            last_tick_scanline: Cell::new(u16::MAX),
            dots_in_line: Cell::new(0),
            render_seen: Cell::new(false),
        };
        mapper.sync_prg_banks(memory);
        mapper
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
            // `$5203`: scanline compare target. Takes effect for the next
            // detection compare; never acknowledges a pending IRQ, so an
            // ISR can write a larger target and stack a second split in the
            // same frame (#72 resolution 2).
            0x5203 => self.irq_target.set(value),
            // `$5204` write: bit 7 is the IRQ enable flag. Only the line is
            // gated — pending, target, counter and in-frame are untouched
            // either way, so disabling then re-enabling re-asserts at once
            // when pending is set.
            0x5204 => self.irq_enabled.set(value & 0x80 != 0),
            // Skeleton: every other MMC5 register (`$5100` PRG mode, `$5101`
            // CHR mode, `$5104` ExRAM mode, `$5105` nametables, `$5120-$512B`
            // CHR banks, `$5130` CHR high, `$5200-$5202` split, `$5205-$5206`
            // multiplier, `$5010/$5015` audio, `$5C00-$5FFF` ExRAM) is
            // accepted and ignored; later tickets implement them.
            _ => {}
        }
    }

    /// Fetch-stream feed for the scanline-IRQ detector: a render-enabled
    /// read proves the PPU fetched this dot, latching the current scanline
    /// as genuinely rendering and holding off the end-of-frame exit. Reads
    /// with the mask off (the PPU's unconditional dot-0 fill still reaches
    /// the cartridge) are invisible here, exactly like the silent PPU they
    /// model. Always falls through (`None`): CHR/ExRAM/split overrides
    /// arrive in later tickets (#78/#79/#82).
    fn ppu_read(&self, _addr: u16, render: &PpuRenderState) -> Option<u8> {
        if render.rendering_enabled {
            self.render_seen.set(true);
        }
        None
    }

    /// Per-dot heartbeat for the scanline counter: detects scanline starts
    /// at ~PPU cycle 4 and retires the in-frame flag at frame end.
    ///
    /// Model note — why not the hardware's literal 3-identical-read
    /// detector: this PPU renders per-pixel through per-tile fills (four
    /// reads per tile, no dummy prefetch fetches), so the hardware's dummy-
    /// fetch triple never appears in the stream the hook sees. The mapper
    /// instead observes the same scanline starts at the same raster
    /// position through this heartbeat: the dot-0 render fetch always
    /// precedes the fourth tick of a rendered visible scanline, so
    /// detecting there lands on the same ~cycle 4 the hardware's
    /// attribute-fetch detection reports. Likewise the end-of-frame exit is
    /// line-granular (a boundary crossed with no render-enabled reads)
    /// rather than the hardware's 3-M2-cycle /RD silence, because this PPU
    /// skips fetches for disabled layers mid-line while still issuing its
    /// dot-0 fill with the mask off — per-dot silence does not imply
    /// end-of-frame here. Observable outcomes match: no counting while
    /// disabled, counter reset on re-enable, in-frame cleared across
    /// vblank.
    ///
    /// Not implemented (no seam): the NMI-vector (`$FFFA/B`) acknowledge +
    /// clear and the `$2001`-write clear — both need hooks this mapper does
    /// not have. Games acknowledge through the `$5204` read in the ISR, so
    /// the missing paths only affect un-acked pending across NMI.
    fn on_ppu_tick(&mut self, scanline: u16) {
        if scanline != self.last_tick_scanline.get() {
            self.last_tick_scanline.set(scanline);
            self.dots_in_line.set(1);
            // A boundary crossed with no render-enabled reads since the
            // previous boundary: the PPU stopped rendering (vblank lines,
            // or the mask went off) — retire in-frame.
            if !self.render_seen.replace(false) {
                self.irq_in_frame.set(false);
            }
        } else {
            self.dots_in_line.set(self.dots_in_line.get() + 1);
        }
        // Scanline-start detection: a rendered visible scanline at ~cycle 4.
        if scanline < 240
            && self.dots_in_line.get() == DETECT_DOTS_IN_LINE
            && self.render_seen.get()
        {
            if !self.irq_in_frame.get() {
                self.irq_in_frame.set(true);
                self.irq_counter.set(0);
            } else {
                let counter = self.irq_counter.get().wrapping_add(1);
                self.irq_counter.set(counter);
                let target = self.irq_target.get();
                if target != 0 && counter == target {
                    self.irq_pending.set(true);
                }
            }
        }
    }

    /// The CPU IRQ line: asserted only when the sticky pending flag is set
    /// *and* the `$5204` enable flag is set (pending sets regardless of
    /// enable, per hardware).
    fn irq_pending(&self) -> bool {
        self.irq_pending.get() && self.irq_enabled.get()
    }

    /// `$5204` read: bit 7 is the sticky pending flag (cleared as the read
    /// acknowledges the IRQ), bit 6 is the live in-frame flag. Low bits are
    /// zero here; the caller (`NesMcu`) fills them from the open-bus latch.
    fn read_expansion(&self, address: u16) -> Option<u8> {
        match address {
            0x5204 => {
                let pending = self.irq_pending.replace(false);
                Some(u8::from(pending) << 7 | u8::from(self.irq_in_frame.get()) << 6)
            }
            _ => None,
        }
    }

    /// `$5204` peek: same status bits, never acknowledges.
    fn peek_expansion(&self, address: u16) -> Option<u8> {
        match address {
            0x5204 => {
                Some(u8::from(self.irq_pending.get()) << 7 | u8::from(self.irq_in_frame.get()) << 6)
            }
            _ => None,
        }
    }

    fn caps(&self) -> CartridgeCaps {
        CartridgeCaps {
            on_ppu_tick: true,
            notify_vram_address: false,
            irq_pending: true,
            ppu_read: true,
            read_expansion: true,
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

    // --- Scanline IRQ (ticket #81, #72 resolutions) ---

    fn render_on() -> PpuRenderState {
        PpuRenderState {
            rendering_enabled: true,
            sprite_8x16: false,
            fetch_phase: 0,
        }
    }

    fn enable_irq(mapper: &mut MMC5, memory: &mut CartridgeMemory, target: u8) {
        mapper.cpu_write(0x5203, target, SystemClock::default(), memory);
        mapper.cpu_write(0x5204, 0x80, SystemClock::default(), memory);
    }

    /// Drive one fully-rendered visible scanline through the detector: the
    /// line-change tick, the dot-0 render fetch that latches the line as
    /// rendered, then ticks through the detection dot.
    fn run_rendered_scanline(mapper: &mut MMC5, line: u16) {
        mapper.on_ppu_tick(line);
        mapper.ppu_read(0x2000, &render_on());
        for _ in 0..3 {
            mapper.on_ppu_tick(line);
        }
    }

    /// Drive one mask-off scanline: only the PPU's unconditional dot-0
    /// fill reaches the cartridge, with rendering disabled, so the detector
    /// must ignore it (silent PPU).
    fn run_dark_scanline(mapper: &mut MMC5, line: u16) {
        mapper.on_ppu_tick(line);
        mapper.ppu_read(0x2000, &PpuRenderState::disabled());
        for _ in 0..3 {
            mapper.on_ppu_tick(line);
        }
    }

    /// Ticks 1-3 of a rendered target scanline: the line-change tick plus
    /// the dot-0 fetch and two more ticks, stopping just before detection.
    fn run_scanline_to_before_detect(mapper: &mut MMC5, line: u16) {
        mapper.on_ppu_tick(line);
        mapper.ppu_read(0x2000, &render_on());
        mapper.on_ppu_tick(line);
        mapper.on_ppu_tick(line);
    }

    fn status(mapper: &MMC5) -> u8 {
        mapper.read_expansion(0x5204).expect("$5204 is handled")
    }

    // IRQ asserts at ~PPU cycle 4 of the target scanline: pending is still
    // clear after the first three ticks of the target line and set after
    // the fourth (spec #74 user story 17, #72 resolution 1).
    #[test]
    fn irq_asserts_at_fourth_tick_of_target_scanline() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        enable_irq(&mut mapper, &mut memory, 5);

        for line in 0..5 {
            run_rendered_scanline(&mut mapper, line);
            assert!(!mapper.irq_pending(), "no IRQ through line {line}");
        }
        run_scanline_to_before_detect(&mut mapper, 5);
        assert!(!mapper.irq_pending(), "no IRQ before the detection tick");
        mapper.on_ppu_tick(5);
        assert!(mapper.irq_pending(), "IRQ asserts on the fourth tick");
        assert_ne!(0, status(&mapper) & 0x80, "pending flag reads set");
    }

    // `$5203 = 0` never matches, even when the counter passes through 0
    // at the top of the frame (#72 resolution 2).
    #[test]
    fn target_zero_never_matches() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        enable_irq(&mut mapper, &mut memory, 0);

        for line in 0..240 {
            run_rendered_scanline(&mut mapper, line);
            assert!(!mapper.irq_pending(), "no IRQ on line {line}");
        }
        assert_eq!(0, status(&mapper) & 0x80, "pending never sets");
    }

    // Writing `$5203` never acknowledges: an ISR that reads `$5204` then
    // writes a larger target stacks a second split later in the same frame
    // (#72 resolution 2). Without the ack read, pending survives the write.
    #[test]
    fn rearm_with_larger_target_stacks_second_irq() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        enable_irq(&mut mapper, &mut memory, 5);

        for line in 0..=5 {
            run_rendered_scanline(&mut mapper, line);
        }
        assert!(mapper.irq_pending(), "first IRQ fires at line 5");

        mapper.cpu_write(0x5203, 10, SystemClock::default(), &mut memory);
        assert_ne!(
            0,
            status(&mapper) & 0x80,
            "$5203 write does not acknowledge"
        );
        // The status read above acknowledged; re-arm for the second split.
        mapper.cpu_write(0x5203, 10, SystemClock::default(), &mut memory);
        for line in 6..=10 {
            run_rendered_scanline(&mut mapper, line);
        }
        assert!(mapper.irq_pending(), "second IRQ fires at line 10");
    }

    // `$5204` read bit 7 is pending cleared-on-read, bit 6 is the live
    // in-frame flag: set while the frame renders, clear after rendering
    // stops (#72 resolution 3).
    #[test]
    fn status_read_clears_pending_keeps_in_frame() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        enable_irq(&mut mapper, &mut memory, 2);

        for line in 0..=2 {
            run_rendered_scanline(&mut mapper, line);
        }
        assert!(mapper.irq_pending());
        assert_eq!(0xc0, status(&mapper), "pending + in-frame read set");
        assert_eq!(0x40, status(&mapper), "pending cleared, in-frame live");
        assert!(!mapper.irq_pending(), "IRQ line drops on acknowledge");

        // A scanline boundary with no rendered reads retires in-frame.
        run_dark_scanline(&mut mapper, 3);
        run_dark_scanline(&mut mapper, 4);
        assert_eq!(0, status(&mapper) & 0x40, "in-frame clears off-frame");
    }

    // Counting is gated by the PPU-passed render state: mask-off scanlines
    // never detect, never set in-frame, and never count (#72 resolution 4).
    #[test]
    fn mask_off_scanlines_never_count() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        enable_irq(&mut mapper, &mut memory, 5);

        for line in 0..240 {
            run_dark_scanline(&mut mapper, line);
            assert!(!mapper.irq_pending(), "no IRQ on dark line {line}");
        }
        assert_eq!(0, status(&mapper), "neither flag sets while disabled");
    }

    // A fully dark line retires in-frame (the line-granular form of the
    // hardware /RD-idle exit), so the next rendered line restarts the
    // counter at 0 instead of continuing it: with the target at 1 and line
    // 1 dark, the IRQ lands on line 3, not line 2.
    #[test]
    fn dark_line_restarts_counter() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        enable_irq(&mut mapper, &mut memory, 1);

        run_rendered_scanline(&mut mapper, 0);
        assert_eq!(0x40, status(&mapper), "frame starts, counter at 0");
        run_dark_scanline(&mut mapper, 1);
        assert!(!mapper.irq_pending(), "dark line detects nothing");
        run_rendered_scanline(&mut mapper, 2);
        assert!(!mapper.irq_pending(), "counter restarted, not continued");
        run_rendered_scanline(&mut mapper, 3);
        assert!(mapper.irq_pending(), "IRQ lands one live line later");
    }

    // The counter resets every frame: vblank retires in-frame, so the next
    // frame's first line re-arms from zero and the same target fires again.
    #[test]
    fn counter_resets_each_frame() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        enable_irq(&mut mapper, &mut memory, 5);

        for line in 0..=5 {
            run_rendered_scanline(&mut mapper, line);
        }
        assert!(mapper.irq_pending(), "frame 1 fires at line 5");
        assert_ne!(0, status(&mapper) & 0x80, "ISR observes pending");
        assert_eq!(0, status(&mapper) & 0x80, "ISR acknowledges");

        for line in 6..240 {
            run_rendered_scanline(&mut mapper, line);
        }
        for line in 240..262 {
            mapper.on_ppu_tick(line);
        }
        assert_eq!(0, status(&mapper) & 0x40, "vblank retires in-frame");

        for line in 0..=5 {
            run_rendered_scanline(&mut mapper, line);
        }
        assert!(mapper.irq_pending(), "frame 2 fires at line 5 again");
    }

    // The pending flag sets regardless of enable; enable gates only the
    // CPU line. Disabling drops the line but keeps pending, and
    // re-enabling with pending set asserts at once.
    #[test]
    fn irq_line_requires_enable_but_pending_sets_regardless() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        mapper.cpu_write(0x5203, 2, SystemClock::default(), &mut memory);

        for line in 0..=2 {
            run_rendered_scanline(&mut mapper, line);
        }
        assert!(!mapper.irq_pending(), "line stays low while disabled");
        assert_ne!(0, status(&mapper) & 0x80, "pending still sets");
        assert_eq!(0, status(&mapper) & 0x80, "status read acknowledges");

        mapper.cpu_write(0x5203, 2, SystemClock::default(), &mut memory);
        for line in 3..=4 {
            run_rendered_scanline(&mut mapper, line);
        }
        assert!(!mapper.irq_pending());
        mapper.cpu_write(0x5204, 0x80, SystemClock::default(), &mut memory);
        // Pending was acknowledged above, so enabling alone asserts nothing
        // until the counter matches again...
        run_rendered_scanline(&mut mapper, 5);
        run_rendered_scanline(&mut mapper, 6);
        assert!(!mapper.irq_pending(), "counter moved past the target");
    }

    // Disabling with pending set drops the line but keeps the flag, and
    // re-enabling asserts immediately.
    #[test]
    fn disable_keeps_pending_reenable_asserts_at_once() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        enable_irq(&mut mapper, &mut memory, 2);

        for line in 0..=2 {
            run_rendered_scanline(&mut mapper, line);
        }
        assert!(mapper.irq_pending());
        mapper.cpu_write(0x5204, 0x00, SystemClock::default(), &mut memory);
        assert!(!mapper.irq_pending(), "line drops while disabled");
        assert_ne!(
            0,
            mapper.peek_expansion(0x5204).expect("$5204 peeks") & 0x80,
            "pending survives disable"
        );
        mapper.cpu_write(0x5204, 0x80, SystemClock::default(), &mut memory);
        assert!(mapper.irq_pending(), "re-enable asserts at once");
    }

    // The debugger peek reports the same status bits but never
    // acknowledges: peeking twice keeps pending set, reading clears it.
    #[test]
    fn peek_reports_status_without_acknowledging() {
        let (mut mapper, mut memory) = board(2, [0; 6]);
        enable_irq(&mut mapper, &mut memory, 1);

        run_rendered_scanline(&mut mapper, 0);
        run_rendered_scanline(&mut mapper, 1);
        assert!(mapper.irq_pending());
        let peek = || mapper.peek_expansion(0x5204).expect("$5204 peeks");
        assert_eq!(0xc0, peek(), "peek sees pending + in-frame");
        assert_eq!(0xc0, peek(), "peek does not acknowledge");
        assert!(mapper.irq_pending(), "line still asserted after peeks");
        assert_eq!(0xc0, status(&mapper), "read sees the same bits");
        assert_eq!(0x40, peek(), "read acknowledged, in-frame live");
    }

    // Unhandled expansion addresses fall through: `$5203` is write-only
    // and every other register belongs to a later ticket.
    #[test]
    fn unhandled_expansion_reads_fall_through() {
        let (mapper, _) = board(2, [0; 6]);
        assert_eq!(None, mapper.read_expansion(0x5203));
        assert_eq!(None, mapper.read_expansion(0x5200));
        assert_eq!(None, mapper.peek_expansion(0x5203));
        assert_eq!(None, mapper.peek_expansion(0x5c00));
    }

    // The chip declares every hook the IRQ uses, so the cartridge seam
    // actually forwards ticks, fetches and status reads to it.
    #[test]
    fn caps_declare_all_irq_hooks() {
        let (mapper, _) = board(2, [0; 6]);
        assert_eq!(
            CartridgeCaps {
                on_ppu_tick: true,
                notify_vram_address: false,
                irq_pending: true,
                ppu_read: true,
                read_expansion: true,
            },
            mapper.caps()
        );
    }
}
