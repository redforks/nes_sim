//! Interrupt handshake — typed per-dot lines feeding the CPU.
//!
//! The PPU↔CPU NMI line and the IRQ-producer↔CPU IRQ level were previously
//! three ad-hoc bools threaded through `NesMachine::tick` with only comments
//! enforcing their timing. This module concentrates that knowledge behind one
//! interface: one `InterruptLines` value per dot, produced atomically,
//! consumed atomically.
//!
//! See ADR-0008 and CONTEXT.md "CPU Interrupt Handshake".

/// Atomically produced PPU NMI bundle for one dot.
///
/// `level = v_blank && nmi_enable` (the `/NMI` line level).
/// `race_cancel = true` when a `$2000` or `$2002` access on the
/// `vbl_set_cycle` dot suppressed the same-dot assertion — the level's
/// rise never asserted and the edge must be retracted via
/// `consumed_through` (ADR-0006).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NmiLines {
    /// `/NMI` line level after this dot's PPU tick.
    pub level: bool,
    /// Same-dot suppression flag coalesced from all racing sites.
    pub race_cancel: bool,
}

impl NmiLines {
    pub fn new(level: bool, race_cancel: bool) -> Self {
        Self { level, race_cancel }
    }
}

/// Per-dot bundle driven into the CPU detectors.
///
/// `irq_level` is already time-corrected: the APU level sampled
/// **before** `tick_apu` (visible on the next dot) OR-ed with the
/// cartridge latch's CPU-quantized level. Callers build this from
/// `NmiLines` + `ApuIrqSampler` + `CartridgeIrqLatch` before the single
/// `Cpu::update_interrupt_lines` call.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InterruptLines {
    pub nmi: NmiLines,
    pub irq_level: bool,
}

impl InterruptLines {
    pub fn new(nmi: NmiLines, irq_level: bool) -> Self {
        Self { nmi, irq_level }
    }
}

/// Two-stage dot-captured / CPU-cycle-latched quantization of mapper IRQ.
///
/// Mapper counters (`MMC3::clock_irq`, `Vrc24`, …) are PPU-dot-clocked, but
/// `cpu_interrupts_v2` / `mmc3_irq_tests` pin the CPU-cycle quantization:
/// a counter trip during a PPU dot becomes CPU-visible only on the next
/// CPU-cycle boundary. The two bools that were free fields in `NesMachine`
/// live here with the invariant attached.
///
/// Protocol per dot (see `NesMachine::tick`):
/// - `capture_next(level)` every dot, post-`tick_ppu`
/// - `latch_on_cpu_tick()` only on `is_cpu_clock()` dots — copies `next → latched`
/// - `level()` is the CPU-visible latched level fed to `InterruptLines`
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct CartridgeIrqLatch {
    next: bool,
    latched: bool,
}

impl CartridgeIrqLatch {
    pub fn new() -> Self {
        Self::default()
    }

    /// Capture the raw mapper IRQ level for this dot (call every dot).
    pub fn capture_next(&mut self, level: bool) {
        self.next = level;
    }

    /// Latch `next → latched` on CPU dots only (call only when `is_cpu_clock()`).
    pub fn latch_on_cpu_tick(&mut self) {
        self.latched = self.next;
    }

    /// CPU-visible latched level.
    pub fn level(&self) -> bool {
        self.latched
    }

    /// Raw next value (for debugging / tests).
    pub fn next(&self) -> bool {
        self.next
    }

    pub fn reset(&mut self) {
        self.next = false;
        self.latched = false;
    }
}

/// APU IRQ sampling with the one-dot skew made structural.
///
/// The APU's frame IRQ / DMC IRQ flag is **sampled BEFORE `tick_apu`**
/// runs this dot; a transition the step raises or clears reaches the
/// CPU's IRQ input on the **next dot** (a one-dot `pre-tick → visible
/// next dot` mapping). The APU flag never drives a physical line, so
/// the mapping is an emulation choice pinned by blargg's
/// `cpu_interrupts_v2` 3-nmi_and_irq (moving the sample after `tick_apu`
/// fails that ROM). Calling `sample_before_tick` before `tick_apu`
/// encodes the ordering in the interface, not in a comment.
///
/// The sampler itself is stateless; it exists so the ordering is a
/// named call, not a free-floating line order in `NesMachine::tick`.
#[derive(Debug, Clone, Copy, Default)]
pub struct ApuIrqSampler;

impl ApuIrqSampler {
    pub fn new() -> Self {
        Self
    }

    /// Sample the current APU IRQ level **before** `tick_apu`.
    /// The returned level is the CPU-visible level for the next dot only
    /// after OR-ing with the cartridge latch; the caller must not call
    /// `tick_apu` before this sample.
    #[inline]
    pub fn sample_before_tick(&self, pending: bool) -> bool {
        pending
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cartridge_latch_quantizes_to_cpu_cycle() {
        let mut latch = CartridgeIrqLatch::new();
        // dot 0: pending false, cpu_tick
        latch.capture_next(false);
        latch.latch_on_cpu_tick();
        assert!(!latch.level());

        // dot 1: mapper raises mid-CPU-cycle (non-CPU dot)
        latch.capture_next(true);
        // no latch — level still false (not yet CPU-visible)
        assert!(!latch.level());

        // dot 2: CPU dot — latch copies
        latch.latch_on_cpu_tick();
        assert!(latch.level());

        // dot 3-4: mapper clears on non-CPU dots
        latch.capture_next(false);
        assert!(latch.level()); // still latched
        latch.capture_next(false);
        // next CPU dot: clears
        latch.latch_on_cpu_tick();
        assert!(!latch.level());
    }

    #[test]
    fn apu_sampler_is_identity_but_named() {
        let sampler = ApuIrqSampler::new();
        assert!(sampler.sample_before_tick(true));
        assert!(!sampler.sample_before_tick(false));
    }

    #[test]
    fn nmi_lines_bundles_atomically() {
        let lines = NmiLines::new(true, true);
        assert!(lines.level);
        assert!(lines.race_cancel);
        let il = InterruptLines::new(lines, false);
        assert!(il.nmi.race_cancel);
        assert!(!il.irq_level);
    }
}
