//! Interrupt handshake — typed per-dot lines feeding the CPU.
//!
//! The PPU↔CPU NMI line and the IRQ-producer↔CPU IRQ level were previously
//! three ad-hoc bools threaded through `NesMachine::tick` with only comments
//! enforcing their timing. This module concentrates that knowledge behind one
//! interface: one `InterruptLines` value per dot, produced atomically,
//! consumed atomically.
//!
//! See ADR-0008 and GLOSSARY.md "CPU Interrupt Handshake".

/// Atomically produced PPU NMI bundle for one dot.
///
/// `level = v_blank && nmi_enable` (the `/NMI` line level).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NmiLines {
    /// `/NMI` line level after this dot's PPU tick.
    pub level: bool,
}

impl NmiLines {
    pub fn new(level: bool) -> Self {
        Self { level }
    }
}

/// Per-dot bundle driven into the CPU detectors.
///
/// `irq_level` is already time-corrected: the APU level sampled
/// **before** `tick_apu` (visible on the next dot) OR-ed with the
/// cartridge latch's CPU-quantized level. Callers build this from
/// `NmiLines` + the APU level + `CartridgeIrqLatch` before the single
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
    fn nmi_lines_bundles_atomically() {
        let lines = NmiLines::new(true);
        assert!(lines.level);
        let il = InterruptLines::new(lines, false);
        assert!(il.nmi.level);
        assert!(!il.irq_level);
    }
}
