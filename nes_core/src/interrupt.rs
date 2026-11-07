//! Interrupt handshake — typed per-dot lines feeding the CPU.
//!
//! The PPU↔CPU NMI line and the IRQ-producer↔CPU IRQ level were previously
//! three ad-hoc bools threaded through `NesMachine::tick` with only comments
//! enforcing their timing. This module concentrates that knowledge behind one
//! interface: one `InterruptLines` value per dot, produced atomically,
//! consumed atomically.
//!
//! See ADR-0008 and GLOSSARY.md "CPU Interrupt Handshake".

/// Per-dot bundle driven into the CPU detectors.
///
/// `nmi` is the PPU NMI line level after this dot's PPU tick
/// (`v_blank && nmi_enable`, the `/NMI` line level).
/// `irq_level` is already time-corrected: the APU level sampled
/// **before** `tick_apu` (visible on the next dot) OR-ed with the
/// cartridge level, which callers sample only on the CPU dot
/// (post-`tick_ppu`, dot 2) — mapper counters are PPU-dot-clocked, so
/// this CPU-cycle quantization defers a mid-cycle trip to the next
/// CPU boundary. Callers build this from the PPU NMI bool + the APU
/// level + the cartridge level before the single
/// `Cpu::update_interrupt_lines` call.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InterruptLines {
    pub nmi: bool,
    pub irq_level: bool,
}

impl InterruptLines {
    pub fn new(nmi: bool, irq_level: bool) -> Self {
        Self { nmi, irq_level }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interrupt_lines_bundle_nmi_bool() {
        let il = InterruptLines::new(true, false);
        assert!(il.nmi);
        assert!(!il.irq_level);
    }
}
