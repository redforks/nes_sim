pub mod bk2;
pub mod bus;
// Shared chunked audio-push routine for the SDL, web, and capture sinks.
pub mod audio_chunk;
pub(crate) mod cpu;
pub(crate) mod dma_bus;
pub(crate) mod hotpath_cpu;
pub mod ines;

pub mod machine;
pub mod mcu;
pub mod movie;
pub mod nes;
pub mod nes_machine;
mod spin_skip;
pub mod view;
// Test utilities (private, for tests only)
#[cfg(test)]
mod test_utils;

/// Test scaffold for the $2002 spin-wait skip (spec #167, ticket #168):
/// synthetic poll-loop ROM builder, frame-boundary observer, and the
/// skip-off differential sweep. Test-only; no emulator behaviour.
#[cfg(test)]
mod spin_skip_test;

pub use cpu::*;

#[derive(Debug, Default, Clone, Copy)]
pub struct SystemClock(u64);

impl SystemClock {
    /// Test seam (spec #167): construct a clock at an absolute dot count.
    /// Production code always advances clocks via [`Self::inc`]; this
    /// constructor exists only for tests that must address one specific
    /// absolute dot — the spin-skip tests use it to force DMA timing at
    /// exact dots.
    #[cfg(test)]
    pub(crate) fn from_cycles(cycles: u64) -> SystemClock {
        SystemClock(cycles)
    }

    pub fn inc(self) -> SystemClock {
        SystemClock(self.0.wrapping_add(1))
    }

    pub fn is_apu_clock(self) -> bool {
        self.is_cpu_clock()
        // self.cpu_clock_phase() == CpuClockPhase::Last
    }

    pub fn is_cpu_clock(self) -> bool {
        // self.0.is_multiple_of(SYSTEM_CYCLES_PER_CPU_CYCLE)
        (self.0 % SYSTEM_CYCLES_PER_CPU_CYCLE) == 2
    }

    pub fn is_even_cpu_cycle(self) -> bool {
        (self.0 / SYSTEM_CYCLES_PER_CPU_CYCLE).is_multiple_of(2)
    }

    /// DMA get phase. Which CPU-cycle parity carries the get phase is a
    /// power-on coin flip on hardware; this emulator pins get to odd CPU
    /// cycles — the alignment blargg's test ROM tables encode
    /// (cpu_interrupts_v2 4-irq_and_dma, sprdma_and_dmc_dma).
    pub fn is_apu_get_clock(self) -> bool {
        !self.is_even_cpu_cycle()
    }

    pub fn cycles(self) -> u64 {
        self.0
    }
}

pub const SYSTEM_CYCLES_PER_PPU_CYCLE: u64 = 1;
const PPU_CYCLES_PER_CPU_CYCLE: u64 = 3;
pub const SYSTEM_CYCLES_PER_CPU_CYCLE: u64 = SYSTEM_CYCLES_PER_PPU_CYCLE * PPU_CYCLES_PER_CPU_CYCLE;
