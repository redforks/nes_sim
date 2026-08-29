//! DMA Bus — single owner for DMC and OAM DMA arbitration.
//!
//! The bus concentrates the `DMC-wins` arbitration policy, the single
//! `is_busy` quiescence predicate, and the one `suppress`/`reset` path
//! behind `tick(clock) -> BusOwner`. The `dmc_drove_bus` bool and the
//! `NesMcu` collision predicate become a private branch inside this module.
//!
//! See ADR-0009 and CONTEXT.md "DMA Bus".

use crate::nes::dmc_dma::DmcDma;
use crate::{
    Cpu, SystemClock,
    mcu::Mcu,
    nes::{NesMcu, apu::AudioDriver},
    render::Render,
};

/// Per-dot holder of the bus returned by [`Bus::tick`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BusOwner {
    /// CPU owns the bus this dot (no OAM DMA held it). DMC DMA alone is
    /// signalled via `cpu.frozen`, not via this enum — see ADR-0009 §2
    /// "Halt unification left explicit".
    Idle,
    /// OAM DMA held the bus this dot (halt, alignment, or transfer) without
    /// a colliding DMC read.
    Oam,
    /// OAM was active but a DMC read won the bus this dot (DMC-wins). Still
    /// counts as OAM holding for stall purposes; variant exists for
    /// diagnostics and to keep the `DMC-wins` policy explicit.
    Dmc,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct OamActive {
    pub(crate) page: u8,
    pub(crate) startup_cycles: usize,
    pub(crate) transfer_cycle: usize,
    pub(crate) latch: u8,
    pub(crate) pause_cycles: usize,
}

/// Bus owning the active DMA transfer for both channels.
///
/// Deletion test: removing this module scatters arbitration, `is_busy`
/// aggregation, and reset quiescence across `NesMachine` + `NesMcu`.
pub struct Bus {
    dmc: DmcDma,
    oam: Option<OamActive>,
}

impl Default for Bus {
    fn default() -> Self {
        Self::new()
    }
}

impl Bus {
    pub fn new() -> Self {
        Self {
            dmc: DmcDma::default(),
            oam: None,
        }
    }

    /// True while any DMA is in flight.
    ///
    /// Includes the OAM request queue still queued in the producer (`NesMcu`
    /// `oam_dma_pending`) that has not yet been drained on the next APU tick.
    /// Callers do not query `dmc_dma` or `mcu.oam_dma_active()` separately.
    pub fn is_busy<R: Render, D: AudioDriver>(&self, mcu: &NesMcu<R, D>) -> bool {
        self.dmc.is_busy() || self.oam.is_some() || mcu.has_oam_dma_pending()
    }

    /// Single suppression path for reset drain: discard freshly generated DMC
    /// fetch requests so the drain terminates even with DMC still playing.
    pub fn suppress_new_dmc_requests<R: Render, D: AudioDriver>(&mut self, mcu: &mut NesMcu<R, D>) {
        // `NesMcu::suppress_new_dmc_dma_requests` drains `Apu::take_dmc_dma_request`
        // in a loop. Keep the loop here so Bus is the single suppression seam.
        mcu.suppress_new_dmc_dma_requests();
    }

    /// Single reset path: `dmc.reset()` + `oam = None`.
    /// Time keeps running on the master `SystemClock` (caller re-anchors its
    /// time-relative state before calling this).
    pub fn reset(&mut self) {
        self.dmc.reset();
        self.oam = None;
    }

    /// Advance one dot.
    ///
    /// Called only on APU clocks from `NesMachine::tick` (or every dot with
    /// an internal no-op). Drives at most one bus access and returns who
    /// held the bus this dot. Internally `let dmc_drove = dmc.tick(cpu,clock);`
    /// then `oam_tick(clock, dmc_drove)` — the bool never leaves this method,
    /// and the `NesMcu` predicate becomes a private branch.
    pub fn tick<R: Render, D: AudioDriver>(
        &mut self,
        cpu: &mut Cpu<NesMcu<R, D>>,
        clock: SystemClock,
    ) -> BusOwner {
        if !clock.is_apu_clock() {
            return BusOwner::Idle;
        }

        // DMC tick first — it may freeze the CPU and it decides `dmc_drove`
        // (true only on the read cycle that actually transfers a byte).
        let dmc_drove = self.dmc.tick(cpu, clock);

        let oam_active = self.tick_oam_internal(cpu, clock, dmc_drove);

        // Owner reflects OAM activity only; standalone DMC is signalled via
        // `cpu.frozen` (ADR-0009 §2 "Halt unification left explicit").
        // DMC-wins is visible as `Dmc` when OAM is active and DMC drove.
        if oam_active {
            if dmc_drove {
                return BusOwner::Dmc;
            } else {
                return BusOwner::Oam;
            }
        }
        BusOwner::Idle
    }

    /// Shared OAM state machine — single source of truth for both the
    /// production `tick` (via `Cpu`) and the hand-fed-bool test seam.
    fn oam_tick<R: Render, D: AudioDriver>(
        mcu: &mut NesMcu<R, D>,
        oam: &mut Option<OamActive>,
        clock: SystemClock,
        dmc_drove: bool,
    ) -> bool {
        if let Some(mut dma) = oam.take() {
            if dma.pause_cycles > 0 {
                // OAM alignment cycle after a DMC DMA collision: no transfer.
                dma.pause_cycles -= 1;
            } else if dmc_drove && dma.startup_cycles == 0 && dma.transfer_cycle.is_multiple_of(2) {
                // DMC DMA wins the bus: the OAM read is aborted and must be
                // redone after an alignment cycle. Skipping this cycle and one
                // alignment cycle preserves the get/put phase.
                dma.pause_cycles = 1;
            } else if dma.startup_cycles > 0 {
                dma.startup_cycles -= 1;
            } else {
                let byte_index = dma.transfer_cycle / 2;
                if dma.transfer_cycle.is_multiple_of(2) {
                    let addr = ((dma.page as u16) << 8) | byte_index as u16;
                    dma.latch = mcu.read(addr);
                } else {
                    mcu.ppu_mut().write_oam_data(dma.latch);
                }
                dma.transfer_cycle += 1;
            }

            if dma.startup_cycles == 0 && dma.transfer_cycle == 512 {
                // Transfer complete — drop, but this final cycle still counted as OAM holding.
                return true;
            }

            *oam = Some(dma);
            return true;
        }

        if let Some(page) = mcu.take_oam_dma_pending() {
            // The pending request is consumed on the first tick after the
            // $4014 write cycle; that tick is the DMA halt cycle. If it is a
            // get cycle the write happened on a put cycle and one alignment
            // cycle is needed before the first read; otherwise none.
            let startup_cycles = if clock.is_apu_get_clock() { 1 } else { 0 };
            *oam = Some(OamActive {
                page,
                startup_cycles,
                transfer_cycle: 0,
                latch: 0,
                pause_cycles: 0,
            });
            return true;
        }

        false
    }

    /// Internal OAM state machine — moved from `NesMcu::tick_oam_dma`.
    ///
    /// Returns true if OAM DMA held the bus this dot (including halt,
    /// alignment, pause, and transfer cycles). The `dmc_drove` predicate is
    /// the private `DMC-wins` branch.
    fn tick_oam_internal<R: Render, D: AudioDriver>(
        &mut self,
        cpu: &mut Cpu<NesMcu<R, D>>,
        clock: SystemClock,
        dmc_drove: bool,
    ) -> bool {
        Self::oam_tick(cpu.mcu_mut(), &mut self.oam, clock, dmc_drove)
    }

    // -------------------------------------------------------------------------
    // Test-only helpers — keep OAM tests close to the bus seam
    // -------------------------------------------------------------------------

    #[cfg(test)]
    pub(crate) fn oam_state(&self) -> Option<OamActive> {
        self.oam
    }

    #[cfg(test)]
    pub(crate) fn oam_ref(&self) -> Option<&OamActive> {
        self.oam.as_ref()
    }

    /// For migrating the hand-fed-bool OAM tests without booting a full Cpu.
    /// Drives only the OAM side with an injected `dmc_drove` flag, using a
    /// direct `&mut NesMcu` (no Cpu frozen handling). Mirrors the old
    /// `NesMcu::tick_oam_dma` signature for mechanical migration; new tests
    /// should drive `Bus::tick` with a real `Cpu`.
    #[cfg(test)]
    pub(crate) fn tick_oam_for_test<R: Render, D: AudioDriver>(
        &mut self,
        mcu: &mut NesMcu<R, D>,
        clock: SystemClock,
        dmc_drove: bool,
    ) -> bool {
        Self::oam_tick(mcu, &mut self.oam, clock, dmc_drove)
    }
}
