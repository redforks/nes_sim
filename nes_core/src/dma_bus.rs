//! DmaBus — single owner for DMC and OAM DMA arbitration.
//!
//! The `DmaBus` concentrates the `DMC-wins` arbitration policy, the single
//! `is_busy` quiescence predicate, and the one `suppress`/`reset` path
//! behind `tick(cpu, bus, clock) -> BusOwner`. The `dmc_drove_bus` bool and
//! the `NesMcu` collision predicate become a private branch inside this
//! module. It arbitrates on the CPU's external [`Bus`](crate::bus::Bus),
//! which owns the memory map; this module owns only the arbitration state.
//!
//! See ADR-0009, ADR-0017 and GLOSSARY.md "DmaBus".

use crate::bus::Bus;
use crate::nes::dmc_dma::{DmcDma, DmcDmaState};
use crate::{Cpu, mcu::Mcu, nes::NesMcu};

/// Per-dot holder of the bus returned by [`DmaBus::tick`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BusOwner {
    /// CPU owns the bus this dot (no OAM DMA held it). DMC DMA alone is
    /// signalled via `cpu.frozen_reason`, not via this enum — see ADR-0009 §2
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
    pub(crate) needs_startup_align: bool,
    pub(crate) transfer_cycle: u16,
    pub(crate) latch: u8,
    pub(crate) needs_pause_align: bool,
}

/// What the OAM transfer needs from the machine besides plain `Mcu` reads:
/// the queued `$4014` request and the OAM write of the put phase.
///
/// The production impl is `NesMcu`; tests implement it for a recording `Mcu`
/// double, so the routing of the source read is observable at the `Mcu` seam.
pub(crate) trait OamDmaMcu: Mcu {
    fn take_oam_dma_pending(&mut self) -> Option<u8>;
    fn write_oam_data(&mut self, value: u8);
}

impl OamDmaMcu for NesMcu {
    fn take_oam_dma_pending(&mut self) -> Option<u8> {
        NesMcu::take_oam_dma_pending(self)
    }

    fn write_oam_data(&mut self, value: u8) {
        self.ppu_mut().write_oam_data(value);
    }
}

/// Bus owning the active DMA transfer for both channels.
///
/// Deletion test: removing this module scatters arbitration, `is_busy`
/// aggregation, and reset quiescence across `NesMachine` + `NesMcu`.
pub struct DmaBus {
    dmc: DmcDma,
    oam: Option<OamActive>,
}

impl Default for DmaBus {
    fn default() -> Self {
        Self::new()
    }
}

/// Full [`DmaBus`] execution state for save-states (ticket #184): the
/// in-flight DMC DMA phase plus the active OAM transfer, if any.
#[derive(Debug, Clone, Copy)]
pub(crate) struct DmaBusState {
    pub dmc: DmcDmaState,
    pub oam: Option<OamActive>,
}

impl DmaBus {
    /// Capture the arbitration state.
    pub(crate) fn capture_dma_bus_state(&self) -> DmaBusState {
        DmaBusState {
            dmc: self.dmc.capture_dmc_dma_state(),
            oam: self.oam,
        }
    }

    /// Restore a state previously produced by [`Self::capture_dma_bus_state`].
    pub(crate) fn restore_dma_bus_state(&mut self, s: &DmaBusState) {
        self.dmc.restore_dmc_dma_state(&s.dmc);
        self.oam = s.oam;
    }
}

impl DmaBus {
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
    pub fn is_busy(&self, mcu: &NesMcu) -> bool {
        self.dmc.is_busy() || self.oam.is_some() || mcu.has_oam_dma_pending()
    }

    /// Single suppression path for reset drain: discard freshly generated DMC
    /// fetch requests so the drain terminates even with DMC still playing.
    pub fn suppress_new_dmc_requests(&mut self, mcu: &mut NesMcu) {
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
    /// Called only on APU clocks from `NesMachine::tick`. Drives at most
    /// one bus access and returns who held the bus this dot. The current
    /// dot is read off the bus's
    /// master clock (ADR-0018). Internally `let dmc_drove = dmc.tick(cpu,bus);`
    /// then `oam_tick(bus, dmc_drove)` — the bool never leaves this method,
    /// and the `NesMcu` predicate becomes a private branch.
    pub fn tick(&mut self, cpu: &mut Cpu, bus: &mut Bus<NesMcu>) -> BusOwner {
        // DMC tick first — it may freeze the CPU and it decides `dmc_drove`
        // (true only on the read cycle that actually transfers a byte).
        let dmc_drove = self.dmc.tick(cpu, bus);
        Self::oam_tick_inner(bus, &mut self.oam, dmc_drove)
    }

    /// Shared OAM state machine — single source of truth for both the
    /// production `tick` (via `Cpu`) and the hand-fed-bool test seam.
    fn oam_tick_inner<M: OamDmaMcu>(
        bus: &mut Bus<M>,
        oam: &mut Option<OamActive>,
        dmc_drove: bool,
    ) -> BusOwner {
        fn non_idle_bus_owner(dmc_drove: bool) -> BusOwner {
            if dmc_drove {
                BusOwner::Dmc
            } else {
                BusOwner::Oam
            }
        }

        if let Some(dma) = oam.as_mut() {
            if dma.needs_pause_align {
                // OAM alignment cycle after a DMC DMA collision: no transfer.
                dma.needs_pause_align = false;
            } else if dmc_drove && !dma.needs_startup_align && dma.transfer_cycle % 2 == 0 {
                // DMC DMA wins the bus: the OAM read is aborted and must be
                // redone after an alignment cycle. Skipping this cycle and one
                // alignment cycle preserves the get/put phase.
                dma.needs_pause_align = true;
            } else if dma.needs_startup_align {
                dma.needs_startup_align = false;
            } else {
                let mcu = bus.mcu_mut();
                if dma.transfer_cycle % 2 == 0 {
                    let offset = (dma.transfer_cycle / 2) as u8;
                    dma.latch = match dma.page {
                        0 => mcu.read_zero_page(offset),
                        1 => mcu.read_stack_page(offset),
                        _ => mcu.read(((dma.page as u16) << 8) | offset as u16),
                    };
                } else {
                    mcu.write_oam_data(dma.latch);
                }
                dma.transfer_cycle += 1;
                if dma.transfer_cycle == 512 {
                    *oam = None;
                }
            }

            return non_idle_bus_owner(dmc_drove);
        }

        if let Some(page) = bus.mcu_mut().take_oam_dma_pending() {
            // The pending request is consumed on the first tick after the
            // $4014 write cycle; that tick is the DMA halt cycle. If it is a
            // get cycle the write happened on a put cycle and one alignment
            // cycle is needed before the first read; otherwise none.
            *oam = Some(OamActive {
                page,
                needs_startup_align: bus.clock().is_apu_get_clock(),
                transfer_cycle: 0,
                latch: 0,
                needs_pause_align: false,
            });
            return non_idle_bus_owner(dmc_drove);
        }

        BusOwner::Idle
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
    /// direct `&mut Bus` (no Cpu frozen handling). New tests should drive
    /// `DmaBus::tick` with a real `Cpu`.
    #[cfg(test)]
    pub(crate) fn tick_oam_for_test<M: OamDmaMcu>(
        &mut self,
        bus: &mut Bus<M>,
        dmc_drove: bool,
    ) -> BusOwner {
        Self::oam_tick_inner(bus, &mut self.oam, dmc_drove)
    }
}
