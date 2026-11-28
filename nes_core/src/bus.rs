//! Memory Bus — the CPU's external bus, owning the address-space `Mcu`
//! and the master [`SystemClock`].
//!
//! A hardware 6502 owns no memory: it drives an address/data bus into
//! which the system address decode is wired. `Bus` models that seam — it
//! holds the `Mcu` (the wired map) and the master clock, so `Cpu`
//! methods that read or advance a dot receive `&mut Bus`/`&Bus` instead
//! of threading a separate `clock` value. DMA arbitration (DMC and OAM)
//! lives next door, arbitrating *on* this bus: see
//! `DmaBus` (ADR-0009).
//!
//! See ADR-0015, ADR-0018 and GLOSSARY.md "Bus".

use crate::SystemClock;
use crate::mcu::Mcu;

/// The CPU's external bus: single owner of the memory map (`Mcu`) and
/// the master [`SystemClock`] the CPU executes against.
pub struct Bus<M: Mcu> {
    mcu: M,
    clock: SystemClock,
}

impl<M: Mcu> Bus<M> {
    pub fn new(mcu: M) -> Self {
        Self {
            mcu,
            clock: SystemClock::default(),
        }
    }

    /// The memory map wired into this bus.
    #[inline]
    pub fn mcu(&self) -> &M {
        &self.mcu
    }

    /// Mutable access to the memory map (writes translate reads).
    #[inline]
    pub fn mcu_mut(&mut self) -> &mut M {
        &mut self.mcu
    }

    /// The master clock value for the current dot.
    #[inline]
    pub fn clock(&self) -> SystemClock {
        self.clock
    }

    /// Advance the master clock by one and return the new dot.
    ///
    /// Returns the *post-increment* value so the machine loop can chain
    /// the dot directly (`let clock = bus.advance()`) without a separate
    /// `bus.clock()` read or manual `inc()`. The machine loop is the
    /// only caller per dot.
    #[inline]
    pub fn advance(&mut self) -> SystemClock {
        self.clock = self.clock.inc();
        self.clock
    }

    /// Stage an arbitrary clock value (test/harness hatch: e.g. DMC
    /// get/put alignment tests need to target a specific phase).
    pub fn set_clock(&mut self, clock: SystemClock) {
        self.clock = clock;
    }
}
