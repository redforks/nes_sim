//! Memory Bus — the CPU's external bus, owning the address-space `Mcu`.
//!
//! A hardware 6502 owns no memory: it drives an address/data bus into
//! which the system address decode is wired. `Bus` models that seam — it
//! holds the `Mcu` (the wired map) and `Cpu` methods that touch memory
//! receive `&mut Bus`/`&Bus` instead of owning it. DMA arbitration (DMC
//! and OAM) lives next door, arbitrating *on* this bus: see
//! [`crate::dma_bus::DmaBus`] (ADR-0009).
//!
//! See ADR-0015 and GLOSSARY.md "Bus".

use crate::mcu::Mcu;

/// The CPU's external bus: single owner of the memory map (`Mcu`) the CPU
/// reads and writes through.
pub struct Bus<M: Mcu> {
    mcu: M,
}

impl<M: Mcu> Bus<M> {
    pub fn new(mcu: M) -> Self {
        Self { mcu }
    }

    /// The memory map wired into this bus.
    pub fn mcu(&self) -> &M {
        &self.mcu
    }

    /// Mutable access to the memory map (writes translate reads).
    pub fn mcu_mut(&mut self) -> &mut M {
        &mut self.mcu
    }
}
