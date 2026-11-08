use crate::SystemClock;
use crate::ines::INesFile;
use crate::mcu::Mcu;
use crate::nes::{
    apu::{Apu, AudioDriver},
    controller::{Button, Controller, Zapper},
    lower_ram::LowerRam,
    ppu::{Ppu, Timing},
};
use crate::render::Render;
use arraydeque::ArrayDeque;
pub mod apu;
pub mod controller;
pub(crate) mod dmc_dma;
mod lower_ram;
mod mapper;
pub use mapper::Mmc3IrqOverride;
pub mod ppu;

pub struct NesMcu<R: Render, D: AudioDriver> {
    lower_ram: LowerRam,
    ppu: Ppu<R>,
    cartridge: mapper::Cartridge,
    controller: Controller,
    apu: Apu<D>,
    oam_dma_pending: Option<u8>,
    /// CPU data bus open bus value: the last value read by the CPU.
    /// Reading write-only or unmapped addresses returns this value.
    open_bus: u8,
    /// Joypad /OE lines stay asserted across contiguous reads of the same
    /// register on NES-001 hardware, clocking the shift register once per
    /// contiguous set rather than once per read.
    joypad1_oe: bool,
    joypad2_oe: bool,
    /// Length-counter register writes ($4000/$4003/…) queued to land one CPU
    /// cycle after their store instruction: on hardware the 6502 write
    /// completes on the following phi2 edge, so a scheduled length clock
    /// sharing that boundary resolves BEFORE the write does — blargg's
    /// len_halt/len_reload timing races. Entries are (apply_at_tick,
    /// address, value), kept in ascending time order.
    ///
    /// A fixed 2-slot `ArrayDeque` (no heap): production never holds more
    /// than 1 (tick_apu drains on the same dots cpu.tick runs, one write
    /// per cycle max) and the tightest direct-API sequence queues 2, so
    /// the buffer never fills; the push site debug-asserts this.
    deferred_apu_writes: ArrayDeque<(u64, u16, u8), 2>,
}

impl<R: Render, D: AudioDriver> NesMcu<R, D> {
    /// Latch the CPU open bus with the value a read returned.
    ///
    /// Every read — generic dispatch or fast path — goes through here, so
    /// the two routes cannot drift (see GLOSSARY "Fast Path Contract").
    fn latch_read(&mut self, value: u8, read_controller: bool) -> u8 {
        self.open_bus = value;
        if !read_controller {
            self.joypad1_oe = false;
            self.joypad2_oe = false;
        }
        value
    }

    /// Apply the observable side effects of a CPU write: latch the open bus
    /// and deassert both joypad /OE lines, so a store breaks contiguous-read
    /// detection regardless of which route performed it.
    ///
    /// Called first thing by both the generic `write` and the two fast-path
    /// write overrides — the same point in the sequence — so the routes
    /// cannot drift in either effect or order.
    fn latch_write(&mut self, value: u8) {
        self.open_bus = value;
        self.joypad1_oe = false;
        self.joypad2_oe = false;
    }
    pub fn new(file: &INesFile, renderer: R, audio_driver: D) -> Self {
        Self::new_with_mmc3_irq_override(file, renderer, audio_driver, Mmc3IrqOverride::Auto)
    }

    /// [`Self::new`] with an explicit MMC3 IRQ-revision override — the
    /// harness/test hook (step 1) of [`mmc3_irq_revision_is_alternate`].
    /// [`Mmc3IrqOverride::Auto`] uses [`mapper::create_cartridge_with_mmc3_irq_override`]
    /// auto-detection.
    pub(crate) fn new_with_mmc3_irq_override(
        file: &INesFile,
        renderer: R,
        audio_driver: D,
        irq_override: Mmc3IrqOverride,
    ) -> Self {
        let cartridge = mapper::create_cartridge_with_mmc3_irq_override(file, irq_override);
        let ppu = Ppu::new(renderer);

        Self {
            lower_ram: LowerRam::new(),
            ppu,
            cartridge,
            controller: Controller::new(),
            apu: Apu::new(audio_driver),
            oam_dma_pending: None,
            open_bus: 0,
            joypad1_oe: false,
            joypad2_oe: false,
            deferred_apu_writes: ArrayDeque::new(),
        }
    }

    /// Reset all bus devices to their power-up contract. The DMA bus is now
    /// owned by `DmaBus` and reset separately; by the time this
    /// runs, DMA work must already be quiesced (see [`crate::NesMachine::reset`]),
    /// so OAM pending should be idle here. The bus owns the active OAM state.
    ///
    /// Deferred APU writes are discarded, not re-anchored: each write
    /// anchors itself to the `SystemClock` carried by its own `write` call,
    /// so no time-relative state needs surviving the reset boundary.
    pub fn reset(&mut self) {
        debug_assert!(self.oam_dma_pending.is_none());
        self.deferred_apu_writes.clear();
        self.ppu.reset();
        self.apu.reset();
    }

    /// True while an OAM DMA request is still queued in the producer.
    /// The active transfer lives in `DmaBus`; use
    /// `DmaBus::is_busy(mcu)` for the full quiescence predicate.
    pub fn has_oam_dma_pending(&self) -> bool {
        self.oam_dma_pending.is_some()
    }

    pub(crate) fn take_oam_dma_pending(&mut self) -> Option<u8> {
        self.oam_dma_pending.take()
    }

    /// Discard DMC sample-fetch requests generated while a machine reset is
    /// pending. Without this, a continuously playing DMC channel would spawn
    /// fresh DMA requests forever during the reset drain, keeping the bus
    /// busy until the safety bound trips.
    pub fn suppress_new_dmc_dma_requests(&mut self) {
        while self.apu.take_dmc_dma_request().is_some() {}
    }

    fn ppu_dma(&mut self, address: u8) {
        self.oam_dma_pending = Some(address);
    }

    pub fn tick_ppu(&mut self) {
        self.ppu.tick(&mut self.cartridge);
    }

    pub fn tick_apu(&mut self, clock: SystemClock) {
        self.apu.tick();
        while let Some(&(at, address, value)) = self.deferred_apu_writes.front() {
            if at > clock.cycles() {
                break;
            }
            self.deferred_apu_writes.pop_front();
            self.apu.write(address, value, clock);
        }
    }

    pub fn apu_irq_pending(&self) -> bool {
        self.apu.request_irq()
    }

    pub fn cartridge_irq_pending(&self) -> bool {
        self.cartridge.irq_pending()
    }

    pub fn flush_audio(&mut self) {
        self.apu.flush();
    }

    pub fn press_controller_a(&mut self, button: Button) {
        self.controller.a.press(button);
    }

    pub fn release_controller_a(&mut self, button: Button) {
        self.controller.a.release(button);
    }

    pub fn press_controller_b(&mut self, button: Button) {
        self.controller.b.press(button);
    }

    pub fn release_controller_b(&mut self, button: Button) {
        self.controller.b.release(button);
    }

    pub fn connect_zapper(&mut self, connected: bool) {
        self.controller.zapper.set_connected(connected);
    }

    pub fn aim_zapper(&mut self, x: u16, y: u16) {
        self.controller.zapper.aim(x, y);
    }

    pub fn trigger_zapper(&mut self) {
        self.controller.zapper.trigger();
    }

    /// Advance the Zapper by one CPU cycle.
    pub fn tick_zapper(&mut self) {
        self.controller.zapper.clock();
    }

    pub fn zapper(&self) -> &Zapper {
        &self.controller.zapper
    }

    pub fn zapper_mut(&mut self) -> &mut Zapper {
        &mut self.controller.zapper
    }

    /// Port-2 status bits at the current beam position: trigger on bit 4,
    /// light sense on bit 3. Sampled against the PPU's live framebuffer.
    fn zapper_bits(&self) -> u8 {
        let timing = self.ppu.timing();
        let renderer = self.ppu.renderer();
        self.controller
            .zapper
            .read(timing.scanline(), timing.dot(), |x, y| {
                renderer.pixel_brightness(x, y)
            })
    }

    pub fn ppu_timing(&self) -> &Timing {
        self.ppu.timing()
    }

    pub fn ppu(&self) -> &Ppu<R> {
        &self.ppu
    }

    pub fn ppu_mut(&mut self) -> &mut Ppu<R> {
        &mut self.ppu
    }

    pub fn apu(&self) -> &Apu<D> {
        &self.apu
    }

    pub fn apu_mut(&mut self) -> &mut Apu<D> {
        &mut self.apu
    }

    pub fn read_vram(&self, addr: u16) -> u8 {
        self.ppu().read_vram(addr, &self.cartridge)
    }
}

impl<R: Render, D: AudioDriver> Mcu for NesMcu<R, D> {
    fn read(&mut self, address: u16) -> u8 {
        let value = match address {
            0x0000..=0x1fff => self.lower_ram.read(address),
            0x2000..=0x3fff => self.ppu.read_ppureg(address, &mut self.cartridge),
            0x4016 => {
                let prev_joypad1_oe = self.joypad1_oe;
                self.joypad1_oe = true;
                self.joypad2_oe = false;
                let value = self.controller.a.read_strobed(!prev_joypad1_oe);
                return self.latch_read(value, true);
            }
            0x4017 => {
                let prev_joypad2_oe = self.joypad2_oe;
                self.joypad1_oe = false;
                self.joypad2_oe = true;
                let value = self.controller.b.read_strobed(!prev_joypad2_oe) | self.zapper_bits();
                return self.latch_read(value, true);
            }
            0x4015 => self.apu.read(address),
            // PRG-ROM / PRG-RAM from cartridge; unmapped and disabled RAM is open bus
            0x6000..=0x7fff => {
                if !self.cartridge.prg_ram_enabled() {
                    self.open_bus
                } else {
                    self.cartridge.read(address)
                }
            }
            0x8000..=0xffff => self.cartridge.read(address),
            0x4020..=0x5fff => self.open_bus,
            // Write-only APU/IO registers and unused test registers: open bus
            0x4000..=0x401f => self.open_bus,
        };
        self.latch_read(value, false)
    }

    fn peek(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.lower_ram.peek(address),
            0x2000..=0x3fff => self.ppu.peek(address, &self.cartridge),
            0x4015 => self.apu.peek(address),
            0x4016 => self.controller.peek(address),
            0x4017 => self.controller.peek(address) | self.zapper_bits(),
            0x4020..=0x5fff => self.open_bus,
            0x6000..=0x7fff => {
                if !self.cartridge.prg_ram_enabled() {
                    self.open_bus
                } else {
                    self.cartridge.read(address)
                }
            }
            0x8000..=0xffff => self.cartridge.read(address),
            0x4000..=0x401f => self.open_bus,
        }
    }

    fn write(&mut self, address: u16, value: u8, clock: SystemClock) {
        self.latch_write(value);
        match address {
            0x0000..=0x1fff => self.lower_ram.write(address, value, clock),
            0x2000..=0x3fff => self.ppu.write_ppureg(address, value, &mut self.cartridge),
            0x4000..=0x401f => match address {
                0x4014 => self.ppu_dma(value),
                0x4016 => self.controller.write(address, value, clock),
                0x4000 | 0x4003 | 0x4004 | 0x4007 | 0x400B | 0x400F | 0x4013 => {
                    // Production bound is 1 (tick_apu drains on the same
                    // dots cpu.tick runs, one write per cycle max); the
                    // tightest direct-API sequence queues 2. Louder than a
                    // silent `.ok()` drop if that ever stops holding.
                    debug_assert!(self.deferred_apu_writes.len() < 2);
                    self.deferred_apu_writes
                        .push_back((clock.cycles() + 3, address, value))
                        .ok();
                }
                _ => self.apu.write(address, value, clock),
            },
            0x4020..=0x5fff => {}
            0x6000..=0x7fff => {
                if self.cartridge.prg_ram_enabled() {
                    self.cartridge.write(address, value, clock);
                }
            }
            0x8000..=0xffff => self.cartridge.write(address, value, clock),
        }
    }

    fn read_zero_page(&mut self, address: u8) -> u8 {
        let value = self.lower_ram.read_zero_page(address);
        self.latch_read(value, false)
    }

    fn read_stack_page(&mut self, address: u8) -> u8 {
        let value = self.lower_ram.read_stack_page(address);
        self.latch_read(value, false)
    }

    fn write_zero_page(&mut self, address: u8, value: u8, clock: SystemClock) {
        self.latch_write(value);
        self.lower_ram.write_zero_page(address, value, clock);
    }

    fn write_stack_page(&mut self, address: u8, value: u8, clock: SystemClock) {
        self.latch_write(value);
        self.lower_ram.write_stack_page(address, value, clock);
    }
}

#[cfg(test)]
mod tests;
