use crate::SystemClock;
use crate::ines::INesFile;
use crate::mcu::Mcu;
use crate::nes::{
    apu::{Apu, AudioDriver},
    controller::{Button, Controller, Zapper},
    lower_ram::LowerRam,
    mapper::CartridgeOperation,
    ppu::{Ppu, Timing},
};
use crate::render::Render;
pub mod apu;
pub mod controller;
pub(crate) mod dmc_dma;
mod lower_ram;
mod mapper;
pub mod ppu;

pub struct NesMcu<R: Render, D: AudioDriver> {
    lower_ram: LowerRam,
    ppu: Ppu<R>,
    cartridge: Box<dyn mapper::Cartridge>,
    cartridge_caps: mapper::CartridgeCaps,
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
    deferred_apu_writes: Vec<(u64, u16, u8)>,
    /// Most recent system tick seen by [`Self::tick_apu`].
    last_apu_tick: u64,
    /// Current system clock sampled at the top of `NesMachine::tick`.
    /// Used to timestamp `Cartridge::write` for MMC1's consecutive-cycle
    /// filter (1 CPU cycle = 3 PPU cycles).
    current_clock: SystemClock,
}

impl<R: Render, D: AudioDriver> NesMcu<R, D> {
    pub fn new(file: &INesFile, renderer: R, audio_driver: D) -> Self {
        let (cartridge, mirroring) = mapper::create_cartridge(file);
        let cartridge_caps = cartridge.ppu_capabilities();
        let ppu = Ppu::new(renderer, mirroring);

        Self {
            lower_ram: LowerRam::new(),
            ppu,
            cartridge,
            cartridge_caps,
            controller: Controller::new(),
            apu: Apu::new(audio_driver),
            oam_dma_pending: None,
            open_bus: 0,
            joypad1_oe: false,
            joypad2_oe: false,
            deferred_apu_writes: Vec::new(),
            last_apu_tick: 0,
            current_clock: SystemClock::default(),
        }
    }

    /// Reset all bus devices to their power-up contract. The DMA bus is now
    /// owned by [`crate::bus::Bus`] and reset separately; by the time this
    /// runs, DMA work must already be quiesced (see [`crate::NesMachine::reset`]),
    /// so OAM pending should be idle here. The bus owns the active OAM state.
    ///
    /// The master clock keeps running across resets, so time-relative state
    /// (deferred APU write landing points) must be re-anchored from `clock`
    /// instead of restarting at zero.
    pub fn reset(&mut self, clock: SystemClock) {
        debug_assert!(self.oam_dma_pending.is_none());
        self.last_apu_tick = clock.cycles();
        self.deferred_apu_writes.clear();
        self.ppu.reset();
        self.apu.reset();
    }

    /// True while an OAM DMA request is still queued in the producer.
    /// The active transfer lives in [`crate::bus::Bus`]; use
    /// `Bus::is_busy(mcu)` for the full quiescence predicate.
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
        let caps = self.cartridge_caps;
        let cartridge = &mut *self.cartridge;
        self.ppu.tick(cartridge, caps);
    }

    pub(crate) fn set_clock(&mut self, clock: SystemClock) {
        self.current_clock = clock;
    }
    pub fn tick_apu(&mut self, clock: SystemClock) {
        self.last_apu_tick = clock.cycles();
        self.apu.tick(clock);
        while self
            .deferred_apu_writes
            .first()
            .is_some_and(|(at, _, _)| *at <= clock.cycles())
        {
            let (_, address, value) = self.deferred_apu_writes.remove(0);
            self.apu.write(address, value);
        }
    }

    pub fn apu_irq_pending(&self) -> bool {
        self.apu.request_irq()
    }

    pub fn cartridge_irq_pending(&self) -> bool {
        if !self.cartridge_caps.irq_pending {
            return false;
        }
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
        self.ppu().read_vram(addr, &*self.cartridge)
    }
}

impl<R: Render, D: AudioDriver> Mcu for NesMcu<R, D> {
    fn read(&mut self, address: u16) -> u8 {
        let prev_joypad1_oe = std::mem::replace(&mut self.joypad1_oe, address == 0x4016);
        let prev_joypad2_oe = std::mem::replace(&mut self.joypad2_oe, address == 0x4017);
        let caps = self.cartridge_caps;
        let value = match address {
            0x0000..=0x1fff => self.lower_ram.read(address),
            0x2000..=0x3fff => self.ppu.read_ppureg(address, &mut *self.cartridge, caps),
            0x4016 => self.controller.a.read_strobed(!prev_joypad1_oe),
            0x4017 => self.controller.b.read_strobed(!prev_joypad2_oe) | self.zapper_bits(),
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
        self.open_bus = value;
        value
    }

    fn peek(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.lower_ram.peek(address),
            0x2000..=0x3fff => self.ppu.peek(address, &*self.cartridge),
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

    fn write(&mut self, address: u16, value: u8) {
        self.open_bus = value;
        self.joypad1_oe = false;
        self.joypad2_oe = false;
        match address {
            0x0000..=0x1fff => self.lower_ram.write(address, value),
            0x2000..=0x3fff => {
                let caps = self.cartridge_caps;
                let cart = &mut *self.cartridge;
                self.ppu.write_ppureg(address, value, cart, caps)
            }
            0x4000..=0x401f => match address {
                0x4014 => self.ppu_dma(value),
                0x4016 => self.controller.write(address, value),
                0x4000 | 0x4003 | 0x4004 | 0x4007 | 0x400B | 0x400F | 0x4013 => {
                    self.deferred_apu_writes
                        .push((self.last_apu_tick + 3, address, value));
                }
                _ => self.apu.write(address, value),
            },
            0x4020..=0x5fff => {}
            0x6000..=0x7fff => {
                if self.cartridge.prg_ram_enabled()
                    && let CartridgeOperation::UpdateNametableMirroring(mirroring) =
                        self.cartridge.write(address, value, self.current_clock)
                {
                    self.ppu.set_mirroring(mirroring);
                }
            }
            0x8000..=0xffff => {
                if let CartridgeOperation::UpdateNametableMirroring(mirroring) =
                    self.cartridge.write(address, value, self.current_clock)
                {
                    self.ppu.set_mirroring(mirroring);
                }
            }
        }
    }

    fn read_zero_page(&mut self, address: u8) -> u8 {
        self.lower_ram.read(address as u16)
    }

    fn read_stack_page(&mut self, address: u8) -> u8 {
        self.lower_ram.read(0x100 + address as u16)
    }

    fn write_zero_page(&mut self, address: u8, value: u8) {
        self.lower_ram.write(address as u16, value);
    }

    fn write_stack_page(&mut self, address: u8, value: u8) {
        self.lower_ram.write(0x100 + address as u16, value);
    }
}

#[cfg(test)]
mod tests;
