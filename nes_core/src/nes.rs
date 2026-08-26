use crate::SystemClock;
use crate::ines::INesFile;
use crate::mcu::Mcu;
use crate::nes::apu::{Apu, AudioDriver};
use crate::nes::controller::{Button, Controller, Zapper};
use crate::nes::lower_ram::LowerRam;
use crate::nes::ppu::{Ppu, Timing};
use crate::render::Render;

pub mod apu;
pub mod controller;
pub(crate) mod dmc_dma;
mod lower_ram;
mod mapper;
pub mod ppu;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct OamDmaState {
    page: u8,
    startup_cycles: usize,
    transfer_cycle: usize,
    latch: u8,
    /// Remaining pause cycles inserted after a DMC DMA collision: one OAM
    /// alignment cycle before the aborted read is redone.
    pause_cycles: usize,
}

pub struct NesMcu<R: Render, D: AudioDriver> {
    lower_ram: LowerRam,
    ppu: Ppu<R>,
    controller: Controller,
    apu: Apu<D>,
    oam_dma_pending: Option<u8>,
    oam_dma: Option<OamDmaState>,
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
}

impl<R: Render, D: AudioDriver> NesMcu<R, D> {
    pub fn new(file: &INesFile, renderer: R, audio_driver: D) -> Self {
        let (cartridge, mirroring) = mapper::create_cartridge(file);
        let ppu = Ppu::new(renderer, mirroring, cartridge);

        Self {
            lower_ram: LowerRam::new(),
            ppu,
            controller: Controller::new(),
            apu: Apu::new(audio_driver),
            oam_dma_pending: None,
            oam_dma: None,
            open_bus: 0,
            joypad1_oe: false,
            joypad2_oe: false,
            deferred_apu_writes: Vec::new(),
            last_apu_tick: 0,
        }
    }

    pub fn reset(&mut self) {
        self.deferred_apu_writes.clear();
        self.last_apu_tick = 0;
        self.ppu.reset();
        self.apu.reset();
    }

    fn ppu_dma(&mut self, address: u8) {
        self.oam_dma_pending = Some(address);
    }

    pub fn tick_ppu(&mut self) {
        self.ppu.tick();
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
        self.ppu.cartridge_irq_pending()
    }

    pub fn flush_audio(&mut self) {
        self.apu.flush();
    }

    pub fn tick_oam_dma(&mut self, clock: SystemClock, dmc_drove_bus: bool) -> bool {
        if let Some(mut dma) = self.oam_dma {
            self.oam_dma = None;
            if dma.pause_cycles > 0 {
                // OAM alignment cycle after a DMC DMA collision: no transfer.
                dma.pause_cycles -= 1;
            } else if dmc_drove_bus
                && dma.startup_cycles == 0
                && dma.transfer_cycle.is_multiple_of(2)
            {
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
                    dma.latch = self.read(addr);
                } else {
                    self.ppu.write_oam_data(dma.latch);
                }
                dma.transfer_cycle += 1;
            }

            if dma.startup_cycles == 0 && dma.transfer_cycle == 512 {
                return true;
            }

            self.oam_dma = Some(dma);
            return true;
        }

        if let Some(page) = self.oam_dma_pending {
            self.oam_dma_pending = None;
            // The pending request is consumed on the first tick after the
            // $4014 write cycle; that tick is the DMA halt cycle. If it is a
            // get cycle the write happened on a put cycle and one alignment
            // cycle is needed before the first read; otherwise none.
            let startup_cycles = if clock.is_apu_get_clock() { 1 } else { 0 };
            self.oam_dma = Some(OamDmaState {
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
        self.ppu().read_vram(addr)
    }
}

impl<R: Render, D: AudioDriver> Mcu for NesMcu<R, D> {
    fn read(&mut self, address: u16) -> u8 {
        let prev_joypad1_oe = std::mem::replace(&mut self.joypad1_oe, address == 0x4016);
        let prev_joypad2_oe = std::mem::replace(&mut self.joypad2_oe, address == 0x4017);
        let value = match address {
            0x0000..=0x1fff => self.lower_ram.read(address),
            0x2000..=0x3fff | 0x4100..=0xffff => self.ppu.read(address),
            0x4016 => self.controller.a.read_strobed(!prev_joypad1_oe),
            0x4017 => self.controller.b.read_strobed(!prev_joypad2_oe) | self.zapper_bits(),
            0x4015 => self.apu.read(address),
            // Write-only APU/IO registers and unused test registers: open bus
            0x4000..=0x401f => self.open_bus,
            // Unallocated I/O space: open bus
            0x4020..=0x40ff => self.open_bus,
        };
        self.open_bus = value;
        value
    }

    fn peek(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.lower_ram.peek(address),
            0x2000..=0x3fff => self.ppu.peek(address),
            0x4015 => self.apu.peek(address),
            0x4016 => self.controller.peek(address),
            0x4017 => self.controller.peek(address) | self.zapper_bits(),
            0x4000..=0x401f => self.open_bus,
            0x4020..=0x40ff => self.open_bus,
            0x4100..=0xffff => self.ppu.peek(address),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        self.open_bus = value;
        self.joypad1_oe = false;
        self.joypad2_oe = false;
        match address {
            0x0000..=0x1fff => self.lower_ram.write(address, value),
            0x2000..=0x3fff | 0x4100..=0xffff => self.ppu.write(address, value),
            0x4000..=0x401f => match address {
                0x4014 => self.ppu_dma(value),
                0x4016 => self.controller.write(address, value),
                0x4000 | 0x4003 | 0x4004 | 0x4007 | 0x400B | 0x400F | 0x4013 => {
                    self.deferred_apu_writes
                        .push((self.last_apu_tick + 3, address, value));
                }
                _ => self.apu.write(address, value),
            },
            // Unallocated I/O space: writes are ignored
            0x4020..=0x40ff => {}
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
