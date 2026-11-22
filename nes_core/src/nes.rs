use crate::SystemClock;
use crate::ines::INesFile;
use crate::mcu::Mcu;
use crate::nes::{
    apu::{Apu, ApuState},
    controller::{Button, Controller, ControllerState, Zapper},
    lower_ram::LowerRam,
    ppu::{Ppu, PpuState, Timing},
};
use arraydeque::ArrayDeque;
pub mod apu;
pub mod controller;
pub(crate) mod dmc_dma;
mod lower_ram;
pub(crate) mod mapper;
pub use mapper::Mmc3IrqOverride;
pub mod ppu;

pub struct NesMcu {
    lower_ram: LowerRam,
    ppu: Ppu,
    cartridge: mapper::Cartridge,
    controller: Controller,
    apu: Apu,
    oam_dma_pending: Option<u8>,
    /// CPU data bus open bus value: the last value read by the CPU.
    /// Reading write-only or unmapped addresses returns this value.
    open_bus: u8,
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

/// Full [`NesMcu`] state for save-states (ticket #184) in core types.
/// The cartridge is captured separately via its own snapshot API; the
/// framebuffer-adjacent PPU outputs stay excluded (see [`PpuState`]).
#[derive(Debug, Clone)]
pub(crate) struct McuState {
    pub ram: [u8; 0x800],
    pub ppu: PpuState,
    pub apu: ApuState,
    pub controller: ControllerState,
    pub oam_dma_pending: Option<u8>,
    pub open_bus: u8,
    /// Pending length-counter register writes, oldest first (max 2).
    pub deferred_apu_writes: Vec<(u64, u16, u8)>,
}

impl NesMcu {
    /// Single open-bus latch routine shared by the read, peek, and write
    /// paths: update the open-bus latch (and deassert both joypad /OE lines
    /// unless a controller read keeps them asserted). `pub(crate)` so the
    /// seam stays narrow (ADR-0010).
    pub(crate) fn latch(&mut self, value: u8, read_controller: bool) -> u8 {
        self.open_bus = value;
        if !read_controller {
            self.controller.a.oe = false;
            self.controller.b.oe = false;
        }
        value
    }
    pub fn new(file: &INesFile) -> Self {
        Self::new_with_mmc3_irq_override(file, Mmc3IrqOverride::Auto)
    }

    /// [`Self::new`] with an explicit MMC3 IRQ-revision override — the
    /// harness/test hook (step 1) of [`mmc3_irq_revision_is_alternate`].
    /// [`Mmc3IrqOverride::Auto`] uses [`mapper::create_cartridge_with_mmc3_irq_override`]
    /// auto-detection.
    pub(crate) fn new_with_mmc3_irq_override(
        file: &INesFile,
        irq_override: Mmc3IrqOverride,
    ) -> Self {
        let cartridge = mapper::create_cartridge_with_mmc3_irq_override(file, irq_override);
        let ppu = Ppu::new();

        Self {
            lower_ram: LowerRam::new(),
            ppu,
            cartridge,
            controller: Controller::new(),
            apu: Apu::new(),
            oam_dma_pending: None,
            open_bus: 0,
            deferred_apu_writes: ArrayDeque::new(),
        }
    }

    /// Soft reset (front-panel RESET button): PPU registers re-latch and
    /// the APU falls silent while RAM, the cartridge, controllers, and the
    /// open-bus latch survive — execution restarts, state persists. The
    /// DMA bus is owned by `DmaBus` and reset separately; by the time this
    /// runs, DMA work must already be quiesced (see [`crate::NesMachine::soft_reset`]),
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

    /// Hard reset (power cycle): the full power-up contract. The cartridge
    /// is rebuilt from the ROM (mapper state never survives a power cycle
    /// on hardware), RAM/PPU/APU return to power-up contents, and the bus
    /// latch clears. Held controller buttons stay held — they are physical.
    ///
    /// Unlike soft reset this aborts DMA instead of draining it: power loss
    /// does not wait for in-flight transfers, so the caller resets the
    /// `DmaBus` without ticking. OAM pending is cleared, not asserted on.
    pub fn reset_hard(&mut self, file: &INesFile, irq_override: Mmc3IrqOverride) {
        self.cartridge = mapper::create_cartridge_with_mmc3_irq_override(file, irq_override);
        self.lower_ram.reset_hard();
        self.ppu.reset_hard();
        self.apu.reset_hard();
        self.controller.reset_hard();
        self.oam_dma_pending = None;
        self.deferred_apu_writes.clear();
        self.open_bus = 0;
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

    #[hotpath::measure(impl_type = "NesMcu")]
    pub fn tick_apu(&mut self, clock: SystemClock) {
        // Expansion audio (MMC5 `$5000-$5015`) advances on the same CPU
        // cycle and its levels reach the APU mixer before the tick emits:
        // silence (zeros) for boards without expansion audio.
        if self.cartridge.want_tick_audio() {
            let expansion = self.cartridge.tick_audio();
            self.apu.set_expansion_mix(expansion);
        }
        self.apu.tick();
        if let Some(&(at, address, value)) = self.deferred_apu_writes.front()
            && at <= clock.cycles()
        {
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

    pub fn clear_audio_buffer(&mut self) {
        self.apu.clear_audio_buffer();
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
    /// light sense on bit 3. Sampled against pixels already buffered
    /// this frame (ticket #64): the PPU resolves the buffered palette
    /// index live, so light is detectable mid-frame instead of waiting
    /// for the vblank flush to materialize the framebuffer.
    fn zapper_bits(&self) -> u8 {
        let timing = self.ppu.timing();
        self.controller
            .zapper
            .read(timing.scanline(), timing.dot(), |x, y| {
                self.ppu.pixel_brightness(x, y)
            })
    }

    pub fn ppu_timing(&self) -> &Timing {
        self.ppu.timing()
    }

    pub fn ppu(&self) -> &Ppu {
        &self.ppu
    }

    pub fn ppu_mut(&mut self) -> &mut Ppu {
        &mut self.ppu
    }

    /// Cartridge access for the save-state DTO (ticket #192): snapshot
    /// capture on save, mapper/memory restore on load.
    pub(crate) fn cartridge(&self) -> &mapper::Cartridge {
        &self.cartridge
    }

    /// Cartridge access for the save-state DTO (ticket #192).
    pub(crate) fn cartridge_mut(&mut self) -> &mut mapper::Cartridge {
        &mut self.cartridge
    }

    /// Capture the full MCU state for save-states (ticket #184): RAM,
    /// PPU, APU, controller sampling, and the bus latches/queues. The
    /// cartridge is captured separately via its own snapshot API.
    pub(crate) fn capture_mcu_state(&self) -> McuState {
        McuState {
            ram: self.lower_ram.snapshot_ram(),
            ppu: self.ppu.capture_ppu_state(),
            apu: self.apu.capture_apu_state(),
            controller: self.controller.capture_controller_state(),
            oam_dma_pending: self.oam_dma_pending,
            open_bus: self.open_bus,
            deferred_apu_writes: self.deferred_apu_writes.iter().copied().collect(),
        }
    }

    /// Restore a state previously produced by [`Self::capture_mcu_state`].
    pub(crate) fn restore_mcu_state(&mut self, s: &McuState) {
        self.lower_ram.restore_ram(&s.ram);
        self.ppu.restore_ppu_state(&s.ppu);
        self.apu.restore_apu_state(&s.apu);
        self.controller.restore_controller_state(&s.controller);
        self.oam_dma_pending = s.oam_dma_pending;
        self.open_bus = s.open_bus;
        self.deferred_apu_writes.clear();
        for w in &s.deferred_apu_writes {
            let _ = self.deferred_apu_writes.push_back(*w);
        }
    }

    pub fn apu(&self) -> &Apu {
        &self.apu
    }

    pub fn apu_mut(&mut self) -> &mut Apu {
        &mut self.apu
    }

    pub fn read_vram(&self, addr: u16) -> u8 {
        self.ppu().read_vram(addr, &self.cartridge)
    }
}

impl Mcu for NesMcu {
    fn read(&mut self, address: u16) -> u8 {
        let value = match address {
            // PRG-ROM / PRG-RAM from cartridge; unmapped and disabled RAM is open bus
            0x8000..=0xffff => {
                let value = self.cartridge.read(address);
                // PCM stream read mode: a `$8000-$BFFF` read feeds the MMC5
                // DAC (caps-gated, so other boards pay one branch).
                self.cartridge.notify_prg_read(address, value);
                value
            }
            0x0000..=0x1fff => self.lower_ram.read(address),
            0x2000..=0x3fff => self.ppu.read_ppureg(address, &mut self.cartridge),
            0x6000..=0x7fff => {
                if !self.cartridge.prg_ram_enabled() {
                    self.open_bus
                } else {
                    self.cartridge.read_prg_ram(address)
                }
            }
            0x4016 => {
                let value = self.controller.a.read_strobed();
                self.controller.a.oe = true;
                self.controller.b.oe = false;
                return self.latch(value, true);
            }
            0x4017 => {
                let value = self.controller.b.read_strobed() | self.zapper_bits();
                self.controller.a.oe = false;
                self.controller.b.oe = true;
                return self.latch(value, true);
            }
            0x4015 => self.apu.read(address),
            // Cartridge expansion area (MMC5 `$5010`/`$5015` audio and
            // `$5204` status): the chip answers handled registers itself;
            // everything else is open bus. Full-byte registers (audio
            // status, ExRAM, multiplier) latch the chip's byte directly.
            0x5000..=0x5fff => match self.cartridge.read_expansion(address) {
                Some(value) => {
                    let value = if address == 0x5204 {
                        expansion_status_5204(value, self.open_bus)
                    } else {
                        value
                    };
                    self.latch(value, false)
                }
                None => self.latch(self.open_bus, false),
            },
            // Write-only APU/IO registers and unused test registers: open bus
            _ => self.open_bus,
        };
        self.latch(value, false)
    }

    fn peek(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.lower_ram.peek(address),
            0x2000..=0x3fff => self.ppu.peek(address, &self.cartridge),
            0x4015 => self.apu.peek(address),
            0x4016 => self.controller.peek(address),
            0x4017 => self.controller.peek(address) | self.zapper_bits(),
            0x6000..=0x7fff => {
                if !self.cartridge.prg_ram_enabled() {
                    self.open_bus
                } else {
                    self.cartridge.read_prg_ram(address)
                }
            }
            0x8000..=0xffff => self.cartridge.read(address),
            // Expansion-area peek: never acknowledges, so inspecting `$5204`
            // or `$5010` cannot clear a pending IRQ.
            0x5000..=0x5fff => match self.cartridge.peek_expansion(address) {
                Some(value) => {
                    if address == 0x5204 {
                        expansion_status_5204(value, self.open_bus)
                    } else {
                        value
                    }
                }
                None => self.open_bus,
            },
            _ => self.open_bus,
        }
    }

    fn write(&mut self, address: u16, value: u8, clock: SystemClock) {
        self.latch(value, false);
        match address {
            0x0000..=0x1fff => self.lower_ram.write(address, value, clock),
            0x2000..=0x3fff => self.ppu.write_ppureg(address, value, &mut self.cartridge),
            0x4000..=0x401f => match address {
                0x4014 => self.ppu_dma(value),
                0x4016 => self.controller.write(value),
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
            0x6000..=0x7fff => {
                if self.cartridge.prg_ram_enabled() {
                    self.cartridge.write_prg_ram(address, value, clock);
                }
            }
            // Cartridge expansion area (MMC5 registers, Action 53 register
            // select): forwards to the mapper chip; chips without expansion
            // registers ignore it.
            0x5000..=0x5fff => self.cartridge.write_expansion(address, value, clock),
            0x8000..=0xffff => self.cartridge.write(address, value, clock),
            _ => {}
        }
    }

    fn read_zero_page(&mut self, address: u8) -> u8 {
        let value = self.lower_ram.read_zero_page(address);
        self.latch(value, false)
    }

    fn read_stack_page(&mut self, address: u8) -> u8 {
        let value = self.lower_ram.read_stack_page(address);
        self.latch(value, false)
    }

    fn write_zero_page(&mut self, address: u8, value: u8, clock: SystemClock) {
        self.latch(value, false);
        self.lower_ram.write_zero_page(address, value, clock);
    }

    fn write_stack_page(&mut self, address: u8, value: u8, clock: SystemClock) {
        self.latch(value, false);
        self.lower_ram.write_stack_page(address, value, clock);
    }
}

/// `$5204` carries status in bits 7-6 only; its low bits read the bus latch
/// (same convention as the `$2002` low-bits combine). Shared by the `Mcu`
/// read and peek paths above: peeking never acknowledges.
fn expansion_status_5204(value: u8, open_bus: u8) -> u8 {
    (value & 0xC0) | (open_bus & 0x3F)
}

#[cfg(test)]
mod tests;
