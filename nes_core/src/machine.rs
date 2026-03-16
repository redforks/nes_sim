use crate::mcu::Mcu;
use crate::nes::nes_apu::NesApu;
use crate::nes::nes_mcu::PpuCartridgeState;
use crate::{Cpu, EmptyPlugin, ExecuteResult, Plugin};
use std::cell::RefCell;
use std::rc::Rc;

/// Safety limit: maximum CPU instruction ticks per `process_frame()` call.
/// Two full frames worth of ticks; prevents an infinite loop if VBlank never fires
/// (e.g. when the PPU is not connected or NES hardware is not present).
const MAX_TICKS_PER_FRAME: u32 = 60000;

pub struct Machine<P> {
    cpu: Cpu,
    p: P,
    /// Shared PPU + cartridge state. Present when NES hardware is attached.
    /// Machine ticks the PPU directly on every CPU cycle.
    ppu_cartridge: Option<Rc<RefCell<PpuCartridgeState>>>,
    /// APU frame counter state. Present when NES hardware is attached.
    /// Machine ticks the APU directly on every CPU cycle.
    apu: Option<Rc<RefCell<NesApu>>>,
}

impl Machine<EmptyPlugin> {
    pub fn new(mcu: Box<dyn Mcu>) -> Self {
        Machine::with_plugin(EmptyPlugin(), mcu)
    }
}

impl<P: Plugin> Machine<P> {
    pub fn with_plugin(p: P, mcu: Box<dyn Mcu>) -> Self {
        let mut machine = Machine {
            cpu: Cpu::new(mcu),
            p,
            ppu_cartridge: None,
            apu: None,
        };
        machine.cpu.reset();
        machine
    }

    /// Attach NES PPU/cartridge state for direct PPU ticking from Machine.
    pub fn attach_ppu_cartridge(&mut self, state: Rc<RefCell<PpuCartridgeState>>) {
        self.ppu_cartridge = Some(state);
    }

    /// Attach NES APU state for direct APU ticking from Machine.
    pub fn attach_apu(&mut self, apu: Rc<RefCell<NesApu>>) {
        self.apu = Some(apu);
    }

    /// Run the machine for a single frame.
    ///
    /// Executes CPU instructions until the PPU enters VBlank (scanline 241, dot 1),
    /// which marks the natural end of a rendered frame. This is the correct approach
    /// because the PPU has finished rendering all 240 visible scanlines at that point.
    ///
    /// If VBlank does not occur within `MAX_TICKS_PER_FRAME` instruction ticks
    /// (safety guard for MCUs without a real PPU), the function returns early.
    ///
    /// A frame is ~29780.5 CPU cycles (262 scanlines × 341 dots / 3 PPU:CPU ratio).
    pub fn process_frame(&mut self) -> ExecuteResult {
        for _ in 0..MAX_TICKS_PER_FRAME {
            match self.tick_once() {
                ExecuteResult::Continue => {}
                other => return other,
            }
            if self.take_vblank() {
                return ExecuteResult::Continue;
            }
        }
        ExecuteResult::Continue
    }

    pub fn run_ticks(&mut self, ticks: u32) -> ExecuteResult {
        for _ in 0..ticks {
            match self.tick_once() {
                ExecuteResult::Continue => continue,
                other => return other,
            }
        }
        ExecuteResult::Continue
    }

    /// Execute one CPU instruction and tick PPU/APU for the appropriate cycles.
    ///
    /// When NES hardware is attached (`ppu_cartridge` / `apu` are `Some`), PPU
    /// and APU are ticked directly by Machine instead of through the `Mcu` trait.
    /// When no NES hardware is attached, the `Mcu` trait default (no-op) methods
    /// are used for backwards compatibility.
    fn tick_once(&mut self) -> ExecuteResult {
        if let (Some(ppu_cart), Some(apu)) = (&self.ppu_cartridge, &self.apu) {
            // NES mode: tick PPU and APU directly (not via Mcu trait).
            // Clone Rc to avoid borrow checker conflict with self.cpu mutations.
            let ppu_cart = Rc::clone(ppu_cart);
            let apu = Rc::clone(apu);
            self.tick_nes(&ppu_cart, &apu)
        } else {
            // Generic mode: delegate to Mcu trait methods (default no-ops).
            self.cpu.tick(&mut self.p)
        }
    }

    /// Tick one CPU instruction with direct PPU/APU access (NES mode).
    ///
    /// `Cpu::tick()` internally calls `mcu.tick_ppu()` and `mcu.tick_apu()`, but
    /// `MappingMcu` provides no-op implementations for both. We therefore tick the
    /// PPU and APU directly here, after the instruction executes.
    ///
    /// Cycle accuracy note: `cpu.tick()` returns `ExecuteResult`, not a cycle count.
    /// We approximate by ticking 3 PPU dots and 1 APU step per instruction call.
    /// This is sufficient for correct vblank detection (checked after every instruction),
    /// though not cycle-accurate for audio timing. A future improvement would be to
    /// expose cycle count from `Cpu::tick` via a cycle-counting `Plugin`.
    fn tick_nes(
        &mut self,
        ppu_cart: &Rc<RefCell<PpuCartridgeState>>,
        apu: &Rc<RefCell<NesApu>>,
    ) -> ExecuteResult {
        if self.cpu.is_halted() {
            // Tick PPU/APU even when CPU is halted
            for _ in 0..3 {
                if ppu_cart.borrow_mut().tick_ppu() {
                    self.cpu.nmi();
                }
            }
            if apu.borrow_mut().tick() {
                self.cpu.set_irq(true);
            }
            return ExecuteResult::Continue;
        }

        // Execute the instruction. MappingMcu's tick_ppu/tick_apu are no-ops,
        // so there are no PPU/APU side effects inside cpu.tick().
        let result = self.cpu.tick(&mut self.p);

        // Tick PPU (3 dots) and APU (1 step) to approximate one CPU cycle worth
        // of PPU/APU progress. IRQ is forwarded to the CPU as needed.
        for _ in 0..3 {
            if ppu_cart.borrow_mut().tick_ppu() {
                self.cpu.nmi();
            }
        }
        if apu.borrow_mut().tick() {
            self.cpu.set_irq(true);
        }
        self.cpu.set_irq(apu.borrow().request_irq());

        result
    }

    /// Check and clear the vblank flag.
    /// Returns true if VBlank started since the last call.
    fn take_vblank(&mut self) -> bool {
        if let Some(ppu_cart) = &self.ppu_cartridge {
            ppu_cart.borrow_mut().take_vblank()
        } else {
            self.cpu.mcu().take_vblank()
        }
    }

    pub fn reset(&mut self) {
        self.cpu.reset()
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.cpu.pc = pc;
    }
}

#[cfg(test)]
mod tests;
