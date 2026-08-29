use crate::SystemClock;
use crate::cpu::CpuSnapshot;
use crate::mcu::Mcu;

/// Read-model snapshot exposed to plugins/tools — the per-dot view (ADR-0010).
/// Replaces direct `&Cpu<M>` + `cpu.mcu().ppu()` walks. Holds an owned
/// CPU snapshot plus a shared borrow of the address space for `peek`/`read_vram` etc.
/// `M` is the underlying `Mcu` (e.g. `MockMcu` or `NesMcu<R,D>`).
pub struct MachineView<'a, M: Mcu> {
    pub cpu: CpuSnapshot,
    mcu: &'a M,
    pub clock: SystemClock,
}

impl<'a, M: Mcu> MachineView<'a, M> {
    pub fn new(cpu_snapshot: CpuSnapshot, mcu: &'a M, clock: SystemClock) -> Self {
        Self {
            cpu: cpu_snapshot,
            mcu,
            clock,
        }
    }

    /// Borrow the CPU snapshot (registers + PC).
    pub fn cpu(&self) -> &CpuSnapshot {
        &self.cpu
    }

    /// Peek memory at `addr` via the underlying `Mcu::peek` (read-only).
    pub fn peek(&self, addr: u16) -> u8 {
        self.mcu.peek(addr)
    }

    /// Clock for this dot.
    pub fn clock(&self) -> SystemClock {
        self.clock
    }

    /// Whether CPU is halted (executed KIL / invalid opcode).
    pub fn is_halted(&self) -> bool {
        self.cpu.halt
    }
}

// NesMcu-specific extensions — only available when `M = NesMcu<R,D>`.
// Placed here rather than in `nes.rs` to keep the view seam in one module.
impl<'a, R, D> MachineView<'a, crate::nes::NesMcu<R, D>>
where
    R: crate::render::Render,
    D: crate::nes::apu::AudioDriver,
{
    pub fn ppu_in_vblank(&self) -> bool {
        self.mcu.ppu().in_vblank()
    }
    pub fn ppu_rendering_enabled(&self) -> bool {
        self.mcu.ppu().rendering_enabled()
    }
    pub fn ppu_frame_no(&self) -> usize {
        self.mcu.ppu().timing().frame_no()
    }
    pub fn read_vram(&self, addr: u16) -> u8 {
        self.mcu.read_vram(addr)
    }
    pub fn in_vblank(&self) -> bool {
        self.ppu_in_vblank()
    } // alias for older code
    pub fn ppu_timing(&self) -> &crate::nes::ppu::Timing {
        self.mcu.ppu().timing()
    }
}

impl<'a, const N: usize, D> MachineView<'a, crate::nes::NesMcu<crate::render::ImageRender<N>, D>>
where
    D: crate::nes::apu::AudioDriver,
{
    pub fn borrow_image_bytes(&self) -> &[u8] {
        self.mcu.ppu().renderer().as_bytes()
    }

    /// Typed RGBA pixels (`width*height` entries of `[R,G,B,A]`).
    pub fn borrow_pixels(&self) -> &[[u8; 4]] {
        self.mcu.ppu().renderer().as_pixels()
    }

    /// Output width (`256*N`).
    pub fn image_width(&self) -> u32 {
        crate::render::ImageRender::<N>::width()
    }

    /// Output height (`240*N`).
    pub fn image_height(&self) -> u32 {
        crate::render::ImageRender::<N>::height()
    }

    /// Compatibility alias — returns typed pixels (was `&RgbaImage`).
    pub fn borrow_image(&self) -> &[[u8; 4]] {
        self.borrow_pixels()
    }
}
