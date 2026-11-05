use crate::bus::Bus;
use crate::mcu::Mcu;
use crate::{
    Cpu, SystemClock,
    nes::{NesMcu, apu::AudioDriver},
    render::Render,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DmcDmaType {
    // Load DMAs occur after $4015 D4 is set, but only if the sample buffer is
    // empty. They are scheduled to halt the CPU on a get cycle during the 2nd
    // APU cycle after the write (that is, the 3rd or 4th CPU cycle).
    Load,
    // Reload DMAs occur in response to the sample buffer being emptied. Unlike
    // load DMAs, they are scheduled to halt the CPU on a put cycle.
    Reload,
}

/// Memory-side seam for DMC DMA: the bus-facing endpoints DMA touches
/// outside the CPU — decoded memory reads and the APU DMC request/supply
/// pair. The CPU-side ops (halt projection, freeze/unfreeze) stay direct
/// `Cpu` calls, not part of this seam: the CPU owns the halt state, the
/// memory lives on the [`Bus`](crate::bus::Bus) (ADR-0015).
#[cfg_attr(test, mockall::automock)]
pub(crate) trait DmcDmaMem {
    /// Read memory for dma
    ///
    /// If no memory activity in this cycle, then cpu will repeat last read operation
    fn read_mem(&mut self, addr: u16) -> u8;

    /// Take dmc dma request from apu, None if no current request
    fn take_dmc_dma_request(&mut self) -> Option<(DmcDmaType, u16)>;

    /// Supply dmc dma result to apu
    fn supply_dmc_byte(&mut self, byte: u8);
}

impl<R: Render, D: AudioDriver> DmcDmaMem for NesMcu<R, D> {
    fn read_mem(&mut self, addr: u16) -> u8 {
        self.read(addr)
    }

    fn take_dmc_dma_request(&mut self) -> Option<(DmcDmaType, u16)> {
        self.apu_mut().take_dmc_dma_request()
    }

    fn supply_dmc_byte(&mut self, byte: u8) {
        self.apu_mut().supply_dmc_byte(byte);
    }
}

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
enum State {
    #[default]
    Inactive,
    // If dmc started because `DmcDmaType::Load`, should delay a apu cycle (two cpu cycles), then try to halt
    DelayForLoad(u8),
    TryHalt {
        first_attempt: bool,
        halt_on_put: bool,
    },
    AlignOrRead,
    Dummy,
    Read,
    // DMA read cycle done, CPU still halted; unfreeze on the next cycle so
    // the CPU resumes execution only after the DMA read cycle
    Unfreeze,
}

/// Dma controller for apu dmc channel
#[derive(Debug, Default)]
pub struct DmcDma {
    /// true after `.request()` method, and before dma finished
    state: State,
    addr: u16,
    /// Address the halted CPU repeats on the bus during the halt/dummy
    /// cycles: the pending read's address, or the next fetch address when
    /// the halt landed on an internal cycle.
    halt_bus_addr: u16,
}

impl DmcDma {
    /// Cancel all DMA work: the next `tick` is a no-op. Callers dropping
    /// pending CPU activity (machine reset) must clear this state, or a
    /// mid-transfer machine would resume post-reset with a stale address.
    pub fn reset(&mut self) {
        *self = DmcDma::default();
    }

    /// True while a transfer is in flight (or waiting to halt the bus).
    /// When the CPU is frozen by DMA-halt semantics this stays true until
    /// the read cycle completes and the CPU is unfrozen.
    pub fn is_busy(&self) -> bool {
        !matches!(self.state, State::Inactive)
    }
}
impl DmcDma {
    fn read(&mut self, mem: &mut impl DmcDmaMem) {
        let byte = mem.read_mem(self.addr);
        mem.supply_dmc_byte(byte);
        self.state = State::Unfreeze;
    }

    fn dummy_read(&self, mem: &mut impl DmcDmaMem) {
        mem.read_mem(self.halt_bus_addr);
    }

    /// Advance the DMA state machine by one tick. The current dot (put/get
    /// phase decisions) is read off the bus's master clock (ADR-0018).
    /// Returns true if the DMA performed its memory read this cycle (it
    /// drove the bus).
    pub fn tick<M: Mcu + DmcDmaMem>(&mut self, cpu: &mut Cpu, bus: &mut Bus<M>) -> bool {
        let clock = bus.clock();
        self.tick_mem(cpu, bus.mcu_mut(), clock)
    }

    fn tick_mem(&mut self, cpu: &mut Cpu, mem: &mut impl DmcDmaMem, clock: SystemClock) -> bool {
        match self.state {
            State::Inactive => {
                if let Some((dmc_dma_type, addr)) = mem.take_dmc_dma_request() {
                    self.addr = addr;
                    if dmc_dma_type == DmcDmaType::Load {
                        self.state = State::DelayForLoad(1);
                    } else {
                        self.state = State::TryHalt {
                            halt_on_put: true,
                            first_attempt: true,
                        };
                        // reload requests, no request cycle
                        self.tick_mem(cpu, mem, clock);
                    };
                }
            }
            State::DelayForLoad(mut n) => {
                n -= 1;
                self.state = if n == 0 {
                    State::TryHalt {
                        halt_on_put: false,
                        first_attempt: true,
                    }
                } else {
                    State::DelayForLoad(n)
                };
            }
            State::TryHalt {
                halt_on_put,
                first_attempt,
            } => {
                // 如果是第一次尝试，必须严格对齐目标相位
                if first_attempt && halt_on_put == clock.is_apu_get_clock() {
                    // 相位不对，CPU 照常运行，不消耗 DMA 周期
                    return false;
                }

                if cpu.try_freeze() {
                    // The halt cycle repeats the pending read on the bus;
                    // later no-op DMA cycles keep repeating that address.
                    match cpu.dma_halt_bus_addr() {
                        Some(addr) => {
                            mem.read_mem(addr);
                            self.halt_bus_addr = addr;
                        }
                        None => {
                            self.halt_bus_addr = cpu.pc();
                        }
                    }
                    self.state = State::Dummy;
                } else {
                    self.state = State::TryHalt {
                        first_attempt: false,
                        halt_on_put,
                    };
                }
            }
            State::AlignOrRead => {
                if clock.is_apu_get_clock() {
                    self.read(mem);
                    return true;
                } else {
                    self.state = State::Read;
                    self.dummy_read(mem);
                }
            }
            State::Dummy => {
                self.state = State::AlignOrRead;
                self.dummy_read(mem);
            }
            State::Read => {
                self.read(mem);
                return true;
            }
            State::Unfreeze => {
                cpu.unfreeze();
                self.state = State::Inactive;
            }
        }
        false
    }
}
#[cfg(test)]
mod tests;
