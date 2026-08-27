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

#[cfg_attr(test, mockall::automock)]
pub(crate) trait NesDmaSupport {
    /// Address the pending CPU cycle puts on the bus; repeated externally
    /// while the CPU is halted by DMA. `None` for purely internal cycles.
    fn dma_halt_bus_addr(&self) -> Option<u16>;

    /// Return false if cpu next operation is write, return true
    /// if cpu freezed.
    fn try_freeze(&mut self) -> bool;

    fn last_read_addr(&self) -> u16;

    /// unfreeze cpu, called when finished dma
    fn unfreeze(&mut self);

    /// Read memory for dma
    ///
    /// If no memory activitity in this cycle, then cpu will repeat last read operation
    fn read_mem(&mut self, addr: u16) -> u8;

    /// Take dmc dma request from apu, None if no current request
    fn take_dmc_dma_request(&mut self) -> Option<(DmcDmaType, u16)>;

    /// Supply dmc dma result to apu
    fn supply_dmc_byte(&mut self, byte: u8);

    fn is_get_cycle(&self, clock: SystemClock) -> bool;
}

impl<R: Render, D: AudioDriver> NesDmaSupport for Cpu<NesMcu<R, D>> {
    fn try_freeze(&mut self) -> bool {
        debug_assert!(!self.frozen);
        if self.can_pause() {
            self.frozen = true;
            true
        } else {
            false
        }
    }

    fn last_read_addr(&self) -> u16 {
        self.last_read_addr.unwrap_or(self.pc())
    }

    fn dma_halt_bus_addr(&self) -> Option<u16> {
        Cpu::dma_halt_bus_addr(self)
    }

    fn unfreeze(&mut self) {
        debug_assert!(self.frozen);
        self.frozen = false;
    }

    fn read_mem(&mut self, addr: u16) -> u8 {
        self.read_byte_for_dma(addr)
    }

    fn take_dmc_dma_request(&mut self) -> Option<(DmcDmaType, u16)> {
        self.mcu_mut().apu_mut().take_dmc_dma_request()
    }

    fn supply_dmc_byte(&mut self, byte: u8) {
        self.mcu_mut().apu_mut().supply_dmc_byte(byte);
    }

    fn is_get_cycle(&self, clock: SystemClock) -> bool {
        clock.is_apu_get_clock()
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
    cpu_last_read_addr: u16,
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
    fn read(&mut self, cpu: &mut impl NesDmaSupport) {
        let byte = cpu.read_mem(self.addr);
        cpu.supply_dmc_byte(byte);
        self.state = State::Unfreeze;
    }

    fn dummy_read(&self, cpu: &mut impl NesDmaSupport) {
        cpu.read_mem(self.cpu_last_read_addr);
    }

    /// Advance the DMA state machine by one tick. Returns true if the DMA
    /// performed its memory read this cycle (it drove the bus).
    pub fn tick(&mut self, cpu: &mut impl NesDmaSupport, clock: SystemClock) -> bool {
        match self.state {
            State::Inactive => {
                if let Some((dmc_dma_type, addr)) = cpu.take_dmc_dma_request() {
                    self.addr = addr;
                    if dmc_dma_type == DmcDmaType::Load {
                        self.state = State::DelayForLoad(1);
                    } else {
                        self.state = State::TryHalt {
                            halt_on_put: true,
                            first_attempt: true,
                        };
                        // reload requests, no request cycle
                        self.tick(cpu, clock);
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
                if first_attempt && halt_on_put == cpu.is_get_cycle(clock) {
                    // 相位不对，CPU 照常运行，不消耗 DMA 周期
                    return false;
                }

                if cpu.try_freeze() {
                    // The halt cycle repeats the pending read on the bus;
                    // later no-op DMA cycles keep repeating that address.
                    match cpu.dma_halt_bus_addr() {
                        Some(addr) => {
                            cpu.read_mem(addr);
                            self.cpu_last_read_addr = addr;
                        }
                        None => {
                            self.cpu_last_read_addr = cpu.last_read_addr();
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
                if cpu.is_get_cycle(clock) {
                    self.read(cpu);
                    return true;
                } else {
                    self.state = State::Read;
                    self.dummy_read(cpu);
                }
            }
            State::Dummy => {
                self.state = State::AlignOrRead;
                self.dummy_read(cpu);
            }
            State::Read => {
                self.read(cpu);
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
