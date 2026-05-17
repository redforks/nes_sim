use crate::{
    Cpu, get_system_clock,
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
    /// Return false if cpu next operation is write, return true
    /// if cpu freezed.
    fn try_freeze(&mut self) -> bool;

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

    fn is_get_cycle(&self) -> bool;
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

    fn is_get_cycle(&self) -> bool {
        get_system_clock().is_apu_get_clock()
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
}

/// Dma controller for apu dmc channel
#[derive(Debug, Default)]
pub struct DmcDma {
    /// true after `.request()` method, and before dma finished
    state: State,
    addr: u16,
}

impl DmcDma {
    fn read(&mut self, cpu: &mut impl NesDmaSupport) {
        let byte = cpu.read_mem(self.addr);
        cpu.supply_dmc_byte(byte);
        self.state = State::Inactive;
        cpu.unfreeze();
    }

    pub fn tick(&mut self, cpu: &mut impl NesDmaSupport) {
        match self.state {
            State::Inactive => {
                if let Some((dmc_dma_type, addr)) = cpu.take_dmc_dma_request() {
                    self.addr = addr;
                    self.state = if dmc_dma_type == DmcDmaType::Load {
                        State::DelayForLoad(2)
                    } else {
                        State::TryHalt {
                            halt_on_put: true,
                            first_attempt: true,
                        }
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
                if first_attempt && (!halt_on_put != cpu.is_get_cycle()) {
                    // 相位不对，CPU 照常运行，不消耗 DMA 周期
                    return;
                }

                // 尝试挂起 CPU
                if cpu.try_freeze() {
                    self.state = State::Dummy;
                } else {
                    // 挂起失败（CPU 正在执行写操作）
                    // 下一个周期继续尝试，但此时不再是 "first_attempt"，解绑相位限制
                    self.state = State::TryHalt {
                        first_attempt: false,
                        halt_on_put,
                    };
                }
            }
            State::AlignOrRead => {
                if cpu.is_get_cycle() {
                    self.read(cpu);
                } else {
                    self.state = State::Read;
                }
            }
            State::Dummy => {
                self.state = State::AlignOrRead;
            }
            State::Read => self.read(cpu),
        }
    }
}

#[cfg(test)]
mod tests;
