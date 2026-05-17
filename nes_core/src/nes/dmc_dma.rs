use crate::{
    Cpu, get_system_clock,
    nes::{NesMcu, apu::AudioDriver},
    render::Render,
};

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
    fn take_dmc_dma_request(&mut self) -> Option<u16>;

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

    fn take_dmc_dma_request(&mut self) -> Option<u16> {
        self.mcu_mut().apu_mut().take_dmc_dma_request()
    }

    fn supply_dmc_byte(&mut self, byte: u8) {
        self.mcu_mut().apu_mut().supply_dmc_byte(byte);
    }

    fn is_get_cycle(&self) -> bool {
        get_system_clock().is_apu_get_clock()
    }
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
                if let Some(addr) = cpu.take_dmc_dma_request() {
                    self.addr = addr;
                    self.state = State::TryHaltCpu;
                }
            }
            State::TryHaltCpu => {
                if cpu.try_freeze() {
                    self.state = State::Dummy;
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

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
enum State {
    #[default]
    Inactive,
    TryHaltCpu,
    AlignOrRead,
    Dummy,
    Read,
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;
    use mockall::*;

    #[test]
    fn dma_not_activated() {
        let mut cpu = MockNesDmaSupport::new();
        let mut dma = DmcDma::default();

        cpu.expect_take_dmc_dma_request()
            .times(1)
            .return_const(None);
        dma.tick(&mut cpu);
        assert_eq!(State::Inactive, dma.state);
    }

    #[test]
    fn dma_requested() {
        let mut cpu = MockNesDmaSupport::new();
        let mut seq = Sequence::new();
        let mut dma = DmcDma::default();

        cpu.expect_take_dmc_dma_request()
            .times(1)
            .in_sequence(&mut seq)
            .return_const(Some(0x1234));

        // get request
        dma.tick(&mut cpu);
        assert_eq!(State::TryHaltCpu, dma.state);
        cpu.checkpoint();
        cpu.checkpoint();

        // try freeze cpu but failed
        cpu.expect_try_freeze()
            .times(1)
            .in_sequence(&mut seq)
            .return_const(false);
        dma.tick(&mut cpu);
        assert_eq!(State::TryHaltCpu, dma.state);
        cpu.checkpoint();
        cpu.checkpoint();

        // try freeze cpu again and succeed
        cpu.expect_try_freeze()
            .times(1)
            .in_sequence(&mut seq)
            .return_const(true);
        dma.tick(&mut cpu);
        assert_eq!(State::Dummy, dma.state);
        cpu.checkpoint();
        cpu.checkpoint();

        // dummy
        dma.tick(&mut cpu);
        assert_eq!(State::AlignOrRead, dma.state);
        cpu.checkpoint();
        cpu.checkpoint();

        // apu is put cycle, so it is an align cycle, no dma read
        cpu.expect_is_get_cycle()
            .times(1)
            .in_sequence(&mut seq)
            .return_const(false);
        dma.tick(&mut cpu);
        assert_eq!(State::Read, dma.state);
        cpu.checkpoint();
        cpu.checkpoint();

        // read and supply to apu
        cpu.expect_read_mem()
            .with(eq(0x1234))
            .times(1)
            .in_sequence(&mut seq)
            .return_const(0xcd);
        cpu.expect_supply_dmc_byte()
            .with(eq(0xcd))
            .times(1)
            .in_sequence(&mut seq)
            .return_const(());
        cpu.expect_unfreeze()
            .times(1)
            .in_sequence(&mut seq)
            .return_const(());
        dma.tick(&mut cpu);
        assert_eq!(State::Inactive, dma.state);
    }
}
