use crate::mcu::Mcu;

mod cpu2;

pub use cpu2::Cpu;

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum ExecuteResult {
    Continue,
    Stop(u8),
    ShouldReset,
}

pub trait Plugin<M: Mcu> {
    fn start(&mut self, cpu: &mut Cpu<M>);

    fn end(&mut self, cpu: &mut Cpu<M>, cycles: u8);

    fn should_stop(&self) -> ExecuteResult {
        ExecuteResult::Continue
    }
}

impl<M: Mcu> Plugin<M> for Box<dyn Plugin<M>> {
    fn start(&mut self, cpu: &mut Cpu<M>) {
        self.as_mut().start(cpu);
    }

    fn end(&mut self, cpu: &mut Cpu<M>, cycles: u8) {
        self.as_mut().end(cpu, cycles);
    }

    fn should_stop(&self) -> ExecuteResult {
        self.as_ref().should_stop()
    }
}

pub struct EmptyPlugin<M: Mcu> {
    _phantom: std::marker::PhantomData<M>,
}

impl<M: Mcu> EmptyPlugin<M> {
    pub fn new() -> Self {
        Self {
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<M: Mcu> Plugin<M> for EmptyPlugin<M> {
    fn start(&mut self, _: &mut Cpu<M>) {}

    fn end(&mut self, _: &mut Cpu<M>, _cycles: u8) {}
}

impl<M: Mcu> Default for EmptyPlugin<M> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Copy, strum_macros::Display, PartialEq, Eq)]
#[repr(u8)]
pub enum Flag {
    #[strum(serialize = "C")]
    Carry = 0x01u8,
    #[strum(serialize = "Z")]
    Zero = 0x02u8,
    #[strum(serialize = "I")]
    InterruptDisabled = 0x04u8,
    #[strum(serialize = "D")]
    Decimal = 0x08u8,
    #[strum(serialize = "B")]
    Break = 0x10u8,
    NotUsed = 0x20u8,
    #[strum(serialize = "V")]
    Overflow = 0x40u8,
    #[strum(serialize = "N")]
    Negative = 0x80u8,
}

#[cfg(test)]
mod tests;
