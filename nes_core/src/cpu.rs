use crate::mcu::Mcu;
use std::collections::VecDeque;

mod microcode;
use self::microcode::Microcode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    A,
    X,
    Y,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CpuMode {
    Normal,
    /// Set when cpu detected nmi_requested, reset to Normal if nmi handle complete
    Nmi,
    /// Set when hit some illegal/undocument hlt instruction, reset to normal if call `reset()`
    Halt,
}

pub struct Cpu<M: Mcu> {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub status: u8,
    mcu: M,

    pub opcode: u8,
    pub ab: u16,
    pub alu: u8,

    pub(crate) irq_pending: bool,
    nmi_requested: bool,
    mode: CpuMode,

    microcode_queue: VecDeque<Microcode>,
}

impl<M: Mcu> Cpu<M> {
    pub fn new(mcu: M) -> Cpu<M> {
        let mut r = Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0,
            status: Flag::InterruptDisabled as u8,
            mcu,
            opcode: 0,
            ab: 0,
            alu: 0,
            irq_pending: false,
            microcode_queue: VecDeque::with_capacity(8),
            mode: CpuMode::Normal,
            nmi_requested: false,
        };
        r.reset();
        r
    }

    pub fn mcu(&self) -> &M {
        &self.mcu
    }

    pub fn mcu_mut(&mut self) -> &mut M {
        &mut self.mcu
    }

    pub fn reset(&mut self) {
        self.pc = self.read_word(0xFFFC);
        self.status = 0;
        self.set_flag(Flag::InterruptDisabled, true);
        self.set_flag(Flag::NotUsed, true);
        self.irq_pending = false;
        self.microcode_queue.clear();
        self.nmi_requested = false;
        self.mode = CpuMode::Normal;
        self.sp = 0xFD;
    }

    /// Cpu will enter nmi before exec next instrnuction
    pub fn request_nmi(&mut self) {
        self.nmi_requested = true;
    }

    pub fn set_irq(&mut self, enabled: bool) {
        self.irq_pending = enabled;
    }

    pub fn is_halted(&self) -> bool {
        self.mode == CpuMode::Halt
    }

    /// Return true if just execute FetchAndDecode microcode, used for plugin to know when instruction just decoded
    fn tick_inner(&mut self) -> bool {
        if self.is_halted() {
            return false;
        }

        let code = match self.pop_microcode() {
            Some(v) => v,
            None => {
                if self.nmi_requested {
                    // reset nmi_requested, because nmi signal is edge detected, ignored if cpu is already in nmi mode
                    self.nmi_requested = false;
                    if self.mode == CpuMode::Normal {
                        self.mode = CpuMode::Nmi;
                        // need extra two cycles for cpu to start nmi process
                        self.push_microcodes(&[
                            Microcode::Nop,
                            Microcode::Nop,
                            Microcode::PushPc,
                            Microcode::PushStatus {
                                set_disable_interrupt: true,
                                break_flag: false,
                            },
                            Microcode::Nop,
                            Microcode::LoadNmiAddress,
                        ]);
                        Microcode::Nop
                    } else {
                        Microcode::FetchAndDecode
                    }
                } else if self.irq_pending {
                    self.push_microcodes(&[
                        Microcode::Nop,
                        Microcode::Nop,
                        Microcode::PushPc,
                        Microcode::PushStatus {
                            set_disable_interrupt: true,
                            break_flag: false,
                        },
                        Microcode::Nop,
                        Microcode::LoadIrqAddress,
                    ]);
                    Microcode::IncPc
                } else {
                    Microcode::FetchAndDecode
                }
            }
        };
        code.exec(self);
        code == Microcode::FetchAndDecode
    }

    pub fn tick(&mut self) {
        self.tick_inner();
    }

    pub fn execute_instruction<T: Plugin<M>>(&mut self, plugin: &mut T) -> ExecuteResult {
        if self.is_halted() {
            return ExecuteResult::Halt;
        }

        if self.microcode_queue.is_empty() {
            self.push_microcode(Microcode::FetchAndDecode);
        }

        plugin.start(self);
        while !self.microcode_queue.is_empty() {
            if self.tick_inner() {
                plugin.decoded(self);
            }
        }
        plugin.end(self);

        plugin.should_stop()
    }

    fn push_pc(&mut self) {
        self.push_stack((self.pc >> 8) as u8);
        self.push_stack(self.pc as u8);
    }

    fn pop_pc(&mut self) {
        let low = self.pop_stack() as u16;
        let high = self.pop_stack() as u16;
        self.pc = (high << 8) | low;
    }

    pub fn flag(&self, flag: Flag) -> bool {
        (self.status & flag as u8) != 0
    }

    pub(crate) fn set_flag(&mut self, flag: Flag, v: bool) {
        if v {
            self.status |= flag as u8;
        } else {
            self.status &= !(flag as u8);
        }
    }

    fn inc_pc(&mut self, delta: i8) {
        self.pc = self.pc.wrapping_add(delta as u16);
    }

    pub(crate) fn update_negative_flag(&mut self, value: u8) {
        self.set_flag(Flag::Negative, value & 0x80 != 0);
    }

    pub(crate) fn update_zero_flag(&mut self, value: u8) {
        self.set_flag(Flag::Zero, value == 0);
    }

    pub(crate) fn read_byte(&mut self, addr: u16) -> u8 {
        self.mcu.read(addr)
    }

    pub fn peek_byte(&mut self, addr: u16) -> u8 {
        self.mcu.read(addr)
    }

    pub(crate) fn inc_read_byte(&mut self) -> u8 {
        let addr = self.pc;
        self.inc_pc(1);
        self.mcu.read(addr)
    }

    pub(crate) fn write_byte(&mut self, addr: u16, value: u8) {
        self.mcu.write(addr, value);
    }

    pub(crate) fn read_word(&mut self, addr: u16) -> u16 {
        let low = self.mcu.read(addr) as u16;
        let high = self.mcu.read(addr.wrapping_add(1)) as u16;
        (high << 8) | low
    }

    pub(crate) fn read_word_in_same_page(&mut self, addr: u16) -> u16 {
        self.read_word_in_page(addr & 0xff00, addr as u8)
    }

    fn read_word_in_page(&mut self, page: u16, offset: u8) -> u16 {
        let low = self.mcu.read(page | offset as u16) as u16;
        let high = self.mcu.read(page | offset.wrapping_add(1) as u16) as u16;
        (high << 8) | low
    }

    pub(crate) fn push_stack(&mut self, value: u8) {
        self.mcu.write(0x100 + self.sp as u16, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    pub(crate) fn pop_stack(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.mcu.read(0x100 + self.sp as u16)
    }

    pub fn peek_stack(&mut self) -> u8 {
        let addr = 0x100 + self.sp.wrapping_add(1) as u16;
        self.mcu.read(addr)
    }

    pub(crate) fn halt(&mut self) {
        self.mode = CpuMode::Halt;
    }

    fn pch(&self) -> u8 {
        (self.pc >> 8) as u8
    }

    fn abh(&self) -> u8 {
        (self.ab >> 8) as u8
    }

    fn abl(&self) -> u8 {
        (self.ab & 0xff) as u8
    }

    fn set_abh(&mut self, v: u8) {
        self.ab = (self.ab & 0x00ff) | ((v as u16) << 8);
    }

    fn set_abl(&mut self, v: u8) {
        self.ab = (self.ab & 0xff00) | v as u16;
    }

    fn load_alu(&mut self) {
        self.alu = self.mcu.read(self.ab);
    }

    pub(crate) fn adc(&mut self, val: u8) {
        let carry = self.flag(Flag::Carry) as u8;
        let (sum, carry0) = self.a.overflowing_add(val);
        let (sum, carry1) = sum.overflowing_add(carry);
        self.set_flag(Flag::Carry, carry0 || carry1);
        self.set_flag(Flag::Overflow, !(self.a ^ val) & (self.a ^ sum) & 0x80 != 0);
        self.update_zero_flag(sum);
        self.update_negative_flag(sum);
        self.a = sum;
    }

    pub(crate) fn adc_alu(&mut self) {
        self.adc(self.alu);
    }

    fn sbc(&mut self) {
        let val = self.alu ^ 0xFF;
        let carry = self.flag(Flag::Carry) as u8;
        let (sum, carry0) = self.a.overflowing_add(val);
        let (sum, carry1) = sum.overflowing_add(carry);
        self.set_flag(Flag::Carry, carry0 || carry1);
        self.set_flag(Flag::Overflow, !(self.a ^ val) & (self.a ^ sum) & 0x80 != 0);
        self.update_zero_flag(sum);
        self.update_negative_flag(sum);
        self.a = sum;
    }

    fn ora(&mut self) {
        self.a |= self.alu;
        self.update_zero_flag(self.a);
        self.update_negative_flag(self.a);
    }

    fn eor(&mut self) {
        self.a ^= self.alu;
        self.update_zero_flag(self.a);
        self.update_negative_flag(self.a);
    }

    fn cmp(&mut self) {
        let t = self.a.wrapping_sub(self.alu);
        self.update_negative_flag(t);
        self.update_zero_flag(t);
        self.set_flag(Flag::Carry, self.a >= self.alu);
    }

    fn cpx(&mut self) {
        let t = self.x.wrapping_sub(self.alu);
        self.update_negative_flag(t);
        self.update_zero_flag(t);
        self.set_flag(Flag::Carry, self.x >= self.alu);
    }

    fn cpy(&mut self) {
        let t = self.y.wrapping_sub(self.alu);
        self.update_negative_flag(t);
        self.update_zero_flag(t);
        self.set_flag(Flag::Carry, self.y >= self.alu);
    }

    fn tax(&mut self) {
        self.x = self.a;
        self.update_negative_flag(self.x);
        self.update_zero_flag(self.x);
    }

    fn txa(&mut self) {
        self.a = self.x;
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
    }

    fn tay(&mut self) {
        self.y = self.a;
        self.update_negative_flag(self.y);
        self.update_zero_flag(self.y);
    }

    fn tya(&mut self) {
        self.a = self.y;
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
    }

    fn tsx(&mut self) {
        self.x = self.sp;
        self.update_negative_flag(self.x);
        self.update_zero_flag(self.x);
    }

    fn txs(&mut self) {
        self.sp = self.x;
    }

    fn inx(&mut self) {
        self.x = self.x.wrapping_add(1);
        self.update_negative_flag(self.x);
        self.update_zero_flag(self.x);
    }

    fn iny(&mut self) {
        self.y = self.y.wrapping_add(1);
        self.update_negative_flag(self.y);
        self.update_zero_flag(self.y);
    }

    fn dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.update_negative_flag(self.x);
        self.update_zero_flag(self.x);
    }

    fn dey(&mut self) {
        self.y = self.y.wrapping_sub(1);
        self.update_negative_flag(self.y);
        self.update_zero_flag(self.y);
    }

    fn alr(&mut self) {
        self.a &= self.alu;
        self.set_flag(Flag::Carry, self.a & 0x01 != 0);
        self.a >>= 1;
        self.update_zero_flag(self.a);
        self.update_negative_flag(self.a);
    }

    fn anc(&mut self) {
        self.a &= self.alu;
        self.update_zero_flag(self.a);
        self.update_negative_flag(self.a);
        self.set_flag(Flag::Carry, self.a & 0x80 != 0);
    }

    fn arr(&mut self) {
        self.a &= self.alu;
        self.a = (self.a >> 1) | ((self.flag(Flag::Carry) as u8) << 7);
        self.update_zero_flag(self.a);
        self.update_negative_flag(self.a);
        self.set_flag(Flag::Carry, self.a & 0x40 != 0);
        self.set_flag(Flag::Overflow, ((self.a >> 6) ^ (self.a >> 5)) & 1 != 0);
    }

    fn axs(&mut self) {
        let v = self.a & self.x;
        let (x, borrow) = v.overflowing_sub(self.alu);
        self.x = x;
        self.update_negative_flag(self.x);
        self.update_zero_flag(self.x);
        self.set_flag(Flag::Carry, !borrow);
    }

    fn lax(&mut self) {
        self.a = self.alu;
        self.x = self.alu;
        self.update_negative_flag(self.alu);
        self.update_zero_flag(self.alu);
    }

    fn sax(&mut self) {
        self.write_byte(self.ab, self.a & self.x);
    }

    fn dcp(&mut self) {
        let v = self.alu.wrapping_sub(1);
        self.write_byte(self.ab, v);
        let t = self.a.wrapping_sub(v);
        self.update_negative_flag(t);
        self.update_zero_flag(t);
        self.set_flag(Flag::Carry, self.a >= v);
    }

    fn isc(&mut self) {
        let v = self.alu.wrapping_add(1);
        self.write_byte(self.ab, v);
        self.alu = v;
        self.sbc();
    }

    fn inc(&mut self) {
        self.alu = self.alu.wrapping_add(1);
        self.write_byte(self.ab, self.alu);
        self.update_negative_flag(self.alu);
        self.update_zero_flag(self.alu);
    }

    fn dec(&mut self) {
        self.alu = self.alu.wrapping_sub(1);
        self.write_byte(self.ab, self.alu);
        self.update_negative_flag(self.alu);
        self.update_zero_flag(self.alu);
    }

    fn rra(&mut self) {
        let carry = self.alu & 0x01 != 0;
        self.alu = (self.alu >> 1) | ((self.flag(Flag::Carry) as u8) << 7);
        self.write_byte(self.ab, self.alu);
        self.set_flag(Flag::Carry, carry);
        self.adc_alu();
    }

    fn rla(&mut self) {
        let new = (self.alu << 1) | (self.flag(Flag::Carry) as u8);
        self.set_flag(Flag::Carry, self.alu & 0x80 != 0);
        self.alu = new;
        self.write_byte(self.ab, self.alu);
        self.and();
    }

    fn slo(&mut self) {
        self.set_flag(Flag::Carry, self.alu & 0x80 != 0);
        self.alu <<= 1;
        self.write_byte(self.ab, self.alu);
        self.ora();
    }

    fn sre(&mut self) {
        self.set_flag(Flag::Carry, self.alu & 0x01 != 0);
        self.alu >>= 1;
        self.write_byte(self.ab, self.alu);
        self.eor();
    }

    fn shx(&mut self) {
        let v = self.x & self.abh().wrapping_add(1);
        let addr = (self.abl() as u16) | ((v as u16) << 8);
        self.write_byte(addr, v);
    }

    fn shy(&mut self) {
        let v = self.y & self.abh().wrapping_add(1);
        let addr = (self.abl() as u16) | ((v as u16) << 8);
        self.write_byte(addr, v);
    }

    fn tas(&mut self) {
        let v = self.a & self.x;
        self.sp = v;
        let out = v & self.abh().wrapping_add(1);
        let addr = (self.abl() as u16) | ((out as u16) << 8);
        self.write_byte(addr, out);
    }

    fn pha(&mut self) {
        self.push_stack(self.a);
    }

    fn pla(&mut self) {
        self.a = self.pop_stack();
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
    }

    fn push_status(&mut self, set_disable_interrupt: bool, break_flag: bool) {
        self.push_stack(if break_flag {
            self.status | Flag::Break as u8 | Flag::NotUsed as u8
        } else {
            self.status | Flag::NotUsed as u8
        });
        if set_disable_interrupt {
            self.set_flag(Flag::InterruptDisabled, true);
        }
    }

    fn plp(&mut self) {
        let saved = self.pop_stack();
        let break_flag = self.flag(Flag::Break);
        let not_used = self.flag(Flag::NotUsed);
        self.status = saved;
        self.set_flag(Flag::Break, break_flag);
        self.set_flag(Flag::NotUsed, not_used);
    }

    fn jmp_absolute(&mut self) {
        self.pc = self.ab;
    }

    fn jmp_indirect(&mut self) {
        self.pc = self.read_word_in_same_page(self.ab);
    }

    fn jsr(&mut self) {
        let ret = self.pc.wrapping_sub(1);
        self.push_stack((ret >> 8) as u8);
        self.push_stack(ret as u8);
        self.pc = self.ab;
    }

    fn and(&mut self) {
        self.a &= self.alu;
        self.update_zero_flag(self.a);
        self.update_negative_flag(self.a);
    }

    fn bit(&mut self) {
        self.set_flag(Flag::Negative, self.alu & 0x80 != 0);
        self.set_flag(Flag::Overflow, self.alu & 0x40 != 0);
        self.update_zero_flag(self.a & self.alu);
    }

    fn asl(&mut self, val: u8) -> u8 {
        self.set_flag(Flag::Carry, val & 0x80 != 0);
        let result = val << 1;
        self.update_zero_flag(result);
        self.update_negative_flag(result);
        result
    }

    fn push_microcode(&mut self, microcode: Microcode) {
        self.microcode_queue.push_back(microcode);
    }

    pub(crate) fn push_microcodes(&mut self, microcodes: &[Microcode]) {
        for &code in microcodes {
            self.microcode_queue.push_back(code);
        }
    }

    /// Return how many microcodes are in queue, used for plugin to know how many cycles left for current instruction
    pub fn microcodes_len(&self) -> usize {
        self.microcode_queue.len()
    }

    fn pop_microcode(&mut self) -> Option<Microcode> {
        self.microcode_queue.pop_front()
    }

    fn retain_cycle(&mut self) {
        self.microcode_queue.push_front(Microcode::Nop);
    }
}

// ExecuteResult, Plugin, EmptyPlugin, and Flag remain from original cpu.rs

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum ExecuteResult {
    Continue,
    Stop(u8),
    ShouldReset,
    /// Cpu halted because of executed an invalid instruction, only reset can recover
    Halt,
}

pub trait Plugin<M: Mcu> {
    /// Before start execute new instruction
    fn start(&mut self, cpu: &mut Cpu<M>);

    /// After fetch and decoded microcodes, before execute
    fn decoded(&mut self, _cpu: &Cpu<M>) {}

    /// After execute instruction
    fn end(&mut self, cpu: &mut Cpu<M>);

    /// After execute an instruction, tell cpu should stop execution or not
    fn should_stop(&self) -> ExecuteResult {
        ExecuteResult::Continue
    }
}

impl<M: Mcu> Plugin<M> for Box<dyn Plugin<M>> {
    fn start(&mut self, cpu: &mut Cpu<M>) {
        self.as_mut().start(cpu);
    }

    fn decoded(&mut self, cpu: &Cpu<M>) {
        self.as_mut().decoded(cpu);
    }

    fn end(&mut self, cpu: &mut Cpu<M>) {
        self.as_mut().end(cpu);
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

    fn end(&mut self, _: &mut Cpu<M>) {}
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
use self::microcode::{zero_page_addr, zero_page_load_alu, zero_page_x_addr};

#[cfg(test)]
mod tests;
