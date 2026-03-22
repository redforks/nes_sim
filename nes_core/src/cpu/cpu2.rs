use crate::{
    cpu::{ExecuteResult, Flag, Plugin},
    mcu::Mcu,
};
use microcode::Microcode;
use std::collections::VecDeque;

mod microcode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Register {
    A,
    X,
    Y,
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
    pub(crate) change_irq_flag_pending: Option<bool>,
    pub(crate) is_halt: bool,

    microcode_queue: VecDeque<Microcode>,
    total_cycles: u64,
}

impl<M: Mcu> Cpu<M> {
    pub fn new(mcu: M) -> Cpu<M> {
        Cpu {
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
            change_irq_flag_pending: None,
            is_halt: false,
            microcode_queue: VecDeque::with_capacity(8),
            total_cycles: 0,
        }
    }

    pub fn mcu(&self) -> &M {
        &self.mcu
    }

    pub fn mcu_mut(&mut self) -> &mut M {
        &mut self.mcu
    }

    pub fn reset(&mut self) {
        self.pc = self.read_word(0xFFFC);
        self.set_flag(Flag::InterruptDisabled, true);
        self.irq_pending = false;
        self.change_irq_flag_pending = None;
        self.is_halt = false;
        self.microcode_queue.clear();
    }

    pub fn nmi(&mut self) {
        self.microcode_queue.clear();
        self.tick_bus();
        self.tick_bus();
        self.push_pc();
        self.push_status();
        self.pc = self.read_word(0xFFFA);
    }

    pub(crate) fn irq(&mut self) {
        self.microcode_queue.clear();
        self.tick_bus();
        self.tick_bus();
        self.push_pc();
        self.push_status();
        self.set_flag(Flag::InterruptDisabled, true);
        self.pc = self.read_word(0xFFFE);
    }

    pub fn set_irq(&mut self, enabled: bool) {
        self.irq_pending = enabled;
    }

    pub(crate) fn pending_set_interrupt_disabled_flag(&mut self, v: bool) {
        self.change_irq_flag_pending = Some(v);
    }

    pub fn tick<T: Plugin<M>>(&mut self, plugin: &mut T) -> (ExecuteResult, u8) {
        if self.is_halt {
            return (ExecuteResult::Continue, 1);
        }

        if self.microcode_queue.is_empty() {
            self.handle_pending_irq();
        }

        let before = self.total_cycles;
        plugin.start(self);
        self.execute_instruction();
        let cycles = (self.total_cycles - before) as u8;
        plugin.end(self, cycles);

        self.set_irq(self.mcu.request_irq());
        (plugin.should_stop(), cycles)
    }

    fn execute_instruction(&mut self) {
        let fetch = Microcode::FetchAndDecode;
        fetch.exec(self);

        while let Some(code) = self.pop_microcode() {
            code.exec(self);
            if self.is_halt && self.microcode_queue.is_empty() {
                break;
            }
        }
    }

    fn handle_pending_irq(&mut self) {
        if self.irq_pending && !self.flag(Flag::InterruptDisabled) {
            if let Some(v) = self.change_irq_flag_pending.take() {
                self.set_flag(Flag::InterruptDisabled, v);
            }
            self.irq();
        } else if let Some(v) = self.change_irq_flag_pending.take() {
            self.set_flag(Flag::InterruptDisabled, v);
        }
    }

    pub(crate) fn push_pc(&mut self) {
        self.push_stack((self.pc >> 8) as u8);
        self.push_stack(self.pc as u8);
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

    pub(crate) fn inc_pc(&mut self, delta: i8) {
        self.pc = self.pc.wrapping_add(delta as u16);
    }

    pub(crate) fn update_negative_flag(&mut self, value: u8) {
        self.set_flag(Flag::Negative, value & 0x80 != 0);
    }

    pub(crate) fn update_zero_flag(&mut self, value: u8) {
        self.set_flag(Flag::Zero, value == 0);
    }

    pub(crate) fn read_byte(&mut self, addr: u16) -> u8 {
        self.tick_bus();
        self.mcu.read(addr)
    }

    pub fn peek_byte(&mut self, addr: u16) -> u8 {
        self.mcu.read(addr)
    }

    pub(crate) fn inc_read_byte(&mut self) -> u8 {
        let addr = self.pc;
        self.inc_pc(1);
        self.tick_bus();
        self.mcu.read(addr)
    }

    pub(crate) fn write_byte(&mut self, addr: u16, value: u8) {
        self.tick_bus();
        self.mcu.write(addr, value);
    }

    pub(crate) fn read_word(&mut self, addr: u16) -> u16 {
        self.tick_bus();
        let low = self.mcu.read(addr) as u16;
        self.tick_bus();
        let high = self.mcu.read(addr.wrapping_add(1)) as u16;
        (high << 8) | low
    }

    pub(crate) fn read_word_in_same_page(&mut self, addr: u16) -> u16 {
        self.read_word_in_page(addr & 0xff00, addr as u8)
    }

    fn read_word_in_page(&mut self, page: u16, offset: u8) -> u16 {
        self.tick_bus();
        let low = self.mcu.read(page | offset as u16) as u16;
        self.tick_bus();
        let high = self.mcu.read(page | offset.wrapping_add(1) as u16) as u16;
        (high << 8) | low
    }

    pub(crate) fn push_stack(&mut self, value: u8) {
        self.tick_bus();
        self.mcu.write(0x100 + self.sp as u16, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    pub(crate) fn pop_stack(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.tick_bus();
        self.mcu.read(0x100 + self.sp as u16)
    }

    pub fn peek_stack(&mut self) -> u8 {
        let addr = 0x100 + self.sp.wrapping_add(1) as u16;
        self.mcu.read(addr)
    }

    pub(crate) fn push_status(&mut self) {
        self.push_stack(self.status | Flag::NotUsed as u8);
    }

    pub(crate) fn halt(&mut self) {
        self.is_halt = true;
    }

    pub fn is_halted(&self) -> bool {
        self.is_halt
    }

    fn tick_bus(&mut self) {
        self.total_cycles = self.total_cycles.wrapping_add(1);
        self.mcu.tick();
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
        self.write_byte(self.ab, v);
    }

    fn shy(&mut self) {
        let v = self.y & self.abh().wrapping_add(1);
        self.write_byte(self.ab, v);
    }

    fn tas(&mut self) {
        let v = self.a & self.x;
        self.sp = v;
        let out = v & self.abh().wrapping_add(1);
        self.write_byte(self.ab, out);
    }

    fn pha(&mut self) {
        self.push_stack(self.a);
    }

    fn pla(&mut self) {
        self.a = self.pop_stack();
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
    }

    fn php(&mut self) {
        self.push_stack(self.status | Flag::Break as u8 | Flag::NotUsed as u8);
    }

    fn plp(&mut self) {
        let saved = self.pop_stack();
        let saved_break_flag = self.flag(Flag::Break);
        let cur_i_flag = self.flag(Flag::InterruptDisabled);
        let saved_i_flag = (saved & Flag::InterruptDisabled as u8) != 0;
        self.status = saved;
        self.set_flag(Flag::Break, saved_break_flag);
        self.set_flag(Flag::InterruptDisabled, cur_i_flag);
        self.pending_set_interrupt_disabled_flag(saved_i_flag);
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

    fn rts(&mut self) {
        let lo = self.pop_stack() as u16;
        let hi = self.pop_stack() as u16;
        self.pc = ((hi << 8) | lo).wrapping_add(1);
    }

    fn rti(&mut self) {
        let v = self.pop_stack();
        self.status = v & 0b1100_1111;
        self.pc = self.pop_stack() as u16 | ((self.pop_stack() as u16) << 8);
    }

    fn brk(&mut self) {
        self.pc = self.pc.wrapping_add(1);
        let pc = self.pc;
        self.set_flag(Flag::Break, true);
        self.push_stack((pc >> 8) as u8);
        self.push_stack(pc as u8);
        self.push_stack(self.status | Flag::Break as u8 | Flag::NotUsed as u8);
        self.set_flag(Flag::InterruptDisabled, true);
        self.pc = self.read_word(0xFFFE);
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

    fn push_microcodes(&mut self, microcodes: &[Microcode]) {
        for &code in microcodes {
            self.microcode_queue.push_back(code);
        }
    }

    fn pop_microcode(&mut self) -> Option<Microcode> {
        self.microcode_queue.pop_front()
    }

    fn retain_cycle(&mut self) {
        self.microcode_queue.push_front(Microcode::Nop);
    }
}

#[cfg(test)]
mod tests;
