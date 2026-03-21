use std::collections::VecDeque;

use crate::{
    cpu::{microcode::Microcode, Flag},
    mcu::Mcu,
};

/// New designed CPU based on microcode execution
pub struct Cpu2<M: Mcu> {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub status: u8,
    mcu: M,

    // hidden internal registers
    /// Last fetched Op Code
    pub opcode: u8,
    /// Address bus value, set by addressing microcodes
    pub ab: u16,
    /// Internal register of ALU
    pub alu: u8,

    microcode_queue: VecDeque<Microcode>,
}

impl<M: Mcu> Cpu2<M> {
    pub fn new(mcu: M) -> Cpu2<M> {
        let microcode_queue = VecDeque::with_capacity(8);

        Cpu2 {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0,
            status: 0,
            mcu,
            opcode: 0,
            ab: 0,
            alu: 0,
            microcode_queue,
        }
    }

    /// Tick the CPU, executing one microcode cycle.
    pub fn tick(&mut self) {
        let microcode = match self.pop_microcode() {
            Some(code) => code,
            None => Microcode::FetchAndDecode,
        };

        microcode.exec(self);
    }

    pub fn reset(&mut self) {
        self.pc = self.read_word(0xFFFC);
        self.microcode_queue.clear();
    }

    #[cfg(test)]
    pub fn mcu(&self) -> &M {
        &self.mcu
    }

    /// Return high byte of pc
    pub(crate) fn pch(&self) -> u8 {
        (self.pc >> 8) as u8
    }

    /// Return low byte of pc
    pub(crate) fn pcl(&self) -> u8 {
        (self.pc & 0xff) as u8
    }

    /// Set high byte of pc
    pub(crate) fn set_pch(&mut self, v: u8) {
        self.pc = (self.pc & 0xff) | ((v as u16) << 8);
    }

    /// Set low byte of pc
    pub(crate) fn set_pcl(&mut self, v: u8) {
        self.pc = (self.pc & 0xff00) | (v as u16);
    }

    /// Return high byte of address bus
    pub(crate) fn abh(&self) -> u8 {
        (self.ab >> 8) as u8
    }

    /// Return low byte of address bus
    pub(crate) fn abl(&self) -> u8 {
        (self.ab & 0xff) as u8
    }

    /// Set high byte of address bus
    pub(crate) fn set_abh(&mut self, v: u8) {
        self.ab = (self.ab & 0xff) | ((v as u16) << 8);
    }

    /// Set low byte of address bus
    pub(crate) fn set_abl(&mut self, v: u8) {
        self.ab = (self.ab & 0xff00) | (v as u16);
    }

    fn inc_pc(&mut self, delta: i8) {
        self.pc = self.pc.wrapping_add(delta as u16);
    }

    pub(crate) fn inc_read_byte(&mut self) -> u8 {
        let addr = self.pc;
        self.inc_pc(1);
        self.mcu.tick();
        self.mcu.read(addr)
    }

    pub(crate) fn read_byte(&mut self, addr: u16) -> u8 {
        self.mcu.tick();
        self.mcu.read(addr)
    }

    pub(crate) fn write_byte(&mut self, addr: u16, value: u8) {
        self.mcu.tick();
        self.mcu.write(addr, value);
    }

    pub(crate) fn flag(&self, flag: Flag) -> bool {
        (self.status & flag as u8) != 0
    }

    pub(crate) fn set_flag(&mut self, flag: Flag, v: bool) {
        if v {
            self.status |= flag as u8;
        } else {
            self.status &= !(flag as u8);
        }
    }

    /// Set the negative flag to the high bit of `value`.
    pub(crate) fn update_negative_flag(&mut self, value: u8) {
        self.set_flag(Flag::Negative, value & 0x80 != 0);
    }

    /// Set the zero flag to `true` if `value` is zero.
    pub(crate) fn update_zero_flag(&mut self, value: u8) {
        self.set_flag(Flag::Zero, value == 0);
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

    pub(crate) fn and(&mut self, val: u8) {
        self.a &= val;
        self.update_zero_flag(self.a);
        self.update_negative_flag(self.a);
    }

    pub(crate) fn push_microcode(&mut self, microcode: Microcode) {
        self.microcode_queue.push_back(microcode);
    }

    pub(crate) fn pop_microcode(&mut self) -> Option<Microcode> {
        self.microcode_queue.pop_front()
    }

    /// Retain the current cycle by set Nop as next microcode, used in "oops" cycles of AbsoluteIndexed and Indirect Indexed addressing
    pub(crate) fn retain_cycle(&mut self) {
        self.microcode_queue.push_front(Microcode::Nop);
    }

    fn read_word(&mut self, addr: u16) -> u16 {
        self.mcu.tick();
        let low = self.mcu.read(addr) as u16;
        self.mcu.tick();
        let high = self.mcu.read(addr.wrapping_add(1)) as u16;
        (high << 8) | low
    }

    pub(crate) fn push_stack(&mut self, value: u8) {
        self.mcu.tick();
        self.mcu.write(0x100 + self.sp as u16, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    pub(crate) fn pop_stack(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.mcu.tick();
        self.mcu.read(0x100 + self.sp as u16)
    }
}

#[cfg(test)]
mod tests;
