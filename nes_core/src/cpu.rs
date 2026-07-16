use crate::{SystemClock, cpu::microcode::opcode, mcu::Mcu};
use arraydeque::ArrayDeque;
use microcode::{InterruptSequences, Microcode};
#[cfg(debug_assertions)]
use std::{cell::Cell, rc::Rc};

mod microcode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Register {
    A,
    X,
    Y,
}

#[derive(Default, Debug)]
struct IrqDetector {
    irq_pending: bool,
    irq_input: bool,
}

impl IrqDetector {
    fn update_irq_input(&mut self, v: bool) {
        self.irq_input = v;
    }

    fn detect_irq(&mut self, interrupt_disabled: bool) {
        self.irq_pending = !interrupt_disabled && self.irq_input;
    }

    fn irq_pending(&self) -> bool {
        self.irq_pending
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum NmiState {
    #[default]
    Idle,
    NmiPending,
    InNmi,
}

#[derive(Default, Debug)]
struct NmiDetector {
    state: NmiState,
    last_nmi_input: bool,
    nmi_input: bool,
}

impl NmiDetector {
    fn update_nmi_input(&mut self, v: bool) {
        self.nmi_input = v;
    }

    fn detect_nmi(&mut self) -> bool {
        let rising_edge = self.last_nmi_input != self.nmi_input && self.nmi_input;
        self.last_nmi_input = self.nmi_input;
        if rising_edge && self.state == NmiState::Idle {
            self.state = NmiState::NmiPending;
            true
        } else {
            false
        }
    }

    fn take_nmi_pending(&mut self) -> bool {
        if self.state == NmiState::NmiPending {
            self.state = NmiState::InNmi;
            true
        } else {
            false
        }
    }

    fn leave_nmi(&mut self) {
        self.state = NmiState::Idle;
    }
}

pub struct Cpu<M: Mcu> {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub status: u8,
    last_status: u8,
    mcu: M,

    opcode: u8,
    /// address bus, which memory byte that cpu current select
    ab: u16,
    /// data bus, what byte that cpu will save or get from memory bus
    db: u8, // save low byte during indexed addressing
    alu: u8,

    need_check_interrupt: bool,
    nmi_detecteor: NmiDetector,
    irq_detector: IrqDetector,
    halt: bool,
    pub(crate) last_read_addr: Option<u16>,

    microcode_queue: ArrayDeque<Microcode, 8>,

    #[cfg(debug_assertions)]
    mem_acc_count: Rc<Cell<usize>>,

    pub(crate) frozen: bool,
}

impl<M: Mcu> Cpu<M> {
    pub fn new(mcu: M) -> Cpu<M> {
        let mut r = Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0,
            status: 0,
            last_status: 0,
            mcu,
            opcode: 0,
            need_check_interrupt: false,
            nmi_detecteor: Default::default(),
            irq_detector: Default::default(),
            ab: 0,
            db: 0,
            alu: 0,
            microcode_queue: ArrayDeque::new(),
            halt: false,
            #[cfg(debug_assertions)]
            mem_acc_count: Default::default(),
            frozen: false,
            last_read_addr: None,
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

    /// Set CPU program counter. Panics if there are pending microcodes;
    /// callers must drain the queue before calling set_pc.
    pub fn set_pc(&mut self, pc: u16) {
        assert!(
            self.microcodes_empty(),
            "microcode queue must be empty before setting PC"
        );
        self.pc = pc;
    }

    pub fn microcodes_empty(&self) -> bool {
        self.microcode_queue.is_empty()
    }

    fn next_microcode(&self) -> Microcode {
        self.microcode_queue
            .front()
            .copied()
            .unwrap_or(Microcode::FetchAndDecode)
    }

    /// Return true if cpu can be paused for execute dma, return true if next microcode
    /// not write operation
    pub(crate) fn can_pause(&self) -> bool {
        !self.next_microcode().is_write_operation()
    }

    pub fn reset(&mut self) {
        self.set_flag(Flag::InterruptDisabled, true);
        self.set_flag(Flag::NotUsed, true);
        self.need_check_interrupt = false;
        self.microcode_queue.clear();
        self.halt = false;
        self.sp = self.sp.wrapping_sub(3);
        self.nmi_detecteor = Default::default();
        self.irq_detector = Default::default();
        self.frozen = false;
        self.last_read_addr = None;
        self.last_status = self.status;

        self.push_microcodes(&InterruptSequences::RESET);
    }

    pub fn set_irq(&mut self, enabled: bool) {
        self.irq_detector.update_irq_input(enabled);
    }

    pub fn is_halted(&self) -> bool {
        self.halt
    }

    fn inc_mem_count(&mut self) {
        #[cfg(debug_assertions)]
        self.mem_acc_count.set(self.mem_acc_count.get() + 1);
    }

    fn reset_mem_count(&mut self) {
        #[cfg(debug_assertions)]
        self.mem_acc_count.set(0);
    }

    /// Return true if just execute current instruction
    pub fn tick<P: Plugin<M>>(
        &mut self,
        plugin: &mut P,
        clock: SystemClock,
    ) -> (ExecuteResult, bool) {
        self.reset_mem_count();
        self.last_read_addr = None;

        #[cfg(debug_assertions)]
        let _guard = scopeguard::guard(self.mem_acc_count.clone(), |acc_count| {
            assert!(
                acc_count.get() <= 1,
                "Multiple memory accesses in a single tick: {}",
                acc_count.get()
            );
        });

        if self.frozen {
            return (ExecuteResult::Continue, false);
        }

        if self.is_halted() {
            return (ExecuteResult::Halt, false);
        }

        let code = match self.pop_microcode() {
            Some(v) => v,
            None => {
                plugin.start(self, clock);
                if std::mem::take(&mut self.need_check_interrupt) {
                    if self.nmi_detecteor.take_nmi_pending() {
                        self.push_enter_interrupt_microcodes(true)
                    } else if self.irq_detector.irq_pending() {
                        self.push_enter_interrupt_microcodes(false)
                    } else {
                        Microcode::FetchAndDecode
                    }
                } else {
                    Microcode::FetchAndDecode
                }
            }
        };

        self.last_status = self.status;
        code.exec(self);

        if self.microcode_queue.is_empty() {
            plugin.end(self, clock);
            (plugin.should_stop(), true)
        } else {
            (ExecuteResult::Continue, false)
        }
    }

    pub fn detect_interrupt(&mut self) {
        if self.nmi_detecteor.detect_nmi() {
            self.need_check_interrupt = false;
        }
        let disabled = if matches!(self.opcode, opcode::CLI | opcode::SEI | opcode::PLP) {
            (self.last_status & Flag::InterruptDisabled as u8) != 0
        } else {
            self.flag(Flag::InterruptDisabled)
        };
        self.irq_detector.detect_irq(disabled);
        // Detect interrupt at the second-to-last cycle
        if self.microcode_queue.len() == 1 && !InterruptSequences::is_end(self.microcode_queue[0]) {
            self.need_check_interrupt = true;
        }
    }

    pub fn flag(&self, flag: Flag) -> bool {
        (self.status & flag as u8) != 0
    }

    fn set_flag(&mut self, flag: Flag, v: bool) {
        let mask = flag as u8;
        self.status = (self.status & !mask) | (if v { mask } else { 0 });
    }

    fn inc_pc(&mut self, delta: i8) {
        self.pc = self.pc.wrapping_add(delta as u16);
    }

    fn update_negative_flag(&mut self, value: u8) {
        self.set_flag(Flag::Negative, value & 0x80 != 0);
    }

    fn update_zero_flag(&mut self, value: u8) {
        self.set_flag(Flag::Zero, value == 0);
    }

    pub(crate) fn read_byte_for_dma(&mut self, addr: u16) -> u8 {
        self.mcu.read(addr)
    }

    fn read_byte(&mut self, addr: u16) -> u8 {
        self.last_read_addr = Some(addr);
        self.inc_mem_count();
        self.mcu.read(addr)
    }

    pub fn peek_byte(&self, addr: u16) -> u8 {
        self.mcu.peek(addr)
    }

    fn read_pc_byte(&mut self) {
        self.read_byte(self.pc);
    }

    fn inc_read_byte(&mut self) -> u8 {
        let addr = self.pc;
        self.inc_pc(1);
        self.read_byte(addr)
    }

    fn write_byte(&mut self, addr: u16, value: u8) {
        self.inc_mem_count();
        self.mcu.write(addr, value);
    }

    fn push_stack(&mut self, value: u8) {
        self.inc_mem_count();
        self.mcu.write(0x100 + self.sp as u16, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn pop_stack(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        let addr = 0x100 + self.sp as u16;
        self.read_byte(addr)
    }

    #[cfg(test)]
    fn peek_stack(&mut self) -> u8 {
        let addr = 0x100 + self.sp.wrapping_add(1) as u16;
        self.mcu.peek(addr)
    }

    fn halt(&mut self) {
        self.halt = true;
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
        self.alu = self.read_byte(self.ab);
    }

    fn adc(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        let val = self.alu;
        let carry = self.flag(Flag::Carry) as u8;
        let (sum, carry0) = self.a.overflowing_add(val);
        let (sum, carry1) = sum.overflowing_add(carry);
        self.set_flag(Flag::Carry, carry0 || carry1);
        self.set_flag(Flag::Overflow, !(self.a ^ val) & (self.a ^ sum) & 0x80 != 0);
        self.set_a(sum);
    }

    fn sbc(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        let val = self.alu ^ 0xFF;
        let carry = self.flag(Flag::Carry) as u8;
        let (sum, carry0) = self.a.overflowing_add(val);
        let (sum, carry1) = sum.overflowing_add(carry);
        self.set_flag(Flag::Carry, carry0 || carry1);
        self.set_flag(Flag::Overflow, !(self.a ^ val) & (self.a ^ sum) & 0x80 != 0);
        self.set_a(sum);
    }

    fn ora(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        self.set_a(self.a | self.alu);
    }

    fn eor(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        self.set_a(self.a ^ self.alu);
    }

    fn cmp(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        let t = self.a.wrapping_sub(self.alu);
        self.update_zero_negative_flags(t);
        self.set_flag(Flag::Carry, self.a >= self.alu);
    }

    fn cpx(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        let t = self.x.wrapping_sub(self.alu);
        self.update_zero_negative_flags(t);
        self.set_flag(Flag::Carry, self.x >= self.alu);
    }

    fn cpy(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        let t = self.y.wrapping_sub(self.alu);
        self.update_zero_negative_flags(t);
        self.set_flag(Flag::Carry, self.y >= self.alu);
    }

    fn alr(&mut self) {
        self.a &= self.alu;
        self.set_flag(Flag::Carry, self.a & 0x01 != 0);
        self.set_a(self.a >> 1);
    }

    fn anc(&mut self) {
        self.set_a(self.a & self.alu);
        self.set_flag(Flag::Carry, self.a & 0x80 != 0);
    }

    fn arr(&mut self) {
        self.a &= self.alu;
        self.set_a((self.a >> 1) | ((self.flag(Flag::Carry) as u8) << 7));
        self.set_flag(Flag::Carry, self.a & 0x40 != 0);
        self.set_flag(Flag::Overflow, ((self.a >> 6) ^ (self.a >> 5)) & 1 != 0);
    }

    fn axs(&mut self) {
        let v = self.a & self.x;
        let (x, borrow) = v.overflowing_sub(self.alu);
        self.set_x(x);
        self.set_flag(Flag::Carry, !borrow);
    }

    fn lax(&mut self) {
        self.set_a(self.alu);
        self.set_x(self.alu);
    }

    fn sax(&mut self) {
        self.write_byte(self.ab, self.a & self.x);
    }

    fn dcp(&mut self) {
        let v = self.alu.wrapping_sub(1);
        self.write_byte(self.ab, v);
        self.update_zero_negative_flags(self.a.wrapping_sub(v));
        self.set_flag(Flag::Carry, self.a >= v);
    }

    fn isc(&mut self) {
        let v = self.alu.wrapping_add(1);
        self.write_byte(self.ab, v);
        self.alu = v;
        self.sbc(false);
    }

    fn rra(&mut self) {
        let carry = self.alu & 0x01 != 0;
        self.alu = (self.alu >> 1) | ((self.flag(Flag::Carry) as u8) << 7);
        self.write_byte(self.ab, self.alu);
        self.set_flag(Flag::Carry, carry);
        self.adc(false);
    }

    fn rla(&mut self) {
        let new = (self.alu << 1) | (self.flag(Flag::Carry) as u8);
        self.set_flag(Flag::Carry, self.alu & 0x80 != 0);
        self.alu = new;
        self.write_byte(self.ab, self.alu);
        self.and(false);
    }

    fn slo(&mut self) {
        self.set_flag(Flag::Carry, self.alu & 0x80 != 0);
        self.alu <<= 1;
        self.write_byte(self.ab, self.alu);
        self.ora(false);
    }

    fn sre(&mut self) {
        self.set_flag(Flag::Carry, self.alu & 0x01 != 0);
        self.alu >>= 1;
        self.write_byte(self.ab, self.alu);
        self.eor(false);
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

    fn sha(&mut self) {
        // SHA (AHX/AXA): store A & X & (high-byte of addr + 1) at address
        let out = self.a & self.x & self.abh().wrapping_add(1);
        let addr = (self.abl() as u16) | ((out as u16) << 8);
        self.write_byte(addr, out);
    }

    fn tas(&mut self) {
        let v = self.a & self.x;
        self.sp = v;
        let out = v & self.abh().wrapping_add(1);
        let addr = (self.abl() as u16) | ((out as u16) << 8);
        self.write_byte(addr, out);
    }

    fn push_status(&mut self, break_flag: bool, check_nmi: bool) {
        if check_nmi {
            self.nmi_detecteor.detect_nmi();
            if self.nmi_detecteor.take_nmi_pending() {
                // eprintln!("nmi hijack");
                self.push_status(break_flag, false);
                self.microcode_queue.clear();
                self.push_microcodes(&[Microcode::LoadNmiPcL, Microcode::LoadNmiPcH]);
                return;
            }
        }

        self.push_stack(if break_flag {
            self.status | Flag::Break as u8 | Flag::NotUsed as u8
        } else {
            self.status | Flag::NotUsed as u8
        });
    }

    fn plp(&mut self) {
        let break_flag = self.flag(Flag::Break);
        self.status = self.pop_stack();
        self.set_flag(Flag::Break, break_flag);
        self.set_flag(Flag::NotUsed, true);
    }

    fn set_pc_to_ab(&mut self) {
        self.pc = self.ab;
    }

    fn and(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }
        self.set_a(self.a & self.alu);
    }

    fn bit(&mut self) {
        let v = self.read_byte(self.ab);
        self.set_flag(Flag::Negative, v & 0x80 != 0);
        self.set_flag(Flag::Overflow, v & 0x40 != 0);
        self.update_zero_flag(self.a & v);
    }

    fn push_microcodes(&mut self, microcodes: &[Microcode]) {
        self.microcode_queue.extend_back(microcodes.iter().copied());
    }

    /// And return the first microcode
    fn push_enter_interrupt_microcodes(&mut self, nmi: bool) -> Microcode {
        // if nmi {
        //     eprintln!("enter nmi");
        // }
        self.push_microcodes(if nmi {
            &InterruptSequences::NMI
        } else {
            &InterruptSequences::IRQ
        });
        Microcode::FetchOnly
    }

    fn pop_microcode(&mut self) -> Option<Microcode> {
        self.microcode_queue.pop_front()
    }

    fn push_microcode(&mut self, microcode: Microcode) {
        match self.microcode_queue.push_front(microcode) {
            Ok(_) => (),
            Err(_) => debug_assert!(
                false,
                "Microcode queue overflow, maybe some microcode is too long?"
            ),
        }
    }

    fn retain_cycle(&mut self) {
        self.push_microcode(Microcode::Nop);
    }

    /// Update cpu nmi signal line, may trigger nmi
    pub fn update_nmi_line(&mut self, nmi: bool) {
        self.nmi_detecteor.update_nmi_input(nmi);
    }

    fn load_nmi_pcl(&mut self) {
        self.set_flag(Flag::InterruptDisabled, true);
        self.pc = self.read_byte(0xFFFA) as u16;
    }

    fn load_nmi_pch(&mut self) {
        self.pc |= (self.read_byte(0xFFFB) as u16) << 8;
    }

    fn load_irq_pcl(&mut self) {
        self.set_flag(Flag::InterruptDisabled, true);
        self.pc = self.read_byte(0xFFFE) as u16;
    }

    fn load_irq_pch(&mut self) {
        let high = self.read_byte(0xFFFF);
        self.pc |= (high as u16) << 8;
    }

    /// Set register A and update negative and zero flags.
    fn set_a(&mut self, val: u8) {
        self.a = val;
        self.update_zero_negative_flags(val);
    }

    /// Set register X and update negative and zero flags.
    fn set_x(&mut self, val: u8) {
        self.x = val;
        self.update_zero_negative_flags(val);
    }

    /// Set register Y and update negative and zero flags.
    fn set_y(&mut self, val: u8) {
        self.y = val;
        self.update_zero_negative_flags(val);
    }

    fn update_zero_negative_flags(&mut self, val: u8) {
        self.update_zero_flag(val);
        self.update_negative_flag(val);
    }
}

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
    fn start(&mut self, cpu: &Cpu<M>, system_clock: SystemClock);

    /// After execute instruction
    fn end(&mut self, cpu: &Cpu<M>, system_clock: SystemClock);

    /// After execute an instruction, tell cpu should stop execution or not
    fn should_stop(&self) -> ExecuteResult {
        ExecuteResult::Continue
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
    fn start(&mut self, _: &Cpu<M>, _: SystemClock) {}

    fn end(&mut self, _: &Cpu<M>, _: SystemClock) {}
}

impl<M: Mcu> Default for EmptyPlugin<M> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Flag {
    Carry = 0x01u8,
    Zero = 0x02u8,
    InterruptDisabled = 0x04u8,
    Decimal = 0x08u8,
    Break = 0x10u8,
    NotUsed = 0x20u8,
    Overflow = 0x40u8,
    Negative = 0x80u8,
}

#[cfg(test)]
mod tests;
