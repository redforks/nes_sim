use std::convert::From;
use std::ops::BitAnd;
use std::panic::panic_any;

mod addressing;
mod instruction;
pub mod mcu_mem;
pub mod nes;

use crate::Agu::{Absolute, AbsoluteX, Literal, RegisterA, RegisterY, ZeroPage};
use addressing::Address;

/// Address generation unit
#[derive(Debug, Clone, Copy, PartialEq)]
enum Agu {
    Literal(u8),
    ZeroPage(u8),
    Absolute(u16),
    ZeroPageX(u8),
    ZeroPageY(u8),
    AbsoluteX(u16),
    AbsoluteY(u16),
    IndirectX(u8),
    IndirectY(u8),
    RegisterA,
    RegisterX,
    RegisterY,
    RegisterSP,
    Status,
    CarryFlag,
    ZeroFlag,
    NegativeFlag,
    OverflowFlag,
}

fn is_cross_page(a: u16, b: u16) -> bool {
    let a = (a >> 8) as u8;
    let b = (b >> 8) as u8;
    a != b
}

fn plus_one_if_cross_page(v: u8, a: u16, b: u16) -> u8 {
    if is_cross_page(a, b) {
        v + 1
    } else {
        v
    }
}

impl Agu {
    /// Return  (address, ticks, operands) ticks: count for the given addressing mode
    /// operands: number of operands in bytes for the given addressing mode
    fn address(&self, cpu: &Cpu) -> (u16, u8) {
        match self {
            &Agu::Literal(_) => panic_any("Literal not supported"),
            &Agu::ZeroPage(addr) => (addr as u16, 1),
            &Agu::Absolute(addr) => (addr, 2),
            &Agu::ZeroPageX(addr) => (addr.wrapping_add(cpu.x) as u16, 2),
            &Agu::ZeroPageY(addr) => (addr.wrapping_add(cpu.y) as u16, 2),
            &Agu::AbsoluteX(addr) => {
                let r = addr.wrapping_add(cpu.x as u16) as u16;
                (r, plus_one_if_cross_page(2, addr, r))
            }
            &Agu::AbsoluteY(addr) => {
                let r = addr.wrapping_add(cpu.y as u16) as u16;
                (r, plus_one_if_cross_page(2, addr, r))
            }
            &Agu::IndirectX(addr) => (cpu.read_zero_page_word(addr.wrapping_add(cpu.x)), 4),
            &Agu::IndirectY(addr) => {
                let addr = cpu.read_zero_page_word(addr);
                let r = addr.wrapping_add(cpu.y as u16);
                (r, plus_one_if_cross_page(3, addr, r))
            }
            Agu::RegisterA => panic_any("RegisterA not supported"),
            Agu::RegisterX => panic_any("RegisterX not supported"),
            Agu::RegisterY => panic_any("RegisterY not supported"),
            Agu::RegisterSP => panic_any("RegisterSP not supported"),
            Agu::Status => panic_any("Status not supported"),
            Agu::CarryFlag => panic_any("CarryFlag not supported"),
            Agu::ZeroFlag => panic_any("ZeroFlag not supported"),
            Agu::NegativeFlag => panic_any("NegativeFlag not supported"),
            Agu::OverflowFlag => panic_any("OverflowFlag not supported"),
        }
    }

    fn is_register(&self) -> bool {
        match self {
            &Agu::RegisterA
            | &Agu::RegisterX
            | &Agu::RegisterY
            | &Agu::RegisterSP
            | &Agu::Status => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Transfer {
    src: Agu,
    dest: Agu,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TransferNoTouchFlags {
    src: Agu,
    dest: Agu,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Push(Agu);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Pop(Agu);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Dec(Agu);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Inc(Agu);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Adc(Agu);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Sbc(Agu);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct And(Agu);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Eor(Agu);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ora(Agu);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Asl(Agu);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Lsr(Agu);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Rol(Agu);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ror(Agu);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Clc();

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Cld();

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Cli();

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Clv();

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Sec();

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Sed();

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Sei();

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Cmp {
    register: Agu,
    memory: Agu,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ConditionBranch {
    offset: i8,
    register: Agu,
    negative: bool,
}

impl ConditionBranch {
    fn new(offset: i8, register: Agu, negative: bool) -> ConditionBranch {
        ConditionBranch {
            offset,
            register,
            negative,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Jmp(u16);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IndirectJmp(u16);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Jsr(u16);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Rts();

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Brk();

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Rti();

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Bit(Agu);

trait InstructionType {
    // (pcDelta, tickCount)
    fn execute(&self, cpu: &mut Cpu) -> u8;
}

/// Bin operate with A register and value from Agu, store result to A register.
trait AccumulatorOpInstructionType {
    fn agu(&self) -> Agu;
    fn op(a: u8, v: u8) -> u8;

    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let agu = self.agu();
        let (val, ticks) = cpu.get(agu);
        cpu.a = Self::op(cpu.a, val);
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        ticks + 2
    }
}

trait ShiftInstructionType {
    fn agu(&self) -> Agu;
    fn op(cpu: &Cpu, v: u8) -> (u8, bool);

    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let agu = self.agu();
        let (val, ticks) = cpu.get(agu);
        let (v, carry_flag) = Self::op(cpu, val);
        cpu.set_flag(CarryFlag, carry_flag);
        cpu.put(agu, v);
        cpu.update_negative_flag(v);
        cpu.update_zero_flag(v);
        ticks + 2
    }
}

impl InstructionType for Transfer {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let (val, ticks) = cpu.get(self.src);
        cpu.put(self.dest, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        ticks + 2
    }
}

impl InstructionType for TransferNoTouchFlags {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let (val, ticks) = cpu.get(self.src);
        cpu.put(self.dest, val);
        ticks + 3
    }
}

impl InstructionType for Push {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let (val, ..) = cpu.get(self.0);
        cpu.push_stack(val);
        3
    }
}

impl InstructionType for Pop {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let val = cpu.pop_stack();
        cpu.put(self.0, val);
        match self.0 {
            Agu::RegisterA => {
                cpu.update_negative_flag(val);
                cpu.update_zero_flag(val);
            }
            Agu::Status => {}
            _ => {
                panic!("Pop can only be used with A or Status")
            }
        }
        4
    }
}

impl InstructionType for Dec {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let (val, ticks) = cpu.get(self.0);
        let val = val.wrapping_sub(1);
        cpu.put(self.0, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        if self.0.is_register() {
            2
        } else {
            4 + ticks
        }
    }
}

impl InstructionType for Inc {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let (val, ticks) = cpu.get(self.0);
        let val = val.wrapping_add(1);
        cpu.put(self.0, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        if self.0.is_register() {
            2
        } else {
            4 + ticks
        }
    }
}

impl InstructionType for Adc {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let (val, ticks) = cpu.get(self.0);
        cpu.adc(val);
        ticks + 2
    }
}

impl InstructionType for Sbc {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let (val, ticks) = cpu.get(self.0);
        cpu.adc(!val);
        ticks + 2
    }
}

impl AccumulatorOpInstructionType for And {
    fn agu(&self) -> Agu {
        self.0
    }
    fn op(a: u8, v: u8) -> u8 {
        a & v
    }
}

impl AccumulatorOpInstructionType for Eor {
    fn agu(&self) -> Agu {
        self.0
    }
    fn op(a: u8, v: u8) -> u8 {
        a ^ v
    }
}

impl AccumulatorOpInstructionType for Ora {
    fn agu(&self) -> Agu {
        self.0
    }
    fn op(a: u8, v: u8) -> u8 {
        a | v
    }
}

impl ShiftInstructionType for Asl {
    fn agu(&self) -> Agu {
        self.0
    }

    fn op(_: &Cpu, v: u8) -> (u8, bool) {
        let carry_flag = v & 0x80 != 0;
        (v << 1, carry_flag)
    }
}

impl ShiftInstructionType for Lsr {
    fn agu(&self) -> Agu {
        self.0
    }

    fn op(_: &Cpu, v: u8) -> (u8, bool) {
        let carry_flag = v & 1 != 0;
        (v >> 1, carry_flag)
    }
}

impl ShiftInstructionType for Rol {
    fn agu(&self) -> Agu {
        self.0
    }

    fn op(cpu: &Cpu, v: u8) -> (u8, bool) {
        let carry_flag = v & 0x80 != 0;
        (v << 1 & 0xfe | cpu.flag(CarryFlag) as u8, carry_flag)
    }
}

impl ShiftInstructionType for Ror {
    fn agu(&self) -> Agu {
        self.0
    }

    fn op(cpu: &Cpu, v: u8) -> (u8, bool) {
        let carry_flag = v & 1 != 0;
        (
            v >> 1 & 0x7f | ((cpu.flag(CarryFlag) as u8) << 7),
            carry_flag,
        )
    }
}

impl InstructionType for Clc {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        cpu.set_flag(CarryFlag, false);
        2
    }
}

impl InstructionType for Cld {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        cpu.set_flag(DecimalModeFlag, false);
        2
    }
}

impl InstructionType for Cli {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        cpu.set_flag(InterruptDisableFlag, false);
        2
    }
}

impl InstructionType for Clv {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        cpu.set_flag(OverflowFlag, false);
        2
    }
}

impl InstructionType for Sec {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        cpu.set_flag(CarryFlag, true);
        2
    }
}

impl InstructionType for Sed {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        cpu.set_flag(DecimalModeFlag, true);
        2
    }
}

impl InstructionType for Sei {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        cpu.set_flag(InterruptDisableFlag, true);
        2
    }
}

impl InstructionType for Cmp {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let (reg_val, _) = cpu.get(self.register);
        let (val, ticks) = cpu.get(self.memory);
        let t = reg_val.wrapping_sub(val);
        cpu.update_negative_flag(t);
        cpu.update_zero_flag(t);
        cpu.set_flag(CarryFlag, reg_val >= val);
        ticks + 2
    }
}

impl InstructionType for ConditionBranch {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let (v, ..) = cpu.get(self.register);
        let v = if self.negative { v == 0 } else { v != 0 };
        if v {
            let pc = cpu.pc;
            let dest = ((pc as i32).wrapping_add(self.offset as i32)) as u16;
            cpu.pc = dest;
            plus_one_if_cross_page(3, pc, dest)
        } else {
            2
        }
    }
}

impl Jmp {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        cpu.pc = self.0;
        3
    }
}

impl IndirectJmp {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        cpu.pc = cpu.read_word(self.0);
        5
    }
}

impl Jsr {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let pc = cpu.pc - 1;
        cpu.push_stack((pc >> 8) as u8);
        cpu.push_stack(pc as u8);
        cpu.pc = self.0;
        6
    }
}

impl Rts {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let l = cpu.pop_stack();
        let h = cpu.pop_stack();
        cpu.pc = (h as u16) << 8 | l as u16 + 1;
        6
    }
}

impl Brk {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let pc = cpu.pc.wrapping_add(1);
        cpu.push_stack((pc >> 8) as u8);
        cpu.push_stack(pc as u8);
        let (status, ..) = cpu.get(Agu::Status);
        cpu.push_stack(status);
        cpu.set_flag(InterruptDisableFlag, true);
        cpu.pc = cpu.read_word(0xFFFE);
        7
    }
}

impl Rti {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let v = cpu.pop_stack();
        cpu.put(Agu::Status, v);
        cpu.pc = cpu.pop_stack() as u16 | (cpu.pop_stack() as u16) << 8;
        6
    }
}

impl InstructionType for Bit {
    fn execute(&self, cpu: &mut Cpu) -> u8 {
        let (v, ticks) = cpu.get(self.0);
        cpu.set_flag(NegativeFlag, (v & 0x80) != 0);
        cpu.set_flag(OverflowFlag, (v & 0x70) != 0);
        let v = v & cpu.a;
        cpu.update_zero_flag(v);
        ticks + 2
    }
}

fn execute_next(cpu: &mut Cpu) -> u8 {
    use addressing::*;
    use instruction::*;

    let op_code = cpu.inc_read_byte();
    let a = (op_code & 0b1110_0000) >> 5;
    let b = (op_code & 0b0001_1100) >> 2;
    let c = op_code & 0b0000_0011;

    let zero_page = |cpu: &mut Cpu| ZeroPage(cpu.inc_read_byte());
    let zero_page_x = |cpu: &mut Cpu| ZeroPageX(cpu.inc_read_byte());
    let zero_page_y = |cpu: &mut Cpu| ZeroPageY(cpu.inc_read_byte());
    let absolute = |cpu: &mut Cpu| Absolute(cpu.inc_read_word());
    let absolute_x = |cpu: &mut Cpu| AbsoluteX(cpu.inc_read_word());
    let absolute_y = |cpu: &mut Cpu| AbsoluteY(cpu.inc_read_word());
    let literal = |cpu: &mut Cpu| Literal(cpu.inc_read_byte());
    let cond_branch = |cpu: &mut Cpu, flag: Flag| {
        new_condition_branch(cpu.inc_read_byte() as i8, FlagAddr(flag), false)
    };
    let neg_cond_branch = |cpu: &mut Cpu, flag: Flag| {
        new_condition_branch(cpu.inc_read_byte() as i8, FlagAddr(flag), true)
    };
    let indirect_x = |cpu: &mut Cpu| IndirectX(cpu.inc_read_byte());
    let indirect_y = |cpu: &mut Cpu| IndirectY(cpu.inc_read_byte());
    let x = RegisterX;
    let y = RegisterY;
    let aa = RegisterA;
    let sp = RegisterSP;

    match (c, a, b) {
        (0, 0, 0) => new_brk()(cpu),
        (0, 0, 2) => new_push(RegisterStatus())(cpu),
        (0, 0, 4) => neg_cond_branch(cpu, Flag::Negative)(cpu),
        (0, 0, 6) => new_clear_bit(FlagAddr(Flag::Carry))(cpu),

        (0, 1, 0) => new_jsr(cpu.inc_read_word())(cpu),
        (0, 1, 1) => new_bit(zero_page(cpu))(cpu),
        (0, 1, 2) => new_pop(RegisterStatus())(cpu),
        (0, 1, 3) => new_bit(absolute(cpu))(cpu),
        (0, 1, 4) => cond_branch(cpu, Flag::Negative)(cpu),
        (0, 1, 6) => new_set_bit(FlagAddr(Flag::Carry))(cpu),

        (0, 2, 0) => new_rti()(cpu),
        (0, 2, 2) => new_push(aa())(cpu),
        (0, 2, 3) => new_jmp(cpu.inc_read_word())(cpu),
        (0, 2, 4) => neg_cond_branch(cpu, Flag::Overflow)(cpu),
        (0, 2, 6) => new_clear_bit(FlagAddr(Flag::Interrupt))(cpu),

        (0, 3, 0) => new_rts()(cpu),
        (0, 3, 2) => new_pop(aa())(cpu),
        (0, 3, 3) => new_indirect_jmp(cpu.inc_read_word())(cpu),
        (0, 3, 4) => cond_branch(cpu, Flag::Overflow)(cpu),
        (0, 3, 6) => new_set_bit(FlagAddr(Flag::Interrupt))(cpu),

        (0, 4, 1) => new_transfer_no_touch_flags(zero_page(cpu), y())(cpu),
        (0, 4, 2) => new_dec(y())(cpu),
        (0, 4, 3) => new_transfer_no_touch_flags(absolute(cpu), y())(cpu),
        (0, 4, 4) => neg_cond_branch(cpu, Flag::Carry)(cpu),
        (0, 4, 5) => new_transfer_no_touch_flags(zero_page_x(cpu), y())(cpu),
        (0, 4, 6) => new_transfer(aa(), y())(cpu),

        (0, 5, 0) => new_transfer(y(), literal(cpu))(cpu),
        (0, 5, 1) => new_transfer(y(), zero_page(cpu))(cpu),
        (0, 5, 2) => new_transfer(y(), aa())(cpu),
        (0, 5, 3) => new_transfer(y(), absolute(cpu))(cpu),
        (0, 5, 4) => cond_branch(cpu, Flag::Carry)(cpu),
        (0, 5, 5) => new_transfer(y(), zero_page_x(cpu))(cpu),
        (0, 5, 6) => new_clear_bit(FlagAddr(Flag::Overflow))(cpu),
        (0, 5, 7) => new_transfer(y(), absolute_x(cpu))(cpu),

        (0, 6, 0) => new_cmp(y(), literal(cpu))(cpu),
        (0, 6, 1) => new_cmp(y(), zero_page(cpu))(cpu),
        (0, 6, 2) => new_inc(y())(cpu),
        (0, 6, 3) => new_cmp(y(), absolute(cpu))(cpu),
        (0, 6, 4) => neg_cond_branch(cpu, Flag::Zero)(cpu),
        (0, 6, 6) => new_clear_bit(FlagAddr(Flag::Decimal))(cpu),

        (0, 7, 0) => new_cmp(x(), literal(cpu))(cpu),
        (0, 7, 1) => new_cmp(x(), zero_page(cpu))(cpu),
        (0, 7, 2) => new_inc(x())(cpu),
        (0, 7, 3) => new_cmp(x(), absolute(cpu))(cpu),
        (0, 7, 4) => cond_branch(cpu, Flag::Zero)(cpu),
        (0, 7, 6) => new_set_bit(FlagAddr(Flag::Decimal))(cpu),

        (1, 0, 0) => new_ora(indirect_x(cpu))(cpu),
        (1, 0, 1) => new_ora(zero_page(cpu))(cpu),
        (1, 0, 2) => new_ora(literal(cpu))(cpu),
        (1, 0, 3) => new_ora(absolute(cpu))(cpu),
        (1, 0, 4) => new_ora(indirect_y(cpu))(cpu),
        (1, 0, 5) => new_ora(zero_page_x(cpu))(cpu),
        (1, 0, 6) => new_ora(absolute_y(cpu))(cpu),
        (1, 0, 7) => new_ora(absolute_x(cpu))(cpu),

        (1, 1, 0) => new_and(indirect_x(cpu))(cpu),
        (1, 1, 1) => new_and(zero_page(cpu))(cpu),
        (1, 1, 2) => new_and(literal(cpu))(cpu),
        (1, 1, 3) => new_and(absolute(cpu))(cpu),
        (1, 1, 4) => new_and(indirect_y(cpu))(cpu),
        (1, 1, 5) => new_and(zero_page_x(cpu))(cpu),
        (1, 1, 6) => new_and(absolute_y(cpu))(cpu),
        (1, 1, 7) => new_and(absolute_x(cpu))(cpu),

        (1, 2, 0) => new_eor(indirect_x(cpu))(cpu),
        (1, 2, 1) => new_eor(zero_page(cpu))(cpu),
        (1, 2, 2) => new_eor(literal(cpu))(cpu),
        (1, 2, 3) => new_eor(absolute(cpu))(cpu),
        (1, 2, 4) => new_eor(indirect_y(cpu))(cpu),
        (1, 2, 5) => new_eor(zero_page_x(cpu))(cpu),
        (1, 2, 6) => new_eor(absolute_y(cpu))(cpu),
        (1, 2, 7) => new_eor(absolute_x(cpu))(cpu),

        (1, 3, 0) => new_adc(indirect_x(cpu))(cpu),
        (1, 3, 1) => new_adc(zero_page(cpu))(cpu),
        (1, 3, 2) => new_adc(literal(cpu))(cpu),
        (1, 3, 3) => new_adc(absolute(cpu))(cpu),
        (1, 3, 4) => new_adc(indirect_y(cpu))(cpu),
        (1, 3, 5) => new_adc(zero_page_x(cpu))(cpu),
        (1, 3, 6) => new_adc(absolute_y(cpu))(cpu),
        (1, 3, 7) => new_adc(absolute_x(cpu))(cpu),

        (1, 4, 0) => new_transfer_no_touch_flags(indirect_x(cpu), aa())(cpu),
        (1, 4, 1) => new_transfer_no_touch_flags(zero_page(cpu), aa())(cpu),
        (1, 4, 3) => new_transfer_no_touch_flags(absolute(cpu), aa())(cpu),
        (1, 4, 4) => new_transfer_no_touch_flags(indirect_y(cpu), aa())(cpu),
        (1, 4, 5) => new_transfer_no_touch_flags(zero_page_x(cpu), aa())(cpu),
        (1, 4, 6) => new_transfer_no_touch_flags(absolute_y(cpu), aa())(cpu),
        (1, 4, 7) => new_transfer_no_touch_flags(absolute_x(cpu), aa())(cpu),

        (1, 5, 0) => new_transfer(aa(), indirect_x(cpu))(cpu),
        (1, 5, 1) => new_transfer(aa(), zero_page(cpu))(cpu),
        (1, 5, 2) => new_transfer(aa(), literal(cpu))(cpu),
        (1, 5, 3) => new_transfer(aa(), absolute(cpu))(cpu),
        (1, 5, 4) => new_transfer(aa(), indirect_y(cpu))(cpu),
        (1, 5, 5) => new_transfer(aa(), zero_page_x(cpu))(cpu),
        (1, 5, 6) => new_transfer(aa(), absolute_y(cpu))(cpu),
        (1, 5, 7) => new_transfer(aa(), absolute_x(cpu))(cpu),

        (1, 6, 0) => new_cmp(aa(), indirect_x(cpu))(cpu),
        (1, 6, 1) => new_cmp(aa(), zero_page(cpu))(cpu),
        (1, 6, 2) => new_cmp(aa(), literal(cpu))(cpu),
        (1, 6, 3) => new_cmp(aa(), absolute(cpu))(cpu),
        (1, 6, 4) => new_cmp(aa(), indirect_y(cpu))(cpu),
        (1, 6, 5) => new_cmp(aa(), zero_page_x(cpu))(cpu),
        (1, 6, 6) => new_cmp(aa(), absolute_y(cpu))(cpu),
        (1, 6, 7) => new_cmp(aa(), absolute_x(cpu))(cpu),

        (1, 7, 0) => new_sbc(indirect_x(cpu))(cpu),
        (1, 7, 1) => new_sbc(zero_page(cpu))(cpu),
        (1, 7, 2) => new_sbc(literal(cpu))(cpu),
        (1, 7, 3) => new_sbc(absolute(cpu))(cpu),
        (1, 7, 4) => new_sbc(indirect_y(cpu))(cpu),
        (1, 7, 5) => new_sbc(zero_page_x(cpu))(cpu),
        (1, 7, 6) => new_sbc(absolute_y(cpu))(cpu),
        (1, 7, 7) => new_sbc(absolute_x(cpu))(cpu),

        (2, 0, 1) => new_asl(zero_page(cpu))(cpu),
        (2, 0, 2) => new_asl(aa())(cpu),
        (2, 0, 3) => new_asl(absolute(cpu))(cpu),
        (2, 0, 5) => new_asl(zero_page_x(cpu))(cpu),
        (2, 0, 7) => new_asl(absolute_x(cpu))(cpu),

        (2, 1, 1) => new_rol(zero_page(cpu))(cpu),
        (2, 1, 2) => new_rol(aa())(cpu),
        (2, 1, 3) => new_rol(absolute(cpu))(cpu),
        (2, 1, 5) => new_rol(zero_page_x(cpu))(cpu),
        (2, 1, 7) => new_rol(absolute_x(cpu))(cpu),

        (2, 2, 1) => new_lsr(zero_page(cpu))(cpu),
        (2, 2, 2) => new_lsr(aa())(cpu),
        (2, 2, 3) => new_lsr(absolute(cpu))(cpu),
        (2, 2, 5) => new_lsr(zero_page_x(cpu))(cpu),
        (2, 2, 7) => new_lsr(absolute_x(cpu))(cpu),

        (2, 3, 1) => new_ror(zero_page(cpu))(cpu),
        (2, 3, 2) => new_ror(aa())(cpu),
        (2, 3, 3) => new_ror(absolute(cpu))(cpu),
        (2, 3, 5) => new_ror(zero_page_x(cpu))(cpu),
        (2, 3, 7) => new_ror(absolute_x(cpu))(cpu),

        (2, 4, 1) => new_transfer_no_touch_flags(zero_page(cpu), x())(cpu),
        (2, 4, 2) => new_transfer(aa(), x())(cpu),
        (2, 4, 3) => new_transfer_no_touch_flags(absolute(cpu), x())(cpu),
        (2, 4, 5) => new_transfer_no_touch_flags(zero_page_y(cpu), x())(cpu),
        (2, 4, 6) => new_transfer_no_touch_flags(sp(), x())(cpu),

        (2, 5, 0) => new_transfer(x(), literal(cpu))(cpu),
        (2, 5, 1) => new_transfer(x(), zero_page(cpu))(cpu),
        (2, 5, 2) => new_transfer(x(), aa())(cpu),
        (2, 5, 3) => new_transfer(x(), absolute(cpu))(cpu),
        (2, 5, 5) => new_transfer(x(), zero_page_y(cpu))(cpu),
        (2, 5, 6) => new_transfer(x(), sp())(cpu),
        (2, 5, 7) => new_transfer(x(), absolute_y(cpu))(cpu),

        (2, 6, 1) => new_dec(zero_page(cpu))(cpu),
        (2, 6, 2) => new_dec(x())(cpu),
        (2, 6, 3) => new_dec(absolute(cpu))(cpu),
        (2, 6, 5) => new_dec(zero_page_x(cpu))(cpu),
        (2, 6, 7) => new_dec(absolute_x(cpu))(cpu),

        (2, 7, 1) => new_inc(zero_page(cpu))(cpu),
        (2, 7, 2) => new_nop()(cpu),
        (2, 7, 3) => new_inc(absolute(cpu))(cpu),
        (2, 7, 5) => new_inc(zero_page_x(cpu))(cpu),
        (2, 7, 7) => new_inc(absolute_x(cpu))(cpu),

        _ => panic!("Unknown opcode: {:02x} @ {:04x}", op_code, cpu.pc),
    }
}

pub trait FlagBit {
    const BIT: u8;
}

pub struct CarryFlag;

pub struct DecimalModeFlag;

pub struct InterruptDisableFlag;

pub struct ZeroFlag;

pub struct BreakFlag;

pub struct OverflowFlag;

pub struct NegativeFlag;

impl FlagBit for NegativeFlag {
    const BIT: u8 = 0x80;
}

impl FlagBit for OverflowFlag {
    const BIT: u8 = 0x40;
}

impl FlagBit for BreakFlag {
    const BIT: u8 = 0x10;
}

impl FlagBit for DecimalModeFlag {
    const BIT: u8 = 0x8;
}

impl FlagBit for InterruptDisableFlag {
    const BIT: u8 = 0x4;
}

impl FlagBit for CarryFlag {
    const BIT: u8 = 0x1;
}

impl FlagBit for ZeroFlag {
    const BIT: u8 = 0x2;
}

// Trait to sync instruction execution  times.
pub trait Plugin {
    fn start(&mut self, cpu: &Cpu);

    // return true to stop cpu
    fn end(&mut self, cpu: &Cpu);
}

pub trait Mcu {
    fn read(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, value: u8);
}

#[allow(dead_code)]
pub struct Cpu {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub status: u8,
    mcu: Box<dyn Mcu>,

    /// if remains_clock not zero, new instruction will not be executed.
    remain_clocks: u16,
}

impl Cpu {
    pub fn reset(&mut self) {
        self.pc = self.read_word(0xFFFC);
    }

    pub fn new(mcu: Box<dyn Mcu>) -> Cpu {
        Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0,
            status: 0,
            mcu,
            remain_clocks: 0,
        }
    }

    pub fn flag<T: FlagBit>(&self, _: T) -> bool {
        (self.status & T::BIT) != 0
    }

    fn set_flag<T: FlagBit>(&mut self, _: T, value: bool) {
        if value {
            self.status |= T::BIT;
        } else {
            self.status &= !T::BIT;
        }
    }

    pub fn clock_tick<T: Plugin>(&mut self, plugin: &mut T) {
        if self.remain_clocks != 0 {
            self.remain_clocks -= 1;
            return;
        }

        plugin.start(self);
        self.remain_clocks = execute_next(self) as u16 - 1;
        plugin.end(self);
    }

    fn adc(&mut self, val: u8) {
        // https://stackoverflow.com/a/29193951/1305678
        let t = self.a as u16 + val as u16 + self.flag(CarryFlag) as u16;
        self.set_flag(
            OverflowFlag,
            (self.a ^ (t as u8)) & (val ^ (t as u8)) & 0x80 == 0x80,
        );
        self.set_flag(CarryFlag, t & 0x100 == 0x100);
        self.update_negative_flag(t);
        self.a = t as u8;
        self.update_zero_flag(self.a);
    }

    fn inc_pc(&mut self, delta: i8) {
        self.pc = self.pc.wrapping_add(delta as u16);
    }

    fn update_negative_flag<T: BitAnd<Output = T> + Copy + From<u8> + PartialEq + Default>(
        &mut self,
        value: T,
    ) {
        self.set_flag(NegativeFlag, value & T::from(0x80) != T::default());
    }

    fn update_zero_flag<T: PartialEq + Copy + Default>(&mut self, value: T) {
        self.set_flag(ZeroFlag, value == T::default());
    }

    fn read_byte(&self, addr: u16) -> u8 {
        self.mcu.read(addr)
    }

    fn inc_read_byte(&mut self) -> u8 {
        let addr = self.pc;
        self.inc_pc(1);
        self.read_byte(addr)
    }

    fn inc_read_word(&mut self) -> u16 {
        let addr = self.pc;
        self.inc_pc(2);
        self.read_word(addr)
    }

    fn write_byte(&mut self, addr: u16, value: u8) {
        self.mcu.write(addr, value);
    }

    fn read_word(&self, addr: u16) -> u16 {
        (self.read_byte(addr) as u16) | ((self.read_byte(addr.wrapping_add(1)) as u16) << 8)
    }

    fn read_zero_page_word(&self, addr: u8) -> u16 {
        (self.read_byte(addr as u16) as u16)
            | ((self.read_byte(addr.wrapping_add(1) as u16) as u16) << 8)
    }

    fn get2<A: Address>(&self, addr: &A) -> (u8, u8) {
        addr.get(self)
    }

    fn put2<A: Address>(&mut self, addr: &A, value: u8) -> u8 {
        addr.set(self, value)
    }

    /// Return (value, operand bytes, address ticks)
    fn get(&self, agu: Agu) -> (u8, u8) {
        match agu {
            Agu::Literal(val) => (val, 0),
            Agu::RegisterA => (self.a, 0),
            Agu::RegisterX => (self.x, 0),
            Agu::RegisterY => (self.y, 0),
            Agu::RegisterSP => (self.sp, 0),
            Agu::Status => (self.status | 0b0011_0000, 0),
            Agu::CarryFlag => (self.flag(CarryFlag) as u8, 0),
            Agu::ZeroFlag => (self.flag(ZeroFlag) as u8, 0),
            Agu::NegativeFlag => (self.flag(NegativeFlag) as u8, 0),
            Agu::OverflowFlag => (self.flag(OverflowFlag) as u8, 0),
            _ => {
                let (addr, ticks) = agu.address(self);
                (self.read_byte(addr), ticks)
            }
        }
    }

    fn put(&mut self, agu: Agu, value: u8) {
        match agu {
            Agu::Literal(_) => panic_any("Literal not supported"),
            Agu::RegisterA => self.a = value,
            Agu::RegisterX => self.x = value,
            Agu::RegisterY => self.y = value,
            Agu::RegisterSP => self.sp = value,
            Agu::Status => self.status = value & 0b1100_1111,
            _ => {
                let (addr, _) = agu.address(self);
                self.write_byte(addr, value);
            }
        }
    }

    fn push_stack(&mut self, value: u8) {
        self.write_byte(0x100 + self.sp as u16, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn pop_stack(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.read_byte(0x100 + self.sp as u16)
    }

    pub fn peek_stack(&self) -> u8 {
        let addr = 0x100 + self.sp.wrapping_add(1) as u16;
        self.read_byte(addr)
    }
}
