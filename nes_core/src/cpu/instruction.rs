use super::addressing::{Address, RegisterStatus};
use super::{extra_tick_if_cross_page, Cpu, Flag};
use crate::cpu::addressing::{Addressing, FlagAddr, Literal};
use crate::mcu::Mcu;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Instruction {
    // Arithmetic / logic
    Adc(Addressing),
    Sbc(Addressing),
    And(Addressing),
    Ora(Addressing),
    Eor(Addressing),

    // Immediate/accumulator unofficial ops
    Anc(Literal),
    Alr(Literal),
    Arr(Literal),
    Sbx(Literal),

    // Shifts/rotates
    Asl(Addressing),
    Lsr(Addressing),
    Rol(Addressing),
    Ror(Addressing),

    // Load / store / transfer
    Transfer(Addressing, Addressing),
    TransferNoTouchFlags(Addressing, Addressing),

    // Stack
    Pha,
    Php,
    Pla,
    Plp,

    // Inc / Dec
    Inc(Addressing),
    Dec(Addressing),

    // Compare / Bit / Branches
    Cmp(Addressing, Addressing),
    Bit(Addressing),
    ConditionBranch(i8, FlagAddr, bool),

    // Jumps / calls / returns / interrupts
    Jmp(u16),
    IndirectJmp(u16),
    Jsr(u16),
    Rts,
    Brk,
    Rti,

    // Flags
    ClearBit(FlagAddr),
    SetBit(FlagAddr),

    // Misc
    Nop,
    NopWithAddr(Addressing),
    Hlt,

    // Unofficial combination ops
    Aso(Addressing),
    Rla(Addressing),
    Lse(Addressing),
    Rra(Addressing),
    Sax(Addressing),
    Lax(Addressing),
    Dcp(Addressing),
    Isc(Addressing),
    All(Addressing, Addressing),
    Ane(Literal),
    NopStore(Addressing),
}

impl Instruction {
    /// Execute the instruction, and returns cycle this instruction takes.
    pub fn exec<M: Mcu>(self, cpu: &mut Cpu<M>) -> u8 {
        match self {
            Instruction::Adc(addressing) => {
                let (val, ticks) = addressing.read(cpu);
                cpu.adc(val);
                1 + ticks
            }
            Instruction::Sbc(addressing) => {
                let (val, ticks) = addressing.read(cpu);
                cpu.adc(!val);
                1 + ticks
            }
            Instruction::And(addressing) => {
                let (val, ticks) = addressing.read(cpu);
                cpu.a &= val;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                1 + ticks
            }
            Instruction::Ora(addressing) => {
                let (val, ticks) = addressing.read(cpu);
                cpu.a |= val;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                1 + ticks
            }
            Instruction::Eor(addressing) => {
                let (val, ticks) = addressing.read(cpu);
                cpu.a ^= val;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                1 + ticks
            }

            Instruction::Anc(Literal(v)) => {
                cpu.a &= v;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                cpu.set_flag(Flag::Carry, cpu.a & 0x80 != 0);
                2
            }
            Instruction::Alr(Literal(v)) => {
                cpu.a &= v;
                cpu.set_flag(Flag::Carry, cpu.a & 0x01 != 0);
                cpu.a >>= 1;
                cpu.update_zero_flag(cpu.a);
                cpu.set_flag(Flag::Negative, false);
                2
            }
            Instruction::Arr(Literal(v)) => {
                cpu.a &= v;
                cpu.a = cpu.a >> 1 | (cpu.flag(Flag::Carry) as u8) << 7;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                cpu.set_flag(Flag::Carry, cpu.a & 0x40 != 0);
                cpu.set_flag(Flag::Overflow, ((cpu.a >> 6) ^ (cpu.a >> 5)) & 1 != 0);
                2
            }
            Instruction::Sbx(Literal(v)) => {
                let val = cpu.a & cpu.x;
                let (x, borrow) = val.overflowing_sub(v);
                cpu.x = x;
                cpu.update_negative_flag(x);
                cpu.update_zero_flag(x);
                cpu.set_flag(Flag::Carry, !borrow);
                2
            }

            Instruction::Asl(addressing) => match addressing {
                Addressing::Accumulator | Addressing::RegisterA => {
                    let v = cpu.a;
                    cpu.set_flag(Flag::Carry, v & 0x80 != 0);
                    cpu.a = v << 1;
                    cpu.update_negative_flag(cpu.a);
                    cpu.update_zero_flag(cpu.a);
                    2
                }
                _ => {
                    let (v, _ticks) = addressing.read(cpu);
                    let t = v << 1;
                    let ticks_set = addressing.write(cpu, t);
                    cpu.update_negative_flag(t);
                    cpu.update_zero_flag(t);
                    if addressing.is_register() {
                        2
                    } else {
                        ticks_set + 3
                    }
                }
            },
            Instruction::Lsr(addressing) => match addressing {
                Addressing::Accumulator | Addressing::RegisterA => {
                    let v = cpu.a;
                    cpu.set_flag(Flag::Carry, v & 0x01 != 0);
                    cpu.a = v >> 1;
                    cpu.update_zero_flag(cpu.a);
                    cpu.set_flag(Flag::Negative, false);
                    2
                }
                _ => {
                    let (v, _ticks) = addressing.read(cpu);
                    cpu.set_flag(Flag::Carry, v & 0x01 != 0);
                    let t = v >> 1;
                    let ticks_set = addressing.write(cpu, t);
                    cpu.update_negative_flag(t);
                    cpu.update_zero_flag(t);
                    if addressing.is_register() {
                        2
                    } else {
                        ticks_set + 3
                    }
                }
            },
            Instruction::Rol(addressing) => match addressing {
                Addressing::Accumulator | Addressing::RegisterA => {
                    let v = cpu.a;
                    let new = (v << 1) | (cpu.flag(Flag::Carry) as u8);
                    cpu.set_flag(Flag::Carry, v & 0x80 != 0);
                    cpu.a = new;
                    cpu.update_negative_flag(cpu.a);
                    cpu.update_zero_flag(cpu.a);
                    2
                }
                _ => {
                    let (v, _ticks) = addressing.read(cpu);
                    let new = (v << 1) | (cpu.flag(Flag::Carry) as u8);
                    let ticks_set = addressing.write(cpu, new);
                    cpu.set_flag(Flag::Carry, v & 0x80 != 0);
                    cpu.update_negative_flag(new);
                    cpu.update_zero_flag(new);
                    if addressing.is_register() {
                        2
                    } else {
                        ticks_set + 3
                    }
                }
            },
            Instruction::Ror(addressing) => match addressing {
                Addressing::Accumulator | Addressing::RegisterA => {
                    let v = cpu.a;
                    let new = (v >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7);
                    cpu.set_flag(Flag::Carry, v & 0x01 != 0);
                    cpu.a = new;
                    cpu.update_negative_flag(cpu.a);
                    cpu.update_zero_flag(cpu.a);
                    2
                }
                _ => {
                    let (v, _ticks) = addressing.read(cpu);
                    let new = (v >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7);
                    let ticks_set = addressing.write(cpu, new);
                    cpu.set_flag(Flag::Carry, v & 0x01 != 0);
                    cpu.update_negative_flag(new);
                    cpu.update_zero_flag(new);
                    if addressing.is_register() {
                        2
                    } else {
                        ticks_set + 3
                    }
                }
            },

            Instruction::Transfer(dest, src) => {
                let (val, ticks_src) = src.read(cpu);
                let ticks_dst = dest.write(cpu, val);
                cpu.update_negative_flag(val);
                cpu.update_zero_flag(val);
                1 + ticks_src + ticks_dst
            }
            Instruction::TransferNoTouchFlags(dest, src) => {
                let (val, ticks_src) = src.read(cpu);
                let ticks_dst = dest.write(cpu, val);
                1 + ticks_src + ticks_dst
            }

            Instruction::Pha => {
                cpu.push_stack(cpu.a);
                3
            }
            Instruction::Php => {
                cpu.set_flag(Flag::Break, true);
                cpu.push_status();
                3
            }
            Instruction::Pla => {
                cpu.a = cpu.pop_stack();
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                4
            }
            Instruction::Plp => {
                let saved = cpu.pop_stack();
                let saved_break_flag = cpu.flag(Flag::Break);
                let cur_i_flag = cpu.flag(Flag::InterruptDisabled);
                let save_i_flag = (saved & Flag::InterruptDisabled as u8) != 0;
                cpu.status = saved;
                cpu.set_flag(Flag::Break, saved_break_flag);
                cpu.set_flag(Flag::InterruptDisabled, cur_i_flag);
                cpu.pending_set_interrupt_disabled_flag(save_i_flag);
                4
            }

            Instruction::Inc(addressing) => match addressing {
                Addressing::Accumulator | Addressing::RegisterA => {
                    cpu.a = cpu.a.wrapping_add(1);
                    cpu.update_negative_flag(cpu.a);
                    cpu.update_zero_flag(cpu.a);
                    2
                }
                _ => {
                    let (val, _r) = addressing.read(cpu);
                    let val = val.wrapping_add(1);
                    let ticks_set = addressing.write(cpu, val);
                    cpu.update_negative_flag(val);
                    cpu.update_zero_flag(val);
                    if addressing.is_register() {
                        2
                    } else {
                        ticks_set + 3
                    }
                }
            },
            Instruction::Dec(addressing) => match addressing {
                Addressing::Accumulator | Addressing::RegisterA => {
                    cpu.a = cpu.a.wrapping_sub(1);
                    cpu.update_negative_flag(cpu.a);
                    cpu.update_zero_flag(cpu.a);
                    2
                }
                _ => {
                    let (val, _r) = addressing.read(cpu);
                    let val = val.wrapping_sub(1);
                    let ticks_set = addressing.write(cpu, val);
                    cpu.update_negative_flag(val);
                    cpu.update_zero_flag(val);
                    if addressing.is_register() {
                        2
                    } else {
                        ticks_set + 3
                    }
                }
            },

            Instruction::Cmp(r, m) => {
                let (r_val, _r_ticks) = r.read(cpu);
                let (val, ticks) = m.read(cpu);
                let t = r_val.wrapping_sub(val);
                cpu.update_negative_flag(t);
                cpu.update_zero_flag(t);
                cpu.set_flag(Flag::Carry, r_val >= val);
                1 + ticks
            }
            Instruction::Bit(dest) => {
                let (v, ticks) = dest.read(cpu);
                cpu.set_flag(Flag::Negative, (v & 0x80) != 0);
                cpu.set_flag(Flag::Overflow, (v & 0x40) != 0);
                cpu.update_zero_flag(v & cpu.a);
                1 + ticks
            }
            Instruction::ConditionBranch(offset, flagaddr, negative) => {
                let (v, _ticks) = flagaddr.get(cpu);
                let should_jump = if negative { v == 0 } else { v != 0 };
                if should_jump {
                    let pc = cpu.pc;
                    let dest = ((pc as i16).wrapping_add(offset as i16)) as u16;
                    cpu.pc = dest;
                    3 + extra_tick_if_cross_page(pc, dest)
                } else {
                    2
                }
            }

            Instruction::Jmp(addr) => {
                cpu.pc = addr;
                3
            }
            Instruction::IndirectJmp(offset) => {
                cpu.pc = cpu.read_word_in_same_page(offset);
                5
            }
            Instruction::Jsr(addr) => {
                cpu.pc = cpu.pc.wrapping_sub(1);
                cpu.push_pc();
                cpu.pc = addr;
                6
            }
            Instruction::Rts => {
                let l = cpu.pop_stack();
                let h = cpu.pop_stack();
                cpu.pc = ((h as u16) << 8) | (l as u16);
                cpu.inc_pc(1);
                6
            }
            Instruction::Brk => {
                cpu.pc = cpu.pc.wrapping_add(1);
                cpu.push_pc();
                cpu.set_flag(Flag::Break, true);
                cpu.push_status();
                cpu.set_flag(Flag::InterruptDisabled, true);
                cpu.pc = cpu.read_word(0xFFFE);
                7
            }
            Instruction::Rti => {
                let v = cpu.pop_stack();
                RegisterStatus().set(cpu, v);
                cpu.pc = cpu.pop_stack() as u16 | ((cpu.pop_stack() as u16) << 8);
                6
            }

            Instruction::ClearBit(dest) => {
                if dest.0 == Flag::InterruptDisabled {
                    cpu.pending_set_interrupt_disabled_flag(false);
                } else {
                    dest.set(cpu, 0);
                }
                2
            }
            Instruction::SetBit(dest) => {
                if dest.0 == Flag::InterruptDisabled {
                    cpu.pending_set_interrupt_disabled_flag(true);
                } else {
                    dest.set(cpu, 1);
                }
                2
            }

            Instruction::Nop => 2,
            Instruction::NopWithAddr(dest) => {
                let (_v, ticks) = dest.read(cpu);
                1 + ticks
            }
            Instruction::Hlt => {
                cpu.halt();
                2
            }

            Instruction::Aso(dest) => {
                let (v, _ticks) = dest.read(cpu);
                cpu.set_flag(Flag::Carry, (v & 0x80) != 0);
                let t = v << 1;
                let ticks_set = dest.write(cpu, t);
                cpu.a |= t;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                ticks_set + 3
            }
            Instruction::Rla(dest) => {
                let (v, _ticks) = dest.read(cpu);
                let carry = v & 0x80 != 0;
                let t = (v << 1) | cpu.flag(Flag::Carry) as u8;
                let ticks_set = dest.write(cpu, t);
                cpu.set_flag(Flag::Carry, carry);
                cpu.a &= t;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                ticks_set + 3
            }
            Instruction::Lse(dest) => {
                let (v, _ticks) = dest.read(cpu);
                cpu.set_flag(Flag::Carry, (v & 0x01) != 0);
                let t = v >> 1;
                let ticks_set = dest.write(cpu, t);
                cpu.a ^= t;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                ticks_set + 3
            }
            Instruction::Rra(dest) => {
                let (v, _ticks) = dest.read(cpu);
                let carry = v & 0x01 != 0;
                let v = (v >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7);
                let ticks_set = dest.write(cpu, v);
                cpu.set_flag(Flag::Carry, carry);
                cpu.update_negative_flag(v);
                cpu.adc(v);
                ticks_set + 3
            }
            Instruction::Sax(dest) => {
                let v = cpu.a & cpu.x;
                let ticks = dest.write(cpu, v);
                1 + ticks
            }
            Instruction::Lax(dest) => {
                let (v, ticks) = dest.read(cpu);
                cpu.a = v;
                cpu.x = v;
                cpu.update_negative_flag(v);
                cpu.update_zero_flag(v);
                1 + ticks
            }
            Instruction::Dcp(dest) => {
                let (v, _ticks) = dest.read(cpu);
                let t = v.wrapping_sub(1);
                let ticks_set = dest.write(cpu, t);
                let (t2, borrow) = cpu.a.overflowing_sub(t);
                cpu.update_negative_flag(t2);
                cpu.update_zero_flag(t2);
                cpu.set_flag(Flag::Carry, !borrow);
                ticks_set + 3
            }
            Instruction::Isc(dest) => {
                let (v, _ticks) = dest.read(cpu);
                let t = v.wrapping_add(1);
                let ticks_set = dest.write(cpu, t);
                cpu.adc(!t);
                ticks_set + 3
            }
            Instruction::Ane(Literal(v)) => {
                cpu.a = cpu.x & v;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                2
            }
            Instruction::NopStore(dest) => {
                let ticks = dest.write(cpu, cpu.a);
                1 + ticks
            }
            Instruction::All(r, dest) => {
                let (addr, _) = dest.calc_addr(cpu);
                let r = match r {
                    Addressing::RegisterX => cpu.x,
                    Addressing::RegisterY => cpu.y,
                    _ => panic!("ALL unsupported source: {:?}", r),
                };
                let v = r & ((addr >> 8) as u8).wrapping_add(1);
                let addr = (addr & 0xff) | ((v as u16) << 8);
                cpu.write_byte(addr, v);
                5
            }
        }
    }
}

/// LDA, LDX, LDY, TAX, TAY, TSX, TXA, TYA
pub fn new_transfer<M: Mcu, S, D>(dest: D, src: S) -> impl FnMut(&mut Cpu<M>) -> u8
where
    S: Address<M>,
    D: Address<M>,
{
    move |cpu| {
        let (val, ticks) = src.get(cpu);
        let ticks2 = dest.set(cpu, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        ticks + ticks2
    }
}

// STA, STX, STY
pub fn new_transfer_no_touch_flags<M: Mcu, S, D>(dest: D, src: S) -> impl FnMut(&mut Cpu<M>) -> u8
where
    S: Address<M>,
    D: Address<M>,
{
    move |cpu| {
        let (val, ticks) = src.get(cpu);
        let ticks2 = dest.set(cpu, val);
        ticks + ticks2
    }
}

pub fn new_pha<M: Mcu>() -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        cpu.push_stack(cpu.a);
        3
    }
}

pub fn new_php<M: Mcu>() -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        cpu.set_flag(Flag::Break, true);
        cpu.push_status();
        3
    }
}

pub fn new_pla<M: Mcu>() -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        cpu.a = cpu.pop_stack();
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        4
    }
}

pub fn new_plp<M: Mcu>() -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let saved = cpu.pop_stack();
        let saved_break_flag = cpu.flag(Flag::Break);
        let cur_i_flag = cpu.flag(Flag::InterruptDisabled);
        let save_i_flag = (saved & Flag::InterruptDisabled as u8) != 0;
        cpu.status = saved;
        cpu.set_flag(Flag::Break, saved_break_flag);
        cpu.set_flag(Flag::InterruptDisabled, cur_i_flag);
        cpu.pending_set_interrupt_disabled_flag(save_i_flag);
        4
    }
}

// DEC, DEX, DEY
pub fn new_dec<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (val, _) = dest.get(cpu);
        let val = val.wrapping_sub(1);
        let ticks_set = dest.set(cpu, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        if dest.is_register() {
            2
        } else {
            ticks_set + 3
        }
    }
}

// INC, INX, INY
pub fn new_inc<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (val, _) = dest.get(cpu);
        let val = val.wrapping_add(1);
        let ticks_set = dest.set(cpu, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        if dest.is_register() {
            2
        } else {
            ticks_set + 3
        }
    }
}

pub fn new_adc<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (val, ticks) = dest.get(cpu);
        cpu.adc(val);
        1 + ticks
    }
}

pub fn new_sbc<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (val, ticks) = dest.get(cpu);
        cpu.adc(!val);
        1 + ticks
    }
}

fn new_acc_op<M: Mcu, F: Fn(u8, u8) -> u8, S: Address<M>>(
    _op_name: &str,
    op: F,
    src: S,
) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (val, ticks) = src.get(cpu);
        cpu.a = op(cpu.a, val);
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        1 + ticks
    }
}

pub fn new_and<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    new_acc_op("and", |a, b| a & b, dest)
}

pub fn new_anc<M: Mcu>(Literal(v): Literal) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        cpu.a &= v;
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        cpu.set_flag(Flag::Carry, cpu.a & 0x80 != 0);
        2
    }
}

pub fn new_alr<M: Mcu>(Literal(v): Literal) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        cpu.a &= v;
        cpu.set_flag(Flag::Carry, cpu.a & 0x01 != 0);
        cpu.a >>= 1;
        cpu.update_zero_flag(cpu.a);
        cpu.set_flag(Flag::Negative, false);
        2
    }
}

pub fn new_arr<M: Mcu>(Literal(v): Literal) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        cpu.a &= v;
        cpu.a = cpu.a >> 1 | (cpu.flag(Flag::Carry) as u8) << 7;
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        cpu.set_flag(Flag::Carry, cpu.a & 0x40 != 0);
        cpu.set_flag(Flag::Overflow, ((cpu.a >> 6) ^ (cpu.a >> 5)) & 1 != 0);
        2
    }
}

pub fn new_sbx<M: Mcu>(Literal(v): Literal) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let val = cpu.a & cpu.x;
        let (x, borrow) = val.overflowing_sub(v);
        cpu.x = x;
        cpu.update_negative_flag(x);
        cpu.update_zero_flag(x);
        cpu.set_flag(Flag::Carry, !borrow);
        2
    }
}

pub fn new_eor<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    new_acc_op("eor", |a, b| a ^ b, dest)
}

pub fn new_ora<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    new_acc_op("ora", |a, b| a | b, dest)
}

fn new_shift_op<M: Mcu, F: Fn(&Cpu<M>, u8) -> (u8, u8), D: Address<M>>(
    _op_name: &str,
    op: F,
    dest: D,
) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (val, _) = dest.get(cpu);
        let (val, carry_flag) = op(cpu, val);
        cpu.set_flag(Flag::Carry, carry_flag != 0);
        let ticks_set = dest.set(cpu, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        if dest.is_register() {
            2
        } else {
            ticks_set + 3
        }
    }
}

pub fn new_asl<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    new_shift_op("asl", |_, v| (v << 1, v & 0x80), dest)
}

pub fn new_lsr<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    new_shift_op("lsr", |_, v| (v >> 1, v & 0x01), dest)
}

pub fn new_rol<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    new_shift_op(
        "rol",
        |cpu, v| ((v << 1) | (cpu.flag(Flag::Carry) as u8), v & 0x80),
        dest,
    )
}

pub fn new_ror<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    new_shift_op(
        "ror",
        |cpu, v| ((v >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7), v & 0x01),
        dest,
    )
}

// Clc, Cld, Cli, Clv
pub fn new_clear_bit<M: Mcu>(dest: FlagAddr) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        if dest.0 == Flag::InterruptDisabled {
            cpu.pending_set_interrupt_disabled_flag(false);
        } else {
            dest.set(cpu, 0);
        }
        2
    }
}

// Sec, Sed, Sei
pub fn new_set_bit<M: Mcu>(dest: FlagAddr) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        if dest.0 == Flag::InterruptDisabled {
            cpu.pending_set_interrupt_disabled_flag(true);
        } else {
            dest.set(cpu, 1);
        }
        2
    }
}

pub fn new_cmp<N: Mcu, R: Address<N>, M: Address<N>>(r: R, m: M) -> impl FnMut(&mut Cpu<N>) -> u8 {
    move |cpu| {
        let (r_val, _) = r.get(cpu);
        let (val, ticks) = m.get(cpu);
        let t = r_val.wrapping_sub(val);
        cpu.update_negative_flag(t);
        cpu.update_zero_flag(t);
        cpu.set_flag(Flag::Carry, r_val >= val);
        1 + ticks
    }
}

pub fn new_condition_branch<M: Mcu, R: Address<M>>(
    offset: i8,
    r: R,
    negative: bool,
) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (v, _) = r.get(cpu);
        let should_jump = if negative { v == 0 } else { v != 0 };
        if should_jump {
            let pc = cpu.pc;
            let dest = ((pc as i16).wrapping_add(offset as i16)) as u16;
            cpu.pc = dest;
            3 + extra_tick_if_cross_page(pc, dest)
        } else {
            2
        }
    }
}

pub fn new_jmp<M: Mcu>(addr: u16) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        cpu.pc = addr;
        3
    }
}

pub fn new_indirect_jmp<M: Mcu>(offset: u16) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        cpu.pc = cpu.read_word_in_same_page(offset);
        5
    }
}

pub fn new_jsr<M: Mcu>(addr: u16) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        cpu.pc = cpu.pc.wrapping_sub(1);
        cpu.push_pc();
        cpu.pc = addr;
        6
    }
}

pub fn new_rts<M: Mcu>() -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let l = cpu.pop_stack();
        let h = cpu.pop_stack();
        cpu.pc = ((h as u16) << 8) | (l as u16);
        cpu.inc_pc(1);
        6
    }
}

pub fn new_brk<M: Mcu>() -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        cpu.pc = cpu.pc.wrapping_add(1);
        cpu.push_pc();
        cpu.set_flag(Flag::Break, true);
        cpu.push_status();
        cpu.set_flag(Flag::InterruptDisabled, true);
        cpu.pc = cpu.read_word(0xFFFE);
        7
    }
}

pub fn new_rti<M: Mcu>() -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let v = cpu.pop_stack();
        RegisterStatus().set(cpu, v);
        cpu.pc = cpu.pop_stack() as u16 | ((cpu.pop_stack() as u16) << 8);
        6
    }
}

pub fn new_bit<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (v, ticks) = dest.get(cpu);
        cpu.set_flag(Flag::Negative, (v & 0x80) != 0);
        cpu.set_flag(Flag::Overflow, (v & 0x40) != 0);
        cpu.update_zero_flag(v & cpu.a);
        1 + ticks
    }
}

pub fn new_nop<M: Mcu>() -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |_| 2
}

pub fn new_nop_with_addr<N: Mcu, D: Address<N>>(dest: D) -> impl FnMut(&mut Cpu<N>) -> u8 {
    move |cpu| {
        let (_, ticks) = dest.get(cpu);
        1 + ticks
    }
}

pub fn new_hlt<M: Mcu>() -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        cpu.halt();
        2
    }
}

pub fn new_aso<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (v, _ticks) = dest.get(cpu);
        cpu.set_flag(Flag::Carry, (v & 0x80) != 0);
        let t = v << 1;
        let ticks_set = dest.set(cpu, t);
        cpu.a |= t;
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        if dest.is_register() {
            2
        } else {
            ticks_set + 3
        }
    }
}

pub fn new_rla<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (v, _ticks) = dest.get(cpu);
        let carry = v & 0x80 != 0;
        let t = (v << 1) | cpu.flag(Flag::Carry) as u8;
        let ticks_set = dest.set(cpu, t);
        cpu.set_flag(Flag::Carry, carry);
        cpu.a &= t;
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        if dest.is_register() {
            2
        } else {
            ticks_set + 3
        }
    }
}

pub fn new_lse<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (v, _ticks) = dest.get(cpu);
        cpu.set_flag(Flag::Carry, (v & 0x01) != 0);
        let t = v >> 1;
        let ticks_set = dest.set(cpu, t);
        cpu.a ^= t;
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        if dest.is_register() {
            2
        } else {
            ticks_set + 3
        }
    }
}

pub fn new_rra<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (v, _ticks) = dest.get(cpu);
        let carry = v & 0x01 != 0;
        let v = (v >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7);
        let ticks_set = dest.set(cpu, v);
        cpu.set_flag(Flag::Carry, carry);
        cpu.update_negative_flag(v);
        cpu.adc(v);
        if dest.is_register() {
            2
        } else {
            ticks_set + 3
        }
    }
}

pub fn new_sax<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let v = cpu.a & cpu.x;
        let ticks = dest.set(cpu, v);
        1 + ticks
    }
}

pub fn new_lax<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (v, ticks) = dest.get(cpu);
        cpu.a = v;
        cpu.x = v;
        cpu.update_negative_flag(v);
        cpu.update_zero_flag(v);
        1 + ticks
    }
}

pub fn new_dcp<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (v, _ticks) = dest.get(cpu);
        let t = v.wrapping_sub(1);
        let ticks_set = dest.set(cpu, t);
        let (t, borrow) = cpu.a.overflowing_sub(t);
        cpu.update_negative_flag(t);
        cpu.update_zero_flag(t);
        cpu.set_flag(Flag::Carry, !borrow);
        if dest.is_register() {
            2
        } else {
            ticks_set + 3
        }
    }
}

pub fn new_isc<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (v, _ticks) = dest.get(cpu);
        let t = v.wrapping_add(1);
        let ticks_set = dest.set(cpu, t);
        cpu.adc(!t);
        if dest.is_register() {
            2
        } else {
            ticks_set + 3
        }
    }
}

pub fn new_all<N: Mcu, R: Address<N>, D: Address<N>>(
    _name: &str,
    r: R,
    dest: D,
) -> impl FnMut(&mut Cpu<N>) -> u8 {
    move |cpu| {
        let (addr, _) = dest.calc_addr(cpu);
        let r = r.get(cpu).0;
        let v = r & ((addr >> 8) as u8).wrapping_add(1);
        let addr = (addr & 0xff) | ((v as u16) << 8);
        cpu.write_byte(addr, v);
        5
    }
}

pub fn new_ane<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let (v, ticks) = dest.get(cpu);
        cpu.a = cpu.x & v;
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        1 + ticks
    }
}

pub fn new_nop_store<M: Mcu, D: Address<M>>(dest: D) -> impl FnMut(&mut Cpu<M>) -> u8 {
    move |cpu| {
        let ticks = dest.set(cpu, cpu.a); // dummy write
        1 + ticks
    }
}
