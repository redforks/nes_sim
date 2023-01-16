use super::addressing::{Address, RegisterStatus};
use super::{extra_tick_if_cross_page, Cpu, Flag};
use crate::cpu::addressing::Literal;
use log::debug;

/// LDA, LDX, LDY, TAX, TAY, TSX, TXA, TYA
pub fn new_transfer<S, D>(dest: D, src: S) -> impl FnMut(&mut Cpu) -> u8
where
    S: Address,
    D: Address,
{
    debug!("transfer {} {}", dest, src);

    move |cpu| {
        let (val, ticks) = src.get(cpu);
        dest.set(cpu, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        ticks + 2
    }
}

// STA, STX, STY
pub fn new_transfer_no_touch_flags<S, D>(dest: D, src: S) -> impl FnMut(&mut Cpu) -> u8
where
    S: Address,
    D: Address,
{
    debug!("transfer_no_touch {} {}", dest, src);

    move |cpu| {
        let (val, ticks) = src.get(cpu);
        dest.set(cpu, val);
        ticks + 2
    }
}

pub fn new_pha() -> impl FnMut(&mut Cpu) -> u8 {
    debug!("pha");

    move |cpu| {
        cpu.push_stack(cpu.a);
        3
    }
}

pub fn new_php() -> impl FnMut(&mut Cpu) -> u8 {
    debug!("php");

    move |cpu| {
        cpu.set_flag(Flag::Break, true);
        cpu.push_status();
        3
    }
}

pub fn new_pla() -> impl FnMut(&mut Cpu) -> u8 {
    debug!("pla");

    move |cpu| {
        cpu.a = cpu.pop_stack();
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        4
    }
}

pub fn new_plp() -> impl FnMut(&mut Cpu) -> u8 {
    debug!("plp");

    move |cpu| {
        let saved = cpu.pop_stack();
        let saved_break_flag = cpu.flag(Flag::Break);
        cpu.status = saved;
        cpu.set_flag(Flag::Break, saved_break_flag);
        4
    }
}

// DEC, DEX, DEY
pub fn new_dec<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("dec {}", dest);

    move |cpu| {
        let (val, ticks) = dest.get(cpu);
        let val = val.wrapping_sub(1);
        dest.set(cpu, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        if dest.is_register() {
            2
        } else {
            4 + ticks
        }
    }
}

// INC, INX, INY
pub fn new_inc<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("inc {}", dest);

    move |cpu| {
        let (val, ticks) = dest.get(cpu);
        let val = val.wrapping_add(1);
        dest.set(cpu, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        if dest.is_register() {
            2
        } else {
            4 + ticks
        }
    }
}

pub fn new_adc<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("adc {}", dest);

    move |cpu| {
        let (val, ticks) = dest.get(cpu);
        cpu.adc(val);
        2 + ticks
    }
}

pub fn new_sbc<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("sbc {}", dest);

    move |cpu| {
        let (val, ticks) = dest.get(cpu);
        cpu.adc(!val);
        2 + ticks
    }
}

fn new_acc_op<F: Fn(u8, u8) -> u8, S: Address>(
    op_name: &str,
    op: F,
    src: S,
) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("{} {}", op_name, src);

    move |cpu| {
        let (val, ticks) = src.get(cpu);
        cpu.a = op(cpu.a, val);
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        2 + ticks
    }
}

pub fn new_and<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    new_acc_op("and", |a, b| a & b, dest)
}

pub fn new_anc(Literal(v): Literal) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("anc {}", Literal(v));

    move |cpu| {
        cpu.a &= v;
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        cpu.set_flag(Flag::Carry, cpu.a & 0x80 != 0);
        2
    }
}

pub fn new_alr(Literal(v): Literal) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("alr {}", Literal(v));

    move |cpu| {
        cpu.a &= v;
        cpu.set_flag(Flag::Carry, cpu.a & 0x01 != 0);
        cpu.a >>= 1;
        cpu.update_zero_flag(cpu.a);
        cpu.set_flag(Flag::Negative, false);
        2
    }
}

pub fn new_arr(Literal(v): Literal) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("arr {}", Literal(v));

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

pub fn new_sbx(Literal(v): Literal) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("sbx {}", Literal(v));

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

pub fn new_eor<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    new_acc_op("eor", |a, b| a ^ b, dest)
}

pub fn new_ora<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    new_acc_op("ora", |a, b| a | b, dest)
}

fn new_shift_op<F: Fn(&Cpu, u8) -> (u8, u8), D: Address>(
    op_name: &str,
    op: F,
    dest: D,
) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("{} {}", op_name, dest);

    move |cpu| {
        let (val, ticks) = dest.get(cpu);
        let (val, carry_flag) = op(cpu, val);
        cpu.set_flag(Flag::Carry, carry_flag != 0);
        dest.set(cpu, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        2 + ticks
    }
}

pub fn new_asl<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    new_shift_op("asl", |_, v| (v << 1, v & 0x80), dest)
}

pub fn new_lsr<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    new_shift_op("lsr", |_, v| (v >> 1, v & 0x01), dest)
}

pub fn new_rol<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    new_shift_op(
        "rol",
        |cpu, v| ((v << 1) | (cpu.flag(Flag::Carry) as u8), v & 0x80),
        dest,
    )
}

pub fn new_ror<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    new_shift_op(
        "ror",
        |cpu, v| ((v >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7), v & 0x01),
        dest,
    )
}

// Clc, Cld, Cli, Clv
pub fn new_clear_bit<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("clear {}", dest);

    move |cpu| {
        dest.set(cpu, 0);
        2
    }
}

// Sec, Sed, Sei
pub fn new_set_bit<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("set {}", dest);

    move |cpu| {
        dest.set(cpu, 1);
        2
    }
}

pub fn new_cmp<R: Address, M: Address>(r: R, m: M) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("cmp {} {}", r, m);

    move |cpu| {
        let (r_val, _) = r.get(cpu);
        let (val, ticks) = m.get(cpu);
        let t = r_val.wrapping_sub(val);
        cpu.update_negative_flag(t);
        cpu.update_zero_flag(t);
        cpu.set_flag(Flag::Carry, r_val >= val);
        2 + ticks
    }
}

pub fn new_condition_branch<R: Address>(
    offset: i8,
    r: R,
    negative: bool,
) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("branch {} {} {}", offset, r, negative);

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

pub fn new_jmp(addr: u16) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("jmp ${:x}", addr);

    move |cpu| {
        cpu.pc = addr;
        3
    }
}

pub fn new_indirect_jmp(offset: u16) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("indirect_jmp ${:x}", offset);

    move |cpu| {
        let addr = offset;
        let low = cpu.read_byte(addr) as u16;

        let low_addr = addr as u8;
        let high_addr = addr & 0xff00;
        let addr = high_addr | (low_addr.wrapping_add(1) as u16);
        let high = cpu.read_byte(addr) as u16;

        cpu.pc = (high << 8) | low;
        5
    }
}

pub fn new_jsr(addr: u16) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("jsr {}", addr);

    move |cpu| {
        let pc = cpu.pc.wrapping_sub(1);
        cpu.push_stack((pc >> 8) as u8);
        cpu.push_stack(pc as u8);
        cpu.pc = addr;
        6
    }
}

pub fn new_rts() -> impl FnMut(&mut Cpu) -> u8 {
    debug!("rts");

    move |cpu| {
        let l = cpu.pop_stack();
        let h = cpu.pop_stack();
        cpu.pc = ((h as u16) << 8) | (l as u16);
        cpu.inc_pc(1);
        6
    }
}

pub fn new_brk() -> impl FnMut(&mut Cpu) -> u8 {
    debug!("brk");

    move |cpu| {
        let pc = cpu.pc.wrapping_add(1);
        cpu.push_stack((pc >> 8) as u8);
        cpu.push_stack(pc as u8);
        cpu.set_flag(Flag::Break, true);
        cpu.push_status();
        cpu.set_flag(Flag::InterruptDisabled, true);
        cpu.pc = cpu.read_word(0xFFFE);
        7
    }
}

pub fn new_rti() -> impl FnMut(&mut Cpu) -> u8 {
    debug!("rti");

    move |cpu| {
        let v = cpu.pop_stack();
        RegisterStatus().set(cpu, v);
        cpu.pc = cpu.pop_stack() as u16 | ((cpu.pop_stack() as u16) << 8);
        6
    }
}

pub fn new_bit<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("bit {}", dest);

    move |cpu| {
        let (v, ticks) = dest.get(cpu);
        cpu.set_flag(Flag::Negative, (v & 0x80) != 0);
        cpu.set_flag(Flag::Overflow, (v & 0x70) != 0);
        cpu.update_zero_flag(v & cpu.a);
        ticks + 2
    }
}

pub fn new_nop() -> impl FnMut(&mut Cpu) -> u8 {
    debug!("nop");

    move |_| 2
}

pub fn new_nop_with_addr<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("nop {}", dest);

    move |cpu| {
        let (_, ticks) = dest.get(cpu);
        ticks + 2
    }
}

pub fn new_aso<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("aso {}", dest);

    move |cpu| {
        let (v, ticks) = dest.get(cpu);
        cpu.set_flag(Flag::Carry, (v & 0x80) != 0);
        let t = v << 1;
        dest.set(cpu, t);
        cpu.a |= t;
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        ticks + 2
    }
}

pub fn new_rla<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("rla {}", dest);

    move |cpu| {
        let (v, ticks) = dest.get(cpu);
        let carry = v & 0x80 != 0;
        let t = (v << 1) | cpu.flag(Flag::Carry) as u8;
        dest.set(cpu, t);
        cpu.set_flag(Flag::Carry, carry);
        cpu.a &= t;
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        ticks + 2
    }
}

pub fn new_lse<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("lse {}", dest);

    move |cpu| {
        let (v, ticks) = dest.get(cpu);
        cpu.set_flag(Flag::Carry, (v & 0x01) != 0);
        let t = v >> 1;
        dest.set(cpu, t);
        cpu.a ^= t;
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        ticks + 2
    }
}

pub fn new_rra<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("rra {}", dest);

    move |cpu| {
        let (v, ticks) = dest.get(cpu);
        let carry = v & 0x01 != 0;
        let v = (v >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7);
        dest.set(cpu, v);
        cpu.set_flag(Flag::Carry, carry);
        cpu.update_negative_flag(v);
        cpu.adc(v);
        ticks + 2
    }
}

pub fn new_sax<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("sax {}", dest);

    move |cpu| {
        let v = cpu.a & cpu.x;
        dest.set(cpu, v);
        2
    }
}

pub fn new_lax<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("lax {}", dest);

    move |cpu| {
        let (v, ticks) = dest.get(cpu);
        cpu.a = v;
        cpu.x = v;
        cpu.update_negative_flag(v);
        cpu.update_zero_flag(v);
        ticks + 2
    }
}

pub fn new_dcp<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("dcp {}", dest);

    move |cpu| {
        let (v, ticks) = dest.get(cpu);
        let t = v.wrapping_sub(1);
        dest.set(cpu, t);
        let (t, borrow) = cpu.a.overflowing_sub(t);
        cpu.update_negative_flag(t);
        cpu.update_zero_flag(t);
        cpu.set_flag(Flag::Carry, !borrow);
        ticks + 2
    }
}

pub fn new_isc<D: Address>(dest: D) -> impl FnMut(&mut Cpu) -> u8 {
    debug!("isc {}", dest);

    move |cpu| {
        let (v, ticks) = dest.get(cpu);
        let t = v.wrapping_add(1);
        dest.set(cpu, t);
        cpu.adc(!t);
        ticks + 2
    }
}
