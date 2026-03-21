use super::{extra_cycles_if_cross_page, Cpu, Flag};
use crate::cpu::addressing::{Addressing, BranchAddressing};
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
    Anc(Addressing),
    Alr(Addressing),
    Arr(Addressing),
    Sbx(Addressing),

    // Shifts/rotates
    Asl(Addressing),
    Lsr(Addressing),
    Rol(Addressing),
    Ror(Addressing),

    // Load / store / transfer
    Lda(Addressing),
    Ldx(Addressing),
    Ldy(Addressing),
    Sta(Addressing),
    Stx(Addressing),
    Sty(Addressing),
    Tax,
    Txa,
    Tay,
    Tya,
    Tsx,
    Txs,

    // Stack
    Pha,
    Php,
    Pla,
    Plp,

    // Inc / Dec
    Inc(Addressing),
    Dec(Addressing),
    Inx,
    Iny,
    Dex,
    Dey,

    // Compare / Bit / Branches
    Cmp(Addressing),
    Cpx(Addressing),
    Cpy(Addressing),
    Bit(Addressing),
    Bcc(BranchAddressing),
    Bcs(BranchAddressing),
    Bne(BranchAddressing),
    Beq(BranchAddressing),
    Bpl(BranchAddressing),
    Bmi(BranchAddressing),
    Bvc(BranchAddressing),
    Bvs(BranchAddressing),

    // Jumps / calls / returns / interrupts
    Jmp(BranchAddressing),
    Jsr(BranchAddressing),
    Rts,
    Brk,
    Rti,

    // Flags
    Clc,
    Sec,
    Cld,
    Sed,
    Cli,
    Sei,
    Clv,

    // Misc
    Nop(Addressing),
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
    Shx(Addressing),
    Shy(Addressing),
    Ane(Addressing),
    Sha(Addressing),
    Tas(Addressing),
}

impl Instruction {
    /// Execute the instruction, and returns cycle this instruction takes.
    pub fn exec<M: Mcu>(self, cpu: &mut Cpu<M>) -> u8 {
        match self {
            Instruction::Adc(addressing) => {
                let (v, cycles) = read_operand(cpu, addressing);
                cpu.adc(v);
                2 + cycles
            }
            Instruction::Sbc(addressing) => {
                let (v, cycles) = read_operand(cpu, addressing);
                cpu.adc(!v);
                2 + cycles
            }
            Instruction::And(addressing) => {
                let (v, cycles) = read_operand(cpu, addressing);
                cpu.a &= v;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                2 + cycles
            }
            Instruction::Ora(addressing) => {
                let (v, cycles) = read_operand(cpu, addressing);
                cpu.a |= v;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                2 + cycles
            }
            Instruction::Eor(addressing) => {
                let (v, cycles) = read_operand(cpu, addressing);
                cpu.a ^= v;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                2 + cycles
            }

            Instruction::Anc(addressing) => {
                let (v, cycles) = read_operand(cpu, addressing);
                cpu.a &= v;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                cpu.set_flag(Flag::Carry, cpu.a & 0x80 != 0);
                2 + cycles
            }
            Instruction::Alr(addressing) => {
                let (v, cycles) = read_operand(cpu, addressing);
                cpu.a &= v;
                cpu.set_flag(Flag::Carry, cpu.a & 0x01 != 0);
                cpu.a >>= 1;
                cpu.update_zero_flag(cpu.a);
                cpu.set_flag(Flag::Negative, false);
                2 + cycles
            }
            Instruction::Arr(addressing) => {
                let (v, cycles) = read_operand(cpu, addressing);
                cpu.a &= v;
                cpu.a = cpu.a >> 1 | (cpu.flag(Flag::Carry) as u8) << 7;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                cpu.set_flag(Flag::Carry, cpu.a & 0x40 != 0);
                cpu.set_flag(Flag::Overflow, ((cpu.a >> 6) ^ (cpu.a >> 5)) & 1 != 0);
                2 + cycles
            }
            Instruction::Sbx(addressing) => {
                let (v, cycles) = read_operand(cpu, addressing);
                let val = cpu.a & cpu.x;
                let (x, borrow) = val.overflowing_sub(v);
                cpu.x = x;
                cpu.update_negative_flag(x);
                cpu.update_zero_flag(x);
                cpu.set_flag(Flag::Carry, !borrow);
                2 + cycles
            }

            Instruction::Asl(addressing) => match addressing {
                Addressing::Accumulator => {
                    let v = cpu.a;
                    cpu.set_flag(Flag::Carry, v & 0x80 != 0);
                    cpu.a = v << 1;
                    cpu.update_negative_flag(cpu.a);
                    cpu.update_zero_flag(cpu.a);
                    2
                }
                _ => {
                    let (v, cycles) = read_operand(cpu, addressing);
                    let t = v << 1;
                    let cycles_set = write_operand(cpu, addressing, t);
                    cpu.update_negative_flag(t);
                    cpu.update_zero_flag(t);
                    if addressing.is_register() {
                        2
                    } else {
                        cycles_set + 3
                    }
                }
            },
            Instruction::Lsr(addressing) => match addressing {
                Addressing::Accumulator => {
                    let v = cpu.a;
                    cpu.set_flag(Flag::Carry, v & 0x01 != 0);
                    cpu.a = v >> 1;
                    cpu.update_zero_flag(cpu.a);
                    cpu.set_flag(Flag::Negative, false);
                    2
                }
                _ => {
                    let (v, cycles) = read_operand(cpu, addressing);
                    cpu.set_flag(Flag::Carry, v & 0x01 != 0);
                    let t = v >> 1;
                    let cycles_set = write_operand(cpu, addressing, t);
                    cpu.update_negative_flag(t);
                    cpu.update_zero_flag(t);
                    if addressing.is_register() {
                        2
                    } else {
                        cycles_set + 3
                    }
                }
            },
            Instruction::Rol(addressing) => match addressing {
                Addressing::Accumulator => {
                    let v = cpu.a;
                    let new = (v << 1) | (cpu.flag(Flag::Carry) as u8);
                    cpu.set_flag(Flag::Carry, v & 0x80 != 0);
                    cpu.a = new;
                    cpu.update_negative_flag(cpu.a);
                    cpu.update_zero_flag(cpu.a);
                    2
                }
                _ => {
                    let (v, cycles) = read_operand(cpu, addressing);
                    let new = (v << 1) | (cpu.flag(Flag::Carry) as u8);
                    let cycles_set = write_operand(cpu, addressing, new);
                    cpu.set_flag(Flag::Carry, v & 0x80 != 0);
                    cpu.update_negative_flag(new);
                    cpu.update_zero_flag(new);
                    if addressing.is_register() {
                        2
                    } else {
                        cycles_set + 3
                    }
                }
            },
            Instruction::Ror(addressing) => match addressing {
                Addressing::Accumulator => {
                    let v = cpu.a;
                    let new = (v >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7);
                    cpu.set_flag(Flag::Carry, v & 0x01 != 0);
                    cpu.a = new;
                    cpu.update_negative_flag(cpu.a);
                    cpu.update_zero_flag(cpu.a);
                    2
                }
                _ => {
                    let (v, cycles) = read_operand(cpu, addressing);
                    let new = (v >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7);
                    let cycles_set = write_operand(cpu, addressing, new);
                    cpu.set_flag(Flag::Carry, v & 0x01 != 0);
                    cpu.update_negative_flag(new);
                    cpu.update_zero_flag(new);
                    if addressing.is_register() {
                        2
                    } else {
                        cycles_set + 3
                    }
                }
            },

            Instruction::Lda(addressing) => {
                let (val, cycles) = read_operand(cpu, addressing);
                cpu.a = val;
                cpu.update_negative_flag(val);
                cpu.update_zero_flag(val);
                2 + cycles
            }
            Instruction::Ldx(addressing) => {
                let (val, cycles) = read_operand(cpu, addressing);
                cpu.x = val;
                cpu.update_negative_flag(val);
                cpu.update_zero_flag(val);
                2 + cycles
            }
            Instruction::Ldy(addressing) => {
                let (val, cycles) = read_operand(cpu, addressing);
                cpu.y = val;
                cpu.update_negative_flag(val);
                cpu.update_zero_flag(val);
                2 + cycles
            }
            Instruction::Sta(addressing) => {
                let cycles = write_operand(cpu, addressing, cpu.a);
                2 + cycles
            }
            Instruction::Stx(addressing) => {
                let cycles = write_operand(cpu, addressing, cpu.x);
                2 + cycles
            }
            Instruction::Sty(addressing) => {
                let cycles = write_operand(cpu, addressing, cpu.y);
                2 + cycles
            }
            Instruction::Tax => {
                cpu.x = cpu.a;
                cpu.update_negative_flag(cpu.x);
                cpu.update_zero_flag(cpu.x);
                2
            }
            Instruction::Txa => {
                cpu.a = cpu.x;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                2
            }
            Instruction::Tay => {
                cpu.y = cpu.a;
                cpu.update_negative_flag(cpu.y);
                cpu.update_zero_flag(cpu.y);
                2
            }
            Instruction::Tya => {
                cpu.a = cpu.y;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                2
            }
            Instruction::Tsx => {
                cpu.x = cpu.sp;
                cpu.update_negative_flag(cpu.x);
                cpu.update_zero_flag(cpu.x);
                2
            }
            Instruction::Txs => {
                cpu.sp = cpu.x;
                2
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
                Addressing::Accumulator => {
                    cpu.a = cpu.a.wrapping_add(1);
                    cpu.update_negative_flag(cpu.a);
                    cpu.update_zero_flag(cpu.a);
                    2
                }
                _ => {
                    let (val, cycles_set) = read_operand(cpu, addressing);
                    let val = val.wrapping_add(1);
                    let cycles_set = write_operand(cpu, addressing, val);
                    cpu.update_negative_flag(val);
                    cpu.update_zero_flag(val);
                    cycles_set + 3
                }
            },
            Instruction::Dec(addressing) => match addressing {
                Addressing::Accumulator => {
                    cpu.a = cpu.a.wrapping_sub(1);
                    cpu.update_negative_flag(cpu.a);
                    cpu.update_zero_flag(cpu.a);
                    2
                }
                _ => {
                    let (val, cycles_set) = read_operand(cpu, addressing);
                    let val = val.wrapping_sub(1);
                    let cycles_set = write_operand(cpu, addressing, val);
                    cpu.update_negative_flag(val);
                    cpu.update_zero_flag(val);
                    cycles_set + 3
                }
            },
            Instruction::Inx => {
                cpu.x = cpu.x.wrapping_add(1);
                cpu.update_negative_flag(cpu.x);
                cpu.update_zero_flag(cpu.x);
                2
            }
            Instruction::Iny => {
                cpu.y = cpu.y.wrapping_add(1);
                cpu.update_negative_flag(cpu.y);
                cpu.update_zero_flag(cpu.y);
                2
            }
            Instruction::Dex => {
                cpu.x = cpu.x.wrapping_sub(1);
                cpu.update_negative_flag(cpu.x);
                cpu.update_zero_flag(cpu.x);
                2
            }
            Instruction::Dey => {
                cpu.y = cpu.y.wrapping_sub(1);
                cpu.update_negative_flag(cpu.y);
                cpu.update_zero_flag(cpu.y);
                2
            }

            Instruction::Cmp(m) => {
                let (val, cycles) = read_operand(cpu, m);
                let t = cpu.a.wrapping_sub(val);
                cpu.update_negative_flag(t);
                cpu.update_zero_flag(t);
                cpu.set_flag(Flag::Carry, cpu.a >= val);
                2 + cycles
            }
            Instruction::Cpx(m) => {
                let (val, cycles) = read_operand(cpu, m);
                let t = cpu.x.wrapping_sub(val);
                cpu.update_negative_flag(t);
                cpu.update_zero_flag(t);
                cpu.set_flag(Flag::Carry, cpu.x >= val);
                2 + cycles
            }
            Instruction::Cpy(m) => {
                let (val, cycles) = read_operand(cpu, m);
                let t = cpu.y.wrapping_sub(val);
                cpu.update_negative_flag(t);
                cpu.update_zero_flag(t);
                cpu.set_flag(Flag::Carry, cpu.y >= val);
                2 + cycles
            }
            Instruction::Bit(dest) => {
                let (v, cycles) = read_operand(cpu, dest);
                cpu.set_flag(Flag::Negative, (v & 0x80) != 0);
                cpu.set_flag(Flag::Overflow, (v & 0x40) != 0);
                cpu.update_zero_flag(v & cpu.a);
                2 + cycles
            }
            Instruction::Bcc(addr) => branch(cpu, addr, !cpu.flag(Flag::Carry)),
            Instruction::Bcs(addr) => branch(cpu, addr, cpu.flag(Flag::Carry)),
            Instruction::Bne(addr) => branch(cpu, addr, !cpu.flag(Flag::Zero)),
            Instruction::Beq(addr) => branch(cpu, addr, cpu.flag(Flag::Zero)),
            Instruction::Bpl(addr) => branch(cpu, addr, !cpu.flag(Flag::Negative)),
            Instruction::Bmi(addr) => branch(cpu, addr, cpu.flag(Flag::Negative)),
            Instruction::Bvc(addr) => branch(cpu, addr, !cpu.flag(Flag::Overflow)),
            Instruction::Bvs(addr) => branch(cpu, addr, cpu.flag(Flag::Overflow)),

            Instruction::Jmp(addr) => {
                cpu.pc = addr.calc_addr(cpu);
                3
            }
            Instruction::Jsr(addr) => {
                cpu.pc = cpu.pc.wrapping_sub(1);
                cpu.push_pc();
                cpu.pc = addr.calc_addr(cpu);
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
                // Previously used RegisterStatus to restore status byte; write
                // flags directly to CPU status while preserving the break &
                // unused bits behavior.
                cpu.status = v & 0b1100_1111; // mask unused bits
                cpu.pc = cpu.pop_stack() as u16 | ((cpu.pop_stack() as u16) << 8);
                6
            }

            Instruction::Clc => {
                cpu.set_flag(Flag::Carry, false);
                2
            }
            Instruction::Sec => {
                cpu.set_flag(Flag::Carry, true);
                2
            }
            Instruction::Cld => {
                cpu.set_flag(Flag::Decimal, false);
                2
            }
            Instruction::Sed => {
                cpu.set_flag(Flag::Decimal, true);
                2
            }
            Instruction::Cli => {
                cpu.pending_set_interrupt_disabled_flag(false);
                2
            }
            Instruction::Sei => {
                cpu.pending_set_interrupt_disabled_flag(true);
                2
            }
            Instruction::Clv => {
                cpu.set_flag(Flag::Overflow, false);
                2
            }

            Instruction::Nop(dest) => match dest {
                Addressing::Implied => 2,
                _ => {
                    let (_v, cycles) = read_operand(cpu, dest);
                    2 + cycles
                }
            },
            Instruction::Hlt => {
                cpu.halt();
                2
            }

            Instruction::Aso(dest) => {
                let (v, cycles) = read_operand(cpu, dest);
                cpu.set_flag(Flag::Carry, (v & 0x80) != 0);
                let t = v << 1;
                let cycles_set = write_operand(cpu, dest, t);
                cpu.a |= t;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                cycles_set + 3
            }
            Instruction::Rla(dest) => {
                let (v, cycles) = read_operand(cpu, dest);
                let carry = v & 0x80 != 0;
                let t = (v << 1) | cpu.flag(Flag::Carry) as u8;
                let cycles_set = write_operand(cpu, dest, t);
                cpu.set_flag(Flag::Carry, carry);
                cpu.a &= t;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                cycles_set + 3
            }
            Instruction::Lse(dest) => {
                let (v, cycles) = read_operand(cpu, dest);
                cpu.set_flag(Flag::Carry, (v & 0x01) != 0);
                let t = v >> 1;
                let cycles_set = write_operand(cpu, dest, t);
                cpu.a ^= t;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                cycles_set + 3
            }
            Instruction::Rra(dest) => {
                let (v, cycles) = read_operand(cpu, dest);
                let carry = v & 0x01 != 0;
                let v = (v >> 1) | ((cpu.flag(Flag::Carry) as u8) << 7);
                let cycles_set = write_operand(cpu, dest, v);
                cpu.set_flag(Flag::Carry, carry);
                cpu.update_negative_flag(v);
                cpu.adc(v);
                cycles_set + 3
            }
            Instruction::Sax(dest) => {
                let v = cpu.a & cpu.x;
                let cycles = write_operand(cpu, dest, v);
                2 + cycles
            }
            Instruction::Lax(dest) => {
                let (v, cycles) = read_operand(cpu, dest);
                cpu.a = v;
                cpu.x = v;
                cpu.update_negative_flag(v);
                cpu.update_zero_flag(v);
                2 + cycles
            }
            Instruction::Dcp(dest) => {
                let (v, cycles) = read_operand(cpu, dest);
                let t = v.wrapping_sub(1);
                let cycles_set = write_operand(cpu, dest, t);
                let (t2, borrow) = cpu.a.overflowing_sub(t);
                cpu.update_negative_flag(t2);
                cpu.update_zero_flag(t2);
                cpu.set_flag(Flag::Carry, !borrow);
                cycles_set + 3
            }
            Instruction::Isc(dest) => {
                let (v, cycles) = read_operand(cpu, dest);
                let t = v.wrapping_add(1);
                let cycles_set = write_operand(cpu, dest, t);
                cpu.adc(!t);
                cycles_set + 3
            }
            Instruction::Ane(addressing) => {
                let (v, cycles) = read_operand(cpu, addressing);
                cpu.a = cpu.x & v;
                cpu.update_negative_flag(cpu.a);
                cpu.update_zero_flag(cpu.a);
                2 + cycles
            }
            Instruction::Sha(dest) => {
                // SHA stores A & X & highbyte(addr) to memory (unofficial opcode behaviour)
                let (addr, cycles) = dest.calc_addr(cpu);
                let v = cpu.a & cpu.x & ((addr >> 8) as u8).wrapping_add(1);
                cpu.write_byte(addr, v);
                2 + cycles
            }
            Instruction::Tas(dest) => {
                // TAS: stores A & X to memory and also sets SP high byte (unofficial)
                // Emulate prior NopStore behavior: write A to dest
                let cycles = write_operand(cpu, dest, cpu.a);
                2 + cycles
            }
            Instruction::Shx(dest) => {
                let (addr, _) = dest.calc_addr(cpu);
                let v = cpu.x & ((addr >> 8) as u8).wrapping_add(1);
                let addr = (addr & 0xff) | ((v as u16) << 8);
                cpu.write_byte(addr, v);
                5
            }
            Instruction::Shy(dest) => {
                let (addr, _) = dest.calc_addr(cpu);
                let v = cpu.y & ((addr >> 8) as u8).wrapping_add(1);
                let addr = (addr & 0xff) | ((v as u16) << 8);
                cpu.write_byte(addr, v);
                5
            }
        }
    }
}

fn branch<M: Mcu>(cpu: &mut Cpu<M>, addr: BranchAddressing, should_jump: bool) -> u8 {
    if should_jump {
        let pc = cpu.pc;
        let dest = addr.calc_addr(cpu);
        cpu.pc = dest;
        3 + extra_cycles_if_cross_page(pc, dest)
    } else {
        2
    }
}

fn read_operand<M: Mcu>(cpu: &mut Cpu<M>, addressing: Addressing) -> (u8, u8) {
    match addressing {
        Addressing::Accumulator => (cpu.a, 0),
        Addressing::Immediate(v) => (v, 0),
        Addressing::Implied => unreachable!("can't read implied addressing"),
        _ => {
            let (addr, cycles) = addressing.calc_addr(cpu);
            (cpu.read_byte(addr), cycles)
        }
    }
}

fn write_operand<M: Mcu>(cpu: &mut Cpu<M>, addressing: Addressing, value: u8) -> u8 {
    match addressing {
        Addressing::Accumulator => {
            cpu.a = value;
            0
        }
        Addressing::Immediate(_) => unreachable!("can't write to immediate addressing"),
        Addressing::Implied => unreachable!("can't write to implied addressing"),
        _ => {
            let (addr, cycles) = addressing.calc_addr(cpu);
            cpu.write_byte(addr, value);
            cycles
        }
    }
}

// The `Instruction` enum provides cycle-accurate implementations; the
// `new_*` constructor helpers were previously exposed but are not used by the
// rest of the workspace. They were removed to eliminate dead-code warnings and
// keep the public API minimal. If you need factory helpers later we can add
// them back behind feature flags.
