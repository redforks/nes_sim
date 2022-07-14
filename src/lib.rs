use std::ops::{BitAnd, Shl};
use std::convert::From;
use std::num::FpCategory::Zero;
use std::panic::panic_any;
use crate::Agu::Absolute;

mod test;

/// Address generation unit
enum Agu {
    Literal(u8),
    ZeroPage(u8),
    Absolute(u16),
    ZeroPageX(u8),
    ZeroPageY(u8),
    AbsoluteX(u16),
    AbsoluteY(u16),
    Indirect(u16),
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
    fn address(&self, cpu: &Cpu) -> (u16, u8, u8) {
        match self {
            &Agu::Literal(_) => panic_any("Literal not supported"),
            &Agu::ZeroPage(addr) => (addr as u16, 1, 1),
            &Absolute(addr) => (addr, 2, 2),
            &Agu::ZeroPageX(addr) => (addr.wrapping_add(cpu.x) as u16, 2, 1),
            &Agu::ZeroPageY(addr) => (addr.wrapping_add(cpu.y) as u16, 2, 1),
            &Agu::AbsoluteX(addr) => {
                let r = addr.wrapping_add(cpu.x as u16) as u16;
                (r, plus_one_if_cross_page(2, addr, r), 2)
            }
            &Agu::AbsoluteY(addr) => {
                let r = addr.wrapping_add(cpu.y as u16) as u16;
                (r, plus_one_if_cross_page(2, addr, r), 2)
            }
            &Agu::Indirect(addr) => (cpu.read_word(addr), 2, 2),
            &Agu::IndirectX(addr) => (cpu.read_zero_page_word(addr.wrapping_add(cpu.x)), 4, 1),
            &Agu::IndirectY(addr) => {
                let addr = cpu.read_zero_page_word(addr);
                let r = addr.wrapping_add(cpu.y as u16);
                (r, plus_one_if_cross_page(3, addr, r), 1)
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
            &Agu::RegisterA | &Agu::RegisterX | &Agu::RegisterY | &Agu::RegisterSP | &Agu::Status => true,
            _ => false,
        }
    }
}

#[allow(dead_code)]
enum Instruction {
    // LDA, LDX, LDY, TAX, TAY, TSX, TXA, TYA
    Transfer(Transfer),
    // STA, STX, STY
    TransferNoTouchFlags(TransferNoTouchFlags),
    Txs(Txs),

    // PHA, PHP
    Push(Push),
    // PLA, PLP
    Pop(Pop),

    // DEC, DEX, DEY
    Dec(Dec),
    // INC, INX, INY
    Inc(Inc),

    Adc(Adc),
    Sbc(Sbc),

    And(And),
    Eor(Eor),
    Ora(Ora),

    Asl(Asl),
    Lsr(Lsr),
    Rol(Rol),
    Ror(Ror),

    Clc(Clc),
    Cld(Cld),
    Cli(Cli),
    Clv(Clv),
    Sec(Sec),
    Sed(Sed),
    Sei(Sei),

    // Cmp, Cpx, Cpy
    Cmp(Cmp),

    // Bcc, Bcs, Bne, Beq, Bmi, Bpl, Bvc, Bvs
    ConditionBranch(ConditionBranch),

    Jmp(Jmp),
    IndirectJmp(IndirectJmp),
    Jsr(Jsr),
    Rts(Rts),
    Brk(Brk),
    Rti(Rti),

    Bit(Bit),
    Nop,
}

struct Transfer {
    src: Agu,
    dest: Agu,
}

struct TransferNoTouchFlags {
    src: Agu,
    dest: Agu,
}

struct Txs {}

struct Push(Agu);

struct Pop(Agu);

struct Dec(Agu);

struct Inc(Agu);

struct Adc(Agu);

struct Sbc(Agu);

struct And(Agu);

struct Eor(Agu);

struct Ora(Agu);

struct Asl(Agu);

struct Lsr(Agu);

struct Rol(Agu);

struct Ror(Agu);

struct Clc ();

struct Cld ();

struct Cli ();

struct Clv ();

struct Sec ();

struct Sed ();

struct Sei ();

struct Cmp {
    register: Agu,
    memory: Agu,
}

struct ConditionBranch {
    offset: u8,
    register: Agu,
    negative: bool,
}

impl ConditionBranch {
    fn new(offset: u8, register: Agu, negative: bool) -> ConditionBranch {
        ConditionBranch {
            offset,
            register,
            negative,
        }
    }
}

struct Jmp(u16);

struct IndirectJmp(u16);

struct Jsr(u16);

struct Rts ();

struct Brk ();

struct Rti ();

struct Bit(Agu);

trait InstructionType {
    // (pcDelta, tickCount)
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8);
}

/// Bin operate with A register and value from Agu, store result to A register.
trait AccumulatorOpInstructionType {
    fn agu(&self) -> &Agu;
    fn op(a: u8, v: u8) -> u8;

    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let agu = self.agu();
        let (val, operands, ticks) = cpu.get(agu);
        cpu.a = Self::op(cpu.a, val);
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
        (operands + 1, ticks + 2)
    }
}

trait ShiftInstructionType {
    fn agu(&self) -> &Agu;
    fn op(cpu: &Cpu, v: u8) -> (u8, bool);

    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let agu = self.agu();
        let (val, operands, ticks) = cpu.get(agu);
        let (v, carry_flag) = Self::op(cpu, val);
        cpu.set_flag(CarryFlag, carry_flag);
        cpu.put(agu, v);
        cpu.update_negative_flag(v);
        cpu.update_zero_flag(v);
        (operands + 1, ticks + 2)
    }
}

impl InstructionType for Transfer {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let (val, operands, ticks) = cpu.get(&self.src);
        cpu.put(&self.dest, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        (operands + 1, ticks + 2)
    }
}

impl InstructionType for TransferNoTouchFlags {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let (val, operands, ticks) = cpu.get(&self.src);
        cpu.put(&self.dest, val);
        (operands + 1, ticks + 3)
    }
}

impl InstructionType for Txs {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        cpu.sp = cpu.x;
        (1, 2)
    }
}

impl InstructionType for Push {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let (val, ..) = cpu.get(&self.0);
        cpu.push_stack(val);
        (1, 3)
    }
}

impl InstructionType for Pop {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let val = cpu.pop_stack();
        cpu.put(&self.0, val);
        (1, 4)
    }
}

impl InstructionType for Dec {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let (val, operands, ticks) = cpu.get(&self.0);
        let val = val.wrapping_sub(1);
        cpu.put(&self.0, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        (1 + operands, if self.0.is_register() { 2 } else { 4 + ticks })
    }
}

impl InstructionType for Inc {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let (val, operands, ticks) = cpu.get(&self.0);
        let val = val.wrapping_add(1);
        cpu.put(&self.0, val);
        cpu.update_negative_flag(val);
        cpu.update_zero_flag(val);
        (1 + operands, if self.0.is_register() { 2 } else { 4 + ticks })
    }
}

impl InstructionType for Adc {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let (val, operands, ticks) = cpu.get(&self.0);

        let t: u16 = cpu.a as u16 + val as u16 + (cpu.flag(CarryFlag)) as u16;
        cpu.set_flag(OverflowFlag, cpu.a & 0x80 != (t as u8) & 0x80);
        cpu.update_zero_flag(t);
        cpu.update_negative_flag(cpu.a);
        cpu.set_flag(CarryFlag, t > 255);
        cpu.a = t as u8;

        (operands + 1, ticks + 2)
    }
}

impl InstructionType for Sbc {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let (val, operands, ticks) = cpu.get(&self.0);

        let t = cpu.a as i16 - val as i16 - !(cpu.flag(CarryFlag) as i16);
        cpu.set_flag(OverflowFlag, t > 127 || t < -128);
        cpu.set_flag(CarryFlag, t >= 0);
        cpu.update_negative_flag(t);
        cpu.update_zero_flag(t);
        cpu.a = t as u8;

        (operands + 1, ticks + 2)
    }
}

impl AccumulatorOpInstructionType for And {
    fn agu(&self) -> &Agu { &self.0 }
    fn op(a: u8, v: u8) -> u8 { a & v }
}

impl AccumulatorOpInstructionType for Eor {
    fn agu(&self) -> &Agu { &self.0 }
    fn op(a: u8, v: u8) -> u8 { a ^ v }
}

impl AccumulatorOpInstructionType for Ora {
    fn agu(&self) -> &Agu { &self.0 }
    fn op(a: u8, v: u8) -> u8 { a | v }
}

impl ShiftInstructionType for Asl {
    fn agu(&self) -> &Agu { &self.0 }

    fn op(_: &Cpu, v: u8) -> (u8, bool) {
        let carry_flag = v & 0x80 != 0;
        (v << 1, carry_flag)
    }
}

impl ShiftInstructionType for Lsr {
    fn agu(&self) -> &Agu { &self.0 }

    fn op(_: &Cpu, v: u8) -> (u8, bool) {
        let carry_flag = v & 1 != 0;
        (v >> 1, carry_flag)
    }
}

impl ShiftInstructionType for Rol {
    fn agu(&self) -> &Agu { &self.0 }

    fn op(cpu: &Cpu, v: u8) -> (u8, bool) {
        let carry_flag = v & 0x80 != 0;
        (v << 1 & 0xfe | cpu.flag(CarryFlag) as u8, carry_flag)
    }
}

impl ShiftInstructionType for Ror {
    fn agu(&self) -> &Agu { &self.0 }

    fn op(cpu: &Cpu, v: u8) -> (u8, bool) {
        let carry_flag = v & 1 != 0;
        (v >> 1 & 0x7f | ((cpu.flag(CarryFlag) as u8) << 7), carry_flag)
    }
}

impl InstructionType for Clc {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        cpu.set_flag(CarryFlag, false);
        (1, 2)
    }
}

impl InstructionType for Cld {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        cpu.set_flag(DecimalModeFlag, false);
        (1, 2)
    }
}

impl InstructionType for Cli {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        cpu.set_flag(InterruptDisableFlag, false);
        (1, 2)
    }
}

impl InstructionType for Clv {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        cpu.set_flag(OverflowFlag, false);
        (1, 2)
    }
}

impl InstructionType for Sec {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        cpu.set_flag(CarryFlag, true);
        (1, 2)
    }
}

impl InstructionType for Sed {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        cpu.set_flag(DecimalModeFlag, true);
        (1, 2)
    }
}

impl InstructionType for Sei {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        cpu.set_flag(InterruptDisableFlag, true);
        (1, 2)
    }
}

impl InstructionType for Cmp {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let (reg_val, _, _) = cpu.get(&self.register);
        let (val, operands, ticks) = cpu.get(&self.memory);
        let t = reg_val - val;
        cpu.update_negative_flag(t);
        cpu.update_zero_flag(t);
        cpu.set_flag(CarryFlag, reg_val > val);
        (operands + 1, ticks + 2)
    }
}

impl InstructionType for ConditionBranch {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let (v, _, _) = cpu.get(&self.register);
        let v = if self.negative { v == 0 } else { v != 0 };
        if v {
            (self.offset, plus_one_if_cross_page(3, cpu.pc, self.offset as u16 + cpu.pc))
        } else {
            (2, 2)
        }
    }
}

impl Jmp {
    fn execute(&self) -> (u16, u8) {
        (self.0, 3)
    }
}

impl IndirectJmp {
    fn execute(&self, cpu: &Cpu) -> (u16, u8) {
        let addr = cpu.read_word(self.0);
        (addr, 5)
    }
}

impl Jsr {
    fn execute(&self, cpu: &mut Cpu) -> (u16, u8) {
        let pc = cpu.pc.wrapping_add(2);
        cpu.push_stack((pc >> 8) as u8);
        cpu.push_stack(pc as u8);
        (self.0, 6)
    }
}

impl Rts {
    fn execute(&self, cpu: &mut Cpu) -> (u16, u8) {
        let l = cpu.pop_stack();
        let h = cpu.pop_stack();
        let addr = (h as u16) << 8 | l as u16;
        (addr, 6)
    }
}

impl Brk {
    fn execute(&self, cpu: &mut Cpu) -> (u16, u8) {
        let pc = cpu.pc.wrapping_add(2);
        cpu.push_stack((pc >> 8) as u8);
        cpu.push_stack(pc as u8);
        let (status, ..) = cpu.get(&Agu::Status);
        cpu.push_stack(status);
        (cpu.read_word(0xFFFE), 7)
    }
}

impl Rti {
    fn execute(&self, cpu: &mut Cpu) -> (u16, u8) {
        let v = cpu.pop_stack();
        cpu.put(&Agu::Status, v);
        let pc = cpu.pop_stack() as u16 | (cpu.pop_stack() as u16) << 8;
        (pc, 6)
    }
}

impl InstructionType for Bit {
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let (v, operands, ticks) = cpu.get(&self.0);
        let v = v & cpu.a;
        cpu.set_flag(NegativeFlag, v & 0x80 != 0);
        cpu.set_flag(OverflowFlag, v & 0x70 != 0);
        cpu.update_zero_flag(v);
        (operands + 1, ticks + 2)
    }
}

fn decode(cpu: &Cpu) -> Instruction {
    let op_code = cpu.read_byte(cpu.pc);
    let a = op_code & 0b1110_0000 >> 5;
    let b = op_code & 0b0001_1100 >> 2;
    let c = op_code & 0b0000_0011;
    let read_u8 = || cpu.read_byte(cpu.pc.wrapping_add(1));
    let read_u16 = || cpu.read_word(cpu.pc.wrapping_add(1));
    let literal = || Agu::Literal(read_u8());
    let zero_page = || Agu::ZeroPage(read_u8());
    let zero_page_x = || Agu::ZeroPageX(read_u8());
    let absolute = || Absolute(read_u16());
    let absolute_x = || Agu::AbsoluteX(read_u16());
    let absolute_y = || Agu::AbsoluteY(read_u16());
    // let indirect = || Agu::Indirect(read_u16());
    let indirect_x = || Agu::IndirectX(read_u8());
    let indirect_y = || Agu::IndirectY(read_u8());
    let cond_branch = |agu| Instruction::ConditionBranch(ConditionBranch::new(read_u8(), agu, false));
    let neg_cond_branch = |agu| Instruction::ConditionBranch(ConditionBranch::new(read_u8(), agu, true));
    let transfer = |dest, src| Instruction::Transfer(Transfer { src, dest });
    let dec = |agu| Instruction::Dec(Dec(agu));
    let inc = |agu| Instruction::Inc(Inc(agu));
    let cmp = |register, memory| Instruction::Cmp(Cmp { register, memory });
    let (aa, x, y) = (Agu::RegisterA, Agu::RegisterX, Agu::RegisterY);
    let ora = |agu| Instruction::Ora(Ora(agu));
    let and = |agu| Instruction::And(And(agu));
    let eor = |agu| Instruction::Eor(Eor(agu));
    let adc = |agu| Instruction::Adc(Adc(agu));
    let sbc = |agu| Instruction::Sbc(Sbc(agu));

    match (c, a, b) {
        (0, 0, 0) => Instruction::Brk(Brk()),
        (0, 0, 2) => Instruction::Push(Push(Agu::Status)),
        (0, 0, 4) => neg_cond_branch(Agu::NegativeFlag),
        (0, 0, 6) => Instruction::Clc(Clc()),

        (0, 1, 0) => Instruction::Jsr(Jsr(read_u16())),
        (0, 1, 1) => Instruction::Bit(Bit(zero_page())),
        (0, 1, 2) => Instruction::Pop(Pop(Agu::Status)),
        (0, 1, 3) => Instruction::Bit(Bit(absolute())),
        (0, 1, 4) => cond_branch(Agu::NegativeFlag),
        (0, 1, 6) => Instruction::Sec(Sec()),

        (0, 2, 0) => Instruction::Rti(Rti()),
        (0, 2, 2) => Instruction::Push(Push(aa)),
        (0, 2, 3) => Instruction::Jmp(Jmp(read_u16())),
        (0, 2, 4) => neg_cond_branch(Agu::OverflowFlag),
        (0, 2, 6) => Instruction::Cli(Cli()),

        (0, 3, 0) => Instruction::Rts(Rts()),
        (0, 3, 2) => Instruction::Pop(Pop(aa)),
        (0, 3, 3) => Instruction::IndirectJmp(IndirectJmp(read_u16())),
        (0, 3, 4) => cond_branch(Agu::OverflowFlag),
        (0, 3, 6) => Instruction::Sei(Sei()),

        (0, 4, 1) => transfer(zero_page(), y),
        (0, 4, 2) => dec(Agu::RegisterY),
        (0, 4, 3) => transfer(absolute(), y),
        (0, 4, 4) => neg_cond_branch(Agu::CarryFlag),
        (0, 4, 5) => transfer(zero_page_x(), y),
        (0, 4, 6) => transfer(aa, y),

        (0, 5, 0) => transfer(y, literal()),
        (0, 5, 1) => transfer(y, zero_page()),
        (0, 5, 2) => transfer(y, aa),
        (0, 5, 3) => transfer(y, absolute()),
        (0, 5, 4) => cond_branch(Agu::CarryFlag),
        (0, 5, 5) => transfer(y, zero_page_x()),
        (0, 5, 6) => Instruction::Clv(Clv()),
        (0, 5, 7) => transfer(y, absolute_x()),

        (0, 6, 0) => cmp(y, literal()),
        (0, 6, 1) => cmp(y, zero_page()),
        (0, 6, 2) => inc(y),
        (0, 6, 3) => cmp(y, absolute()),
        (0, 6, 4) => neg_cond_branch(Agu::ZeroFlag),
        (0, 6, 6) => Instruction::Cld(Cld()),

        (0, 7, 0) => cmp(x, literal()),
        (0, 7, 1) => cmp(x, zero_page()),
        (0, 7, 2) => inc(x),
        (0, 7, 3) => cmp(x, absolute()),
        (0, 7, 4) => cond_branch(Agu::ZeroFlag),
        (0, 7, 6) => Instruction::Sed(Sed()),

        (1, 0, 0) => ora(indirect_x()),
        (1, 0, 1) => ora(zero_page()),
        (1, 0, 2) => ora(literal()),
        (1, 0, 3) => ora(absolute()),
        (1, 0, 4) => ora(indirect_y()),
        (1, 0, 5) => ora(zero_page_x()),
        (1, 0, 6) => ora(absolute_y()),
        (1, 0, 7) => ora(absolute_x()),

        (1, 1, 0) => and(indirect_x()),
        (1, 1, 1) => and(zero_page()),
        (1, 1, 2) => and(literal()),
        (1, 1, 3) => and(absolute()),
        (1, 1, 4) => and(indirect_y()),
        (1, 1, 5) => and(zero_page_x()),
        (1, 1, 6) => and(absolute_y()),
        (1, 1, 7) => and(absolute_x()),

        (1, 2, 0) => eor(indirect_x()),
        (1, 2, 1) => eor(zero_page()),
        (1, 2, 2) => eor(literal()),
        (1, 2, 3) => eor(absolute()),
        (1, 2, 4) => eor(indirect_y()),
        (1, 2, 5) => eor(zero_page_x()),
        (1, 2, 6) => eor(absolute_y()),
        (1, 2, 7) => eor(absolute_x()),

        (1, 3, 0) => adc(indirect_x()),
        (1, 3, 1) => adc(zero_page()),
        (1, 3, 2) => adc(literal()),
        (1, 3, 3) => adc(absolute()),
        (1, 3, 4) => adc(indirect_y()),
        (1, 3, 5) => adc(zero_page_x()),
        (1, 3, 6) => adc(absolute_y()),
        (1, 3, 7) => adc(absolute_x()),

        (1, 4, 0) => transfer(indirect_x(), aa),
        (1, 4, 1) => transfer(zero_page(), aa),
        (1, 4, 3) => transfer(absolute(), aa),
        (1, 4, 4) => transfer(indirect_y(), aa),
        (1, 4, 5) => transfer(zero_page_x(), aa),
        (1, 4, 6) => transfer(absolute_y(), aa),
        (1, 4, 7) => transfer(absolute_x(), aa),

        (1, 5, 0) => transfer(aa, indirect_x()),
        (1, 5, 1) => transfer(aa, zero_page()),
        (1, 5, 2) => transfer(aa, literal()),
        (1, 5, 3) => transfer(aa, absolute()),
        (1, 5, 4) => transfer(aa, indirect_y()),
        (1, 5, 5) => transfer(aa, zero_page_x()),
        (1, 5, 6) => transfer(aa, absolute_y()),
        (1, 5, 7) => transfer(aa, absolute_x()),

        (1, 6, 0) => cmp(aa, indirect_x()),
        (1, 6, 1) => cmp(aa, zero_page()),
        (1, 6, 2) => cmp(aa, literal()),
        (1, 6, 3) => cmp(aa, absolute()),
        (1, 6, 4) => cmp(aa, indirect_y()),
        (1, 6, 5) => cmp(aa, zero_page_x()),
        (1, 6, 6) => cmp(aa, absolute_y()),
        (1, 6, 7) => cmp(aa, absolute_x()),

        (1, 7, 0) => sbc(indirect_x()),
        (1, 7, 1) => sbc(zero_page()),
        (1, 7, 2) => sbc(literal()),
        (1, 7, 3) => sbc(absolute()),
        (1, 7, 4) => sbc(indirect_y()),
        (1, 7, 5) => sbc(zero_page_x()),
        (1, 7, 6) => sbc(absolute_y()),
        (1, 7, 7) => sbc(absolute_x()),

        _ => panic_any(format!("Unknown opcode: {:02x}", op_code)),
    }
}

trait FlagBit {
    const BIT: u8;
}

struct CarryFlag;

struct DecimalModeFlag;

struct InterruptDisableFlag;

struct ZeroFlag;

struct BreakFlag;

struct OverflowFlag;

struct NegativeFlag;

impl FlagBit for NegativeFlag { const BIT: u8 = 0x80; }

impl FlagBit for OverflowFlag { const BIT: u8 = 0x40; }

impl FlagBit for BreakFlag { const BIT: u8 = 0x10; }

impl FlagBit for DecimalModeFlag { const BIT: u8 = 0x8; }

impl FlagBit for InterruptDisableFlag { const BIT: u8 = 0x4; }

impl FlagBit for CarryFlag { const BIT: u8 = 0x1; }

impl FlagBit for ZeroFlag { const BIT: u8 = 0x2; }

// Trait to sync instruction execution  times.
trait SyncInstructionCycle {
    fn start(&mut self);

    fn end(&mut self, cycles: u8);
}

#[allow(dead_code)]
struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    status: u8,
    memory: [u8; 0x10000],
}

impl Cpu {
    fn new(pc: u16) -> Cpu {
        Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc,
            sp: 0,
            status: 0,
            memory: [0; 0x10000],
        }
    }

    fn flag<T: FlagBit>(&self, _: T) -> bool {
        (self.status & T::BIT) != 0
    }

    fn set_flag<T: FlagBit>(&mut self, _: T, value: bool) {
        if value {
            self.status |= T::BIT;
        } else {
            self.status &= !T::BIT;
        }
    }

    fn execute<T: SyncInstructionCycle>(&mut self, inst: Instruction, cycle_sync: &mut T) {
        cycle_sync.start();
        let mut absolute_pc: i32 = -1;
        let (pc, cycles) = match inst {
            Instruction::Transfer(inst) => inst.execute(self),
            Instruction::TransferNoTouchFlags(inst) => inst.execute(self),
            Instruction::Txs(inst) => inst.execute(self),
            Instruction::Push(inst) => inst.execute(self),
            Instruction::Pop(inst) => inst.execute(self),
            Instruction::Dec(inst) => inst.execute(self),
            Instruction::Inc(inst) => inst.execute(self),
            Instruction::Adc(inst) => inst.execute(self),
            Instruction::Sbc(inst) => inst.execute(self),
            Instruction::And(inst) => inst.execute(self),
            Instruction::Eor(inst) => inst.execute(self),
            Instruction::Ora(inst) => inst.execute(self),
            Instruction::Asl(inst) => inst.execute(self),
            Instruction::Lsr(inst) => inst.execute(self),
            Instruction::Rol(inst) => inst.execute(self),
            Instruction::Ror(inst) => inst.execute(self),
            Instruction::Clc(inst) => inst.execute(self),
            Instruction::Cld(inst) => inst.execute(self),
            Instruction::Cli(inst) => inst.execute(self),
            Instruction::Clv(inst) => inst.execute(self),
            Instruction::Sec(inst) => inst.execute(self),
            Instruction::Sed(inst) => inst.execute(self),
            Instruction::Sei(inst) => inst.execute(self),
            Instruction::Cmp(inst) => inst.execute(self),
            Instruction::ConditionBranch(inst) => inst.execute(self),
            Instruction::Jmp(jmp) => {
                let (addr, ticks) = jmp.execute();
                absolute_pc = addr as i32;
                (1, ticks)
            }
            Instruction::IndirectJmp(jmp) => {
                let (addr, ticks) = jmp.execute(self);
                absolute_pc = addr as i32;
                (1, ticks)
            }
            Instruction::Jsr(jmp) => {
                let (addr, ticks) = jmp.execute(self);
                absolute_pc = addr as i32;
                (1, ticks)
            }
            Instruction::Rts(jmp) => {
                let (addr, ticks) = jmp.execute(self);
                absolute_pc = addr as i32;
                (1, ticks)
            }
            Instruction::Brk(jmp) => {
                let (addr, ticks) = jmp.execute(self);
                absolute_pc = addr as i32;
                (1, ticks)
            }
            Instruction::Rti(jmp) => {
                let (addr, ticks) = jmp.execute(self);
                absolute_pc = addr as i32;
                (1, ticks)
            }
            Instruction::Bit(inst) => inst.execute(self),
            Instruction::Nop => (1, 2),
        };

        if absolute_pc != -1 {
            self.pc = absolute_pc as u16;
        } else {
            self.inc_pc(pc);
        }

        cycle_sync.end(cycles);
    }

    fn inc_pc(&mut self, delta: u8) {
        self.pc = self.pc.wrapping_add(delta as u16);
    }

    fn update_negative_flag<T: BitAnd<Output=T> + Copy + From<u8> + PartialEq + Default>(&mut self, value: T) {
        self.set_flag(NegativeFlag, value & T::from(0x80) != T::default());
    }

    fn update_zero_flag<T: PartialEq + Copy + Default>(&mut self, value: T) {
        self.set_flag(ZeroFlag, value == T::default());
    }

    fn read_byte(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn write_byte(&mut self, addr: u16, value: u8) {
        self.memory[addr as usize] = value;
    }

    fn read_word(&self, addr: u16) -> u16 {
        (self.read_byte(addr) as u16) | ((self.read_byte(addr.wrapping_add(1)) as u16) << 8)
    }

    fn read_zero_page_word(&self, addr: u8) -> u16 {
        (self.read_byte(addr as u16) as u16) | ((self.read_byte(addr.wrapping_add(1) as u16) as u16) << 8)
    }

    fn write_word(&mut self, addr: u16, value: u16) {
        self.write_byte(addr, (value & 0xff) as u8);
        self.write_byte(addr.wrapping_add(1), ((value >> 8) & 0xff) as u8);
    }

    /// Return (value, operand bytes, address ticks)
    fn get(&self, agu: &Agu) -> (u8, u8, u8) {
        match agu {
            &Agu::Literal(val) => (val, 1, 0),
            &Agu::RegisterA => (self.a, 0, 0),
            &Agu::RegisterX => (self.x, 0, 0),
            &Agu::RegisterY => (self.y, 0, 0),
            &Agu::RegisterSP => (self.sp, 0, 0),
            &Agu::Status => {
                (self.status | 0b0011_0000, 0, 0)
            }
            &Agu::CarryFlag => (self.flag(CarryFlag) as u8, 0, 0),
            &Agu::ZeroFlag => (self.flag(ZeroFlag) as u8, 0, 0),
            &Agu::NegativeFlag => (self.flag(NegativeFlag) as u8, 0, 0),
            &Agu::OverflowFlag => (self.flag(OverflowFlag) as u8, 0, 0),
            _ => {
                let (addr, operand_bytes, ticks) = agu.address(self);
                (self.read_byte(addr), operand_bytes, ticks)
            }
        }
    }

    /// Return (operand bytes, address ticks)
    fn put(&mut self, agu: &Agu, value: u8) -> (u8, u8) {
        match agu {
            &Agu::Literal(_) => panic_any("Literal not supported"),
            &Agu::RegisterA => {
                self.a = value;
                (0, 0)
            }
            &Agu::RegisterX => {
                self.x = value;
                (0, 0)
            }
            &Agu::RegisterY => {
                self.y = value;
                (0, 0)
            }
            &Agu::RegisterSP => {
                self.sp = value;
                (0, 0)
            }
            &Agu::Status => {
                let saved = self.status & 0b0011_0000;
                self.status = value & 0b1100_1111 | saved;
                (0, 0)
            }
            _ => {
                let (addr, operand_bytes, ticks) = agu.address(self);
                self.write_byte(addr, value);
                (operand_bytes, ticks)
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
}
