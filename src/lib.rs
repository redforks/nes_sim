use std::ops::{BitAnd, Shl};
use std::convert::From;
use std::panic::panic_any;

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
            &Agu::Absolute(addr) => (addr, 2, 2),
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
    Brk(Brk),
}

struct Transfer {
    src: Agu,
    dest: Agu,
}

struct TransferNoTouchFlags {
    src: Agu,
    dest: Agu,
}

struct Txs();

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

struct Clc();

struct Cld();

struct Cli();

struct Clv();

struct Sec();

struct Sed();

struct Sei();

struct Cmp {
    register: Agu,
    memory: Agu,
}

struct ConditionBranch {
    offset: u8,
    register: Agu,
    negative: bool,
}

struct Jmp(u16);

struct IndirectJmp(u16);

struct Jsr(u16);

struct Brk();

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

impl Brk {
    fn execute(&self, cpu: &mut Cpu) -> (u16, u8) {
        let pc = cpu.pc.wrapping_add(2);
        cpu.push_stack((pc >> 8) as u8);
        cpu.push_stack(pc as u8);
        let (status, ..) = cpu.get(Agu.Status);
        cpu.push_stack(status);
        cpu.set_flag(BreakFlag, true);
        cpu.set_flag(UnusedFlag, true);
        (cpu.read_word(0xFFFE), 7)
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
        let mut absolute_pc: i32  = -1;
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
            },
            Instruction::IndirectJmp(jmp) => {
                let (addr, ticks) = jmp.execute(self);
                absolute_pc = addr as i32;
                (1, ticks)
            },
            Instruction::Jsr(jmp) => {
                let (addr, ticks) = jmp.execute(self);
                absolute_pc = addr as i32;
                (1, ticks)
            },
            Instruction::Brk(jmp) => {
                let (addr, ticks) = jmp.execute(self);
                absolute_pc = addr as i32;
                (1, ticks)
            },
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
