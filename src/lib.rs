use std::ops::{BitAnd};
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
                let high = (addr >> 8) as u8;
                let r = addr.wrapping_add(cpu.x as u16) as u16;
                (r, if high == (r >> 8) as u8 { 2 } else { 3 }, 2)
            }
            &Agu::AbsoluteY(addr) => {
                let high = (addr >> 8) as u8;
                let r = addr.wrapping_add(cpu.y as u16) as u16;
                (r, if high == (r >> 8) as u8 { 2 } else { 3 }, 2)
            }
            &Agu::Indirect(addr) => (cpu.read_word(addr), 2, 2),
            &Agu::IndirectX(addr) => (cpu.read_zero_page_word(addr.wrapping_add(cpu.x)), 4, 1),
            &Agu::IndirectY(addr) => {
                let addr = cpu.read_zero_page_word(addr);
                let high = (addr >> 8) as u8;
                let r = addr.wrapping_add(cpu.y as u16);
                (r, if high == (r >> 8) as u8 { 3 } else { 4 }, 1)
            }
            Agu::RegisterA => panic_any("RegisterA not supported"),
            Agu::RegisterX => panic_any("RegisterX not supported"),
            Agu::RegisterY => panic_any("RegisterY not supported"),
            Agu::RegisterSP => panic_any("RegisterSP not supported"),
            Agu::Status => panic_any("Status not supported"),
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

trait InstructionType {
    // (pcDelta, tickCount)
    fn execute(&self, cpu: &mut Cpu) -> (u8, u8);
}

/// Bin operate with A register and value from Agu, store result to A register.
trait AccumulatorOpInstructionType {
    fn agu(&self)->&Agu;
    fn op(a: u8, v: u8)->u8;

    fn execute(&self, cpu: &mut Cpu) -> (u8, u8) {
        let agu = self.agu();
        let (val, operands, ticks) = cpu.get(agu);
        cpu.a = Self::op(cpu.a, val);
        cpu.update_negative_flag(cpu.a);
        cpu.update_zero_flag(cpu.a);
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

trait FlagBit {
    const BIT: u8;
}

struct CarryFlag;

struct ZeroFlag;

struct BreakFlag;

struct OverflowFlag;

struct NegativeFlag;

impl FlagBit for NegativeFlag { const BIT: u8 = 0x80; }

impl FlagBit for OverflowFlag { const BIT: u8 = 0x40; }

impl FlagBit for BreakFlag { const BIT: u8 = 0x10; }

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
        };
        cycle_sync.end(cycles);

        self.inc_pc(pc);
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
