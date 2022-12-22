use log::debug;
use std::convert::From;
use std::ops::BitAnd;

mod addressing;
mod instruction;
pub mod mcu_mem;
pub mod nes;

fn is_cross_page(a: u16, b: u16) -> bool {
    let a = (a >> 8) as u8;
    let b = (b >> 8) as u8;
    a != b
}

fn extra_tick_if_cross_page(a: u16, b: u16) -> u8 {
    if is_cross_page(a, b) {
        1
    } else {
        0
    }
}

fn execute_next(cpu: &mut Cpu) -> u8 {
    use addressing::*;
    use instruction::*;

    let op_code = cpu.inc_read_byte();
    let a = (op_code & 0b1110_0000) >> 5;
    let b = (op_code & 0b0001_1100) >> 2;
    let c = op_code & 0b0000_0011;
    debug!("op_code: {:0x} {} {} {}", op_code, c, a, b);

    fn r_b(cpu: &mut Cpu) -> u8 {
        cpu.inc_read_byte()
    }
    fn r_w(cpu: &mut Cpu) -> u16 {
        cpu.inc_read_word()
    }

    let zero_page = |cpu: &mut Cpu| ZeroPage(r_b(cpu));
    let zero_page_x = |cpu: &mut Cpu| ZeroPageX(r_b(cpu));
    let zero_page_y = |cpu: &mut Cpu| ZeroPageY(r_b(cpu));
    let absolute = |cpu: &mut Cpu| Absolute(r_w(cpu));
    let absolute_x = |cpu: &mut Cpu| AbsoluteX(r_w(cpu));
    let absolute_y = |cpu: &mut Cpu| AbsoluteY(r_w(cpu));
    let literal = |cpu: &mut Cpu| Literal(r_b(cpu));
    let cond_branch =
        |cpu: &mut Cpu, flag: Flag| new_condition_branch(r_b(cpu) as i8, FlagAddr(flag), false);
    let neg_cond_branch =
        |cpu: &mut Cpu, flag: Flag| new_condition_branch(r_b(cpu) as i8, FlagAddr(flag), true);
    let indirect_x = |cpu: &mut Cpu| IndirectX(r_b(cpu));
    let indirect_y = |cpu: &mut Cpu| IndirectY(r_b(cpu));
    let x = RegisterX;
    let y = RegisterY;
    let aa = RegisterA;
    let sp = RegisterSP;

    match (c, a, b) {
        (0, 0, 0) => new_brk()(cpu),
        (0, 0, 2) => new_push(RegisterStatus())(cpu),
        (0, 0, 4) => neg_cond_branch(cpu, Flag::Negative)(cpu),
        (0, 0, 6) => new_clear_bit(FlagAddr(Flag::Carry))(cpu),

        (0, 1, 0) => new_jsr(r_w(cpu))(cpu),
        (0, 1, 1) => new_bit(zero_page(cpu))(cpu),
        (0, 1, 2) => new_pop(RegisterStatus())(cpu),
        (0, 1, 3) => new_bit(absolute(cpu))(cpu),
        (0, 1, 4) => cond_branch(cpu, Flag::Negative)(cpu),
        (0, 1, 6) => new_set_bit(FlagAddr(Flag::Carry))(cpu),

        (0, 2, 0) => new_rti()(cpu),
        (0, 2, 2) => new_push(aa())(cpu),
        (0, 2, 3) => new_jmp(r_w(cpu))(cpu),
        (0, 2, 4) => neg_cond_branch(cpu, Flag::Overflow)(cpu),
        (0, 2, 6) => new_clear_bit(FlagAddr(Flag::Interrupt))(cpu),

        (0, 3, 0) => new_rts()(cpu),
        (0, 3, 2) => new_pop(aa())(cpu),
        (0, 3, 3) => new_indirect_jmp(r_w(cpu))(cpu),
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

#[deprecated]
pub trait FlagBit {
    const BIT: u8;
}

#[deprecated]
pub struct CarryFlag;

#[deprecated]
pub struct DecimalModeFlag;

#[deprecated]
pub struct InterruptDisableFlag;

#[deprecated]
pub struct ZeroFlag;

#[deprecated]
pub struct BreakFlag;

#[deprecated]
pub struct OverflowFlag;

#[deprecated]
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

    #[deprecated]
    pub fn flag<T: FlagBit>(&self, _: T) -> bool {
        (self.status & T::BIT) != 0
    }

    #[deprecated]
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
