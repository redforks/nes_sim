use super::{is_cross_page, Cpu};
use std::fmt::{Display, Formatter};

pub trait Address: Display {
    /// return (value, ticks)
    fn get(&self, cpu: &Cpu) -> (u8, u8) {
        let (addr, ticks) = self.calc_addr(cpu);
        (cpu.read_byte(addr), ticks)
    }

    /// return ticks
    fn set(&self, cpu: &mut Cpu, val: u8) -> u8 {
        let (addr, ticks) = self.calc_addr(cpu);
        cpu.write_byte(addr, val);
        ticks
    }

    fn calc_addr(&self, _: &Cpu) -> (u16, u8) {
        panic!("calc_addr not implemented");
    }

    fn is_register(&self) -> bool {
        false
    }
}

#[derive(Clone, Copy)]
pub struct Literal(pub u8);

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "literal {}", self.0)
    }
}

impl Address for Literal {
    fn get(&self, _: &Cpu) -> (u8, u8) {
        (self.0, 0)
    }

    fn set(&self, _: &mut Cpu, _: u8) -> u8 {
        panic!("can't set value to literal");
    }
}

#[derive(Clone, Copy)]
pub struct ZeroPage(pub u8);

impl Display for ZeroPage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ZeroPage ${:02X}", self.0)
    }
}

impl Address for ZeroPage {
    fn calc_addr(&self, _: &Cpu) -> (u16, u8) {
        (self.0 as u16, 1)
    }
}

#[derive(Clone, Copy)]
pub struct Absolute(pub u16);

impl Display for Absolute {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Absolute ${:04X}", self.0)
    }
}

impl Address for Absolute {
    fn calc_addr(&self, _: &Cpu) -> (u16, u8) {
        (self.0, 2)
    }
}

#[derive(Clone, Copy)]
pub struct ZeroPageX(pub u8);

impl Display for ZeroPageX {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ZeroPageX ${:02X}", self.0)
    }
}

impl Address for ZeroPageX {
    fn calc_addr(&self, cpu: &Cpu) -> (u16, u8) {
        (self.0.wrapping_add(cpu.x) as u16, 2)
    }
}

#[derive(Clone, Copy)]
pub struct ZeroPageY(pub u8);

impl Display for ZeroPageY {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ZeroPageY ${:02X}", self.0)
    }
}

impl Address for ZeroPageY {
    fn calc_addr(&self, cpu: &Cpu) -> (u16, u8) {
        (self.0.wrapping_add(cpu.y) as u16, 2)
    }
}

#[derive(Clone, Copy)]
pub struct AbsoluteX(pub u16);

impl Display for AbsoluteX {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "AbsoluteX ${:04X}", self.0)
    }
}

impl Address for AbsoluteX {
    fn calc_addr(&self, cpu: &Cpu) -> (u16, u8) {
        let r = self.0.wrapping_add(cpu.x as u16);
        (r, 2 + extra_tick_if_cross_page(self.0, r))
    }
}

#[derive(Clone, Copy)]
pub struct AbsoluteY(pub u16);

impl Display for AbsoluteY {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "AbsoluteY ${:04X}", self.0)
    }
}

impl Address for AbsoluteY {
    fn calc_addr(&self, cpu: &Cpu) -> (u16, u8) {
        let r = self.0.wrapping_add(cpu.y as u16);
        (r, 2 + extra_tick_if_cross_page(self.0, r))
    }
}

#[derive(Clone, Copy)]
pub struct IndirectX(pub u8);

impl Display for IndirectX {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "IndirectX ${:02X}", self.0)
    }
}

impl Address for IndirectX {
    fn calc_addr(&self, cpu: &Cpu) -> (u16, u8) {
        let addr = self.0.wrapping_add(cpu.x);
        (cpu.read_zero_page_word(addr), 4)
    }
}

#[derive(Clone, Copy)]
pub struct IndirectY(pub u8);

impl Display for IndirectY {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "IndirectY ${:02X}", self.0)
    }
}

impl Address for IndirectY {
    fn calc_addr(&self, cpu: &Cpu) -> (u16, u8) {
        let addr = cpu.read_zero_page_word(self.0);
        let r = addr.wrapping_add(cpu.y as u16);
        (r, 3 + extra_tick_if_cross_page(addr, r))
    }
}

#[derive(Clone, Copy)]
pub struct RegisterA();

impl Display for RegisterA {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "RegisterA")
    }
}

impl Address for RegisterA {
    fn get(&self, cpu: &Cpu) -> (u8, u8) {
        (cpu.a, 0)
    }

    fn set(&self, cpu: &mut Cpu, val: u8) -> u8 {
        cpu.a = val;
        0
    }
}

#[derive(Clone, Copy)]
pub struct RegisterX();

impl Display for RegisterX {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "RegisterX")
    }
}

impl Address for RegisterX {
    fn get(&self, cpu: &Cpu) -> (u8, u8) {
        (cpu.x, 0)
    }

    fn set(&self, cpu: &mut Cpu, val: u8) -> u8 {
        cpu.x = val;
        0
    }

    fn is_register(&self) -> bool {
        true
    }
}

#[derive(Clone, Copy)]
pub struct RegisterY();

impl Display for RegisterY {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "RegisterY")
    }
}

impl Address for RegisterY {
    fn get(&self, cpu: &Cpu) -> (u8, u8) {
        (cpu.y, 0)
    }

    fn set(&self, cpu: &mut Cpu, val: u8) -> u8 {
        cpu.y = val;
        0
    }

    fn is_register(&self) -> bool {
        true
    }
}

#[derive(Clone, Copy)]
pub struct RegisterSP();

impl Display for RegisterSP {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "RegisterSP")
    }
}

impl Address for RegisterSP {
    fn get(&self, cpu: &Cpu) -> (u8, u8) {
        (cpu.sp, 0)
    }

    fn set(&self, cpu: &mut Cpu, val: u8) -> u8 {
        cpu.sp = val;
        0
    }

    fn is_register(&self) -> bool {
        true
    }
}

#[derive(Clone, Copy)]
pub struct RegisterStatus();

impl Display for RegisterStatus {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "RegisterStatus")
    }
}

impl Address for RegisterStatus {
    fn get(&self, cpu: &Cpu) -> (u8, u8) {
        (cpu.status | 0b0011_0000, 0)
    }

    fn set(&self, cpu: &mut Cpu, val: u8) -> u8 {
        cpu.status = val & 0b1100_1111;
        0
    }

    fn is_register(&self) -> bool {
        true
    }
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum Flag {
    Carry = 0x01u8,
    Zero = 0x02u8,
    Interrupt = 0x04u8,
    Decimal = 0x08u8,
    Break = 0x10u8,
    Overflow = 0x40u8,
    Negative = 0x80u8,
}

#[derive(Clone, Copy)]
pub struct FlagAddr(pub Flag);

impl Display for FlagAddr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Address for FlagAddr {
    fn get(&self, cpu: &Cpu) -> (u8, u8) {
        (cpu.status & self.0 as u8, 0)
    }

    fn set(&self, cpu: &mut Cpu, val: u8) -> u8 {
        if val == 0 {
            cpu.status &= !(self.0 as u8);
        } else {
            cpu.status |= self.0 as u8;
        }
        0
    }

    fn is_register(&self) -> bool {
        true
    }
}

fn extra_tick_if_cross_page(a: u16, b: u16) -> u8 {
    if is_cross_page(a, b) {
        1
    } else {
        0
    }
}
