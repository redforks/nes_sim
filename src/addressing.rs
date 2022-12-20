use super::{is_cross_page, Cpu};

pub trait Address {
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
struct Literal(u8);

impl Address for Literal {
    fn get(&self, _: &Cpu) -> (u8, u8) {
        (self.0, 0)
    }

    fn set(&self, _: &mut Cpu, _: u8) -> u8 {
        panic!("can't set value to literal");
    }
}

#[derive(Clone, Copy)]
struct ZeroPage(u8);

impl Address for ZeroPage {
    fn calc_addr(&self, _: &Cpu) -> (u16, u8) {
        (self.0 as u16, 1)
    }
}

#[derive(Clone, Copy)]
struct Absolute(u16);

impl Address for Absolute {
    fn calc_addr(&self, _: &Cpu) -> (u16, u8) {
        (self.0, 2)
    }
}

#[derive(Clone, Copy)]
struct ZeroPageX(u8);

impl Address for ZeroPageX {
    fn calc_addr(&self, cpu: &Cpu) -> (u16, u8) {
        (self.0.wrapping_add(cpu.x) as u16, 2)
    }
}

#[derive(Clone, Copy)]
struct ZeroPageY(u8);

impl Address for ZeroPageY {
    fn calc_addr(&self, cpu: &Cpu) -> (u16, u8) {
        (self.0.wrapping_add(cpu.y) as u16, 2)
    }
}

#[derive(Clone, Copy)]
struct AbsoluteX(u16);

impl Address for AbsoluteX {
    fn calc_addr(&self, cpu: &Cpu) -> (u16, u8) {
        let r = self.0.wrapping_add(cpu.x as u16);
        (r, 2 + extra_tick_if_cross_page(self.0, r))
    }
}

#[derive(Clone, Copy)]
struct AbsoluteY(u16);

impl Address for AbsoluteY {
    fn calc_addr(&self, cpu: &Cpu) -> (u16, u8) {
        let r = self.0.wrapping_add(cpu.y as u16);
        (r, 2 + extra_tick_if_cross_page(self.0, r))
    }
}

#[derive(Clone, Copy)]
struct IndirectX(u8);

impl Address for IndirectX {
    fn calc_addr(&self, cpu: &Cpu) -> (u16, u8) {
        let addr = self.0.wrapping_add(cpu.x);
        (cpu.read_zero_page_word(addr), 4)
    }
}

#[derive(Clone, Copy)]
struct IndirectY(u8);

impl Address for IndirectY {
    fn calc_addr(&self, cpu: &Cpu) -> (u16, u8) {
        let addr = cpu.read_zero_page_word(self.0);
        let r = addr.wrapping_add(cpu.y as u16);
        (r, 3 + extra_tick_if_cross_page(addr, r))
    }
}

#[derive(Clone, Copy)]
pub struct RegisterA();

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
struct RegisterX();

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
struct RegisterY();

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
struct RegisterSP();

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
