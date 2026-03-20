use super::{extra_tick_if_cross_page, Cpu, Flag};
use crate::mcu::Mcu;
use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BranchAddressing {
    Relative(u8),
    Absolute(u16),
    AbsoluteIndirect(u16),
}

impl Display for BranchAddressing {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BranchAddressing::Relative(offset) => write!(f, "${:02x}", offset),
            BranchAddressing::Absolute(addr) => write!(f, "${:04x}", addr),
            BranchAddressing::AbsoluteIndirect(addr) => write!(f, "(${:04x})", addr),
        }
    }
}

impl BranchAddressing {
    pub fn calc_addr<M: Mcu>(&self, cpu: &mut Cpu<M>) -> u16 {
        match self {
            BranchAddressing::Relative(offset) => cpu.pc.wrapping_add((*offset as i8) as u16),
            BranchAddressing::Absolute(addr) => *addr,
            BranchAddressing::AbsoluteIndirect(addr) => cpu.read_word_in_same_page(*addr),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Addressing {
    Accumulator,
    Implied,
    Immediate(u8),
    Absolute(u16),
    ZeroPage(u8),
    AbsoluteIndexedWithX(u16),
    AbsoluteIndexedWithY(u16),
    ZeroPageIndexedWithX(u8),
    ZeroPageIndexedWithY(u8),
    ZeroPageIndexedIndirect(u8),
    ZeroPageIndexedIndirectWithY(u8),
    // RegisterA variant kept for test compatibility; runtime uses Accumulator
    RegisterA,
    RegisterX,
    RegisterY,
    RegisterSP,
}

impl Display for Addressing {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Addressing::Accumulator => write!(f, ""),
            Addressing::Implied => write!(f, ""),
            Addressing::Immediate(v) => write!(f, "#${:02x}", v),
            Addressing::Absolute(a) => write!(f, "${:04x}", a),
            Addressing::ZeroPage(z) => write!(f, "${:02x}", z),
            Addressing::AbsoluteIndexedWithX(a) => write!(f, "${:04x},X", a),
            Addressing::AbsoluteIndexedWithY(a) => write!(f, "${:04x},Y", a),
            Addressing::ZeroPageIndexedWithX(z) => write!(f, "${:02x},X", z),
            Addressing::ZeroPageIndexedWithY(z) => write!(f, "${:02x},Y", z),
            Addressing::ZeroPageIndexedIndirect(z) => write!(f, "(${:02x},X)", z),
            Addressing::ZeroPageIndexedIndirectWithY(z) => write!(f, "(${:02x}),Y", z),
            Addressing::RegisterA => write!(f, "A"),
            Addressing::RegisterX => write!(f, "X"),
            Addressing::RegisterY => write!(f, "Y"),
            Addressing::RegisterSP => write!(f, "SP"),
            // register status removed from Addressing; use `RegisterStatus` type when
            // accessing the full status register behavior.
        }
    }
}

impl Addressing {
    pub fn is_register(&self) -> bool {
        matches!(
            self,
            Addressing::Accumulator
                | Addressing::RegisterX
                | Addressing::RegisterY
                | Addressing::RegisterSP
        )
    }

    pub fn calc_addr<M: Mcu>(&self, cpu: &mut Cpu<M>) -> (u16, u8) {
        match self {
            Addressing::Accumulator => (cpu.a as u16, 1),
            Addressing::Implied => panic!("can't calc addr for implied addressing"),
            Addressing::Immediate(v) => (*v as u16, 1),
            Addressing::Absolute(a) => (*a, 2),
            Addressing::ZeroPage(z) => (*z as u16, 1),
            Addressing::AbsoluteIndexedWithX(a) => {
                let r = a.wrapping_add(cpu.x as u16);
                (r, 2 + extra_tick_if_cross_page(*a, r))
            }
            Addressing::AbsoluteIndexedWithY(a) => {
                let r = a.wrapping_add(cpu.y as u16);
                (r, 2 + extra_tick_if_cross_page(*a, r))
            }
            Addressing::ZeroPageIndexedWithX(z) => (z.wrapping_add(cpu.x) as u16, 2),
            Addressing::ZeroPageIndexedWithY(z) => (z.wrapping_add(cpu.y) as u16, 2),
            Addressing::ZeroPageIndexedIndirect(z) => {
                (cpu.read_zero_page_word(cpu.x.wrapping_add(*z)), 4)
            }
            Addressing::ZeroPageIndexedIndirectWithY(z) => {
                let base = cpu.read_zero_page_word(*z);
                let r = base.wrapping_add(cpu.y as u16);
                (r, 3 + extra_tick_if_cross_page(base, r))
            }
            Addressing::RegisterA => (cpu.a as u16, 1),
            Addressing::RegisterX => (cpu.x as u16, 1),
            Addressing::RegisterY => (cpu.y as u16, 1),
            Addressing::RegisterSP => (cpu.sp as u16, 1),
            // RegisterStatus removed from Addressing
        }
    }

    /// Return value and extra cycles used
    pub fn read<M: Mcu>(&self, cpu: &mut Cpu<M>) -> (u8, u8) {
        match self {
            Addressing::Accumulator => (cpu.a, 0),
            Addressing::Implied => unreachable!("can't read implied addressing"),
            Addressing::Immediate(v) => (*v, 0),
            Addressing::ZeroPage(z) => {
                let addr = *z as u16;
                (cpu.read_byte(addr), 0)
            }
            Addressing::Absolute(a) => {
                let addr = *a;
                (cpu.read_byte(addr), 0)
            }
            Addressing::AbsoluteIndexedWithX(a) => {
                let base = *a;
                let r = base.wrapping_add(cpu.x as u16);
                (cpu.read_byte(r), 4 + extra_tick_if_cross_page(base, r))
            }
            Addressing::AbsoluteIndexedWithY(a) => {
                let base = *a;
                let r = base.wrapping_add(cpu.y as u16);
                (cpu.read_byte(r), 4 + extra_tick_if_cross_page(base, r))
            }
            Addressing::ZeroPageIndexedWithX(z) => {
                let addr = z.wrapping_add(cpu.x) as u16;
                (cpu.read_byte(addr), 4)
            }
            Addressing::ZeroPageIndexedWithY(z) => {
                let addr = z.wrapping_add(cpu.y) as u16;
                (cpu.read_byte(addr), 4)
            }
            Addressing::ZeroPageIndexedIndirect(z) => {
                // (zp, X)
                let addr = cpu.read_zero_page_word(cpu.x.wrapping_add(*z));
                (cpu.read_byte(addr), 6)
            }
            Addressing::ZeroPageIndexedIndirectWithY(z) => {
                // (zp), Y
                let base = cpu.read_zero_page_word(*z);
                let r = base.wrapping_add(cpu.y as u16);
                (cpu.read_byte(r), 5 + extra_tick_if_cross_page(base, r))
            }
            Addressing::RegisterA => (cpu.a, 1),
            Addressing::RegisterX => (cpu.x, 1),
            Addressing::RegisterY => (cpu.y, 1),
            Addressing::RegisterSP => (cpu.sp, 1),
            // RegisterStatus removed from Addressing
        }
    }

    /// Write value and return extra cycles it takes
    pub fn write<M: Mcu>(&self, cpu: &mut Cpu<M>, val: u8) -> u8 {
        // Return the number of cycles required to perform a write to this addressing
        // mode. Some addressing modes (like Immediate) are invalid targets for
        // writes and will panic.
        match self {
            Addressing::Accumulator => {
                cpu.a = val;
                0
            }
            Addressing::Implied => unreachable!("can't write to implied addressing"),
            Addressing::Immediate(_) => unreachable!("can't write to immediate addressing"),
            Addressing::ZeroPage(z) => {
                let addr = *z as u16;
                cpu.write_byte(addr, val);
                0
            }
            Addressing::Absolute(a) => {
                cpu.write_byte(*a, val);
                0
            }
            Addressing::AbsoluteIndexedWithX(a) => {
                let addr = a.wrapping_add(cpu.x as u16);
                cpu.write_byte(addr, val);
                5
            }
            Addressing::AbsoluteIndexedWithY(a) => {
                let addr = a.wrapping_add(cpu.y as u16);
                cpu.write_byte(addr, val);
                5
            }
            Addressing::ZeroPageIndexedWithX(z) => {
                let addr = z.wrapping_add(cpu.x) as u16;
                cpu.write_byte(addr, val);
                4
            }
            Addressing::ZeroPageIndexedWithY(z) => {
                let addr = z.wrapping_add(cpu.y) as u16;
                cpu.write_byte(addr, val);
                4
            }
            Addressing::ZeroPageIndexedIndirect(z) => {
                // (zp, X)
                let addr = cpu.read_zero_page_word(cpu.x.wrapping_add(*z));
                cpu.write_byte(addr, val);
                6
            }
            Addressing::ZeroPageIndexedIndirectWithY(z) => {
                // (zp), Y
                let base = cpu.read_zero_page_word(*z);
                let addr = base.wrapping_add(cpu.y as u16);
                cpu.write_byte(addr, val);
                6
            }
            Addressing::RegisterA => {
                cpu.a = val;
                1
            }
            Addressing::RegisterX => {
                cpu.x = val;
                1
            }
            Addressing::RegisterY => {
                cpu.y = val;
                1
            }
            Addressing::RegisterSP => {
                cpu.sp = val;
                1
            } // RegisterStatus removed from Addressing
        }
    }
}

pub trait Address<M: Mcu>: Display + Copy {
    /// return (value, ticks)
    fn get(&self, cpu: &mut Cpu<M>) -> (u8, u8) {
        let (addr, ticks) = self.calc_addr(cpu);
        (cpu.read_byte(addr), ticks + 1) // +1 for the memory read
    }

    /// return ticks
    fn set(&self, cpu: &mut Cpu<M>, val: u8) -> u8 {
        let (addr, ticks) = self.calc_addr_write(cpu);
        cpu.write_byte(addr, val);
        ticks + 1 // +1 for the memory write
    }

    fn calc_addr(&self, _: &mut Cpu<M>) -> (u16, u8) {
        panic!("calc_addr not implemented");
    }

    fn calc_addr_write(&self, cpu: &mut Cpu<M>) -> (u16, u8) {
        self.calc_addr(cpu)
    }

    fn is_register(&self) -> bool {
        false
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Literal(pub u8);

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "#${:02x}", self.0)
    }
}

impl<M: Mcu> Address<M> for Literal {
    fn get(&self, _: &mut Cpu<M>) -> (u8, u8) {
        (self.0, 1)
    }

    fn set(&self, _: &mut Cpu<M>, _: u8) -> u8 {
        panic!("can't set value to literal");
    }
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub struct ZeroPage(pub u8);

#[cfg(test)]
impl Display for ZeroPage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "${:02x}", self.0)
    }
}

#[cfg(test)]
impl<M: Mcu> Address<M> for ZeroPage {
    fn calc_addr(&self, _: &mut Cpu<M>) -> (u16, u8) {
        (self.0 as u16, 1)
    }
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub struct Absolute(pub u16);

#[cfg(test)]
impl Display for Absolute {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "${:04x}", self.0)
    }
}

#[cfg(test)]
impl<M: Mcu> Address<M> for Absolute {
    fn calc_addr(&self, _: &mut Cpu<M>) -> (u16, u8) {
        (self.0, 2)
    }
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub struct ZeroPageX(pub u8);

#[cfg(test)]
impl Display for ZeroPageX {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "${:02x},X", self.0)
    }
}

#[cfg(test)]
impl<M: Mcu> Address<M> for ZeroPageX {
    fn calc_addr(&self, cpu: &mut Cpu<M>) -> (u16, u8) {
        (self.0.wrapping_add(cpu.x) as u16, 2)
    }
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub struct ZeroPageY(pub u8);

#[cfg(test)]
impl Display for ZeroPageY {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "${:02x},Y", self.0)
    }
}

#[cfg(test)]
impl<M: Mcu> Address<M> for ZeroPageY {
    fn calc_addr(&self, cpu: &mut Cpu<M>) -> (u16, u8) {
        (self.0.wrapping_add(cpu.y) as u16, 2)
    }
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub struct AbsoluteX(pub u16);

#[cfg(test)]
impl Display for AbsoluteX {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "${:04x},X", self.0)
    }
}

#[cfg(test)]
impl<M: Mcu> Address<M> for AbsoluteX {
    fn calc_addr(&self, cpu: &mut Cpu<M>) -> (u16, u8) {
        let r = self.0.wrapping_add(cpu.x as u16);
        (r, 2 + extra_tick_if_cross_page(self.0, r))
    }

    fn calc_addr_write(&self, cpu: &mut Cpu<M>) -> (u16, u8) {
        let r = self.0.wrapping_add(cpu.x as u16);
        (r, 3)
    }
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub struct AbsoluteY(pub u16);

#[cfg(test)]
impl Display for AbsoluteY {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "${:04x},Y", self.0)
    }
}

#[cfg(test)]
impl<M: Mcu> Address<M> for AbsoluteY {
    fn calc_addr(&self, cpu: &mut Cpu<M>) -> (u16, u8) {
        let r = self.0.wrapping_add(cpu.y as u16);
        (r, 2 + extra_tick_if_cross_page(self.0, r))
    }

    fn calc_addr_write(&self, cpu: &mut Cpu<M>) -> (u16, u8) {
        let r = self.0.wrapping_add(cpu.y as u16);
        (r, 3)
    }
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub struct IndirectX(pub u8);

#[cfg(test)]
impl Display for IndirectX {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "(${:02x},X)", self.0)
    }
}

#[cfg(test)]
impl<M: Mcu> Address<M> for IndirectX {
    fn calc_addr(&self, cpu: &mut Cpu<M>) -> (u16, u8) {
        (cpu.read_zero_page_word(cpu.x.wrapping_add(self.0)), 4)
    }
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub struct IndirectY(pub u8);

#[cfg(test)]
impl Display for IndirectY {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "(${:02x}),Y", self.0)
    }
}

#[cfg(test)]
impl<M: Mcu> Address<M> for IndirectY {
    fn calc_addr(&self, cpu: &mut Cpu<M>) -> (u16, u8) {
        let addr = cpu.read_zero_page_word(self.0);
        let r = addr.wrapping_add(cpu.y as u16);
        (r, 3 + extra_tick_if_cross_page(addr, r))
    }

    fn calc_addr_write(&self, cpu: &mut Cpu<M>) -> (u16, u8) {
        let addr = cpu.read_zero_page_word(self.0);
        let r = addr.wrapping_add(cpu.y as u16);
        (r, 4)
    }
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub struct RegisterA();

#[cfg(test)]
impl Display for RegisterA {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "A")
    }
}

#[cfg(test)]
impl<M: Mcu> Address<M> for RegisterA {
    fn get(&self, cpu: &mut Cpu<M>) -> (u8, u8) {
        (cpu.a, 1)
    }

    fn set(&self, cpu: &mut Cpu<M>, val: u8) -> u8 {
        cpu.a = val;
        1
    }

    fn is_register(&self) -> bool {
        true
    }
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub struct RegisterX();

#[cfg(test)]
impl Display for RegisterX {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "X")
    }
}

#[cfg(test)]
impl<M: Mcu> Address<M> for RegisterX {
    fn get(&self, cpu: &mut Cpu<M>) -> (u8, u8) {
        (cpu.x, 1)
    }

    fn set(&self, cpu: &mut Cpu<M>, val: u8) -> u8 {
        cpu.x = val;
        1
    }

    fn is_register(&self) -> bool {
        true
    }
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub struct RegisterY();

#[cfg(test)]
impl Display for RegisterY {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Y")
    }
}

#[cfg(test)]
impl<M: Mcu> Address<M> for RegisterY {
    fn get(&self, cpu: &mut Cpu<M>) -> (u8, u8) {
        (cpu.y, 1)
    }

    fn set(&self, cpu: &mut Cpu<M>, val: u8) -> u8 {
        cpu.y = val;
        1
    }

    fn is_register(&self) -> bool {
        true
    }
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub struct RegisterSP();

#[cfg(test)]
impl Display for RegisterSP {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "SP")
    }
}

#[cfg(test)]
impl<M: Mcu> Address<M> for RegisterSP {
    fn get(&self, cpu: &mut Cpu<M>) -> (u8, u8) {
        (cpu.sp, 1)
    }

    fn set(&self, cpu: &mut Cpu<M>, val: u8) -> u8 {
        cpu.sp = val;
        1
    }

    fn is_register(&self) -> bool {
        true
    }
}

// `RegisterStatus` removed; use `FlagAddr` to access individual flags or
// manipulate `cpu.status` directly when you need to read/write the full
// status byte. Keeping FlagAddr provides a safe, typed way to access flags.

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct FlagAddr(pub Flag);

impl Display for FlagAddr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<M: Mcu> Address<M> for FlagAddr {
    fn get(&self, cpu: &mut Cpu<M>) -> (u8, u8) {
        (cpu.flag(self.0) as u8, 1)
    }

    fn set(&self, cpu: &mut Cpu<M>, val: u8) -> u8 {
        cpu.set_flag(self.0, val != 0);
        1
    }

    fn is_register(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests;
