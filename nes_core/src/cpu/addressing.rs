use super::{extra_cycles_if_cross_page, Cpu};
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
        }
    }
}

impl Addressing {
    pub fn is_register(&self) -> bool {
        matches!(self, Addressing::Accumulator)
    }

    pub fn calc_addr<M: Mcu>(&self, cpu: &mut Cpu<M>) -> (u16, u8) {
        match self {
            Addressing::Accumulator => (cpu.a as u16, 0),
            Addressing::Implied => panic!("can't calc addr for implied addressing"),
            Addressing::Immediate(v) => (*v as u16, 0),
            Addressing::Absolute(a) => (*a, 2),
            Addressing::ZeroPage(z) => (*z as u16, 1),
            Addressing::AbsoluteIndexedWithX(a) => {
                let r = a.wrapping_add(cpu.x as u16);
                (r, 2 + extra_cycles_if_cross_page(*a, r))
            }
            Addressing::AbsoluteIndexedWithY(a) => {
                let r = a.wrapping_add(cpu.y as u16);
                (r, 2 + extra_cycles_if_cross_page(*a, r))
            }
            Addressing::ZeroPageIndexedWithX(z) => (z.wrapping_add(cpu.x) as u16, 2),
            Addressing::ZeroPageIndexedWithY(z) => (z.wrapping_add(cpu.y) as u16, 2),
            Addressing::ZeroPageIndexedIndirect(z) => {
                (cpu.read_zero_page_word(cpu.x.wrapping_add(*z)), 4)
            }
            Addressing::ZeroPageIndexedIndirectWithY(z) => {
                let base = cpu.read_zero_page_word(*z);
                let r = base.wrapping_add(cpu.y as u16);
                (r, 3 + extra_cycles_if_cross_page(base, r))
            }
        }
    }
}
