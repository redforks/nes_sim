use crate::mcu::Mcu;
use addressing::*;
use instruction::Instruction;

mod addressing;
mod cpu2;
mod instruction;

pub use cpu2::Cpu;

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum ExecuteResult {
    Continue,
    Stop(u8),
    ShouldReset,
}

fn is_cross_page(a: u16, b: u16) -> bool {
    let a = (a >> 8) as u8;
    let b = (b >> 8) as u8;
    a != b
}

fn extra_cycles_if_cross_page(a: u16, b: u16) -> u8 {
    is_cross_page(a, b) as u8
}

fn decode_next<M: Mcu>(cpu: &mut Cpu<M>) -> Instruction {
    let op_code = cpu.inc_read_byte();
    let a = (op_code & 0b1110_0000) >> 5;
    let b = (op_code & 0b0001_1100) >> 2;
    let c = op_code & 0b0000_0011;

    fn r_b<M: Mcu>(cpu: &mut Cpu<M>) -> u8 {
        cpu.inc_read_byte()
    }
    fn r_w<M: Mcu>(cpu: &mut Cpu<M>) -> u16 {
        cpu.inc_read_word()
    }

    let zero_page = |cpu: &mut Cpu<M>| Addressing::ZeroPage(r_b(cpu));
    let zero_page_x = |cpu: &mut Cpu<M>| Addressing::ZeroPageIndexedWithX(r_b(cpu));
    let zero_page_y = |cpu: &mut Cpu<M>| Addressing::ZeroPageIndexedWithY(r_b(cpu));
    let absolute = |cpu: &mut Cpu<M>| Addressing::Absolute(r_w(cpu));
    let absolute_x = |cpu: &mut Cpu<M>| Addressing::AbsoluteIndexedWithX(r_w(cpu));
    let absolute_y = |cpu: &mut Cpu<M>| Addressing::AbsoluteIndexedWithY(r_w(cpu));
    let literal = |cpu: &mut Cpu<M>| Addressing::Immediate(r_b(cpu));
    let indirect_x = |cpu: &mut Cpu<M>| Addressing::ZeroPageIndexedIndirect(r_b(cpu));
    let indirect_y = |cpu: &mut Cpu<M>| Addressing::ZeroPageIndexedIndirectWithY(r_b(cpu));

    match (c, a, b) {
        (0, 0, 0) => Instruction::Brk,
        (0, 0, 1) => Instruction::Nop(zero_page(cpu)),
        (0, 0, 2) => Instruction::Php,
        (0, 0, 3) => Instruction::Nop(absolute(cpu)),
        (0, 0, 4) => Instruction::Bpl(BranchAddressing::Relative(r_b(cpu))),
        (0, 0, 5) => Instruction::Nop(zero_page_x(cpu)),
        (0, 0, 6) => Instruction::Clc,
        (0, 0, 7) => Instruction::Nop(absolute_x(cpu)),

        (0, 1, 0) => Instruction::Jsr(BranchAddressing::Absolute(r_w(cpu))),
        (0, 1, 1) => Instruction::Bit(zero_page(cpu)),
        (0, 1, 2) => Instruction::Plp,
        (0, 1, 3) => Instruction::Bit(absolute(cpu)),
        (0, 1, 4) => Instruction::Bmi(BranchAddressing::Relative(r_b(cpu))),
        (0, 1, 5) => Instruction::Nop(zero_page_x(cpu)),
        (0, 1, 6) => Instruction::Sec,
        (0, 1, 7) => Instruction::Nop(absolute_x(cpu)),

        (0, 2, 0) => Instruction::Rti,
        (0, 2, 1) => Instruction::Nop(zero_page(cpu)),
        (0, 2, 2) => Instruction::Pha,
        (0, 2, 3) => Instruction::Jmp(BranchAddressing::Absolute(r_w(cpu))),
        (0, 2, 4) => Instruction::Bvc(BranchAddressing::Relative(r_b(cpu))),
        (0, 2, 5) => Instruction::Nop(zero_page_x(cpu)),
        (0, 2, 6) => Instruction::Cli,
        (0, 2, 7) => Instruction::Nop(absolute_x(cpu)),

        (0, 3, 0) => Instruction::Rts,
        (0, 3, 1) => Instruction::Nop(zero_page(cpu)),
        (0, 3, 2) => Instruction::Pla,
        (0, 3, 3) => Instruction::Jmp(BranchAddressing::AbsoluteIndirect(r_w(cpu))),
        (0, 3, 4) => Instruction::Bvs(BranchAddressing::Relative(r_b(cpu))),
        (0, 3, 5) => Instruction::Nop(zero_page_x(cpu)),
        (0, 3, 6) => Instruction::Sei,
        (0, 3, 7) => Instruction::Nop(absolute_x(cpu)),

        (0, 4, 0) => Instruction::Nop(literal(cpu)),
        (0, 4, 1) => Instruction::Sty(zero_page(cpu)),
        (0, 4, 2) => Instruction::Dey,
        (0, 4, 3) => Instruction::Sty(absolute(cpu)),
        (0, 4, 4) => Instruction::Bcc(BranchAddressing::Relative(r_b(cpu))),
        (0, 4, 5) => Instruction::Sty(zero_page_x(cpu)),
        (0, 4, 6) => Instruction::Tya,
        (0, 4, 7) => Instruction::Shy(absolute_x(cpu)),

        (0, 5, 0) => Instruction::Ldy(literal(cpu)),
        (0, 5, 1) => Instruction::Ldy(zero_page(cpu)),
        (0, 5, 2) => Instruction::Tay,
        (0, 5, 3) => Instruction::Ldy(absolute(cpu)),
        (0, 5, 4) => Instruction::Bcs(BranchAddressing::Relative(r_b(cpu))),
        (0, 5, 5) => Instruction::Ldy(zero_page_x(cpu)),
        (0, 5, 6) => Instruction::Clv,
        (0, 5, 7) => Instruction::Ldy(absolute_x(cpu)),
        (0, 6, 7) => Instruction::Nop(absolute_x(cpu)),

        (0, 6, 0) => Instruction::Cpy(literal(cpu)),
        (0, 6, 1) => Instruction::Cpy(zero_page(cpu)),
        (0, 6, 2) => Instruction::Iny,
        (0, 6, 3) => Instruction::Cpy(absolute(cpu)),
        (0, 6, 4) => Instruction::Bne(BranchAddressing::Relative(r_b(cpu))),
        (0, 6, 5) => Instruction::Nop(zero_page_x(cpu)),
        (0, 6, 6) => Instruction::Cld,

        (0, 7, 0) => Instruction::Cpx(literal(cpu)),
        (0, 7, 1) => Instruction::Cpx(zero_page(cpu)),
        (0, 7, 2) => Instruction::Inx,
        (0, 7, 3) => Instruction::Cpx(absolute(cpu)),
        (0, 7, 4) => Instruction::Beq(BranchAddressing::Relative(r_b(cpu))),
        (0, 7, 5) => Instruction::Nop(zero_page_x(cpu)),
        (0, 7, 6) => Instruction::Sed,
        (0, 7, 7) => Instruction::Nop(absolute_x(cpu)),

        (1, 0, 0) => Instruction::Ora(indirect_x(cpu)),
        (1, 0, 1) => Instruction::Ora(zero_page(cpu)),
        (1, 0, 2) => Instruction::Ora(literal(cpu)),
        (1, 0, 3) => Instruction::Ora(absolute(cpu)),
        (1, 0, 4) => Instruction::Ora(indirect_y(cpu)),
        (1, 0, 5) => Instruction::Ora(zero_page_x(cpu)),
        (1, 0, 6) => Instruction::Ora(absolute_y(cpu)),
        (1, 0, 7) => Instruction::Ora(absolute_x(cpu)),

        (1, 1, 0) => Instruction::And(indirect_x(cpu)),
        (1, 1, 1) => Instruction::And(zero_page(cpu)),
        (1, 1, 2) => Instruction::And(literal(cpu)),
        (1, 1, 3) => Instruction::And(absolute(cpu)),
        (1, 1, 4) => Instruction::And(indirect_y(cpu)),
        (1, 1, 5) => Instruction::And(zero_page_x(cpu)),
        (1, 1, 6) => Instruction::And(absolute_y(cpu)),
        (1, 1, 7) => Instruction::And(absolute_x(cpu)),

        (1, 2, 0) => Instruction::Eor(indirect_x(cpu)),
        (1, 2, 1) => Instruction::Eor(zero_page(cpu)),
        (1, 2, 2) => Instruction::Eor(literal(cpu)),
        (1, 2, 3) => Instruction::Eor(absolute(cpu)),
        (1, 2, 4) => Instruction::Eor(indirect_y(cpu)),
        (1, 2, 5) => Instruction::Eor(zero_page_x(cpu)),
        (1, 2, 6) => Instruction::Eor(absolute_y(cpu)),
        (1, 2, 7) => Instruction::Eor(absolute_x(cpu)),

        (1, 3, 0) => Instruction::Adc(indirect_x(cpu)),
        (1, 3, 1) => Instruction::Adc(zero_page(cpu)),
        (1, 3, 2) => Instruction::Adc(literal(cpu)),
        (1, 3, 3) => Instruction::Adc(absolute(cpu)),
        (1, 3, 4) => Instruction::Adc(indirect_y(cpu)),
        (1, 3, 5) => Instruction::Adc(zero_page_x(cpu)),
        (1, 3, 6) => Instruction::Adc(absolute_y(cpu)),
        (1, 3, 7) => Instruction::Adc(absolute_x(cpu)),

        (1, 4, 0) => Instruction::Sta(indirect_x(cpu)),
        (1, 4, 1) => Instruction::Sta(zero_page(cpu)),
        (1, 4, 2) => Instruction::Nop(literal(cpu)),
        (1, 4, 3) => Instruction::Sta(absolute(cpu)),
        (1, 4, 4) => Instruction::Sta(indirect_y(cpu)),
        (1, 4, 5) => Instruction::Sta(zero_page_x(cpu)),
        (1, 4, 6) => Instruction::Sta(absolute_y(cpu)),
        (1, 4, 7) => Instruction::Sta(absolute_x(cpu)),

        (1, 5, 0) => Instruction::Lda(indirect_x(cpu)),
        (1, 5, 1) => Instruction::Lda(zero_page(cpu)),
        (1, 5, 2) => Instruction::Lda(literal(cpu)),
        (1, 5, 3) => Instruction::Lda(absolute(cpu)),
        (1, 5, 4) => Instruction::Lda(indirect_y(cpu)),
        (1, 5, 5) => Instruction::Lda(zero_page_x(cpu)),
        (1, 5, 6) => Instruction::Lda(absolute_y(cpu)),
        (1, 5, 7) => Instruction::Lda(absolute_x(cpu)),

        (1, 6, 0) => Instruction::Cmp(indirect_x(cpu)),
        (1, 6, 1) => Instruction::Cmp(zero_page(cpu)),
        (1, 6, 2) => Instruction::Cmp(literal(cpu)),
        (1, 6, 3) => Instruction::Cmp(absolute(cpu)),
        (1, 6, 4) => Instruction::Cmp(indirect_y(cpu)),
        (1, 6, 5) => Instruction::Cmp(zero_page_x(cpu)),
        (1, 6, 6) => Instruction::Cmp(absolute_y(cpu)),
        (1, 6, 7) => Instruction::Cmp(absolute_x(cpu)),

        (1, 7, 0) => Instruction::Sbc(indirect_x(cpu)),
        (1, 7, 1) => Instruction::Sbc(zero_page(cpu)),
        (1, 7, 2) => Instruction::Sbc(literal(cpu)),
        (1, 7, 3) => Instruction::Sbc(absolute(cpu)),
        (1, 7, 4) => Instruction::Sbc(indirect_y(cpu)),
        (1, 7, 5) => Instruction::Sbc(zero_page_x(cpu)),
        (1, 7, 6) => Instruction::Sbc(absolute_y(cpu)),
        (1, 7, 7) => Instruction::Sbc(absolute_x(cpu)),

        (2, 0, 0) => Instruction::Hlt,
        (2, 0, 1) => Instruction::Asl(zero_page(cpu)),
        (2, 0, 2) => Instruction::Asl(Addressing::Accumulator),
        (2, 0, 3) => Instruction::Asl(absolute(cpu)),
        (2, 0, 4) => Instruction::Hlt,
        (2, 0, 5) => Instruction::Asl(zero_page_x(cpu)),
        (2, 0, 6) => Instruction::Nop(Addressing::Implied),
        (2, 0, 7) => Instruction::Asl(absolute_x(cpu)),

        (2, 1, 0) => Instruction::Hlt,
        (2, 1, 1) => Instruction::Rol(zero_page(cpu)),
        (2, 1, 2) => Instruction::Rol(Addressing::Accumulator),
        (2, 1, 3) => Instruction::Rol(absolute(cpu)),
        (2, 1, 4) => Instruction::Hlt,
        (2, 1, 5) => Instruction::Rol(zero_page_x(cpu)),
        (2, 1, 6) => Instruction::Nop(Addressing::Implied),
        (2, 1, 7) => Instruction::Rol(absolute_x(cpu)),

        (2, 2, 0) => Instruction::Hlt,
        (2, 2, 1) => Instruction::Lsr(zero_page(cpu)),
        (2, 2, 2) => Instruction::Lsr(Addressing::Accumulator),
        (2, 2, 3) => Instruction::Lsr(absolute(cpu)),
        (2, 2, 4) => Instruction::Hlt,
        (2, 2, 5) => Instruction::Lsr(zero_page_x(cpu)),
        (2, 2, 6) => Instruction::Nop(Addressing::Implied),
        (2, 2, 7) => Instruction::Lsr(absolute_x(cpu)),

        (2, 3, 0) => Instruction::Hlt,
        (2, 3, 1) => Instruction::Ror(zero_page(cpu)),
        (2, 3, 2) => Instruction::Ror(Addressing::Accumulator),
        (2, 3, 3) => Instruction::Ror(absolute(cpu)),
        (2, 3, 4) => Instruction::Hlt,
        (2, 3, 5) => Instruction::Ror(zero_page_x(cpu)),
        (2, 3, 6) => Instruction::Nop(Addressing::Implied),
        (2, 3, 7) => Instruction::Ror(absolute_x(cpu)),

        (2, 4, 0) => Instruction::Nop(literal(cpu)),
        (2, 4, 1) => Instruction::Stx(zero_page(cpu)),
        (2, 4, 2) => Instruction::Txa,
        (2, 4, 3) => Instruction::Stx(absolute(cpu)),
        (2, 4, 4) => Instruction::Hlt,
        (2, 4, 5) => Instruction::Stx(zero_page_y(cpu)),
        (2, 4, 6) => Instruction::Txs,
        (2, 4, 7) => Instruction::Shx(absolute_y(cpu)),

        (2, 5, 0) => Instruction::Ldx(literal(cpu)),
        (2, 5, 1) => Instruction::Ldx(zero_page(cpu)),
        (2, 5, 2) => Instruction::Tax,
        (2, 5, 3) => Instruction::Ldx(absolute(cpu)),
        (2, 5, 4) => Instruction::Hlt,
        (2, 5, 5) => Instruction::Ldx(zero_page_y(cpu)),
        (2, 5, 6) => Instruction::Tsx,
        (2, 5, 7) => Instruction::Ldx(absolute_y(cpu)),

        (2, 6, 0) => Instruction::Nop(literal(cpu)),
        (2, 6, 1) => Instruction::Dec(zero_page(cpu)),
        (2, 6, 2) => Instruction::Dex,
        (2, 6, 3) => Instruction::Dec(absolute(cpu)),
        (2, 6, 4) => Instruction::Hlt,
        (2, 6, 5) => Instruction::Dec(zero_page_x(cpu)),
        (2, 6, 6) => Instruction::Nop(Addressing::Implied),
        (2, 6, 7) => Instruction::Dec(absolute_x(cpu)),

        (2, 7, 0) => Instruction::Nop(literal(cpu)),
        (2, 7, 1) => Instruction::Inc(zero_page(cpu)),
        (2, 7, 2) => Instruction::Inx,
        (2, 7, 3) => Instruction::Inc(absolute(cpu)),
        (2, 7, 4) => Instruction::Hlt,
        (2, 7, 5) => Instruction::Inc(zero_page_x(cpu)),
        (2, 7, 6) => Instruction::Nop(Addressing::Implied),
        (2, 7, 7) => Instruction::Inc(absolute_x(cpu)),

        (3, 0, 0) => Instruction::Aso(indirect_x(cpu)),
        (3, 0, 1) => Instruction::Aso(zero_page(cpu)),
        (3, 0, 2) => Instruction::Anc(Addressing::Immediate(r_b(cpu))),
        (3, 0, 3) => Instruction::Aso(absolute(cpu)),
        (3, 0, 4) => Instruction::Aso(indirect_y(cpu)),
        (3, 0, 5) => Instruction::Aso(zero_page_x(cpu)),
        (3, 0, 6) => Instruction::Aso(absolute_y(cpu)),
        (3, 0, 7) => Instruction::Aso(absolute_x(cpu)),

        (3, 1, 0) => Instruction::Rla(indirect_x(cpu)),
        (3, 1, 1) => Instruction::Rla(zero_page(cpu)),
        (3, 1, 2) => Instruction::Anc(Addressing::Immediate(r_b(cpu))),
        (3, 1, 3) => Instruction::Rla(absolute(cpu)),
        (3, 1, 4) => Instruction::Rla(indirect_y(cpu)),
        (3, 1, 5) => Instruction::Rla(zero_page_x(cpu)),
        (3, 1, 6) => Instruction::Rla(absolute_y(cpu)),
        (3, 1, 7) => Instruction::Rla(absolute_x(cpu)),

        (3, 2, 0) => Instruction::Lse(indirect_x(cpu)),
        (3, 2, 1) => Instruction::Lse(zero_page(cpu)),
        (3, 2, 2) => Instruction::Alr(Addressing::Immediate(r_b(cpu))),
        (3, 2, 3) => Instruction::Lse(absolute(cpu)),
        (3, 2, 4) => Instruction::Lse(indirect_y(cpu)),
        (3, 2, 5) => Instruction::Lse(zero_page_x(cpu)),
        (3, 2, 6) => Instruction::Lse(absolute_y(cpu)),
        (3, 2, 7) => Instruction::Lse(absolute_x(cpu)),

        (3, 3, 0) => Instruction::Rra(indirect_x(cpu)),
        (3, 3, 1) => Instruction::Rra(zero_page(cpu)),
        (3, 3, 2) => Instruction::Arr(Addressing::Immediate(r_b(cpu))),
        (3, 3, 3) => Instruction::Rra(absolute(cpu)),
        (3, 3, 4) => Instruction::Rra(indirect_y(cpu)),
        (3, 3, 5) => Instruction::Rra(zero_page_x(cpu)),
        (3, 3, 6) => Instruction::Rra(absolute_y(cpu)),
        (3, 3, 7) => Instruction::Rra(absolute_x(cpu)),

        (3, 4, 0) => Instruction::Sax(indirect_x(cpu)),
        (3, 4, 1) => Instruction::Sax(zero_page(cpu)),
        (3, 4, 2) => Instruction::Ane(Addressing::Immediate(r_b(cpu))),
        (3, 4, 3) => Instruction::Sax(absolute(cpu)),
        (3, 4, 4) => Instruction::Sha(indirect_y(cpu)),
        (3, 4, 5) => Instruction::Sax(zero_page_y(cpu)),
        (3, 4, 6) => Instruction::Tas(absolute_y(cpu)),
        (3, 4, 7) => Instruction::Tas(absolute_y(cpu)),

        (3, 5, 0) => Instruction::Lax(indirect_x(cpu)),
        (3, 5, 1) => Instruction::Lax(zero_page(cpu)),
        (3, 5, 2) => Instruction::Lax(literal(cpu)),
        (3, 5, 3) => Instruction::Lax(absolute(cpu)),
        (3, 5, 4) => Instruction::Lax(indirect_y(cpu)),
        (3, 5, 5) => Instruction::Lax(zero_page_y(cpu)),
        (3, 5, 6) => Instruction::Lax(absolute_y(cpu)),
        (3, 5, 7) => Instruction::Lax(absolute_y(cpu)),

        (3, 6, 0) => Instruction::Dcp(indirect_x(cpu)),
        (3, 6, 1) => Instruction::Dcp(zero_page(cpu)),
        (3, 6, 2) => Instruction::Sbx(Addressing::Immediate(r_b(cpu))),
        (3, 6, 3) => Instruction::Dcp(absolute(cpu)),
        (3, 6, 4) => Instruction::Dcp(indirect_y(cpu)),
        (3, 6, 5) => Instruction::Dcp(zero_page_x(cpu)),
        (3, 6, 6) => Instruction::Dcp(absolute_y(cpu)),
        (3, 6, 7) => Instruction::Dcp(absolute_x(cpu)),

        (3, 7, 0) => Instruction::Isc(indirect_x(cpu)),
        (3, 7, 1) => Instruction::Isc(zero_page(cpu)),
        (3, 7, 2) => Instruction::Sbc(literal(cpu)),
        (3, 7, 3) => Instruction::Isc(absolute(cpu)),
        (3, 7, 4) => Instruction::Isc(indirect_y(cpu)),
        (3, 7, 5) => Instruction::Isc(zero_page_x(cpu)),
        (3, 7, 6) => Instruction::Isc(absolute_y(cpu)),
        (3, 7, 7) => Instruction::Isc(absolute_x(cpu)),

        _ => panic!(
            "Unknown opcode: {:02x} ({}, {}, {}) @ {:04x}",
            op_code, c, a, b, cpu.pc
        ),
    }
}

fn execute_next<M: Mcu>(cpu: &mut Cpu<M>) -> u8 {
    decode_next(cpu).exec(cpu)
}

pub trait Plugin<M: Mcu> {
    fn start(&mut self, cpu: &mut Cpu<M>);

    fn end(&mut self, cpu: &mut Cpu<M>, cycles: u8);

    fn should_stop(&self) -> ExecuteResult {
        ExecuteResult::Continue
    }
}

impl<M: Mcu> Plugin<M> for Box<dyn Plugin<M>> {
    fn start(&mut self, cpu: &mut Cpu<M>) {
        self.as_mut().start(cpu);
    }

    fn end(&mut self, cpu: &mut Cpu<M>, cycles: u8) {
        self.as_mut().end(cpu, cycles);
    }

    fn should_stop(&self) -> ExecuteResult {
        self.as_ref().should_stop()
    }
}

pub struct EmptyPlugin<M: Mcu> {
    _phantom: std::marker::PhantomData<M>,
}

impl<M: Mcu> EmptyPlugin<M> {
    pub fn new() -> Self {
        Self {
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<M: Mcu> Plugin<M> for EmptyPlugin<M> {
    fn start(&mut self, _: &mut Cpu<M>) {}

    fn end(&mut self, _: &mut Cpu<M>, _cycles: u8) {}
}

impl<M: Mcu> Default for EmptyPlugin<M> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Copy, strum_macros::Display, PartialEq, Eq)]
#[repr(u8)]
pub enum Flag {
    #[strum(serialize = "C")]
    Carry = 0x01u8,
    #[strum(serialize = "Z")]
    Zero = 0x02u8,
    #[strum(serialize = "I")]
    InterruptDisabled = 0x04u8,
    #[strum(serialize = "D")]
    Decimal = 0x08u8,
    #[strum(serialize = "B")]
    Break = 0x10u8,
    NotUsed = 0x20u8,
    #[strum(serialize = "V")]
    Overflow = 0x40u8,
    #[strum(serialize = "N")]
    Negative = 0x80u8,
}

#[cfg(test)]
mod tests;
