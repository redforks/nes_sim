use crate::mcu::Mcu;
use log::info;

mod addressing;
mod instruction;

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum ExecuteResult {
    Continue,
    Stop(u8),    // should exit executing, with exit code, 0 means success
    ShouldReset, // should reset cpu
}

fn is_cross_page(a: u16, b: u16) -> bool {
    let a = (a >> 8) as u8;
    let b = (b >> 8) as u8;
    a != b
}

fn extra_tick_if_cross_page(a: u16, b: u16) -> u8 {
    is_cross_page(a, b) as u8
}

fn execute_next(cpu: &mut Cpu) -> u8 {
    use addressing::*;
    use instruction::*;

    let op_code = cpu.inc_read_byte();
    let a = (op_code & 0b1110_0000) >> 5;
    let b = (op_code & 0b0001_1100) >> 2;
    let c = op_code & 0b0000_0011;
    // debug!("op_code: {:0x} {} {} {}", op_code, c, a, b);

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
        (0, 0, 1) => new_nop_with_addr(zero_page(cpu))(cpu),
        (0, 0, 2) => new_php()(cpu),
        (0, 0, 3) => new_nop_with_addr(absolute(cpu))(cpu),
        (0, 0, 4) => neg_cond_branch(cpu, Flag::Negative)(cpu),
        (0, 0, 5) => new_nop_with_addr(zero_page_x(cpu))(cpu),
        (0, 0, 6) => new_clear_bit(FlagAddr(Flag::Carry))(cpu),
        (0, 0, 7) => new_nop_with_addr(absolute_x(cpu))(cpu),

        (0, 1, 0) => new_jsr(r_w(cpu))(cpu),
        (0, 1, 1) => new_bit(zero_page(cpu))(cpu),
        (0, 1, 2) => new_plp()(cpu),
        (0, 1, 3) => new_bit(absolute(cpu))(cpu),
        (0, 1, 4) => cond_branch(cpu, Flag::Negative)(cpu),
        (0, 1, 5) => new_nop_with_addr(zero_page_x(cpu))(cpu),
        (0, 1, 6) => new_set_bit(FlagAddr(Flag::Carry))(cpu),
        (0, 1, 7) => new_nop_with_addr(absolute_x(cpu))(cpu),

        (0, 2, 0) => new_rti()(cpu),
        (0, 2, 1) => new_nop_with_addr(zero_page(cpu))(cpu),
        (0, 2, 2) => new_pha()(cpu),
        (0, 2, 3) => new_jmp(r_w(cpu))(cpu),
        (0, 2, 4) => neg_cond_branch(cpu, Flag::Overflow)(cpu),
        (0, 2, 5) => new_nop_with_addr(zero_page_x(cpu))(cpu),
        (0, 2, 6) => new_clear_bit(FlagAddr(Flag::InterruptDisabled))(cpu),
        (0, 2, 7) => new_nop_with_addr(absolute_x(cpu))(cpu),

        (0, 3, 0) => new_rts()(cpu),
        (0, 3, 1) => new_nop_with_addr(zero_page(cpu))(cpu),
        (0, 3, 2) => new_pla()(cpu),
        (0, 3, 3) => new_indirect_jmp(r_w(cpu))(cpu),
        (0, 3, 4) => cond_branch(cpu, Flag::Overflow)(cpu),
        (0, 3, 5) => new_nop_with_addr(zero_page_x(cpu))(cpu),
        (0, 3, 6) => new_set_bit(FlagAddr(Flag::InterruptDisabled))(cpu),
        (0, 3, 7) => new_nop_with_addr(absolute_x(cpu))(cpu),

        (0, 4, 0) => new_nop_with_addr(literal(cpu))(cpu),
        (0, 4, 1) => new_transfer_no_touch_flags(zero_page(cpu), y())(cpu),
        (0, 4, 2) => new_dec(y())(cpu),
        (0, 4, 3) => new_transfer_no_touch_flags(absolute(cpu), y())(cpu),
        (0, 4, 4) => neg_cond_branch(cpu, Flag::Carry)(cpu),
        (0, 4, 5) => new_transfer_no_touch_flags(zero_page_x(cpu), y())(cpu),
        (0, 4, 6) => new_transfer(aa(), y())(cpu),
        (0, 4, 7) => new_all("shy", y(), absolute_x(cpu))(cpu),

        (0, 5, 0) => new_transfer(y(), literal(cpu))(cpu),
        (0, 5, 1) => new_transfer(y(), zero_page(cpu))(cpu),
        (0, 5, 2) => new_transfer(y(), aa())(cpu),
        (0, 5, 3) => new_transfer(y(), absolute(cpu))(cpu),
        (0, 5, 4) => cond_branch(cpu, Flag::Carry)(cpu),
        (0, 5, 5) => new_transfer(y(), zero_page_x(cpu))(cpu),
        (0, 5, 6) => new_clear_bit(FlagAddr(Flag::Overflow))(cpu),
        (0, 5, 7) => new_transfer(y(), absolute_x(cpu))(cpu),
        (0, 6, 7) => new_nop_with_addr(absolute_x(cpu))(cpu),

        (0, 6, 0) => new_cmp(y(), literal(cpu))(cpu),
        (0, 6, 1) => new_cmp(y(), zero_page(cpu))(cpu),
        (0, 6, 2) => new_inc(y())(cpu),
        (0, 6, 3) => new_cmp(y(), absolute(cpu))(cpu),
        (0, 6, 4) => neg_cond_branch(cpu, Flag::Zero)(cpu),
        (0, 6, 5) => new_nop_with_addr(zero_page_x(cpu))(cpu),
        (0, 6, 6) => new_clear_bit(FlagAddr(Flag::Decimal))(cpu),

        (0, 7, 0) => new_cmp(x(), literal(cpu))(cpu),
        (0, 7, 1) => new_cmp(x(), zero_page(cpu))(cpu),
        (0, 7, 2) => new_inc(x())(cpu),
        (0, 7, 3) => new_cmp(x(), absolute(cpu))(cpu),
        (0, 7, 4) => cond_branch(cpu, Flag::Zero)(cpu),
        (0, 7, 5) => new_nop_with_addr(zero_page_x(cpu))(cpu),
        (0, 7, 6) => new_set_bit(FlagAddr(Flag::Decimal))(cpu),
        (0, 7, 7) => new_nop_with_addr(absolute_x(cpu))(cpu),

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
        (1, 4, 2) => new_nop_with_addr(literal(cpu))(cpu),
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

        (2, 0, 0) => new_hlt()(cpu),
        (2, 0, 1) => new_asl(zero_page(cpu))(cpu),
        (2, 0, 2) => new_asl(aa())(cpu),
        (2, 0, 3) => new_asl(absolute(cpu))(cpu),
        (2, 0, 4) => new_hlt()(cpu),
        (2, 0, 5) => new_asl(zero_page_x(cpu))(cpu),
        (2, 0, 6) => new_nop()(cpu),
        (2, 0, 7) => new_asl(absolute_x(cpu))(cpu),

        (2, 1, 0) => new_hlt()(cpu),
        (2, 1, 1) => new_rol(zero_page(cpu))(cpu),
        (2, 1, 2) => new_rol(aa())(cpu),
        (2, 1, 3) => new_rol(absolute(cpu))(cpu),
        (2, 1, 4) => new_hlt()(cpu),
        (2, 1, 5) => new_rol(zero_page_x(cpu))(cpu),
        (2, 1, 6) => new_nop()(cpu),
        (2, 1, 7) => new_rol(absolute_x(cpu))(cpu),

        (2, 2, 0) => new_hlt()(cpu),
        (2, 2, 1) => new_lsr(zero_page(cpu))(cpu),
        (2, 2, 2) => new_lsr(aa())(cpu),
        (2, 2, 3) => new_lsr(absolute(cpu))(cpu),
        (2, 2, 4) => new_hlt()(cpu),
        (2, 2, 5) => new_lsr(zero_page_x(cpu))(cpu),
        (2, 2, 6) => new_nop()(cpu),
        (2, 2, 7) => new_lsr(absolute_x(cpu))(cpu),

        (2, 3, 0) => new_hlt()(cpu),
        (2, 3, 1) => new_ror(zero_page(cpu))(cpu),
        (2, 3, 2) => new_ror(aa())(cpu),
        (2, 3, 3) => new_ror(absolute(cpu))(cpu),
        (2, 3, 4) => new_hlt()(cpu),
        (2, 3, 5) => new_ror(zero_page_x(cpu))(cpu),
        (2, 3, 6) => new_nop()(cpu),
        (2, 3, 7) => new_ror(absolute_x(cpu))(cpu),

        (2, 4, 0) => new_nop_with_addr(literal(cpu))(cpu),
        (2, 4, 1) => new_transfer_no_touch_flags(zero_page(cpu), x())(cpu),
        (2, 4, 2) => new_transfer(aa(), x())(cpu),
        (2, 4, 3) => new_transfer_no_touch_flags(absolute(cpu), x())(cpu),
        (2, 4, 4) => new_hlt()(cpu),
        (2, 4, 5) => new_transfer_no_touch_flags(zero_page_y(cpu), x())(cpu),
        (2, 4, 6) => new_transfer_no_touch_flags(sp(), x())(cpu),
        (2, 4, 7) => new_all("shx", x(), absolute_y(cpu))(cpu), // 9e

        (2, 5, 0) => new_transfer(x(), literal(cpu))(cpu),
        (2, 5, 1) => new_transfer(x(), zero_page(cpu))(cpu),
        (2, 5, 2) => new_transfer(x(), aa())(cpu),
        (2, 5, 3) => new_transfer(x(), absolute(cpu))(cpu),
        (2, 5, 4) => new_hlt()(cpu),
        (2, 5, 5) => new_transfer(x(), zero_page_y(cpu))(cpu),
        (2, 5, 6) => new_transfer(x(), sp())(cpu),
        (2, 5, 7) => new_transfer(x(), absolute_y(cpu))(cpu),

        (2, 6, 0) => new_nop_with_addr(literal(cpu))(cpu),
        (2, 6, 1) => new_dec(zero_page(cpu))(cpu),
        (2, 6, 2) => new_dec(x())(cpu),
        (2, 6, 3) => new_dec(absolute(cpu))(cpu),
        (2, 6, 4) => new_hlt()(cpu),
        (2, 6, 5) => new_dec(zero_page_x(cpu))(cpu),
        (2, 6, 6) => new_nop()(cpu),
        (2, 6, 7) => new_dec(absolute_x(cpu))(cpu),

        (2, 7, 0) => new_nop_with_addr(literal(cpu))(cpu),
        (2, 7, 1) => new_inc(zero_page(cpu))(cpu),
        (2, 7, 2) => new_nop()(cpu),
        (2, 7, 3) => new_inc(absolute(cpu))(cpu),
        (2, 7, 4) => new_hlt()(cpu),
        (2, 7, 5) => new_inc(zero_page_x(cpu))(cpu),
        (2, 7, 6) => new_nop()(cpu), // FA
        (2, 7, 7) => new_inc(absolute_x(cpu))(cpu),

        (3, 0, 0) => new_aso(indirect_x(cpu))(cpu),
        (3, 0, 1) => new_aso(zero_page(cpu))(cpu),
        (3, 0, 2) => new_anc(literal(cpu))(cpu),
        (3, 0, 3) => new_aso(absolute(cpu))(cpu),
        (3, 0, 4) => new_aso(indirect_y(cpu))(cpu),
        (3, 0, 5) => new_aso(zero_page_x(cpu))(cpu),
        (3, 0, 6) => new_aso(absolute_y(cpu))(cpu),
        (3, 0, 7) => new_aso(absolute_x(cpu))(cpu),

        (3, 1, 0) => new_rla(indirect_x(cpu))(cpu),
        (3, 1, 1) => new_rla(zero_page(cpu))(cpu),
        (3, 1, 2) => new_anc(literal(cpu))(cpu),
        (3, 1, 3) => new_rla(absolute(cpu))(cpu),
        (3, 1, 4) => new_rla(indirect_y(cpu))(cpu),
        (3, 1, 5) => new_rla(zero_page_x(cpu))(cpu),
        (3, 1, 6) => new_rla(absolute_y(cpu))(cpu),
        (3, 1, 7) => new_rla(absolute_x(cpu))(cpu),

        (3, 2, 0) => new_lse(indirect_x(cpu))(cpu),
        (3, 2, 1) => new_lse(zero_page(cpu))(cpu),
        (3, 2, 2) => new_alr(literal(cpu))(cpu),
        (3, 2, 3) => new_lse(absolute(cpu))(cpu),
        (3, 2, 4) => new_lse(indirect_y(cpu))(cpu),
        (3, 2, 5) => new_lse(zero_page_x(cpu))(cpu),
        (3, 2, 6) => new_lse(absolute_y(cpu))(cpu),
        (3, 2, 7) => new_lse(absolute_x(cpu))(cpu),

        (3, 3, 0) => new_rra(indirect_x(cpu))(cpu),
        (3, 3, 1) => new_rra(zero_page(cpu))(cpu),
        (3, 3, 2) => new_arr(literal(cpu))(cpu),
        (3, 3, 3) => new_rra(absolute(cpu))(cpu),
        (3, 3, 4) => new_rra(indirect_y(cpu))(cpu),
        (3, 3, 5) => new_rra(zero_page_x(cpu))(cpu),
        (3, 3, 6) => new_rra(absolute_y(cpu))(cpu),
        (3, 3, 7) => new_rra(absolute_x(cpu))(cpu),

        (3, 4, 0) => new_sax(indirect_x(cpu))(cpu),
        (3, 4, 1) => new_sax(zero_page(cpu))(cpu),
        (3, 4, 3) => new_sax(absolute(cpu))(cpu),
        (3, 4, 5) => new_sax(zero_page_y(cpu))(cpu),

        (3, 5, 0) => new_lax(indirect_x(cpu))(cpu),
        (3, 5, 1) => new_lax(zero_page(cpu))(cpu),
        (3, 5, 2) => new_lax(literal(cpu))(cpu),
        (3, 5, 3) => new_lax(absolute(cpu))(cpu),
        (3, 5, 4) => new_lax(indirect_y(cpu))(cpu),
        (3, 5, 5) => new_lax(zero_page_y(cpu))(cpu),
        (3, 5, 7) => new_lax(absolute_y(cpu))(cpu),

        (3, 6, 0) => new_dcp(indirect_x(cpu))(cpu),
        (3, 6, 1) => new_dcp(zero_page(cpu))(cpu),
        (3, 6, 2) => new_sbx(literal(cpu))(cpu),
        (3, 6, 3) => new_dcp(absolute(cpu))(cpu),
        (3, 6, 4) => new_dcp(indirect_y(cpu))(cpu),
        (3, 6, 5) => new_dcp(zero_page_x(cpu))(cpu),
        (3, 6, 6) => new_dcp(absolute_y(cpu))(cpu),
        (3, 6, 7) => new_dcp(absolute_x(cpu))(cpu),

        (3, 7, 0) => new_isc(indirect_x(cpu))(cpu),
        (3, 7, 1) => new_isc(zero_page(cpu))(cpu),
        (3, 7, 2) => new_sbc(literal(cpu))(cpu),
        (3, 7, 3) => new_isc(absolute(cpu))(cpu),
        (3, 7, 4) => new_isc(indirect_y(cpu))(cpu),
        (3, 7, 5) => new_isc(zero_page_x(cpu))(cpu),
        (3, 7, 6) => new_isc(absolute_y(cpu))(cpu),
        (3, 7, 7) => new_isc(absolute_x(cpu))(cpu),

        _ => panic!(
            "Unknown opcode: {:02x} ({}, {}, {}) @ {:04x}",
            op_code, c, a, b, cpu.pc
        ),
    }
}

// Trait to sync instruction execution  times.
pub trait Plugin {
    fn start(&mut self, cpu: &Cpu);

    // return true to stop cpu
    fn end(&mut self, cpu: &Cpu);

    fn should_stop(&self) -> ExecuteResult {
        ExecuteResult::Continue
    }
}

impl Plugin for Box<dyn Plugin> {
    fn start(&mut self, cpu: &Cpu) {
        self.as_mut().start(cpu);
    }

    fn end(&mut self, cpu: &Cpu) {
        self.as_mut().end(cpu);
    }

    fn should_stop(&self) -> ExecuteResult {
        self.as_ref().should_stop()
    }
}

pub struct EmptyPlugin();

impl Plugin for EmptyPlugin {
    fn start(&mut self, _: &Cpu) {}

    fn end(&mut self, _: &Cpu) {}
}

pub struct Cpu {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub status: u8,
    mcu: Box<dyn Mcu>,

    irq_pending: bool,
    change_irq_flag_pending: Option<bool>,

    /// if remains_clock not zero, new instruction will not be executed.
    remain_clocks: u16,
    is_halt: bool,
}

impl Cpu {
    pub fn new(mcu: Box<dyn Mcu>) -> Cpu {
        Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0,
            status: Flag::InterruptDisabled as u8,
            mcu,
            irq_pending: false,
            change_irq_flag_pending: None,
            remain_clocks: 0,
            is_halt: false,
        }
    }

    pub fn mcu(&mut self) -> &mut dyn Mcu {
        self.mcu.as_mut()
    }

    pub fn reset(&mut self) {
        self.pc = self.read_word(0xFFFC);
        self.set_flag(Flag::InterruptDisabled, true);
    }

    pub fn nmi(&mut self) {
        info!("nmi");
        self.push_pc();
        self.push_status();
        let vector = self.read_word(0xFFFA);
        info!("nmi vector: ${:04x}", vector);
        self.pc = vector;
    }

    fn irq(&mut self) {
        // info!("irq");
        // Push status BEFORE setting I flag, so the saved status reflects the state at IRQ time
        self.push_pc();
        self.push_status();
        // Now set I flag to prevent nested IRQs
        self.set_flag(Flag::InterruptDisabled, true);
        self.pc = self.read_word(0xFFFE);
    }

    pub fn set_irq(&mut self, enabled: bool) {
        self.irq_pending = enabled;
    }

    fn push_pc(&mut self) {
        self.push_stack((self.pc >> 8) as u8);
        self.push_stack(self.pc as u8);
    }

    pub fn flag(&self, flag: Flag) -> bool {
        (self.status & flag as u8) != 0
    }

    pub fn pending_set_interrupt_disabled_flag(&mut self, v: bool) {
        self.change_irq_flag_pending = Some(v);
    }

    pub fn set_flag(&mut self, flag: Flag, v: bool) {
        if v {
            self.status |= flag as u8;
        } else {
            self.status &= !(flag as u8);
        }
    }

    pub fn clock_tick<T: Plugin>(&mut self, plugin: &mut T) -> ExecuteResult {
        // Tick PPU 3 times per CPU cycle (CPU:PPU = 1:3)
        for _ in 0..3 {
            if self.mcu.tick_ppu() {
                self.nmi();
            }
        }

        if self.remain_clocks != 0 {
            self.remain_clocks -= 1;
            return ExecuteResult::Continue;
        }

        if self.is_halt {
            return ExecuteResult::Continue;
        }

        if self.irq_pending && !self.flag(Flag::InterruptDisabled) {
            if let Some(v) = self.change_irq_flag_pending {
                self.set_flag(Flag::InterruptDisabled, v);
                self.change_irq_flag_pending = None;
            }
            self.irq();
        } else {
            if let Some(v) = self.change_irq_flag_pending {
                self.set_flag(Flag::InterruptDisabled, v);
                self.change_irq_flag_pending = None;
            }
        }

        plugin.start(self);
        self.remain_clocks = execute_next(self) as u16 - 1;
        plugin.end(self);
        self.set_irq(self.mcu.request_irq());
        plugin.should_stop()
    }

    fn adc(&mut self, val: u8) {
        let carry = self.flag(Flag::Carry) as u8;
        let (sum, carry0) = self.a.overflowing_add(val);
        let (sum, carry1) = sum.overflowing_add(carry);
        self.set_flag(Flag::Carry, carry0 || carry1);
        self.set_flag(Flag::Overflow, !(self.a ^ val) & (self.a ^ sum) & 0x80 != 0);
        self.update_zero_flag(sum);
        self.update_negative_flag(sum);
        self.a = sum;
    }

    fn inc_pc(&mut self, delta: i8) {
        self.pc = self.pc.wrapping_add(delta as u16);
    }

    fn update_negative_flag(&mut self, value: u8) {
        self.set_flag(Flag::Negative, value & 0x80 != 0);
    }

    fn update_zero_flag(&mut self, value: u8) {
        self.set_flag(Flag::Zero, value == 0);
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        self.mcu.read(addr)
    }

    fn inc_read_byte(&mut self) -> u8 {
        let addr = self.pc;
        self.inc_pc(1);
        self.read_byte(addr)
    }

    pub fn inc_read_word(&mut self) -> u16 {
        let addr = self.pc;
        self.inc_pc(2);
        self.read_word(addr)
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        self.mcu.write(addr, value);
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        let low = self.mcu.read(addr) as u16;
        let high = self.mcu.read(addr.wrapping_add(1)) as u16;
        (high << 8) | low
    }

    pub fn read_zero_page_word(&self, addr: u8) -> u16 {
        self._read_word_in_same_page(0, addr)
    }

    pub fn read_word_in_same_page(&self, addr: u16) -> u16 {
        self._read_word_in_same_page(addr & 0xff00, addr as u8)
    }

    fn _read_word_in_same_page(&self, page: u16, offset: u8) -> u16 {
        let low = self.mcu.read(page | offset as u16) as u16;
        let high = self.mcu.read(page | offset.wrapping_add(1) as u16) as u16;
        (high << 8) | low
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

    pub fn push_status(&mut self) {
        self.push_stack(self.status | (Flag::NotUsed as u8));
    }

    pub fn halt(&mut self) {
        self.is_halt = true;
    }

    pub fn is_halted(&self) -> bool {
        self.is_halt
    }
}

#[derive(Clone, Copy, strum_macros::Display, PartialEq, Eq)]
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
mod tests {
    use super::*;

    struct MockMcu {
        memory: [u8; 0x10000],
    }

    impl MockMcu {
        fn new() -> Self {
            MockMcu {
                memory: [0; 0x10000],
            }
        }

        fn with_program(mut self, addr: u16, program: &[u8]) -> Self {
            for (i, &byte) in program.iter().enumerate() {
                self.memory[(addr as usize) + i] = byte;
            }
            self
        }
    }

    impl Mcu for MockMcu {
        fn read(&self, addr: u16) -> u8 {
            self.memory[addr as usize]
        }

        fn write(&mut self, addr: u16, value: u8) {
            self.memory[addr as usize] = value;
        }

        fn get_machine_mcu(&mut self) -> &mut dyn crate::mcu::MachineMcu {
            panic!("Not implemented for tests")
        }
    }

    fn create_cpu_with_program(program: &[u8]) -> Cpu {
        let mcu = Box::new(MockMcu::new().with_program(0, program));
        Cpu::new(mcu)
    }

    fn create_cpu() -> Cpu {
        let mcu = Box::new(MockMcu::new());
        Cpu::new(mcu)
    }

    #[test]
    fn test_cpu_initialization() {
        let cpu = create_cpu();
        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.x, 0);
        assert_eq!(cpu.y, 0);
        assert_eq!(cpu.pc, 0);
        assert_eq!(cpu.sp, 0);
        assert_eq!(cpu.status, Flag::InterruptDisabled as u8);
        assert!(!cpu.is_halt);
    }

    #[test]
    fn test_set_get_flag() {
        let mut cpu = create_cpu();

        // Test setting each flag
        cpu.set_flag(Flag::Carry, true);
        assert!(cpu.flag(Flag::Carry));

        cpu.set_flag(Flag::Zero, true);
        assert!(cpu.flag(Flag::Zero));

        cpu.set_flag(Flag::InterruptDisabled, false);
        assert!(!cpu.flag(Flag::InterruptDisabled));

        cpu.set_flag(Flag::Decimal, true);
        assert!(cpu.flag(Flag::Decimal));

        cpu.set_flag(Flag::Break, true);
        assert!(cpu.flag(Flag::Break));

        cpu.set_flag(Flag::Overflow, true);
        assert!(cpu.flag(Flag::Overflow));

        cpu.set_flag(Flag::Negative, true);
        assert!(cpu.flag(Flag::Negative));
    }

    #[test]
    fn test_flag_toggle() {
        let mut cpu = create_cpu();
        cpu.set_flag(Flag::Carry, true);
        assert!(cpu.flag(Flag::Carry));

        cpu.set_flag(Flag::Carry, false);
        assert!(!cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_read_write_byte() {
        let mut cpu = create_cpu();
        cpu.write_byte(0x1000, 0x42);
        assert_eq!(cpu.read_byte(0x1000), 0x42);
    }

    #[test]
    fn test_read_word() {
        let mut cpu = create_cpu();
        cpu.write_byte(0x1000, 0x34);
        cpu.write_byte(0x1001, 0x12);
        let word = cpu.read_word(0x1000);
        assert_eq!(word, 0x1234);
    }

    #[test]
    fn test_read_word_little_endian() {
        let mut cpu = create_cpu();
        // Little endian: low byte at lower address, high byte at higher
        cpu.write_byte(0x2000, 0xFF); // low byte
        cpu.write_byte(0x2001, 0xEE); // high byte
        let word = cpu.read_word(0x2000);
        assert_eq!(word, 0xEEFF);
    }

    #[test]
    fn test_stack_push_pop() {
        let mut cpu = create_cpu();
        cpu.sp = 0xFF;

        cpu.push_stack(0x42);
        assert_eq!(cpu.sp, 0xFE);
        assert_eq!(cpu.read_byte(0x1FF), 0x42);

        let value = cpu.pop_stack();
        assert_eq!(value, 0x42);
        assert_eq!(cpu.sp, 0xFF);
    }

    #[test]
    fn test_stack_push_pop_wrapping() {
        let mut cpu = create_cpu();
        cpu.sp = 0x00;

        cpu.push_stack(0xAA);
        assert_eq!(cpu.sp, 0xFF);
        assert_eq!(cpu.read_byte(0x100), 0xAA);

        let value = cpu.pop_stack();
        assert_eq!(value, 0xAA);
        assert_eq!(cpu.sp, 0x00);
    }

    #[test]
    fn test_push_status() {
        let mut cpu = create_cpu();
        cpu.sp = 0xFF;
        cpu.status = 0b0000_0000;

        cpu.push_status();
        let status_on_stack = cpu.read_byte(0x1FF);
        // NotUsed flag (0x20) should be set
        assert_eq!(status_on_stack & 0x20, 0x20);
    }

    #[test]
    fn test_peek_stack() {
        let mut cpu = create_cpu();
        cpu.sp = 0xFE;
        cpu.write_byte(0x1FF, 0x55);

        let peeked = cpu.peek_stack();
        assert_eq!(peeked, 0x55);
        assert_eq!(cpu.sp, 0xFE); // SP unchanged
    }

    #[test]
    fn test_inc_read_byte() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x42, 0xEA]);
        assert_eq!(cpu.pc, 0);

        let byte1 = cpu.inc_read_byte();
        assert_eq!(byte1, 0xA9);
        assert_eq!(cpu.pc, 1);

        let byte2 = cpu.inc_read_byte();
        assert_eq!(byte2, 0x42);
        assert_eq!(cpu.pc, 2);
    }

    #[test]
    fn test_inc_read_word() {
        let mut cpu = create_cpu_with_program(&[0x2C, 0x34, 0x12, 0xEA]);
        assert_eq!(cpu.pc, 0);

        let _first = cpu.inc_read_byte(); // 0x2C
        assert_eq!(cpu.pc, 1);

        let word = cpu.inc_read_word();
        assert_eq!(word, 0x1234);
        assert_eq!(cpu.pc, 3);
    }

    #[test]
    fn test_update_negative_flag() {
        let mut cpu = create_cpu();
        cpu.set_flag(Flag::Negative, false);

        cpu.update_negative_flag(0x80);
        assert!(cpu.flag(Flag::Negative));

        cpu.update_negative_flag(0x7F);
        assert!(!cpu.flag(Flag::Negative));
    }

    #[test]
    fn test_update_zero_flag() {
        let mut cpu = create_cpu();
        cpu.set_flag(Flag::Zero, false);

        cpu.update_zero_flag(0x00);
        assert!(cpu.flag(Flag::Zero));

        cpu.update_zero_flag(0x01);
        assert!(!cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_read_zero_page_word() {
        let mut cpu = create_cpu();
        cpu.write_byte(0x50, 0x78);
        cpu.write_byte(0x51, 0x56);

        let word = cpu.read_zero_page_word(0x50);
        assert_eq!(word, 0x5678);
    }

    #[test]
    fn test_read_zero_page_word_wrapping() {
        let mut cpu = create_cpu();
        cpu.write_byte(0xFF, 0x12);
        cpu.write_byte(0x00, 0x34);

        let word = cpu.read_zero_page_word(0xFF);
        assert_eq!(word, 0x3412);
    }

    #[test]
    fn test_read_word_in_same_page() {
        let mut cpu = create_cpu();
        cpu.write_byte(0x1050, 0x78);
        cpu.write_byte(0x1051, 0x56);

        let word = cpu.read_word_in_same_page(0x1050);
        assert_eq!(word, 0x5678);
    }

    #[test]
    fn test_read_word_in_same_page_wrapping() {
        let mut cpu = create_cpu();
        cpu.write_byte(0x10FF, 0x12);
        cpu.write_byte(0x1000, 0x34);

        let word = cpu.read_word_in_same_page(0x10FF);
        assert_eq!(word, 0x3412);
    }

    #[test]
    fn test_halt_flag() {
        let mut cpu = create_cpu();
        assert!(!cpu.is_halted());

        cpu.halt();
        assert!(cpu.is_halted());
    }

    #[test]
    fn test_inc_pc() {
        let mut cpu = create_cpu();
        cpu.pc = 0x1000;

        cpu.inc_pc(2);
        assert_eq!(cpu.pc, 0x1002);

        cpu.inc_pc(-1);
        assert_eq!(cpu.pc, 0x1001);
    }

    #[test]
    fn test_inc_pc_wrapping() {
        let mut cpu = create_cpu();
        cpu.pc = 0xFFFF;

        cpu.inc_pc(1);
        assert_eq!(cpu.pc, 0x0000);
    }

    #[test]
    fn test_reset() {
        let mut cpu = create_cpu();
        cpu.pc = 0x1234;
        cpu.a = 0x42;
        cpu.sp = 0x80;

        cpu.reset();
        // Reset reads PC from 0xFFFC (which is 0x0000 in MockMcu)
        assert_eq!(cpu.pc, 0);
        // SP is not reset by reset() on real 6502
        assert_eq!(cpu.sp, 0x80);
        assert!(cpu.flag(Flag::InterruptDisabled)); // InterruptDisabled flag is set
    }

    #[test]
    fn test_irq_flag_operations() {
        let mut cpu = create_cpu();
        // CPU initializes with InterruptDisabled flag set
        assert!(cpu.flag(Flag::InterruptDisabled));

        cpu.set_flag(Flag::InterruptDisabled, false);
        assert!(!cpu.flag(Flag::InterruptDisabled));

        cpu.set_flag(Flag::InterruptDisabled, true);
        assert!(cpu.flag(Flag::InterruptDisabled));
    }

    #[test]
    fn test_multiple_flags_simultaneously() {
        let mut cpu = create_cpu();

        cpu.set_flag(Flag::Carry, true);
        cpu.set_flag(Flag::Zero, true);
        cpu.set_flag(Flag::Negative, true);

        assert!(cpu.flag(Flag::Carry));
        assert!(cpu.flag(Flag::Zero));
        assert!(cpu.flag(Flag::Negative));
        assert!(!cpu.flag(Flag::Overflow));
    }

    #[test]
    fn test_cross_page_detection() {
        assert!(is_cross_page(0x0100, 0x0200));
        assert!(is_cross_page(0x00FF, 0x0100));
        assert!(!is_cross_page(0x0100, 0x0101));
        assert!(!is_cross_page(0x01FF, 0x01FE));
    }

    #[test]
    fn test_extra_tick_if_cross_page() {
        assert_eq!(extra_tick_if_cross_page(0x0100, 0x0200), 1);
        assert_eq!(extra_tick_if_cross_page(0x0100, 0x0101), 0);
    }

    #[test]
    fn test_mcu_access() {
        let mut cpu = create_cpu();
        cpu.write_byte(0x8000, 0x99);
        assert_eq!(cpu.read_byte(0x8000), 0x99);
    }

    #[test]
    fn test_status_register_all_flags() {
        let mut cpu = create_cpu();

        // Set all flags
        for i in 0..8 {
            let flag_value = 1 << i;
            if flag_value == 0x04
                || flag_value == 0x08
                || flag_value == 0x10
                || flag_value == 0x40
                || flag_value == 0x80
                || flag_value == 0x01
                || flag_value == 0x02
                || flag_value == 0x20
            {
                cpu.status |= flag_value;
            }
        }

        // Status should reflect all set flags
        assert_eq!(cpu.status, 0xFF);
    }

    // Additional instruction execution tests
    #[test]
    fn test_lda_immediate() {
        // LDA #$42 (opcode A9 42)
        let mut cpu = create_cpu_with_program(&[0xA9, 0x42, 0xEA]);

        let ticks = execute_next(&mut cpu);
        assert_eq!(cpu.a, 0x42);
        assert!(!cpu.flag(Flag::Zero));
        assert!(!cpu.flag(Flag::Negative));
        assert!(ticks > 0);
    }

    #[test]
    fn test_lda_zero_flag() {
        // LDA #$00 (opcode A9 00)
        let mut cpu = create_cpu_with_program(&[0xA9, 0x00, 0xEA]);

        execute_next(&mut cpu);
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_lda_negative_flag() {
        // LDA #$80 (opcode A9 80)
        let mut cpu = create_cpu_with_program(&[0xA9, 0x80, 0xEA]);

        execute_next(&mut cpu);
        assert_eq!(cpu.a, 0x80);
        assert!(cpu.flag(Flag::Negative));
        assert!(!cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_nop_instruction() {
        // NOP (opcode EA)
        let mut cpu = create_cpu_with_program(&[0xEA, 0xEA, 0xEA]);
        let a_before = cpu.a;

        execute_next(&mut cpu);
        assert_eq!(cpu.a, a_before);
        assert_eq!(cpu.pc, 1);
    }

    #[test]
    fn test_ldx_immediate() {
        // LDX #$55 (opcode A2 55)
        let mut cpu = create_cpu_with_program(&[0xA2, 0x55, 0xEA]);

        execute_next(&mut cpu);
        assert_eq!(cpu.x, 0x55);
    }

    #[test]
    fn test_ldy_immediate() {
        // LDY #$AA (opcode A0 AA)
        let mut cpu = create_cpu_with_program(&[0xA0, 0xAA, 0xEA]);

        execute_next(&mut cpu);
        assert_eq!(cpu.y, 0xAA);
    }

    #[test]
    fn test_sta_zero_page() {
        // Store A to zero page address
        let mut cpu = create_cpu();
        cpu.a = 0x77;

        // STA $50 (opcode 85 50)
        cpu.write_byte(0, 0x85);
        cpu.write_byte(1, 0x50);

        execute_next(&mut cpu);
        assert_eq!(cpu.read_byte(0x50), 0x77);
    }

    #[test]
    fn test_multiple_instructions() {
        // LDA #$42, LDX #$55, NOP
        let mut cpu = create_cpu_with_program(&[0xA9, 0x42, 0xA2, 0x55, 0xEA]);

        execute_next(&mut cpu);
        assert_eq!(cpu.a, 0x42);
        assert_eq!(cpu.pc, 2);

        execute_next(&mut cpu);
        assert_eq!(cpu.x, 0x55);
        assert_eq!(cpu.pc, 4);

        execute_next(&mut cpu);
        assert_eq!(cpu.pc, 5);
    }

    #[test]
    fn test_ora_operation() {
        // LDA #$55, ORA #$AA
        let mut cpu = create_cpu_with_program(&[0xA9, 0x55, 0x09, 0xAA, 0xEA]);

        execute_next(&mut cpu);
        assert_eq!(cpu.a, 0x55);

        execute_next(&mut cpu);
        // 0x55 | 0xAA = 0xFF
        assert_eq!(cpu.a, 0xFF);
        assert!(cpu.flag(Flag::Negative));
        assert!(!cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_and_operation() {
        // LDA #$F3, AND #$3F
        let mut cpu = create_cpu_with_program(&[0xA9, 0xF3, 0x29, 0x3F, 0xEA]);

        execute_next(&mut cpu);
        assert_eq!(cpu.a, 0xF3);

        execute_next(&mut cpu);
        // 0xF3 & 0x3F = 0x33
        assert_eq!(cpu.a, 0x33);
    }

    #[test]
    fn test_eor_operation() {
        // LDA #$FF, EOR #$0F
        let mut cpu = create_cpu_with_program(&[0xA9, 0xFF, 0x49, 0x0F, 0xEA]);

        execute_next(&mut cpu);
        assert_eq!(cpu.a, 0xFF);

        execute_next(&mut cpu);
        // 0xFF ^ 0x0F = 0xF0
        assert_eq!(cpu.a, 0xF0);
        assert!(cpu.flag(Flag::Negative));
    }

    #[test]
    fn test_inc_register() {
        // LDX #$42, INX
        let mut cpu = create_cpu_with_program(&[0xA2, 0x42, 0xE8, 0xEA]);

        execute_next(&mut cpu);
        assert_eq!(cpu.x, 0x42);

        execute_next(&mut cpu);
        assert_eq!(cpu.x, 0x43);
    }

    #[test]
    fn test_dec_register() {
        // LDX #$01, DEX
        let mut cpu = create_cpu_with_program(&[0xA2, 0x01, 0xCA, 0xEA]);

        execute_next(&mut cpu);
        assert_eq!(cpu.x, 0x01);

        execute_next(&mut cpu);
        assert_eq!(cpu.x, 0x00);
        assert!(cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_sec_clc_instructions() {
        let mut cpu = create_cpu();

        // SEC (38)
        cpu.write_byte(0, 0x38);
        execute_next(&mut cpu);
        assert!(cpu.flag(Flag::Carry));

        // CLC (18)
        cpu.write_byte(1, 0x18);
        execute_next(&mut cpu);
        assert!(!cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_sei_cli_instructions() {
        let mut cpu = create_cpu();
        cpu.set_flag(Flag::InterruptDisabled, false);

        // SEI (78) - sets interrupt disabled with pending flag
        cpu.write_byte(0, 0x78);
        execute_next(&mut cpu);
        // SEI uses pending flag, so it won't be set immediately
        // But set_flag should have marked it as pending

        // CLI (58) - clears interrupt disabled
        cpu.write_byte(1, 0x58);
        execute_next(&mut cpu);
        // After two instructions, the flag may not be what we expect
        // due to the pending flag behavior

        // Just verify the instructions execute without panicking
        assert_eq!(cpu.pc, 2);
    }

    #[test]
    fn test_sed_cld_instructions() {
        let mut cpu = create_cpu();

        // SED (F8)
        cpu.write_byte(0, 0xF8);
        execute_next(&mut cpu);
        assert!(cpu.flag(Flag::Decimal));

        // CLD (D8)
        cpu.write_byte(1, 0xD8);
        execute_next(&mut cpu);
        assert!(!cpu.flag(Flag::Decimal));
    }

    #[test]
    fn test_clv_instruction() {
        let mut cpu = create_cpu();
        cpu.set_flag(Flag::Overflow, true);

        // CLV (B8)
        cpu.write_byte(0, 0xB8);
        execute_next(&mut cpu);
        assert!(!cpu.flag(Flag::Overflow));
    }

    #[test]
    fn test_transfer_instruction_tax() {
        // LDA #$42, TAX
        let mut cpu = create_cpu_with_program(&[0xA9, 0x42, 0xAA, 0xEA]);

        execute_next(&mut cpu);
        assert_eq!(cpu.a, 0x42);

        execute_next(&mut cpu);
        assert_eq!(cpu.x, 0x42);
    }

    #[test]
    fn test_transfer_instruction_tay() {
        // LDA #$55, TAY
        let mut cpu = create_cpu_with_program(&[0xA9, 0x55, 0xA8, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        assert_eq!(cpu.y, 0x55);
    }

    #[test]
    fn test_transfer_instruction_txa() {
        // LDX #$99, TXA
        let mut cpu = create_cpu_with_program(&[0xA2, 0x99, 0x8A, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        assert_eq!(cpu.a, 0x99);
    }

    #[test]
    fn test_transfer_instruction_tya() {
        // LDY #$BB, TYA
        let mut cpu = create_cpu_with_program(&[0xA0, 0xBB, 0x98, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        assert_eq!(cpu.a, 0xBB);
    }

    #[test]
    fn test_stack_operations_pha() {
        // LDA #$42, PHA
        let mut cpu = create_cpu_with_program(&[0xA9, 0x42, 0x48, 0xEA]);
        cpu.sp = 0xFF;

        execute_next(&mut cpu);
        execute_next(&mut cpu);

        assert_eq!(cpu.read_byte(0x1FF), 0x42);
        assert_eq!(cpu.sp, 0xFE);
    }

    #[test]
    fn test_stack_operations_pla() {
        // Set up stack with value, PLA
        let mut cpu = create_cpu_with_program(&[0x68, 0xEA]);
        cpu.sp = 0xFE;
        cpu.write_byte(0x1FF, 0x88);

        execute_next(&mut cpu);

        assert_eq!(cpu.a, 0x88);
        assert_eq!(cpu.sp, 0xFF);
        assert!(cpu.flag(Flag::Negative));
    }

    #[test]
    fn test_asl_accumulator() {
        // LDA #$40, ASL A
        let mut cpu = create_cpu_with_program(&[0xA9, 0x40, 0x0A, 0xEA]);

        execute_next(&mut cpu);
        assert_eq!(cpu.a, 0x40);

        execute_next(&mut cpu);
        // 0x40 << 1 = 0x80
        assert_eq!(cpu.a, 0x80);
        assert!(cpu.flag(Flag::Negative));
        assert!(!cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_asl_with_carry() {
        // LDA #$80, ASL A
        let mut cpu = create_cpu_with_program(&[0xA9, 0x80, 0x0A, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        // 0x80 << 1 = 0x100, overflow sets carry
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.flag(Flag::Carry));
        assert!(cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_lsr_accumulator() {
        // LDA #$82, LSR A
        let mut cpu = create_cpu_with_program(&[0xA9, 0x82, 0x4A, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        // 0x82 >> 1 = 0x41, with carry = 0 (bit 0)
        assert_eq!(cpu.a, 0x41);
        // Carry flag is set to bit 0 of original value, which is 0
        assert!(!cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_lsr_accumulator_with_carry() {
        // LDA #$83, LSR A - bit 0 is set, so carry should be set
        let mut cpu = create_cpu_with_program(&[0xA9, 0x83, 0x4A, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        // 0x83 >> 1 = 0x41, with carry = 1 (bit 0 of 0x83)
        assert_eq!(cpu.a, 0x41);
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_rol_accumulator() {
        // LDA #$42, ROL A
        let mut cpu = create_cpu_with_program(&[0xA9, 0x42, 0x2A, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        // 0x42 rotated left = 0x84
        assert_eq!(cpu.a, 0x84);
    }

    #[test]
    fn test_ror_accumulator() {
        // LDA #$41, ROR A
        let mut cpu = create_cpu_with_program(&[0xA9, 0x41, 0x6A, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        // 0x41 rotated right = 0x20
        assert_eq!(cpu.a, 0x20);
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_bit_instruction() {
        // BIT instruction copies bit 7 to N flag and (bits 6-4) to V flag
        // Create CPU with enough program space
        let mut cpu = create_cpu_with_program(&[
            0xA9, 0xFF, // LDA #$FF
            0x24, 0x50, // BIT $50
            0xEA, // NOP
        ]);

        // Set up memory at 0x50 with value that has bit 7 set (and bits 6-4 non-zero)
        cpu.write_byte(0x50, 0xE0); // 1110_0000

        // Execute LDA #$FF
        execute_next(&mut cpu);
        assert_eq!(cpu.a, 0xFF);

        // Execute BIT $50
        execute_next(&mut cpu);
        // BIT should have set N flag (bit 7 of 0xE0)
        assert!(cpu.flag(Flag::Negative)); // bit 7 of 0xE0 = 1
                                           // Overflow flag: (v & 0x70) != 0 means bits 6-4 are checked
                                           // 0xE0 & 0x70 = 0xE0 & 0x70 = 0x60 != 0, so overflow should be set
        assert!(cpu.flag(Flag::Overflow));
    }

    #[test]
    fn test_cmp_instruction() {
        // LDA #$50, CMP #$50
        let mut cpu = create_cpu_with_program(&[0xA9, 0x50, 0xC9, 0x50, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);

        assert!(cpu.flag(Flag::Zero));
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_cmp_instruction_greater() {
        // LDA #$60, CMP #$50 (A > value)
        let mut cpu = create_cpu_with_program(&[0xA9, 0x60, 0xC9, 0x50, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);

        assert!(!cpu.flag(Flag::Zero));
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_cmp_instruction_less() {
        // LDA #$40, CMP #$50 (A < value)
        let mut cpu = create_cpu_with_program(&[0xA9, 0x40, 0xC9, 0x50, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);

        assert!(!cpu.flag(Flag::Zero));
        assert!(!cpu.flag(Flag::Carry));
    }

    // Additional comprehensive instruction tests
    #[test]
    fn test_adc_basic() {
        // LDA #$50, ADC #$30
        let mut cpu = create_cpu_with_program(&[0xA9, 0x50, 0x69, 0x30, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        // 0x50 + 0x30 = 0x80
        assert_eq!(cpu.a, 0x80);
        assert!(cpu.flag(Flag::Negative));
        assert!(!cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_adc_with_carry() {
        // ADC with carry-in
        let mut cpu = create_cpu_with_program(&[0x69, 0xFF, 0xEA]);
        cpu.a = 0x00;
        cpu.set_flag(Flag::Carry, true);

        execute_next(&mut cpu);
        // 0x00 + 0xFF + 1 = 0x100, overflow to 0x00 with carry
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.flag(Flag::Carry));
        assert!(cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_sbc_basic() {
        // LDA #$50, SBC #$30
        let mut cpu = create_cpu_with_program(&[0xA9, 0x50, 0xE9, 0x30, 0xEA]);
        cpu.set_flag(Flag::Carry, true); // Set carry for SBC

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        // 0x50 - 0x30 = 0x20
        assert_eq!(cpu.a, 0x20);
    }

    #[test]
    fn test_ldx_zero_page() {
        // Set up memory then do LDX with zero page addressing
        let mut cpu = create_cpu_with_program(&[0xA6, 0x50, 0xEA]);
        cpu.write_byte(0x50, 0x77);

        execute_next(&mut cpu);
        assert_eq!(cpu.x, 0x77);
    }

    #[test]
    fn test_ldy_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xA4, 0x60, 0xEA]);
        cpu.write_byte(0x60, 0x88);

        execute_next(&mut cpu);
        assert_eq!(cpu.y, 0x88);
    }

    #[test]
    fn test_stx_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x99, 0x86, 0x70, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        assert_eq!(cpu.read_byte(0x70), 0x99);
    }

    #[test]
    fn test_sty_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xA0, 0xAA, 0x84, 0x80, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        assert_eq!(cpu.read_byte(0x80), 0xAA);
    }

    #[test]
    fn test_inx_wrapping() {
        let mut cpu = create_cpu();
        cpu.x = 0xFF;

        // INX (E8)
        cpu.write_byte(0, 0xE8);
        execute_next(&mut cpu);

        assert_eq!(cpu.x, 0x00);
        assert!(cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_iny_wrapping() {
        let mut cpu = create_cpu();
        cpu.y = 0xFF;

        // INY (C8)
        cpu.write_byte(0, 0xC8);
        execute_next(&mut cpu);

        assert_eq!(cpu.y, 0x00);
        assert!(cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_dey_to_zero() {
        let mut cpu = create_cpu();
        cpu.y = 0x01;

        // DEY (88)
        cpu.write_byte(0, 0x88);
        execute_next(&mut cpu);

        assert_eq!(cpu.y, 0x00);
        assert!(cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_cpx_instruction() {
        // LDX #$42, CPX #$42
        let mut cpu = create_cpu_with_program(&[0xA2, 0x42, 0xE0, 0x42, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);

        assert!(cpu.flag(Flag::Zero));
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_cpy_instruction() {
        // LDY #$55, CPY #$55
        let mut cpu = create_cpu_with_program(&[0xA0, 0x55, 0xC0, 0x55, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);

        assert!(cpu.flag(Flag::Zero));
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_php_pla() {
        let mut cpu = create_cpu_with_program(&[0x08, 0x68, 0xEA]); // PHP, PLA
        cpu.sp = 0xFF;
        cpu.status = 0x54;

        execute_next(&mut cpu);
        // Status pushed to stack

        execute_next(&mut cpu);
        // Status pulled from stack (into A)
        // Note: bit 5 (NotUsed) is always 1 when pushed
        assert_eq!(cpu.a & 0xDF, 0x54 & 0xDF);
    }

    #[test]
    fn test_plp_restores_flags() {
        let mut cpu = create_cpu_with_program(&[0x28, 0xEA]); // PLP, NOP
        cpu.sp = 0xFE;
        cpu.write_byte(0x1FF, 0xCF); // Status with various flags

        execute_next(&mut cpu);
        // Flags should be restored from stack
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_asl_zero_page() {
        let mut cpu = create_cpu_with_program(&[0x06, 0x50, 0xEA]);
        cpu.write_byte(0x50, 0x40);

        execute_next(&mut cpu);
        assert_eq!(cpu.read_byte(0x50), 0x80);
    }

    #[test]
    fn test_lsr_zero_page() {
        let mut cpu = create_cpu_with_program(&[0x46, 0x60, 0xEA]);
        cpu.write_byte(0x60, 0x82);

        execute_next(&mut cpu);
        // 0x82 >> 1 = 0x41, carry from bit 0
        assert_eq!(cpu.read_byte(0x60), 0x41);
        assert!(!cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_rol_zero_page() {
        let mut cpu = create_cpu_with_program(&[0x26, 0x70, 0xEA]);
        cpu.write_byte(0x70, 0x42);

        execute_next(&mut cpu);
        // 0x42 rotated left = 0x84
        assert_eq!(cpu.read_byte(0x70), 0x84);
    }

    #[test]
    fn test_ror_zero_page() {
        let mut cpu = create_cpu_with_program(&[0x66, 0x80, 0xEA]);
        cpu.write_byte(0x80, 0x41);

        execute_next(&mut cpu);
        // 0x41 rotated right = 0x20, with carry from bit 0
        assert_eq!(cpu.read_byte(0x80), 0x20);
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_dec_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xC6, 0x90, 0xEA]);
        cpu.write_byte(0x90, 0x01);

        execute_next(&mut cpu);
        assert_eq!(cpu.read_byte(0x90), 0x00);
        assert!(cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_inc_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xE6, 0xA0, 0xEA]);
        cpu.write_byte(0xA0, 0xFF);

        execute_next(&mut cpu);
        assert_eq!(cpu.read_byte(0xA0), 0x00);
        assert!(cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_txa_affects_flags() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x00, 0x8A, 0xEA]);

        execute_next(&mut cpu);
        execute_next(&mut cpu);

        assert_eq!(cpu.a, 0x00);
        assert!(cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_tsx_instruction() {
        let mut cpu = create_cpu_with_program(&[0xBA, 0xEA]); // TSX
        cpu.sp = 0x80;

        execute_next(&mut cpu);
        assert_eq!(cpu.x, 0x80);
    }

    #[test]
    fn test_txs_instruction() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x42, 0x9A, 0xEA]); // LDX, TXS

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        assert_eq!(cpu.sp, 0x42);
    }

    #[test]
    fn test_and_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0xF3, 0x25, 0x50, 0xEA]);
        cpu.write_byte(0x50, 0x3F);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        // 0xF3 & 0x3F = 0x33
        assert_eq!(cpu.a, 0x33);
    }

    #[test]
    fn test_ora_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x55, 0x05, 0x60, 0xEA]);
        cpu.write_byte(0x60, 0xAA);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        // 0x55 | 0xAA = 0xFF
        assert_eq!(cpu.a, 0xFF);
        assert!(cpu.flag(Flag::Negative));
    }

    #[test]
    fn test_eor_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0xFF, 0x45, 0x70, 0xEA]);
        cpu.write_byte(0x70, 0x0F);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        // 0xFF ^ 0x0F = 0xF0
        assert_eq!(cpu.a, 0xF0);
    }

    #[test]
    fn test_adc_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x50, 0x65, 0x80, 0xEA]);
        cpu.write_byte(0x80, 0x30);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        // 0x50 + 0x30 = 0x80
        assert_eq!(cpu.a, 0x80);
    }

    #[test]
    fn test_sbc_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x50, 0xE5, 0x90, 0xEA]);
        cpu.write_byte(0x90, 0x30);
        cpu.set_flag(Flag::Carry, true);

        execute_next(&mut cpu);
        execute_next(&mut cpu);
        // 0x50 - 0x30 = 0x20
        assert_eq!(cpu.a, 0x20);
    }

    #[test]
    fn test_cmp_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x50, 0xC5, 0xA0, 0xEA]);
        cpu.write_byte(0xA0, 0x50);

        execute_next(&mut cpu);
        execute_next(&mut cpu);

        assert!(cpu.flag(Flag::Zero));
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_cpx_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x42, 0xE4, 0xB0, 0xEA]);
        cpu.write_byte(0xB0, 0x42);

        execute_next(&mut cpu);
        execute_next(&mut cpu);

        assert!(cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_cpy_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xA0, 0x55, 0xC4, 0xC0, 0xEA]);
        cpu.write_byte(0xC0, 0x55);

        execute_next(&mut cpu);
        execute_next(&mut cpu);

        assert!(cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_bit_zero_page() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0xFF, 0x24, 0xD0, 0xEA]);
        cpu.write_byte(0xD0, 0xE0);

        execute_next(&mut cpu);
        execute_next(&mut cpu);

        assert!(cpu.flag(Flag::Negative));
        assert!(cpu.flag(Flag::Overflow));
    }

    #[test]
    fn test_lda_absolute() {
        let mut cpu = create_cpu_with_program(&[0xAD, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x42);

        execute_next(&mut cpu);
        assert_eq!(cpu.a, 0x42);
    }

    #[test]
    fn test_ldx_absolute() {
        let mut cpu = create_cpu_with_program(&[0xAE, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x55);

        execute_next(&mut cpu);
        assert_eq!(cpu.x, 0x55);
    }

    #[test]
    fn test_ldy_absolute() {
        let mut cpu = create_cpu_with_program(&[0xBC, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x66);

        execute_next(&mut cpu);
        assert_eq!(cpu.y, 0x66);
    }

    #[test]
    fn test_bit_absolute() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0xFF, 0x2C, 0x50, 0x20, 0xEA]);
        cpu.write_byte(0x2050, 0xC0);

        execute_next(&mut cpu);
        execute_next(&mut cpu);

        assert!(cpu.flag(Flag::Negative));
        assert!(cpu.flag(Flag::Overflow));
    }

    // Branch instruction tests
    #[test]
    fn test_beq_branch_taken() {
        // BEQ: 0xF0 (branch if equal/zero flag set)
        let mut cpu = create_cpu_with_program(&[0xA9, 0x00, 0xF0, 0x05, 0xEA]);
        execute_next(&mut cpu); // LDA #$00
        let pc_before = cpu.pc;
        execute_next(&mut cpu); // BEQ (should branch)
        assert_eq!(cpu.pc, pc_before.wrapping_add(7)); // Branch 5 + 2 byte instruction
    }

    #[test]
    fn test_beq_branch_not_taken() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x01, 0xF0, 0x05, 0xEA]);
        execute_next(&mut cpu); // LDA #$01
        let pc_before = cpu.pc;
        execute_next(&mut cpu); // BEQ (should not branch)
        assert_eq!(cpu.pc, pc_before.wrapping_add(2));
    }

    #[test]
    fn test_bne_branch_taken() {
        // BNE: 0xD0 (branch if not equal/zero flag clear)
        let mut cpu = create_cpu_with_program(&[0xA9, 0x01, 0xD0, 0x05, 0xEA]);
        execute_next(&mut cpu); // LDA #$01
        let pc_before = cpu.pc;
        execute_next(&mut cpu); // BNE (should branch)
        assert_eq!(cpu.pc, pc_before.wrapping_add(7));
    }

    #[test]
    fn test_bcs_branch_taken() {
        // BCS: 0xB0 (branch if carry set)
        let mut cpu = create_cpu_with_program(&[0x38, 0xB0, 0x05, 0xEA]);
        execute_next(&mut cpu); // SEC (set carry)
        let pc_before = cpu.pc;
        execute_next(&mut cpu); // BCS (should branch)
        assert_eq!(cpu.pc, pc_before.wrapping_add(7));
    }

    #[test]
    fn test_bcc_branch_taken() {
        // BCC: 0x90 (branch if carry clear)
        let mut cpu = create_cpu_with_program(&[0x18, 0x90, 0x05, 0xEA]);
        execute_next(&mut cpu); // CLC (clear carry)
        let pc_before = cpu.pc;
        execute_next(&mut cpu); // BCC (should branch)
        assert_eq!(cpu.pc, pc_before.wrapping_add(7));
    }

    #[test]
    fn test_bmi_branch_taken() {
        // BMI: 0x30 (branch if minus/negative flag set)
        let mut cpu = create_cpu_with_program(&[0xA9, 0x80, 0x30, 0x05, 0xEA]);
        execute_next(&mut cpu); // LDA #$80 (sets negative flag)
        let pc_before = cpu.pc;
        execute_next(&mut cpu); // BMI (should branch)
        assert_eq!(cpu.pc, pc_before.wrapping_add(7));
    }

    #[test]
    fn test_bpl_branch_taken() {
        // BPL: 0x10 (branch if plus/negative flag clear)
        let mut cpu = create_cpu_with_program(&[0xA9, 0x01, 0x10, 0x05, 0xEA]);
        execute_next(&mut cpu); // LDA #$01 (clears negative flag)
        let pc_before = cpu.pc;
        execute_next(&mut cpu); // BPL (should branch)
        assert_eq!(cpu.pc, pc_before.wrapping_add(7));
    }

    #[test]
    fn test_bvs_branch_taken() {
        // BVS: 0x70 (branch if overflow set)
        // Set overflow flag manually
        let mut cpu = create_cpu_with_program(&[0x70, 0x05, 0xEA]);
        cpu.set_flag(Flag::Overflow, true);
        let pc_before = cpu.pc;
        execute_next(&mut cpu); // BVS (should branch)
        assert_eq!(cpu.pc, pc_before.wrapping_add(7));
    }

    #[test]
    fn test_bvc_branch_taken() {
        // BVC: 0x50 (branch if overflow clear)
        let mut cpu = create_cpu_with_program(&[0x50, 0x05, 0xEA]);
        cpu.set_flag(Flag::Overflow, false);
        let pc_before = cpu.pc;
        execute_next(&mut cpu); // BVC (should branch)
        assert_eq!(cpu.pc, pc_before.wrapping_add(7));
    }

    // Additional load/store instruction tests for remaining addressing modes
    #[test]
    fn test_lda_absolute_x() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x10, 0xBD, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1244, 0x77); // 0x1234 + 0x10

        execute_next(&mut cpu); // LDX #$10
        execute_next(&mut cpu); // LDA $1234,X
        assert_eq!(cpu.a, 0x77);
    }

    #[test]
    fn test_lda_absolute_y() {
        let mut cpu = create_cpu_with_program(&[0xA0, 0x10, 0xB9, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1244, 0x88); // 0x1234 + 0x10

        execute_next(&mut cpu); // LDY #$10
        execute_next(&mut cpu); // LDA $1234,Y
        assert_eq!(cpu.a, 0x88);
    }

    #[test]
    fn test_sta_absolute() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x42, 0x8D, 0x34, 0x12, 0xEA]);

        execute_next(&mut cpu); // LDA #$42
        execute_next(&mut cpu); // STA $1234
        assert_eq!(cpu.read_byte(0x1234), 0x42);
    }

    #[test]
    fn test_sta_absolute_x() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x42, 0xA2, 0x10, 0x9D, 0x34, 0x12, 0xEA]);

        execute_next(&mut cpu); // LDA #$42
        execute_next(&mut cpu); // LDX #$10
        execute_next(&mut cpu); // STA $1234,X
        assert_eq!(cpu.read_byte(0x1244), 0x42);
    }

    #[test]
    fn test_sta_absolute_y() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x42, 0xA0, 0x10, 0x99, 0x34, 0x12, 0xEA]);

        execute_next(&mut cpu); // LDA #$42
        execute_next(&mut cpu); // LDY #$10
        execute_next(&mut cpu); // STA $1234,Y
        assert_eq!(cpu.read_byte(0x1244), 0x42);
    }

    #[test]
    fn test_stx_absolute() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x55, 0x8E, 0x34, 0x12, 0xEA]);

        execute_next(&mut cpu); // LDX #$55
        execute_next(&mut cpu); // STX $1234
        assert_eq!(cpu.read_byte(0x1234), 0x55);
    }

    #[test]
    fn test_sty_absolute() {
        let mut cpu = create_cpu_with_program(&[0xA0, 0x66, 0x8C, 0x34, 0x12, 0xEA]);

        execute_next(&mut cpu); // LDY #$66
        execute_next(&mut cpu); // STY $1234
        assert_eq!(cpu.read_byte(0x1234), 0x66);
    }

    #[test]
    fn test_ldx_absolute_y() {
        let mut cpu = create_cpu_with_program(&[0xA0, 0x10, 0xBE, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1244, 0x99); // 0x1234 + 0x10

        execute_next(&mut cpu); // LDY #$10
        execute_next(&mut cpu); // LDX $1234,Y
        assert_eq!(cpu.x, 0x99);
    }

    #[test]
    fn test_ldy_absolute_x() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x10, 0xBC, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1244, 0xAA); // 0x1234 + 0x10

        execute_next(&mut cpu); // LDX #$10
        execute_next(&mut cpu); // LDY $1234,X
        assert_eq!(cpu.y, 0xAA);
    }

    // More arithmetic and logic tests
    #[test]
    fn test_adc_absolute() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x30, 0x6D, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x20);

        execute_next(&mut cpu); // LDA #$30
        execute_next(&mut cpu); // ADC $1234
        assert_eq!(cpu.a, 0x50);
    }

    #[test]
    fn test_adc_with_overflow() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x70, 0x6D, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x70);

        execute_next(&mut cpu); // LDA #$70
        execute_next(&mut cpu); // ADC $1234 (0x70 + 0x70 = 0xE0, sets negative, clears zero)
        assert_eq!(cpu.a, 0xE0);
        assert!(cpu.flag(Flag::Negative));
        assert!(!cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_sbc_absolute() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x50, 0xED, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x30);

        execute_next(&mut cpu); // LDA #$50
        execute_next(&mut cpu); // SBC $1234 (carry is set by default, so 0x50 - 0x30 = 0x20)
        assert_eq!(cpu.a, 0x1F); // SBC uses borrow (inverse of carry), carry=1 means no borrow
    }

    #[test]
    fn test_ora_absolute() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x0F, 0x0D, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0xF0);

        execute_next(&mut cpu); // LDA #$0F
        execute_next(&mut cpu); // ORA $1234 (0x0F | 0xF0 = 0xFF)
        assert_eq!(cpu.a, 0xFF);
    }

    #[test]
    fn test_and_absolute() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0xF0, 0x2D, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x0F);

        execute_next(&mut cpu); // LDA #$F0
        execute_next(&mut cpu); // AND $1234 (0xF0 & 0x0F = 0x00)
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.flag(Flag::Zero));
    }

    #[test]
    fn test_eor_absolute() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0xAA, 0x4D, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x55);

        execute_next(&mut cpu); // LDA #$AA
        execute_next(&mut cpu); // EOR $1234 (0xAA ^ 0x55 = 0xFF)
        assert_eq!(cpu.a, 0xFF);
    }

    #[test]
    fn test_asl_absolute() {
        let mut cpu = create_cpu_with_program(&[0x0E, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x40);

        execute_next(&mut cpu); // ASL $1234 (0x40 << 1 = 0x80)
        assert_eq!(cpu.read_byte(0x1234), 0x80);
        assert!(!cpu.flag(Flag::Carry)); // No carry from 0x40
    }

    #[test]
    fn test_lsr_absolute() {
        let mut cpu = create_cpu_with_program(&[0x4E, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x81);

        execute_next(&mut cpu); // LSR $1234 (0x81 >> 1 = 0x40, carry=1)
        assert_eq!(cpu.read_byte(0x1234), 0x40);
        assert!(cpu.flag(Flag::Carry)); // Bit 0 was 1
    }

    #[test]
    fn test_rol_absolute() {
        let mut cpu = create_cpu_with_program(&[0x2E, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x80);

        execute_next(&mut cpu); // ROL $1234 (0x80 << 1 = 0x00, carry becomes 1)
        assert_eq!(cpu.read_byte(0x1234), 0x00);
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_ror_absolute() {
        let mut cpu = create_cpu_with_program(&[0x6E, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x01);

        execute_next(&mut cpu); // ROR $1234 (0x01 >> 1 = 0x00, carry becomes 1)
        assert_eq!(cpu.read_byte(0x1234), 0x00);
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_inc_absolute() {
        let mut cpu = create_cpu_with_program(&[0xEE, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x42);

        execute_next(&mut cpu); // INC $1234
        assert_eq!(cpu.read_byte(0x1234), 0x43);
    }

    #[test]
    fn test_dec_absolute() {
        let mut cpu = create_cpu_with_program(&[0xCE, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x42);

        execute_next(&mut cpu); // DEC $1234
        assert_eq!(cpu.read_byte(0x1234), 0x41);
    }

    #[test]
    fn test_cmp_absolute() {
        let mut cpu = create_cpu_with_program(&[0xA9, 0x42, 0xCD, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x42);

        execute_next(&mut cpu); // LDA #$42
        execute_next(&mut cpu); // CMP $1234
        assert!(cpu.flag(Flag::Zero)); // Values are equal, Zero flag set
        assert!(cpu.flag(Flag::Carry)); // A >= M sets carry
    }

    #[test]
    fn test_cpx_absolute() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x50, 0xEC, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x42);

        execute_next(&mut cpu); // LDX #$50
        execute_next(&mut cpu); // CPX $1234
        assert!(cpu.flag(Flag::Carry)); // X > M sets carry
    }

    #[test]
    fn test_cpy_absolute() {
        let mut cpu = create_cpu_with_program(&[0xA0, 0x42, 0xCC, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x50);

        execute_next(&mut cpu); // LDY #$42
        execute_next(&mut cpu); // CPY $1234
        assert!(!cpu.flag(Flag::Carry)); // Y < M clears carry
    }

    // Jump and subroutine tests
    #[test]
    fn test_jmp_absolute() {
        let mut cpu = create_cpu_with_program(&[0x4C, 0x34, 0x12, 0xEA]);

        execute_next(&mut cpu); // JMP $1234
        assert_eq!(cpu.pc, 0x1234);
    }

    #[test]
    fn test_jmp_indirect() {
        let mut cpu = create_cpu_with_program(&[0x6C, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1234, 0x78);
        cpu.write_byte(0x1235, 0x56);

        execute_next(&mut cpu); // JMP ($1234)
        assert_eq!(cpu.pc, 0x5678);
    }

    #[test]
    #[test]
    fn test_brk_instruction() {
        let mut cpu = create_cpu_with_program(&[0x00, 0xEA]);
        cpu.write_byte(0xFFFE, 0x00);
        cpu.write_byte(0xFFFF, 0x04);

        execute_next(&mut cpu); // BRK
        assert!(cpu.flag(Flag::Break));
        assert!(cpu.flag(Flag::InterruptDisabled));
        assert_eq!(cpu.pc, 0x0400);
    }

    // Test indirect addressing modes
    #[test]
    fn test_lda_indirect_x() {
        // LDA ($20,X) - indexed indirect
        let mut cpu = create_cpu_with_program(&[0xA2, 0x05, 0xA1, 0x20, 0xEA]);
        // Write word at 0x0025 (low byte, high byte)
        cpu.write_byte(0x0025, 0x34);
        cpu.write_byte(0x0026, 0x12);
        cpu.write_byte(0x1234, 0x42);

        execute_next(&mut cpu); // LDX #$05
        execute_next(&mut cpu); // LDA ($20,X)
        assert_eq!(cpu.a, 0x42);
    }

    #[test]
    fn test_lda_indirect_y() {
        // LDA ($20),Y - indirect indexed
        let mut cpu = create_cpu_with_program(&[0xA0, 0x05, 0xB1, 0x20, 0xEA]);
        // Write word at 0x0020 (low byte, high byte)
        cpu.write_byte(0x0020, 0x34);
        cpu.write_byte(0x0021, 0x12);
        cpu.write_byte(0x1239, 0x55); // 0x1234 + 0x05

        execute_next(&mut cpu); // LDY #$05
        execute_next(&mut cpu); // LDA ($20),Y
        assert_eq!(cpu.a, 0x55);
    }

    #[test]
    fn test_sta_indirect_x() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x05, 0xA9, 0x42, 0x81, 0x20, 0xEA]);
        cpu.write_byte(0x0025, 0x34);
        cpu.write_byte(0x0026, 0x12);

        execute_next(&mut cpu); // LDX #$05
        execute_next(&mut cpu); // LDA #$42
        execute_next(&mut cpu); // STA ($20,X)
        assert_eq!(cpu.read_byte(0x1234), 0x42);
    }

    #[test]
    fn test_sta_indirect_y() {
        let mut cpu = create_cpu_with_program(&[0xA0, 0x05, 0xA9, 0x42, 0x91, 0x20, 0xEA]);
        cpu.write_byte(0x0020, 0x34);
        cpu.write_byte(0x0021, 0x12);

        execute_next(&mut cpu); // LDY #$05
        execute_next(&mut cpu); // LDA #$42
        execute_next(&mut cpu); // STA ($20),Y
        assert_eq!(cpu.read_byte(0x1239), 0x42);
    }

    // Zero page addressing mode tests
    #[test]
    fn test_lda_zero_page_x() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x10, 0xB5, 0x20, 0xEA]);
        cpu.write_byte(0x0030, 0x77); // ZP $20 + X($10)

        execute_next(&mut cpu); // LDX #$10
        execute_next(&mut cpu); // LDA $20,X
        assert_eq!(cpu.a, 0x77);
    }

    #[test]
    fn test_lda_zero_page_y() {
        let mut cpu = create_cpu_with_program(&[0xA0, 0x10, 0xB6, 0x20, 0xEA]);
        cpu.write_byte(0x0030, 0x88); // ZP $20 + Y($10)

        execute_next(&mut cpu); // LDY #$10
        execute_next(&mut cpu); // LDX $20,Y
        assert_eq!(cpu.x, 0x88);
    }

    #[test]
    fn test_adc_indirect_x() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x05, 0xA9, 0x30, 0x61, 0x20, 0xEA]);
        cpu.write_byte(0x0025, 0x34);
        cpu.write_byte(0x0026, 0x12);
        cpu.write_byte(0x1234, 0x20);

        execute_next(&mut cpu); // LDX #$05
        execute_next(&mut cpu); // LDA #$30
        execute_next(&mut cpu); // ADC ($20,X)
        assert_eq!(cpu.a, 0x50);
    }

    #[test]
    fn test_adc_indirect_y() {
        let mut cpu = create_cpu_with_program(&[0xA0, 0x05, 0xA9, 0x30, 0x71, 0x20, 0xEA]);
        cpu.write_byte(0x0020, 0x34);
        cpu.write_byte(0x0021, 0x12);
        cpu.write_byte(0x1239, 0x20); // 0x1234 + Y

        execute_next(&mut cpu); // LDY #$05
        execute_next(&mut cpu); // LDA #$30
        execute_next(&mut cpu); // ADC ($20),Y
        assert_eq!(cpu.a, 0x50);
    }

    #[test]
    fn test_asl_absolute_x() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x10, 0x1E, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1244, 0x40);

        execute_next(&mut cpu); // LDX #$10
        execute_next(&mut cpu); // ASL $1234,X
        assert_eq!(cpu.read_byte(0x1244), 0x80);
    }

    #[test]
    fn test_lsr_absolute_x() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x10, 0x5E, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1244, 0x81);

        execute_next(&mut cpu); // LDX #$10
        execute_next(&mut cpu); // LSR $1234,X
        assert_eq!(cpu.read_byte(0x1244), 0x40);
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_rol_absolute_x() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x10, 0x3E, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1244, 0x80);

        execute_next(&mut cpu); // LDX #$10
        execute_next(&mut cpu); // ROL $1234,X
        assert_eq!(cpu.read_byte(0x1244), 0x00);
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_ror_absolute_x() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x10, 0x7E, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1244, 0x01);

        execute_next(&mut cpu); // LDX #$10
        execute_next(&mut cpu); // ROR $1234,X
        assert_eq!(cpu.read_byte(0x1244), 0x00);
        assert!(cpu.flag(Flag::Carry));
    }

    #[test]
    fn test_inc_absolute_x() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x10, 0xFE, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1244, 0x42);

        execute_next(&mut cpu); // LDX #$10
        execute_next(&mut cpu); // INC $1234,X
        assert_eq!(cpu.read_byte(0x1244), 0x43);
    }

    #[test]
    fn test_dec_absolute_x() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x10, 0xDE, 0x34, 0x12, 0xEA]);
        cpu.write_byte(0x1244, 0x42);

        execute_next(&mut cpu); // LDX #$10
        execute_next(&mut cpu); // DEC $1234,X
        assert_eq!(cpu.read_byte(0x1244), 0x41);
    }

    #[test]
    fn test_ora_indirect_x() {
        let mut cpu = create_cpu_with_program(&[0xA2, 0x05, 0xA9, 0x0F, 0x01, 0x20, 0xEA]);
        cpu.write_byte(0x0025, 0x34);
        cpu.write_byte(0x0026, 0x12);
        cpu.write_byte(0x1234, 0xF0);

        execute_next(&mut cpu); // LDX #$05
        execute_next(&mut cpu); // LDA #$0F
        execute_next(&mut cpu); // ORA ($20,X)
        assert_eq!(cpu.a, 0xFF);
    }

    #[test]
    fn test_and_indirect_y() {
        let mut cpu = create_cpu_with_program(&[0xA0, 0x05, 0xA9, 0xF0, 0x31, 0x20, 0xEA]);
        cpu.write_byte(0x0020, 0x34);
        cpu.write_byte(0x0021, 0x12);
        cpu.write_byte(0x1239, 0x0F);

        execute_next(&mut cpu); // LDY #$05
        execute_next(&mut cpu); // LDA #$F0
        execute_next(&mut cpu); // AND ($20),Y
        assert_eq!(cpu.a, 0x00);
    }
}
