use crate::{SystemClock, cpu::microcode::opcode, mcu::Mcu};
use arraydeque::ArrayDeque;
use microcode::{InterruptSequences, Microcode};

mod microcode;
mod reg16;

use self::reg16::Register16;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Register {
    A,
    X,
    Y,
}

#[derive(Default, Debug)]
struct IrqDetector {
    irq_pending: bool,
    irq_input: bool,
    irq_line_changed_at: Option<SystemClock>,
}

impl IrqDetector {
    fn update_irq_input(&mut self, v: bool, clock: SystemClock) {
        if self.irq_input != v {
            self.irq_line_changed_at = Some(clock);
        }
        self.irq_input = v;
    }

    fn detect_irq(&mut self, interrupt_disabled: bool) {
        self.irq_pending = !interrupt_disabled && self.irq_input;
    }

    fn irq_pending(&self) -> bool {
        self.irq_pending
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum NmiState {
    #[default]
    Idle,
    NmiPending,
    InNmi,
}

#[derive(Default, Debug)]
struct NmiDetector {
    state: NmiState,
    last_nmi_input: bool,
    nmi_input: bool,
    nmi_line_changed_at: Option<SystemClock>,
}

impl NmiDetector {
    fn update_nmi_input(&mut self, v: bool, clock: SystemClock) {
        if self.nmi_input != v {
            self.nmi_line_changed_at = Some(clock);
        }
        self.nmi_input = v;
    }

    fn detect_nmi(&mut self) -> bool {
        let rising_edge = self.last_nmi_input != self.nmi_input && self.nmi_input;
        self.last_nmi_input = self.nmi_input;
        if rising_edge && self.state == NmiState::Idle {
            self.state = NmiState::NmiPending;
            true
        } else {
            false
        }
    }

    fn take_nmi_pending(&mut self) -> bool {
        if self.state == NmiState::NmiPending {
            self.state = NmiState::InNmi;
            true
        } else {
            false
        }
    }

    fn leave_nmi(&mut self) {
        self.state = NmiState::Idle;
    }
}

enum InterruptType {
    Nmi,
    Irq,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ValueSource {
    Immediate,
    /// Read mem from zero page
    ZeroPage,
    /// Read mem for other cases
    Mem,
}

trait ValueSourceTrait {
    fn value<M: Mcu>(cpu: &mut Cpu<M>) -> u8;
}

struct Immediate;
struct Alu;
struct ZeroPage;
struct Mem;

impl ValueSourceTrait for Immediate {
    fn value<M: Mcu>(cpu: &mut Cpu<M>) -> u8 {
        cpu.inc_read_byte()
    }
}

impl ValueSourceTrait for Alu {
    fn value<M: Mcu>(cpu: &mut Cpu<M>) -> u8 {
        cpu.alu
    }
}

impl ValueSourceTrait for ZeroPage {
    fn value<M: Mcu>(cpu: &mut Cpu<M>) -> u8 {
        cpu.last_read_addr = Some(cpu.ab.get());
        cpu.mcu.read_zero_page(cpu.ab.low())
    }
}

impl ValueSourceTrait for Mem {
    fn value<M: Mcu>(cpu: &mut Cpu<M>) -> u8 {
        cpu.last_read_addr = Some(cpu.ab.get());
        cpu.mcu.read(cpu.ab.get())
    }
}

trait ValueTargetTrait {
    fn write<M: Mcu>(cpu: &mut Cpu<M>, value: u8);
}

impl ValueTargetTrait for ZeroPage {
    fn write<M: Mcu>(cpu: &mut Cpu<M>, value: u8) {
        cpu.mcu.write_zero_page(cpu.ab.low(), value);
    }
}

impl ValueTargetTrait for Mem {
    fn write<M: Mcu>(cpu: &mut Cpu<M>, value: u8) {
        cpu.mcu.write(cpu.ab.get(), value);
    }
}

pub struct Cpu<M: Mcu> {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pc: Register16,
    pub sp: u8,
    pub status: u8,
    last_status: u8,

    opcode: u8,
    /// address bus, which memory byte that cpu current select
    ab: Register16,
    /// data bus, what byte that cpu will save or get from memory bus
    db: u8, // save low byte during indexed addressing
    alu: u8,

    entering_interrupt: bool,
    nmi_detecteor: NmiDetector,
    interrupt_detected: Option<InterruptType>,
    irq_detector: IrqDetector,
    pub(crate) last_read_addr: Option<u16>,

    track_interrupt: bool,
    pub(crate) frozen: bool,
    halt: bool,

    microcode_queue: ArrayDeque<Microcode, 8>,
    mcu: M,
}

impl<M: Mcu> Cpu<M> {
    pub fn new(mcu: M) -> Cpu<M> {
        let mut r = Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: Register16::default(),
            sp: 0,
            status: 0,
            last_status: 0,
            mcu,
            opcode: 0,
            entering_interrupt: false,
            interrupt_detected: None,
            nmi_detecteor: Default::default(),
            irq_detector: Default::default(),
            ab: Register16::default(),
            db: 0,
            alu: 0,
            microcode_queue: ArrayDeque::new(),
            halt: false,
            track_interrupt: std::env::var("NES_INTERRUPT_TRACK").is_ok(),
            frozen: false,
            last_read_addr: None,
        };
        r.reset();
        r
    }

    pub fn mcu(&self) -> &M {
        &self.mcu
    }

    pub fn mcu_mut(&mut self) -> &mut M {
        &mut self.mcu
    }

    /// Set CPU program counter. Panics if there are pending microcodes;
    /// callers must drain the queue before calling set_pc.
    pub fn set_pc(&mut self, pc: u16) {
        assert!(
            self.microcodes_empty(),
            "microcode queue must be empty before setting PC"
        );
        self.pc.set(pc);
    }

    pub fn pc(&self) -> u16 {
        self.pc.get()
    }

    pub fn microcodes_empty(&self) -> bool {
        self.microcode_queue.is_empty()
    }

    fn next_microcode(&self) -> Microcode {
        self.microcode_queue
            .front()
            .copied()
            .unwrap_or(Microcode::FetchAndDecode)
    }

    /// Return true if cpu can be paused for execute dma, return true if next microcode
    /// not write operation
    pub(crate) fn can_pause(&self) -> bool {
        !self.next_microcode().is_write_operation()
    }

    pub fn reset(&mut self) {
        self.set_flag(Flag::InterruptDisabled, true);
        self.set_flag(Flag::NotUsed, true);
        self.entering_interrupt = false;
        self.microcode_queue.clear();
        self.interrupt_detected = None;
        self.halt = false;
        self.sp = self.sp.wrapping_sub(3);
        self.nmi_detecteor = Default::default();
        self.irq_detector = Default::default();
        self.frozen = false;
        self.last_read_addr = None;
        self.last_status = self.status;

        self.push_microcodes(&InterruptSequences::RESET);
    }

    pub fn set_irq(&mut self, enabled: bool, clock: SystemClock) {
        self.irq_detector.update_irq_input(enabled, clock);
    }

    pub fn is_halted(&self) -> bool {
        self.halt
    }

    /// Return true if just execute current instruction
    pub fn tick<P: Plugin<M>>(
        &mut self,
        plugin: &mut P,
        clock: SystemClock,
    ) -> (ExecuteResult, bool) {
        self.last_read_addr = None;

        if self.frozen {
            if self.track_interrupt {
                println!("[{}] (frozen)", clock.cycles());
            }
            return (ExecuteResult::Continue, false);
        }

        if self.is_halted() {
            if self.track_interrupt {
                println!("[{}] (halted)", clock.cycles());
            }
            return (ExecuteResult::Halt, false);
        }

        let code = match self.pop_microcode() {
            Some(v) => v,
            None => {
                plugin.start(self, clock);
                match self.interrupt_detected.take() {
                    Some(InterruptType::Nmi) => self.push_enter_interrupt_microcodes(true),
                    Some(InterruptType::Irq) => self.push_enter_interrupt_microcodes(false),
                    None => Microcode::FetchAndDecode,
                }
            }
        };

        self.last_status = self.status;
        code.exec(self);
        self.detect_interrupt(clock);

        if self.track_interrupt {
            self.dump_interrupt_track(clock);
        }

        if self.microcode_queue.is_empty() {
            plugin.end(self, clock);
            (plugin.should_stop(), true)
        } else {
            (ExecuteResult::Continue, false)
        }
    }

    fn detect_interrupt(&mut self, clock: SystemClock) {
        self.nmi_detecteor.detect_nmi();
        let disabled = if matches!(self.opcode, opcode::CLI | opcode::SEI | opcode::PLP) {
            (self.last_status & Flag::InterruptDisabled as u8) != 0
        } else {
            self.flag(Flag::InterruptDisabled)
        };
        self.irq_detector.detect_irq(disabled);
        // Detect interrupt at the second-to-last cycle
        if self.microcode_queue.len() == 1 && InterruptSequences::is_end(self.microcode_queue[0]) {
            self.entering_interrupt = true;
        }
        // Detect interrupt at the second-to-last cycle
        if self.microcode_queue.len() == 0 && !std::mem::take(&mut self.entering_interrupt) {
            if self
                .nmi_detecteor
                .nmi_line_changed_at
                .is_some_and(|v| (clock.0 - v.0) > 1)
                && self.nmi_detecteor.take_nmi_pending()
            {
                if self.track_interrupt {
                    println!("${:x}, carry flag: {}", self.status, self.flag(Flag::Carry));
                }
                self.interrupt_detected = Some(InterruptType::Nmi);
            } else if self.irq_detector.irq_pending() {
                self.interrupt_detected = Some(InterruptType::Irq);
            } else {
                assert!(self.interrupt_detected.is_none());
            }
        }
    }

    pub fn flag(&self, flag: Flag) -> bool {
        (self.status & flag as u8) != 0
    }

    fn set_flag(&mut self, flag: Flag, v: bool) {
        let mask = flag as u8;
        self.status = (self.status & !mask) | (if v { mask } else { 0 });
    }

    fn update_negative_flag(&mut self, value: u8) {
        self.set_flag(Flag::Negative, value & 0x80 != 0);
    }

    fn update_zero_flag(&mut self, value: u8) {
        self.set_flag(Flag::Zero, value == 0);
    }

    pub(crate) fn read_byte_for_dma(&mut self, addr: u16) -> u8 {
        self.mcu.read(addr)
    }

    fn read_byte(&mut self, addr: u16) -> u8 {
        self.last_read_addr = Some(addr);
        self.mcu.read(addr)
    }

    pub fn peek_byte(&self, addr: u16) -> u8 {
        self.mcu.peek(addr)
    }

    fn read_pc_byte(&mut self) {
        self.read_byte(self.pc.get());
    }

    fn inc_read_byte(&mut self) -> u8 {
        let addr = self.pc.get();
        self.pc.wrapping_add(1);
        self.read_byte(addr)
    }

    fn write_byte(&mut self, value: u8) {
        self.write_mem(self.ab.get(), value);
    }

    fn write_mem(&mut self, addr: u16, value: u8) {
        self.mcu.write(addr, value);
    }

    fn push_stack(&mut self, value: u8) {
        self.mcu.write_stack_page(self.sp, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn pop_stack(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.mcu.read_stack_page(self.sp)
    }

    #[cfg(test)]
    fn peek_stack(&mut self) -> u8 {
        let addr = 0x100 + self.sp.wrapping_add(1) as u16;
        self.mcu.peek(addr)
    }

    fn halt(&mut self) {
        self.halt = true;
    }

    fn pch(&self) -> u8 {
        self.pc.high()
    }

    fn abh(&self) -> u8 {
        self.ab.high()
    }

    fn abl(&self) -> u8 {
        self.ab.low()
    }

    fn set_abh(&mut self, v: u8) {
        self.ab.set_high(v);
    }

    fn set_abl(&mut self, v: u8) {
        self.ab.set_low(v);
    }

    fn load_alu(&mut self) {
        self.alu = self.read_byte(self.ab.get());
    }

    fn do_adc(&mut self, val: u8) {
        let carry = self.flag(Flag::Carry) as u16;
        let sum = (self.a as u16) + (val as u16) + carry;
        self.set_flag(Flag::Carry, (sum >> 8) != 0);
        let result = sum as u8;
        self.set_flag(
            Flag::Overflow,
            (self.a ^ result) & (val ^ result) & 0x80 != 0,
        );
        self.set_register(Register::A, result);
    }

    fn adc<S: ValueSourceTrait>(&mut self) {
        let val = S::value(self);
        self.do_adc(val);
    }

    fn sbc<S: ValueSourceTrait>(&mut self) {
        let val = S::value(self);
        let val = val ^ 0xFF;
        self.do_adc(val);
    }

    fn ora<S: ValueSourceTrait>(&mut self) {
        let val = S::value(self);
        self.set_register(Register::A, self.a | val);
    }

    fn eor<S: ValueSourceTrait>(&mut self) {
        let val = S::value(self);
        self.set_register(Register::A, self.a ^ val);
    }

    fn cmp<S: ValueSourceTrait>(&mut self) {
        let val = S::value(self);
        let t = self.a.wrapping_sub(val);
        self.update_zero_negative_flags(t);
        self.set_flag(Flag::Carry, self.a >= val);
    }

    fn cpx<S: ValueSourceTrait>(&mut self) {
        let val = S::value(self);
        let t = self.x.wrapping_sub(val);
        self.update_zero_negative_flags(t);
        self.set_flag(Flag::Carry, self.x >= val);
    }

    fn cpy<S: ValueSourceTrait>(&mut self) {
        let val = S::value(self);
        let t = self.y.wrapping_sub(val);
        self.update_zero_negative_flags(t);
        self.set_flag(Flag::Carry, self.y >= val);
    }

    fn alr(&mut self) {
        let a_and_alu = self.a & self.alu;
        self.set_register(Register::A, a_and_alu >> 1);
        self.set_flag(Flag::Carry, a_and_alu & 0x01 != 0);
    }

    fn anc(&mut self) {
        self.set_register(Register::A, self.a & self.alu);
        self.set_flag(Flag::Carry, self.a & 0x80 != 0);
    }

    fn arr(&mut self) {
        self.a &= self.alu;
        let val = (self.a >> 1) | ((self.flag(Flag::Carry) as u8) << 7);
        self.set_register(Register::A, val);
        self.set_flag(Flag::Carry, self.a & 0x40 != 0);
        self.set_flag(Flag::Overflow, ((self.a >> 6) ^ (self.a >> 5)) & 1 != 0);
    }

    fn axs(&mut self) {
        let v = self.a & self.x;
        let (x, borrow) = v.overflowing_sub(self.alu);
        self.set_register(Register::X, x);
        self.set_flag(Flag::Carry, !borrow);
    }

    fn lax(&mut self) {
        self.set_register(Register::A, self.alu);
        self.set_register(Register::X, self.alu);
    }

    fn sax(&mut self) {
        self.write_byte(self.a & self.x);
    }

    fn dcp(&mut self) {
        let v = self.alu.wrapping_sub(1);
        self.write_byte(v);
        self.update_zero_negative_flags(self.a.wrapping_sub(v));
        self.set_flag(Flag::Carry, self.a >= v);
    }

    fn isc(&mut self) {
        let v = self.alu.wrapping_add(1);
        self.write_byte(v);
        self.alu = v;
        self.sbc::<Alu>();
    }

    fn rra(&mut self) {
        let carry = self.alu & 0x01 != 0;
        self.alu = (self.alu >> 1) | ((self.flag(Flag::Carry) as u8) << 7);
        self.write_byte(self.alu);
        self.set_flag(Flag::Carry, carry);
        self.adc::<Alu>();
    }

    fn rla(&mut self) {
        let new = (self.alu << 1) | (self.flag(Flag::Carry) as u8);
        self.set_flag(Flag::Carry, self.alu & 0x80 != 0);
        self.alu = new;
        self.write_byte(self.alu);
        self.and::<Alu>();
    }

    fn slo(&mut self) {
        self.set_flag(Flag::Carry, self.alu & 0x80 != 0);
        self.alu <<= 1;
        self.write_byte(self.alu);
        self.ora::<Alu>();
    }

    fn sre(&mut self) {
        self.set_flag(Flag::Carry, self.alu & 0x01 != 0);
        self.alu >>= 1;
        self.write_byte(self.alu);
        self.eor::<Alu>();
    }

    fn shx(&mut self) {
        let v = self.x & self.abh().wrapping_add(1);
        self.ab.set_high(v);
        self.write_byte(v);
    }

    fn shy(&mut self) {
        let v = self.y & self.abh().wrapping_add(1);
        self.ab.set_high(v);
        self.write_byte(v);
    }

    fn sha(&mut self) {
        // SHA (AHX/AXA): store A & X & (high-byte of addr + 1) at address
        let out = self.a & self.x & self.abh().wrapping_add(1);
        self.ab.set_high(out);
        self.write_byte(out);
    }

    fn tas(&mut self) {
        let v = self.a & self.x;
        self.sp = v;
        let out = v & self.abh().wrapping_add(1);
        self.ab.set_high(out);
        self.write_byte(out);
    }

    fn push_status(&mut self, break_flag: bool, check_nmi: bool) {
        if check_nmi {
            if self.nmi_detecteor.take_nmi_pending() {
                if self.track_interrupt {
                    println!(
                        "hijack: ${:x}, carry flag: {}",
                        self.status,
                        self.flag(Flag::Carry)
                    );
                }
                self.push_status(break_flag, false);
                self.microcode_queue.clear();
                self.push_microcodes(&[Microcode::LoadNmiPcL, Microcode::LoadNmiPcH]);
                return;
            }
        }

        self.push_stack(if break_flag {
            self.status | Flag::Break as u8
        } else {
            self.status
        });
    }

    fn plp(&mut self) {
        let break_flag = self.flag(Flag::Break);
        self.status = self.pop_stack();
        self.set_flag(Flag::Break, break_flag);
        self.set_flag(Flag::NotUsed, true);
    }

    fn set_pc_to_ab(&mut self) {
        self.pc.set(self.ab.get());
    }

    fn and<S: ValueSourceTrait>(&mut self) {
        let v = S::value(self);
        self.set_register(Register::A, self.a & v);
    }

    fn bit<S: ValueSourceTrait>(&mut self) {
        let v = S::value(self);
        self.set_flag(Flag::Overflow, v & 0x40 != 0);
        self.update_negative_flag(v);
        self.update_zero_flag(self.a & v);
    }

    fn push_microcodes(&mut self, microcodes: &[Microcode]) {
        self.microcode_queue.extend_back(microcodes.iter().copied());
    }

    /// And return the first microcode
    fn push_enter_interrupt_microcodes(&mut self, nmi: bool) -> Microcode {
        // if nmi {
        //     eprintln!("enter nmi");
        // }
        self.push_microcodes(if nmi {
            &InterruptSequences::NMI
        } else {
            &InterruptSequences::IRQ
        });
        Microcode::FetchOnly
    }

    fn pop_microcode(&mut self) -> Option<Microcode> {
        self.microcode_queue.pop_front()
    }

    fn push_microcode(&mut self, microcode: Microcode) {
        match self.microcode_queue.push_front(microcode) {
            Ok(_) => (),
            Err(_) => debug_assert!(
                false,
                "Microcode queue overflow, maybe some microcode is too long?"
            ),
        }
    }

    fn retain_cycle(&mut self) {
        self.push_microcode(Microcode::Nop);
    }

    /// Update cpu nmi signal line, may trigger nmi
    pub fn update_nmi_line(&mut self, nmi: bool, clock: SystemClock) {
        self.nmi_detecteor.update_nmi_input(nmi, clock);
    }

    fn load_nmi_pcl(&mut self) {
        self.set_flag(Flag::InterruptDisabled, true);
        let low = self.read_byte(0xFFFA);
        self.pc.set_low(low);
    }

    fn load_nmi_pch(&mut self) {
        let high = self.read_byte(0xFFFB);
        self.pc.set_high(high);
    }

    fn load_irq_pcl(&mut self) {
        self.set_flag(Flag::InterruptDisabled, true);
        let low = self.read_byte(0xFFFE);
        self.pc.set_low(low);
    }

    fn load_irq_pch(&mut self) {
        let high = self.read_byte(0xFFFF);
        self.pc.set_high(high);
    }

    fn set_register(&mut self, register: Register, val: u8) {
        match register {
            Register::A => self.a = val,
            Register::X => self.x = val,
            Register::Y => self.y = val,
        }
        self.update_zero_negative_flags(val);
    }

    fn update_zero_negative_flags(&mut self, val: u8) {
        self.update_zero_flag(val);
        self.update_negative_flag(val);
    }

    fn dump_interrupt_track(&self, clock: SystemClock) {
        let irq_str = match self.irq_detector.irq_line_changed_at {
            Some(t) => format!("{}@{}", self.irq_detector.irq_input as u8, t.cycles()),
            None => format!("{}", self.irq_detector.irq_input as u8),
        };
        let nmi_str = match self.nmi_detecteor.nmi_line_changed_at {
            Some(t) => format!("{}@{}", self.nmi_detecteor.nmi_input as u8, t.cycles()),
            None => format!("{}", self.nmi_detecteor.nmi_input as u8),
        };
        let int_str = match self.interrupt_detected {
            Some(InterruptType::Nmi) => "NMI",
            Some(InterruptType::Irq) => "IRQ",
            None => "none",
        };
        let next = self.next_microcode().to_string();
        let opcode_mnemonic = OPCODE_MNEMONICS[self.opcode as usize];
        println!(
            "[{}] irq={} nmi={} nmi_st={:?} i={} op=${:02X}/{} q={} next={} int={}",
            clock.cycles(),
            irq_str,
            nmi_str,
            self.nmi_detecteor.state,
            self.flag(Flag::InterruptDisabled) as u8,
            self.opcode,
            opcode_mnemonic,
            self.microcode_queue.len(),
            next,
            int_str,
        );
    }

    fn indexed_h(&mut self) {
        self.ab.wrapping_inc_low();
        let high = self.read_byte(self.ab.get());
        self.ab.set(self.db as u16 | ((high as u16) << 8));
    }
}

const OPCODE_MNEMONICS: [&str; 256] = {
    let mut m = ["???"; 256];
    m[0] = "BRK";
    m[1] = "ORA";
    m[2] = "KIL";
    m[3] = "SLO";
    m[4] = "NOP";
    m[5] = "ORA";
    m[6] = "ASL";
    m[7] = "SLO";
    m[8] = "PHP";
    m[9] = "ORA";
    m[10] = "ASL";
    m[11] = "ANC";
    m[12] = "NOP";
    m[13] = "ORA";
    m[14] = "ASL";
    m[15] = "SLO";
    m[16] = "BPL";
    m[17] = "ORA";
    m[18] = "KIL";
    m[19] = "SLO";
    m[20] = "NOP";
    m[21] = "ORA";
    m[22] = "ASL";
    m[23] = "SLO";
    m[24] = "CLC";
    m[25] = "ORA";
    m[26] = "NOP";
    m[27] = "SLO";
    m[28] = "NOP";
    m[29] = "ORA";
    m[30] = "ASL";
    m[31] = "SLO";
    m[32] = "JSR";
    m[33] = "AND";
    m[34] = "KIL";
    m[35] = "RLA";
    m[36] = "BIT";
    m[37] = "AND";
    m[38] = "ROL";
    m[39] = "RLA";
    m[40] = "PLP";
    m[41] = "AND";
    m[42] = "ROL";
    m[43] = "ANC";
    m[44] = "BIT";
    m[45] = "AND";
    m[46] = "ROL";
    m[47] = "RLA";
    m[48] = "BMI";
    m[49] = "AND";
    m[50] = "KIL";
    m[51] = "RLA";
    m[52] = "NOP";
    m[53] = "AND";
    m[54] = "ROL";
    m[55] = "RLA";
    m[56] = "SEC";
    m[57] = "AND";
    m[58] = "NOP";
    m[59] = "RLA";
    m[60] = "NOP";
    m[61] = "AND";
    m[62] = "ROL";
    m[63] = "RLA";
    m[64] = "RTI";
    m[65] = "EOR";
    m[66] = "KIL";
    m[67] = "SRE";
    m[68] = "NOP";
    m[69] = "EOR";
    m[70] = "LSR";
    m[71] = "SRE";
    m[72] = "PHA";
    m[73] = "EOR";
    m[74] = "LSR";
    m[75] = "ALR";
    m[76] = "JMP";
    m[77] = "EOR";
    m[78] = "LSR";
    m[79] = "SRE";
    m[80] = "BVC";
    m[81] = "EOR";
    m[82] = "KIL";
    m[83] = "SRE";
    m[84] = "NOP";
    m[85] = "EOR";
    m[86] = "LSR";
    m[87] = "SRE";
    m[88] = "CLI";
    m[89] = "EOR";
    m[90] = "NOP";
    m[91] = "SRE";
    m[92] = "NOP";
    m[93] = "EOR";
    m[94] = "LSR";
    m[95] = "SRE";
    m[96] = "RTS";
    m[97] = "ADC";
    m[98] = "KIL";
    m[99] = "RRA";
    m[100] = "NOP";
    m[101] = "ADC";
    m[102] = "ROR";
    m[103] = "RRA";
    m[104] = "PLA";
    m[105] = "ADC";
    m[106] = "ROR";
    m[107] = "ARR";
    m[108] = "JMP";
    m[109] = "ADC";
    m[110] = "ROR";
    m[111] = "RRA";
    m[112] = "BVS";
    m[113] = "ADC";
    m[114] = "KIL";
    m[115] = "RRA";
    m[116] = "NOP";
    m[117] = "ADC";
    m[118] = "ROR";
    m[119] = "RRA";
    m[120] = "SEI";
    m[121] = "ADC";
    m[122] = "NOP";
    m[123] = "RRA";
    m[124] = "NOP";
    m[125] = "ADC";
    m[126] = "ROR";
    m[127] = "RRA";
    m[128] = "NOP";
    m[129] = "STA";
    m[130] = "NOP";
    m[131] = "SAX";
    m[132] = "STY";
    m[133] = "STA";
    m[134] = "STX";
    m[135] = "SAX";
    m[136] = "DEY";
    m[137] = "NOP";
    m[138] = "TXA";
    m[139] = "ANE";
    m[140] = "STY";
    m[141] = "STA";
    m[142] = "STX";
    m[143] = "SAX";
    m[144] = "BCC";
    m[145] = "STA";
    m[146] = "KIL";
    m[147] = "SHA";
    m[148] = "STY";
    m[149] = "STA";
    m[150] = "STX";
    m[151] = "SAX";
    m[152] = "TYA";
    m[153] = "STA";
    m[154] = "TXS";
    m[155] = "TAS";
    m[156] = "SHY";
    m[157] = "STA";
    m[158] = "SHX";
    m[159] = "SHA";
    m[160] = "LDY";
    m[161] = "LDA";
    m[162] = "LDX";
    m[163] = "LAX";
    m[164] = "LDY";
    m[165] = "LDA";
    m[166] = "LDX";
    m[167] = "LAX";
    m[168] = "TAY";
    m[169] = "LDA";
    m[170] = "TAX";
    m[171] = "LAX";
    m[172] = "LDY";
    m[173] = "LDA";
    m[174] = "LDX";
    m[175] = "LAX";
    m[176] = "BCS";
    m[177] = "LDA";
    m[178] = "KIL";
    m[179] = "LAX";
    m[180] = "LDY";
    m[181] = "LDA";
    m[182] = "LDX";
    m[183] = "LAX";
    m[184] = "CLV";
    m[185] = "LDA";
    m[186] = "TSX";
    m[187] = "LAS";
    m[188] = "LDY";
    m[189] = "LDA";
    m[190] = "LDX";
    m[191] = "LAX";
    m[192] = "CPY";
    m[193] = "CMP";
    m[194] = "NOP";
    m[195] = "DCP";
    m[196] = "CPY";
    m[197] = "CMP";
    m[198] = "DEC";
    m[199] = "DCP";
    m[200] = "INY";
    m[201] = "CMP";
    m[202] = "DEX";
    m[203] = "AXS";
    m[204] = "CPY";
    m[205] = "CMP";
    m[206] = "DEC";
    m[207] = "DCP";
    m[208] = "BNE";
    m[209] = "CMP";
    m[210] = "KIL";
    m[211] = "DCP";
    m[212] = "NOP";
    m[213] = "CMP";
    m[214] = "DEC";
    m[215] = "DCP";
    m[216] = "CLD";
    m[217] = "CMP";
    m[218] = "NOP";
    m[219] = "DCP";
    m[220] = "NOP";
    m[221] = "CMP";
    m[222] = "DEC";
    m[223] = "DCP";
    m[224] = "CPX";
    m[225] = "SBC";
    m[226] = "NOP";
    m[227] = "ISC";
    m[228] = "CPX";
    m[229] = "SBC";
    m[230] = "INC";
    m[231] = "ISC";
    m[232] = "INX";
    m[233] = "SBC";
    m[234] = "NOP";
    m[235] = "SBC";
    m[236] = "CPX";
    m[237] = "SBC";
    m[238] = "INC";
    m[239] = "ISC";
    m[240] = "BEQ";
    m[241] = "SBC";
    m[242] = "KIL";
    m[243] = "ISC";
    m[244] = "NOP";
    m[245] = "SBC";
    m[246] = "INC";
    m[247] = "ISC";
    m[248] = "SED";
    m[249] = "SBC";
    m[250] = "NOP";
    m[251] = "ISC";
    m[252] = "NOP";
    m[253] = "SBC";
    m[254] = "INC";
    m[255] = "ISC";
    m
};
#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum ExecuteResult {
    Continue,
    Stop(u8),
    ShouldReset,
    /// Cpu halted because of executed an invalid instruction, only reset can recover
    Halt,
}

pub trait Plugin<M: Mcu> {
    /// Before start execute new instruction
    fn start(&mut self, cpu: &Cpu<M>, system_clock: SystemClock);

    /// After execute instruction
    fn end(&mut self, cpu: &Cpu<M>, system_clock: SystemClock);

    /// After execute an instruction, tell cpu should stop execution or not
    fn should_stop(&self) -> ExecuteResult {
        ExecuteResult::Continue
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
    fn start(&mut self, _: &Cpu<M>, _: SystemClock) {}

    fn end(&mut self, _: &Cpu<M>, _: SystemClock) {}
}

impl<M: Mcu> Default for EmptyPlugin<M> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Flag {
    Carry = 0x01u8,
    Zero = 0x02u8,
    InterruptDisabled = 0x04u8,
    Decimal = 0x08u8,
    Break = 0x10u8,
    NotUsed = 0x20u8,
    Overflow = 0x40u8,
    Negative = 0x80u8,
}

#[cfg(test)]
mod tests;
