use self::microcode::{opcode, Microcode};
use crate::mcu::Mcu;
use arraydeque::ArrayDeque;
#[cfg(debug_assertions)]
use std::{cell::Cell, rc::Rc};

mod microcode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    A,
    X,
    Y,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CpuMode {
    Normal,
    /// Set when cpu detected nmi_requested, reset to Normal if nmi handle complete
    Nmi,
    /// Set when hit some illegal/undocument hlt instruction, reset to normal if call `reset()`
    Halt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct OamDmaState {
    startup_cycles: usize,
    remaining_cpu_cycles: usize,
    remaining_transfer_cpu_cycles: usize,
    irq_was_pending: bool,
    dma_started_at: usize,
}

impl OamDmaState {
    fn dmc_overlap_penalty(&self) -> usize {
        match self.remaining_transfer_cpu_cycles {
            3 => 1,
            1 => 3,
            _ => 2,
        }
    }
}

/// DMC DMA state machine.
///
/// On real hardware the DMC DMA steals 3–4 CPU cycles depending on
/// bus alignment.  Each phase is a pure CPU stall cycle.  During the
/// DmaRead phase the sample byte is fetched and any side-effecting
/// "phantom reads" ($4016/$4017, $2007) are modelled using the CPU's
/// microcode queue and address bus value (`cpu.ab`).
///
/// During OAM DMA the stall phases are absorbed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DmcDmaPhase {
    /// First stall cycle (CPU halted).
    Halt,
    /// Bus alignment stall.
    Align,
    /// Dummy read stall.
    Dummy,
    /// DMA read cycle — sample byte is fetched here.
    DmaRead,
}

pub struct Cpu<M: Mcu> {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub status: u8,
    cycles: usize,
    mcu: M,

    opcode: u8,
    address_latch: u16,
    indexed_address_latch: u8, // save low byte during indexed addressing
    alu: u8,

    /// cpu irq line, true means irq is requested, map to Low level of cpu irq pin
    irq_line: bool,
    irq_requested_at: Option<usize>,
    irq_inhibit: Option<bool>,
    nmi_line: bool,
    /// cycles that nmi_requested, in real 6502 cpu, cpu check nmi at the last cycle of instruction,
    /// if set nmi line of ppu, such as STA $2000, nmi line set after last cycle of STA instruction, then
    /// cpu can not know nmi signal until the next instruction, we save the cycle that nmi requested to detect this
    nmi_requested_at: Option<usize>,
    defer_nmi_poll: bool,
    /// A taken non-page-crossing branch suppresses IRQ on its last
    /// cycle.  This flag is set when such a branch completes, causing
    /// the next instruction-boundary IRQ check to be skipped.
    branch_irq_defer: bool,
    allow_late_irq_nmi_hijack: bool,
    /// Set to the DMA completion cycle when DMA ends without IRQ pending.
    /// While set, apply penultimate-cycle IRQ sampling: the IRQ must have been
    /// asserted for at least 1 CPU cycle (3 PPU ticks) before being acted on.
    /// Cleared when the IRQ is taken or the window expires.
    post_dma_irq_defer: Option<usize>,
    oam_dma_pending: Option<usize>,
    oam_dma: Option<OamDmaState>,
    /// DMC DMA state machine – `None` when no DMC DMA is in progress.
    dmc_dma: Option<DmcDmaPhase>,
    mode: CpuMode,

    microcode_queue: ArrayDeque<Microcode, 8>,
    irq_hijacked: bool,

    #[cfg(debug_assertions)]
    mem_acc_count: Rc<Cell<usize>>,
}

impl<M: Mcu> Cpu<M> {
    pub fn new(mcu: M) -> Cpu<M> {
        let mut r = Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0,
            status: 0,
            cycles: 0,
            mcu,
            opcode: 0,
            address_latch: 0,
            indexed_address_latch: 0,
            alu: 0,
            irq_line: false,
            irq_requested_at: None,
            irq_inhibit: None,
            nmi_line: false,
            microcode_queue: ArrayDeque::new(),
            mode: CpuMode::Normal,
            nmi_requested_at: None,
            defer_nmi_poll: false,
            branch_irq_defer: false,
            allow_late_irq_nmi_hijack: false,
            post_dma_irq_defer: None,
            oam_dma_pending: None,
            oam_dma: None,
            dmc_dma: None,
            irq_hijacked: false,
            #[cfg(debug_assertions)]
            mem_acc_count: Default::default(),
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

    /// Drain all pending microcodes by ticking the CPU while the microcode queue
    /// is non-empty. The provided `plugin` will be used for tick hooks.
    pub fn drain_microcodes<P: Plugin<M>>(&mut self, plugin: &mut P) {
        while !self.microcode_queue.is_empty() {
            // Ignore execute result; we're only interested in running queued microcodes
            let _ = self.tick(plugin);
        }
    }

    /// Set CPU program counter, draining any pending microcodes (e.g. reset vector loads)
    /// before applying the new PC. This ensures external callers can set PC without
    /// being later overwritten by queued microcode operations.
    pub fn set_pc(&mut self, pc: u16) {
        let mut drain = EmptyPlugin::<M>::new();
        self.drain_microcodes(&mut drain);
        self.pc = pc;
    }

    pub fn reset(&mut self) {
        self.inner_set_flag(Flag::InterruptDisabled, true);
        self.inner_set_flag(Flag::NotUsed, true);
        self.irq_line = false;
        self.irq_requested_at = None;
        self.irq_inhibit = None;
        self.microcode_queue.clear();
        self.nmi_requested_at = None;
        self.defer_nmi_poll = false;
        self.branch_irq_defer = false;
        self.allow_late_irq_nmi_hijack = false;
        self.post_dma_irq_defer = None;
        self.oam_dma_pending = None;
        self.oam_dma = None;
        self.dmc_dma = None;
        self.mode = CpuMode::Normal;
        self.sp = self.sp.wrapping_sub(3);

        // Reset process takes 7 cycles, push 7 Nop microcodes to ppu/apu run as a real device, and make Plugin to get correct total cycles
        self.push_microcodes(&[
            Microcode::Nop,
            Microcode::Nop,
            Microcode::Nop,
            Microcode::Nop,
            Microcode::Nop,
            Microcode::LoadResetPcL,
            Microcode::LoadResetPcH,
        ]);
    }

    pub fn total_cycles(&self) -> usize {
        self.cycles
    }

    /// Cpu will enter nmi before exec next instrnuction
    pub fn request_nmi(&mut self) {
        self.nmi_requested_at = Some(self.cycles);
    }

    pub fn set_irq(&mut self, enabled: bool) {
        if enabled && !self.irq_line {
            self.irq_requested_at = Some(self.cycles);
        } else if !enabled {
            self.irq_requested_at = None;
        }
        self.irq_line = enabled;
    }

    fn nmi_ready(&self) -> bool {
        self.nmi_requested_at
            .is_some_and(|cycles| self.cycles > cycles + 5)
    }

    pub fn is_halted(&self) -> bool {
        self.mode == CpuMode::Halt
    }

    pub fn request_oam_dma(&mut self) {
        let cpu_cycle = self.cycles / 3;
        let startup_cycles = if cpu_cycle.is_multiple_of(2) { 1 } else { 2 };
        self.oam_dma_pending = Some(startup_cycles);
    }

    /// Request a DMC DMA stall.
    /// `is_reload`: true for reload DMAs (output unit emptied buffer, 4 cycles),
    ///              false for load DMAs ($4015 write with empty buffer, 3 cycles).
    pub fn request_dmc_dma(&mut self, is_reload: bool) {
        if self.dmc_dma.is_none() {
            if self.oam_dma.is_some() {
                // During OAM DMA, only the eventual DMC read steals the bus.
                // The halt/align/dummy phases are absorbed while waiting for
                // the next GET cycle.
                self.dmc_dma = Some(DmcDmaPhase::DmaRead);
                return;
            }
            if is_reload {
                // Reload DMA: scheduled on PUT cycle -> 4 stall cycles
                // Halt + Align + Dummy + DmaRead
                self.dmc_dma = Some(DmcDmaPhase::Halt);
            } else {
                // Load DMA: scheduled on GET cycle -> 3 stall cycles
                // Halt + Dummy + DmaRead (skip Align)
                self.dmc_dma = Some(DmcDmaPhase::Align);
            }
        }
    }

    fn inc_mem_count(&mut self) {
        #[cfg(debug_assertions)]
        self.mem_acc_count.set(self.mem_acc_count.get() + 1);
    }

    fn reset_mem_count(&mut self) {
        #[cfg(debug_assertions)]
        self.mem_acc_count.set(0);
    }

    /// Return true if just execute current instruction
    pub fn tick<P: Plugin<M>>(&mut self, plugin: &mut P) -> (ExecuteResult, bool) {
        self.reset_mem_count();

        #[cfg(debug_assertions)]
        let _guard = scopeguard::guard(self.mem_acc_count.clone(), |acc_count| {
            assert!(
                acc_count.get() <= 1,
                "Multiple memory accesses in a single tick: {}",
                acc_count.get()
            );
        });

        self.cycles = self.cycles.wrapping_add(1);

        if !self.cycles.is_multiple_of(3) {
            return (ExecuteResult::Continue, false);
        }

        // CPU cycle number (incremented every 3 PPU ticks).
        let cpu_cycle = self.cycles / 3;
        // Get cycle = even CPU cycle (read bus), put cycle = odd (write bus).
        let is_get_cycle = cpu_cycle % 2 == 0;

        // ── Advance DMC DMA state machine ──
        // Each phase is a pure CPU stall cycle (outside OAM DMA).
        // During OAM DMA the phases are absorbed.
        if let Some(phase) = self.dmc_dma {
            let in_oam_dma = self.oam_dma.is_some();
            let oam_absorbs_dma = in_oam_dma || self.oam_dma_pending.is_some();

            match phase {
                DmcDmaPhase::Halt => {
                    self.dmc_dma = Some(DmcDmaPhase::Align);
                    if !oam_absorbs_dma {
                        return (ExecuteResult::Continue, false);
                    }
                }
                DmcDmaPhase::Align => {
                    self.dmc_dma = Some(DmcDmaPhase::Dummy);
                    if !oam_absorbs_dma {
                        return (ExecuteResult::Continue, false);
                    }
                }
                DmcDmaPhase::Dummy => {
                    self.dmc_dma = Some(DmcDmaPhase::DmaRead);
                    if !oam_absorbs_dma {
                        return (ExecuteResult::Continue, false);
                    }
                }
                DmcDmaPhase::DmaRead => {
                    if !oam_absorbs_dma {
                        // Not in OAM DMA: this is a stall cycle for DMC DMA read.
                        self.perform_dmc_dma_on_stall();
                        return (ExecuteResult::Continue, false);
                    }
                    // In OAM DMA: DMC DMA read happens on GET cycle without adding stall cycles.
                    // Perform the read and fall through to OAM DMA processing.
                    if is_get_cycle {
                        if let Some(dma) = &mut self.oam_dma {
                            if std::env::var_os("NES_DMA_DEBUG").is_some() {
                                eprintln!(
                                    "overlap cycle={} startup={} remain={} transfer={}",
                                    cpu_cycle,
                                    dma.startup_cycles,
                                    dma.remaining_cpu_cycles,
                                    dma.remaining_transfer_cpu_cycles
                                );
                            }
                            dma.remaining_cpu_cycles += dma.dmc_overlap_penalty();
                        }
                        self.perform_dmc_dma_on_stall();
                    }
                }
            }
        }

        // ── OAM DMA processing ──
        if let Some(mut dma) = self.oam_dma {
            if dma.startup_cycles > 0 {
                dma.startup_cycles -= 1;
            } else {
                dma.remaining_cpu_cycles -= 1;
                if dma.remaining_transfer_cpu_cycles > 0 {
                    dma.remaining_transfer_cpu_cycles -= 1;
                }
            }

            if dma.startup_cycles == 0 && dma.remaining_cpu_cycles == 0 {
                if !dma.irq_was_pending {
                    // IRQ wasn't pending when DMA started. Record the DMA completion
                    // cycle so post-DMA IRQ boundaries apply penultimate-cycle sampling.
                    self.post_dma_irq_defer = Some(self.cycles);
                    if self
                        .irq_requested_at
                        .is_some_and(|irq_at| irq_at + 3 <= self.cycles)
                    {
                        self.defer_nmi_poll = true;
                    }
                }
                self.oam_dma = None;
            } else {
                self.oam_dma = Some(dma);
            }
            return (ExecuteResult::Continue, false);
        }

        if let Some(startup_cycles) = self.oam_dma_pending.take() {
            self.oam_dma = Some(OamDmaState {
                startup_cycles,
                remaining_cpu_cycles: 512,
                remaining_transfer_cpu_cycles: 512,
                irq_was_pending: self.irq_line,
                dma_started_at: self.cycles,
            });
            return (ExecuteResult::Continue, false);
        }

        if self.is_halted() {
            return (ExecuteResult::Halt, true);
        }

        let code = match self.pop_microcode() {
            Some(v) => v,
            None => {
                plugin.start(self);
                let irq_inhibit = self.irq_inhibit.take();
                let branch_defer = std::mem::take(&mut self.branch_irq_defer);

                if std::mem::take(&mut self.defer_nmi_poll) {
                    Microcode::FetchAndDecode
                } else if self.nmi_ready() {
                    self.nmi_requested_at = None;
                    self.mode = CpuMode::Nmi;
                    self.push_microcodes(&[
                        Microcode::Nop,
                        Microcode::PushPcH,
                        Microcode::PushPcL,
                        Microcode::PushStatus {
                            set_disable_interrupt: true,
                            break_flag: false,
                        },
                        Microcode::LoadNmiPcL,
                        Microcode::LoadNmiPcH,
                    ]);
                    Microcode::Nop
                } else if self.irq_line
                    && !irq_inhibit.unwrap_or_else(|| self.flag(Flag::InterruptDisabled))
                {
                    // A taken non-page-crossing branch ignores IRQ during
                    // its last clock.  Only defer if the IRQ was first
                    // visible during the penalty cycle (last 3 PPU ticks
                    // = last CPU cycle).  If visible earlier, the CPU
                    // sampled it on the penultimate cycle and should
                    // take it normally.
                    let defer_for_branch = branch_defer
                        && self
                            .irq_requested_at
                            .is_some_and(|at| self.cycles.wrapping_sub(at) <= 3);
                    if defer_for_branch {
                        Microcode::FetchAndDecode
                    } else {
                        // After DMA, model the 6502's penultimate-cycle IRQ sampling.
                        let should_defer = self.post_dma_irq_defer.is_some_and(|dma_end| {
                            self.irq_requested_at.is_some_and(|at| {
                                if self.cycles <= dma_end + 3 {
                                    at + 3 > dma_end
                                } else {
                                    at + 3 > self.cycles
                                }
                            })
                        });
                        if should_defer {
                            Microcode::FetchAndDecode
                        } else {
                            self.post_dma_irq_defer = None;
                            self.push_microcodes(&[
                                Microcode::Nop,
                                Microcode::PushPcH,
                                Microcode::PushPcL,
                                Microcode::PushStatus {
                                    set_disable_interrupt: true,
                                    break_flag: false,
                                },
                                Microcode::LoadIrqPcL,
                                Microcode::LoadIrqPcH,
                            ]);
                            self.allow_late_irq_nmi_hijack = true;
                            Microcode::Nop
                        }
                    }
                } else {
                    // No IRQ pending. Expire the post-DMA window after enough
                    // time has passed (a few CPU cycles).
                    if self
                        .post_dma_irq_defer
                        .is_some_and(|dma_end| self.cycles > dma_end + 12)
                    {
                        self.post_dma_irq_defer = None;
                    }
                    Microcode::FetchAndDecode
                }
            }
        };
        code.exec(self);
        if self.microcode_queue.is_empty() {
            self.allow_late_irq_nmi_hijack = false;
            if self.opcode == opcode::RTI {
                self.mode = CpuMode::Normal;
            };
            plugin.end(self);
            (plugin.should_stop(), true)
        } else {
            (ExecuteResult::Continue, false)
        }
    }

    pub fn flag(&self, flag: Flag) -> bool {
        (self.status & flag as u8) != 0
    }

    fn inner_set_flag(&mut self, flag: Flag, v: bool) {
        let mask = flag as u8;
        self.status = (self.status & !mask) | (if v { mask } else { 0 });
    }

    fn save_irq_inhibit(&mut self) {
        if matches!(self.opcode, opcode::CLI | opcode::SEI | opcode::PLP) {
            self.irq_inhibit = Some(self.flag(Flag::InterruptDisabled));
        }
    }

    fn set_flag(&mut self, flag: Flag, v: bool) {
        self.save_irq_inhibit();
        self.inner_set_flag(flag, v);
    }

    fn inc_pc(&mut self, delta: i8) {
        self.pc = self.pc.wrapping_add(delta as u16);
    }

    fn update_negative_flag(&mut self, value: u8) {
        self.inner_set_flag(Flag::Negative, value & 0x80 != 0);
    }

    fn update_zero_flag(&mut self, value: u8) {
        self.inner_set_flag(Flag::Zero, value == 0);
    }

    fn read_byte(&mut self, addr: u16) -> u8 {
        self.inc_mem_count();
        self.mcu.read(addr)
    }

    pub fn peek_byte(&mut self, addr: u16) -> u8 {
        self.mcu.peek(addr)
    }

    fn inc_read_byte(&mut self) -> u8 {
        let addr = self.pc;
        self.inc_pc(1);
        self.inc_mem_count();
        self.mcu.read(addr)
    }

    fn write_byte(&mut self, addr: u16, value: u8) {
        self.inc_mem_count();
        self.mcu.write(addr, value);
    }

    fn push_stack(&mut self, value: u8) {
        self.inc_mem_count();
        self.mcu.write(0x100 + self.sp as u16, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn pop_stack(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        let addr = 0x100 + self.sp as u16;
        self.inc_mem_count();
        self.mcu.read(addr)
    }

    /// Perform the DMC DMA read during a dedicated stall cycle.
    /// Uses the CPU's microcode queue and `self.ab` to detect phantom-read
    /// side effects: on real hardware the CPU's address bus holds the
    /// address of whatever memory operation it was about to perform when
    /// it was halted.
    fn perform_dmc_dma_on_stall(&mut self) {
        if let Some(sample_addr) = self.mcu.take_dmc_dma_address() {
            let phantom_addr = match self.microcode_queue.front() {
                Some(Microcode::LoadR(_) | Microcode::Bit | Microcode::Lax) => self.address_latch,
                _ => self.pc,
            };
            let byte = self.mcu.perform_dmc_dma_read(sample_addr, phantom_addr);
            self.mcu.supply_dmc_dma_byte(byte);
        }
        self.dmc_dma = None;
    }

    pub fn peek_stack(&mut self) -> u8 {
        let addr = 0x100 + self.sp.wrapping_add(1) as u16;
        self.mcu.peek(addr)
    }

    fn halt(&mut self) {
        self.mode = CpuMode::Halt;
    }

    fn pch(&self) -> u8 {
        (self.pc >> 8) as u8
    }

    fn abh(&self) -> u8 {
        (self.address_latch >> 8) as u8
    }

    fn abl(&self) -> u8 {
        (self.address_latch & 0xff) as u8
    }

    fn set_abh(&mut self, v: u8) {
        self.address_latch = (self.address_latch & 0x00ff) | ((v as u16) << 8);
    }

    fn set_abl(&mut self, v: u8) {
        self.address_latch = (self.address_latch & 0xff00) | v as u16;
    }

    fn load_alu(&mut self) {
        self.inc_mem_count();
        self.alu = self.mcu.read(self.address_latch);
    }

    fn adc(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        let val = self.alu;
        let carry = self.flag(Flag::Carry) as u8;
        let (sum, carry0) = self.a.overflowing_add(val);
        let (sum, carry1) = sum.overflowing_add(carry);
        self.inner_set_flag(Flag::Carry, carry0 || carry1);
        self.inner_set_flag(Flag::Overflow, !(self.a ^ val) & (self.a ^ sum) & 0x80 != 0);
        self.set_a(sum);
    }

    fn sbc(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        let val = self.alu ^ 0xFF;
        let carry = self.flag(Flag::Carry) as u8;
        let (sum, carry0) = self.a.overflowing_add(val);
        let (sum, carry1) = sum.overflowing_add(carry);
        self.inner_set_flag(Flag::Carry, carry0 || carry1);
        self.inner_set_flag(Flag::Overflow, !(self.a ^ val) & (self.a ^ sum) & 0x80 != 0);
        self.set_a(sum);
    }

    fn ora(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        self.set_a(self.a | self.alu);
    }

    fn eor(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        self.set_a(self.a ^ self.alu);
    }

    fn cmp(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        let t = self.a.wrapping_sub(self.alu);
        self.update_zero_negative_flags(t);
        self.inner_set_flag(Flag::Carry, self.a >= self.alu);
    }

    fn cpx(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        let t = self.x.wrapping_sub(self.alu);
        self.update_zero_negative_flags(t);
        self.inner_set_flag(Flag::Carry, self.x >= self.alu);
    }

    fn cpy(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }

        let t = self.y.wrapping_sub(self.alu);
        self.update_zero_negative_flags(t);
        self.inner_set_flag(Flag::Carry, self.y >= self.alu);
    }

    fn alr(&mut self) {
        self.a &= self.alu;
        self.inner_set_flag(Flag::Carry, self.a & 0x01 != 0);
        self.set_a(self.a >> 1);
    }

    fn anc(&mut self) {
        self.set_a(self.a & self.alu);
        self.inner_set_flag(Flag::Carry, self.a & 0x80 != 0);
    }

    fn arr(&mut self) {
        self.a &= self.alu;
        self.set_a((self.a >> 1) | ((self.flag(Flag::Carry) as u8) << 7));
        self.inner_set_flag(Flag::Carry, self.a & 0x40 != 0);
        self.inner_set_flag(Flag::Overflow, ((self.a >> 6) ^ (self.a >> 5)) & 1 != 0);
    }

    fn axs(&mut self) {
        let v = self.a & self.x;
        let (x, borrow) = v.overflowing_sub(self.alu);
        self.set_x(x);
        self.inner_set_flag(Flag::Carry, !borrow);
    }

    fn lax(&mut self) {
        self.set_a(self.alu);
        self.set_x(self.alu);
    }

    fn sax(&mut self) {
        self.write_byte(self.address_latch, self.a & self.x);
    }

    fn dcp(&mut self) {
        let v = self.alu.wrapping_sub(1);
        self.write_byte(self.address_latch, v);
        self.update_zero_negative_flags(self.a.wrapping_sub(v));
        self.inner_set_flag(Flag::Carry, self.a >= v);
    }

    fn isc(&mut self) {
        let v = self.alu.wrapping_add(1);
        self.write_byte(self.address_latch, v);
        self.alu = v;
        self.sbc(false);
    }

    fn rra(&mut self) {
        let carry = self.alu & 0x01 != 0;
        self.alu = (self.alu >> 1) | ((self.flag(Flag::Carry) as u8) << 7);
        self.write_byte(self.address_latch, self.alu);
        self.inner_set_flag(Flag::Carry, carry);
        self.adc(false);
    }

    fn rla(&mut self) {
        let new = (self.alu << 1) | (self.flag(Flag::Carry) as u8);
        self.inner_set_flag(Flag::Carry, self.alu & 0x80 != 0);
        self.alu = new;
        self.write_byte(self.address_latch, self.alu);
        self.and(false);
    }

    fn slo(&mut self) {
        self.inner_set_flag(Flag::Carry, self.alu & 0x80 != 0);
        self.alu <<= 1;
        self.write_byte(self.address_latch, self.alu);
        self.ora(false);
    }

    fn sre(&mut self) {
        self.inner_set_flag(Flag::Carry, self.alu & 0x01 != 0);
        self.alu >>= 1;
        self.write_byte(self.address_latch, self.alu);
        self.eor(false);
    }

    fn shx(&mut self) {
        let v = self.x & self.abh().wrapping_add(1);
        let addr = (self.abl() as u16) | ((v as u16) << 8);
        self.write_byte(addr, v);
    }

    fn shy(&mut self) {
        let v = self.y & self.abh().wrapping_add(1);
        let addr = (self.abl() as u16) | ((v as u16) << 8);
        self.write_byte(addr, v);
    }

    fn sha(&mut self) {
        // SHA (AHX/AXA): store A & X & (high-byte of addr + 1) at address
        let out = self.a & self.x & self.abh().wrapping_add(1);
        let addr = (self.abl() as u16) | ((out as u16) << 8);
        self.write_byte(addr, out);
    }

    fn tas(&mut self) {
        let v = self.a & self.x;
        self.sp = v;
        let out = v & self.abh().wrapping_add(1);
        let addr = (self.abl() as u16) | ((out as u16) << 8);
        self.write_byte(addr, out);
    }

    fn pha(&mut self) {
        self.push_stack(self.a);
    }

    fn pop_stack_into_alu(&mut self) {
        self.alu = self.pop_stack();
    }

    fn push_status(&mut self, set_disable_interrupt: bool, break_flag: bool) {
        self.push_stack(if break_flag {
            self.status | Flag::Break as u8 | Flag::NotUsed as u8
        } else {
            self.status | Flag::NotUsed as u8
        });
        if set_disable_interrupt {
            self.inner_set_flag(Flag::InterruptDisabled, true);
        }
    }

    fn plp(&mut self) {
        self.save_irq_inhibit();
        let saved = self.pop_stack();
        let break_flag = self.flag(Flag::Break);
        let not_used = self.flag(Flag::NotUsed);
        self.status = saved;
        self.inner_set_flag(Flag::Break, break_flag);
        self.inner_set_flag(Flag::NotUsed, not_used);
    }

    fn set_pc_to_ab(&mut self) {
        self.pc = self.address_latch;
    }

    fn and(&mut self, load_alu: bool) {
        if load_alu {
            self.load_alu();
        }
        self.set_a(self.a & self.alu);
    }

    fn bit(&mut self) {
        let v = self.read_byte(self.address_latch);
        self.inner_set_flag(Flag::Negative, v & 0x80 != 0);
        self.inner_set_flag(Flag::Overflow, v & 0x40 != 0);
        self.update_zero_flag(self.a & v);
    }

    fn push_microcodes(&mut self, microcodes: &[Microcode]) {
        self.microcode_queue.extend_back(microcodes.iter().copied());
    }

    /// Return how many microcodes are in queue, used for draining pending reset/interrupt work.
    #[cfg(test)]
    fn microcodes_len(&self) -> usize {
        self.microcode_queue.len()
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
    pub fn update_nmi_line(&mut self, nmi: bool, (scanline, dot): (u16, u16)) {
        if self.nmi_line != nmi {
            self.nmi_line = nmi;
            if nmi {
                self.request_nmi();
            }
        }

        if !nmi && scanline == 241 && dot <= 4 {
            self.nmi_requested_at = None;
        }
    }

    fn load_irq_pcl(&mut self) {
        let low = if self.nmi_ready() {
            self.nmi_requested_at = None;
            self.irq_hijacked = true;
            // hijacked by nmi
            self.mode = CpuMode::Nmi;
            self.read_byte(0xFFFA)
        } else {
            self.irq_hijacked = false;
            self.defer_nmi_poll = true;
            self.read_byte(0xFFFE)
        };
        self.pc = low as u16;
    }

    fn load_irq_pch(&mut self) {
        let high = if self.irq_hijacked {
            // hijacked by nmi
            self.mode = CpuMode::Nmi;
            self.inner_set_flag(Flag::InterruptDisabled, true);
            self.read_byte(0xFFFB)
        } else {
            self.defer_nmi_poll = true;
            self.read_byte(0xFFFF)
        };
        self.pc |= (high as u16) << 8;
    }

    /// Set register A and update negative and zero flags.
    fn set_a(&mut self, val: u8) {
        self.a = val;
        self.update_zero_negative_flags(val);
    }

    /// Set register X and update negative and zero flags.
    fn set_x(&mut self, val: u8) {
        self.x = val;
        self.update_zero_negative_flags(val);
    }

    /// Set register Y and update negative and zero flags.
    fn set_y(&mut self, val: u8) {
        self.y = val;
        self.update_zero_negative_flags(val);
    }

    fn update_zero_negative_flags(&mut self, val: u8) {
        self.update_zero_flag(val);
        self.update_negative_flag(val);
    }
}

// ExecuteResult, Plugin, EmptyPlugin, and Flag remain from original cpu.rs

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
    fn start(&mut self, cpu: &mut Cpu<M>);

    /// After execute instruction
    fn end(&mut self, cpu: &mut Cpu<M>);

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
    fn start(&mut self, _: &mut Cpu<M>) {}

    fn end(&mut self, _: &mut Cpu<M>) {}
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
