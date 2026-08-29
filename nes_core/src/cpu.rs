use crate::view::MachineView;
use crate::{SystemClock, cpu::microcode::opcode, mcu::Mcu};
use arraydeque::ArrayDeque;
use microcode::{
    AOrMemory, CrossPageBehavior, HijackEligibility, HijackSite, IncDecTarget, InterruptSequences,
    InterruptWindow, Microcode, OpAfterAddressing,
};

mod microcode;
mod reg16;

use self::reg16::Register16;
/// Snapshot of CPU registers — the read-model exposed to plugins/tools.
/// Replaces direct `cpu.a/x/y/sp/status/pc` field access (ADR-0010).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CpuSnapshot {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub sp: u8,
    pub status: u8,
    pub pc: u16,
    pub halt: bool,
}

/// Named result of `Cpu::tick` — replaces the unnamed `(ExecuteResult, bool)` tuple.
/// `instruction_complete` is true when the microcode queue drained (instruction boundary).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TickOutcome {
    pub control: ExecuteResult,
    pub instruction_complete: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Register {
    A,
    X,
    Y,
}

#[derive(Default, Debug)]
struct IrqDetector {
    irq_pending: bool,
    irq_input: bool,
    /// Last line-transition dot. Write-only outside interrupt tracking —
    /// stamped only while `NES_INTERRUPT_TRACK` is set so production builds
    /// carry no dead bookkeeping; read by `dump_interrupt_track`.
    irq_line_changed_at: Option<SystemClock>,
}

impl IrqDetector {
    fn update_irq_input(&mut self, v: bool, clock: SystemClock, track: bool) {
        if track && self.irq_input != v {
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
#[derive(Default, Debug)]
struct NmiDetector {
    nmi_pending: bool,
    /// Line level observed by the previous CPU-cycle sample.
    last_sampled_level: bool,
    nmi_input: bool,
    /// Levels at the two preceding dots: `[t-2, t-1]` while processing dot
    /// `t`. The 6502 effectively samples /NMI at the FIRST dot of each CPU
    /// cycle (`clock % 3 == 0`); blargg's ppu_vbl_nmi 06-suppression and
    /// 07-nmi_on_timing pin this phase from both sides: an assertion is seen
    /// iff it covers such a dot.
    level_history: [bool; 2],
    /// `asserted_since` value for each of those dots, shifted in lockstep —
    /// a lagged sample that observes a rise must still know when it rose,
    /// even if the line has already fallen by sampling time.
    stamp_history: [Option<u64>; 2],
    nmi_line_changed_at: Option<SystemClock>,
    /// System cycle at which the current /NMI assertion began.
    asserted_since: Option<u64>,
    /// `asserted_since` value of the assertion that latched `nmi_pending`.
    pending_asserted_since: Option<u64>,
    /// Newest `asserted_since` whose edge has already been consumed by a
    /// dispatch or vector hijack; suppresses re-latching the same assertion
    /// while it is still high on the line.
    consumed_through: Option<u64>,
}

impl NmiDetector {
    fn update_nmi_input(&mut self, v: bool, clock: SystemClock) {
        let prev = self.nmi_input;
        let prev_stamp = self.asserted_since;
        self.level_history = [self.level_history[1], prev];
        self.stamp_history = [self.stamp_history[1], prev_stamp];
        if prev != v {
            self.nmi_line_changed_at = Some(clock);
            if v {
                self.asserted_since = Some(clock.cycles());
            } else {
                self.asserted_since = None;
            }
        }
        self.nmi_input = v;
    }

    fn cancel_rising_edge_at(&mut self, clock: SystemClock) {
        if self.nmi_line_changed_at.map(SystemClock::cycles) == Some(clock.cycles())
            && self.nmi_input
        {
            // The line never really asserted: consume the stamp so neither
            // the sampler nor a vector-hijack check can fire it later.
            self.mark_consumed();
            self.nmi_input = false;
            self.last_sampled_level = false;
            self.nmi_line_changed_at = None;
        }
    }

    fn detect_nmi(&mut self) -> bool {
        // Sample even while a previous edge is still pending: freezing the
        // sampler would hide newer edges (and leave a stale last-seen level
        // behind once the pending edge is taken).
        let sampled_level = self.level_history[0];
        let rising_edge = !self.last_sampled_level && sampled_level;
        self.last_sampled_level = sampled_level;
        if rising_edge {
            let stamp = self.stamp_history[0];
            // Skip edges whose assertion was already consumed while high
            // (see `mark_consumed`): stamps are monotonic, so only a
            // genuinely newer assertion may latch.
            if !self
                .consumed_through
                .is_some_and(|c| stamp.is_some_and(|s| s <= c))
            {
                self.nmi_pending = true;
                self.pending_asserted_since = stamp;
            }
        }
        self.nmi_pending
    }

    /// Record that the newest visible assertion's edge was consumed (by an
    /// end-of-instruction dispatch here, or a vector hijack): neither the
    /// sampler nor a later hijack check may fire it again while the line
    /// stays high.
    fn mark_consumed(&mut self) {
        self.nmi_pending = false;
        let newest = self
            .pending_asserted_since
            .max(self.nmi_input.then_some(self.asserted_since).flatten());
        self.consumed_through = self.consumed_through.max(newest);
    }

    /// Decide the interrupt-vector switch performed during BRK / IRQ
    /// sequences ("NMI hijacks the vector"). Hardware resolves the vector
    /// early in the sequence: blargg's cpu_interrupts_v2 "2-nmi_and_brk"
    /// pins an assertion on the last dot of BRK's PushPch cycle as still
    /// hijacking, and one on the first PushPcl dot as deferring to normal
    /// post-dispatch service — a decision boundary at the end of cycle 3,
    /// seven dots before the BRK sequence's vector-fetch tick. The site's
    /// deadline is declared in the sequence's window; an assertion counts
    /// iff its newest unconsumed edge had risen by then. Consumes the
    /// edge.
    fn consume_hijack_edge(&mut self, cutoff: u64) -> bool {
        let newest = self
            .nmi_pending
            .then_some(self.pending_asserted_since)
            .flatten()
            .max(self.nmi_input.then_some(self.asserted_since).flatten());
        match newest {
            Some(t0) if t0 <= cutoff && !self.consumed_through.is_some_and(|c| c >= t0) => {
                self.mark_consumed();
                true
            }
            _ => false,
        }
    }

    /// The PushStatus site's rule: consume the sampler-latched pending
    /// edge iff it had risen by `cutoff`. Newer un-latched rises on the
    /// line do not participate and stay eligible for the later
    /// vector-decision site; only the consumed edge is stamped.
    fn consume_latched_edge(&mut self, cutoff: u64) -> bool {
        match self
            .nmi_pending
            .then_some(self.pending_asserted_since)
            .flatten()
        {
            Some(t0) if t0 <= cutoff && !self.consumed_through.is_some_and(|c| c >= t0) => {
                self.consume_at(t0);
                true
            }
            _ => false,
        }
    }

    /// Stamp `consumed_through` through `t0`, clearing the pending latch.
    fn consume_at(&mut self, t0: u64) {
        self.nmi_pending = false;
        self.consumed_through = self.consumed_through.max(Some(t0));
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum InterruptType {
    Nmi,
    Irq,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum ValueSource {
    Immediate,
    /// Read mem from zero page
    ZeroPage,
    /// Read mem for other cases
    Mem,
}

/// What a Microcode cycle drives on the bus — the single classification
/// (ADR-0007) every bus-side consumer (DMC DMA freeze/halt) delegates to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BusCycle {
    /// A read cycle; the address it drives is resolved from CPU state
    /// (or is a fixed vector address).
    Read(ReadAddress),
    /// A write cycle. The CPU is never frozen on a write cycle.
    Write,
    /// No bus access of its own; a RDY halt repeats the last completed read.
    Internal,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ReadAddress {
    /// Instruction stream: opcode and operand fetches, plus the hardware
    /// dummy reads that drive PC (implied ops, branch offset cycles).
    ProgramCounter,
    /// The address bus latch, already index-adjusted per variant.
    Latch(u16),
    /// Stack page: pop cycles read at $0100 | (SP + 1).
    Stack(u16),
    /// Fixed interrupt vector addresses.
    Fixed(u16),
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

/// The interrupt recognition window armed for the microcode sequence
/// currently executing (ADR-0006). `started_at` is the cycle executing
/// micro-op 0: stamped eagerly when the sequence dispatches its head on
/// the arming tick (NMI/IRQ), lazily on the first pop otherwise (RESET,
/// BRK after its fetch, hijack continuations).
#[derive(Clone, Copy)]
struct ActiveInterruptWindow {
    window: &'static InterruptWindow,
    started_at: Option<SystemClock>,
}

pub struct Cpu<M: Mcu> {
    a: u8,
    x: u8,
    y: u8,
    pc: Register16,
    sp: u8,
    status: u8,
    last_status: u8,

    opcode: u8,
    /// address bus, which memory byte that cpu current select
    ab: Register16,
    /// data bus, what byte that cpu will save or get from memory bus
    db: u8, // save low byte during indexed addressing
    alu: u8,

    active_interrupt_window: Option<ActiveInterruptWindow>,
    nmi_detecteor: NmiDetector,
    interrupt_detected: Option<InterruptType>,
    irq_detector: IrqDetector,
    request_detect_interrupt: Option<bool>,
    pub(crate) last_read_addr: Option<u16>,
    /// System tick of the microcode executing this `tick()`; interrupt
    /// decisions (the BRK/IRQ vector-hijack window) measure against it.
    now: SystemClock,

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
            active_interrupt_window: None,
            interrupt_detected: None,
            nmi_detecteor: Default::default(),
            irq_detector: Default::default(),
            request_detect_interrupt: None,
            ab: Register16::default(),
            db: 0,
            alu: 0,
            last_read_addr: None,
            now: SystemClock::default(),
            microcode_queue: ArrayDeque::new(),
            halt: false,
            track_interrupt: std::env::var("NES_INTERRUPT_TRACK").is_ok(),
            frozen: false,
        };
        r.reset();
        r
    }

    pub(crate) fn mcu(&self) -> &M {
        &self.mcu
    }

    pub(crate) fn mcu_mut(&mut self) -> &mut M {
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

    /// Read-only accessors for the register file — the per-dot contract (ADR-0010).
    /// Fields are private; writes go via internal methods or test helpers.
    pub fn a(&self) -> u8 {
        self.a
    }
    pub fn x(&self) -> u8 {
        self.x
    }
    pub fn y(&self) -> u8 {
        self.y
    }
    pub fn sp(&self) -> u8 {
        self.sp
    }
    pub fn status(&self) -> u8 {
        self.status
    }
    /// Owned snapshot of the register file for plugins/tools.
    pub fn snapshot(&self) -> CpuSnapshot {
        CpuSnapshot {
            a: self.a,
            x: self.x,
            y: self.y,
            sp: self.sp,
            status: self.status,
            pc: self.pc.get(),
            halt: self.halt,
        }
    }

    /// Read-model view for plugins — snapshot + address-space peek (ADR-0010).
    pub fn view(&self, clock: SystemClock) -> crate::view::MachineView<'_, M> {
        crate::view::MachineView::new(self.snapshot(), &self.mcu, clock)
    }

    /// Test-only helpers to set registers without exposing `pub` writes.
    #[cfg(test)]
    pub fn set_a(&mut self, v: u8) {
        self.a = v;
    }
    #[cfg(test)]
    pub fn set_x(&mut self, v: u8) {
        self.x = v;
    }
    #[cfg(test)]
    pub fn set_y(&mut self, v: u8) {
        self.y = v;
    }
    #[cfg(test)]
    pub fn set_sp(&mut self, v: u8) {
        self.sp = v;
    }
    #[cfg(test)]
    pub fn set_status(&mut self, v: u8) {
        self.status = v;
    }

    pub fn microcodes_empty(&self) -> bool {
        self.microcode_queue.is_empty()
    }

    /// Drain microcodes to the next instruction boundary (queue empty).
    ///
    /// CPU-only drain: each step calls `self.tick(plugin, *clock)` then
    /// `*clock = clock.inc()`. No PPU/APU/DMA device is ticked and time
    /// advances by exactly `queue.len()` dots (one per microcode, not one
    /// per CPU cycle) — the setup-time seam. For full device interleaving
    /// use `NesMachine::run_to_instruction_boundary` which ticks via
    /// `NesMachine::tick()`.
    pub fn run_to_instruction_boundary<P: Plugin<M>>(
        &mut self,
        plugin: &mut P,
        clock: &mut SystemClock,
    ) {
        while !self.microcodes_empty() {
            self.tick(plugin, *clock);
            *clock = clock.inc();
        }
    }

    pub(crate) fn next_microcode(&self) -> Microcode {
        self.microcode_queue
            .front()
            .copied()
            .unwrap_or(Microcode::FetchAndDecode)
    }

    /// True if the CPU can be paused for DMA: the pending cycle is not a
    /// write. DMC DMA must not freeze the CPU during a write cycle.
    pub(crate) fn can_pause(&self) -> bool {
        !matches!(self.pending_bus_cycle(), BusCycle::Write)
    }

    /// Address the CPU drives on the bus for its pending cycle, mirroring
    /// how a RDY-halt externally repeats the current read cycle. Returns
    /// `None` when the pending cycle is purely internal (no bus access of
    /// its own); callers then repeat the last completed read instead. Must
    /// not be called with a write cycle pending — the `can_pause` gate in
    /// `NesDmaSupport::try_freeze` guarantees that, and this projection
    /// panics if the gate is ever bypassed (a swallowed write would be far
    /// harder to debug).
    pub(crate) fn dma_halt_bus_addr(&self) -> Option<u16> {
        match self.pending_bus_cycle() {
            BusCycle::Read(ReadAddress::ProgramCounter) => Some(self.pc.get()),
            BusCycle::Read(ReadAddress::Latch(addr)) => Some(addr),
            BusCycle::Read(ReadAddress::Stack(addr)) => Some(addr),
            BusCycle::Read(ReadAddress::Fixed(addr)) => Some(addr),
            BusCycle::Internal => None,
            BusCycle::Write => unreachable!(
                "DMA halt projection called with a write cycle pending; the can_pause gate must prevent freezing mid-write"
            ),
        }
    }

    /// The bus cycle the queue-front Microcode drives this cycle.
    pub(crate) fn pending_bus_cycle(&self) -> BusCycle {
        self.bus_cycle(self.next_microcode())
    }

    /// The bus cycle this Microcode would drive as the pending cycle,
    /// resolved against current CPU state. Its contract is the cycle
    /// *hardware* would drive — which may be richer than what `exec`
    /// touches: `Nop` repeats a PC fetch (the implied-op dummy read) and
    /// the zero-page index-add cycle repeats the unindexed read, though
    /// exec performs neither.
    pub(crate) fn bus_cycle(&self, mc: Microcode) -> BusCycle {
        match mc {
            // Instruction-stream fetches (opcode, operands, branch offsets,
            // the JSR high byte) and the implied-op dummy read (`Nop`).
            Microcode::FetchAndDecode
            | Microcode::FetchOnly
            | Microcode::AbsoluteL
            | Microcode::AbsoluteH
            | Microcode::ZeroPage
            | Microcode::SkipImmediate
            | Microcode::BranchRelative(_)
            | Microcode::ImmediateWithOp(_)
            | Microcode::AlrImmediate
            | Microcode::AncImmediate
            | Microcode::ArrImmediate
            | Microcode::AxsImmediate
            | Microcode::AneImmediate
            | Microcode::LaxImmediate
            | Microcode::LoadPcAbsoluteH
            | Microcode::Nop => BusCycle::Read(ReadAddress::ProgramCounter),
            // Immediate-mode operand fetch.
            Microcode::LoadR(ValueSource::Immediate, _) => {
                BusCycle::Read(ReadAddress::ProgramCounter)
            }
            // Zero-page index add: hardware reads the unindexed address
            // while adding the index; exec skips that read.
            Microcode::ZeroPageIndexedX | Microcode::ZeroPageIndexedY => {
                BusCycle::Read(ReadAddress::Latch(self.ab.get()))
            }
            // (ind) high byte: low-byte increment without page carry.
            Microcode::IndexedH | Microcode::IndexedHAndJump => {
                let ab = self.ab.get();
                let low = (ab as u8).wrapping_add(1) as u16;
                BusCycle::Read(ReadAddress::Latch((ab & 0xFF00) | low))
            }
            Microcode::IndexedXWithOp { op, first_clock } => {
                self.indexed_with_op_cycle(op, first_clock, self.x)
            }
            Microcode::IndexedYWithOp { op, first_clock } => {
                self.indexed_with_op_cycle(op, first_clock, self.y)
            }
            // Interrupt vector fetches. LoadIrqPcL's hijack site (ADR-0006)
            // may redirect the read to the NMI vector inside exec; the
            // declared default cycle is the IRQ vector.
            Microcode::LoadNmiPcL => BusCycle::Read(ReadAddress::Fixed(0xFFFA)),
            Microcode::LoadNmiPcH => BusCycle::Read(ReadAddress::Fixed(0xFFFB)),
            Microcode::LoadResetPcL => BusCycle::Read(ReadAddress::Fixed(0xFFFC)),
            Microcode::LoadResetPcH => BusCycle::Read(ReadAddress::Fixed(0xFFFD)),
            Microcode::LoadIrqPcL => BusCycle::Read(ReadAddress::Fixed(0xFFFE)),
            Microcode::LoadIrqPcH => BusCycle::Read(ReadAddress::Fixed(0xFFFF)),
            // Stack pops read at $0100 | (SP + 1).
            Microcode::Plp | Microcode::PopPcL | Microcode::PopPcH | Microcode::PopStack => {
                BusCycle::Read(ReadAddress::Stack(self.stack_pop_addr()))
            }
            // Reads through the address latch. Standalone `Las` is queued
            // only as the page-crossed LAS abs,y refetch cycle: hardware
            // drives the operand read at the (already index-adjusted) latch
            // there, even though exec performs register math on the stale
            // ALU (pre-existing exec defect — see the golden's LAS row).
            Microcode::LoadR(ValueSource::ZeroPage, _)
            | Microcode::LoadR(ValueSource::Mem, _)
            | Microcode::LoadIntoAlu(_)
            | Microcode::IndexedL
            | Microcode::Lax
            | Microcode::Las => BusCycle::Read(ReadAddress::Latch(self.ab.get())),
            // ALU operand reads; the immediate payload (never built into
            // sequences) classifies as its mode's fetch.
            Microcode::Adc(src)
            | Microcode::Sbc(src)
            | Microcode::Cmp(src)
            | Microcode::Cpx(src)
            | Microcode::Cpy(src)
            | Microcode::Ora(src)
            | Microcode::Eor(src)
            | Microcode::And(src)
            | Microcode::Bit(src) => match src {
                ValueSource::Immediate => BusCycle::Read(ReadAddress::ProgramCounter),
                ValueSource::ZeroPage | ValueSource::Mem => {
                    BusCycle::Read(ReadAddress::Latch(self.ab.get()))
                }
            },
            // Write cycles: stores, stack pushes, and the RMW tail
            // (first write of the old value, second of the new).
            Microcode::StoreR(..)
            | Microcode::StoreAlu(..)
            | Microcode::Shx
            | Microcode::Shy
            | Microcode::Sha
            | Microcode::Tas
            | Microcode::Sax
            | Microcode::Rla
            | Microcode::Dcp
            | Microcode::Isc
            | Microcode::Rra
            | Microcode::Slo
            | Microcode::Sre
            | Microcode::PushStatus { .. }
            | Microcode::PushStack(_)
            | Microcode::Asl(AOrMemory::Memory)
            | Microcode::Lsr(AOrMemory::Memory)
            | Microcode::Rol(AOrMemory::Memory)
            | Microcode::Ror(AOrMemory::Memory)
            | Microcode::IncDec(IncDecTarget::IncrementAlu | IncDecTarget::DecrementAlu) => {
                BusCycle::Write
            }
            // Purely internal cycles: flags, transfers, register INC/DEC,
            // the ALU shift/rotate step, JAM.
            Microcode::Asl(AOrMemory::Accumulator)
            | Microcode::Lsr(AOrMemory::Accumulator)
            | Microcode::Rol(AOrMemory::Accumulator)
            | Microcode::Ror(AOrMemory::Accumulator)
            | Microcode::IncDec(
                IncDecTarget::IncrementX
                | IncDecTarget::IncrementY
                | IncDecTarget::DecrementX
                | IncDecTarget::DecrementY,
            )
            | Microcode::SetFlag(_)
            | Microcode::ClearFlag(_)
            | Microcode::Transfer(_)
            | Microcode::IncPc
            | Microcode::UpdateAFromAlu
            | Microcode::SkipDetectInterrupt
            | Microcode::Kill => BusCycle::Internal,
        }
    }

    /// The indexed-with-op cycle: a read op with `FirstClock` reads the
    /// operand at the indexed address when no page is crossed; otherwise
    /// (crossing, or `FirstClockAlways` — every store) it is the dummy read
    /// at (old high | new low), the 6502's page-fault emulation.
    fn indexed_with_op_cycle(
        &self,
        op: OpAfterAddressing,
        first_clock: CrossPageBehavior,
        idx: u8,
    ) -> BusCycle {
        let ab = self.ab.get();
        let new_ab = ab.wrapping_add(idx as u16);
        let dummy = (ab & 0xFF00) | (new_ab & 0xFF);
        let crossed = new_ab & 0xFF00 != ab & 0xFF00;
        if crossed || matches!(first_clock, CrossPageBehavior::FirstClockAlways) {
            BusCycle::Read(ReadAddress::Latch(dummy))
            // Store-class ops — keep in sync with the Microcode write arm
            // above (StoreR/Shx/Shy/Sha/Tas classify as Write there).
        } else if matches!(
            op,
            OpAfterAddressing::StoreA
                | OpAfterAddressing::Shx
                | OpAfterAddressing::Shy
                | OpAfterAddressing::Sha
                | OpAfterAddressing::Tas
        ) {
            BusCycle::Write
        } else {
            BusCycle::Read(ReadAddress::Latch(new_ab))
        }
    }

    /// Stack address a pop cycle reads: $0100 | (SP + 1).
    fn stack_pop_addr(&self) -> u16 {
        0x100 | self.sp.wrapping_add(1) as u16
    }
    pub fn reset(&mut self) {
        self.set_flag(Flag::InterruptDisabled, true);
        self.set_flag(Flag::NotUsed, true);
        self.microcode_queue.clear();
        self.interrupt_detected = None;
        self.halt = false;
        self.sp = self.sp.wrapping_sub(3);
        self.nmi_detecteor = Default::default();
        self.irq_detector = Default::default();
        self.request_detect_interrupt = None;
        self.frozen = false;
        self.last_read_addr = None;
        self.last_status = self.status;

        self.push_microcodes(&InterruptSequences::RESET);
        self.arm_interrupt_window(&InterruptSequences::RESET_WINDOW, false);
    }

    pub(crate) fn set_irq(&mut self, enabled: bool, clock: SystemClock) {
        self.irq_detector
            .update_irq_input(enabled, clock, self.track_interrupt);
    }

    /// Single published interrupt entry — updates both IRQ level and NMI
    /// line atomically, owning the same-tick race-retract internally.
    ///
    /// `lines.irq_level` is already time-corrected (APU +1, cartridge
    /// latch quantized); `lines.nmi.race_cancel` is the same-dot vblank
    /// race suppression consumed via `consumed_through` (ADR-0006 third
    /// writer alongside the two hijack sites).
    pub fn update_interrupt_lines(
        &mut self,
        lines: crate::interrupt::InterruptLines,
        clock: SystemClock,
    ) {
        self.set_irq(lines.irq_level, clock);
        self.update_nmi_line(lines.nmi.level, clock);
        if lines.nmi.race_cancel {
            self.cancel_nmi_rising_edge(clock);
        }
    }

    pub fn is_halted(&self) -> bool {
        self.halt
    }

    /// Return true if just execute current instruction
    pub fn tick<P: Plugin<M>>(&mut self, plugin: &mut P, clock: SystemClock) -> TickOutcome {
        self.now = clock;

        if self.frozen {
            if self.track_interrupt {
                println!("[{}] (frozen)", clock.cycles());
            }
            return TickOutcome {
                control: ExecuteResult::Continue,
                instruction_complete: false,
            };
        }

        if self.is_halted() {
            if self.track_interrupt {
                println!("[{}] (halted)", clock.cycles());
            }
            return TickOutcome {
                control: ExecuteResult::Halt,
                instruction_complete: false,
            };
        }

        if let Some(active) = self.active_interrupt_window.as_mut() {
            if active.started_at.is_none() {
                active.started_at = Some(clock);
            }
        }
        let code = match self.pop_microcode() {
            Some(v) => v,
            None => {
                let view = MachineView::new(self.snapshot(), &self.mcu, clock);
                plugin.start(&view, clock);
                match self.interrupt_detected.take() {
                    Some(InterruptType::Nmi) => self.push_enter_interrupt_microcodes(true),
                    Some(InterruptType::Irq) => self.push_enter_interrupt_microcodes(false),
                    None => Microcode::FetchAndDecode,
                }
            }
        };

        self.last_status = self.status;
        code.exec(self);
        // Suppression is declared: the armed window's `suppress_final_poll`
        // owns the final-cycle poll. The queue drains exactly on the final
        // op of the armed sequence (or a hijack continuation's tail), so
        // an armed window over an empty queue is that cycle.
        let suppress = self.active_interrupt_window.is_some_and(|active| {
            active.window.suppress_final_poll && self.microcode_queue.is_empty()
        });
        if self.microcode_queue.is_empty() {
            self.active_interrupt_window = None;
        }
        self.detect_interrupt(clock, suppress);

        if self.track_interrupt {
            self.dump_interrupt_track(clock);
        }
        if self.microcode_queue.is_empty() {
            let view = MachineView::new(self.snapshot(), &self.mcu, clock);
            plugin.end(&view, clock);
            TickOutcome {
                control: plugin.should_stop(),
                instruction_complete: true,
            }
        } else {
            TickOutcome {
                control: ExecuteResult::Continue,
                instruction_complete: false,
            }
        }
    }

    fn do_detect_interrupt(&mut self, clock: SystemClock) {
        if self.nmi_detecteor.nmi_pending
            && self.nmi_detecteor.pending_asserted_since.is_some_and(|t0| {
                if self.track_interrupt {
                    dbg!((clock.0, t0));
                }
                // Recognition latency runs from the /NMI edge that set the
                // pending latch — NOT from `nmi_line_changed_at`, which the
                // falling edge re-stamps. A short assertion (e.g. NMI enabled
                // a few PPU dots before vblank ends, blargg ppu_vbl_nmi
                // 07-nmi_on_timing) would otherwise postpone its own
                // recognition until after the interrupt window closed.
                (clock.0 - t0) > 1
            })
        {
            self.nmi_detecteor.mark_consumed();
            if self.track_interrupt {
                println!(
                    "Enter NMI: ${:x}, carry flag: {}",
                    self.status,
                    self.flag(Flag::Carry)
                );
            }
            self.interrupt_detected = Some(InterruptType::Nmi);
        } else if self.irq_detector.irq_pending() {
            self.interrupt_detected = Some(InterruptType::Irq);
        } else {
            debug_assert!(self.interrupt_detected.is_none());
        }
    }

    fn detect_interrupt(&mut self, clock: SystemClock, suppress_final_poll: bool) {
        self.nmi_detecteor.detect_nmi();
        let disabled = if matches!(self.opcode, opcode::CLI | opcode::SEI | opcode::PLP) {
            (self.last_status & Flag::InterruptDisabled as u8) != 0
        } else {
            self.flag(Flag::InterruptDisabled)
        };
        self.irq_detector.detect_irq(disabled);

        let request = std::mem::take(&mut self.request_detect_interrupt);
        // The standard poll fires on the instruction's final cycle —
        // suppressed on the final cycle of an interrupt sequence, whose
        // recognition belongs to its declared hijack sites.
        let is_last_op = self.microcode_queue.is_empty() && !suppress_final_poll;
        if let (None, true) | (Some(true), _) = (request, is_last_op) {
            self.do_detect_interrupt(clock)
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

    fn push_status(&mut self, break_flag: bool) {
        if let Some((_, cutoff)) = self.declared_hijack_site(HijackEligibility::LatchedOnly) {
            if self.nmi_detecteor.consume_latched_edge(cutoff) {
                self.switch_to_nmi_vector(&[Microcode::LoadNmiPcL, Microcode::LoadNmiPcH]);
                self.push_status(break_flag);
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

    /// Arm the recognition window of an interrupt sequence about to run.
    /// With `starts_now` the sequence's first micro-op executes on the
    /// current tick (the dispatch head); otherwise the start is stamped
    /// on the first pop after arming (RESET, BRK after its fetch, hijack
    /// continuations).
    fn arm_interrupt_window(&mut self, window: &'static InterruptWindow, starts_now: bool) {
        self.active_interrupt_window = Some(ActiveInterruptWindow {
            window,
            started_at: starts_now.then_some(self.now),
        });
    }

    /// The declared hijack site for the micro-op executing this cycle, if
    /// the armed window schedules one at this sequence index, with its
    /// cutoff in dots. `expected` pins the caller's op kind to the
    /// declared rule (PushStatus = LatchedOnly, LoadIrqPcL = NewestVisible).
    fn declared_hijack_site(
        &self,
        expected: HijackEligibility,
    ) -> Option<(&'static HijackSite, u64)> {
        let active = self.active_interrupt_window.as_ref()?;
        let started = active.started_at?;
        let index = (self.now.cycles() - started.cycles()) / 3;
        debug_assert!(
            index < u64::from(active.window.sequence_cycles),
            "sequence index past the declared window"
        );
        let site = active
            .window
            .hijack_sites
            .iter()
            .find(|s| u64::from(s.op_index) == index)?;
        debug_assert_eq!(site.eligibility, expected, "window and op disagree");
        Some((site, started.cycles() + site.deadline_dots))
    }

    /// Switch an in-flight BRK/IRQ vector fetch to the NMI vector: stamp
    /// the hijack for the interrupt tracker, arm the NMI window for the
    /// continuation (lazy start — its first op runs next cycle; the tail
    /// is the NMI sequence's final cycles), and replace the remaining
    /// queue with the tail ops.
    fn switch_to_nmi_vector(&mut self, tail: &[Microcode]) {
        if self.track_interrupt {
            println!(
                "hijack: ${:x}, carry flag: {}",
                self.status,
                self.flag(Flag::Carry)
            );
        }
        self.arm_interrupt_window(&InterruptSequences::NMI_WINDOW, false);
        self.microcode_queue.clear();
        self.push_microcodes(tail);
    }

    /// Push the NMI/IRQ microcode sequence and return its head for
    /// execution on the current cycle. The sequence owns both hardware
    /// dead cycles (T1/T2) as its first two ops, so interrupt entry is
    /// exactly seven CPU cycles from dispatch to vector fetch.
    fn push_enter_interrupt_microcodes(&mut self, nmi: bool) -> Microcode {
        let (sequence, window) = if nmi {
            (&InterruptSequences::NMI, &InterruptSequences::NMI_WINDOW)
        } else {
            (&InterruptSequences::IRQ, &InterruptSequences::IRQ_WINDOW)
        };
        // The head (T1) runs on this cycle — the sequence starts now; the
        // rest drains from the queue on the following ticks.
        self.arm_interrupt_window(window, true);
        self.push_microcodes(&sequence[1..]);
        sequence[0]
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
    pub(crate) fn update_nmi_line(&mut self, nmi: bool, clock: SystemClock) {
        self.nmi_detecteor.update_nmi_input(nmi, clock);
    }
    /// Retract an NMI rising edge latched on `clock` itself (same-tick vblank
    /// race resolution; see `NmiDetector::cancel_rising_edge_at`).
    /// Third writer of `consumed_through` alongside the two hijack sites
    /// (ADR-0006 / ADR-0008).
    pub(crate) fn cancel_nmi_rising_edge(&mut self, clock: SystemClock) {
        self.nmi_detecteor.cancel_rising_edge_at(clock);
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
        if let Some((_, cutoff)) = self.declared_hijack_site(HijackEligibility::NewestVisible) {
            if self.nmi_detecteor.consume_hijack_edge(cutoff) {
                self.switch_to_nmi_vector(&[Microcode::LoadNmiPcH]);
                self.load_nmi_pcl();
                return;
            }
        }

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
            "[{}] pc={:X} irq={} nmi={} nmi_st={:?} i={} op=${:02X}/{} q={} next={} int={}",
            clock.cycles(),
            self.pc(),
            irq_str,
            nmi_str,
            self.nmi_detecteor.nmi_pending,
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
    /// Before start execute new instruction — receives a read-model view (ADR-0010).
    fn start(&mut self, view: &MachineView<M>, system_clock: SystemClock);

    /// After execute instruction — receives the same view.
    fn end(&mut self, view: &MachineView<M>, system_clock: SystemClock);

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
    fn start(&mut self, _: &MachineView<M>, _: SystemClock) {}

    fn end(&mut self, _: &MachineView<M>, _: SystemClock) {}
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
