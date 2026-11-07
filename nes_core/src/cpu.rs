use std::num::NonZeroU64;

use crate::view::MachineView;
use crate::{SYSTEM_CYCLES_PER_CPU_CYCLE, SystemClock, bus::Bus, mcu::Mcu};
use arraydeque::ArrayDeque;
use microcode::{
    AOrMemory, CrossPageBehavior, HijackEligibility, HijackSite, IncDecTarget, InterruptSequences,
    InterruptWindow, Microcode, OpAfterAddressing,
};

pub(crate) mod microcode;
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
    irq_input: bool,
}

impl IrqDetector {
    fn update_irq_input(&mut self, v: bool) {
        self.irq_input = v;
    }

    /// The IRQ line is edge-latched into the instruction-end poll, but its
    /// pending state is purely a function of the line and the I flag.
    fn detect_irq(&self, interrupt_disabled: bool) -> bool {
        !interrupt_disabled && self.irq_input
    }
}
#[derive(Default, Debug)]
struct NmiDetector {
    nmi_pending: bool,
    /// Line level observed by the previous CPU-cycle sample.
    last_sampled_level: bool,
    nmi_input: bool,
    /// Line level and its `asserted_since` stamp as of the FIRST dot of
    /// the current CPU cycle (`clock % 3 == 0`) — the sample `detect_nmi`
    /// reads. The 6502 effectively samples /NMI at that dot;
    /// blargg's ppu_vbl_nmi 06-suppression and 07-nmi_on_timing pin this
    /// phase from both sides: an assertion is seen iff it covers such a
    /// dot. The stamp rides along because a lagged sample that observes a
    /// rise must still know when it rose, even if the line has already
    /// fallen by sampling time. Captured in `update_nmi_input` on
    /// `% 3 == 0` dots; `detect_nmi` runs on CPU dots (`% 3 == 2`), two
    /// dots later, so this is exactly the t-2 sample.
    lagged_sample: (bool, Option<NonZeroU64>),
    /// System cycle at which the current /NMI assertion began.
    asserted_since: Option<NonZeroU64>,
    /// `asserted_since` value of the assertion that latched `nmi_pending`.
    /// `Some` exactly while `nmi_pending` is true: set at latch in
    /// `detect_nmi`, cleared with the latch in `mark_consumed`/
    /// `consume_at` (the stamp survives in `consumed_through`).
    pending_asserted_since: Option<NonZeroU64>,
    /// Newest `asserted_since` whose edge has already been consumed by a
    /// dispatch or vector hijack; suppresses re-latching the same assertion
    /// while it is still high on the line.
    consumed_through: Option<NonZeroU64>,
}

impl NmiDetector {
    fn update_nmi_input(&mut self, v: bool, clock: SystemClock) {
        let prev = self.nmi_input;
        if prev != v {
            if v {
                self.asserted_since = NonZeroU64::new(clock.cycles());
            } else {
                self.asserted_since = None;
            }
        }
        self.nmi_input = v;
        if clock.cycles() % SYSTEM_CYCLES_PER_CPU_CYCLE == 0 {
            self.lagged_sample = (v, self.asserted_since);
        }
    }

    fn detect_nmi(&mut self) -> bool {
        // Sample even while a previous edge is still pending: freezing the
        // sampler would hide newer edges (and leave a stale last-seen level
        // behind once the pending edge is taken).
        let sampled_level = self.lagged_sample.0;
        let rising_edge = !self.last_sampled_level && sampled_level;
        self.last_sampled_level = sampled_level;
        if rising_edge {
            let stamp = self.lagged_sample.1;
            // Skip edges whose assertion was already consumed while high
            // (see `mark_consumed`): stamps are monotonic, so only a
            // genuinely newer assertion may latch.
            if !self.edge_consumed(stamp) {
                self.nmi_pending = true;
                self.pending_asserted_since = stamp;
            }
        }
        self.nmi_pending
    }

    /// True when `stamp` names an assertion already consumed by a
    /// dispatch or vector hijack.
    fn edge_consumed(&self, stamp: Option<NonZeroU64>) -> bool {
        self.consumed_through
            .is_some_and(|c| stamp.is_some_and(|s| s <= c))
    }

    /// Record that the newest visible assertion's edge was consumed (by an
    /// end-of-instruction dispatch here, or a vector hijack): neither the
    /// sampler nor a later hijack check may fire it again while the line
    /// stays high. The consumed stamp is folded into `consumed_through`
    /// before the pending latch is cleared, so clearing
    /// `pending_asserted_since` loses nothing.
    fn mark_consumed(&mut self) {
        let newest = self
            .pending_asserted_since
            .max(self.asserted_since.filter(|_| self.nmi_input));
        self.consumed_through = self.consumed_through.max(newest);
        self.nmi_pending = false;
        self.pending_asserted_since = None;
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
            .pending_asserted_since
            .max(self.asserted_since.filter(|_| self.nmi_input));
        match newest {
            Some(t0) if u64::from(t0) <= cutoff && !self.edge_consumed(Some(t0)) => {
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
        match self.pending_asserted_since {
            Some(t0) if u64::from(t0) <= cutoff && !self.edge_consumed(Some(t0)) => {
                self.consume_at(t0);
                true
            }
            _ => false,
        }
    }

    /// Stamp `consumed_through` through `t0`, clearing the pending latch.
    fn consume_at(&mut self, t0: NonZeroU64) {
        self.consumed_through = self.consumed_through.max(Some(t0));
        self.nmi_pending = false;
        self.pending_asserted_since = None;
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
    fn value<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) -> u8;
}

struct Immediate;
struct Alu;
struct ZeroPage;
struct Mem;

impl ValueSourceTrait for Immediate {
    fn value<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) -> u8 {
        cpu.inc_read_byte(bus)
    }
}

impl ValueSourceTrait for Alu {
    fn value<M: Mcu>(cpu: &mut Cpu, _bus: &mut Bus<M>) -> u8 {
        cpu.alu
    }
}

impl ValueSourceTrait for ZeroPage {
    fn value<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) -> u8 {
        bus.mcu_mut().read_zero_page(cpu.ab.low())
    }
}

impl ValueSourceTrait for Mem {
    fn value<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>) -> u8 {
        bus.mcu_mut().read(cpu.ab.get())
    }
}

trait ValueTargetTrait {
    fn write<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>, value: u8);
}

impl ValueTargetTrait for ZeroPage {
    fn write<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>, value: u8) {
        let clock = bus.clock();
        bus.mcu_mut().write_zero_page(cpu.ab.low(), value, clock);
    }
}

impl ValueTargetTrait for Mem {
    fn write<M: Mcu>(cpu: &mut Cpu, bus: &mut Bus<M>, value: u8) {
        let clock = bus.clock();
        bus.mcu_mut().write(cpu.ab.get(), value, clock);
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

pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    pc: Register16,
    sp: u8,
    status: u8,

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

    pub(crate) frozen: bool,
    halt: bool,

    microcode_queue: ArrayDeque<Microcode, 8>,
}

impl Cpu {
    pub fn new() -> Cpu {
        let mut r = Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: Register16::default(),
            sp: 0,
            status: 0,
            active_interrupt_window: None,
            interrupt_detected: None,
            nmi_detecteor: Default::default(),
            irq_detector: Default::default(),
            request_detect_interrupt: None,
            ab: Register16::default(),
            db: 0,
            alu: 0,
            microcode_queue: ArrayDeque::new(),
            halt: false,
            frozen: false,
        };
        r.reset();
        r
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
    /// CPU-only drain: each step calls `self.tick(bus, plugin)` then
    /// advances the *bus's* master clock by one dot. No PPU/APU/DMA device
    /// is ticked and time advances by exactly `queue.len()` dots (one per
    /// microcode, not one per CPU cycle) — the setup-time seam. For full
    /// device interleaving use `NesMachine::run_to_instruction_boundary`
    /// which ticks via `NesMachine::tick()`.
    pub fn run_to_instruction_boundary<P: Plugin<M>, M: Mcu>(
        &mut self,
        bus: &mut Bus<M>,
        plugin: &mut P,
    ) {
        while !self.microcodes_empty() {
            self.tick(bus, plugin);
            bus.advance();
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

    /// Freeze entry for DMA-halt (the CPU-side half of the DMC DMA seam):
    /// true when the halt landed and the CPU is frozen; a pending write
    /// cycle must never be frozen, so freezing is refused then.
    pub(crate) fn try_freeze(&mut self) -> bool {
        debug_assert!(!self.frozen);
        if self.can_pause() {
            self.frozen = true;
            true
        } else {
            false
        }
    }

    /// Unfreeze counterpart of [`Cpu::try_freeze`], called when the DMA
    /// read cycle completes.
    pub(crate) fn unfreeze(&mut self) {
        debug_assert!(self.frozen);
        self.frozen = false;
    }

    /// Test-only staging for DMA-seam unit tests (`dmc_dma/tests.rs`):
    /// replace the pending microcode — and pin the address latch when the
    /// staged cycle drives one — so the pending bus cycle, and with it the
    /// freeze outcome, is exact.
    #[cfg(test)]
    pub(crate) fn stage_pending_cycle_for_test(&mut self, mc: Microcode, latch: Option<u16>) {
        self.frozen = false;
        self.microcode_queue.clear();
        self.push_microcode(mc);
        if let Some(addr) = latch {
            self.ab.set(addr);
        }
    }

    /// Test-only counterpart of the staging above: pin the next fetch
    /// address (`pc`) that internal-cycle DMA halts repeat on the bus.
    #[cfg(test)]
    pub(crate) fn set_next_fetch_pc_for_test(&mut self, pc: u16) {
        self.pc.set(pc);
    }

    /// Address the CPU drives on the bus for its pending cycle, mirroring
    /// how a RDY-halt externally repeats the current read cycle. Returns
    /// `None` when the pending cycle is purely internal (no bus access of
    /// its own); callers then repeat the next fetch address instead. Must
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
    /// resolved against current CPU state. Exec drives every classified
    /// cycle: `Nop` repeats a PC fetch (the implied-op dummy read), the
    /// zero-page index-add cycle repeats the unindexed read, and the
    /// stack/branch dummy reads drive their hardware addresses.
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
            // Hardware adds the index while re-reading the base address on
            // the bus; exec performs that dummy read (zero page is RAM only,
            // so it is side-effect-free).
            Microcode::ZeroPageIndexedX | Microcode::ZeroPageIndexedY => {
                BusCycle::Read(ReadAddress::Latch(self.ab.get()))
            }
            // Taken-branch page-cross fixup: the address was computed when
            // the cycle was queued.
            Microcode::DummyReadAt(addr) => BusCycle::Read(ReadAddress::Latch(addr)),
            // RTS/RTI/PLP pre-increment cycle: hardware reads $0100 | SP
            // and increments SP without pulling a value.
            Microcode::DummyStackRead => BusCycle::Read(ReadAddress::Stack(0x100 | self.sp as u16)),
            // (ind) high byte: low-byte increment without page carry.
            Microcode::IndexedH(_) | Microcode::IndexedHAndJump => {
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
            // there, and exec performs it via `load_alu`.
            Microcode::LoadR(ValueSource::ZeroPage, _)
            | Microcode::LoadR(ValueSource::Mem, _)
            | Microcode::LoadIntoAlu(_)
            | Microcode::IndexedL(_)
            | Microcode::Lax(_)
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
            | Microcode::Sax(_)
            | Microcode::Rla(_)
            | Microcode::Dcp(_)
            | Microcode::Isc(_)
            | Microcode::Rra(_)
            | Microcode::Slo(_)
            | Microcode::Sre(_)
            | Microcode::PushStatus { .. }
            | Microcode::PushStack(_)
            | Microcode::Asl(AOrMemory::Memory, _)
            | Microcode::Lsr(AOrMemory::Memory, _)
            | Microcode::Rol(AOrMemory::Memory, _)
            | Microcode::Ror(AOrMemory::Memory, _)
            | Microcode::IncDec(IncDecTarget::IncrementAlu | IncDecTarget::DecrementAlu, _) => {
                BusCycle::Write
            }
            // Purely internal cycles: flags, transfers, register INC/DEC,
            // the ALU shift/rotate step, JAM.
            Microcode::Asl(AOrMemory::Accumulator, _)
            | Microcode::Lsr(AOrMemory::Accumulator, _)
            | Microcode::Rol(AOrMemory::Accumulator, _)
            | Microcode::Ror(AOrMemory::Accumulator, _)
            | Microcode::IncDec(
                IncDecTarget::IncrementX
                | IncDecTarget::IncrementY
                | IncDecTarget::DecrementX
                | IncDecTarget::DecrementY,
                _,
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

        self.push_microcodes(&InterruptSequences::RESET);
        self.arm_interrupt_window(&InterruptSequences::RESET_WINDOW, None);
    }

    pub(crate) fn set_irq(&mut self, enabled: bool) {
        self.irq_detector.update_irq_input(enabled);
    }

    /// Single published interrupt entry — updates both IRQ level and NMI
    /// line atomically.
    ///
    /// `lines.irq_level` is already time-corrected (APU +1, cartridge
    /// latch quantized).
    pub fn update_interrupt_lines(
        &mut self,
        lines: crate::interrupt::InterruptLines,
        clock: SystemClock,
    ) {
        self.set_irq(lines.irq_level);
        self.update_nmi_line(lines.nmi, clock);
    }

    pub fn is_halted(&self) -> bool {
        self.halt
    }

    /// Return true if just execute current instruction
    #[hotpath::measure(impl_type = "Cpu")]
    pub fn tick<P: Plugin<M>, M: Mcu>(&mut self, bus: &mut Bus<M>, plugin: &mut P) -> TickOutcome {
        crate::hotpath_cpu::hotpath_cpu_alias!(
            Self,
            concat!(module_path!(), "::Cpu::tick"),
            "tick"
        );
        if self.frozen {
            return TickOutcome {
                control: ExecuteResult::Continue,
                instruction_complete: false,
            };
        }

        if self.is_halted() {
            return TickOutcome {
                control: ExecuteResult::Halt,
                instruction_complete: false,
            };
        }

        if let Some(active) = self.active_interrupt_window.as_mut()
            && active.started_at.is_none()
        {
            active.started_at = Some(bus.clock());
        }
        let code = match self.pop_microcode() {
            Some(v) => v,
            None => {
                let view = MachineView::new(self.snapshot(), bus.mcu(), bus.clock());
                plugin.start(&view);
                match self.interrupt_detected.take() {
                    Some(InterruptType::Nmi) => {
                        self.push_enter_interrupt_microcodes(true, bus.clock())
                    }
                    Some(InterruptType::Irq) => {
                        self.push_enter_interrupt_microcodes(false, bus.clock())
                    }
                    None => Microcode::FetchAndDecode,
                }
            }
        };

        let last_status = self.status;
        code.exec(self, bus);
        let instruction_complete = self.microcode_queue.is_empty();
        // Suppression is declared: the armed window's `suppress_final_poll`
        // owns the final-cycle poll. The queue drains exactly on the final
        // op of the armed sequence (or a hijack continuation's tail), so
        // an armed window over an empty queue is that cycle.
        let is_last_op = instruction_complete
            && !self
                .active_interrupt_window
                .is_some_and(|active| active.window.suppress_final_poll);
        if instruction_complete {
            self.active_interrupt_window = None;
        }
        self.detect_interrupt(bus.clock(), is_last_op, last_status, code);

        if instruction_complete {
            let view = MachineView::new(self.snapshot(), bus.mcu(), bus.clock());
            plugin.end(&view);
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

    fn do_detect_interrupt(&mut self, clock: SystemClock, irq_pending: bool) {
        if self.nmi_detecteor.nmi_pending
            && self
                .nmi_detecteor
                .pending_asserted_since
                .is_some_and(|t0| (clock.0 - u64::from(t0)) > 1)
        {
            self.nmi_detecteor.mark_consumed();
            self.interrupt_detected = Some(InterruptType::Nmi);
        } else if irq_pending {
            self.interrupt_detected = Some(InterruptType::Irq);
        } else {
            debug_assert!(self.interrupt_detected.is_none());
        }
    }

    fn detect_interrupt(
        &mut self,
        clock: SystemClock,
        is_last_op: bool,
        last_status: u8,
        last_op: Microcode,
    ) {
        self.nmi_detecteor.detect_nmi();
        // CLI/SEI/PLP are polled against the pre-cycle status: their own
        // write to the I flag must not enable or disable the IRQ taken at
        // their final cycle (blargg cpu_interrupts_v2 1-cli_latency).
        let writes_interrupt_disable = matches!(
            last_op,
            Microcode::ClearFlag(Flag::InterruptDisabled)
                | Microcode::SetFlag(Flag::InterruptDisabled)
                | Microcode::Plp
        );
        let disabled = if writes_interrupt_disable {
            (last_status & Flag::InterruptDisabled as u8) != 0
        } else {
            self.flag(Flag::InterruptDisabled)
        };
        let irq_pending = self.irq_detector.detect_irq(disabled);

        let request = std::mem::take(&mut self.request_detect_interrupt);
        // The standard poll fires on the instruction's final cycle —
        // suppressed on the final cycle of an interrupt sequence, whose
        // recognition belongs to its declared hijack sites.
        if request.unwrap_or(is_last_op) {
            self.do_detect_interrupt(clock, irq_pending)
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

    fn read_byte<M: Mcu>(&mut self, bus: &mut Bus<M>, addr: u16) -> u8 {
        bus.mcu_mut().read(addr)
    }

    pub fn peek_byte<M: Mcu>(&self, bus: &Bus<M>, addr: u16) -> u8 {
        bus.mcu().peek(addr)
    }

    fn read_pc_byte<M: Mcu>(&mut self, bus: &mut Bus<M>) {
        self.read_byte(bus, self.pc.get());
    }

    fn inc_read_byte<M: Mcu>(&mut self, bus: &mut Bus<M>) -> u8 {
        let addr = self.pc.get();
        self.pc.wrapping_add(1);
        self.read_byte(bus, addr)
    }

    fn write_byte<M: Mcu>(&mut self, bus: &mut Bus<M>, value: u8) {
        self.write_mem(bus, self.ab.get(), value);
    }

    /// Writes a value to the address bus latch through the addressing
    /// flavour the sequence builder chose (spec #46, ticket #49). A `$zp`
    /// sequence carries `ValueSource::ZeroPage`, so its write takes
    /// `Mcu::write_zero_page` and skips the bus address-range match; every
    /// other sequence carries `ValueSource::Mem` and writes generically.
    /// The flavour decides *how* the byte is written, never *whether* a
    /// write happens.
    fn write_value<M: Mcu>(&mut self, bus: &mut Bus<M>, src: ValueSource, value: u8) {
        match src {
            ValueSource::ZeroPage => <ZeroPage as ValueTargetTrait>::write(self, bus, value),
            ValueSource::Mem => self.write_byte(bus, value),
            ValueSource::Immediate => unreachable!("an immediate has no write target"),
        }
    }

    fn write_mem<M: Mcu>(&mut self, bus: &mut Bus<M>, addr: u16, value: u8) {
        let clock = bus.clock();
        bus.mcu_mut().write(addr, value, clock);
    }

    fn push_stack<M: Mcu>(&mut self, bus: &mut Bus<M>, value: u8) {
        let clock = bus.clock();
        bus.mcu_mut().write_stack_page(self.sp, value, clock);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn pop_stack<M: Mcu>(&mut self, bus: &mut Bus<M>) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        bus.mcu_mut().read_stack_page(self.sp)
    }

    #[cfg(test)]
    fn peek_stack<M: Mcu>(&self, bus: &Bus<M>) -> u8 {
        bus.mcu().peek(self.stack_pop_addr())
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

    /// Loads the byte at the address latch into the ALU through `S`: the
    /// zero-page fast path for `$zp` sequences, the generic path otherwise.
    fn load_alu<S: ValueSourceTrait, M: Mcu>(&mut self, bus: &mut Bus<M>) {
        self.alu = S::value(self, bus);
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

    fn adc<S: ValueSourceTrait, M: Mcu>(&mut self, bus: &mut Bus<M>) {
        let val = S::value(self, bus);
        self.do_adc(val);
    }

    fn sbc<S: ValueSourceTrait, M: Mcu>(&mut self, bus: &mut Bus<M>) {
        let val = !S::value(self, bus);
        self.do_adc(val);
    }

    fn ora<S: ValueSourceTrait, M: Mcu>(&mut self, bus: &mut Bus<M>) {
        let val = S::value(self, bus);
        self.set_register(Register::A, self.a | val);
    }

    fn eor<S: ValueSourceTrait, M: Mcu>(&mut self, bus: &mut Bus<M>) {
        let val = S::value(self, bus);
        self.set_register(Register::A, self.a ^ val);
    }

    fn compare(&mut self, lhs: u8, rhs: u8) {
        let t = lhs.wrapping_sub(rhs);
        self.update_zero_negative_flags(t);
        self.set_flag(Flag::Carry, lhs >= rhs);
    }

    fn cmp<S: ValueSourceTrait, M: Mcu>(&mut self, bus: &mut Bus<M>) {
        let val = S::value(self, bus);
        self.compare(self.a, val);
    }

    fn cpx<S: ValueSourceTrait, M: Mcu>(&mut self, bus: &mut Bus<M>) {
        let val = S::value(self, bus);
        self.compare(self.x, val);
    }

    fn cpy<S: ValueSourceTrait, M: Mcu>(&mut self, bus: &mut Bus<M>) {
        let val = S::value(self, bus);
        self.compare(self.y, val);
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

    /// Loads the operand through `S` and copies it to A and X. `S` is the
    /// read route the sequence builder chose (`ZeroPage` for `$zp` modes,
    /// `Mem` for absolute and pointer targets), so the zero-page decision is
    /// a monomorphised trait impl, not a run-time test (spec #46, ticket
    /// #50).
    fn lax<S: ValueSourceTrait, M: Mcu>(&mut self, bus: &mut Bus<M>) {
        self.alu = S::value(self, bus);
        self.set_register(Register::A, self.alu);
        self.set_register(Register::X, self.alu);
    }

    // The undocumented RMW and SAX helpers below take the addressing
    // flavour their sequence was built with and route the result through
    // `Cpu::write_value` (spec #46, ticket #49), exactly as the documented
    // RMW family does through `Microcode::write_alu`.
    fn sax<M: Mcu>(&mut self, bus: &mut Bus<M>, src: ValueSource) {
        self.write_value(bus, src, self.a & self.x);
    }

    fn dcp<M: Mcu>(&mut self, bus: &mut Bus<M>, src: ValueSource) {
        let v = self.alu.wrapping_sub(1);
        self.write_value(bus, src, v);
        self.update_zero_negative_flags(self.a.wrapping_sub(v));
        self.set_flag(Flag::Carry, self.a >= v);
    }

    fn isc<M: Mcu>(&mut self, bus: &mut Bus<M>, src: ValueSource) {
        let v = self.alu.wrapping_add(1);
        self.write_value(bus, src, v);
        self.alu = v;
        self.sbc::<Alu, _>(bus);
    }

    fn rra<M: Mcu>(&mut self, bus: &mut Bus<M>, src: ValueSource) {
        let carry = self.alu & 0x01 != 0;
        self.alu = (self.alu >> 1) | ((self.flag(Flag::Carry) as u8) << 7);
        self.write_value(bus, src, self.alu);
        self.set_flag(Flag::Carry, carry);
        self.adc::<Alu, _>(bus);
    }

    fn rla<M: Mcu>(&mut self, bus: &mut Bus<M>, src: ValueSource) {
        let new = (self.alu << 1) | (self.flag(Flag::Carry) as u8);
        self.set_flag(Flag::Carry, self.alu & 0x80 != 0);
        self.alu = new;
        self.write_value(bus, src, self.alu);
        self.and::<Alu, _>(bus);
    }

    fn slo<M: Mcu>(&mut self, bus: &mut Bus<M>, src: ValueSource) {
        self.set_flag(Flag::Carry, self.alu & 0x80 != 0);
        self.alu <<= 1;
        self.write_value(bus, src, self.alu);
        self.ora::<Alu, _>(bus);
    }

    fn sre<M: Mcu>(&mut self, bus: &mut Bus<M>, src: ValueSource) {
        self.set_flag(Flag::Carry, self.alu & 0x01 != 0);
        self.alu >>= 1;
        self.write_value(bus, src, self.alu);
        self.eor::<Alu, _>(bus);
    }

    /// Store `value & (ABH + 1)` at the address latch — the high-byte
    /// corruption shared by SHX, SHY, SHA and TAS.
    fn store_high_byte<M: Mcu>(&mut self, bus: &mut Bus<M>, value: u8) {
        let out = value & self.abh().wrapping_add(1);
        self.ab.set_high(out);
        self.write_byte(bus, out);
    }

    fn shx<M: Mcu>(&mut self, bus: &mut Bus<M>) {
        self.store_high_byte(bus, self.x);
    }

    fn shy<M: Mcu>(&mut self, bus: &mut Bus<M>) {
        self.store_high_byte(bus, self.y);
    }

    fn sha<M: Mcu>(&mut self, bus: &mut Bus<M>) {
        // SHA (AHX/AXA): store A & X & (high-byte of addr + 1) at address
        self.store_high_byte(bus, self.a & self.x);
    }

    fn tas<M: Mcu>(&mut self, bus: &mut Bus<M>) {
        let v = self.a & self.x;
        self.sp = v;
        self.store_high_byte(bus, v);
    }

    fn push_status<M: Mcu>(&mut self, bus: &mut Bus<M>, break_flag: bool) {
        let clock = bus.clock();
        if let Some((_, cutoff)) = self.declared_hijack_site(HijackEligibility::LatchedOnly, clock)
            && self.nmi_detecteor.consume_latched_edge(cutoff)
        {
            self.switch_to_nmi_vector(&[Microcode::LoadNmiPcL, Microcode::LoadNmiPcH]);
            self.push_status(bus, break_flag);
            return;
        }

        self.push_stack(
            bus,
            if break_flag {
                self.status | Flag::Break as u8
            } else {
                self.status
            },
        );
    }

    fn plp<M: Mcu>(&mut self, bus: &mut Bus<M>) {
        let break_flag = self.flag(Flag::Break);
        self.status = self.pop_stack(bus);
        self.set_flag(Flag::Break, break_flag);
        self.set_flag(Flag::NotUsed, true);
    }

    /// Zero-page index add cycle: hardware re-reads the unindexed base
    /// address on the bus (RAM only, so side-effect-free) and latches the
    /// wrapped indexed address.
    fn zero_page_indexed<M: Mcu>(&mut self, bus: &mut Bus<M>, index: u8) {
        let _ = bus.mcu_mut().read_zero_page(self.ab.low());
        self.ab.set(self.abl().wrapping_add(index) as u16);
    }

    fn set_pc_to_ab(&mut self) {
        self.pc.set(self.ab.get());
    }

    fn and<S: ValueSourceTrait, M: Mcu>(&mut self, bus: &mut Bus<M>) {
        let v = S::value(self, bus);
        self.set_register(Register::A, self.a & v);
    }

    #[inline(always)]
    fn bit<S: ValueSourceTrait, M: Mcu>(&mut self, bus: &mut Bus<M>) {
        let v = S::value(self, bus);
        self.set_flag(Flag::Overflow, v & 0x40 != 0);
        self.update_negative_flag(v);
        self.update_zero_flag(self.a & v);
    }

    fn push_microcodes(&mut self, microcodes: &[Microcode]) {
        self.microcode_queue.extend_back(microcodes.iter().copied())
    }

    /// Arm the recognition window of an interrupt sequence about to run.
    /// With `Some(clock)` the sequence's first micro-op executes on the
    /// current tick (the dispatch head); `None` stamps the start on the
    /// first pop after arming (RESET, BRK after its fetch, hijack
    /// continuations).
    fn arm_interrupt_window(
        &mut self,
        window: &'static InterruptWindow,
        started_at: Option<SystemClock>,
    ) {
        self.active_interrupt_window = Some(ActiveInterruptWindow { window, started_at });
    }

    /// The declared hijack site for the micro-op executing this cycle, if
    /// the armed window schedules one at this sequence index, with its
    /// cutoff in dots. `expected` pins the caller's op kind to the
    /// declared rule (PushStatus = LatchedOnly, LoadIrqPcL = NewestVisible).
    fn declared_hijack_site(
        &self,
        expected: HijackEligibility,
        clock: SystemClock,
    ) -> Option<(&'static HijackSite, u64)> {
        let active = self.active_interrupt_window.as_ref()?;
        let started = active.started_at?;
        let index = (clock.cycles() - started.cycles()) / 3;
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
        self.arm_interrupt_window(&InterruptSequences::NMI_WINDOW, None);
        self.microcode_queue.clear();
        self.push_microcodes(tail);
    }

    /// Push the NMI/IRQ microcode sequence and return its head for
    /// execution on the current cycle. The sequence owns both hardware
    /// dead cycles (T1/T2) as its first two ops, so interrupt entry is
    /// exactly seven CPU cycles from dispatch to vector fetch.
    fn push_enter_interrupt_microcodes(&mut self, nmi: bool, clock: SystemClock) -> Microcode {
        let (sequence, window) = if nmi {
            (&InterruptSequences::NMI, &InterruptSequences::NMI_WINDOW)
        } else {
            (&InterruptSequences::IRQ, &InterruptSequences::IRQ_WINDOW)
        };
        // The head (T1) runs on this cycle — the sequence starts now; the
        // rest drains from the queue on the following ticks.
        self.arm_interrupt_window(window, Some(clock));
        self.push_microcodes(&sequence[1..]);
        sequence[0]
    }

    fn pop_microcode(&mut self) -> Option<Microcode> {
        self.microcode_queue.pop_front()
    }
    fn push_microcode(&mut self, microcode: Microcode) {
        if self.microcode_queue.push_front(microcode).is_err() {
            debug_assert!(
                false,
                "Microcode queue overflow, maybe some microcode is too long?"
            );
        }
    }

    fn retain_cycle(&mut self) {
        self.push_microcode(Microcode::Nop);
    }

    /// Update cpu nmi signal line, may trigger nmi
    pub(crate) fn update_nmi_line(&mut self, nmi: bool, clock: SystemClock) {
        self.nmi_detecteor.update_nmi_input(nmi, clock);
    }

    fn load_vector_low<M: Mcu>(&mut self, bus: &mut Bus<M>, address: u16, disable_interrupt: bool) {
        if disable_interrupt {
            self.set_flag(Flag::InterruptDisabled, true);
        }
        let low = self.read_byte(bus, address);
        self.pc.set_low(low);
    }

    fn load_vector_high<M: Mcu>(&mut self, bus: &mut Bus<M>, address: u16) {
        let high = self.read_byte(bus, address);
        self.pc.set_high(high);
    }

    fn load_nmi_pcl<M: Mcu>(&mut self, bus: &mut Bus<M>) {
        self.load_vector_low(bus, 0xFFFA, true);
    }

    fn load_nmi_pch<M: Mcu>(&mut self, bus: &mut Bus<M>) {
        self.load_vector_high(bus, 0xFFFB);
    }

    fn load_irq_pcl<M: Mcu>(&mut self, bus: &mut Bus<M>) {
        let clock = bus.clock();
        if let Some((_, cutoff)) =
            self.declared_hijack_site(HijackEligibility::NewestVisible, clock)
            && self.nmi_detecteor.consume_hijack_edge(cutoff)
        {
            self.switch_to_nmi_vector(&[Microcode::LoadNmiPcH]);
            self.load_nmi_pcl(bus);
            return;
        }

        self.load_vector_low(bus, 0xFFFE, true);
    }

    fn load_irq_pch<M: Mcu>(&mut self, bus: &mut Bus<M>) {
        self.load_vector_high(bus, 0xFFFF);
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

    /// Reads the pointer's low byte through `S`: the zero-page fast path
    /// for the `(zp,X)`/`(zp),Y` modes, the generic path for `JMP ($xxxx)`.
    fn indexed_l<S: ValueSourceTrait, M: Mcu>(&mut self, bus: &mut Bus<M>) {
        self.db = S::value(self, bus);
    }

    /// Reads the pointer's high byte at the low-byte-incremented (no
    /// carry) address, through `S`: the zero-page fast path for the
    /// `(zp,X)`/`(zp),Y` modes, the generic path for `JMP ($xxxx)`.
    fn indexed_h<S: ValueSourceTrait, M: Mcu>(&mut self, bus: &mut Bus<M>) {
        self.ab.wrapping_inc_low();
        let high = S::value(self, bus);
        self.ab.set(self.db as u16 | ((high as u16) << 8));
    }
}
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
    /// The current dot is read through `view.clock()`.
    fn start(&mut self, view: &MachineView<M>);

    /// After execute instruction — receives the same view.
    fn end(&mut self, view: &MachineView<M>);

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
    fn start(&mut self, _: &MachineView<M>) {}

    fn end(&mut self, _: &MachineView<M>) {}
}

impl<M: Mcu> Default for EmptyPlugin<M> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Flag {
    Carry = 0x01,
    Zero = 0x02,
    InterruptDisabled = 0x04,
    Decimal = 0x08,
    Break = 0x10,
    NotUsed = 0x20,
    Overflow = 0x40,
    Negative = 0x80,
}

#[cfg(test)]
mod routing_tests;
#[cfg(test)]
mod tests;
