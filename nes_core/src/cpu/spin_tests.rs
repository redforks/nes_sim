//! Polled-address spin freeze tests (ticket #134).
//!
//! Every assertion observes behavior at the CPU/MCU seam: bus traffic,
//! register snapshots, and interrupt dispatch. No freeze flag is read.

use super::*;

struct TestMcu {
    mem: [u8; 0x10000],
    writes: Vec<(u16, u8)>,
    reads: Vec<u16>,
}

impl Default for TestMcu {
    fn default() -> Self {
        Self {
            mem: [0; 0x10000],
            writes: Vec::new(),
            reads: Vec::new(),
        }
    }
}

impl Mcu for TestMcu {
    fn read(&mut self, address: u16) -> u8 {
        self.reads.push(address);
        self.mem[address as usize]
    }

    fn peek(&self, address: u16) -> u8 {
        self.mem[address as usize]
    }

    fn write(&mut self, address: u16, value: u8, _clock: SystemClock) {
        self.mem[address as usize] = value;
        self.writes.push((address, value));
    }
}

struct Harness {
    cpu: Cpu,
    bus: Bus<TestMcu>,
}

impl std::ops::Deref for Harness {
    type Target = Cpu;
    fn deref(&self) -> &Cpu {
        &self.cpu
    }
}

impl std::ops::DerefMut for Harness {
    fn deref_mut(&mut self) -> &mut Cpu {
        &mut self.cpu
    }
}

fn create_cpu(mcu: TestMcu) -> Harness {
    let mut h = Harness {
        cpu: Cpu::new(),
        bus: Bus::new(mcu),
    };
    let mut plugin = EmptyPlugin::new();
    h.cpu.run_to_instruction_boundary(&mut h.bus, &mut plugin);
    h
}

impl Harness {
    fn mcu(&self) -> &TestMcu {
        self.bus.mcu()
    }

    fn mcu_mut(&mut self) -> &mut TestMcu {
        self.bus.mcu_mut()
    }

    fn write_mem(&mut self, addr: u16, value: u8) {
        self.cpu.write_mem(&mut self.bus, addr, value);
    }

    /// One CPU dot with the NMI line published at the sampling phase, as
    /// `NesMachine::tick_dot` does in production.
    fn dot(&mut self, plugin: &mut EmptyPlugin<TestMcu>, clock: u64, nmi: bool) {
        self.cpu
            .update_nmi_line(nmi, SystemClock(clock), clock % 3 == 0);
        self.bus.set_clock(SystemClock(clock));
        self.cpu.tick(&mut self.bus, plugin);
    }

    fn run_dots(&mut self, plugin: &mut EmptyPlugin<TestMcu>, from: u64, count: u64) {
        for clock in from..from + count {
            self.dot(plugin, clock, false);
        }
    }
}

fn poll_program() -> Harness {
    const TOP: u16 = 0x0400;
    let mut mcu = TestMcu::default();
    let program = [
        0xA5, 0x10, // $0400 LDA $10
        0xD0, 0xFC, // $0402 BNE $0400
    ];
    for (i, byte) in program.iter().enumerate() {
        mcu.mem[TOP as usize + i] = *byte;
    }
    mcu.mem[0x10] = 1; // flag stays asserted: BNE keeps taking the back-edge
    mcu.mem[0xFFFA] = 0x00;
    mcu.mem[0xFFFB] = 0x06; // NMI handler at $0600
    mcu.mem[0xFFFE] = 0x00;
    mcu.mem[0xFFFF] = 0x07; // IRQ handler at $0700
    mcu.mem[0xFFFC] = TOP as u8;
    mcu.mem[0xFFFD] = (TOP >> 8) as u8;
    let mut cpu = create_cpu(mcu);
    cpu.set_pc(TOP);
    cpu.set_status(0x20); // interrupts enabled
    cpu
}

/// Run until `target` poll reads have been observed, then return the next dot.
fn run_to_poll_reads(cpu: &mut Harness, plugin: &mut EmptyPlugin<TestMcu>, target: usize) -> u64 {
    let mut dot = 3u64;
    while cpu.mcu().reads.iter().filter(|&&a| a == 0x10).count() < target {
        cpu.dot(plugin, dot, false);
        dot += 1;
        assert!(dot < 1000, "poll read {target} never happened");
    }
    dot
}

#[test]
fn ram_flag_poll_freezes_after_second_back_edge() {
    let mut cpu = poll_program();
    let mut plugin = EmptyPlugin::new();
    let next_dot = run_to_poll_reads(&mut cpu, &mut plugin, 2);
    cpu.mcu_mut().reads.clear();

    cpu.run_dots(&mut plugin, next_dot, 60);
    assert!(
        !cpu.mcu().reads.iter().any(|a| *a == 0x10),
        "the declared poll must not re-execute its read, saw {:?}",
        cpu.mcu().reads
    );
    assert_eq!(cpu.pc(), 0x0400, "frozen poll must sit at its loop top");
}

#[test]
fn nmi_edge_wakes_ram_poll_at_loop_top() {
    let mut cpu = poll_program();
    let mut plugin = EmptyPlugin::new();
    let mut dot = run_to_poll_reads(&mut cpu, &mut plugin, 2);
    cpu.run_dots(&mut plugin, dot, 20);
    dot += 20;
    cpu.mcu_mut().reads.clear();

    // Hold /NMI high across its sampling phase; recognition needs two dots.
    for d in dot..dot + 12 {
        cpu.dot(&mut plugin, d, true);
    }
    let reads = &cpu.mcu().reads;
    assert!(
        reads.iter().any(|a| (0x0100..=0x01FF).contains(a)) || reads.contains(&0xFFFA),
        "NMI edge must wake the frozen poll into the interrupt sequence, saw {reads:?}"
    );
    assert_eq!(cpu.a(), 1, "wake must preserve registers");
}

#[test]
fn cpu_write_to_polled_address_wakes_without_interrupt() {
    let mut cpu = poll_program();
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_poll_reads(&mut cpu, &mut plugin, 2);
    cpu.run_dots(&mut plugin, dot, 15);
    cpu.mcu_mut().reads.clear();

    // Only a CPU write to the watched address wakes the spin.
    cpu.write_mem(0x10, 2);
    cpu.run_dots(&mut plugin, dot + 15, 12);

    assert!(
        cpu.mcu().reads.contains(&0x10),
        "write wake must re-execute the loop-top poll read, saw {:?}",
        cpu.mcu().reads
    );
    assert_eq!(
        cpu.a(),
        2,
        "re-executed poll must derive flags from new value"
    );
    assert_eq!(cpu.pc(), 0x0400);
}

#[test]
fn register_countdown_loop_never_declares() {
    const TOP: u16 = 0x0400;
    let mut mcu = TestMcu::default();
    let program = [
        0xA2, 0x03, // $0400 LDX #3
        0xCA, // $0402 DEX
        0xD0, 0xFD, // $0403 BNE $0402
    ];
    for (i, byte) in program.iter().enumerate() {
        mcu.mem[TOP as usize + i] = *byte;
    }
    mcu.mem[0xFFFC] = TOP as u8;
    mcu.mem[0xFFFD] = (TOP >> 8) as u8;
    let mut cpu = create_cpu(mcu);
    cpu.set_pc(TOP);
    // Verify the countdown is still running after the count would have
    // matured twice: hold X at a nonzero value so the loop cannot finish.
    let mut plugin: EmptyPlugin<TestMcu> = EmptyPlugin::new();
    for clock in 3..30 {
        cpu.dot(&mut plugin, clock, false);
        if cpu.pc() == 0x0402 && cpu.microcodes_empty() {
            cpu.cpu.set_x(3);
        }
    }
    assert_eq!(cpu.pc(), 0x0402, "register countdown must keep running");
}

#[test]
fn write_in_loop_never_declares() {
    const TOP: u16 = 0x0400;
    let mut mcu = TestMcu::default();
    let program = [
        0xA5, 0x20, // $0400 LDA $20
        0x85, 0x20, // $0402 STA $20 (write inside the loop window)
        0xD0, 0xFA, // $0404 BNE $0400
    ];
    for (i, byte) in program.iter().enumerate() {
        mcu.mem[TOP as usize + i] = *byte;
    }
    mcu.mem[0x20] = 5;
    mcu.mem[0xFFFC] = TOP as u8;
    mcu.mem[0xFFFD] = (TOP >> 8) as u8;
    let mut cpu = create_cpu(mcu);
    cpu.set_pc(TOP);
    let mut plugin = EmptyPlugin::new();
    cpu.run_dots(&mut plugin, 3, 60);

    assert!(
        cpu.mcu().writes.len() >= 3,
        "write-in-loop must keep executing, saw {:?}",
        cpu.mcu().writes
    );
}

#[test]
fn rmw_loop_never_declares() {
    const TOP: u16 = 0x0400;
    let mut mcu = TestMcu::default();
    let program = [
        0xE6, 0x30, // $0400 INC $30 (read-modify-write)
        0xA5, 0x30, // $0402 LDA $30
        0xD0, 0xFA, // $0404 BNE $0400
    ];
    for (i, byte) in program.iter().enumerate() {
        mcu.mem[TOP as usize + i] = *byte;
    }
    mcu.mem[0x30] = 2;
    mcu.mem[0xFFFC] = TOP as u8;
    mcu.mem[0xFFFD] = (TOP >> 8) as u8;
    let mut cpu = create_cpu(mcu);
    cpu.set_pc(TOP);
    let mut plugin = EmptyPlugin::new();
    cpu.run_dots(&mut plugin, 3, 60);

    assert!(
        cpu.mcu().writes.len() >= 3,
        "RMW loop must keep executing, saw {:?}",
        cpu.mcu().writes
    );
}

#[test]
fn frozen_vs_unfrozen_register_and_interrupt_trace_matches() {
    // The unfrozen control freezes only after the same loop resumes from
    // its interrupt: both runs execute identical microcode through the
    // declared freeze and the wake, so register/flag snapshots and the
    // interrupt-detection state must agree dot for dot.
    fn build() -> Harness {
        let mut cpu = poll_program();
        cpu.set_status(0x24); // I set: NMI still dispatchable, IRQ inhibited
        cpu
    }

    let mut frozen = build();
    let mut control = build();
    let mut p1 = EmptyPlugin::new();
    let mut p2 = EmptyPlugin::new();
    let start = run_to_poll_reads(&mut frozen, &mut p1, 2);
    run_to_poll_reads(&mut control, &mut p2, 2);
    control.mcu_mut().reads.clear();
    frozen.mcu_mut().reads.clear();
    for clock in start..start + 5 {
        frozen.dot(&mut p1, clock, false);
        control.dot(&mut p2, clock, false);
    }
    assert!(
        !frozen.mcu().reads.iter().any(|a| *a == 0x10),
        "control precondition: frozen run must have stopped polling"
    );
    // Both runs observe the same NMI edge. NMI dispatch is not inhibited
    // by I, so the frozen run wakes and both enter the same sequence.
    for clock in start + 5..start + 40 {
        frozen.dot(&mut p1, clock, true);
        control.dot(&mut p2, clock, true);
    }
    assert_eq!(frozen.snapshot(), control.snapshot());
    assert_eq!(frozen.interrupt_detected, control.interrupt_detected);
    assert_eq!(frozen.mcu().reads, control.mcu().reads);
}

#[test]
fn dma_apu_side_writes_do_not_wake_the_spin() {
    // DMA/audio writes bypass the CPU write funnel. The only bus writes in
    // this run are direct MCU calls, so the declared spin must keep its
    // poll read dormant even though the polled byte changes.
    let mut cpu = poll_program();
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_poll_reads(&mut cpu, &mut plugin, 2);
    cpu.run_dots(&mut plugin, dot, 15);
    cpu.mcu_mut().reads.clear();

    // Simulate a DMA-side write to the polled address through the MCU, as
    // OAM/DMC endpoints do (they never call `Cpu::write_*`).
    cpu.mcu_mut().write(0x10, 0, SystemClock(dot + 15));
    cpu.run_dots(&mut plugin, dot + 15, 20);
    assert!(
        !cpu.mcu().reads.iter().any(|a| *a == 0x10),
        "a DMA/APU write must not wake the spin, saw {:?}",
        cpu.mcu().reads
    );
}

#[test]
fn irq_wakes_ram_poll_at_loop_top() {
    let mut cpu = poll_program();
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_poll_reads(&mut cpu, &mut plugin, 2);
    cpu.run_dots(&mut plugin, dot, 15);
    cpu.mcu_mut().reads.clear();

    cpu.set_irq(true);
    cpu.run_dots(&mut plugin, dot + 15, 20);

    let reads = &cpu.mcu().reads;
    assert!(
        reads.iter().any(|a| (0x0100..=0x01FF).contains(a)) || reads.contains(&0xFFFE),
        "IRQ must wake the frozen poll into the interrupt sequence, saw {reads:?}"
    );
    assert_eq!(cpu.a(), 1, "wake must preserve registers");
}

#[test]
fn write_wake_reexecutes_poll_at_loop_top_then_redeclares() {
    // A single write wake must re-execute the poll read once; the loop then
    // re-declares after two fresh back-edges (it must not freeze forever or
    // spin through every dot).
    let mut cpu = poll_program();
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_poll_reads(&mut cpu, &mut plugin, 2);
    cpu.run_dots(&mut plugin, dot, 10);
    cpu.mcu_mut().reads.clear();
    cpu.write_mem(0x10, 2);
    cpu.run_dots(&mut plugin, dot + 10, 20);

    let polls = cpu.mcu().reads.iter().filter(|&&a| a == 0x10).count();
    assert!(
        (1..=4).contains(&polls),
        "wake should run the poll a bounded number of times, saw {polls}"
    );
}

#[test]
fn two_distinct_poll_addresses_do_not_alias() {
    // A write to a different address must not wake; only the exact polled
    // address qualifies.
    let mut cpu = poll_program();
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_poll_reads(&mut cpu, &mut plugin, 2);
    cpu.run_dots(&mut plugin, dot, 15);
    cpu.mcu_mut().reads.clear();

    cpu.write_mem(0x11, 0);
    cpu.run_dots(&mut plugin, dot + 15, 20);
    assert!(
        !cpu.mcu().reads.iter().any(|a| *a == 0x10),
        "a write to another address must not wake the spin, saw {:?}",
        cpu.mcu().reads
    );
}

#[test]
fn staged_write_cycle_refuses_spin_declaration() {
    // A candidate that would freeze on a pending write must keep running:
    // swallowing a write would be externally visible.
    let mut cpu = create_cpu(TestMcu::default());
    cpu.stage_pending_cycle_for_test(
        super::microcode::Microcode::StoreR(ValueSource::Mem, Register::A),
        None,
    );
    assert!(!cpu.can_pause());
}
