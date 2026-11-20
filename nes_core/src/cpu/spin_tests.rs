//! Polled-address spin freeze tests (ticket #134).
//!
//! Every assertion observes behavior at the CPU/MCU seam: bus traffic,
//! register snapshots, and interrupt dispatch. No freeze source is inspected.

use super::*;
use crate::wait_mcu::WaitMcu as TestMcu;

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
            .update_nmi_line(nmi, SystemClock(clock), clock.is_multiple_of(3));
        self.bus.set_clock(SystemClock(clock));
        self.cpu.tick(&mut self.bus, plugin);
    }

    fn run_dots(&mut self, plugin: &mut EmptyPlugin<TestMcu>, from: u64, count: u64) {
        for clock in from..from + count {
            self.dot(plugin, clock, false);
        }
    }
}

/// Run a loop until `target` loop-top reads have occurred, then clear
/// recorded bus traffic and return the next dot.
fn run_to_device_poll_reads(
    cpu: &mut Harness,
    plugin: &mut EmptyPlugin<TestMcu>,
    addr: u16,
    replay_index: usize,
) -> u64 {
    let mut dot = 3u64;
    while cpu.mcu().reads.iter().filter(|&&a| a == addr).count() < 2
        || cpu.mcu().device_replays[replay_index] < 3
    {
        cpu.dot(plugin, dot, false);
        dot += 1;
        assert!(dot < 1000, "poll of {addr:#06x} never froze");
    }
    cpu.mcu_mut().reads.clear();
    dot
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
        !cpu.mcu().reads.contains(&0x10),
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

/// A loop that polls a device register declares a spin like any other
/// poll. Its read is still an action (a controller shift, an APU interrupt
/// acknowledge, a zapper sample), so the freeze replays that action at the
/// observed loop cadence while the real read stays dormant. Device-sourced
/// writes never wake such a spin by decision.
fn device_poll_program(addr: u16) -> Harness {
    const TOP: u16 = 0x0400;
    let mut mcu = TestMcu::default();
    let program = [
        0xAD,
        addr as u8,
        (addr >> 8) as u8, // $0400 LDA $addr
        0xD0,
        0xFB, // $0403 BNE $0400
    ];
    for (i, byte) in program.iter().enumerate() {
        mcu.mem[TOP as usize + i] = *byte;
    }
    mcu.mem[addr as usize] = 1;
    mcu.mem[0xFFFA] = 0x00;
    mcu.mem[0xFFFB] = 0x06;
    mcu.mem[0xFFFC] = TOP as u8;
    mcu.mem[0xFFFD] = (TOP >> 8) as u8;
    mcu.mem[0xFFFE] = 0x00;
    mcu.mem[0xFFFF] = 0x07;
    let mut cpu = create_cpu(mcu);
    cpu.set_pc(TOP);
    cpu.set_status(0x20); // interrupts enabled
    cpu
}

#[test]
fn direct_write_wake_needs_two_fresh_back_edges_before_refreeze() {
    let mut cpu = poll_program();
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_poll_reads(&mut cpu, &mut plugin, 2);
    cpu.run_dots(&mut plugin, dot, 15);
    assert_eq!(
        cpu.spin_loop_top_for_test(),
        Some(0x0400),
        "precondition: the poll must be pinned at its loop top"
    );
    cpu.mcu_mut().reads.clear();

    cpu.write_mem(0x10, 2);
    cpu.run_dots(&mut plugin, dot + 15, 40);

    let polls = cpu.mcu().reads.iter().filter(|&&addr| addr == 0x10).count();
    assert_eq!(
        polls, 2,
        "a woken spin must see two fresh back-edges before re-freezing, saw {polls} polls"
    );
    assert_eq!(cpu.a(), 2, "the re-executed poll must read the new value");
}

#[test]
fn pending_cpu_write_to_polled_address_releases_frozen_spin() {
    let mut cpu = poll_program();
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_poll_reads(&mut cpu, &mut plugin, 2);
    cpu.run_dots(&mut plugin, dot, 15);
    assert_eq!(
        cpu.spin_loop_top_for_test(),
        Some(0x0400),
        "precondition: the poll must be pinned at its loop top"
    );

    // The queued store is a CPU write to the exact polled address. Frozen
    // ticks must let it execute for real instead of swallowing it.
    cpu.set_a(7);
    cpu.stage_pending_cycle_for_test(Microcode::StoreR(ValueSource::Mem, Register::A), Some(0x10));
    // The frozen dot detects the pending store and releases the spin; the
    // store itself executes on the next dot.
    cpu.dot(&mut plugin, dot + 15, false);
    assert!(
        cpu.spin_loop_top_for_test().is_none(),
        "the matching store must release the spin"
    );
    cpu.dot(&mut plugin, dot + 16, false);

    assert!(
        cpu.mcu().writes.contains(&(0x10, 7)),
        "the pending CPU store must reach the MCU, saw {:?}",
        cpu.mcu().writes
    );
    cpu.run_dots(&mut plugin, dot + 17, 40);
    assert!(
        cpu.mcu().reads.contains(&0x10),
        "the poll must resume after the write, saw {:?}",
        cpu.mcu().reads
    );
}

#[test]
fn dma_halt_composes_with_declared_spin_without_clearing_it() {
    let mut cpu = poll_program();
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_poll_reads(&mut cpu, &mut plugin, 2);
    cpu.run_dots(&mut plugin, dot, 10);
    assert_eq!(
        cpu.spin_loop_top_for_test(),
        Some(0x0400),
        "precondition: the poll must be pinned at its loop top"
    );

    // A DMC halt lands while the spin is frozen. It must compose, not panic
    // or replace the spin source.
    assert!(cpu.try_freeze(), "DMA must still be able to halt the CPU");
    assert_eq!(
        cpu.spin_loop_top_for_test(),
        Some(0x0400),
        "the DMA halt must compose with the spin instead of replacing it"
    );
    assert!(
        cpu.dma_halt_bus_addr().is_none(),
        "a declared spin sits on the branch's internal fixup cycle; DMA repeats the next fetch address"
    );

    // The DMA read completes and releases only its own source.
    cpu.unfreeze();
    cpu.mcu_mut().reads.clear();
    cpu.run_dots(&mut plugin, dot + 10, 12);
    assert_eq!(
        cpu.spin_loop_top_for_test(),
        Some(0x0400),
        "DMA unfreeze must not wake the spin"
    );
    assert!(
        !cpu.mcu().reads.contains(&0x10),
        "the spin must keep its poll dormant after DMA completes, saw {:?}",
        cpu.mcu().reads
    );
}

#[test]
fn device_register_polls_freeze_and_replay_effects_without_bus_reads() {
    for (addr, index) in [(0x4015, 0), (0x4016, 1), (0x4017, 2)] {
        let mut cpu = device_poll_program(addr);
        let mut plugin = EmptyPlugin::new();
        let dot = run_to_device_poll_reads(&mut cpu, &mut plugin, addr, index);
        let effects_before = cpu.mcu().device_replays[index];

        cpu.run_dots(&mut plugin, dot, 100);

        assert_eq!(
            cpu.mcu().reads.iter().filter(|&&a| a == addr).count(),
            0,
            "a frozen {addr:#06x} poll must not perform real bus reads"
        );
        let replayed = cpu.mcu().device_replays[index] - effects_before;
        assert!(
            replayed >= 5,
            "a frozen {addr:#06x} poll must replay its device effect at loop cadence, saw {replayed}"
        );
        assert!(
            replayed < 100,
            "replays must be cadence-based, not once per dot"
        );
    }
}

#[test]
fn frozen_device_poll_replays_apu_irq_acknowledgement() {
    let mut cpu = device_poll_program(0x4015);
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_device_poll_reads(&mut cpu, &mut plugin, 0x4015, 0);
    cpu.mcu_mut().apu_irq_pending = true;

    cpu.run_dots(&mut plugin, dot, 30);

    assert!(
        !cpu.mcu().apu_irq_pending,
        "replayed $4015 reads must acknowledge the frame IRQ while frozen"
    );
    assert!(
        cpu.mcu().replays >= 2,
        "the IRQ acknowledgement must come from replayed reads, saw {} replays",
        cpu.mcu().replays
    );
    assert!(
        !cpu.mcu().reads.contains(&0x4015),
        "the IRQ clear must not be a real bus read"
    );
}

#[test]
fn frozen_controller_poll_replays_shift_without_advancing_the_cpu() {
    let mut cpu = device_poll_program(0x4016);
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_device_poll_reads(&mut cpu, &mut plugin, 0x4016, 1);
    let shifts_before = cpu.mcu().controller_shifts[0];
    let pc = cpu.pc();
    let a = cpu.a();

    cpu.run_dots(&mut plugin, dot, 30);

    assert!(
        cpu.mcu().controller_shifts[0] > shifts_before + 2,
        "replayed $4016 reads must keep shifting the controller"
    );
    assert!(
        !cpu.mcu().reads.contains(&0x4016),
        "replays must not be visible on the CPU bus"
    );
    assert_eq!(cpu.pc(), pc, "replay must not advance the frozen CPU");
    assert_eq!(cpu.a(), a, "no synthetic value may reach the program");
}

/// A loop that writes its polled address every iteration never declares a
/// spin: the write disqualifies the candidate. This is the `oam_stress`
/// `LDA $2004 / STA $2004 / BNE` shape on plain RAM — an absolute store must
/// travel the same write-watch funnel as every other CPU write.
fn read_write_program() -> Harness {
    const TOP: u16 = 0x0400;
    let mut mcu = TestMcu::default();
    let program = [
        0xAD, 0x10, 0x00, // $0400 LDA $0010
        0x8D, 0x10, 0x00, // $0403 STA $0010
        0xD0, 0xF8, // $0406 BNE $0400
    ];
    for (i, byte) in program.iter().enumerate() {
        mcu.mem[TOP as usize + i] = *byte;
    }
    mcu.mem[0x10] = 1; // keeps BNE taken, like OAMDATA echoing the value
    mcu.mem[0xFFFC] = TOP as u8;
    mcu.mem[0xFFFD] = (TOP >> 8) as u8;
    let mut cpu = create_cpu(mcu);
    cpu.set_pc(TOP);
    cpu
}

#[test]
fn absolute_store_in_loop_disqualifies_the_poll_candidate() {
    let mut cpu = read_write_program();
    let mut plugin = EmptyPlugin::new();
    // Well past the two back-edges that would declare a read-only poll.
    let mut reads_at = None;
    for dot in 3..3 + 120 {
        cpu.mcu_mut().reads.clear();
        cpu.dot(&mut plugin, dot, false);
        if reads_at.is_none() && cpu.mcu().reads.contains(&0x10) {
            reads_at = Some(dot);
        }
    }
    // A real loop must keep re-executing its reads; a spin would pin its
    // PC and stop producing them after the second back-edge.
    let stores = cpu.mcu().writes.iter().filter(|&&(a, _)| a == 0x10).count();
    assert!(reads_at.is_some(), "the loop must keep reading its flag");
    assert_eq!(
        cpu.spin_loop_top_for_test(),
        None,
        "a write-in-loop must never declare a spin"
    );
    assert!(
        stores >= 5,
        "the loop must keep executing its store, saw {stores} writes to $10"
    );
}

#[test]
fn zapper_register_poll_declares_a_spin() {
    let mut cpu = device_poll_program(0x4017);
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_device_poll_reads(&mut cpu, &mut plugin, 0x4017, 2);
    cpu.run_dots(&mut plugin, dot, 60);

    let polls = cpu.mcu().reads.iter().filter(|&&a| a == 0x4017).count();
    assert_eq!(polls, 0, "a frozen $4017 poll must not read the bus");
    assert!(
        cpu.mcu().device_replays[2] >= 3,
        "a frozen $4017 poll must replay its device action"
    );
}

#[test]
fn dmc_status_poll_declares_a_spin() {
    // The DMC status poll in `sprdma_and_dmc_dma.nes` can only wake from a
    // DMA/APU write, which never wakes a spin by decision. It still freezes
    // with its APU effect replayed; replay itself must not wake it.
    let mut cpu = device_poll_program(0x4015);
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_device_poll_reads(&mut cpu, &mut plugin, 0x4015, 0);
    cpu.run_dots(&mut plugin, dot, 60);

    let polls = cpu.mcu().reads.iter().filter(|&&a| a == 0x4015).count();
    assert_eq!(polls, 0, "a frozen $4015 poll must not read the bus");
    assert!(
        cpu.mcu().device_replays[0] >= 3,
        "a frozen $4015 poll must replay its device action"
    );
}

#[test]
fn device_poll_write_wake_reexecutes_the_read_then_redeclares() {
    let mut cpu = device_poll_program(0x4015);
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_device_poll_reads(&mut cpu, &mut plugin, 0x4015, 0);
    cpu.run_dots(&mut plugin, dot, 10);

    // Only a CPU write to the polled address wakes a device poll. The wake
    // re-executes the real loop-top read; a nonzero status keeps the loop
    // going, so it must declare itself again. Interrupt wakes are pinned by
    // their own exact-dot tests.
    let wake_dot = dot + 10;
    cpu.mcu_mut().reads.clear();
    cpu.write_mem(0x4015, 2);
    cpu.run_dots(&mut plugin, wake_dot, 40);

    let reads = cpu.mcu().reads.iter().filter(|&&a| a == 0x4015).count();
    assert!(
        (1..=2).contains(&reads),
        "the write wake must re-execute the real poll before re-freezing, saw {reads}"
    );
    assert_eq!(cpu.a(), 2, "the real wake read must supply the value");
    assert_eq!(
        cpu.spin_loop_top_for_test(),
        Some(0x0400),
        "a nonzero status must re-declare the spin at its loop top"
    );

    let replays = cpu.mcu().device_replays[0];
    cpu.run_dots(&mut plugin, wake_dot + 40, 60);
    assert!(
        cpu.mcu().device_replays[0] > replays,
        "the re-declared spin must keep replaying its device effect"
    );
    assert_eq!(
        cpu.mcu().reads.iter().filter(|&&a| a == 0x4015).count(),
        reads,
        "the re-declared spin must stop reading the bus"
    );
}

#[test]
fn irq_wakes_device_poll_at_loop_top() {
    let mut cpu = device_poll_program(0x4015);
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_device_poll_reads(&mut cpu, &mut plugin, 0x4015, 0);
    cpu.run_dots(&mut plugin, dot, 15);
    cpu.mcu_mut().reads.clear();

    cpu.set_irq(true);
    cpu.run_dots(&mut plugin, dot + 15, 40);

    let reads = &cpu.mcu().reads;
    assert!(
        reads.iter().any(|a| (0x0100..=0x01FF).contains(a)) || reads.contains(&0xFFFE),
        "IRQ must wake a frozen device poll into the interrupt sequence, saw {reads:?}"
    );
    assert_eq!(cpu.a(), 1, "wake must preserve registers");
}

#[test]
fn nmi_edge_wakes_device_poll_at_loop_top() {
    let mut cpu = device_poll_program(0x4016);
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_device_poll_reads(&mut cpu, &mut plugin, 0x4016, 1);
    cpu.run_dots(&mut plugin, dot, 15);
    cpu.mcu_mut().reads.clear();

    for d in dot + 15..dot + 27 {
        cpu.dot(&mut plugin, d, true);
    }
    let reads = &cpu.mcu().reads;
    assert!(
        reads.iter().any(|a| (0x0100..=0x01FF).contains(a)) || reads.contains(&0xFFFA),
        "NMI must wake a frozen device poll into the interrupt sequence, saw {reads:?}"
    );
    assert_eq!(cpu.a(), 1, "wake must preserve registers");
}

/// A never-freeze control on the same dot grid as the frozen run, advanced
/// to `dot` so a trigger applied next meets the same loop the frozen run's
/// counterfactual would have executed.
fn control_at(dot: u64, addr: u16) -> (Harness, EmptyPlugin<TestMcu>) {
    let mut control = device_poll_program(addr);
    control.set_spin_declaration_disabled_for_test(true);
    let mut plugin = EmptyPlugin::new();
    control.run_dots(&mut plugin, 3, dot - 3);
    control.mcu_mut().reads.clear();
    (control, plugin)
}

/// Absolute dot at which each run first drives a real read of `addr` after
/// `from_dot`. A frozen run can only read once its spin wakes; the control
/// reads at the loop cadence the frozen run would have followed.
fn first_read_dots(
    frozen: &mut Harness,
    frozen_plugin: &mut EmptyPlugin<TestMcu>,
    control: &mut Harness,
    control_plugin: &mut EmptyPlugin<TestMcu>,
    from_dot: u64,
    dots: u64,
    addr: u16,
) -> (Option<u64>, Option<u64>) {
    let (mut frozen_read, mut control_read) = (None, None);
    for d in from_dot..from_dot + dots {
        frozen.dot(frozen_plugin, d, false);
        control.dot(control_plugin, d, false);
        if frozen_read.is_none() && frozen.mcu().reads.contains(&addr) {
            frozen_read = Some(d);
        }
        if control_read.is_none() && control.mcu().reads.contains(&addr) {
            control_read = Some(d);
        }
        frozen.mcu_mut().reads.clear();
        control.mcu_mut().reads.clear();
    }
    (frozen_read, control_read)
}

/// A qualifying CPU write wakes a frozen `$4015` poll at its loop top. The
/// wake dot releases the spin without executing it, so the re-executed
/// loop-top read lands exactly one dot after the executing loop's next
/// loop-top read: the documented loop-top re-execution fallback.
#[test]
fn device_poll_write_wake_reexecutes_the_loop_top_read() {
    let mut frozen = device_poll_program(0x4015);
    let mut frozen_plugin = EmptyPlugin::new();
    let dot = run_to_device_poll_reads(&mut frozen, &mut frozen_plugin, 0x4015, 0);
    let (mut control, mut control_plugin) = control_at(dot, 0x4015);
    frozen.run_dots(&mut frozen_plugin, dot, 3);
    control.run_dots(&mut control_plugin, dot, 3);
    assert_eq!(
        frozen.spin_loop_top_for_test(),
        Some(0x0400),
        "the frozen poll must be pinned at its loop top"
    );
    assert_eq!(
        frozen.pc(),
        control.pc(),
        "both runs must enter the write phase at the same loop position"
    );

    let wake_dot = dot + 3;
    frozen.write_mem(0x4015, 2);
    control.write_mem(0x4015, 2);
    let (frozen_read, control_read) = first_read_dots(
        &mut frozen,
        &mut frozen_plugin,
        &mut control,
        &mut control_plugin,
        wake_dot,
        12,
        0x4015,
    );
    assert_eq!(
        control_read,
        Some(wake_dot + 3),
        "the executing loop's next loop-top read lands three dots after the write"
    );
    assert_eq!(
        frozen_read,
        control_read.map(|d| d + 1),
        "the wake dot cannot execute, so the re-executed read is exactly one dot later"
    );
    assert_eq!(frozen.a(), 2, "the real wake read must supply the value");
}

/// An IRQ edge wakes a frozen `$4016` poll into the interrupt sequence at
/// exactly the dot the executing loop dispatches: the vector fetch lands on
/// the same absolute dot in both runs.
#[test]
fn device_poll_irq_wake_matches_the_executing_loop_dot() {
    let mut frozen = device_poll_program(0x4016);
    let mut frozen_plugin = EmptyPlugin::new();
    let dot = run_to_device_poll_reads(&mut frozen, &mut frozen_plugin, 0x4016, 1);
    let (mut control, mut control_plugin) = control_at(dot, 0x4016);
    frozen.run_dots(&mut frozen_plugin, dot, 15);
    control.run_dots(&mut control_plugin, dot, 15);

    let edge_dot = dot + 15;
    frozen.set_irq(true);
    control.set_irq(true);
    let (frozen_vector, control_vector) = first_read_dots(
        &mut frozen,
        &mut frozen_plugin,
        &mut control,
        &mut control_plugin,
        edge_dot,
        20,
        0xFFFE,
    );
    assert_eq!(
        frozen_vector, control_vector,
        "a frozen device poll must dispatch on the executing loop's IRQ dot"
    );
    assert_eq!(
        frozen_vector,
        Some(edge_dot + 7),
        "the IRQ vector fetch lands seven dots after the edge"
    );
}

/// An NMI edge wakes a frozen `$4017` poll at exactly the dot the executing
/// loop dispatches.
#[test]
fn device_poll_nmi_wake_matches_the_executing_loop_dot() {
    let mut frozen = device_poll_program(0x4017);
    let mut frozen_plugin = EmptyPlugin::new();
    let dot = run_to_device_poll_reads(&mut frozen, &mut frozen_plugin, 0x4017, 2);
    let (mut control, mut control_plugin) = control_at(dot, 0x4017);
    frozen.run_dots(&mut frozen_plugin, dot, 15);
    control.run_dots(&mut control_plugin, dot, 15);

    let edge_dot = dot + 15;
    let (mut frozen_vector, mut control_vector) = (None, None);
    for d in edge_dot..edge_dot + 20 {
        frozen.dot(&mut frozen_plugin, d, true);
        control.dot(&mut control_plugin, d, true);
        if frozen_vector.is_none() && frozen.mcu().reads.contains(&0xFFFA) {
            frozen_vector = Some(d);
        }
        if control_vector.is_none() && control.mcu().reads.contains(&0xFFFA) {
            control_vector = Some(d);
        }
    }
    assert_eq!(
        frozen_vector, control_vector,
        "a frozen device poll must dispatch on the executing loop's NMI dot"
    );
    assert!(
        frozen_vector.is_some(),
        "the NMI must dispatch within the window"
    );
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
        !frozen.mcu().reads.contains(&0x10),
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
        !cpu.mcu().reads.contains(&0x10),
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
        !cpu.mcu().reads.contains(&0x10),
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
