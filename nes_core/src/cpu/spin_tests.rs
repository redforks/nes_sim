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

fn device_poll_trace(addr: u16, disable_spin: bool, dots: u64) -> Vec<u16> {
    let mut cpu = device_poll_program(addr);
    if disable_spin {
        cpu.set_spin_declaration_disabled_for_test(true);
    }
    let mut plugin = EmptyPlugin::new();
    for dot in 3..3 + dots {
        cpu.dot(&mut plugin, dot, false);
    }
    cpu.mcu()
        .reads
        .iter()
        .copied()
        .filter(|a| *a == addr)
        .collect()
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

/// A loop that polls a device register is deliberately not declared a spin:
/// its read is the device action (a controller shift, an APU interrupt
/// acknowledge, a zapper sample) and device-sourced writes never wake a
/// spin. Keeping it executing preserves the read count and order; freezing
/// it would swallow or have to replay a device action.
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
    let mut cpu = create_cpu(mcu);
    cpu.set_pc(TOP);
    cpu
}

#[test]
fn direct_write_wake_needs_two_fresh_back_edges_before_refreeze() {
    let mut cpu = poll_program();
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_poll_reads(&mut cpu, &mut plugin, 2);
    cpu.run_dots(&mut plugin, dot, 15);
    assert!(cpu.is_frozen(), "precondition: spin must be declared");
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
    assert!(cpu.is_frozen(), "precondition: spin must be declared");

    // The queued store is a CPU write to the exact polled address. Frozen
    // ticks must let it execute for real instead of swallowing it.
    cpu.set_a(7);
    cpu.stage_pending_cycle_for_test(Microcode::StoreR(ValueSource::Mem, Register::A), Some(0x10));
    // The frozen dot detects the pending store and releases the spin; the
    // store itself executes on the next dot.
    cpu.dot(&mut plugin, dot + 15, false);
    assert!(!cpu.is_frozen(), "the matching store must release the spin");
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
    assert!(cpu.is_frozen(), "precondition: spin must be declared");

    // A DMC halt lands while the spin is frozen. It must compose, not panic
    // or replace the spin source.
    assert!(cpu.try_freeze(), "DMA must still be able to halt the CPU");
    assert!(cpu.is_frozen());
    assert!(
        cpu.dma_halt_bus_addr().is_none(),
        "a declared spin sits on the branch's internal fixup cycle; DMA repeats the next fetch address"
    );

    // The DMA read completes and releases only its own source.
    cpu.unfreeze();
    assert!(cpu.is_frozen(), "DMA unfreeze must not wake the spin");
    cpu.mcu_mut().reads.clear();
    cpu.run_dots(&mut plugin, dot + 10, 12);
    assert!(
        !cpu.mcu().reads.contains(&0x10),
        "the spin must keep its poll dormant after DMA completes, saw {:?}",
        cpu.mcu().reads
    );
}

#[test]
fn device_register_poll_read_cadence_matches_spin_disabled_control() {
    for addr in [0x4015, 0x4016, 0x4017] {
        let control = device_poll_trace(addr, true, 200);
        let normal = device_poll_trace(addr, false, 200);
        assert_eq!(
            normal, control,
            "a {addr:#06x} poll must not swallow or duplicate device reads"
        );
        assert!(
            normal.len() >= 2,
            "the {addr:#06x} trace must include real repeated polls"
        );
    }
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
    for dot in 3..3 + 120 {
        cpu.dot(&mut plugin, dot, false);
        assert!(
            !cpu.is_frozen(),
            "a write-in-loop must never declare a spin (frozen at dot {dot})"
        );
    }
    let stores = cpu.mcu().writes.iter().filter(|&&(a, _)| a == 0x10).count();
    assert!(
        stores >= 5,
        "the loop must keep executing its store, saw {stores} writes to $10"
    );
}

#[test]
fn zapper_register_poll_is_not_declared_as_a_spin() {
    let mut cpu = device_poll_program(0x4017);
    let mut plugin = EmptyPlugin::new();
    let mut dot = 3u64;
    while cpu.mcu().reads.iter().filter(|&&a| a == 0x4017).count() < 2 {
        cpu.dot(&mut plugin, dot, false);
        dot += 1;
        assert!(dot < 1000, "zapper poll never happened");
    }
    cpu.mcu_mut().reads.clear();
    cpu.run_dots(&mut plugin, dot, 60);

    let polls = cpu.mcu().reads.iter().filter(|&&a| a == 0x4017).count();
    assert!(
        polls >= 3,
        "a $4017 device poll must keep reading the register, saw {polls} reads"
    );
}

#[test]
fn dmc_status_poll_is_not_declared_as_a_spin() {
    // The DMC status poll in `sprdma_and_dmc_dma.nes` can only wake from a
    // DMA/APU write, which never wakes a spin by decision. It must therefore
    // execute for real rather than be declared a spin.
    let mut cpu = device_poll_program(0x4015);
    let mut plugin = EmptyPlugin::new();
    let mut dot = 3u64;
    while cpu.mcu().reads.iter().filter(|&&a| a == 0x4015).count() < 2 {
        cpu.dot(&mut plugin, dot, false);
        dot += 1;
        assert!(dot < 1000, "DMC status poll never happened");
    }
    cpu.mcu_mut().reads.clear();
    cpu.run_dots(&mut plugin, dot, 60);

    let polls = cpu.mcu().reads.iter().filter(|&&a| a == 0x4015).count();
    assert!(
        polls >= 3,
        "a $4015 device poll must keep reading the register, saw {polls} reads"
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
