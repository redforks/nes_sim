//! Frozen $2002 poll side-effect tests (ticket #135).
//!
//! Every assertion observes behavior at the CPU/MCU seam: PPU status
//! through bus reads, `nmi_line`, NMI dispatch, and the PPU write toggle.
//! No freeze flag is read.

use super::*;

/// A CPU-side MCU that presents real `NesMcu`-like $2002 semantics at the
/// `Mcu` seam: mirrors normalize to $2002, a status read clears VBL and
/// resets the $2005/$2006 write toggle, and a read on the race dot keeps
/// VBL from ever setting. Reads are counted so a frozen loop cannot
/// secretly re-drive the bus while its side effects are replayed.
struct StatusMcu {
    mem: [u8; 0x10000],
    status: u8,
    write_toggle: bool,
    nmi_enabled: bool,
    suppressed: bool,
    ppu_reads: Vec<u16>,
    replays: usize,
    other_reads: Vec<u16>,
    writes: Vec<(u16, u8)>,
    scanline: u16,
    dot: u16,
}

impl StatusMcu {
    fn new() -> Self {
        Self {
            mem: [0; 0x10000],
            status: 0,
            write_toggle: false,
            nmi_enabled: false,
            suppressed: false,
            ppu_reads: Vec::new(),
            replays: 0,
            other_reads: Vec::new(),
            writes: Vec::new(),
            scanline: 100,
            dot: 100,
        }
    }

    fn is_status_addr(addr: u16) -> bool {
        (0x2000..=0x3FFF).contains(&addr) && (addr & 0x2007) == 0x2002
    }

    fn status_read(&mut self, addr: u16) -> u8 {
        self.ppu_reads.push(addr);
        if self.scanline == 241 && self.dot == 1 {
            self.suppressed = true;
        }
        let status = self.status;
        self.status &= !0x80;
        self.write_toggle = false;
        status
    }

    fn nmi_line(&self) -> bool {
        self.status & 0x80 != 0 && self.nmi_enabled
    }

    /// Model the PPU's VBL set on scanline 241 dot 1; a prior status read
    /// on that dot suppresses the flag exactly as `Ppu` does.
    fn tick_ppu(&mut self) {
        if self.scanline == 241 && self.dot == 2 && !self.suppressed {
            self.status |= 0x80;
        }
    }
}

impl Mcu for StatusMcu {
    fn status_register_has_vbl(&self) -> bool {
        self.status & 0x80 != 0
    }

    fn replay_status_read_side_effects(&mut self, address: u16) {
        assert!(Self::is_status_addr(address));
        // Side effects only: no bus read is recorded, matching the contract
        // of the MCU hook. Count replays separately from real bus reads.
        self.replays += 1;
        if self.scanline == 241 && self.dot == 1 {
            self.suppressed = true;
        }
        self.status &= !0x80;
        self.write_toggle = false;
    }

    fn read(&mut self, address: u16) -> u8 {
        if Self::is_status_addr(address) {
            self.status_read(address)
        } else {
            self.other_reads.push(address);
            self.mem[address as usize]
        }
    }

    fn peek(&self, address: u16) -> u8 {
        if Self::is_status_addr(address) {
            self.status
        } else {
            self.mem[address as usize]
        }
    }

    fn write(&mut self, address: u16, value: u8, _clock: SystemClock) {
        self.writes.push((address, value));
        if (0x2000..=0x3FFF).contains(&address) {
            match address & 0x2007 {
                0x2000 => self.nmi_enabled = value & 0x80 != 0,
                0x2005 | 0x2006 => self.write_toggle = !self.write_toggle,
                _ => {}
            }
        }
        self.mem[address as usize] = value;
    }
}

struct Harness {
    cpu: Cpu,
    bus: Bus<StatusMcu>,
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

fn create_cpu(mcu: StatusMcu) -> Harness {
    let mut h = Harness {
        cpu: Cpu::new(),
        bus: Bus::new(mcu),
    };
    let mut plugin = EmptyPlugin::new();
    h.cpu.run_to_instruction_boundary(&mut h.bus, &mut plugin);
    h
}

impl Harness {
    fn mcu(&self) -> &StatusMcu {
        self.bus.mcu()
    }

    fn mcu_mut(&mut self) -> &mut StatusMcu {
        self.bus.mcu_mut()
    }

    /// One CPU dot with the PPU beam and /NMI level published as
    /// `NesMachine::tick_dot_2` does (PPU tick, then NMI sample, then CPU).
    fn dot<P: Plugin<StatusMcu>>(
        &mut self,
        plugin: &mut P,
        clock: u64,
        scanline: u16,
        dot: u16,
    ) -> ExecuteResult {
        self.mcu_mut().scanline = scanline;
        self.mcu_mut().dot = dot;
        self.mcu_mut().tick_ppu();
        let nmi = self.mcu().nmi_line();
        self.cpu
            .update_nmi_line(nmi, SystemClock(clock), clock % 3 == 0);
        self.bus.set_clock(SystemClock(clock));
        self.cpu.tick(&mut self.bus, plugin)
    }
}

/// `BIT $addr / BPL top` with VBL clear and bit 6 clear, so the branch
/// keeps taking the back-edge. NMI gates are explicit per test: disabled
/// for side-effect tests that need the spin to stay frozen, enabled for
/// the NMI line test. The poll is declared after two back-edges.
fn status_poll_program(addr: u16, nmi_enabled: bool) -> Harness {
    const TOP: u16 = 0x0400;
    let mut mcu = StatusMcu::new();
    let program = [
        0x2C,
        addr as u8,
        (addr >> 8) as u8, // $0400 BIT $addr
        0x10,
        0xFB, // $0403 BPL $0400
    ];
    for (i, byte) in program.iter().enumerate() {
        mcu.mem[TOP as usize + i] = *byte;
    }
    mcu.nmi_enabled = nmi_enabled;
    mcu.mem[0xFFFA] = 0x00;
    mcu.mem[0xFFFB] = 0x06;
    mcu.mem[0xFFFC] = TOP as u8;
    mcu.mem[0xFFFD] = (TOP >> 8) as u8;
    let mut cpu = create_cpu(mcu);
    cpu.set_pc(TOP);
    cpu.set_status(0x20); // interrupts enabled
    cpu
}

/// Advance until real status polls stop (no new PPU bus reads for 8 dots).
fn run_to_frozen(cpu: &mut Harness, plugin: &mut EmptyPlugin<StatusMcu>) -> u64 {
    let mut dot = 3u64;
    let mut last = usize::MAX;
    let mut quiet = 0;
    while quiet < 8 {
        cpu.dot(plugin, dot, 100, 100);
        dot += 1;
        let reads = cpu.mcu().ppu_reads.len();
        quiet = if reads == last { quiet + 1 } else { 0 };
        last = reads;
        assert!(
            dot < 300,
            "status poll never stopped executing, reads {last}"
        );
    }
    dot
}

#[test]
fn frozen_status_poll_wakes_when_a_skipped_read_would_observe_vbl() {
    // With NMI disabled the only way out of a `$2002 / BPL` wait is the
    // poll itself seeing VBL set. Replaying the clear must not swallow
    // that observation: the frozen spin must resume so the CPU performs
    // the real read and exits the loop.
    let mut cpu = status_poll_program(0x2002, false);
    let mut plugin = EmptyPlugin::new();
    let mut dot = run_to_frozen(&mut cpu, &mut plugin);
    cpu.mcu_mut().status = 0x80;
    cpu.mcu_mut().ppu_reads.clear();

    let mut observed = false;
    for _ in 0..30 {
        cpu.dot(&mut plugin, dot, 100, 100);
        dot += 1;
        if !cpu.mcu().ppu_reads.is_empty() {
            observed = true;
            break;
        }
    }
    assert!(
        observed,
        "VBL set must release the frozen $2002 poll so the CPU can read it"
    );
    assert_eq!(
        cpu.mcu().status & 0x80,
        0,
        "the real poll read after wake must clear VBL"
    );
}

#[test]
fn frozen_status_poll_clears_vbl_without_new_bus_reads() {
    let mut cpu = status_poll_program(0x2002, false);
    let mut plugin = EmptyPlugin::new();
    let mut dot = run_to_frozen(&mut cpu, &mut plugin);
    // Set the write toggle without VBL: a frozen $2002 loop stays frozen
    // only while its awaited status bit is clear. Replays must clear the
    // toggle at loop cadence.
    cpu.mcu_mut().write_toggle = true;
    cpu.mcu_mut().ppu_reads.clear();
    cpu.mcu_mut().other_reads.clear();
    cpu.mcu_mut().replays = 0;

    for _ in 0..25 {
        cpu.dot(&mut plugin, dot, 100, 100);
        dot += 1;
    }
    assert_eq!(
        cpu.mcu().ppu_reads.len(),
        0,
        "frozen loop must not re-drive the PPU bus, saw {:?}",
        cpu.mcu().ppu_reads
    );
    assert!(
        cpu.mcu().replays >= 2 && cpu.mcu().replays < 20,
        "replay must happen once per loop iteration, not every dot: {} replays",
        cpu.mcu().replays
    );
    assert!(
        !cpu.mcu().write_toggle,
        "replayed reads must reset the $2005/$2006 write toggle"
    );
    assert!(
        !cpu.mcu()
            .other_reads
            .iter()
            .any(|a| (0x0400..=0x0405).contains(a)),
        "frozen loop must not re-execute its poll instruction"
    );
}

#[test]
fn frozen_mirrored_status_poll_replays_side_effects() {
    let mut cpu = status_poll_program(0x2FFA, false);
    let mut plugin = EmptyPlugin::new();
    let mut dot = run_to_frozen(&mut cpu, &mut plugin);
    cpu.mcu_mut().write_toggle = true;
    cpu.mcu_mut().ppu_reads.clear();
    cpu.mcu_mut().other_reads.clear();
    cpu.mcu_mut().replays = 0;

    for _ in 0..25 {
        cpu.dot(&mut plugin, dot, 100, 100);
        dot += 1;
    }
    assert!(
        cpu.mcu().replays >= 2,
        "mirrored $2002 polls must replay side effects, saw {}",
        cpu.mcu().replays
    );
    assert!(
        !cpu.mcu().write_toggle,
        "mirror replays must reset the write toggle"
    );
    assert!(
        cpu.mcu().ppu_reads.is_empty(),
        "mirror replays must not re-drive the PPU bus: reads={:?} other={:?} replays={}",
        cpu.mcu().ppu_reads,
        cpu.mcu().other_reads,
        cpu.mcu().replays
    );
}

#[test]
fn frozen_ram_poll_does_not_replay_status_side_effects() {
    let mut cpu = status_poll_program(0x0010, false);
    let mut plugin = EmptyPlugin::new();
    let mut dot = run_to_frozen(&mut cpu, &mut plugin);
    cpu.mcu_mut().write_toggle = true;
    cpu.mcu_mut().replays = 0;

    for _ in 0..25 {
        cpu.dot(&mut plugin, dot, 100, 100);
        dot += 1;
    }
    assert_eq!(cpu.mcu().replays, 0, "only $2002 polls replay side effects");
    assert!(
        cpu.mcu().write_toggle,
        "a RAM spin must not reset the PPU write toggle"
    );
}

#[test]
fn frozen_status_poll_suppresses_vbl_on_race_dot() {
    let mut cpu = status_poll_program(0x2002, false);
    let mut plugin = EmptyPlugin::new();
    let mut dot = run_to_frozen(&mut cpu, &mut plugin);
    // Skipped reads on the race dot must arm the same suppression a real
    // read there would.
    for _ in 0..20 {
        cpu.dot(&mut plugin, dot, 241, 1);
        dot += 1;
    }
    assert!(
        cpu.mcu().suppressed,
        "a skipped read on 241:1 must arm VBL suppression"
    );
    cpu.mcu_mut().status = 0;
    cpu.dot(&mut plugin, dot, 241, 2);
    assert_eq!(
        cpu.mcu().status & 0x80,
        0,
        "VBL must not set after a skipped read on the race dot"
    );
}

#[test]
fn resumed_status_poll_drops_nmi_line_after_cleared_vbl() {
    let mut cpu = status_poll_program(0x2002, true);
    let mut plugin = EmptyPlugin::new();
    let mut dot = run_to_frozen(&mut cpu, &mut plugin);
    cpu.mcu_mut().status = 0x80;
    assert!(cpu.mcu().nmi_line(), "VBL with NMI enabled drives /NMI low");

    let mut saw_real_read = false;
    for _ in 0..30 {
        cpu.dot(&mut plugin, dot, 100, 100);
        dot += 1;
        if !cpu.mcu().ppu_reads.is_empty() {
            saw_real_read = true;
            break;
        }
    }
    assert!(
        saw_real_read,
        "the status poll must resume when VBL appears"
    );
    // The poll resumes on the VBL edge, but the CPU keeps its frozen-state
    // instruction pointer until the next cycle. Run that queued read.
    let mut line_dropped = false;
    for _ in 0..12 {
        cpu.dot(&mut plugin, dot, 100, 100);
        dot += 1;
        if !cpu.mcu().nmi_line() {
            line_dropped = true;
            break;
        }
    }
    assert!(
        line_dropped,
        "the resumed read must clear VBL and drop /NMI: status={:#x} reads={:?}",
        cpu.mcu().status,
        cpu.mcu().ppu_reads
    );
}

#[test]
fn dma_halt_suspends_status_replay_until_the_transfer_ends() {
    let mut cpu = status_poll_program(0x2002, false);
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_frozen(&mut cpu, &mut plugin);
    assert!(cpu.try_freeze(), "DMC halt must compose with the spin");
    cpu.mcu_mut().replays = 0;

    for d in dot..dot + 25 {
        cpu.dot(&mut plugin, d, 100, 100);
    }
    assert_eq!(
        cpu.mcu().replays,
        0,
        "a DMC transfer owns the bus; skipped status reads must not replay"
    );
    cpu.unfreeze();
    for d in dot + 25..dot + 50 {
        cpu.dot(&mut plugin, d, 100, 100);
    }
    assert!(
        cpu.mcu().replays > 0,
        "status replay must resume after the DMA read completes"
    );
}

struct StopPlugin;

impl Plugin<StatusMcu> for StopPlugin {
    fn start(&mut self, _: &MachineView<StatusMcu>) {}
    fn end(&mut self, _: &MachineView<StatusMcu>) {}
    fn should_stop(&self) -> ExecuteResult {
        ExecuteResult::Stop(7)
    }
}

#[test]
fn frozen_status_poll_still_samples_plugin_should_stop() {
    let mut cpu = status_poll_program(0x2002, false);
    let mut plugin = EmptyPlugin::new();
    let dot = run_to_frozen(&mut cpu, &mut plugin);
    let mut stop_plugin = StopPlugin;
    let control = cpu.dot(&mut stop_plugin, dot, 100, 100);
    assert_eq!(
        control,
        ExecuteResult::Stop(7),
        "harness stop/verdict plugins must fire on frozen dots"
    );
}
