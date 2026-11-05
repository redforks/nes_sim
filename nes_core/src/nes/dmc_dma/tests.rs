use super::*;
use crate::bus::Bus;
use crate::cpu::{Flag, ValueSource, microcode::Microcode};
use crate::mcu::Mcu; // cpu freeze staging (ADR-0015)

/// The DMC DMA seam only ever drives the bus's memory through the
/// `DmcDmaMem` endpoint methods; every other `Mcu` op is never reached on
/// the mock, so those impl bodies are unreachable.
impl Mcu for MockDmcDmaMem {
    fn read(&mut self, _address: u16) -> u8 {
        unreachable!("DMC DMA never drives plain Mcu reads")
    }

    fn peek(&self, _address: u16) -> u8 {
        unreachable!("DMC DMA never drives plain Mcu peeks")
    }

    fn write(&mut self, _address: u16, _value: u8) {
        unreachable!("DMC DMA never drives plain Mcu writes")
    }
}
use mockall::predicate::*;
use mockall::*;

#[test]
fn dma_not_activated() {
    let mut t = TestStruct::new();
    t.expect_take_dmc_dma_request(None);
    t.tick_and_assert(State::Inactive, false);
}

#[test]
fn load_dmc_dma() {
    let mut t = TestStruct::new();

    // (get) \ CPU writes to $4015     <- DMC enabled
    // (put) / during this APU cycle   <- DMC enabled
    // (get) CPU reads
    // (halted) (get) CPU reads from address A  <- DMA halt cycle
    // (halted) (put) CPU reads from address A  <- DMA dummy cycle
    // (halted) (get) DMA reads from address B
    // (put) CPU reads from address A  <- CPU resumes execution
    //
    // test starts from 2nd step, because nes_machine tick in this order during one cycle:
    // - tick apu
    // - tick dmc_dma
    // - tick cpu
    //
    // when cpu writes to $4015 to enable dmc channel, dmc_dma get request on the next cycle

    // get request
    t.expect_take_dmc_dma_request(Some((DmcDmaType::Load, 0x1234)));
    t.tick_and_assert(State::DelayForLoad(1), false);

    // delay cpu cycle
    t.tick_and_assert(
        State::TryHalt {
            halt_on_put: false,
            first_attempt: true,
        },
        false,
    );

    // try freeze and succeed: pending read cycle from $2234
    t.stage_read_cycle(0x2234);
    t.expect_read_mem(0x2234, 0x00);
    t.tick_and_assert(State::Dummy, true);

    // dummy
    t.dummy_step();

    // apu is get cycle, it is safe to do dma read
    t.expect_read_mem(0x1234, 0xcd);
    t.expect_supply_dmc_byte_with(0xcd);
    // fetch cycle: CPU is still halted while the DMA drives the bus
    t.tick_and_assert(State::Unfreeze, true);

    // CPU resumes execution on the cycle after the DMA read
    t.unfreeze_tick();
}

#[test]
fn load_dmc_dmadelayed() {
    let mut t = TestStruct::new();

    //    (get) \ CPU writes to $4015     <- DMC enabled
    //    (put) / during this APU cycle   <- DMC enabled
    //    (get) CPU reads
    //    (get) CPU writes                <- DMA attempts to halt
    // (halted) (put) CPU reads from address A  <- DMA halt cycle
    // (halted) (get) CPU reads from address A  <- DMA dummy cycle
    // (halted) (put) CPU reads from address A  <- DMA alignment cycle
    // (halted) (get) DMA reads from address B
    //    (put) CPU reads from address A  <- CPU resumes execution
    //
    // test starts from 2nd step, see comment in `load_dmc_dma()` test

    // get request
    t.expect_take_dmc_dma_request(Some((DmcDmaType::Load, 0x1234)));
    t.tick_and_assert(State::DelayForLoad(1), false);

    // delay cpu cycle
    t.tick_and_assert(
        State::TryHalt {
            halt_on_put: false,
            first_attempt: true,
        },
        false,
    );

    // try freeze but cpu is write
    t.stage_write_cycle(0x2234);
    t.tick_and_assert(
        State::TryHalt {
            first_attempt: false,
            halt_on_put: false,
        },
        true,
    );

    // cpu halt succeed
    t.stage_read_cycle(0x2234);
    t.expect_read_mem(0x2234, 0x00);
    t.tick_and_assert(State::Dummy, false);

    // dummy
    t.dummy_step();

    // apu is put cycle, need an align cycle
    t.expect_read_mem(0x2234, 0x0d);
    t.tick_and_assert(State::Read, false);

    // apu is get cycle, complete the dma
    t.expect_read_mem(0x1234, 0xcd);
    t.expect_supply_dmc_byte_with(0xcd);
    // fetch cycle: CPU is still halted while the DMA drives the bus
    t.tick_and_assert(State::Unfreeze, true);

    // CPU resumes execution on the cycle after the DMA read
    t.unfreeze_tick();
}

#[test]
fn reload_dmc_dma() {
    let mut t = TestStruct::new();

    // (halted) (put) CPU reads from address A  <- DMA halt cycle
    // (halted) (get) CPU reads from address A  <- DMA dummy cycle
    // (halted) (put) CPU reads from address A  <- DMA alignment cycle
    // (halted) (get) DMA reads from address B
    //          (put) CPU reads from address A  <- CPU resumes execution

    // get request; the halt lands on a put cycle (reload requests skip the
    // request cycle) and succeeds against a pending read
    t.expect_take_dmc_dma_request(Some((DmcDmaType::Reload, 0x1234)));
    t.stage_read_cycle(0x2234);
    t.expect_read_mem(0x2234, 0x00);
    t.tick_and_assert(State::Dummy, false);

    // dummy
    t.dummy_step();

    // apu is put cycle, so it is an align cycle, no dma read
    t.expect_read_mem(0x2234, 0x00);
    t.tick_and_assert(State::Read, false);

    // read and supply to apu
    t.expect_read_mem(0x1234, 0xcd);
    t.expect_supply_dmc_byte_with(0xcd);
    // fetch cycle: CPU is still halted while the DMA drives the bus
    t.tick_and_assert(State::Unfreeze, false);

    // CPU resumes execution on the cycle after the DMA read
    t.unfreeze_tick();
}

#[test]
fn reload_dmc_dma_delayed_1_cycle() {
    let mut t = TestStruct::new();

    //          (put) CPU writes                <- DMA attempts to halt
    // (halted) (get) CPU reads from address A  <- DMA halt cycle
    // (halted) (put) CPU reads from address A  <- DMA dummy cycle
    // (halted) (get) DMA reads from address B
    //          (put) CPU reads from address A  <- CPU resumes execution

    // get request; try freeze but cpu is write
    t.expect_take_dmc_dma_request(Some((DmcDmaType::Reload, 0x1234)));
    t.stage_write_cycle(0x2234);
    t.tick_and_assert(
        State::TryHalt {
            halt_on_put: true,
            first_attempt: false,
        },
        false,
    );

    // try freeze and succeed
    t.stage_read_cycle(0x2234);
    t.expect_read_mem(0x2234, 0x00);
    t.tick_and_assert(State::Dummy, false);

    // dummy
    t.dummy_step();

    // read and supply to apu
    t.expect_read_mem(0x1234, 0xcd);
    t.expect_supply_dmc_byte_with(0xcd);
    // fetch cycle: CPU is still halted while the DMA drives the bus
    t.tick_and_assert(State::Unfreeze, true);

    // CPU resumes execution on the cycle after the DMA read
    t.unfreeze_tick();
}

#[test]
fn reload_dmc_dma_delayed_2_cycle() {
    let mut t = TestStruct::new();

    //          (put) CPU writes                <- DMA attempts to halt
    //          (get) CPU writes                <- DMA attempts to halt
    // (halted) (put) CPU reads from address A  <- DMA halt cycle
    // (halted) (get) CPU reads from address A  <- DMA dummy cycle
    // (halted) (put) CPU reads from address A  <- DMA alignment cycle
    // (halted) (get) DMA reads from address B
    //          (put) CPU reads from address A  <- CPU resumes execution

    // get request; try freeze but cpu is write (the reload entry recurses
    // one tick, so this first tick both takes the request and refuses)
    t.expect_take_dmc_dma_request(Some((DmcDmaType::Reload, 0x1234)));
    t.stage_write_cycle(0x2234);
    t.tick_and_assert(
        State::TryHalt {
            halt_on_put: true,
            first_attempt: false,
        },
        false,
    );

    // try again but cpu still write
    t.stage_write_cycle(0x2234);
    t.tick_and_assert(
        State::TryHalt {
            halt_on_put: true,
            first_attempt: false,
        },
        false,
    );

    // try freeze and succeed
    t.stage_read_cycle(0x2234);
    t.expect_read_mem(0x2234, 0x00);
    t.tick_and_assert(State::Dummy, false);

    // dummy
    t.dummy_step();

    // need align
    t.expect_read_mem(0x2234, 0x8f);
    t.tick_and_assert(State::Read, false);
    // read and supply to apu
    t.expect_read_mem(0x1234, 0xcd);
    t.expect_supply_dmc_byte_with(0xcd);
    // fetch cycle: CPU is still halted while the DMA drives the bus
    t.tick_and_assert(State::Unfreeze, false);

    // CPU resumes execution on the cycle after the DMA read
    t.unfreeze_tick();
}

#[test]
fn reload_dmc_dma_delayed_3_cycle() {
    let mut t = TestStruct::new();

    //     (put) CPU writes                <- DMA attempts to halt
    //     (get) CPU writes                <- DMA attempts to halt
    //     (put) CPU writes                <- DMA attempts to halt
    // (halted) (get) CPU reads from address A  <- DMA halt cycle
    // (halted) (put) CPU reads from address A  <- DMA dummy cycle
    // (halted) (get) DMA reads from address B
    //     (put) CPU reads from address A  <- CPU resumes execution

    // get request; try freeze but cpu is write
    t.expect_take_dmc_dma_request(Some((DmcDmaType::Reload, 0x1234)));
    t.stage_write_cycle(0x2234);
    t.tick_and_assert(
        State::TryHalt {
            halt_on_put: true,
            first_attempt: false,
        },
        false,
    );

    // try again but cpu still write
    t.stage_write_cycle(0x2234);
    t.tick_and_assert(
        State::TryHalt {
            halt_on_put: true,
            first_attempt: false,
        },
        false,
    );

    // try again but cpu still write
    t.stage_write_cycle(0x2234);
    t.tick_and_assert(
        State::TryHalt {
            halt_on_put: true,
            first_attempt: false,
        },
        false,
    );

    // try freeze and succeed
    t.stage_read_cycle(0x2234);
    t.expect_read_mem(0x2234, 0x00);
    t.tick_and_assert(State::Dummy, false);

    // dummy
    t.dummy_step();

    // read and supply to apu
    t.expect_read_mem(0x1234, 0xcd);
    t.expect_supply_dmc_byte_with(0xcd);
    // fetch cycle: CPU is still halted while the DMA drives the bus
    t.tick_and_assert(State::Unfreeze, true);

    // CPU resumes execution on the cycle after the DMA read
    t.unfreeze_tick();
}

#[test]
fn halt_on_internal_cycle_repeats_next_fetch_addr() {
    // The halt cycle lands on a purely internal CPU cycle, which drives no
    // bus address of its own: the DMA repeats the CPU's next fetch address.
    let mut t = TestStruct::new();
    t.expect_take_dmc_dma_request(Some((DmcDmaType::Load, 0x1234)));
    t.tick_and_assert(State::DelayForLoad(1), false);
    t.tick_and_assert(
        State::TryHalt {
            halt_on_put: false,
            first_attempt: true,
        },
        false,
    );

    t.stage_internal_cycle(0x0400);
    t.tick_and_assert(State::Dummy, true);

    // The dummy cycle drives the fallback address on the bus.
    t.expect_read_mem(0x0400, 0x00);
    t.tick_and_assert(State::AlignOrRead, false);
}

#[test]
fn is_busy_tracks_state_and_reset_clears_in_flight_work() {
    let mut t = TestStruct::new();
    assert!(!t.dma.is_busy(), "fresh controller must be idle");

    // Start a load DMA so the machine leaves Inactive.
    t.expect_take_dmc_dma_request(Some((DmcDmaType::Load, 0x1234)));
    t.tick_and_assert(State::DelayForLoad(1), false);
    assert!(t.dma.is_busy());

    // Reset must drop everything: a tick on a reset controller is a no-op.
    t.dma.reset();
    assert!(!t.dma.is_busy());
    t.expect_take_dmc_dma_request(None);
    t.tick_and_assert(State::Inactive, false);
}

/// The DMC DMA state machine over a production [`Cpu`] plus a mocked
/// [`DmcDmaMem`] held inside a real [`Bus`] (ADR-0015). The real CPU makes
/// the freeze seam (`try_freeze`/`dma_halt_bus_addr`/`unfreeze`) behave
/// exactly as in production: each tick stages the pending bus cycle to
/// control the freeze outcome, and the mocked put/get phase is staged on
/// the bus's master clock.
struct TestStruct {
    seq: Sequence,
    dma: DmcDma,
    cpu: Cpu,
    bus: Bus<MockDmcDmaMem>,
}

impl TestStruct {
    fn new() -> Self {
        let cpu = Cpu::new();
        let seq = Sequence::new();
        let dma = DmcDma::default();
        Self {
            seq,
            dma,
            cpu,
            bus: Bus::new(MockDmcDmaMem::new()),
        }
    }

    /// Stage a pending read cycle from the address latch: the CPU freezes
    /// and repeats `addr` on the bus until unfrozen.
    fn stage_read_cycle(&mut self, addr: u16) {
        self.cpu
            .stage_pending_cycle_for_test(Microcode::LoadIntoAlu(ValueSource::Mem), Some(addr));
    }

    /// Stage a pending write cycle: the freeze is refused (writes must
    /// never be swallowed).
    fn stage_write_cycle(&mut self, addr: u16) {
        self.cpu
            .stage_pending_cycle_for_test(Microcode::StoreAlu(ValueSource::Mem), Some(addr));
    }

    /// Stage a purely internal pending cycle whose next fetch address is
    /// `pc`: the halt cycle repeats that fallback address on the bus.
    fn stage_internal_cycle(&mut self, pc: u16) {
        self.cpu
            .stage_pending_cycle_for_test(Microcode::SetFlag(Flag::InterruptDisabled), None);
        self.cpu.set_next_fetch_pc_for_test(pc);
    }

    fn tick_and_assert(&mut self, exp_state: State, is_get: bool) {
        // `is_apu_get_clock` pins the get phase to odd CPU cycles.
        let clock = if is_get {
            SystemClock(3)
        } else {
            SystemClock(0)
        };
        self.bus.set_clock(clock);
        self.dma.tick(&mut self.cpu, &mut self.bus);
        assert_eq!(self.dma.state, exp_state);
        self.bus.mcu_mut().checkpoint();
    }

    /// Final Eastern tick: the Unfreeze state releases the CPU, so the next
    /// tick must leave the CPU running again.
    fn unfreeze_tick(&mut self) {
        self.tick_and_assert(State::Inactive, false);
        assert!(!self.cpu.frozen, "CPU must resume after the DMA read cycle");
    }

    fn expect_take_dmc_dma_request(&mut self, req: Option<(DmcDmaType, u16)>) {
        self.bus
            .mcu_mut()
            .expect_take_dmc_dma_request()
            .times(1)
            .in_sequence(&mut self.seq)
            .return_const(req);
    }

    fn expect_read_mem(&mut self, addr: u16, result: u8) {
        self.bus
            .mcu_mut()
            .expect_read_mem()
            .with(eq(addr))
            .times(1)
            .in_sequence(&mut self.seq)
            .return_const(result);
    }

    fn expect_supply_dmc_byte_with(&mut self, value: u8) {
        self.bus
            .mcu_mut()
            .expect_supply_dmc_byte()
            .with(eq(value))
            .times(1)
            .in_sequence(&mut self.seq)
            .return_const(());
    }

    fn dummy_step(&mut self) {
        self.expect_read_mem(0x2234, 0x00);
        self.tick_and_assert(State::AlignOrRead, false);
    }
}
