use super::*;
use mockall::predicate::*;
use mockall::*;

#[test]
fn dma_not_activated() {
    let mut t = TestStruct::new();
    t.expect_take_dmc_dma_request(None);
    t.tick_and_assert(State::Inactive);
}

#[test]
fn load_dmc_dma() {
    let mut t = TestStruct::new();

    // (get) \ CPU writes to $4015     <- DMC enabled
    // (put) / during this APU cycle   <- DMC enabled
    // (get) CPU reads
    // (put) CPU reads
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
    t.tick_and_assert(State::DelayForLoad(2));

    // delay first cpu cycle
    t.tick_and_assert(State::DelayForLoad(1));

    // delay second cpu cycle
    t.tick_and_assert(State::TryHalt {
        halt_on_put: false,
        first_attempt: true,
    });

    // try freeze and succeed
    t.expect_is_get_cycle_and_freeze(true, true);
    t.expect_last_read_addr(0x2234);
    t.tick_and_assert(State::Dummy);

    // dummy
    t.dummy_step();

    // apu is get cycle, it is safe to do dma read
    t.expect_is_get_cycle(true);
    t.expect_read_mem(0x1234, 0xcd);
    t.expect_supply_dmc_byte_with(0xcd);
    t.expect_unfreeze();
    t.tick_and_assert(State::Inactive);
}

#[test]
fn load_dmc_dmadelayed() {
    let mut t = TestStruct::new();

    //    (get) \ CPU writes to $4015     <- DMC enabled
    //    (put) / during this APU cycle   <- DMC enabled
    //    (get) CPU reads
    //    (put) CPU reads
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
    t.tick_and_assert(State::DelayForLoad(2));

    // delay first cpu cycle
    t.tick_and_assert(State::DelayForLoad(1));

    // delay second cpu cycle
    t.tick_and_assert(State::TryHalt {
        halt_on_put: false,
        first_attempt: true,
    });

    // try freeze but cpu is write
    t.expect_is_get_cycle_and_freeze(true, false);
    t.tick_and_assert(State::TryHalt {
        first_attempt: false,
        halt_on_put: false,
    });

    // cpu halt succeed
    t.expect_try_freeze(true);
    t.expect_last_read_addr(0x2234);
    t.tick_and_assert(State::Dummy);

    // dummy
    t.dummy_step();

    // apu is put cycle, need an align cycle
    t.expect_is_get_cycle(false);
    t.expect_read_mem(0x2234, 0x0d);
    t.tick_and_assert(State::Read);

    // apu is get cycle, complete the dma
    t.expect_read_mem(0x1234, 0xcd);
    t.expect_supply_dmc_byte_with(0xcd);
    t.expect_unfreeze();
    t.tick_and_assert(State::Inactive);
}

#[test]
fn reload_dmc_dma() {
    let mut t = TestStruct::new();

    // (halted) (put) CPU reads from address A  <- DMA halt cycle
    // (halted) (get) CPU reads from address A  <- DMA dummy cycle
    // (halted) (put) CPU reads from address A  <- DMA alignment cycle
    // (halted) (get) DMA reads from address B
    //          (put) CPU reads from address A  <- CPU resumes execution

    // get request
    t.expect_take_dmc_dma_request(Some((DmcDmaType::Reload, 0x1234)));
    t.tick_and_assert(State::TryHalt {
        halt_on_put: true,
        first_attempt: true,
    });

    // try freeze and succeed
    t.expect_is_get_cycle_and_freeze(false, true);
    t.expect_last_read_addr(0x2234);
    t.tick_and_assert(State::Dummy);

    // dummy
    t.dummy_step();

    // apu is put cycle, so it is an align cycle, no dma read
    t.expect_is_get_cycle(false);
    t.expect_read_mem(0x2234, 0x00);
    t.tick_and_assert(State::Read);

    // read and supply to apu
    t.expect_read_mem(0x1234, 0xcd);
    t.expect_supply_dmc_byte_with(0xcd);
    t.expect_unfreeze();
    t.tick_and_assert(State::Inactive);
}

#[test]
fn reload_dmc_dma_delayed_1_cycle() {
    let mut t = TestStruct::new();

    //          (put) CPU writes                <- DMA attempts to halt
    // (halted) (get) CPU reads from address A  <- DMA halt cycle
    // (halted) (put) CPU reads from address A  <- DMA dummy cycle
    // (halted) (get) DMA reads from address B
    //          (put) CPU reads from address A  <- CPU resumes execution

    // get request
    t.expect_take_dmc_dma_request(Some((DmcDmaType::Reload, 0x1234)));
    t.tick_and_assert(State::TryHalt {
        halt_on_put: true,
        first_attempt: true,
    });

    // try freeze but cpu is write
    t.expect_is_get_cycle(false);
    t.expect_try_freeze(false);
    t.tick_and_assert(State::TryHalt {
        halt_on_put: true,
        first_attempt: false,
    });

    // try freeze and succeed
    t.expect_try_freeze(true);
    t.expect_last_read_addr(0x2234);
    t.tick_and_assert(State::Dummy);

    // dummy
    t.dummy_step();

    // read and supply to apu
    t.expect_is_get_cycle(true);
    t.expect_read_mem(0x1234, 0xcd);
    t.expect_supply_dmc_byte_with(0xcd);
    t.expect_unfreeze();
    t.tick_and_assert(State::Inactive);
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

    // get request
    t.expect_take_dmc_dma_request(Some((DmcDmaType::Reload, 0x1234)));
    t.tick_and_assert(State::TryHalt {
        halt_on_put: true,
        first_attempt: true,
    });

    // try freeze but cpu is write
    t.expect_is_get_cycle(false);
    t.expect_try_freeze(false);
    t.tick_and_assert(State::TryHalt {
        halt_on_put: true,
        first_attempt: false,
    });

    // try again but cpu still write
    t.expect_try_freeze(false);
    t.tick_and_assert(State::TryHalt {
        halt_on_put: true,
        first_attempt: false,
    });

    // try freeze and succeed
    t.expect_try_freeze(true);
    t.expect_last_read_addr(0x2234);
    t.tick_and_assert(State::Dummy);

    // dummy
    t.dummy_step();

    // need align
    t.expect_is_get_cycle(false);
    t.expect_read_mem(0x2234, 0x8f);
    t.tick_and_assert(State::Read);

    // read and supply to apu
    t.expect_read_mem(0x1234, 0xcd);
    t.expect_supply_dmc_byte_with(0xcd);
    t.expect_unfreeze();
    t.tick_and_assert(State::Inactive);
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

    // get request
    t.expect_take_dmc_dma_request(Some((DmcDmaType::Reload, 0x1234)));
    t.tick_and_assert(State::TryHalt {
        halt_on_put: true,
        first_attempt: true,
    });

    // try freeze but cpu is write
    t.expect_is_get_cycle(false);
    t.expect_try_freeze(false);
    t.tick_and_assert(State::TryHalt {
        halt_on_put: true,
        first_attempt: false,
    });

    // try again but cpu still write
    t.expect_try_freeze(false);
    t.tick_and_assert(State::TryHalt {
        halt_on_put: true,
        first_attempt: false,
    });

    // try again but cpu still write
    t.expect_try_freeze(false);
    t.tick_and_assert(State::TryHalt {
        halt_on_put: true,
        first_attempt: false,
    });

    // try freeze and succeed
    t.expect_try_freeze(true);
    t.expect_last_read_addr(0x2234);
    t.tick_and_assert(State::Dummy);

    // dummy
    t.dummy_step();

    // read and supply to apu
    t.expect_is_get_cycle(true);
    t.expect_read_mem(0x1234, 0xcd);
    t.expect_supply_dmc_byte_with(0xcd);
    t.expect_unfreeze();
    t.tick_and_assert(State::Inactive);
}

struct TestStruct {
    seq: Sequence,
    dma: DmcDma,
    cpu: MockNesDmaSupport,
}

impl TestStruct {
    fn new() -> Self {
        let cpu = MockNesDmaSupport::new();
        let seq = Sequence::new();
        let dma = DmcDma::default();

        Self { seq, dma, cpu }
    }

    fn tick_and_assert(&mut self, exp_state: State) {
        self.dma.tick(&mut self.cpu);
        assert_eq!(self.dma.state, exp_state);
        self.cpu.checkpoint();
        self.cpu.checkpoint();
    }

    fn expect_take_dmc_dma_request(&mut self, req: Option<(DmcDmaType, u16)>) {
        self.cpu
            .expect_take_dmc_dma_request()
            .times(1)
            .in_sequence(&mut self.seq)
            .return_const(req);
    }

    fn expect_try_freeze(&mut self, succeed: bool) {
        self.cpu
            .expect_try_freeze()
            .times(1)
            .in_sequence(&mut self.seq)
            .return_const(succeed);
    }

    fn expect_is_get_cycle(&mut self, is_get: bool) {
        self.cpu
            .expect_is_get_cycle()
            .times(1)
            .in_sequence(&mut self.seq)
            .return_const(is_get);
    }

    fn expect_read_mem(&mut self, addr: u16, result: u8) {
        self.cpu
            .expect_read_mem()
            .with(eq(addr))
            .times(1)
            .in_sequence(&mut self.seq)
            .return_const(result);
    }

    fn expect_unfreeze(&mut self) {
        self.cpu
            .expect_unfreeze()
            .times(1)
            .in_sequence(&mut self.seq)
            .return_const(());
    }

    fn expect_last_read_addr(&mut self, addr: u16) {
        self.cpu
            .expect_last_read_addr()
            .times(1)
            .in_sequence(&mut self.seq)
            .return_const(addr);
    }

    fn expect_supply_dmc_byte_with(&mut self, value: u8) {
        self.cpu
            .expect_supply_dmc_byte()
            .with(eq(value))
            .times(1)
            .in_sequence(&mut self.seq)
            .return_const(());
    }

    fn expect_is_get_cycle_and_freeze(&mut self, is_get: bool, freeze_succeed: bool) {
        self.expect_is_get_cycle(is_get);
        self.expect_try_freeze(freeze_succeed);
    }

    fn dummy_step(&mut self) {
        self.cpu
            .expect_read_mem()
            .with(eq(0x2234u16))
            .times(1)
            .in_sequence(&mut self.seq)
            .return_const(0x00);
        self.dma.tick(&mut self.cpu);
        assert_eq!(State::AlignOrRead, self.dma.state);
        self.cpu.checkpoint();
        self.cpu.checkpoint();
    }
}
