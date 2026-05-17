use super::*;
use mockall::predicate::*;
use mockall::*;

#[test]
fn dma_not_activated() {
    let mut cpu = MockNesDmaSupport::new();
    let mut dma = DmcDma::default();

    cpu.expect_take_dmc_dma_request()
        .times(1)
        .return_const(None);
    dma.tick(&mut cpu);
    assert_eq!(State::Inactive, dma.state);
}

#[test]
fn load_dmc_dma() {
    let mut cpu = MockNesDmaSupport::new();
    let mut seq = Sequence::new();
    let mut dma = DmcDma::default();

    // get request
    cpu.expect_take_dmc_dma_request()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(Some((DmcDmaType::Load, 0x1234)));
    dma.tick(&mut cpu);
    assert_eq!(State::DelayForLoad(2), dma.state);
    cpu.checkpoint();
    cpu.checkpoint();

    // delay first cpu cycle
    dma.tick(&mut cpu);
    assert_eq!(State::DelayForLoad(1), dma.state);

    // delay second cpu cycle
    dma.tick(&mut cpu);
    assert_eq!(
        State::TryHalt {
            halt_on_put: false,
            first_attempt: true
        },
        dma.state
    );

    // try freeze and succeed
    cpu.expect_is_get_cycle()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(true);
    cpu.expect_try_freeze()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(true);
    dma.tick(&mut cpu);
    assert_eq!(State::Dummy, dma.state);
    cpu.checkpoint();
    cpu.checkpoint();

    // dummy
    dma.tick(&mut cpu);
    assert_eq!(State::AlignOrRead, dma.state);
    cpu.checkpoint();
    cpu.checkpoint();

    // apu is put cycle, so it is an align cycle, no dma read
    cpu.expect_is_get_cycle()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(false);
    dma.tick(&mut cpu);
    assert_eq!(State::Read, dma.state);
    cpu.checkpoint();
    cpu.checkpoint();

    // read and supply to apu
    cpu.expect_read_mem()
        .with(eq(0x1234))
        .times(1)
        .in_sequence(&mut seq)
        .return_const(0xcd);
    cpu.expect_supply_dmc_byte()
        .with(eq(0xcd))
        .times(1)
        .in_sequence(&mut seq)
        .return_const(());
    cpu.expect_unfreeze()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(());
    dma.tick(&mut cpu);
    assert_eq!(State::Inactive, dma.state);
}

#[test]
fn reload_dmc_dma() {
    let mut cpu = MockNesDmaSupport::new();
    let mut seq = Sequence::new();
    let mut dma = DmcDma::default();

    // get request
    cpu.expect_take_dmc_dma_request()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(Some((DmcDmaType::Reload, 0x1234)));
    dma.tick(&mut cpu);
    assert_eq!(
        State::TryHalt {
            halt_on_put: true,
            first_attempt: true
        },
        dma.state
    );
    cpu.checkpoint();
    cpu.checkpoint();

    // try freeze but cpu is get
    cpu.expect_is_get_cycle()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(true);
    dma.tick(&mut cpu);
    assert_eq!(
        State::TryHalt {
            halt_on_put: true,
            first_attempt: true
        },
        dma.state
    );
    cpu.checkpoint();
    cpu.checkpoint();

    // try freeze cpu in put cycle but failed
    cpu.expect_is_get_cycle()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(false);
    cpu.expect_try_freeze()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(false);
    dma.tick(&mut cpu);
    assert_eq!(
        State::TryHalt {
            halt_on_put: true,
            first_attempt: false
        },
        dma.state
    );
    cpu.checkpoint();
    cpu.checkpoint();

    // try freeze cpu again and succeed
    cpu.expect_try_freeze()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(true);
    dma.tick(&mut cpu);
    assert_eq!(State::Dummy, dma.state);
    cpu.checkpoint();
    cpu.checkpoint();

    // dummy
    dma.tick(&mut cpu);
    assert_eq!(State::AlignOrRead, dma.state);
    cpu.checkpoint();
    cpu.checkpoint();

    // apu is put cycle, so it is an align cycle, no dma read
    cpu.expect_is_get_cycle()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(false);
    dma.tick(&mut cpu);
    assert_eq!(State::Read, dma.state);
    cpu.checkpoint();
    cpu.checkpoint();

    // read and supply to apu
    cpu.expect_read_mem()
        .with(eq(0x1234))
        .times(1)
        .in_sequence(&mut seq)
        .return_const(0xcd);
    cpu.expect_supply_dmc_byte()
        .with(eq(0xcd))
        .times(1)
        .in_sequence(&mut seq)
        .return_const(());
    cpu.expect_unfreeze()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(());
    dma.tick(&mut cpu);
    assert_eq!(State::Inactive, dma.state);
}
