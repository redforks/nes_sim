use super::*;
use crate::SystemClock;
use crate::nes::apu::Apu;
use crate::nes::controller::Button;
use crate::nes::mapper::{Mirroring, TestCartridge};
use crate::render::ImageRender;

fn test_mcu() -> NesMcu<ImageRender, ()> {
    NesMcu {
        lower_ram: LowerRam::new(),
        ppu: Ppu::new(
            ImageRender::default_dimension(),
            Mirroring::Horizontal,
            Box::new(TestCartridge::new()),
        ),
        controller: Controller::new(),
        apu: Apu::new(()),
        oam_dma_pending: None,
        oam_dma: None,
        open_bus: 0,
    }
}

#[test]
fn test_lower_ram_mirroring() {
    let mut mcu = test_mcu();

    mcu.write(0x0000, 0x42);
    assert_eq!(mcu.read(0x0000), 0x42);
    assert_eq!(mcu.read(0x0800), 0x42);
    assert_eq!(mcu.read(0x1000), 0x42);
    assert_eq!(mcu.read(0x1800), 0x42);
}

#[test]
fn test_frame_counter_inhibit_clears_irq() {
    let mut mcu = test_mcu();

    mcu.write(0x4017, 0x00);
    let mut clock = SystemClock::default();
    for _ in 0..(29_829 * 3) {
        mcu.tick_apu(clock);
        clock = clock.inc();
    }

    mcu.write(0x4017, 0x40);
    assert_eq!(mcu.read(0x4015) & 0x40, 0);
}

#[test]
fn test_length_counter_status_comes_from_apu_controller() {
    let mut mcu = test_mcu();

    mcu.write(0x4000, 0x00);
    mcu.write(0x4002, 0x34);
    mcu.write(0x4015, 0x01);
    mcu.write(0x4003, 0xF8);

    assert_eq!(mcu.read(0x4015) & 0x01, 0x01);

    mcu.write(0x4015, 0x00);
    assert_eq!(mcu.read(0x4015) & 0x01, 0x00);
}

#[test]
fn test_controller_reads_route_through_nes_mcu() {
    let mut mcu = test_mcu();

    mcu.press_controller_a(Button::A);
    mcu.press_controller_a(Button::Left);
    mcu.write(0x4016, 0);

    assert_eq!(mcu.read(0x4016), 0x41);
    assert_eq!(mcu.read(0x4016), 0x40);
    assert_eq!(mcu.read(0x4016), 0x40);
    assert_eq!(mcu.read(0x4016), 0x40);
    assert_eq!(mcu.read(0x4016), 0x40);
    assert_eq!(mcu.read(0x4016), 0x40);
    assert_eq!(mcu.read(0x4016), 0x41);
}

/// OAM DMA startup alignment: the alignment decision must be based on the
/// parity of the $4014 write cycle, not the cycle after it.
///
/// Hardware (doc/dma.md §OAM DMA):
/// - write on get: halt(put), read(get)            -> first read 2 cpu cycles after write
/// - write on put: halt(get), align(put), read(get) -> first read 3 cpu cycles after write
///
/// The write executes during `cpu.tick()` at the end of system tick W; the DMA
/// unit first sees the request on tick W+3 (opposite parity of W).
#[test]
fn oam_dma_first_read_alignment() {
    let mut mcu = test_mcu();

    // Simulate a $4014 write completing on an even (get) cpu cycle.
    let w = SystemClock(8); // 8 % 6 == 2 -> get cycle; 8 % 3 == 2 -> apu clock
    let mut clock = w;
    // Advance to W+3: the first apu tick after the write, where pending is consumed.
    clock = clock.inc();
    clock = clock.inc();
    clock = clock.inc();
    assert!(clock.is_apu_clock());

    mcu.write(0x4014, 0x02); // happens at end of tick W in the real machine
    mcu.tick_oam_dma(clock, false);

    // Write on get: halt at W+3, first OAM read must be at W+6 (get).
    let next = SystemClock(clock.cycles() + 3);
    assert!(next.is_apu_get_clock());
    // With the correct alignment there are no idle startup cycles left:
    // the very next dma tick performs transfer #0 (the first read).
    let dma = mcu.oam_dma.expect("dma active");
    assert_eq!(
        dma.startup_cycles, 0,
        "write-on-get needs no alignment cycle"
    );
}

/// DMC DMA read colliding with an OAM DMA read: DMC wins the cycle, OAM pauses,
/// then needs one alignment cycle before redoing the read (+2 cycles total).
/// (doc/dma.md §DMC DMA during OAM DMA)
#[test]
fn oam_dma_pauses_on_dmc_read_collision() {
    let mut mcu = test_mcu();
    mcu.write(0x4014, 0x02);

    // Consume pending on a put cycle (the halt cycle); write was on get, so
    // startup must be 0 and the first read happens on the very next tick.
    let start = SystemClock(5); // 5 % 6 == 5 -> put; 5 % 3 == 2 -> apu clock
    mcu.tick_oam_dma(start, false);
    let dma = mcu.oam_dma.expect("dma active");
    assert_eq!(
        dma.startup_cycles, 0,
        "write-on-get needs no alignment cycle"
    );

    // Next tick is a get cycle where a normal OAM read (tc=0) would happen.
    let mut clock = SystemClock(start.cycles() + 3);
    assert!(clock.is_apu_get_clock());

    // No collision: the read proceeds.
    mcu.tick_oam_dma(clock, false);
    let dma = mcu.oam_dma.expect("dma active");
    assert_eq!(dma.transfer_cycle, 1);

    clock = SystemClock(clock.cycles() + 3); // put cycle: tc=1 write

    // Advance one pair so OAM is at a read phase again (tc=2), then collide:
    mcu.tick_oam_dma(clock, false); // tc=1 write on put
    clock = SystemClock(clock.cycles() + 3); // get: tc=2 read
    mcu.tick_oam_dma(clock, true); // DMC read collides with OAM read

    // The collided cycle must not advance the transfer...
    let dma = mcu.oam_dma.expect("dma active after collision");
    assert_eq!(dma.transfer_cycle, 2, "collision must pause the OAM read");

    // ...and the following cycle (put) is an OAM alignment cycle, not the write.
    clock = SystemClock(clock.cycles() + 3);
    assert!(!clock.is_apu_get_clock());
    mcu.tick_oam_dma(clock, false);
    let dma = mcu.oam_dma.expect("dma active");
    assert_eq!(dma.transfer_cycle, 2, "alignment cycle must not transfer");

    // Then the read is redone on the next get cycle.
    clock = SystemClock(clock.cycles() + 3);
    assert!(clock.is_apu_get_clock());
    mcu.tick_oam_dma(clock, false);
    let dma = mcu.oam_dma.expect("dma active");
    assert_eq!(dma.transfer_cycle, 3, "read redone, transfer resumed");
}

/// DMC DMA landing at the very start of an OAM DMA whose $4014 write was on a
/// get cycle: the halt cycle is shared, and the DMC dummy/alignment cycles
/// overlap the OAM transfer's first read/write. Only the DMC get itself and
/// one OAM re-alignment cycle cost extra (+2 total).
///
/// doc/dma.md §DMC DMA during OAM DMA, example "DMC DMA at the start of OAM
/// DMA (write on get), taking 2 cycles":
/// ```text
///       (get) CPU writes to $4014
/// (halted) (put) <- shared DMA halt cycle
/// (halted) (get) OAM reads $xx00   <- DMC dummy cycle
/// (halted) (put) OAM writes $2004  <- DMC alignment cycle
/// (halted) (get) DMC reads B       <- OAM paused
/// (halted) (put)                   <- OAM alignment cycle
/// (halted) (get) OAM reads $xx01
/// ```
#[test]
fn dmc_collision_at_start_of_oam_write_on_get() {
    let mut mcu = test_mcu();
    mcu.write(0x4014, 0x02);

    // Consume pending on a put cycle (shared halt); write was on get so there
    // is no alignment cycle before the first read.
    let h = SystemClock(5); // put cycle
    mcu.tick_oam_dma(h, false);
    assert_eq!(mcu.oam_dma.as_ref().unwrap().startup_cycles, 0);

    // H+1 (get): first OAM read proceeds under the DMC dummy cycle.
    mcu.tick_oam_dma(SystemClock(h.cycles() + 3), false);
    assert_eq!(mcu.oam_dma.as_ref().unwrap().transfer_cycle, 1);

    // H+2 (put): first OAM write proceeds under the DMC alignment cycle.
    mcu.tick_oam_dma(SystemClock(h.cycles() + 6), false);
    assert_eq!(mcu.oam_dma.as_ref().unwrap().transfer_cycle, 2);

    // H+3 (get): DMC drives its byte onto the bus; the OAM read of $xx02 is
    // aborted and will be redone after one alignment cycle.
    mcu.tick_oam_dma(SystemClock(h.cycles() + 9), true);
    assert_eq!(mcu.oam_dma.as_ref().unwrap().transfer_cycle, 2);
    assert_eq!(mcu.oam_dma.as_ref().unwrap().pause_cycles, 1);

    // H+4 (put): OAM alignment cycle, no transfer.
    mcu.tick_oam_dma(SystemClock(h.cycles() + 12), false);
    assert_eq!(mcu.oam_dma.as_ref().unwrap().transfer_cycle, 2);

    // H+5 (get): the aborted read is redone.
    mcu.tick_oam_dma(SystemClock(h.cycles() + 15), false);
    assert_eq!(mcu.oam_dma.as_ref().unwrap().transfer_cycle, 3);
}

/// Same as [`dmc_collision_at_start_of_oam_write_on_get`] but for a $4014
/// write on a put cycle: the OAM alignment cycle doubles as the DMC dummy
/// cycle, then the DMC get pauses the not-yet-started OAM read.
///
/// doc/dma.md §DMC DMA during OAM DMA, example "DMC DMA at the start of OAM
/// DMA (write on put), taking 2 cycles":
/// ```text
///       (put) CPU writes to $4014   <- DMC attempts to halt
/// (halted) (get)                    <- shared DMA halt cycle
/// (halted) (put)                    <- DMC dummy + OAM alignment
/// (halted) (get) DMC reads B        <- OAM paused
/// (halted) (put)                    <- OAM alignment cycle
/// (halted) (get) OAM reads $xx00
/// ```
#[test]
fn dmc_collision_at_start_of_oam_write_on_put() {
    let mut mcu = test_mcu();
    mcu.write(0x4014, 0x02);

    // Consume pending on a get cycle (shared halt); write was on put so one
    // alignment cycle precedes the first read.
    let h = SystemClock(8); // get cycle
    mcu.tick_oam_dma(h, false);
    assert_eq!(mcu.oam_dma.as_ref().unwrap().startup_cycles, 1);

    // H+1 (put): OAM alignment cycle doubles as the DMC dummy cycle.
    mcu.tick_oam_dma(SystemClock(h.cycles() + 3), false);
    let dma = mcu.oam_dma.as_ref().unwrap();
    assert_eq!(dma.startup_cycles, 0);
    assert_eq!(dma.transfer_cycle, 0);

    // H+2 (get): DMC drives its byte; the first OAM read is aborted.
    mcu.tick_oam_dma(SystemClock(h.cycles() + 6), true);
    let dma = mcu.oam_dma.as_ref().unwrap();
    assert_eq!(dma.transfer_cycle, 0);
    assert_eq!(dma.pause_cycles, 1);

    // H+3 (put): OAM alignment cycle, still no transfer.
    mcu.tick_oam_dma(SystemClock(h.cycles() + 9), false);
    assert_eq!(mcu.oam_dma.as_ref().unwrap().transfer_cycle, 0);

    // H+4 (get): the first OAM read finally happens.
    mcu.tick_oam_dma(SystemClock(h.cycles() + 12), false);
    assert_eq!(mcu.oam_dma.as_ref().unwrap().transfer_cycle, 1);
}
