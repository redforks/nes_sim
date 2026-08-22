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
