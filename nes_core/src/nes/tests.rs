use super::*;
use crate::SystemClock;
use crate::bus::Bus;
use crate::nes::apu::Apu;
use crate::nes::controller::Button;
use crate::nes::mapper::{Mirroring, TestCartridge};
use crate::render::ImageRender;

fn test_mcu() -> NesMcu<ImageRender<1>, ()> {
    let cartridge = Box::new(TestCartridge::new());
    let cartridge_caps = cartridge.ppu_capabilities();
    NesMcu {
        lower_ram: LowerRam::new(),
        ppu: Ppu::new(ImageRender::<1>::default_dimension(), Mirroring::Horizontal),
        cartridge,
        cartridge_caps,
        controller: Controller::new(),
        apu: Apu::new(()),
        oam_dma_pending: None,
        deferred_apu_writes: Vec::new(),
        last_apu_tick: 0,
        current_clock: SystemClock::default(),
        open_bus: 0,
        joypad1_oe: false,
        joypad2_oe: false,
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
    // Length-register writes land one CPU cycle late; run ticks so the
    // queued store reaches the APU before the status read.
    for t in 0..6u64 {
        mcu.tick_apu(SystemClock(t));
    }

    assert_eq!(mcu.read(0x4015) & 0x01, 0x01);

    mcu.write(0x4015, 0x00);
    assert_eq!(mcu.read(0x4015) & 0x01, 0x00);
}

#[test]
fn test_controller_reads_route_through_nes_mcu() {
    let mut mcu = test_mcu();

    mcu.press_controller_a(Button::A);
    mcu.press_controller_a(Button::Left);
    mcu.write(0x4016, 1); // raise strobe
    mcu.write(0x4016, 0); // falling edge freezes and rewinds

    assert_eq!(mcu.read(0x4016), 0x41); // A pressed (bit 0)
    // Contiguous reads of $4016 keep /OE asserted: NES-001 clocks the
    // shift register once per contiguous set, so this read does not
    // advance the bit position (doc/dma.md "Register conflicts").
    assert_eq!(mcu.read(0x4016), 0x41);

    // Any other bus access deasserts /OE and lets the next read clock.
    mcu.read(0x0000);
    assert_eq!(mcu.read(0x4016), 0x40); // B
    mcu.read(0x0000);
    assert_eq!(mcu.read(0x4016), 0x40); // Select
    mcu.read(0x0000);
    assert_eq!(mcu.read(0x4016), 0x40); // Start
    mcu.read(0x0000);
    assert_eq!(mcu.read(0x4016), 0x40); // Up
    mcu.read(0x0000);
    assert_eq!(mcu.read(0x4016), 0x40); // Down
    mcu.read(0x0000);
    assert_eq!(mcu.read(0x4016), 0x41); // Left pressed (bit 6)
}

/// Back-to-back PPUDATA reads (the page-crossing dummy read of
/// `lda abs,X` followed by the real read, 1 CPU cycle apart) arrive before
/// the PPU finished refilling its buffer: the second read re-returns the
/// previous read's value while still performing the fetch and increment.
#[test]
fn ppudata_double_read_returns_stale_value_but_advances() {
    let mut mcu = test_mcu();

    // VRAM $0000-$0003 = 11 22 33 44.
    mcu.write(0x2006, 0x00);
    mcu.write(0x2006, 0x00);
    for v in [0x11u8, 0x22, 0x33, 0x44] {
        mcu.write(0x2007, v);
    }

    // Rewind to $0000 and prime the buffer with a well-separated read.
    mcu.write(0x2006, 0x00);
    mcu.write(0x2006, 0x00);
    assert_eq!(mcu.read(0x2007), 0x00); // initial buffer, loads VRAM[0]
    for _ in 0..6 {
        mcu.tick_ppu();
    }

    // Normal-paced read: returns VRAM[0], loads VRAM[1].
    let dummy = mcu.read(0x2007);
    // Adjacent-CPU-cycle read: stale return, still fetches VRAM[2].
    let real = mcu.read(0x2007);
    assert_eq!(dummy, 0x11);
    assert_eq!(real, 0x11);

    for _ in 0..6 {
        mcu.tick_ppu();
    }
    // Both increments landed: next paced read sees VRAM[2], then VRAM[3].
    assert_eq!(mcu.read(0x2007), 0x33);
    for _ in 0..6 {
        mcu.tick_ppu();
    }
    assert_eq!(mcu.read(0x2007), 0x44);
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
    let mut bus = Bus::new();

    // Simulate a $4014 write completing on a get cpu cycle.
    let w = SystemClock(11); // 11 % 6 == 5 -> get cycle; 11 % 3 == 2 -> apu clock
    let mut clock = w;
    // Advance to W+3: the first apu tick after the write, where pending is consumed.
    clock = clock.inc();
    clock = clock.inc();
    clock = clock.inc();
    assert!(clock.is_apu_clock());

    mcu.write(0x4014, 0x02); // happens at end of tick W in the real machine
    bus.tick_oam_for_test(&mut mcu, clock, false);

    // Write on get: halt at W+3, first OAM read must be at W+6 (get).
    let next = SystemClock(clock.cycles() + 3);
    assert!(next.is_apu_get_clock());
    // With the correct alignment there are no idle startup cycles left:
    // the very next dma tick performs transfer #0 (the first read).
    let dma = bus.oam_state().expect("dma active");
    assert_eq!(
        dma.startup_cycles, 0,
        "write-on-get needs no alignment cycle"
    );
}
/// OAM DMA completion spans follow the doc/dma.md §OAM DMA diagrams: a
/// $4014 write on a put cycle needs one alignment cycle before the first
/// read (halt + align + 512 = 514 cycles), a write on a get cycle does not
/// (halt + 512 = 513 cycles).
///
/// These spans are pinned end-to-end because they quantize IRQ-relative DMA
/// end positions onto the get/put grid — the cpu_interrupts_v2 sub-test 4
/// (4-irq_and_dma) sweep resolves single CPU clocks through them.
#[test]
fn oam_dma_completion_spans_follow_doc_diagrams() {
    fn completion(w: u64) -> u64 {
        let mut mcu = test_mcu();
        let mut bus = Bus::new();
        mcu.write(0x4014, 0x02); // write executes at the end of tick w
        let mut t = w;
        let mut last_active = w;
        let mut saw_active = false;
        loop {
            t += 1;
            let clock = SystemClock(t);
            if !clock.is_apu_clock() {
                continue;
            }
            if bus.tick_oam_for_test(&mut mcu, clock, false) {
                saw_active = true;
                last_active = t;
            } else if saw_active {
                return last_active;
            }
        }
    }

    // Write on a put cycle (w % 6 == 2): halt lands on a get cycle at w+3,
    // one alignment cycle precedes the transfers (doc/dma.md diagram 2):
    // halt + align + 512 transfers = 514 cycles including the halt.
    // Write on a get cycle (w % 6 == 5): halt lands on a put cycle, no
    // alignment (diagram 1): halt + 512 = 513 cycles.
    let put_write = completion(2);
    let get_write = completion(5);
    assert_eq!(
        put_write - 5,
        1539,
        "write-on-put DMA spans halt + 514 cycles"
    );
    assert_eq!(
        get_write - 8,
        1536,
        "write-on-get DMA spans halt + 513 cycles"
    );
}

/// DMC DMA read colliding with an OAM DMA read: DMC wins the cycle, OAM pauses,
/// then needs one alignment cycle before redoing the read (+2 cycles total).
/// (doc/dma.md §DMC DMA during OAM DMA)
#[test]
fn oam_dma_pauses_on_dmc_read_collision() {
    let mut mcu = test_mcu();
    let mut bus = Bus::new();
    mcu.write(0x4014, 0x02);

    // Consume pending on a put cycle (the halt cycle); write was on get, so
    // startup must be 0 and the first read happens on the very next tick.
    let start = SystemClock(8); // 8 % 6 == 2 -> put; 8 % 3 == 2 -> apu clock
    bus.tick_oam_for_test(&mut mcu, start, false);
    let dma = bus.oam_state().expect("dma active");
    assert_eq!(
        dma.startup_cycles, 0,
        "write-on-get needs no alignment cycle"
    );

    // Next tick is a get cycle where a normal OAM read (tc=0) would happen.
    let mut clock = SystemClock(start.cycles() + 3);
    assert!(clock.is_apu_get_clock());

    // No collision: the read proceeds.
    bus.tick_oam_for_test(&mut mcu, clock, false);
    let dma = bus.oam_state().expect("dma active");
    assert_eq!(dma.transfer_cycle, 1);

    clock = SystemClock(clock.cycles() + 3); // put cycle: tc=1 write

    // Advance one pair so OAM is at a read phase again (tc=2), then collide:
    bus.tick_oam_for_test(&mut mcu, clock, false); // tc=1 write on put
    clock = SystemClock(clock.cycles() + 3); // get: tc=2 read
    bus.tick_oam_for_test(&mut mcu, clock, true); // DMC read collides with OAM read

    // The collided cycle must not advance the transfer...
    let dma = bus.oam_state().expect("dma active after collision");
    assert_eq!(dma.transfer_cycle, 2, "collision must pause the OAM read");

    // ...and the following cycle (put) is an OAM alignment cycle, not the write.
    clock = SystemClock(clock.cycles() + 3);
    assert!(!clock.is_apu_get_clock());
    bus.tick_oam_for_test(&mut mcu, clock, false);
    let dma = bus.oam_state().expect("dma active");
    assert_eq!(dma.transfer_cycle, 2, "alignment cycle must not transfer");

    // Then the read is redone on the next get cycle.
    clock = SystemClock(clock.cycles() + 3);
    assert!(clock.is_apu_get_clock());
    bus.tick_oam_for_test(&mut mcu, clock, false);
    let dma = bus.oam_state().expect("dma active");
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
    let mut bus = Bus::new();
    mcu.write(0x4014, 0x02);

    // Consume pending on a put cycle (shared halt); write was on get so there
    // is no alignment cycle before the first read.
    let h = SystemClock(8); // put cycle
    bus.tick_oam_for_test(&mut mcu, h, false);
    assert_eq!(bus.oam_ref().unwrap().startup_cycles, 0);

    // H+1 (get): first OAM read proceeds under the DMC dummy cycle.
    bus.tick_oam_for_test(&mut mcu, SystemClock(h.cycles() + 3), false);
    assert_eq!(bus.oam_ref().unwrap().transfer_cycle, 1);

    // H+2 (put): first OAM write proceeds under the DMC alignment cycle.
    bus.tick_oam_for_test(&mut mcu, SystemClock(h.cycles() + 6), false);
    assert_eq!(bus.oam_ref().unwrap().transfer_cycle, 2);

    // H+3 (get): DMC drives its byte onto the bus; the OAM read of $xx02 is
    // aborted and will be redone after one alignment cycle.
    bus.tick_oam_for_test(&mut mcu, SystemClock(h.cycles() + 9), true);
    assert_eq!(bus.oam_ref().unwrap().transfer_cycle, 2);
    assert_eq!(bus.oam_ref().unwrap().pause_cycles, 1);

    // H+4 (put): OAM alignment cycle, no transfer.
    bus.tick_oam_for_test(&mut mcu, SystemClock(h.cycles() + 12), false);
    assert_eq!(bus.oam_ref().unwrap().transfer_cycle, 2);

    // H+5 (get): the aborted read is redone.
    bus.tick_oam_for_test(&mut mcu, SystemClock(h.cycles() + 15), false);
    assert_eq!(bus.oam_ref().unwrap().transfer_cycle, 3);
}

#[test]
fn dmc_collision_at_start_of_oam_write_on_put() {
    let mut mcu = test_mcu();
    let mut bus = Bus::new();
    mcu.write(0x4014, 0x02);

    // Consume pending on a get cycle (shared halt); write was on put so one
    // alignment cycle precedes the first read.
    let h = SystemClock(11); // get cycle
    bus.tick_oam_for_test(&mut mcu, h, false);
    assert_eq!(bus.oam_ref().unwrap().startup_cycles, 1);

    // H+1 (put): OAM alignment cycle doubles as the DMC dummy cycle.
    bus.tick_oam_for_test(&mut mcu, SystemClock(h.cycles() + 3), false);
    let dma = bus.oam_ref().unwrap();
    assert_eq!(dma.startup_cycles, 0);
    assert_eq!(dma.transfer_cycle, 0);

    // H+2 (get): DMC drives its byte; the first OAM read is aborted.
    bus.tick_oam_for_test(&mut mcu, SystemClock(h.cycles() + 6), true);
    let dma = bus.oam_ref().unwrap();
    assert_eq!(dma.transfer_cycle, 0);
    assert_eq!(dma.pause_cycles, 1);

    // H+3 (put): OAM alignment cycle, still no transfer.
    bus.tick_oam_for_test(&mut mcu, SystemClock(h.cycles() + 9), false);
    assert_eq!(bus.oam_ref().unwrap().transfer_cycle, 0);

    // H+4 (get): the first OAM read finally happens.
    bus.tick_oam_for_test(&mut mcu, SystemClock(h.cycles() + 12), false);
    assert_eq!(bus.oam_ref().unwrap().transfer_cycle, 1);
}

#[test]
fn test_4017_reads_include_zapper_bits_alongside_controller_b() {
    let mut mcu = test_mcu();

    // Disconnected: $4017 is a plain controller B read (no buttons pressed).
    assert_eq!(mcu.read(0x4017), 0x40);
    assert_eq!(mcu.read(0x4017), 0x40);

    // Connected, aimed at a black pixel (ImageRender starts all-black):
    // every $4017 read now carries the light-sense bit.
    mcu.connect_zapper(true);
    mcu.aim_zapper(10, 10);
    mcu.write(0x4016, 1);
    mcu.write(0x4016, 0);
    assert_eq!(mcu.read(0x4017), 0x48); // no buttons + light absent
    assert_eq!(mcu.read(0x4017), 0x48);

    // Trigger pull ORs bit 4 on top of the controller bits.
    mcu.trigger_zapper();
    mcu.press_controller_b(Button::A);
    mcu.write(0x4016, 1);
    // Trigger pull ORs bit 4 into every read: held trigger + light absent
    // on the black framebuffer = 0x10 | 0x08.
    assert_eq!(mcu.read(0x4017) & 0x18, 0x18);

    // Controller A ($4016) never sees zapper bits.
    assert_eq!(mcu.read(0x4016), 0x40);
}

#[test]
fn test_4017_peek_includes_zapper_bits() {
    let mut mcu = test_mcu();
    mcu.connect_zapper(true);
    mcu.aim_zapper(10, 10);
    assert_eq!(mcu.peek(0x4017) & 0x18, 0x08);
    assert_eq!(mcu.peek(0x4016) & 0x18, 0x00);
}

/// Regression: `reset` used to zero `last_apu_tick` while the master clock
/// kept running, so deferred length-counter writes were anchored against a
/// restarted timeline and could flush immediately instead of one CPU cycle
/// after their issuing store. After the fix they anchor to the running clock.
#[test]
fn reset_reanchors_deferred_apu_writes_to_running_clock() {
    let big = 5_000_000_u64;
    let mut mcu = test_mcu();
    mcu.reset(SystemClock(big));

    // Enable the triangle channel (direct register, not deferred).
    mcu.write(0x4015, 0x04);
    // Arm a deferred length-counter reload: it must land ~one CPU cycle
    // after this write cycle relative to the running clock.
    mcu.write(0x400B, 0x08);
    // Before the anchored landing point the deferred reload must not have
    // flushed: triangle status bit (bit 2 of $4015) stays clear.
    mcu.tick_apu(SystemClock(big + 2));
    let status = mcu.read(0x4015);
    assert_eq!(
        status & 0x04,
        0,
        "length counter reloaded before the deferred write landed"
    );

    // Crossing the landing point flushes the deferred write and the channel
    // becomes active.
    mcu.tick_apu(SystemClock(big + 3));
    let status = mcu.read(0x4015);
    assert_ne!(status & 0x04, 0, "deferred reload never landed");
}
