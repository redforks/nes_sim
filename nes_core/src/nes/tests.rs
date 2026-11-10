use super::*;
use crate::SystemClock;
use crate::bus::Bus;
use crate::dma_bus::{DmaBus, OamDmaMcu};
use crate::ines::INesFile;
use crate::nes::apu::Apu;
use crate::nes::controller::Button;
use crate::nes::mapper::Cartridge;
use crate::nes::mapper::{Mmc3IrqOverride, create_cartridge_with_mmc3_irq_override};
use crate::render::ImageRender;
use crate::test_utils::{McuCall, RecordingMcu};

fn test_mcu() -> NesMcu<ImageRender<1>, ()> {
    test_mcu_with(Cartridge::test())
}

fn test_mcu_with(cartridge: Cartridge) -> NesMcu<ImageRender<1>, ()> {
    NesMcu {
        lower_ram: LowerRam::new(),
        ppu: Ppu::new(ImageRender::<1>::default_dimension()),
        cartridge,
        controller: Controller::new(),
        apu: Apu::new(()),
        oam_dma_pending: None,
        deferred_apu_writes: ArrayDeque::new(),
        open_bus: 0,
        joypad1_oe: false,
        joypad2_oe: false,
    }
}

#[test]
fn test_lower_ram_mirroring() {
    let mut mcu = test_mcu();

    mcu.write(0x0000, 0x42, SystemClock::default());
    assert_eq!(mcu.read(0x0000), 0x42);
    assert_eq!(mcu.read(0x0800), 0x42);
    assert_eq!(mcu.read(0x1000), 0x42);
    assert_eq!(mcu.read(0x1800), 0x42);
}

#[test]
fn test_frame_counter_inhibit_clears_irq() {
    let mut mcu = test_mcu();

    mcu.write(0x4017, 0x00, SystemClock::default());
    // Step the APU on APU clocks (dot % 3 == 2) for one full 29,830-cycle
    // mode-0 period, through the frame IRQ set at CPU cycle 29,828.
    let mut clock = SystemClock(2);
    mcu.tick_apu(clock);
    for _ in 0..29_828 {
        clock = clock.inc().inc().inc();
        mcu.tick_apu(clock);
    }

    mcu.write(0x4017, 0x40, SystemClock::default());
    assert_eq!(mcu.read(0x4015) & 0x40, 0);
}

#[test]
fn test_length_counter_status_comes_from_apu_controller() {
    let mut mcu = test_mcu();

    mcu.write(0x4000, 0x00, SystemClock::default());
    mcu.write(0x4002, 0x34, SystemClock::default());
    mcu.write(0x4015, 0x01, SystemClock::default());
    mcu.write(0x4003, 0xF8, SystemClock::default());
    // Length-register writes land one CPU cycle late; step APU clocks
    // (dot % 3 == 2) so the queued store lands before the status read.
    for cpu_cycle in 0..2u64 {
        mcu.tick_apu(SystemClock(cpu_cycle * 3 + 2));
    }

    assert_eq!(mcu.read(0x4015) & 0x01, 0x01);

    mcu.write(0x4015, 0x00, SystemClock::default());
    assert_eq!(mcu.read(0x4015) & 0x01, 0x00);
}

#[test]
fn test_controller_reads_route_through_nes_mcu() {
    let mut mcu = test_mcu();

    mcu.press_controller_a(Button::A);
    mcu.press_controller_a(Button::Left);
    mcu.write(0x4016, 1, SystemClock::default()); // raise strobe
    mcu.write(0x4016, 0, SystemClock::default()); // falling edge freezes and rewinds

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

/// The fast path may skip address dispatch, never an observable side
/// effect: a zero-page read latches the open bus with the value read,
/// exactly as the generic route does.
#[test]
fn zero_page_read_latches_open_bus_like_generic_read() {
    fn open_bus_after_read(generic: bool) -> u8 {
        let mut mcu = test_mcu();
        mcu.write(0x0010, 0x5A, SystemClock::default());
        // Park the latch on a different value so only the read below can
        // move it to $5A.
        mcu.write(0x0020, 0x00, SystemClock::default());

        let value = if generic {
            mcu.read(0x0010)
        } else {
            mcu.read_zero_page(0x10)
        };
        assert_eq!(value, 0x5A);
        mcu.read(0x4020)
    }

    assert_eq!(open_bus_after_read(true), 0x5A, "generic read latches");
    assert_eq!(open_bus_after_read(false), 0x5A, "fast read latches");
}

/// The stack-page read fast path latches the open bus like the generic
/// route, so the "last value read by the CPU" stays true for pulls too.
#[test]
fn stack_page_read_latches_open_bus_like_generic_read() {
    fn open_bus_after_read(generic: bool) -> u8 {
        let mut mcu = test_mcu();
        mcu.write(0x0180, 0xC3, SystemClock::default());
        mcu.write(0x0020, 0x00, SystemClock::default());

        let value = if generic {
            mcu.read(0x0180)
        } else {
            mcu.read_stack_page(0x80)
        };
        assert_eq!(value, 0xC3);
        mcu.read(0x4020)
    }

    assert_eq!(open_bus_after_read(true), 0xC3, "generic read latches");
    assert_eq!(open_bus_after_read(false), 0xC3, "fast read latches");
}

/// The stack-page write fast path latches the open bus identically to the
/// same write through the generic route.
#[test]
fn stack_page_write_latches_open_bus_like_generic_write() {
    fn open_bus_after_write(generic: bool) -> u8 {
        let mut mcu = test_mcu();
        if generic {
            mcu.write(0x0180, 0x7E, SystemClock::default());
        } else {
            mcu.write_stack_page(0x80, 0x7E, SystemClock::default());
        }
        mcu.read(0x4020)
    }

    assert_eq!(open_bus_after_write(true), 0x7E, "generic write latches");
    assert_eq!(open_bus_after_write(false), 0x7E, "fast write latches");
}

/// A zero-page store clears both joypad /OE lines, exactly like any other
/// store: two $4016 reads separated by one are not contiguous, and the bits
/// they return match a generic store's schedule.
#[test]
fn zero_page_store_clears_joypad_oe_lines() {
    fn poll(generic_store: bool) -> Vec<u8> {
        let mut mcu = test_mcu();
        // A pressed (stage 0) and Select pressed (stage 2), so a skipped
        // clock would be visible as a wrong bit.
        mcu.press_controller_a(Button::A);
        mcu.press_controller_a(Button::Select);
        mcu.press_controller_b(Button::A);
        mcu.press_controller_b(Button::Select);
        mcu.write(0x4016, 1, SystemClock::default());
        mcu.write(0x4016, 0, SystemClock::default());

        let mut bits = Vec::new();
        for _ in 0..4 {
            bits.push(mcu.read(0x4016) & 0x01);
            if generic_store {
                mcu.write(0x0010, 0, SystemClock::default());
            } else {
                mcu.write_zero_page(0x10, 0, SystemClock::default());
            }
        }
        bits
    }

    assert_eq!(poll(false), poll(true));
    assert_eq!(poll(false), vec![0x01, 0x00, 0x01, 0x00]);

    // The same rule holds on the port-2 /OE line ($4017).
    let mut mcu = test_mcu();
    mcu.press_controller_b(Button::A);
    mcu.press_controller_b(Button::Select);
    mcu.write(0x4016, 1, SystemClock::default());
    mcu.write(0x4016, 0, SystemClock::default());

    assert_eq!(mcu.read(0x4017) & 0x01, 0x01); // A
    mcu.write_zero_page(0x10, 0, SystemClock::default());
    assert_eq!(mcu.read(0x4017) & 0x01, 0x00); // B: not contiguous
    assert_eq!(mcu.read(0x4017) & 0x01, 0x00); // contiguous: no advance
}

/// Back-to-back PPUDATA reads (the page-crossing dummy read of
/// `lda abs,X` followed by the real read, 1 CPU cycle apart) land inside
/// the 6-dot ignore window: hardware fully ignores the second read — it
/// re-returns the previous read's value with no address increment, no
/// read-buffer refill and no mapper notification (Mesen2).
#[test]
fn ppudata_second_read_fully_ignored_within_window() {
    let mut mcu = test_mcu();

    // VRAM $0000-$0003 = 11 22 33 44.
    mcu.write(0x2006, 0x00, SystemClock::default());
    mcu.write(0x2006, 0x00, SystemClock::default());
    for v in [0x11u8, 0x22, 0x33, 0x44] {
        mcu.write(0x2007, v, SystemClock::default());
    }

    // Rewind to $0000 and prime the buffer with a well-separated read.
    mcu.write(0x2006, 0x00, SystemClock::default());
    mcu.write(0x2006, 0x00, SystemClock::default());
    assert_eq!(mcu.read(0x2007), 0x00); // initial buffer, loads VRAM[0]
    // 9 dots (3 CPU cycles) clear the 6-dot ignore window: the next read
    // is normal-paced.
    for _ in 0..9 {
        mcu.tick_ppu();
    }

    // Normal-paced read: returns buffered VRAM[0], loads VRAM[1],
    // increments the address.
    let dummy = mcu.read(0x2007);
    // Same-cycle read: fully ignored — stale value, no increment, no
    // refill, no mapper notification.
    let real = mcu.read(0x2007);
    assert_eq!(dummy, 0x11);
    assert_eq!(real, 0x11);

    // The ignored read must not have advanced the address: paced reads
    // continue from VRAM[1] (only `dummy`'s increment landed), then VRAM[2].
    for _ in 0..9 {
        mcu.tick_ppu();
    }
    assert_eq!(mcu.read(0x2007), 0x22);
    for _ in 0..9 {
        mcu.tick_ppu();
    }
    assert_eq!(mcu.read(0x2007), 0x33);
    for _ in 0..9 {
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
    let mut bus = Bus::new(test_mcu());
    let mut dma_bus = DmaBus::new();

    // Simulate a $4014 write completing on a get cpu cycle.
    let w = SystemClock(11); // 11 % 6 == 5 -> get cycle; 11 % 3 == 2 -> apu clock
    let mut clock = w;
    // Advance to W+3: the first apu tick after the write, where pending is consumed.
    clock = clock.inc();
    clock = clock.inc();
    clock = clock.inc();
    assert!(clock.is_apu_clock());

    bus.mcu_mut().write(0x4014, 0x02, SystemClock::default()); // happens at end of tick W in the real machine
    bus.set_clock(clock);
    dma_bus.tick_oam_for_test(&mut bus, false);

    // Write on get: halt at W+3, first OAM read must be at W+6 (get).
    let next = SystemClock(clock.cycles() + 3);
    assert!(next.is_apu_get_clock());
    // With the correct alignment there are no idle startup cycles left:
    // the very next dma tick performs transfer #0 (the first read).
    let dma = dma_bus.oam_state().expect("dma active");
    assert_eq!(
        dma.needs_startup_align, false,
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
        let mut bus = Bus::new(test_mcu());
        let mut dma_bus = DmaBus::new();
        bus.mcu_mut().write(0x4014, 0x02, SystemClock::default()); // write executes at the end of tick w
        let mut t = w;
        let mut last_active = w;
        let mut saw_active = false;
        loop {
            t += 1;
            let clock = SystemClock(t);
            if !clock.is_apu_clock() {
                continue;
            }
            bus.set_clock(clock);
            if dma_bus.tick_oam_for_test(&mut bus, false) {
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
    let mut bus = Bus::new(test_mcu());
    let mut dma_bus = DmaBus::new();
    bus.mcu_mut().write(0x4014, 0x02, SystemClock::default());

    // Consume pending on a put cycle (the halt cycle); write was on get, so
    // startup must be 0 and the first read happens on the very next tick.
    let start = SystemClock(8); // 8 % 6 == 2 -> put; 8 % 3 == 2 -> apu clock
    bus.set_clock(start);
    dma_bus.tick_oam_for_test(&mut bus, false);
    let dma = dma_bus.oam_state().expect("dma active");
    assert_eq!(
        dma.needs_startup_align, false,
        "write-on-get needs no alignment cycle"
    );

    // Next tick is a get cycle where a normal OAM read (tc=0) would happen.
    let mut clock = SystemClock(start.cycles() + 3);
    assert!(clock.is_apu_get_clock());

    // No collision: the read proceeds.
    bus.set_clock(clock);
    dma_bus.tick_oam_for_test(&mut bus, false);
    let dma = dma_bus.oam_state().expect("dma active");
    assert_eq!(dma.transfer_cycle, 1);

    clock = SystemClock(clock.cycles() + 3); // put cycle: tc=1 write

    // Advance one pair so OAM is at a read phase again (tc=2), then collide:
    bus.set_clock(clock);
    dma_bus.tick_oam_for_test(&mut bus, false); // tc=1 write on put
    clock = SystemClock(clock.cycles() + 3); // get: tc=2 read
    bus.set_clock(clock);
    dma_bus.tick_oam_for_test(&mut bus, true); // DMC read collides with OAM read

    // The collided cycle must not advance the transfer...
    let dma = dma_bus.oam_state().expect("dma active after collision");
    assert_eq!(dma.transfer_cycle, 2, "collision must pause the OAM read");

    // ...and the following cycle (put) is an OAM alignment cycle, not the write.
    clock = SystemClock(clock.cycles() + 3);
    assert!(!clock.is_apu_get_clock());
    bus.set_clock(clock);
    dma_bus.tick_oam_for_test(&mut bus, false);
    let dma = dma_bus.oam_state().expect("dma active");
    assert_eq!(dma.transfer_cycle, 2, "alignment cycle must not transfer");

    // Then the read is redone on the next get cycle.
    clock = SystemClock(clock.cycles() + 3);
    assert!(clock.is_apu_get_clock());
    bus.set_clock(clock);
    dma_bus.tick_oam_for_test(&mut bus, false);
    let dma = dma_bus.oam_state().expect("dma active");
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
    let mut bus = Bus::new(test_mcu());
    let mut dma_bus = DmaBus::new();
    bus.mcu_mut().write(0x4014, 0x02, SystemClock::default());

    // Consume pending on a put cycle (shared halt); write was on get so there
    // is no alignment cycle before the first read.
    let h = SystemClock(8); // put cycle
    bus.set_clock(h);
    dma_bus.tick_oam_for_test(&mut bus, false);
    assert_eq!(dma_bus.oam_ref().unwrap().needs_startup_align, false);

    // H+1 (get): first OAM read proceeds under the DMC dummy cycle.
    bus.set_clock(SystemClock(h.cycles() + 3));
    dma_bus.tick_oam_for_test(&mut bus, false);
    assert_eq!(dma_bus.oam_ref().unwrap().transfer_cycle, 1);

    // H+2 (put): first OAM write proceeds under the DMC alignment cycle.
    bus.set_clock(SystemClock(h.cycles() + 6));
    dma_bus.tick_oam_for_test(&mut bus, false);
    assert_eq!(dma_bus.oam_ref().unwrap().transfer_cycle, 2);

    // H+3 (get): DMC drives its byte onto the bus; the OAM read of $xx02 is
    // aborted and will be redone after one alignment cycle.
    bus.set_clock(SystemClock(h.cycles() + 9));
    dma_bus.tick_oam_for_test(&mut bus, true);
    assert_eq!(dma_bus.oam_ref().unwrap().transfer_cycle, 2);
    assert_eq!(dma_bus.oam_ref().unwrap().needs_pause_align, true);

    // H+4 (put): OAM alignment cycle, no transfer.
    bus.set_clock(SystemClock(h.cycles() + 12));
    dma_bus.tick_oam_for_test(&mut bus, false);
    assert_eq!(dma_bus.oam_ref().unwrap().transfer_cycle, 2);

    // H+5 (get): the aborted read is redone.
    bus.set_clock(SystemClock(h.cycles() + 15));
    dma_bus.tick_oam_for_test(&mut bus, false);
    assert_eq!(dma_bus.oam_ref().unwrap().transfer_cycle, 3);
}

#[test]
fn dmc_collision_at_start_of_oam_write_on_put() {
    let mut bus = Bus::new(test_mcu());
    let mut dma_bus = DmaBus::new();
    bus.mcu_mut().write(0x4014, 0x02, SystemClock::default());

    // Consume pending on a get cycle (shared halt); write was on put so one
    // alignment cycle precedes the first read.
    let h = SystemClock(11); // get cycle
    bus.set_clock(h);
    dma_bus.tick_oam_for_test(&mut bus, false);
    assert_eq!(dma_bus.oam_ref().unwrap().needs_startup_align, true);

    // H+1 (put): OAM alignment cycle doubles as the DMC dummy cycle.
    bus.set_clock(SystemClock(h.cycles() + 3));
    dma_bus.tick_oam_for_test(&mut bus, false);
    let dma = dma_bus.oam_ref().unwrap();
    assert_eq!(dma.needs_startup_align, false);
    assert_eq!(dma.transfer_cycle, 0);

    // H+2 (get): DMC drives its byte; the first OAM read is aborted.
    bus.set_clock(SystemClock(h.cycles() + 6));
    dma_bus.tick_oam_for_test(&mut bus, true);
    let dma = dma_bus.oam_ref().unwrap();
    assert_eq!(dma.transfer_cycle, 0);
    assert_eq!(dma.needs_pause_align, true);

    // H+3 (put): OAM alignment cycle, still no transfer.
    bus.set_clock(SystemClock(h.cycles() + 9));
    dma_bus.tick_oam_for_test(&mut bus, false);
    assert_eq!(dma_bus.oam_ref().unwrap().transfer_cycle, 0);

    // H+4 (get): the first OAM read finally happens.
    bus.set_clock(SystemClock(h.cycles() + 12));
    dma_bus.tick_oam_for_test(&mut bus, false);
    assert_eq!(dma_bus.oam_ref().unwrap().transfer_cycle, 1);
}

/// Drive an armed `$4014` transfer (caller writes the source page first) to
/// completion. Returns `None` if the transfer never started or did not
/// finish, so a missing DMA fails the test instead of hanging it; otherwise
/// the number of APU clocks the transfer held the bus, so cycle counts can
/// be compared across source pages.
fn drive_oam_dma<M: OamDmaMcu>(bus: &mut Bus<M>, dma_bus: &mut DmaBus) -> Option<usize> {
    let mut t = SystemClock(11);
    let mut ran = false;
    let mut apu_clocks = 0;
    for _ in 0..2048 {
        t = t.inc();
        if !t.is_apu_clock() {
            continue;
        }
        bus.set_clock(t);
        if dma_bus.tick_oam_for_test(bus, false) {
            ran = true;
            apu_clocks += 1;
        } else if ran {
            return Some(apu_clocks);
        }
    }
    None
}

/// Page ramp for the routing tests: every byte distinct and non-zero, so a
/// stuck, skipped or reordered byte is visible in the recorded order.
fn page_ramp() -> Vec<u8> {
    (0..=255u8).map(|i| i | 0x80).collect()
}

/// The shared recording `Mcu` plus the two things an OAM transfer needs
/// beyond `Mcu`: the `$4014` request queue and the OAM write of the put
/// phase. Reads delegate to [`RecordingMcu`], so the recorded calls say which
/// route each of the 512 source reads took — the one thing a real program
/// cannot observe.
struct OamDmaRecordingMcu {
    mcu: RecordingMcu,
    pending: Option<u8>,
    oam_writes: Vec<u8>,
}

impl OamDmaRecordingMcu {
    /// A 64 KiB image whose `page` holds [`page_ramp`]. Seeding writes the
    /// image directly, so the recording starts empty.
    fn with_page(page: u8) -> Self {
        let base = (page as u16) << 8;
        let ramp = page_ramp();
        Self {
            mcu: RecordingMcu::filled(0).with_program(base, &ramp),
            pending: None,
            oam_writes: Vec::new(),
        }
    }
}

impl Mcu for OamDmaRecordingMcu {
    fn read(&mut self, address: u16) -> u8 {
        self.mcu.read(address)
    }

    fn peek(&self, address: u16) -> u8 {
        self.mcu.peek(address)
    }

    fn write(&mut self, address: u16, value: u8, clock: SystemClock) {
        if address == 0x4014 {
            // The trigger a program uses: a store to $4014 queues the DMA.
            self.pending = Some(value);
        } else {
            self.mcu.write(address, value, clock);
        }
    }

    fn read_zero_page(&mut self, address: u8) -> u8 {
        self.mcu.read_zero_page(address)
    }

    fn read_stack_page(&mut self, address: u8) -> u8 {
        self.mcu.read_stack_page(address)
    }

    // The two fast writes are overridden for the same reason as the reads:
    // inheriting the trait's default would forward to `write`, recording the
    // call as generic, so a test could not tell a fast write from a generic
    // one. Forwarding to `RecordingMcu` records the route without
    // synthesising a generic call.
    fn write_zero_page(&mut self, address: u8, value: u8, clock: SystemClock) {
        self.mcu.write_zero_page(address, value, clock)
    }

    fn write_stack_page(&mut self, address: u8, value: u8, clock: SystemClock) {
        self.mcu.write_stack_page(address, value, clock)
    }
}

impl OamDmaMcu for OamDmaRecordingMcu {
    fn take_oam_dma_pending(&mut self) -> Option<u8> {
        self.pending.take()
    }

    fn write_oam_data(&mut self, value: u8) {
        self.oam_writes.push(value);
    }
}

/// An OAM DMA whose source page is `$0000` reads all 512 bytes through the
/// zero-page fast path, in order, and puts exactly those bytes into OAM.
#[test]
fn oam_dma_from_zero_page_reads_through_zero_page_fast_path() {
    let mut bus = Bus::new(OamDmaRecordingMcu::with_page(0x00));
    let mut dma_bus = DmaBus::new();
    bus.mcu_mut().write(0x4014, 0x00, SystemClock::default());

    bus.set_clock(SystemClock(8));
    dma_bus.tick_oam_for_test(&mut bus, false);
    assert!(
        drive_oam_dma(&mut bus, &mut dma_bus).is_some(),
        "OAM DMA never completed"
    );

    let reads = bus.mcu_mut().mcu.calls();
    assert_eq!(reads.len(), 256, "one read per OAM byte: {reads:?}");
    assert!(
        reads.iter().all(McuCall::is_read_zero_page),
        "zero-page source took another route: {reads:?}"
    );
    assert_eq!(
        reads.iter().map(McuCall::address).collect::<Vec<_>>(),
        (0..=255u16).collect::<Vec<_>>()
    );
    assert_eq!(bus.mcu_mut().oam_writes, page_ramp());
}

/// Positive control for the mid-render case below: with rendering disabled
/// the very same $4014 transfer must land in OAM. Without this, "OAM
/// unchanged" could pass simply because the DMA never transferred anything.
/// OAM is seeded with a 0x00 sentinel and the source page sets bit 7, so
/// every transferred byte differs from the sentinel and is detectable.
#[test]
fn oam_dma_with_rendering_disabled_writes_oam() {
    let mut bus = Bus::new(test_mcu());
    let mut dma_bus = DmaBus::new();

    for i in 0..=255u8 {
        bus.mcu_mut().write(0x2003, i, SystemClock::default());
        bus.mcu_mut().write(0x2004, 0x00, SystemClock::default());
        bus.mcu_mut()
            .write(0x0200 + i as u16, i | 0x80, SystemClock::default());
    }

    bus.mcu_mut().write(0x4014, 0x02, SystemClock::default());
    assert!(
        drive_oam_dma(&mut bus, &mut dma_bus).is_some(),
        "OAM DMA never completed"
    );

    for i in 0..=255u8 {
        bus.mcu_mut().write(0x2003, i, SystemClock::default());
        // Rendering disabled keeps the attribute-byte $E3 mask.
        let expected = if i & 0x03 == 0x02 {
            (i | 0x80) & 0xE3
        } else {
            i | 0x80
        };
        assert_eq!(
            bus.mcu_mut().read(0x2004),
            expected,
            "OAM byte {i:#04x} did not receive its DMA byte"
        );
    }
}

/// OAM DMA writes ride the $2004 write path, so a DMA landing mid-render
/// must hit the same hardware ignore+glitch contract as a CPU $2004 write:
/// OAM stays exactly as it was (nesdev PPU registers §OAMDATA: "This
/// extends to DMA transfers via OAMDMA"). Everything is probed through
/// the MMIO registers — nothing is inspected internally.
#[test]
fn oam_dma_during_rendering_does_not_corrupt_oam() {
    let mut bus = Bus::new(test_mcu());
    let mut dma_bus = DmaBus::new();

    // Seed OAM with a 0x00 sentinel through the registers while rendering
    // is off (default mask), then enable background rendering and advance
    // into the middle of a visible line.
    for i in 0..=255u8 {
        bus.mcu_mut().write(0x2003, i, SystemClock::default());
        bus.mcu_mut().write(0x2004, 0x00, SystemClock::default());
    }
    bus.mcu_mut().write(0x2001, 0x08, SystemClock::default()); // background enable
    for _ in 0..(100 * 341 + 100) {
        bus.mcu_mut().tick_ppu();
    }

    // Scratch page $0200 with a ramp whose every byte differs from the
    // sentinel: bit 7 is always set, so no zero can pass unnoticed.
    for i in 0..=255u8 {
        bus.mcu_mut()
            .write(0x0200 + i as u16, i | 0x80, SystemClock::default());
    }

    bus.mcu_mut().write(0x4014, 0x02, SystemClock::default());
    assert!(
        drive_oam_dma(&mut bus, &mut dma_bus).is_some(),
        "OAM DMA never completed"
    );
    // Disable rendering before probing: mid-render $2004 reads expose
    // eval/fetch traffic, not primary OAM, so the sentinel is pinned with
    // rendering off.
    bus.mcu_mut().write(0x2001, 0x00, SystemClock::default());
    // The transfer ran but wrote nothing: every OAM byte is the sentinel.
    for i in 0..=255u8 {
        bus.mcu_mut().write(0x2003, i, SystemClock::default());
        assert_eq!(
            bus.mcu_mut().read(0x2004),
            0x00,
            "OAM byte {i:#04x} was corrupted"
        );
    }
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
    mcu.write(0x4016, 1, SystemClock::default());
    mcu.write(0x4016, 0, SystemClock::default());
    assert_eq!(mcu.read(0x4017), 0x48); // no buttons + light absent
    assert_eq!(mcu.read(0x4017), 0x48);

    // Trigger pull ORs bit 4 on top of the controller bits.
    mcu.trigger_zapper();
    mcu.press_controller_b(Button::A);
    mcu.write(0x4016, 1, SystemClock::default());
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

/// Ticket #64: Zapper light sense is live mid-frame. With a white
/// backdrop and the beam already past the Aim aperture — but long
/// before the VBL drain materializes any framebuffer pixel — $4017
/// must report light detected ( bit 3 clear), not the black-framebuffer
/// "no light" (0x08) the pre-#64 framebuffer path returned.
#[test]
fn zapper_light_detected_mid_frame_before_drain() {
    let mut mcu = test_mcu();
    // Backdrop entry $3F00 = $30 (white per default.pal, luminance 764).
    mcu.write(0x2006, 0x3f, SystemClock::default());
    mcu.write(0x2006, 0x00, SystemClock::default());
    mcu.write(0x2007, 0x30, SystemClock::default());
    mcu.connect_zapper(true);
    mcu.aim_zapper(50, 10);

    // Beam is on the pre-render line, nowhere near the aim row: no light.
    assert_eq!(mcu.read(0x4017) & 0x18, 0x08, "beam not at aim row yet");

    // Tick until the beam has scanned past the aim aperture on the aim
    // row (scanline 10, dot past the aperture's right edge). The PPU
    // starts at scanline 261, so a generous tick budget (one full frame
    // is 262*341 = 89k dots) replaces a scanline bound.
    let mut reached = false;
    for _ in 0..90_000 {
        mcu.tick_ppu();
        let timing = mcu.ppu_timing();
        if timing.scanline() == 10 && timing.dot() > 60 {
            reached = true;
            break;
        }
    }
    assert!(reached, "beam never scanned past the aim row");

    // Still mid-frame: nothing has drained into the framebuffer yet.
    assert_eq!(
        mcu.ppu().renderer().get_pixel(50, 10),
        [0, 0, 0, 0],
        "no drain before vblank: framebuffer still black at the aim"
    );

    // Light detected: bit 3 clear, trigger bit 4 clear (not pulled).
    assert_eq!(
        mcu.read(0x4017) & 0x18,
        0x00,
        "buffered white backdrop senses as light mid-frame"
    );
}

/// Regression: deferred length-counter writes used to be anchored against
/// a `last_apu_tick` cache that `reset` zeroed while the master clock kept
/// running, so they could flush immediately instead of one CPU cycle after
/// their issuing store. They now anchor to the clock carried by their own
/// `write` call, so a write issued at the running clock lands one CPU
/// cycle later however large that clock is.
#[test]
fn deferred_apu_writes_anchor_to_write_clock() {
    let big = 5_000_000_u64;
    let mut mcu = test_mcu();
    mcu.reset();

    // Enable the triangle channel (direct register, not deferred).
    mcu.write(0x4015, 0x04, SystemClock(big));
    // Arm a deferred length-counter reload: it must land ~one CPU cycle
    // after this write cycle relative to the running clock.
    mcu.write(0x400B, 0x08, SystemClock(big));
    // Before the anchored landing point the deferred reload must not have
    // flushed: triangle status bit (bit 2 of $4015) stays clear. `big` is an
    // APU clock (big % 3 == 2), the last one before the landing at big + 3.
    mcu.tick_apu(SystemClock(big));
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

/// An OAM DMA whose source page is `$0100` reads all 512 bytes through the
/// stack-page fast path, in order, and puts exactly those bytes into OAM.
#[test]
fn oam_dma_from_stack_page_reads_through_stack_page_fast_path() {
    let mut bus = Bus::new(OamDmaRecordingMcu::with_page(0x01));
    let mut dma_bus = DmaBus::new();
    bus.mcu_mut().write(0x4014, 0x01, SystemClock::default());

    assert!(
        drive_oam_dma(&mut bus, &mut dma_bus).is_some(),
        "OAM DMA never completed"
    );

    let reads = bus.mcu_mut().mcu.calls();
    assert_eq!(reads.len(), 256, "one read per OAM byte: {reads:?}");
    assert!(
        reads.iter().all(McuCall::is_read_stack_page),
        "stack-page source took another route: {reads:?}"
    );
    assert_eq!(
        reads.iter().map(McuCall::address).collect::<Vec<_>>(),
        (0x100..=0x1ffu16).collect::<Vec<_>>()
    );
    assert_eq!(bus.mcu_mut().oam_writes, page_ramp());
}

/// Any other source page keeps today's routing, today's byte order and the
/// same cycle budget: only the read path of a `$0000`/`$0100` source changed.
#[test]
fn oam_dma_from_other_pages_routes_and_cycles_are_unchanged() {
    fn dma_from(page: u8) -> (Option<usize>, Bus<OamDmaRecordingMcu>) {
        let mut bus = Bus::new(OamDmaRecordingMcu::with_page(page));
        let mut dma_bus = DmaBus::new();
        bus.mcu_mut().write(0x4014, page, SystemClock::default());
        let apu_clocks = drive_oam_dma(&mut bus, &mut dma_bus);
        (apu_clocks, bus)
    }

    let (plain_clocks, mut plain) = dma_from(0x02);
    let reads = plain.mcu_mut().mcu.calls();
    assert_eq!(reads.len(), 256, "one read per OAM byte: {reads:?}");
    assert!(
        reads
            .iter()
            .all(|call| matches!(call, McuCall::Read { .. })),
        "plain source took a fast path: {reads:?}"
    );
    assert_eq!(
        reads.iter().map(McuCall::address).collect::<Vec<_>>(),
        (0x200..=0x2ffu16).collect::<Vec<_>>()
    );
    assert_eq!(plain.mcu_mut().oam_writes, page_ramp());

    let (zero_page_clocks, _) = dma_from(0x00);
    let (stack_page_clocks, _) = dma_from(0x01);
    assert_eq!(zero_page_clocks, plain_clocks);
    assert_eq!(stack_page_clocks, plain_clocks);
}

/// The same 256 bytes DMA'd from zero page, from the stack page and from a
/// plain page leave identical OAM contents, probed through `$2003`/`$2004`.
#[test]
fn oam_dma_from_zero_and_stack_pages_write_the_same_oam_bytes() {
    fn oam_after_dma_from(source_page: u8) -> Vec<u8> {
        let mut bus = Bus::new(test_mcu());
        let mut dma_bus = DmaBus::new();
        // 0x00 sentinel so a byte that was never transferred is visible.
        for i in 0..=255u8 {
            bus.mcu_mut().write(0x2003, i, SystemClock::default());
            bus.mcu_mut().write(0x2004, 0x00, SystemClock::default());
            bus.mcu_mut().write(
                ((source_page as u16) << 8) | i as u16,
                i.wrapping_mul(3).wrapping_add(0x40),
                SystemClock::default(),
            );
        }
        bus.mcu_mut()
            .write(0x4014, source_page, SystemClock::default());
        assert!(
            drive_oam_dma(&mut bus, &mut dma_bus).is_some(),
            "OAM DMA from page {source_page:#04x} never completed"
        );

        let mut oam = Vec::with_capacity(256);
        for i in 0..=255u8 {
            bus.mcu_mut().write(0x2003, i, SystemClock::default());
            oam.push(bus.mcu_mut().read(0x2004));
        }
        oam
    }

    let plain = oam_after_dma_from(0x02);
    // Positive control: the transfer landed and the attribute mask applied.
    assert_eq!(plain[1], 0x40u8.wrapping_add(3));
    assert_eq!(plain.len(), 256);
    assert_eq!(oam_after_dma_from(0x00), plain);
    assert_eq!(oam_after_dma_from(0x01), plain);
}

#[test]
fn expansion_writes_reach_the_mapper_chip() {
    // Mapper-5 ROM, 32 KB PRG with the bank index as the first byte of each
    // 8 KB bank: `$5115` must repoint the `$A000` window through `NesMcu`,
    // proving the `$5000-$5FFF` cartridge-expansion path forwards to the chip.
    let mut rom = vec![
        0x4e, 0x45, 0x53, 0x1a, 2, 1, 0x50, 0x00, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    let mut prg = vec![0u8; 32 * 1024];
    for (i, chunk) in prg.chunks_mut(8 * 1024).enumerate() {
        chunk[0] = i as u8;
    }
    rom.extend_from_slice(&prg);
    rom.extend_from_slice(&[0; 8 * 1024]);
    let file = INesFile::new(rom).unwrap();
    let cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);
    let mut mcu = test_mcu_with(cartridge);

    assert_eq!(mcu.read(0xa000), 0);
    mcu.write(0x5115, 0x80 | 2, SystemClock::default());
    assert_eq!(mcu.read(0xa000), 2);
}

#[test]
fn expansion_reads_return_the_mmc5_multiplier_product() {
    // Same mapper-5 board shape as `expansion_writes_reach_the_mapper_chip`:
    // `$5205`/`$5206` writes multiply instantly, CPU reads get the low/high
    // product bytes through the `$5000-$5FFF` expansion path (ticket #82).
    let mut rom = vec![
        0x4e, 0x45, 0x53, 0x1a, 2, 1, 0x50, 0x00, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    let mut prg = vec![0u8; 32 * 1024];
    for (i, chunk) in prg.chunks_mut(8 * 1024).enumerate() {
        chunk[0] = i as u8;
    }
    rom.extend_from_slice(&prg);
    rom.extend_from_slice(&[0; 8 * 1024]);
    let file = INesFile::new(rom).unwrap();
    let cartridge = create_cartridge_with_mmc3_irq_override(&file, Mmc3IrqOverride::Auto);
    let mut mcu = test_mcu_with(cartridge);

    // Power-on product: `$FF * $FF = $FE01`.
    assert_eq!(mcu.read(0x5205), 0x01);
    assert_eq!(mcu.read(0x5206), 0xfe);

    mcu.write(0x5205, 0x12, SystemClock::default());
    mcu.write(0x5206, 0x34, SystemClock::default());
    assert_eq!(mcu.read(0x5205), 0xa8);
    assert_eq!(mcu.read(0x5206), 0x03);
    assert_eq!(mcu.peek(0x5205), 0xa8);
    assert_eq!(mcu.peek(0x5206), 0x03);

    // Split registers read back last-written through the same path.
    mcu.write(0x5200, 0xc5, SystemClock::default());
    assert_eq!(mcu.read(0x5200), 0xc5);

    // Unclaimed expansion addresses stay open bus (`$5204` belongs to #81).
    mcu.write(0x5201, 0x37, SystemClock::default());
    assert_eq!(mcu.read(0x5204), mcu.open_bus);
}
