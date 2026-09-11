# NES Emulator

## Language — CHR Storage

Core domain for pattern table data access across mapper implementations.

**CHR (Character Data)**:
Tile graphics data stored in pattern tables, accessed by the PPU during rendering.
_Avoid_: Pattern table data, sprite data

**Cartridge**:
Trait defining CPU+PPU operations for mapper-specific implementations. Contains `read_chr` and `write_chr` methods directly on the trait (alongside `read`, `write` for PRG/CartridgeOperation).

**DirectChr**:
Standalone struct for flat 8KB CHR storage (`[u8; 0x2000]`). Read and write map directly by address modulo 0x2000. Used by mappers with flat (non-banked) CHR: NRom, UxRom, AxRom, CnRom.

**BxRom**:
Mapper 34 board variant (BNROM). Stores CHR as `Vec<u8>` but it is flat 8KB (no banking, modulo 0x2000). PRG banking via writes to `$8000-$FFFF`.
_Avoid_: BxROM, BNROM-with-CHR

**Nina001**:
Mapper 34 board variant (NINA-001). CHR data as `Vec<u8>` with 4KB banking via `$7FFE` (bank 0, selectable 0..15) and `$7FFF` (bank 1, selectable 0..15). PRG banking via `$8000-$FFFF` and `$7FFD` (bit 0).
_Avoid_: Nina-001

**Banked CHR (inline)**:
Mappers with bankable CHR (MMC1, MMC3, Vrc24, J87, CnRom, Nina001) manage their own `Vec<u8>` and caching window inline — no shared abstraction. Write gating via `has_chr_ram` per mapper. CHR write-back to underlying `Vec` is direct (no write-through cache).

## Language — PPU Background Rendering

Core domain for how the PPU rasterizes the background layer.

**BackgroundActivation**:
Scroll-state snapshot latched at dot 0 of each visible scanline (and on-demand via `schedule_background_activation_if_visible` when `$2006` is written mid-scanline). Holds `vram_addr`, `fine_x`, `ctrl`, and `screen_x` activation offset. Provides the positional anchor for all background pixel rendering on that scanline. Once latched, ignores further scroll register changes until the next activation.
_Avoid_: background latch, scroll anchor

**TileCache**:
Four-byte cached render state for the current background tile, stored as a field on `Ppu`. Holds the nametable byte (tile index), attribute byte (2-bit palette select), and both bitplane rows (low, high) at the current fine_y. Filled once per tile from mapper `read_chr` and nametable reads — matching the real PPU's tile-fetch cadence — then consumed for up to 8 horizontal pixels before unconditional refill. The first batch is shorter by `fine_x` to handle sub-tile scroll offsets. Invalidated at dot 0 and on `$2000`/`$2006` register writes.
_Avoid_: tile cache (lowercase c), pattern cache

## Language — System Timing

**Cold-start**:
A construction-only phase from `Ppu::new()` until the first 261→0 scanline wrap. The PPU starts at scanline 261 (pre-render) so that vertical scroll reload, background activation, and MMC3 A12 toggles all run before the first visible scanline 0; sprite evaluation is not part of this preparation — it does not happen on the pre-render line, so scanline 0 starts with no evaluated sprites. After the first wrap, the PPU enters normal frame operation. `Ppu::reset()` does _not_ re-enter Cold-start. Cold-start is a domain concept — no code field represents it; setting initial scanline to 261 is sufficient.
_Avoid_: warm-up, boot-phase, pre-frame, initialization phase

**SystemClock**:
A `Copy` newtype over `u64`. Represents the current master clock cycle. One system cycle equals one PPU dot. Every tick advances the clock by one. Owned by `NesMachine` and passed down to CPU/APU/DMA modules as an immutable parameter — no global state.
_Avoid_: system cycles (use for the raw u64 count only), global clock, static clock

**SystemClock phase**:
`SystemClock` phase helpers `is_cpu_clock()` (`cycles % 3 == 2`, the Last phase), `is_apu_clock()` (same), and `is_even_cpu_cycle()` derive CPU/APU cadence from the master dot. One CPU cycle spans 3 dots; the former `CpuClockPhase {First,Middle,Last}` enum was removed — callers use `clock.is_cpu_clock()` etc, not `clock.phase()`.

**Tick**:
A single system-cycle step. `NesMachine::tick()` advances the clock by one, then calls each device's tick method in order: PPU → cartridge IRQ latch capture → APU (sample IRQ, tick) → Bus (DMC + OAM DMA arbitration) → interrupt lines → CPU (if the bus is `Idle`). Each device uses the clock to decide whether to advance its internal state. The bus is the only DMA owner; DMC and OAM are not ticked separately outside it.
**Microcode**:
A single step of the CPU's internal microcode machine. Multiple microcodes may execute across several ticks to complete one 6502 instruction.

**Run to instruction boundary**:
Draining the CPU's microcode queue until it is empty — the edge between two 6502 instructions (including the interrupt/BRK/RESET tails). Two seams: `Cpu::run_to_instruction_boundary(&mut plugin, &mut clock)` is the CPU-only drain (advances `clock` by one dot per microcode, no device ticks — the setup-time seam used by `NesMachine::set_pc` and tests); `NesMachine::run_to_instruction_boundary()` is the full-interleaving drain (advances `self.clock` via `NesMachine::tick()` with PPU/APU/DMA/NMI interleaving — the faithful seam). Microcodes must be drained via one of these named primitives, never in an ad-hoc tight loop.
_Avoid_: drain, flush queue, run until empty (use the canonical name)

**Frame**:

**Reset quiescence**:
The bus-drain contract of `NesMachine::reset()`. Once the reset line is asserted the CPU stops being fed; PPU/APU keep interleaving and any DMA work already accepted by the bus completes before the device resets apply. Fresh DMC fetch requests are suppressed during the drain so a playing sample channel cannot extend it. The bus owns DMA quiescence (`Bus::is_busy` / `Bus::suppress_new_dmc_requests` / `Bus::reset`); `NesMcu::reset(clock)` re-anchors only time-relative PPU/APU state and `Cpu::reset` clears CPU state — time keeps running on the master `SystemClock`.
_Avoid_: hard abort, mid-DMA teardown, stale DMA

**Per-dot contract**:
The minimal pub surface of `Cpu<M>` (ADR-0010): `tick` (→ `TickOutcome`), `update_interrupt_lines`, `reset`, `is_halted`, `microcodes_empty`, `run_to_instruction_boundary`, plus read-only `a() / x() / y() / sp() / status() / pc() / flag()` and `snapshot()`/`view()`. No `mcu()` walk, no `pub` register fields, no `pub(crate)` DMA helpers — those live behind `Bus`. The one seam for instruction-boundary work is `run_to_instruction_boundary`, not a hand-rolled `while`.
_Avoid_: pub register fields, `cpu.mcu().ppu()`, `(ExecuteResult, bool)` tuple

**CpuSnapshot**:
Owned copy `CpuSnapshot {a, x, y, sp, status, pc, halt}` returned by `Cpu::snapshot()` and held as `MachineView::cpu`. The read-model for register inspection; plugins/tools read `view.cpu.a` not `cpu.a`.
_Avoid_: direct `cpu.a` field access

**MachineView**:
Read-model snapshot `MachineView<'a, M: Mcu> {cpu: CpuSnapshot, mcu: &M, clock}` exposed to `Plugin::start(&MachineView, clock)` / `end`. For `NesMcu` it exposes `ppu_in_vblank()`, `ppu_rendering_enabled()`, `ppu_frame_no()`, `read_vram()`, `borrow_image()` without exposing `&Cpu` or `&Mcu` walks. Built in `Cpu::tick` as `MachineView::new(snapshot, &mcu, clock)` and via `Cpu::view(clock)`.
_Avoid_: `&Cpu<M>` in Plugin, `cpu.mcu().ppu()`, `mcu().peek` outside view

**TickOutcome**:
Named result `TickOutcome {control: ExecuteResult, instruction_complete: bool}` returned by `Cpu::tick`. `instruction_complete` is true when the microcode queue drained (instruction boundary). Replaces the unnamed `(ExecuteResult, bool)` tuple.
_Avoid_: `(ExecuteResult, bool)`, `tick(...).0/.1`, `instruction_done` unnamed bool


## Language — Mapper IRQ

Core domain for cartridge-generated interrupt requests on bank-switched mappers.

**IRQ Enable ('E')**:
Latched mapper control bit gating all IRQ clocking. When clear, neither the prescaler nor the counter advances. Set by IRQ Control writes; overwritten by the acknowledge transfer.
_Avoid_: enable-less counter, always-on IRQ

**Enable-after-acknowledge ('A')**:
Latched mapper control bit copied into IRQ Enable by an acknowledge write. Distinguishes one-shot from repeated IRQs.
_Avoid_: auto-enable, re-arm bit

**IRQ latch**:
Reload value for the IRQ counter, written as two nibbles on VRC4. Rewriting it alone never touches the counter.
_Avoid_: reload register

**IRQ prescaler**:
CPU-cycle divider feeding one counter clock; scanline mode approximates one scanline per count, cycle mode bypasses it. Reset by every IRQ Control write; frozen while disabled.
_Avoid_: dot counter, phase accumulator

**Inverted IRQ counter**:
Down-counting representation of the hardware's up-counter, seeded from the negated latch; reaching zero is the trip point and reseeds from the latch.
_Avoid_: countdown timer, wrapping trick

## Language — CPU Interrupt Recognition

**Interrupt recognition window**:
The span of dots during which an /NMI assertion's newest unconsumed rise is eligible at a hijack site, ending at that site's Hijack deadline. Declared per interrupt sequence as table data next to its micro-ops.
_Avoid_: poll window, recognition period, decision window

**Poll point**:
The CPU cycle within an instruction's microcode sequence where the interrupt lines are sampled for dispatch. Final micro-op by default; taken branches shift it to their last internal cycle; interrupt sequences suppress it on their final cycle.
_Avoid_: detect point, sample point, poll cycle

**Hijack deadline**:
Table-declared cutoff, in dots from sequence start, bounding which NMI rise can switch an in-flight BRK/IRQ vector fetch to the NMI vector.
_Avoid_: decision lag, decision_lag, lag constant

**Edge consumption**:
The single protocol marking an NMI rise consumed — recording the consumed assertion's rise time in `consumed_through` — so the sampler and later hijack checks cannot re-fire it while the line stays high.
_Avoid_: edge take, pending clear, hijack take

## Language — CPU Interrupt Handshake

Core domain for the per-dot PPU↔CPU and IRQ-producer↔CPU line sampling that feeds interrupt recognition.

**Interrupt lines**:
The per-dot bundle `{nmi, irq_level}` driven from devices into the CPU detectors. `nmi` is the `NmiLines` bundle; `irq_level` is the already time-corrected (APU +1, cartridge-quantized) level for this dot.
_Avoid_: interrupt bundle, irq_lines, line bundle

**NMI line bundle (NmiLines)**:
Atomically produced `{level, race_cancel}` from the PPU for one dot. `level = v_blank && nmi_enable`; `race_cancel = true` when a `$2000`/`$2002` access on the `vbl_set_cycle` dot suppressed the assertion — the level's same-dot rise never asserted and the edge must be retracted via `consumed_through`.
_Avoid_: nmi flag, race flag, nmi_race_cancel (field name only)

**APU IRQ skew (sampled IRQ)**:
The one-dot `pre-tick → visible next dot` mapping of the APU IRQ level. The level sampled BEFORE `tick_apu(clock)` becomes CPU-visible on `clock+1`; a transition `tick_apu` raises or clears reaches the detector on the next dot.
_Avoid_: delayed irq, apu lag, one-cycle delay

**Cartridge IRQ latch**:
The two-stage dot-captured / CPU-cycle-latched quantization of mapper IRQ. `next` captures `cartridge_irq_pending()` every dot (post-PPU); `latched` copies `next` on CPU dots only; the CPU sees `latched`. MMC3/VRC counters are PPU-dot-clocked; tests pin the CPU-cycle quantization.
_Avoid_: cartridge bool, irq buffer, latched irq (ambiguous)

**Same-tick retract (race-retract)**:
The same-dot suppression of an NMI edge that `NmiLines.race_cancel` flags, consumed via the unified `consumed_through` edge protocol (`cancel_rising_edge_at(clock)` → `mark_consumed()`), clearing `nmi_input`/`last_sampled_level` so neither sampler nor hijack can re-fire the same assertion while the line stays high.
_Avoid_: nmi cancel, vblank suppression (overloaded), race kill

## Language — CPU Bus Cycles
**Bus cycle classification**:
The single source of truth for what a pending Microcode cycle drives on the bus — driven by `Microcode::exec` itself (ADR-0013, revises ADR-0007). Each `exec` drives its access (PC dummy fetch for `Nop`, `read_zero_page` for `ZeroPage` dummy, `DummyReadAt` for branch fixup, `DummyStackRead` for RTS/RTI/PLP, etc.) and `bus_cycle` is derived from that execution, not a parallel classifier. The exhaustive match over `Microcode` (including `DummyReadAt(u16)`, `DummyStackRead`) classifies each driven access as `Read(ReadAddress::Latch(addr))`, `Read(Stack(...))`, `Write`, or `Internal`; DMC DMA halt decisions consume it and nothing else.
_Avoid_: cycle kind, write-operation check, bus op
**Halt-repeat read**:
The bus behavior while the CPU is RDY-halted by DMA: the bus re-drives the address the pending read cycle would drive. Internal cycles have no address of their own and repeat the last completed read. Write cycles never sit under a halt — the CPU is only halted between them.
_Avoid_: RDY repeat, dummy read (the DMA's own alignment cycles), repeated fetch

## Language — DMA Bus

Core domain for the single bus that the CPU, OAM DMA, and DMC DMA time-share. The bus is the deep module that localizes DMA arbitration; the PPU/APU/MCU are not bus owners.

**Bus (DMA Bus)**:
The module that owns the active DMA transfer for both channels and the arbitration policy between them. Lives in `nes_core::bus` (or `nes::bus`) as `struct Bus { dmc: DmcDma, oam: Option<OamActive> }`. Its interface is `tick(cpu, clock) -> BusOwner`, `is_busy(&mcu) -> bool`, `suppress_new_dmc_requests(&mut mcu)`, `reset()`. `DmcDma`'s 7-state machine stays a private implementation detail behind the bus.
_Avoid_: DMA coordinator (acceptable alias in prose, not in code), bus controller, DMA engine

**BusOwner**:
Per-dot holder of the bus returned by `Bus::tick`: `Idle` (CPU owns the bus), `Oam` (OAM DMA held the bus this dot — includes halt, alignment, and transfer cycles), or `Dmc` (DMC DMA held/drove the bus this dot). The caller (`NesMachine::tick`) skips `Cpu::tick` / `update_interrupt_lines` when the owner is not `Idle`; DMC's `cpu.frozen` stall remains the same mechanism but is driven from inside `Bus::tick`.
_Avoid_: bus state, dma owner, active dma

**DMA arbitration (DMC-wins)**:
The single arbitration policy the bus implements: a DMC DMA read (`dmc_drove_bus` true) that lands on an OAM DMA read phase (`startup_cycles == 0 && transfer_cycle.is_multiple_of(2)`) aborts the OAM read, which is redone after one alignment cycle (`pause_cycles = 1`). The `dmc_drove_bus` bool and the `NesMcu` collision predicate become a private branch inside `Bus::tick`; no external bool is threaded.
_Avoid_: collision handling (ok in commentary, not the canonical term), dma priority

**Bus quiescence**:
The condition `Bus::is_busy(&mcu)` used by `NesMachine::reset` to drain the bus. True while any DMA is active (`dmc.is_busy()` or OAM active) or a `$4014` OAM request is still queued in the producer. Drain loops `while bus.is_busy()` with `bus.suppress_new_dmc_requests()` each dot until quiet, then `bus.reset()` clears both channels at once.
_Avoid_: dma busy, oam active, pending dma

**Raw timer period**:
A channel's timer-register countdown value, loaded directly into a timer. The pulse timer counts once per APU cycle, so a sequencer step lasts 2·(t+1) CPU cycles; the triangle timer counts once per CPU cycle, so a step lasts t+1 CPU cycles.
_Avoid_: period-table value (reserved for precomputed tables)

**Period table**:
Precomputed NTSC CPU-cycle intervals between output clocks for the noise (`$400E` shift-register clocks) and DMC (`$4010` output-level changes). Entries are even because hardware counts these channels internally in APU cycles (2 CPU cycles each); the stored value is already the observable CPU-cycle interval.
_Avoid_: octave framing of noise timing, half-rate correction

## Language -- PPU Sprites

Core domain for sprite (OAM) data representation and tile addressing.

**OAM (Object Attribute Memory)**:
256-byte memory holding 64 Sprites, accessed byte-wise via PPU registers $2003/$2004 and DMA. Typed access through the `Sprite` struct.
_Avoid_: sprite RAM, sprite buffer, OAM buffer

**Sprite**:
4-byte OAM entry containing Y position (top screen coordinate + 1), tile index, Attribute (palette/flip/priority flags), and X position.
_Avoid_: OAM entry, sprite record

**Attribute**:
Bitfield within each Sprite encoding: palette index (bits 0-1), behind-background priority (bit 5), horizontal flip (bit 6), vertical flip (bit 7). Bits 2-4 are unused.
_Avoid_: sprite flags, attribute byte

**TilePosition**:
Decoded tile location combining size (8x8 or 8x16), PatternBank, and tile index within that bank. Used for both background and sprite tile addressing. `resolve_pixel_addr(tile_y)` returns a `(plane0_addr, plane1_addr)` pair of CHR addresses for a given vertical pixel offset. Produced by `Sprite::tile_position()` for sprites and `PpuCtrl::background_tile_position(tile_idx)` for background tiles.
_Avoid_: tile address, sprite tile addr

**PatternBank**:
Either First ($0000) or Second ($1000) pattern table in VRAM. Selected per-sprite based on sprite size and PPU control register, or per-background-tile via `PpuCtrl::background_pattern_table()`.
_Avoid_: pattern table, CHR bank

**Secondary OAM**:
A double-buffered 8-entry sprite buffer inside the PPU. `SpriteManager` evaluates primary OAM during dots 65–256 of visible scanlines (0–239), starting at `OAMADDR` (`effective_index = (start_index + oam_index) & 0x3F` wrapping at 64, termination at 64 scanned) and copies up to 8 in-range sprites into the *next* buffer; the pre-render line (261) evaluates nothing, so scanline 0 starts with an empty buffer. `next_zero_sprite`/`current_zero_sprite` (`Option<Sprite>`) tracks whether OAM entry 0 survived evaluation so `sprite_zero_pixel_opaque` only fires for evaluated zero. At dot 0 each scanline, the buffers swap: the freshly populated buffer becomes *current* and feeds `find_sprite_pixel` for the whole scanline. During sprite tile fetches (dots 257–320 of visible and pre-render scanlines while rendering is enabled) hardware drives `OAMADDR` to 0; software polling `$2003` mid-frame observes that.
_Avoid_: sec OAM, sprite cache

## Language — PPU VBL/NMI Race

**VBL set dot (enter_vblank)**:
Scanline 241 dot 1 — the single PPU tick where the VBL flag would set and the NMI edge would assert. `Ppu::tick` processes the current `timing.dot` then `advance`s; `enter_vblank()` is true only at `(241,1)`.
_Avoid_: vblank flag set, nmi set

**$2002 race window**:
Hardware PPU_frame_timing: a `$2002` read one PPU clock before the set dot (hardware dot 0 of scanline 241, observed as `timing.dot==1` at read time because `NesMachine::tick` advances PPU before the CPU read) suppresses the upcoming set entirely — flag never rises, NMI never asserts, no `race_cancel` needed. A read on the set dot itself (same `vbl_set_cycle`) races the set and wins: flag is set then immediately cleared, `nmi_race_cancel` retracts the edge latched earlier in the same tick. `vbl_set_cycle` stamps the tick that processed `(241,1)` whether or not it was suppressed; `suppress_vblank_pending` is the one-dot pending flag set at `dot==1` read time and consumed at the next `enter_vblank`.
_Avoid_: vblank suppression (overloaded), nmi suppression

## Language — Mapper MMC1

**MMC1 consecutive-cycle write filter**:
MMC1's serial port ignores data-bit writes (`$8000-$FFFF` with bit7 clear) that occur within 1 CPU cycle (3 PPU/system cycles) of the previous mapper write. Reset writes (`bit7 set`) are never ignored but do update the timestamp (`last_write_cycle: Option<SystemClock>`). The filter is checked in `Cartridge::write(address, value, cycle: SystemClock)` — only `Cartridge::write` takes `SystemClock`, not `Mcu::write`; `NesMcu` stamps `current_clock` from `NesMachine::tick` via `set_clock`. Bill & Ted's `INC $FF` (reset $FF then data $00 on next cycle) and Shinsenden's `RRA` (data then reset) pin the two sides.
_Avoid_: write filter, consecutive writes (without qualifier)

## Language — Bus Decoding

**Decode into NesMcu (not Ppu)**:
`Ppu` no longer owns `Box<dyn Cartridge>` nor implements `Mcu`. `NesMcu` owns `cartridge: Box<dyn Cartridge> + cartridge_caps: CartridgeCaps + current_clock: SystemClock` and performs all CPU address decode: `0x0000-0x1FFF` LowerRam, `0x2000-0x3FFF` PPU regs via `Ppu::read_ppureg`/`write_ppureg` (taking `&mut dyn Cartridge, CartridgeCaps`), `0x4000-0x401F` APU/IO, `0x4020-0x5FFF` open bus, `0x6000-0x7FFF` PRG-RAM (open bus if `!prg_ram_enabled()`), `0x8000-0xFFFF` PRG-ROM via `cartridge.read`/`write(cycle)`. Undriven reads (`0x4020-0x5FFF`, disabled PRG-RAM) refresh the CPU open bus (`open_bus` latch) and the PPU latch decays via `PPU_OPEN_BUS_DECAY_TICKS`.
_Avoid_: Ppu as Mcu, cartridge in Ppu, open bus (without qualifier)

## Language — Controller Reading

**Strobe**:
Bit 0 of the value written to `$4016`; drives the 4021 Parallel/Serial control of both controllers. High: the shift register continuously reloads from live button state. Low: reads clock the register one bit each.
_Avoid_: stroke

**Frozen register**:
The button-state snapshot taken when strobe falls; all subsequent polled reads come from it until the next falling edge. Mid-poll input changes never leak into it.
_Avoid_: latch (overloaded), locked copy

**Bit position**:
Index of the next serial stage the poll reads from the frozen register. Advanced once per `$4016`/`$4017` read; reset only by a strobe falling edge.
_Avoid_: read offset

## Language — Zapper

**Zapper**:
Light gun on controller port 2, read through `$4017`: trigger on bit 4, light sense on bit 3 (0 = light detected, 1 = none). Disconnected reads return 0 and leave plain controller-B bits.
_Avoid_: light gun (in identifiers), zapper gun

**Light sense**:
Aperture sampling of the rendered framebuffer around the aim point (7×7 pixels, radius 3): a pixel counts as light when brightness (R+G+B) ≥ 85. Gated by beam position — a row senses only once the beam has scanned it, and its phosphor persists ~20 scanlines.
_Avoid_: pixel detection, photodetection, light detect flag

**Trigger release delay**:
A pulled trigger reads held for ~100 ms (178,977 CPU cycles) from the pull, regardless of how long the button stays down; re-pulling mid-hold does not extend it.
_Avoid_: trigger timeout, debounce

## Language — Framebuffer Rendering

Core domain for PPU pixel output and host-side presentation. Replaces the former `image::RgbaImage` backing.

**Framebuffer**:
Heap-allocated RGBA pixel store `Box<[[u8; 4]; 256 * 240 * N * N]>` owned by `ImageRender<N>`, row-major at `256*N` by `240*N` output pixels, one `[u8; 4]` per pixel.
_Avoid_: RgbaImage, image buffer (ambiguous), raw bytes (when meaning typed pixels)

**Zoom Factor (N)**:
Const generic `N` on `ImageRender<N>`, default `1`. Logical scale where one NES logical pixel (256×240) replicates to an `N×N` block of output pixels. Backing size is `256*240*N*N` entries; `width() = 256*N`, `height() = 240*N`.
_Avoid_: zoom ratio (use Zoom Factor), scale factor (overloaded), N (bare)
**ImageRender<N>**:
`Render` implementation owning the Framebuffer. `set_pixel(x, y, color)` is called at NES logical coordinates `x < 256, y < 240` and writes an `N×N` block at `(x*N .. x*N+N, y*N .. y*N+N)`. Out-of-bounds logical coordinates are silently ignored; callers guarantee in-range. Exposes `as_bytes()`, `width()/height()`, and `pixel_brightness(x,y)` without depending on the `image` crate. Does not implement `Clone` — framebuffer copies are multi-MiB.
_Avoid_: ImageRender (without `N`), RgbaImage wrapper, cloning the framebuffer

## Language — Profiling & Performance

Core domain for measuring and optimizing emulation throughput. Headless != no-render; Self >3% defines a Hot Path.

**Profiling Profile**:
Cargo build profile `[profile.profiling]` inheriting `release` with full debug symbols (`debug=2`, `strip=false`, `lto=false`). Frame pointers forced via `RUSTFLAGS="-C force-frame-pointers=yes"` so `perf record -g` yields complete stacks. Never shipped; only for `perf.data`/`flamegraph`.
_Avoid_: release with debug, bench profile

**Headless Execution**:
Movie playback with no host window (`--headless`), still rendering to `RecordRender` on a DummyWindow. Incurs `set_pixel` cost (~2.3% Self). Distinct from a future `NullRender` zero-render mode.
_Avoid_: no-render, offscreen

**No-throttle**:
Disabling the 60Hz sleep in `PlayMovieAction` (`--no-throttle` skips `target_frame_duration` sleep). Required for throughput measurement; with it, wall time is sleep-bound, not emulation-bound.
_Avoid_: unlimited fps, uncapped

**Frame Budget**:
Wall-clock budget per emulated frame at 60fps: `16.6ms` (`target_frame_duration = 1_000_000_000/60` ns). Headless+no-throttle measures headroom against this budget.
_Avoid_: frame limit, fps target

**Hot Path**:
Instruction sampling Self overhead >3% in `perf report --no-children`. Leaf cost after inlining; Children is call-graph roll-up. Baseline 500-frame `warped.fm2` sample: `Ppu::tick` 42%, `NesMachine::tick` 11%, `tick_apu` 6.7%, `Cpu::tick` 5%, `Pulse::output` 3.6%.
_Avoid_: hot spot (ambiguous), bottleneck (use only after budget miss)

**Sampling (perf)**:
`perf record -F 997 -g` sampling `cpu/cycles:Pu` at ~1kHz with call-graph into `/tmp/perf.data`. Text report via `perf report --stdio`. Overhead 1–2%; trade-off vs Cachegrind's cache-miss model (20× slower, not installed).
_Avoid_: tracing, instrumentation

**Flamegraph**:
Folded-stack visualization via `cargo flamegraph --profile profiling` collapsing `perf` samples into width proportional to Self overhead. Output `/tmp/flamegraph.svg`; complements `perf report` for call-graph navigation.
_Avoid_: flame chart (different), perf report (text)
