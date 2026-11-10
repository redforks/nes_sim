# NES Emulator

## Language — Cartridge and Mapper

Core domain for the cartridge board: the memory it carries, and the chip that decides which bank each address slot points at.

**CHR (Character Data)**:
Tile graphics data stored in pattern tables, accessed by the PPU during rendering.
_Avoid_: Pattern table data, sprite data

**Cartridge**:
The board model: the memory the cartridge carries (PRG ROM, PRG RAM, CHR, nametables) together with the mapper chip that decides which bank every slot of the CPU and PPU address windows points at. Owns all cartridge memory and exposes CPU reads/writes plus PPU-side VRAM access.
_Avoid_: Cartridge trait, mapper-as-cartridge

**Mapper**:
The bank-switching chip on a cartridge board (NROM, UxROM, AxROM, CNROM, MMC1, MMC3, MMC5, VRC4, Nina-001, BxROM, J87). Reacts to CPU register writes and PPU clock events by pointing slots at banks; it carries no cartridge memory and performs no data read of its own.
_Avoid_: Board, cartridge, CHR ROM

**Bank**:
The unit of switchable cartridge memory: 1 KB of bytes. Every switchable granularity (1 KB, 4 KB, 8 KB, 32 KB) is one bank or a run of consecutive banks.
_Avoid_: Page, block, chunk

**Bank slot**:
One position in a bank window. A slot holds a bank number.
_Avoid_: Entry, offset

**Bank window**:
The fixed set of slots the CPU or PPU can address. The PPU window covers `$0000-$3FFF` (8 pattern-table slots, 4 nametable slots, and the 4 `$3000-$3EFF` alias slots that repeat them); the PRG window covers `$8000-$FFFF` (32 PRG-ROM slots). PRG RAM (`$6000-$7FFF`) is a separate bank array outside the window with its own read/write pair. The mapper rewrites it; every access reads it.
_Avoid_: Page table, mapping array

**VRAM (cartridge)**:
The cartridge's PPU-addressable memory as a single address space: pattern tables (`$0000-$1FFF`, CHR ROM or CHR RAM) followed by the four nametable banks (`$2000-$3EFF`). Reached through one read/write pair, so no caller distinguishes CHR from nametable.
_Avoid_: CIRAM (nametable RAM only), CHR (pattern tables only)

**Mirroring**:
The board wiring that decides which nametable bank each of the four nametable slots shows: vertical repeats horizontally, horizontal repeats vertically, single-screen shows one bank in all four slots, four-screen wires all four banks. Expressed as slot entries, never as an address transform.
_Avoid_: address folding, NT remapping

**BxRom**:
Mapper 34 board variant (BNROM): flat 8KB CHR (no banking) and PRG banking via writes to `$8000-$FFFF`.
_Avoid_: BxROM, BNROM-with-CHR

**Nina001**:
Mapper 34 board variant (NINA-001): CHR in two 4KB banks via `$7FFE` (slot 0, selectable 0..15) and `$7FFF` (slot 1, selectable 0..15); PRG banking via `$8000-$FFFF` and `$7FFD` (bit 0).
_Avoid_: Nina-001

## Language — PPU Background Rendering

Core domain for how the PPU rasterizes the background layer.

**BackgroundActivation**:
Scroll-state snapshot latched at dot 0 of each visible scanline (and on-demand via `schedule_background_activation_if_visible` when `$2006` is written mid-scanline). Holds `vram_addr`, `fine_x`, `ctrl`, and `screen_x` activation offset. Provides the positional anchor for all background pixel rendering on that scanline. Once latched, ignores further scroll register changes until the next activation.
_Avoid_: background latch, scroll anchor

**TileCache**:
Four-byte cached render state for the current background tile, stored as a field on `Ppu`. Holds the nametable byte (tile index), attribute byte (2-bit palette select), and both bitplane rows (low, high) at the current fine_y. Filled once per tile from cartridge VRAM reads — matching the real PPU's tile-fetch cadence — then consumed for up to 8 horizontal pixels before unconditional refill. The first batch is shorter by `fine_x` to handle sub-tile scroll offsets. Invalidated at dot 0 and on `$2000`/`$2006` register writes.
_Avoid_: tile cache (lowercase c), pattern cache

## Language — System Timing

**Cold-start**:
A construction-only phase from `Ppu::new()` until the first 261→0 scanline wrap. The PPU starts at scanline 261 (pre-render) so that vertical scroll reload, background activation, and MMC3 A12 toggles all run before the first visible scanline 0; sprite evaluation is not part of this preparation — it does not happen on the pre-render line, so scanline 0 starts with no evaluated sprites. After the first wrap, the PPU enters normal frame operation. `Ppu::reset()` does _not_ re-enter Cold-start. Cold-start is a domain concept — no code field represents it; setting initial scanline to 261 is sufficient.
_Avoid_: warm-up, boot-phase, pre-frame, initialization phase

**SystemClock**:
A `Copy` newtype over `u64`. Represents the current master clock cycle. One system cycle equals one PPU dot. Every tick advances the clock by one. Owned by `Bus` (`Bus<M> {mcu, clock}`, ADR-0018); the machine loop advances it via `Bus::advance()` and CPU/DMA seams read the current dot through `bus.clock()` — no `(bus, clock)` parameter pairs and no hidden stamp copies inside `Cpu` or `NesMcu` (ADR-0018 field trims). Device seams that have no bus in their call chain (`Apu::write`, `Mapper::cpu_write`, `NesMcu::reset/tick_apu`, `Cpu::update_interrupt_lines`) keep a plain `SystemClock` value; `Mcu::write` carries the write's dot. Tests stage dots with `bus.set_clock()`.
_Avoid_: system cycles (use for the raw u64 count only), global clock, static clock, `(bus, clock)` pairs

**SystemClock phase**:
`SystemClock` phase helpers `is_cpu_clock()` (`cycles % 3 == 2`, the Last phase), `is_apu_clock()` (same), and `is_even_cpu_cycle()` derive CPU/APU cadence from the master dot. One CPU cycle spans 3 dots; the former `CpuClockPhase {First,Middle,Last}` enum was removed — callers use `clock.is_cpu_clock()` etc, not `clock.phase()`.

**Tick**:
A single system-cycle step at the bus's current dot (ADR-0018). `NesMachine::tick()` captures `bus.clock()` and calls each device's tick method in order: PPU → cartridge IRQ sample (CPU dot only) → APU (sample IRQ, tick) → `DmaBus` (DMC + OAM DMA arbitration) → interrupt lines → CPU (unless the `DmaBus` returned a non-`Idle` owner), then advances the bus's master clock once after the dot completes. Each device decides whether to advance its internal state from that dot. The `DmaBus` is the only DMA owner; DMC and OAM are not ticked separately outside it.
**Microcode**:
A single step of the CPU's internal microcode machine. Multiple microcodes may execute across several ticks to complete one 6502 instruction.

**Run to instruction boundary**:
Draining the CPU's microcode queue until it is empty — the edge between two 6502 instructions (including the interrupt/BRK/RESET tails). Three siblings: `Cpu::run_to_instruction_boundary(&mut bus, plugin)` is the CPU-only drain (advances the bus's master clock by one dot per microcode, no device ticks — the setup-time seam wrapped by `Machine::run_to_instruction_boundary` / `Machine::drain_to_instruction_boundary(plugin)`); `NesMachine::run_to_instruction_boundary()` is the full-interleaving drain (advances the bus clock via `NesMachine::tick()` with PPU/APU/DMA/NMI interleaving — the faithful seam). Microcodes must be drained via one of these named primitives, never in an ad-hoc tight loop.
_Avoid_: drain, flush queue, run until empty (use the canonical name)

**Frame**:

**Reset quiescence**:
The bus-drain contract of `NesMachine::reset()`. While draining, in-flight OAM DMA holds the bus and DMC DMA freezes the CPU, so the CPU gets no cycles; PPU/APU keep interleaving and any DMA work already accepted by the bus completes before the device resets apply, and any pre-freeze CPU microcodes are discarded by those resets. Fresh DMC fetch requests are suppressed during the drain so a playing sample channel cannot extend it. The `DmaBus` owns DMA quiescence (`DmaBus::is_busy(&mcu)` / `DmaBus::suppress_new_dmc_requests(&mut mcu)` / `DmaBus::reset()`); `NesMcu::reset(clock)` re-anchors only time-relative PPU/APU state and `Cpu::reset` clears CPU state — time keeps running on the master `SystemClock`.
_Avoid_: hard abort, mid-DMA teardown, stale DMA

**Per-dot contract**:
The minimal pub surface of `Cpu` (ADR-0010, ADR-0017, ADR-0018): `tick(&mut self, bus: &mut Bus, plugin)` (→ `ExecuteResult`; the dot is `bus.clock()`), `update_interrupt_lines`, `reset`, `is_halted`, `microcodes_empty`, `run_to_instruction_boundary(&mut bus, plugin)`, plus read-only `a() / x() / y() / sp() / status() / pc() / flag()` and `snapshot()`. No `mcu()` walk, no `pub` register fields, no `pub(crate)` DMA helpers, no standalone `clock` parameter — memory and time both go through the `&mut Bus` parameter and DMA through the `DmaBus`. The one seam for instruction-boundary work is `run_to_instruction_boundary`, not a hand-rolled `while`.
_Avoid_: pub register fields, `cpu.mcu().ppu()`, `(ExecuteResult, bool)` tuple, `cpu.mcu()`

**CpuSnapshot**:
Owned copy `CpuSnapshot {a, x, y, sp, status, pc, halt}` returned by `Cpu::snapshot()` and held as `MachineView::cpu`. The read-model for register inspection; plugins/tools read `view.cpu.a` not `cpu.a`.
_Avoid_: direct `cpu.a` field access

**MachineView**:
Read-model snapshot `MachineView<'a, M: Mcu> {cpu: CpuSnapshot, mcu: &M, clock}` exposed to `Plugin::start(&MachineView)` / `end` (ADR-0018: the plugin seam reads the dot via `view.clock()`, no trailing `SystemClock`). For `NesMcu` it exposes `ppu_in_vblank()`, `ppu_rendering_enabled()`, `ppu_frame_no()`, `read_vram()`, `borrow_image()` without exposing `&Cpu` or `&Mcu` walks. Built as `MachineView::new(cpu.snapshot(), bus.mcu(), clock)` — inside `Cpu::tick` for the per-dot view; external tools (e.g. plugin unit tests) build it over a `(Cpu, Bus)` pair the same way.
_Avoid_: `&Cpu` in Plugin, `cpu.mcu().ppu()`, `mcu().peek` outside view

**Instruction boundary**:
The microcode queue drained, observed via `Cpu::microcodes_empty()`. `Cpu::tick` returns only `ExecuteResult` (`plugin.should_stop()` at a boundary, `Continue` mid-instruction, `Halt` when halted); callers that need the boundary read `microcodes_empty()` after the tick.
_Avoid_: `TickOutcome`, `(ExecuteResult, bool)` tuple, `instruction_done` unnamed bool


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
The per-dot bundle `{nmi, irq_level}` driven from devices into the CPU detectors. `nmi` is the PPU NMI bool (`v_blank && nmi_enable`, the `/NMI` line level); `irq_level` is the already time-corrected (APU +1, cartridge-quantized) level for this dot.
_Avoid_: interrupt bundle, irq_lines, line bundle

**NMI line**:
The PPU NMI bool for one dot: `v_blank && nmi_enable` (the `/NMI` line level), sampled atomically after the PPU tick. Same-dot suppression is not a separate flag: a `$2000`/`$2002` access on the set dot drops the level on the next dot, and the 1-dot assertion falls between the CPU sampler's dots, so no edge latches.
_Avoid_: nmi flag, nmi bundle, race flag

**APU IRQ skew (sampled IRQ)**:
The one-dot `pre-tick → visible next dot` mapping of the APU IRQ level. The level sampled BEFORE `tick_apu(clock)` becomes CPU-visible on `clock+1`; a transition `tick_apu` raises or clears reaches the detector on the next dot.
_Avoid_: delayed irq, apu lag, one-cycle delay

**Cartridge IRQ sample**:
The CPU-cycle quantization of mapper IRQ by sampling `cartridge_irq_pending()` only on the CPU dot (post-PPU, dot 2) in `NesMachine::tick_dot_2`. MMC3/VRC counters are PPU-dot-clocked, so a mid-cycle trip becomes CPU-visible at the next CPU boundary; tests pin the CPU-cycle quantization.
_Avoid_: cartridge bool, irq buffer, latched irq (ambiguous)

## Language — CPU Bus Cycles
**Bus cycle classification**:
The single source of truth for what a pending Microcode cycle drives on the bus — driven by `Microcode::exec` itself (ADR-0013, revises ADR-0007). Each `exec` drives its access (PC dummy fetch for `Nop`, `read_zero_page` for `ZeroPage` dummy, `DummyReadAt` for branch fixup, `DummyStackRead` for RTS/RTI/PLP, etc.) and `bus_cycle` is derived from that execution, not a parallel classifier. The exhaustive match over `Microcode` (including `DummyReadAt(u16)`, `DummyStackRead`) classifies each driven access as `Read(ReadAddress::Latch(addr))`, `Read(Stack(...))`, `Write`, or `Internal`; DMC DMA halt decisions consume it and nothing else.
_Avoid_: cycle kind, write-operation check, bus op
**Halt-repeat read**:
The bus behavior while the CPU is RDY-halted by DMA: the bus re-drives the address the pending read cycle would drive. Internal cycles have no address of their own and repeat the last completed read. Write cycles never sit under a halt — the CPU is only halted between them.
_Avoid_: RDY repeat, dummy read (the DMA's own alignment cycles), repeated fetch

## Language — DMA Bus

Core domain for the single bus that the CPU, OAM DMA, and DMC DMA time-share. The bus is the deep module that localizes DMA arbitration; the PPU/APU/MCU are not bus owners.

**DmaBus**:
The struct that owns the active DMA transfer for both channels and the arbitration policy between them. Lives in `nes_core::dma_bus` as `struct DmaBus { dmc: DmcDma, oam: Option<OamActive> }`. Its interface is `tick(cpu, bus, clock) -> BusOwner`, `is_busy(&mcu) -> bool`, `suppress_new_dmc_requests(&mut mcu)`, `reset()`. `DmcDma`'s 7-state machine stays a private implementation detail behind it. (Formerly called the "DMA Bus", `Bus` in `nes_core::bus`; renamed `DmaBus` in ADR-0017 when the memory `Bus` took that name.)
_Avoid_: bus (unqualified for DMA), `Bus::tick` for DMA

**Bus (memory bus)**:
The CPU's external bus, `Bus<M: Mcu>` in `nes_core::bus` (ADR-0017, ADR-0018): the owner of the address-space `Mcu` **and the master `SystemClock`** a CPU drives. `Cpu` never stores memory or time; every CPU method that reads or writes them takes `&mut Bus<M>` (`bus.clock()` / `Bus::advance()`), and owners hold `cpu: Cpu` next to `bus: Bus<M>` as paired fields. Tests build the pair the same way (`Cpu::new()` + `Bus::new(mcu)`); stage dots with `bus.set_clock()`. `advance()` is machine-loop authority — one call per dot.
_Avoid_: bus (without qualifier) for the MCU wrapper, putting the `Mcu` back inside `Cpu`, advancing the bus clock per device call
_Avoid_: DMA coordinator (acceptable alias in prose, not in code), bus controller, DMA engine
_Avoid_: bus (without qualifier) for the MCU wrapper, putting the `Mcu` back inside `Cpu`
_Avoid_: DMA coordinator (acceptable alias in prose, not in code), bus controller, DMA engine

**BusOwner**:
Per-dot holder of the bus returned by `DmaBus::tick`: `Idle` (CPU owns the bus), `Oam` (OAM DMA held the bus this dot — includes halt, alignment, and transfer cycles), or `Dmc` (DMC DMA held/drove the bus this dot). The caller (`NesMachine::tick`) skips `Cpu::tick` / `update_interrupt_lines` when the owner is not `Idle`; DMC's `cpu.frozen` stall remains the same mechanism but is driven from inside `DmaBus::tick`.
_Avoid_: bus state, dma owner, active dma

**DMA arbitration (DMC-wins)**:
The single arbitration policy the `DmaBus` implements: a DMC DMA read (`dmc_drove_bus` true) that lands on an OAM DMA read phase (`!needs_startup_align && transfer_cycle.is_multiple_of(2)`) aborts the OAM read, which is redone after one alignment cycle (`needs_pause_align = true`). The `dmc_drove_bus` bool and the `NesMcu` collision predicate are a private branch inside `DmaBus::tick`; no external bool is threaded.
_Avoid_: collision handling (ok in commentary, not the canonical term), dma priority

**Bus quiescence**:
The condition `DmaBus::is_busy(&mcu)` used by `NesMachine::reset` to drain the bus. True while any DMA is active (`dmc.is_busy()` or OAM active) or a `$4014` OAM request is still queued in the producer. Drain loops `while dma_bus.is_busy()` with `dma_bus.suppress_new_dmc_requests()` each dot until quiet, then `dma_bus.reset()` clears both channels at once.
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

**Sprite Row Cache**:
The `SpriteManager`'s per-scanline pattern-plane cache, index-parallel to Secondary OAM. Filled lazily on the first pixel request of a scanline: one `resolve_pixel_addr(src_y)` plus two cartridge VRAM reads per evaluated Sprite, with vertical flip and 8x16 height applied at fill time. The per-pixel path decodes from the cached `low`/`high` planes, and `sprite_zero_pixel_opaque` reads the cached row of the evaluated zero Sprite (no own fetch). Invalidated on the same events as the background `TileCache`: the dot-0 Secondary OAM swap, `$2000` control writes, PPUDATA writes below `$2000`, and reset.
_Avoid_: sprite cache, OAM cache

## Language — PPU VBL/NMI Race

**VBL set dot (enter_vblank)**:
Scanline 241 dot 1 — the single PPU tick where the VBL flag would set and the NMI edge would assert. `Ppu::tick` processes the current `timing.dot` then `advance`s; `enter_vblank()` is true only at `(241,1)`.
_Avoid_: vblank flag set, nmi set

**$2002 race window**:
Hardware PPU_frame_timing: a `$2002` read one PPU clock before the set dot (hardware dot 0 of scanline 241, observed as `timing.dot==1` at read time because `NesMachine::tick` advances PPU before the CPU read) suppresses the upcoming set entirely — flag never rises, NMI never asserts. A read on the set dot itself races the set and clears the flag; the resulting 1-dot `/NMI` assertion at `(241,1)` is never sampled by the CPU (which samples the line two dots back, once per CPU cycle), so no NMI fires. `suppress_vblank_pending` is the one-dot pending flag set at `dot==1` read time and consumed at the next `enter_vblank`.
_Avoid_: vblank suppression (overloaded), nmi suppression

## Language — Mapper MMC1

**MMC1 consecutive-cycle write filter**:
MMC1's serial port ignores data-bit writes (`$8000-$FFFF` with bit7 clear) that occur within 1 CPU cycle (3 PPU/system cycles) of the previous mapper write. Reset writes (`bit7 set`) are never ignored but do update the timestamp (`last_write_cycle: Option<SystemClock>`). The filter is checked in the mapper's CPU-write entry point (`Mapper::cpu_write(address, value, cycle: SystemClock)`) — a write arrives at its dot via `Mcu::write(address, value, clock)`, which forwards it (`NesMcu` no longer keeps a stamped `current_clock`). Bill & Ted's `INC $FF` (reset $FF then data $00 on next cycle) and Shinsenden's `RRA` (data then reset) pin the two sides.
_Avoid_: write filter, consecutive writes (without qualifier)

## Language — Bus Decoding

**Decode into NesMcu (not Ppu)**:
`Ppu` does not own the cartridge and does not implement `Mcu`. `NesMcu` owns `cartridge: Cartridge` and performs all CPU address decode: `0x0000-0x1FFF` LowerRam, `0x2000-0x3FFF` PPU regs via `Ppu::read_ppureg`/`write_ppureg` (taking `&mut Cartridge`), `0x4000-0x401F` APU/IO, `0x4020-0x5FFF` open bus, `0x6000-0x7FFF` PRG-RAM (open bus if `!prg_ram_enabled()`), `0x8000-0xFFFF` PRG-ROM via `cartridge.read`/`write(cycle)`. Undriven reads (`0x4020-0x5FFF`, disabled PRG-RAM) refresh the CPU open bus (`open_bus` latch) and the PPU latch decays via `PPU_OPEN_BUS_DECAY_TICKS`.
_Avoid_: Ppu as Mcu, cartridge in Ppu, open bus (without qualifier)

**Fast Path Contract**:
The `Mcu` fast-path methods (`read_zero_page`, `read_stack_page`, `write_zero_page`, `write_stack_page`) may skip the address-range dispatch of `read`/`write`, never an observable side effect; the fast writes forward the same `clock` dot as the generic write. A fast read latches the CPU open bus (`open_bus`) with the value read; a fast write latches it and deasserts both joypad `/OE` lines (`joypad1_oe`, `joypad2_oe`), in that order and at the same point in the sequence as a generic write. The bus wrapper (`NesMcu`) owns these effects and applies the identical ones on both routes, so the generic and fast paths cannot drift; the memory device (`LowerRam`) answers with pure array indexing and owns no bus state. The CPU's zero-page RMW read stamps `last_read_addr` like every other read.
_Avoid_: cheap read, direct RAM access, bypassing the bus

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

**Presentation Plan**:
The pure, SDL-free description of what one frame puts on screen: `FramePresentation { bytes, width, height, dst, overlays }`, built by `plan<N>(frame, overlays, dst_size)` in `nes_sdl::presentation`. It carries the frame's RGBA bytes and dimensions (Zoom Factor applied), the destination rect (the whole window), and the overlay placements scaled from logical 1x 256x240 coordinates into destination pixels. Generic over the Framebuffer Zoom Factor `N`; consumed by the SDL adapter `FramePresenter`. Playback passes an `ImageRender<1>` frame, so SDL scales the logical picture to the window with one nearest copy; the `--output` recording path instead upscales the frame host-side into its own 1024x960 scratch frame.
_Avoid_: render plan, draw plan, frame plan

**Reused Frame Texture**:
The single sdl2 streaming texture owned by `FramePresenter`, created on first present and reused for every frame (recreated only when the frame dimensions change). Upload is one `Texture::update`; drawing is one scaled `Canvas::copy` to the plan's destination rect. Expressible without `unsafe` because sdl2's `unsafe_textures` feature makes a `Texture` owned by the `Canvas` and free of a borrow.
_Avoid_: per-frame texture, texture per frame

## Language — Profiling & Performance

Core domain for measuring and optimizing emulation throughput. Headless != no-render; Self >3% defines a Hot Path.

**Profiling Profile**:
Cargo build profile `[profile.profiling]` inheriting `release` with full debug symbols (`debug=2`, `strip=false`, `lto=false`). Frame pointers forced via `RUSTFLAGS="-C force-frame-pointers=yes"` so `perf record -g` yields complete stacks. Never shipped; only for `perf.data`/`flamegraph`.
_Avoid_: release with debug, bench profile

**Headless Execution**:
Movie playback with no host window (`--headless`), still rendering to `RecordRender` on a DummyWindow. Playback renders a logical 1x frame since spec #38, so `set_pixel` and the frame upload stay below the 1% Self cut-off (the spec measured `set_pixel` ~2.3% while the display path replicated every pixel 4x4). Distinct from a future `NullRender` zero-render mode.
_Avoid_: no-render, offscreen

**No-throttle**:
Disabling the 60Hz sleep in `PlayMovieAction` (`--no-throttle` skips `target_frame_duration` sleep). Required for throughput measurement; with it, wall time is sleep-bound, not emulation-bound.
_Avoid_: unlimited fps, uncapped

**Frame Budget**:
Wall-clock budget per emulated frame at 60fps: `16.6ms` (`target_frame_duration = 1_000_000_000/60` ns). Headless+no-throttle measures headroom against this budget.
_Avoid_: frame limit, fps target

**Hot Path**:
Instruction sampling Self overhead >3% in `perf report --no-children`. Leaf cost after inlining; Children is call-graph roll-up. Baseline 2000-frame windowed `warped.fm2` sample (`--no-throttle`, profiling profile, frame pointers, `perf record -F 997 -g`): `Ppu::tick` 47.7%, `NesMachine::tick` 6.5%, `Microcode::exec` 6.2%, `Cartridge::read` 3.6%, `Cpu::tick` 3.4%, `SpriteManager::update_ctrl_status` 3.4%, `NesMcu::tick_apu` 3.3%, `Cartridge::on_ppu_tick` 3.0%. The shares are relative: spec #38 removed work around these leaves, so the survivors rank higher over a smaller total (instructions per frame 59.0 M to 51.0 M). A small per-Dot leaf can sample above its true cost — `update_ctrl_status` shows 3.4% Self, while removing the per-Dot status merge measured 0.4% of instructions in the spec #38 ablations; confirm a candidate with an ablation instead of acting on one symbol.
_Avoid_: hot spot (ambiguous), bottleneck (use only after budget miss)

**Sampling (perf)**:
`perf record -F 997 -g` sampling `cpu/cycles:Pu` at ~1kHz with call-graph into `/tmp/perf.data`. Text report via `perf report --stdio`. Overhead 1–2%; trade-off vs Cachegrind's cache-miss model (20× slower, not installed).
_Avoid_: tracing, instrumentation

**Flamegraph**:
Folded-stack visualization via `cargo flamegraph --profile profiling` collapsing `perf` samples into width proportional to Self overhead. Output `/tmp/flamegraph.svg`; complements `perf report` for call-graph navigation.
_Avoid_: flame chart (different), perf report (text)
