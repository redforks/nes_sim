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
A construction-only phase from `Ppu::new()` until the first 261→0 scanline wrap. The PPU starts at scanline 261 (pre-render) so that vertical scroll reload, background activation, sprite evaluation, and MMC3 A12 toggles all run before the first visible scanline 0. After the first wrap, the PPU enters normal frame operation. `Ppu::reset()` does _not_ re-enter Cold-start. Cold-start is a domain concept — no code field represents it; setting initial scanline to 261 is sufficient.
_Avoid_: warm-up, boot-phase, pre-frame, initialization phase

**SystemClock**:
A `Copy` newtype over `u64`. Represents the current master clock cycle. One system cycle equals one PPU dot. Every tick advances the clock by one. Owned by `NesMachine` and passed down to CPU/APU/DMA modules as an immutable parameter — no global state.
_Avoid_: system cycles (use for the raw u64 count only), global clock, static clock

**CpuClockPhase**:
Enum with variants `First`, `Middle`, `Last`. Derived from `SystemClock` modulo `SYSTEM_CYCLES_PER_CPU_CYCLE` (3). One CPU cycle spans 3 system ticks (phases 0, 1, 2). CPU and APU advance only on `Last` phase (cycle % 3 == 2). `Middle` is always skipped.

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
The single source of truth for what a pending Microcode cycle drives on the bus: a read from the program counter (instruction stream, including the hardware dummy read of implied ops), a read through the address latch (per-variant adjusted — the no-carry low-byte increment), a read from the stack (pop cycles, $0100 | SP+1), a read at a fixed vector address, a write, or internal (no bus access of its own). Declared in one exhaustive match; DMC DMA halt decisions consume it and nothing else.
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
A double-buffered 8-entry sprite buffer inside the PPU. `SpriteManager` evaluates primary OAM during dots 65–256 and copies up to 8 in-range sprites into the *next* buffer. At dot 0 each scanline, the buffers swap: the freshly populated buffer becomes *current* and feeds `find_sprite_pixel` for the whole scanline. Models the real-hardware internal OAM that avoids a 64-sprite scan per pixel.
_Avoid_: sec OAM, sprite cache

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
