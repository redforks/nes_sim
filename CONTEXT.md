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
A single system-cycle step. `NesMachine::tick()` advances the clock by one, then calls each device's tick method in order: PPU → cartridge IRQ → APU → DMC DMA → OAM DMA → NMI → CPU → interrupt detection. Each device uses the clock to decide whether to advance its internal state.

**Microcode**:
A single step of the CPU's internal microcode machine. Multiple microcodes may execute across several ticks to complete one 6502 instruction. Microcodes must be drained with full device interleaving (PPU/APU/DMA ticked between each) — not in a tight loop.

**Frame**:
One complete PPU frame (262 scanlines × 341 dots). `NesMachine::process_frame()` calls `tick()` in a loop until VBlank (scanline 241, dot 1) or halt.

**Reset quiescence**:
The bus-drain contract of `NesMachine::reset()`. Once the reset line is asserted the CPU stops being fed; PPU/APU keep interleaving and any DMA work already accepted by the bus completes before the device resets apply. Fresh DMC fetch requests are suppressed during the drain so a playing sample channel cannot extend it. Each owner resets its own state under this one seam (`Cpu::reset`, `NesMcu::reset(clock)`, `DmcDma::reset`), and time-relative state re-anchors to the running `SystemClock`.
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

## Language — CPU Bus Cycles

**Bus cycle classification**:
The single source of truth for what a pending Microcode cycle drives on the bus: a read from the program counter (instruction stream, including the hardware dummy read of implied ops), a read through the address latch (per-variant adjusted — the no-carry low-byte increment), a read from the stack (pop cycles, $0100 | SP+1), a read at a fixed vector address, a write, or internal (no bus access of its own). Declared in one exhaustive match; DMC DMA halt decisions consume it and nothing else.
_Avoid_: cycle kind, write-operation check, bus op

**Halt-repeat read**:
The bus behavior while the CPU is RDY-halted by DMA: the bus re-drives the address the pending read cycle would drive. Internal cycles have no address of their own and repeat the last completed read. Write cycles never sit under a halt — the CPU is only halted between them.
_Avoid_: RDY repeat, dummy read (the DMA's own alignment cycles), repeated fetch

## Language — APU Timers

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
