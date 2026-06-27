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

## Language — System Timing

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
