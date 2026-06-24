# NES Emulator — CHR Storage Domain

Core domain for pattern table data access across mapper implementations.

## Language

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
