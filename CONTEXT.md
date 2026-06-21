# NES Emulator — CHR Storage Domain

Core domain for pattern table data access across mapper implementations.

## Language

**CHR (Character Data)**:
Tile graphics data stored in pattern tables, accessed by the PPU during rendering.
_Avoid_: Pattern table data, sprite data

**ChrStorage**:
A trait defining the interface for CHR read and write operations (`read_chr`, `write_chr`). No constructor, no refresh.
_Avoid_: ChrInterface, ChrAccess

**DirectChr**:
A ChrStorage implementation where the full CHR data fits in a fixed 8KB array. Read and write map directly by address modulo 0x2000. No caching layer.
_Avoid_: FlatChr, SimpleChr

**WindowedChr**:
A ChrStorage implementation where CHR data lives in a full memory pool (`Vec<u8>`) with a cached 8KB read window. `read_chr` reads from the cache; write-through to source is provided via an inherent `write_chr_with_source` method (not on the trait). Window refresh is an inherent method, not on the trait.
_Avoid_: BankedChr, CachedChr

**Cartridge**:
Enum dispatching CPU and PPU operations to mapper-specific implementations. Holds a concrete storage type per variant.

**Mapper**:
Struct implementing PRG/CHR bank switching and memory mapping logic. Composes with a concrete `ChrStorage` (either `DirectChr` or `WindowedChr`). Owns `has_chr_ram` policy for write gating.
