use crate::SystemClock;
use crate::ines::INesFile;
use crate::ines::NametableArrangement;
use axrom::AxRom;
use bxrom::BxRom;
use cnrom::CnRom;
use memory::CartridgeMemory;
use mmc1::MMC1;
use mmc3::MMC3;
use mmc5::{MMC5, wram_config};
use nina001::Nina001;
use nrom::NRom;
use uxrom::UxRom;
use vrc24::Vrc24;
pub use vrc24::VrcVariant;

/// NES 2.0 submapper marking an early TxROM board carrying the MMC3A
/// (NEC "old-style") IRQ counter. Per the NESdev iNES-004 submapper
/// table (wiki "NES 2.0 submappers", mirrored by rom-properties'
/// `NESMappers.cpp`): 004:0 = MMC3C, 1 = MMC6, 2 = MMC3C with
/// hard-wired mirroring, 3 = MC-ACC, 4 = MMC3A.
const MMC3A_NES20_SUBMAPPER: u8 = 4;
const MMC3_ALTERNATE_IRQ_SIGNATURES: [&str; 2] = ["6-MMC3_alt", "6-MMC6"];
mod axrom;
mod bxrom;
mod cnrom;
mod j87;
mod memory;
mod mmc1;
mod mmc3;
mod mmc5;
mod nina001;
mod nrom;
mod uxrom;
mod vrc24;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Mirroring {
    LowerBank, // single screen use lower bank
    UpperBank, // single screen use upper bank
    Horizontal,
    Vertical,
    Four,
}

impl Mirroring {
    /// Which physical nametable bank (`$2000`, `$2400`, `$2800`, `$2C00`)
    /// each of the four nametable slots shows.
    pub fn nt_slots(self) -> [u8; 4] {
        match self {
            Mirroring::LowerBank => [0, 0, 0, 0],
            Mirroring::UpperBank => [1, 1, 1, 1],
            Mirroring::Vertical => [0, 1, 0, 1],
            Mirroring::Horizontal => [0, 0, 1, 1],
            Mirroring::Four => [0, 1, 2, 3],
        }
    }
}

impl From<NametableArrangement> for Mirroring {
    fn from(value: NametableArrangement) -> Self {
        match value {
            // Vertical arrangement requires Horizontal mirrored
            NametableArrangement::Vertical => Self::Horizontal,
            NametableArrangement::Horizontal => Self::Vertical,
        }
    }
}

/// Selects the MMC3 IRQ scanline-counter revision for a cartridge — the
/// harness/test hook (step 1) of [`mmc3_irq_revision_is_alternate`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Mmc3IrqOverride {
    /// Run automatic detection: NES 2.0 submapper 004:4 (MMC3A), then
    /// content signatures, then Standard.
    #[default]
    Auto,
    /// Force revision-A (Alternate) semantics.
    ForceAlternate,
    /// Force Standard (revision-B/C) semantics.
    ForceStandard,
}

/// Create a cartridge, with an explicit MMC3 IRQ-revision override — the
/// harness/test hook (step 1) of [`mmc3_irq_revision_is_alternate`]:
/// [`Mmc3IrqOverride::ForceAlternate`] forces revision-A (Alternate)
/// semantics, [`Mmc3IrqOverride::ForceStandard`] forces Standard, and
/// [`Mmc3IrqOverride::Auto`] falls back to automatic detection.
pub fn create_cartridge_with_mmc3_irq_override(
    f: &INesFile,
    irq_override: Mmc3IrqOverride,
) -> Cartridge {
    let mirroring = if f.header().ignore_mirror_control {
        Mirroring::Four
    } else {
        f.header().nametable_arrangement.into()
    };
    let mut memory = CartridgeMemory::from_rom(
        f.read_prg_rom(),
        f.read_chr_rom(),
        f.header().chr_ram_size,
        mirroring,
    );
    let mapper_no = f.header().mapper_no;

    let mapper: Box<dyn Mapper> = match mapper_no {
        0 => Box::new(NRom::new(&mut memory)),
        1 => Box::new(MMC1::new(&mut memory)),
        2 => Box::new(UxRom::new(&mut memory)),
        3 => Box::new(CnRom::new(&mut memory)),
        4 => {
            let alternate_irq_revision = mmc3_irq_revision_is_alternate(
                irq_override,
                f.header().submapper_no,
                f.read_prg_rom(),
            );
            Box::new(MMC3::new(
                &mut memory,
                f.header().ignore_mirror_control,
                alternate_irq_revision,
            ))
        }
        5 => {
            let (wram_size, battery_bytes) = wram_config(f.header(), f.file_version());
            Box::new(MMC5::new(&mut memory, wram_size, battery_bytes))
        }
        7 => Box::new(AxRom::new(&mut memory)),
        34 => {
            // A 16 KB-or-smaller CHR image means the BNROM half of mapper 34.
            if memory.chr_len_bytes() > 0x2000 {
                Box::new(Nina001::new(&mut memory))
            } else {
                Box::new(BxRom::new(&mut memory))
            }
        }
        21 => {
            let submapper = f.header().submapper_no.unwrap_or(1);
            let variant = match submapper {
                2 => VrcVariant::Vrc4c,
                _ => VrcVariant::Vrc4a,
            };
            Box::new(Vrc24::new(&mut memory, variant))
        }
        22 => Box::new(Vrc24::new(&mut memory, VrcVariant::Vrc2a)),
        23 => {
            let submapper = f.header().submapper_no.unwrap_or(1);
            let variant = match submapper {
                2 => VrcVariant::Vrc4e,
                3 => VrcVariant::Vrc2b,
                _ => VrcVariant::Vrc4f,
            };
            Box::new(Vrc24::new(&mut memory, variant))
        }
        25 => {
            let submapper = f.header().submapper_no.unwrap_or(1);
            let variant = match submapper {
                2 => VrcVariant::Vrc4d,
                3 => VrcVariant::Vrc2c,
                _ => VrcVariant::Vrc4b,
            };
            Box::new(Vrc24::new(&mut memory, variant))
        }
        87 => Box::new(j87::J87::new(&mut memory)),
        _ => panic!("Unsupported cartridge mapper no: {}", f.header().mapper_no),
    };

    Cartridge::new(mapper, memory)
}

/// Single detection predicate for the MMC3 IRQ scanline-counter revision
/// (issue #32; decision order pinned by the #31 audit):
///   1. explicit override (harness/test hook),
///   2. NES 2.0 submapper 004:4 (MMC3A / early TxROM) => revision A,
///   3. existing content signatures (`6-MMC6`, `6-MMC3_alt`) as fallback,
///   4. Standard by default.
///
/// Returns `true` when the revision-A (Alternate) semantics apply:
/// after the counter reaches 0 by decrementing, the forced reload must
/// not re-assert the IRQ even with a zero latch (see `mmc3::clock_irq`).
///
/// Unit-testable without ROM probing: `prg_rom` is passed as bytes
/// directly, so an empty slice exercises steps 1, 2 and 4 with no
/// content scan.
pub fn mmc3_irq_revision_is_alternate(
    irq_override: Mmc3IrqOverride,
    submapper_no: Option<u8>,
    prg_rom: &[u8],
) -> bool {
    match irq_override {
        Mmc3IrqOverride::ForceAlternate => return true,
        Mmc3IrqOverride::ForceStandard => return false,
        Mmc3IrqOverride::Auto => {}
    }
    if submapper_no == Some(MMC3A_NES20_SUBMAPPER) {
        return true;
    }
    MMC3_ALTERNATE_IRQ_SIGNATURES
        .iter()
        .any(|signature| rom_contains_signature_bytes(prg_rom, signature))
}

fn rom_contains_signature_bytes(rom: &[u8], signature: &str) -> bool {
    rom.windows(signature.len())
        .any(|window| window == signature.as_bytes())
}

/// Which kind of PPU access a cartridge VRAM read serves (ticket #78).
/// The PPU renders per-pixel, so the MMC5 hardware fetch-counting does not
/// map: the PPU tags each read at the call site instead. MMC5 uses this to
/// pick the sprite vs background CHR bank set in 8x16 mode (wiki §3.5).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PpuFetchKind {
    /// Background pattern fetch (`fetch_tile_data`).
    Background,
    /// Sprite pattern fetch (`SpriteManager::ensure_rows`).
    Sprite,
    /// CPU `$2007` path (`Ppu::read_vram`), idle state, and tests: carries
    /// the last-written-set semantics, so `$2007` sees the most recently
    /// written CHR register range in 8x16 mode.
    #[default]
    Cpu,
}

/// Render state snapshot the PPU hands to [`Mapper::ppu_read`] on every
/// cartridge VRAM fetch (ticket #75 seam). The mapper must not read PPU
/// registers itself: the PPU owns the render gate and passes down exactly
/// what the hook may depend on.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct PpuRenderState {
    /// True when background or sprites are enabled (PPUMASK `$2001`).
    pub rendering_enabled: bool,
    /// True when PPUCTRL `$2000` selects 8x16 sprites.
    pub sprite_8x16: bool,
    /// Fetch phase within the 8-dot tile (`ppu dot % 8`). Informational:
    /// this PPU renders per-pixel, not per-dot shifter, so the phase only
    /// documents *when* in the tile fetch the read happens.
    pub fetch_phase: u8,
    /// What kind of access this read serves (background/sprite pattern
    /// fetch vs CPU `$2007`). See [`PpuFetchKind`].
    pub fetch: PpuFetchKind,
}

impl PpuRenderState {
    /// Render state for non-render reads (CPU `$2007` path, tests):
    /// rendering off, 8x8 sprites, phase 0.
    pub fn disabled() -> Self {
        Self::default()
    }
}

/// Which PPU-side hooks a mapper chip implements, snapshotted by [`Cartridge`]
/// once at construction. Each flag mirrors one [`Mapper`] method: overriding
/// `on_ppu_tick`/`notify_vram_address`/`irq_pending`/`ppu_read` without
/// setting the matching flag here means the override is never called.
#[derive(Debug, Eq, PartialEq, Clone, Copy, Default)]
pub struct CartridgeCaps {
    pub on_ppu_tick: bool,
    pub notify_vram_address: bool,
    pub irq_pending: bool,
    /// When set, [`Cartridge::read_vram`] consults [`Mapper::ppu_chr_bank`]
    /// (CHR bank redirect, ticket #78) and then [`Mapper::ppu_read`] before
    /// the bank-window lookup. MMC5 sets this; nothing else does yet, so
    /// other boards behave exactly as before.
    pub ppu_read: bool,
    /// When set, CPU reads from `$5000-$5FFF` consult
    /// [`Mapper::read_expansion`] (and debugger peeks consult
    /// [`Mapper::peek_expansion`]). Only the MMC5 sets this, so every other
    /// board keeps the pre-seam open-bus answer at zero per-read cost
    /// beyond one bool check (ticket #81: `$5204` status reads).
    pub read_expansion: bool,
}

/// The bank-switching chip on a cartridge board. A mapper owns no cartridge
/// memory and performs no data read: it rewrites bank-window slots, may write
/// PRG-RAM, and reports IRQ lines. See `docs/adr/0015`.
pub trait Mapper {
    /// Called for every CPU write in `$5000-$FFFF`.
    fn cpu_write(
        &mut self,
        address: u16,
        value: u8,
        cycle: SystemClock,
        memory: &mut CartridgeMemory,
    );
    /// Overrides the PRG-RAM window read when the board answers the address
    /// itself (VRC2 has no PRG-RAM). Only consulted for `$6000-$7FFF`, so
    /// CPU instruction fetches never pay for it.
    fn read_prg(&self, _address: u16) -> Option<u8> {
        None
    }
    /// Answers a CPU read from the cartridge expansion area (`$5000-$5FFF`:
    /// MMC5 status registers, ExRAM). Only consulted when the chip declares
    /// [`CartridgeCaps::read_expansion`]; `None` falls through to open bus,
    /// preserving the pre-seam behavior for every other board.
    ///
    /// Takes `&self` like [`Mapper::read_prg`]: ack-on-read side effects
    /// (MMC5 `$5204` clears pending) live behind interior mutability in the
    /// chip impl, exactly like the [`Mapper::ppu_read`] detector state.
    fn read_expansion(&self, _address: u16) -> Option<u8> {
        None
    }
    /// Side-effect-free expansion-area peek for the debugger: same answer as
    /// [`Mapper::read_expansion`] but never acknowledges (a `$5204` peek
    /// must not clear a pending IRQ out from under the CPU).
    fn peek_expansion(&self, _address: u16) -> Option<u8> {
        None
    }
    /// Optional per-fetch PPU-read override, consulted by
    /// [`Cartridge::read_vram`] before the bank-window lookup and only when
    /// the chip declares [`CartridgeCaps::ppu_read`]. Returning `None` falls
    /// through to the 1 KB window. The default is `None`, so boards without
    /// the flag pay one bool check and behave exactly as before.
    ///
    /// Takes `&self`: board state the hook needs (e.g. the MMC5 ExAttr
    /// latch) lives behind interior mutability in the chip impl.
    fn ppu_read(&self, _addr: u16, _render: &PpuRenderState) -> Option<u8> {
        None
    }
    /// Optional CHR bank-redirect channel (ticket #78): consulted by
    /// [`Cartridge::read_vram`] for `$0000-$1FFF` before [`Mapper::ppu_read`]
    /// and only when the chip declares [`CartridgeCaps::ppu_read`] (the same
    /// flag gates both hooks, so no caps churn).
    ///
    /// Returns the 1K CHR bank number the address reads from; the cartridge
    /// applies its pool modulo and serves the byte itself. Returning `None`
    /// falls through to `ppu_read`/the bank window. Addresses at/above
    /// `$2000` must return `None`: the nametable plane belongs to the ExRAM
    /// tickets (#79/#82).
    ///
    /// This exists because `ppu_read` cannot serve CHR data: the mapper owns
    /// no memory (ADR-0015) and `&self` cannot reach the pool, so CHR banking
    /// redirects the bank while `ppu_read` stays the hook for mapper-owned
    /// bytes (ExRAM/fill).
    fn ppu_chr_bank(&self, _addr: u16, _render: &PpuRenderState) -> Option<u16> {
        None
    }
    fn on_ppu_tick(&mut self, _scanline: u16) {}
    fn notify_vram_address(&mut self, _addr: u16) {}
    fn irq_pending(&self) -> bool {
        false
    }
    /// Which of the three hooks above this chip implements. [`Cartridge`]
    /// snapshots this once, so an override that is not declared here is dead
    /// code — `just passed` is what catches a forgotten flag.
    fn caps(&self) -> CartridgeCaps {
        CartridgeCaps::default()
    }
}

/// The cartridge board: bank-mapped memory, the mapper chip, and the
/// capability snapshot taken from that chip at construction. The snapshot
/// gates the board's own forwards, so callers never read capability bits.
pub struct Cartridge {
    memory: CartridgeMemory,
    mapper: Box<dyn Mapper>,
    caps: CartridgeCaps,
    #[cfg(test)]
    vram_read_log: Option<VramReadLog>,
}

impl Cartridge {
    pub fn new(mapper: Box<dyn Mapper>, memory: CartridgeMemory) -> Self {
        let caps = mapper.caps();
        Self {
            memory,
            mapper,
            caps,
            #[cfg(test)]
            vram_read_log: None,
        }
    }

    /// PRG-ROM read for `$8000-$FFFF` via the mapper-banked window.
    ///
    /// PRG-RAM (`$6000-$7FFF`) has its own [`Cartridge::read_prg_ram`]; this
    /// never serves it. Unlike the PPU's side-effect-free peek, this returns
    /// raw cartridge data with no open-bus substitution: a disabled PRG-RAM
    /// window still answers with its bank contents, and `NesMcu` is what
    /// turns that into open bus.
    pub(crate) fn read(&self, address: u16) -> u8 {
        debug_assert!(
            address >= 0x8000,
            "Cartridge::read address {address:#06x} below $8000"
        );
        self.memory.read_prg(address)
    }

    /// PRG-RAM read for `$6000-$7FFF`.
    ///
    /// The mapper chip gets first refusal via [`Mapper::read_prg`] (only
    /// VRC2's bus-byte reads override it), otherwise the board's PRG-RAM
    /// bank answers via [`CartridgeMemory::read_prg_ram`].
    pub(crate) fn read_prg_ram(&self, address: u16) -> u8 {
        debug_assert!(
            (0x6000..=0x7fff).contains(&address),
            "Cartridge::read_prg_ram address {address:#06x} outside $6000-$7FFF"
        );
        if let Some(value) = self.mapper.read_prg(address) {
            return value;
        }
        self.memory.read_prg_ram(address)
    }

    /// PRG-ROM write for `$8000-$FFFF`: mapper register writes only.
    pub fn write(&mut self, address: u16, value: u8, cycle: SystemClock) {
        debug_assert!(
            address >= 0x8000,
            "Cartridge::write address {address:#06x} below $8000"
        );
        let Self { memory, mapper, .. } = self;
        mapper.cpu_write(address, value, cycle, memory);
    }

    /// PRG-RAM write for `$6000-$7FFF` via the mapper's CPU-write path, so
    /// mapper side-effects (NINA-001/J87/VRC2 registers, MMC3 write-protect,
    /// enable gating) stay exactly as on the generic write.
    pub fn write_prg_ram(&mut self, address: u16, value: u8, cycle: SystemClock) {
        debug_assert!(
            (0x6000..=0x7fff).contains(&address),
            "Cartridge::write_prg_ram address {address:#06x} outside $6000-$7FFF"
        );
        let Self { memory, mapper, .. } = self;
        mapper.cpu_write(address, value, cycle, memory);
    }

    /// CPU writes to `$5000-$5FFF`: the cartridge expansion area (MMC5
    /// registers live here). Forwards to the mapper chip like the other
    /// write windows; chips without expansion registers ignore it.
    pub fn write_expansion(&mut self, address: u16, value: u8, cycle: SystemClock) {
        debug_assert!(
            (0x5000..=0x5fff).contains(&address),
            "Cartridge::write_expansion address {address:#06x} outside $5000-$5FFF"
        );
        let Self { memory, mapper, .. } = self;
        mapper.cpu_write(address, value, cycle, memory);
    }

    /// Forwarded only when the mapper declares [`CartridgeCaps::on_ppu_tick`].
    #[inline]
    pub fn on_ppu_tick(&mut self, scanline: u16) {
        if self.caps.on_ppu_tick {
            self.mapper.on_ppu_tick(scanline);
        }
    }

    /// Forwarded only when the mapper declares
    /// [`CartridgeCaps::notify_vram_address`].
    #[inline]
    pub fn notify_vram_address(&mut self, addr: u16) {
        if self.caps.notify_vram_address {
            self.mapper.notify_vram_address(addr);
        }
    }

    /// True when the mapper chip implements `notify_vram_address`
    /// (MMC3/VRC scanline-IRQ A12 filtering). The PPU consults this before
    /// computing per-dot pattern-table fetch phases, so boards without
    /// scanline IRQs pay no per-dot cost for that math.
    pub fn wants_vram_notify(&self) -> bool {
        self.caps.notify_vram_address
    }

    /// The mapper's IRQ line, held low for a board that declares no IRQ
    /// hardware, so the per-CPU-cycle poll never enters the vtable.
    #[inline]
    pub fn irq_pending(&self) -> bool {
        self.caps.irq_pending && self.mapper.irq_pending()
    }

    /// True when the mapper chip implements `ppu_read` (MMC5 per-fetch
    /// overrides). Only render-path callers pay the vtable cost, and only
    /// when this is set — mirroring [`Self::wants_vram_notify`].
    pub fn wants_ppu_read(&self) -> bool {
        self.caps.ppu_read
    }

    /// CPU read from the cartridge expansion area (`$5000-$5FFF`). Only the
    /// MMC5 declares [`CartridgeCaps::read_expansion`]; every other board
    /// answers `None` (the caller turns that into open bus), so the seam
    /// costs non-MMC5 boards one bool check per expansion read.
    pub fn read_expansion(&self, address: u16) -> Option<u8> {
        debug_assert!(
            (0x5000..=0x5fff).contains(&address),
            "Cartridge::read_expansion address {address:#06x} outside $5000-$5FFF"
        );
        if self.caps.read_expansion {
            self.mapper.read_expansion(address)
        } else {
            None
        }
    }

    /// Side-effect-free expansion-area peek: never acknowledges, so the
    /// debugger can inspect `$5204` without clearing a pending IRQ.
    pub fn peek_expansion(&self, address: u16) -> Option<u8> {
        debug_assert!(
            (0x5000..=0x5fff).contains(&address),
            "Cartridge::peek_expansion address {address:#06x} outside $5000-$5FFF"
        );
        if self.caps.read_expansion {
            self.mapper.peek_expansion(address)
        } else {
            None
        }
    }

    /// Cartridge PPU space: pattern tables in `$0000-$1FFF` (CHR ROM or CHR
    /// RAM), nametables in `$2000-$2EFF`.
    ///
    /// The mapper-side PPU-read seam (tickets #75/#78): when the chip declares
    /// [`CartridgeCaps::ppu_read`], [`Mapper::ppu_chr_bank`] gets first refusal
    /// for `$0000-$1FFF` (bank redirect into the CHR pool), then
    /// [`Mapper::ppu_read`] gets first refusal with the PPU-built
    /// [`PpuRenderState`]; `None` at each step falls through to the 1 KB
    /// bank-window lookup below. `$2000+` consults `ppu_read` only.
    pub fn read_vram(&self, address: u16, render: &PpuRenderState) -> u8 {
        #[cfg(test)]
        if let Some(log) = &self.vram_read_log {
            log.record(address);
        }
        if self.caps.ppu_read {
            if address < 0x2000
                && let Some(bank) = self.mapper.ppu_chr_bank(address, render)
            {
                return self.memory.read_chr_bank(bank, address);
            }
            if let Some(byte) = self.mapper.ppu_read(address, render) {
                return byte;
            }
        }
        self.memory.read_vram(address)
    }

    pub fn write_vram(&mut self, address: u16, value: u8) {
        self.memory.write_vram(address, value);
    }

    pub fn prg_ram_enabled(&self) -> bool {
        self.memory.prg_ram_enabled()
    }
}

/// Test-only log of the cartridge VRAM reads made through
/// [`Cartridge::read_vram`]: the pattern-table bytes the PPU fetches and the
/// nametable/attribute bytes it reads. Addresses are recorded in read order;
/// pattern-table reads are the entries below `$2000`.
#[cfg(test)]
#[derive(Clone, Default)]
pub struct VramReadLog {
    addresses: std::rc::Rc<std::cell::RefCell<Vec<u16>>>,
}

#[cfg(test)]
impl VramReadLog {
    /// Every recorded address, in the order the cartridge was read.
    pub fn addresses(&self) -> Vec<u16> {
        self.addresses.borrow().clone()
    }

    /// Number of reads recorded so far.
    pub fn count(&self) -> usize {
        self.addresses.borrow().len()
    }

    /// Drops every recorded read, so a later [`Self::addresses`] starts empty.
    pub fn clear(&self) {
        self.addresses.borrow_mut().clear();
    }

    fn record(&self, address: u16) {
        self.addresses.borrow_mut().push(address);
    }
}

/// Test-only recorder attached to a cartridge, holding both observable logs:
/// the mapper VRAM-address notifications and the cartridge VRAM reads.
/// Obtained from [`Cartridge::test_recording_all`].
#[cfg(test)]
#[derive(Clone, Default)]
pub struct TestRecorder {
    /// Mapper VRAM-address notifications (the log `test_recording` returns).
    pub notified: std::rc::Rc<std::cell::RefCell<Vec<u16>>>,
    /// Cartridge VRAM reads, in read order.
    pub vram_reads: VramReadLog,
}

/// Test mapper: no banking, no IRQ. It always records the VRAM-address
/// notifications it *receives*; whether it receives any is up to its declared
/// capabilities, so `new()` (none declared) probes the capability gate and
/// `recording()` (notification declared) is the one the PPU tests use.
#[cfg(test)]
pub struct TestMapper {
    caps: CartridgeCaps,
    pub notified: std::rc::Rc<std::cell::RefCell<Vec<u16>>>,
}

#[cfg(test)]
impl TestMapper {
    fn new() -> Self {
        Self {
            caps: CartridgeCaps::default(),
            notified: Default::default(),
        }
    }

    fn recording() -> Self {
        Self {
            caps: CartridgeCaps {
                notify_vram_address: true,
                ..CartridgeCaps::default()
            },
            ..Self::new()
        }
    }
}

#[cfg(test)]
impl Mapper for TestMapper {
    fn cpu_write(
        &mut self,
        _address: u16,
        _value: u8,
        _cycle: SystemClock,
        _memory: &mut CartridgeMemory,
    ) {
    }

    fn notify_vram_address(&mut self, addr: u16) {
        self.notified.borrow_mut().push(addr);
    }

    fn caps(&self) -> CartridgeCaps {
        self.caps
    }
}

#[cfg(test)]
impl Cartridge {
    /// A cartridge carrying the test mapper: 32 KB zeroed PRG ROM, 8 KB CHR
    /// RAM, horizontal mirroring.
    pub fn test() -> Self {
        Self::new(Box::new(TestMapper::new()), CartridgeMemory::test(&[], &[]))
    }

    /// Same board, with VRAM-address notifications recorded; the second value
    /// is the observable log. For both logs (notifications and cartridge VRAM
    /// reads), see [`Self::test_recording_all`].
    pub fn test_recording() -> (Self, std::rc::Rc<std::cell::RefCell<Vec<u16>>>) {
        let (cartridge, recorder) = Self::test_recording_all();
        (cartridge, recorder.notified)
    }

    /// Same board, with both observable logs: mapper VRAM-address
    /// notifications and every cartridge VRAM read, in read order.
    pub fn test_recording_all() -> (Self, TestRecorder) {
        let mapper = TestMapper::recording();
        let notified = mapper.notified.clone();
        let vram_reads = VramReadLog::default();
        let mut cartridge = Self::new(Box::new(mapper), CartridgeMemory::test(&[], &[]));
        cartridge.vram_read_log = Some(vram_reads.clone());
        (
            cartridge,
            TestRecorder {
                notified,
                vram_reads,
            },
        )
    }
}

#[cfg(test)]
mod tests;
