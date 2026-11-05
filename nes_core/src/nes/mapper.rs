use crate::SystemClock;
use crate::ines::INesFile;
use crate::ines::NametableArrangement;
use axrom::AxRom;
use bxrom::BxRom;
use cnrom::CnRom;
use memory::CartridgeMemory;
use mmc1::MMC1;
use mmc3::MMC3;
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
    let mapper_no = f.header().mapper_no;
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

/// Which PPU-side hooks a mapper chip implements, snapshotted by [`Cartridge`]
/// once at construction. Each flag mirrors one [`Mapper`] method: overriding
/// `on_ppu_tick`/`notify_vram_address`/`irq_pending` without setting the
/// matching flag here means the override is never called.
#[derive(Debug, Eq, PartialEq, Clone, Copy, Default)]
pub struct CartridgeCaps {
    pub on_ppu_tick: bool,
    pub notify_vram_address: bool,
    pub irq_pending: bool,
}

/// The bank-switching chip on a cartridge board. A mapper owns no cartridge
/// memory and performs no data read: it rewrites bank-window slots, may write
/// PRG-RAM, and reports IRQ lines. See `docs/adr/0015`.
pub trait Mapper {
    /// Called for every CPU write in `$6000-$FFFF`.
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

    /// PRG window read for `$6000-$FFFF`.
    pub fn read(&self, address: u16) -> u8 {
        if address < 0x6000 {
            return 0;
        }
        if address < 0x8000
            && let Some(value) = self.mapper.read_prg(address)
        {
            return value;
        }
        self.memory.read_prg(address)
    }

    pub fn write(&mut self, address: u16, value: u8, cycle: SystemClock) {
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

    /// Cartridge PPU space: pattern tables in `$0000-$1FFF` (CHR ROM or CHR
    /// RAM), nametables in `$2000-$2EFF`.
    pub fn read_vram(&self, address: u16) -> u8 {
        #[cfg(test)]
        if let Some(log) = &self.vram_read_log {
            log.record(address);
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
