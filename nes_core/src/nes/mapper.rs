use crate::SystemClock;
use crate::ines::INesFile;
use crate::ines::NametableArrangement;
use axrom::AxRom;
use bxrom::BxRom;
use cnrom::CnRom;
use mmc1::MMC1;
use mmc3::MMC3;
use nina001::Nina001;
use nrom::NRom;
use uxrom::UxRom;
use vrc24::Vrc24;
pub use vrc24::VrcVariant;

const CARTRIDGE_START_ADDR: u16 = 0x4020;
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
mod mmc1;
mod mmc3;
mod nina001;
mod nrom;
mod uxrom;
mod vrc24;

pub mod chr_storage;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Mirroring {
    LowerBank, // single screen use lower bank
    UpperBank, // single screen use upper bank
    Horizontal,
    Vertical,
    Four,
}

impl Mirroring {
    pub fn name_table_offset(self, addr: u16) -> u16 {
        match self {
            Mirroring::LowerBank => addr & 0x3ff,
            Mirroring::UpperBank => addr & 0x3ff | 0x400,
            Mirroring::Horizontal => {
                let bit = addr & 0x800;
                addr & (!0xfc00) | (bit >> 1)
            }
            Mirroring::Vertical => addr & (!0xf800),
            Mirroring::Four => addr & (!0xf000),
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
) -> (Box<dyn Cartridge>, Mirroring) {
    let mapper_no = f.header().mapper_no;
    let mirroring = if f.header().ignore_mirror_control {
        Mirroring::Four
    } else {
        f.header().nametable_arrangement.into()
    };
    let chr_rom = f.read_chr_rom();
    match mapper_no {
        0 => (Box::new(NRom::new(f.read_prg_rom(), chr_rom)), mirroring),
        1 => (
            Box::new(MMC1::new(f.read_prg_rom(), chr_rom, mirroring)),
            mirroring,
        ),
        2 => (Box::new(UxRom::new(f.read_prg_rom(), chr_rom)), mirroring),
        3 => (Box::new(CnRom::new(f.read_prg_rom(), chr_rom)), mirroring),
        4 => {
            let alternate_irq_revision = mmc3_irq_revision_is_alternate(
                irq_override,
                f.header().submapper_no,
                f.read_prg_rom(),
            );
            (
                Box::new(MMC3::new(
                    f.read_prg_rom(),
                    chr_rom,
                    f.header().chr_ram_size,
                    f.header().ignore_mirror_control,
                    alternate_irq_revision,
                )),
                mirroring,
            )
        }
        7 => (Box::new(AxRom::new(f.read_prg_rom(), chr_rom)), mirroring),
        34 => {
            let is_nina = chr_rom.len() > 0x2000;
            if is_nina {
                (Box::new(Nina001::new(f.read_prg_rom(), chr_rom)), mirroring)
            } else {
                (Box::new(BxRom::new(f.read_prg_rom(), chr_rom)), mirroring)
            }
        }
        21 => {
            let submapper = f.header().submapper_no.unwrap_or(1);
            let variant = match submapper {
                2 => VrcVariant::Vrc4c,
                _ => VrcVariant::Vrc4a,
            };
            (
                Box::new(Vrc24::new(f.read_prg_rom(), chr_rom, variant)),
                mirroring,
            )
        }
        22 => {
            let variant = VrcVariant::Vrc2a;
            (
                Box::new(Vrc24::new(f.read_prg_rom(), chr_rom, variant)),
                mirroring,
            )
        }
        23 => {
            let submapper = f.header().submapper_no.unwrap_or(1);
            let variant = match submapper {
                2 => VrcVariant::Vrc4e,
                3 => VrcVariant::Vrc2b,
                _ => VrcVariant::Vrc4f,
            };
            (
                Box::new(Vrc24::new(f.read_prg_rom(), chr_rom, variant)),
                mirroring,
            )
        }
        25 => {
            let submapper = f.header().submapper_no.unwrap_or(1);
            let variant = match submapper {
                2 => VrcVariant::Vrc4d,
                3 => VrcVariant::Vrc2c,
                _ => VrcVariant::Vrc4b,
            };
            (
                Box::new(Vrc24::new(f.read_prg_rom(), chr_rom, variant)),
                mirroring,
            )
        }
        87 => {
            let prg_len = f.read_prg_rom().len();
            (
                Box::new(j87::J87::new(f.read_prg_rom(), prg_len, chr_rom)),
                mirroring,
            )
        }
        _ => panic!("Unsupported cartridge mapper no: {}", f.header().mapper_no),
    }
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

#[derive(Debug, Clone, Copy, Default)]
pub struct CartridgeCaps {
    pub on_ppu_tick: bool,
    pub notify_vram_address: bool,
    pub irq_pending: bool,
}

pub trait Cartridge {
    fn read(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, value: u8, cycle: SystemClock) -> CartridgeOperation;
    fn on_ppu_tick(&mut self, _scanline: u16) {}
    fn notify_vram_address(&mut self, _addr: u16) {}
    fn irq_pending(&self) -> bool {
        false
    }
    fn ppu_capabilities(&self) -> CartridgeCaps {
        CartridgeCaps::default()
    }
    fn read_chr(&self, _address: u16) -> u8 {
        0
    }
    fn write_chr(&mut self, _address: u16, _value: u8) {}
    /// Whether PRG-RAM at $6000-$7FFF is currently enabled (open bus if disabled).
    /// Mappers with a PRG-RAM disable bit override this.
    fn prg_ram_enabled(&self) -> bool {
        true
    }
}
#[cfg(test)]
pub struct TestCartridge {
    pub(crate) prg_rom: [u8; 0x8000],
    pub(crate) chr: chr_storage::DirectChr,
}

#[cfg(test)]
impl TestCartridge {
    pub fn new() -> Self {
        Self {
            prg_rom: [0; 0x8000],
            chr: chr_storage::DirectChr::empty(),
        }
    }
}

#[cfg(test)]
impl Cartridge for TestCartridge {
    fn read(&self, address: u16) -> u8 {
        if address >= 0x8000 {
            self.prg_rom[(address - 0x8000) as usize]
        } else {
            0
        }
    }

    fn write(&mut self, address: u16, value: u8, _cycle: SystemClock) -> CartridgeOperation {
        let _ = (address, value);
        CartridgeOperation::None
    }

    fn read_chr(&self, address: u16) -> u8 {
        self.chr.read_chr(address)
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        self.chr.write_chr(address, value);
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CartridgeOperation {
    None,
    UpdateNametableMirroring(Mirroring),
}

#[cfg(test)]
mod tests;
