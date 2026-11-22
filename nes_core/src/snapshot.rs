//! Versioned JSON save-state DTO shell (ticket #192, ADR-0020/0023).
//!
//! File format: `serde_json` text. Top-level keys are `format_version`
//! (u32, the single [`SAVE_FORMAT_VERSION`] constant), `rom_sha256` (64
//! lowercase hex), `frame` (u64 `Timing.frame_no`), `clock` (u64, restored
//! into both the bus clock and the PPU cycle counter), then component
//! sections `cpu`, `bus`, `dma`, `ppu`, `apu`, `controller`, `cartridge`.
//!
//! Encoding rules (per #184): byte arrays are lowercase hex strings, one
//! per array; bitfield registers are raw bits; core enums use mirror
//! strings and load refuses unknown names. Core types get no serde
//! derives: only the DTO structs in this module derive
//! `Serialize`/`Deserialize`.
//!
//! Ticket #192 scope: the `cartridge` section is filled from live state
//! via the mapper snapshot API; every other component section exists with
//! its final name but is left defaulted (ticket #193 fills them). The
//! `ppu` section carries the live scanline/dot so load can validate the
//! save-point boundary (scanline 261, dot 0..=2).
//!
//! Load validates everything before mutating anything: format version,
//! ROM hash, mapper tag, enum names, JSON syntax, hex, scanline/dot. A
//! failed load leaves the machine untouched.

use crate::{
    Plugin,
    ines::INesFile,
    nes::{
        NesMcu,
        mapper::{
            Action53State, AxRomState, BxRomState, CartridgeMemorySnapshot, CnRomState, J87State,
            MapperState, Mirroring, Mmc1State, Mmc3State, Mmc5AudioState, Mmc5PcmState,
            Mmc5PulseState, Mmc5State, NRomState, Nina001State, SnapshotError, UxRomState,
            Vrc24State,
        },
    },
    nes_machine::NesMachine,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Save-file layout version. Bumped on any layout change or change to a
/// field's meaning (emulation-behavior changes do not bump it). Old
/// versions are refused, never migrated: there is exactly one constant.
pub const SAVE_FORMAT_VERSION: u32 = 1;

/// Scanline the save point sits at (ADR 0022): end of the first CPU cycle
/// at or after scanline 261, so the boundary PPU dot is 0, 1, or 2.
const SAVE_SCANLINE: u16 = 261;

/// Largest boundary dot (one CPU cycle is three PPU dots).
const MAX_SAVE_DOT: u16 = 2;

/// Mapper tags in a fixed order (the `MapperState` variant names).
const KNOWN_MAPPER_TAGS: [&str; 12] = [
    "NRom", "MMC1", "UxRom", "CnRom", "MMC3", "MMC5", "AxRom", "Action53", "Nina001", "BxRom",
    "Vrc24", "J87",
];

/// ROM identity for save files (ticket #192, maintainer story 28):
/// SHA-256 over PRG bytes then CHR bytes. The existing `rom_hashes` in
/// `bk2.rs` stays private and unchanged.
pub fn rom_sha256(rom: &INesFile) -> [u8; 32] {
    use sha2::Digest as _;
    let mut hasher = sha2::Sha256::new();
    hasher.update(rom.read_prg_rom());
    hasher.update(rom.read_chr_rom());
    hasher.finalize().into()
}

/// Save/load failure. `Display` texts are exactly the spec (#184) texts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SaveError {
    /// Stored version is not this build's version.
    VersionMismatch { found: u32 },
    /// Stored ROM hash differs from the loaded ROM's hash.
    RomMismatch { expected: String, actual: String },
    /// Stored mapper tag differs from the ROM-rebuilt mapper's tag.
    MapperMismatch { saved: String, actual: String },
    /// Stored enum name is not a known variant.
    UnknownEnum { name: String, variant: String },
    /// JSON syntax error or any other structural/type problem.
    Corrupt(String),
    /// A hex field is not valid hex.
    BadHex { path: String },
    /// Mapper snapshot capture/restore failure (save path only: every
    /// load-path mapper failure is pre-empted by the tag comparison).
    Mapper(SnapshotError),
}

impl core::fmt::Display for SaveError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            SaveError::VersionMismatch { found } => write!(
                f,
                "save state format version {found} is not supported; this build reads version {SAVE_FORMAT_VERSION}"
            ),
            SaveError::RomMismatch { expected, actual } => write!(
                f,
                "save state is for a different ROM: expected sha256 {expected}, ROM is {actual}"
            ),
            SaveError::MapperMismatch { saved, actual } => write!(
                f,
                "save state mapper {saved} does not match ROM mapper {actual}"
            ),
            SaveError::UnknownEnum { name, variant } => {
                write!(f, "save state has unknown {name} variant \"{variant}\"")
            }
            SaveError::Corrupt(message) => write!(f, "save state is corrupt: {message}"),
            SaveError::BadHex { path } => {
                write!(f, "save state is corrupt: field {path} is not valid hex")
            }
            SaveError::Mapper(error) => write!(f, "{error}"),
        }
    }
}

impl std::error::Error for SaveError {}

// ---------------------------------------------------------------------------
// Hex helpers
// ---------------------------------------------------------------------------

fn hex_encode(bytes: &[u8]) -> String {
    const DIGITS: &[u8; 16] = b"0123456789abcdef";
    let mut out = String::with_capacity(bytes.len() * 2);
    for &byte in bytes {
        out.push(DIGITS[(byte >> 4) as usize] as char);
        out.push(DIGITS[(byte & 0x0f) as usize] as char);
    }
    out
}

fn hex_decode(path: &str, text: &str) -> Result<Vec<u8>, SaveError> {
    if text.len() % 2 != 0 || !text.bytes().all(|b| b.is_ascii_hexdigit()) {
        return Err(SaveError::BadHex {
            path: path.to_string(),
        });
    }
    let mut out = Vec::with_capacity(text.len() / 2);
    for i in (0..text.len()).step_by(2) {
        match u8::from_str_radix(&text[i..i + 2], 16) {
            Ok(byte) => out.push(byte),
            Err(_) => {
                return Err(SaveError::BadHex {
                    path: path.to_string(),
                });
            }
        }
    }
    Ok(out)
}

fn hex_decode_fixed<const N: usize>(path: &str, text: &str) -> Result<[u8; N], SaveError> {
    let bytes = hex_decode(path, text)?;
    bytes.try_into().map_err(|_| {
        SaveError::Corrupt(format!("field {path} has wrong length, expected {N} bytes"))
    })
}

fn fixed_hex_array<const N: usize, const M: usize>(
    section: &str,
    field: &str,
    texts: &[String; M],
) -> Result<[[u8; N]; M], SaveError> {
    let mut out = [[0u8; N]; M];
    for (slot, text) in texts.iter().enumerate() {
        out[slot] = hex_decode_fixed(&format!("{section}.{field}[{slot}]"), text)?;
    }
    Ok(out)
}

// ---------------------------------------------------------------------------
// DTO structs (the only serde derives in the save path; core types stay
// derive-free per ADR-0020).
// ---------------------------------------------------------------------------

/// Placeholder for component sections ticket #193 fills from live state.
/// Serializes as `{}`.
macro_rules! todo_section {
    ($name:ident, $doc:expr) => {
        #[doc = $doc]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
        struct $name {}
    };
}

todo_section!(
    CpuSection,
    "Ticket #193 fills this from live CPU state (registers, latches, microcode queue, interrupt detectors)."
);
todo_section!(
    BusSection,
    "Ticket #193 fills this from live bus state (system RAM, OAM DMA pending, open bus, deferred APU writes)."
);
todo_section!(
    DmaSection,
    "Ticket #193 fills this from live DMA state (DMC DMA, in-flight OAM DMA)."
);
todo_section!(
    ApuSection,
    "Ticket #193 fills this from live APU state (channels, frame sequencer, mixer, expansion mix, race flags)."
);
todo_section!(
    ControllerSection,
    "Ticket #193 fills this from live controller state (strobe, shift positions, Zapper trigger cycles)."
);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct PpuSection {
    scanline: u16,
    dot: u16,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct SaveDto {
    format_version: u32,
    rom_sha256: String,
    frame: u64,
    clock: u64,
    cpu: CpuSection,
    bus: BusSection,
    dma: DmaSection,
    ppu: PpuSection,
    apu: ApuSection,
    controller: ControllerSection,
    cartridge: CartridgeSectionDto,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct CartridgeSectionDto {
    mapper: String,
    memory: MemorySectionDto,
    state: Value,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct MemorySectionDto {
    nametables: [String; 4],
    chr_ram: Option<String>,
    prg_ram: [String; 8],
    wram_ext: String,
    prg_ram_enabled: bool,
    mirroring: String,
    wram_6000_base: u64,
    ppu_window: [u16; 16],
    prg_window: [u16; 32],
}

// Per-mapper payload DTOs. Field names mirror the core state structs;
// every `[u8; N]` is one lowercase hex string.

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct NRomDto {}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct Mmc1Dto {
    control: u8,
    chr_bank0: u8,
    chr_bank1: u8,
    prg_bank: u8,
    shift_register: u8,
    last_write_cycle: Option<u64>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct UxRomDto {
    selected_prg_bank: u16,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct CnRomDto {}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct Mmc3Dto {
    bank_select: u8,
    chr_bank_registers: String,
    reg_6: u8,
    reg_7: u8,
    prg_ram_enabled: bool,
    prg_ram_write_protect: bool,
    irq_latch: u8,
    irq_counter: u8,
    irq_reload: bool,
    irq_enabled: bool,
    irq_pending: bool,
    prev_a12: bool,
    a12_low_ticks: u8,
    a12_transition_this_scanline: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct Mmc5Dto {
    prg_mode: u8,
    prg_banks: String,
    protect1: u8,
    protect2: u8,
    audio: Mmc5AudioDto,
    chr_mode: u8,
    chr_spr: [u16; 8],
    chr_bg: [u16; 4],
    chr_high: u8,
    last_chr_set_is_bg: bool,
    exram_mode: u8,
    nt_select: u8,
    fill_tile: u8,
    fill_attr: u8,
    exram: String,
    rendering: bool,
    tile_latch: u16,
    irq_target: u8,
    irq_enabled: bool,
    irq_pending: bool,
    irq_in_frame: bool,
    irq_counter: u8,
    last_tick_scanline: u16,
    dots_in_line: u16,
    render_seen: bool,
    split_fetch: u8,
    split_active: bool,
    split_mode: u8,
    split_scroll: u8,
    split_bank: u8,
    mult_a: u8,
    mult_b: u8,
    mult_result: u16,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct Mmc5AudioDto {
    pulse1: Mmc5PulseDto,
    pulse2: Mmc5PulseDto,
    pcm: Mmc5PcmDto,
    frame_acc: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct Mmc5PulseDto {
    timer_period: u16,
    timer_counter: u16,
    divider_period: u8,
    divider_counter: u8,
    length_counter: u8,
    length_halt: bool,
    length_enabled: bool,
    envelope_period: u8,
    envelope_divider_counter: u8,
    envelope_counter: u8,
    envelope_loop: bool,
    envelope_disabled: bool,
    envelope_reset_pending: bool,
    duty: u8,
    sequencer_position: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct Mmc5PcmDto {
    read_mode: bool,
    irq_enabled: bool,
    irq_trip: bool,
    dac: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct AxRomDto {
    selected_prg_bank: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct Action53Dto {
    regs: String,
    selected: u64,
    mirroring_bit: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct Nina001Dto {
    selected_prg_bank: u8,
    chr_bank0: u8,
    chr_bank1: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct BxRomDto {
    selected_prg_bank: u16,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct Vrc24Dto {
    prg_ram_enabled: bool,
    prg_select_0: u8,
    prg_select_1: u8,
    prg_swap_mode: bool,
    irq_latch: u8,
    irq_counter: u8,
    irq_prescaler: u16,
    irq_mode: bool,
    irq_enable: bool,
    irq_enable_after_ack: bool,
    irq_pending: bool,
    microwire_latch: u8,
    chr_nibbles: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct J87Dto {}

// ---------------------------------------------------------------------------
// Core <-> DTO conversions
// ---------------------------------------------------------------------------

fn encode_mirroring(mirroring: Mirroring) -> &'static str {
    match mirroring {
        Mirroring::LowerBank => "LowerBank",
        Mirroring::UpperBank => "UpperBank",
        Mirroring::Horizontal => "Horizontal",
        Mirroring::Vertical => "Vertical",
        Mirroring::Four => "Four",
    }
}

fn decode_mirroring(name: &str) -> Result<Mirroring, SaveError> {
    match name {
        "LowerBank" => Ok(Mirroring::LowerBank),
        "UpperBank" => Ok(Mirroring::UpperBank),
        "Horizontal" => Ok(Mirroring::Horizontal),
        "Vertical" => Ok(Mirroring::Vertical),
        "Four" => Ok(Mirroring::Four),
        _ => Err(SaveError::UnknownEnum {
            name: "Mirroring".to_string(),
            variant: name.to_string(),
        }),
    }
}

fn encode_pulse(state: &Mmc5PulseState) -> Mmc5PulseDto {
    Mmc5PulseDto {
        timer_period: state.timer_period,
        timer_counter: state.timer_counter,
        divider_period: state.divider_period,
        divider_counter: state.divider_counter,
        length_counter: state.length_counter,
        length_halt: state.length_halt,
        length_enabled: state.length_enabled,
        envelope_period: state.envelope_period,
        envelope_divider_counter: state.envelope_divider_counter,
        envelope_counter: state.envelope_counter,
        envelope_loop: state.envelope_loop,
        envelope_disabled: state.envelope_disabled,
        envelope_reset_pending: state.envelope_reset_pending,
        duty: state.duty,
        sequencer_position: state.sequencer_position,
    }
}

fn decode_pulse(dto: &Mmc5PulseDto) -> Mmc5PulseState {
    Mmc5PulseState {
        timer_period: dto.timer_period,
        timer_counter: dto.timer_counter,
        divider_period: dto.divider_period,
        divider_counter: dto.divider_counter,
        length_counter: dto.length_counter,
        length_halt: dto.length_halt,
        length_enabled: dto.length_enabled,
        envelope_period: dto.envelope_period,
        envelope_divider_counter: dto.envelope_divider_counter,
        envelope_counter: dto.envelope_counter,
        envelope_loop: dto.envelope_loop,
        envelope_disabled: dto.envelope_disabled,
        envelope_reset_pending: dto.envelope_reset_pending,
        duty: dto.duty,
        sequencer_position: dto.sequencer_position,
    }
}

fn encode_audio(state: &Mmc5AudioState) -> Mmc5AudioDto {
    Mmc5AudioDto {
        pulse1: encode_pulse(&state.pulse1),
        pulse2: encode_pulse(&state.pulse2),
        pcm: Mmc5PcmDto {
            read_mode: state.pcm.read_mode,
            irq_enabled: state.pcm.irq_enabled,
            irq_trip: state.pcm.irq_trip,
            dac: state.pcm.dac,
        },
        frame_acc: state.frame_acc,
    }
}

fn decode_audio(dto: &Mmc5AudioDto) -> Mmc5AudioState {
    Mmc5AudioState {
        pulse1: decode_pulse(&dto.pulse1),
        pulse2: decode_pulse(&dto.pulse2),
        pcm: Mmc5PcmState {
            read_mode: dto.pcm.read_mode,
            irq_enabled: dto.pcm.irq_enabled,
            irq_trip: dto.pcm.irq_trip,
            dac: dto.pcm.dac,
        },
        frame_acc: dto.frame_acc,
    }
}

/// Serializes one DTO struct into a [`Value`]. Infallible: DTO structs
/// only hold numbers, bools, strings, arrays, and null.
fn to_payload<T: Serialize>(dto: &T) -> Value {
    serde_json::to_value(dto).expect("DTO serialization is infallible")
}

/// Reads one DTO struct out of a payload [`Value`]. Structural problems
/// (wrong types, missing fields) are corrupt-file errors.
fn from_payload<T: for<'de> Deserialize<'de>>(tag: &str, payload: Value) -> Result<T, SaveError> {
    serde_json::from_value(payload)
        .map_err(|error| SaveError::Corrupt(format!("cartridge {tag} state: {error}")))
}

/// Encodes a mapper snapshot: returns the saved tag plus the payload.
/// The tag is stored in `cartridge.mapper`; the payload in
/// `cartridge.state` (no wrapper key, so the tag is stored exactly once).
fn encode_mapper(state: &MapperState) -> (String, Value) {
    let tag = state.tag();
    let payload = match state {
        MapperState::NRom(_) => to_payload(&NRomDto {}),
        MapperState::MMC1(saved) => to_payload(&Mmc1Dto {
            control: saved.control,
            chr_bank0: saved.chr_bank0,
            chr_bank1: saved.chr_bank1,
            prg_bank: saved.prg_bank,
            shift_register: saved.shift_register,
            last_write_cycle: saved.last_write_cycle,
        }),
        MapperState::UxRom(saved) => to_payload(&UxRomDto {
            selected_prg_bank: saved.selected_prg_bank,
        }),
        MapperState::CnRom(_) => to_payload(&CnRomDto {}),
        MapperState::MMC3(saved) => to_payload(&Mmc3Dto {
            bank_select: saved.bank_select,
            chr_bank_registers: hex_encode(&saved.chr_bank_registers),
            reg_6: saved.reg_6,
            reg_7: saved.reg_7,
            prg_ram_enabled: saved.prg_ram_enabled,
            prg_ram_write_protect: saved.prg_ram_write_protect,
            irq_latch: saved.irq_latch,
            irq_counter: saved.irq_counter,
            irq_reload: saved.irq_reload,
            irq_enabled: saved.irq_enabled,
            irq_pending: saved.irq_pending,
            prev_a12: saved.prev_a12,
            a12_low_ticks: saved.a12_low_ticks,
            a12_transition_this_scanline: saved.a12_transition_this_scanline,
        }),
        MapperState::MMC5(saved) => to_payload(&Mmc5Dto {
            prg_mode: saved.prg_mode,
            prg_banks: hex_encode(&saved.prg_banks),
            protect1: saved.protect1,
            protect2: saved.protect2,
            audio: encode_audio(&saved.audio),
            chr_mode: saved.chr_mode,
            chr_spr: saved.chr_spr,
            chr_bg: saved.chr_bg,
            chr_high: saved.chr_high,
            last_chr_set_is_bg: saved.last_chr_set_is_bg,
            exram_mode: saved.exram_mode,
            nt_select: saved.nt_select,
            fill_tile: saved.fill_tile,
            fill_attr: saved.fill_attr,
            exram: hex_encode(&saved.exram),
            rendering: saved.rendering,
            tile_latch: saved.tile_latch,
            irq_target: saved.irq_target,
            irq_enabled: saved.irq_enabled,
            irq_pending: saved.irq_pending,
            irq_in_frame: saved.irq_in_frame,
            irq_counter: saved.irq_counter,
            last_tick_scanline: saved.last_tick_scanline,
            dots_in_line: saved.dots_in_line,
            render_seen: saved.render_seen,
            split_fetch: saved.split_fetch,
            split_active: saved.split_active,
            split_mode: saved.split_mode,
            split_scroll: saved.split_scroll,
            split_bank: saved.split_bank,
            mult_a: saved.mult_a,
            mult_b: saved.mult_b,
            mult_result: saved.mult_result,
        }),
        MapperState::AxRom(saved) => to_payload(&AxRomDto {
            selected_prg_bank: saved.selected_prg_bank,
        }),
        MapperState::Action53(saved) => to_payload(&Action53Dto {
            regs: hex_encode(&saved.regs),
            selected: saved.selected as u64,
            mirroring_bit: saved.mirroring_bit,
        }),
        MapperState::Nina001(saved) => to_payload(&Nina001Dto {
            selected_prg_bank: saved.selected_prg_bank,
            chr_bank0: saved.chr_bank0,
            chr_bank1: saved.chr_bank1,
        }),
        MapperState::BxRom(saved) => to_payload(&BxRomDto {
            selected_prg_bank: saved.selected_prg_bank,
        }),
        MapperState::Vrc24(saved) => to_payload(&Vrc24Dto {
            prg_ram_enabled: saved.prg_ram_enabled,
            prg_select_0: saved.prg_select_0,
            prg_select_1: saved.prg_select_1,
            prg_swap_mode: saved.prg_swap_mode,
            irq_latch: saved.irq_latch,
            irq_counter: saved.irq_counter,
            irq_prescaler: saved.irq_prescaler,
            irq_mode: saved.irq_mode,
            irq_enable: saved.irq_enable,
            irq_enable_after_ack: saved.irq_enable_after_ack,
            irq_pending: saved.irq_pending,
            microwire_latch: saved.microwire_latch,
            chr_nibbles: hex_encode(&saved.chr_nibbles),
        }),
        MapperState::J87(_) => to_payload(&J87Dto {}),
    };
    (tag.to_string(), payload)
}

/// Decodes a mapper payload. The tag was already checked against
/// [`KNOWN_MAPPER_TAGS`] by the caller, so anything else here is
/// unreachable; unknown tags are refused before this runs.
fn decode_mapper(tag: &str, payload: Value) -> Result<MapperState, SaveError> {
    match tag {
        "NRom" => {
            let _dto: NRomDto = from_payload(tag, payload)?;
            Ok(MapperState::NRom(NRomState))
        }
        "MMC1" => {
            let dto: Mmc1Dto = from_payload(tag, payload)?;
            Ok(MapperState::MMC1(Mmc1State {
                control: dto.control,
                chr_bank0: dto.chr_bank0,
                chr_bank1: dto.chr_bank1,
                prg_bank: dto.prg_bank,
                shift_register: dto.shift_register,
                last_write_cycle: dto.last_write_cycle,
            }))
        }
        "UxRom" => {
            let dto: UxRomDto = from_payload(tag, payload)?;
            Ok(MapperState::UxRom(UxRomState {
                selected_prg_bank: dto.selected_prg_bank,
            }))
        }
        "CnRom" => {
            let _dto: CnRomDto = from_payload(tag, payload)?;
            Ok(MapperState::CnRom(CnRomState))
        }
        "MMC3" => {
            let dto: Mmc3Dto = from_payload(tag, payload)?;
            Ok(MapperState::MMC3(Mmc3State {
                bank_select: dto.bank_select,
                chr_bank_registers: hex_decode_fixed(
                    "cartridge.state.chr_bank_registers",
                    &dto.chr_bank_registers,
                )?,
                reg_6: dto.reg_6,
                reg_7: dto.reg_7,
                prg_ram_enabled: dto.prg_ram_enabled,
                prg_ram_write_protect: dto.prg_ram_write_protect,
                irq_latch: dto.irq_latch,
                irq_counter: dto.irq_counter,
                irq_reload: dto.irq_reload,
                irq_enabled: dto.irq_enabled,
                irq_pending: dto.irq_pending,
                prev_a12: dto.prev_a12,
                a12_low_ticks: dto.a12_low_ticks,
                a12_transition_this_scanline: dto.a12_transition_this_scanline,
            }))
        }
        "MMC5" => {
            let dto: Mmc5Dto = from_payload(tag, payload)?;
            Ok(MapperState::MMC5(Mmc5State {
                prg_mode: dto.prg_mode,
                prg_banks: hex_decode_fixed("cartridge.state.prg_banks", &dto.prg_banks)?,
                protect1: dto.protect1,
                protect2: dto.protect2,
                audio: decode_audio(&dto.audio),
                chr_mode: dto.chr_mode,
                chr_spr: dto.chr_spr,
                chr_bg: dto.chr_bg,
                chr_high: dto.chr_high,
                last_chr_set_is_bg: dto.last_chr_set_is_bg,
                exram_mode: dto.exram_mode,
                nt_select: dto.nt_select,
                fill_tile: dto.fill_tile,
                fill_attr: dto.fill_attr,
                exram: hex_decode_fixed("cartridge.state.exram", &dto.exram)?,
                rendering: dto.rendering,
                tile_latch: dto.tile_latch,
                irq_target: dto.irq_target,
                irq_enabled: dto.irq_enabled,
                irq_pending: dto.irq_pending,
                irq_in_frame: dto.irq_in_frame,
                irq_counter: dto.irq_counter,
                last_tick_scanline: dto.last_tick_scanline,
                dots_in_line: dto.dots_in_line,
                render_seen: dto.render_seen,
                split_fetch: dto.split_fetch,
                split_active: dto.split_active,
                split_mode: dto.split_mode,
                split_scroll: dto.split_scroll,
                split_bank: dto.split_bank,
                mult_a: dto.mult_a,
                mult_b: dto.mult_b,
                mult_result: dto.mult_result,
            }))
        }
        "AxRom" => {
            let dto: AxRomDto = from_payload(tag, payload)?;
            Ok(MapperState::AxRom(AxRomState {
                selected_prg_bank: dto.selected_prg_bank,
            }))
        }
        "Action53" => {
            let dto: Action53Dto = from_payload(tag, payload)?;
            Ok(MapperState::Action53(Action53State {
                regs: hex_decode_fixed("cartridge.state.regs", &dto.regs)?,
                selected: dto.selected as usize,
                mirroring_bit: dto.mirroring_bit,
            }))
        }
        "Nina001" => {
            let dto: Nina001Dto = from_payload(tag, payload)?;
            Ok(MapperState::Nina001(Nina001State {
                selected_prg_bank: dto.selected_prg_bank,
                chr_bank0: dto.chr_bank0,
                chr_bank1: dto.chr_bank1,
            }))
        }
        "BxRom" => {
            let dto: BxRomDto = from_payload(tag, payload)?;
            Ok(MapperState::BxRom(BxRomState {
                selected_prg_bank: dto.selected_prg_bank,
            }))
        }
        "Vrc24" => {
            let dto: Vrc24Dto = from_payload(tag, payload)?;
            Ok(MapperState::Vrc24(Vrc24State {
                prg_ram_enabled: dto.prg_ram_enabled,
                prg_select_0: dto.prg_select_0,
                prg_select_1: dto.prg_select_1,
                prg_swap_mode: dto.prg_swap_mode,
                irq_latch: dto.irq_latch,
                irq_counter: dto.irq_counter,
                irq_prescaler: dto.irq_prescaler,
                irq_mode: dto.irq_mode,
                irq_enable: dto.irq_enable,
                irq_enable_after_ack: dto.irq_enable_after_ack,
                irq_pending: dto.irq_pending,
                microwire_latch: dto.microwire_latch,
                chr_nibbles: hex_decode_fixed("cartridge.state.chr_nibbles", &dto.chr_nibbles)?,
            }))
        }
        "J87" => {
            let _dto: J87Dto = from_payload(tag, payload)?;
            Ok(MapperState::J87(J87State))
        }
        _ => Err(SaveError::UnknownEnum {
            name: "Mapper".to_string(),
            variant: tag.to_string(),
        }),
    }
}

fn encode_memory(snapshot: &CartridgeMemorySnapshot) -> MemorySectionDto {
    MemorySectionDto {
        nametables: std::array::from_fn(|slot| hex_encode(&snapshot.nametables[slot])),
        chr_ram: snapshot.chr_ram.as_ref().map(|bytes| hex_encode(bytes)),
        prg_ram: std::array::from_fn(|slot| hex_encode(&snapshot.prg_ram[slot])),
        wram_ext: hex_encode(&snapshot.wram_ext),
        prg_ram_enabled: snapshot.prg_ram_enabled,
        mirroring: encode_mirroring(snapshot.mirroring).to_string(),
        wram_6000_base: snapshot.wram_6000_base as u64,
        ppu_window: snapshot.ppu_window,
        prg_window: snapshot.prg_window,
    }
}

fn decode_memory(dto: &MemorySectionDto) -> Result<CartridgeMemorySnapshot, SaveError> {
    let nametables = fixed_hex_array("cartridge.memory", "nametables", &dto.nametables)?;
    let chr_ram = dto
        .chr_ram
        .as_ref()
        .map(|text| hex_decode("cartridge.memory.chr_ram", text))
        .transpose()?;
    let prg_ram = fixed_hex_array("cartridge.memory", "prg_ram", &dto.prg_ram)?;
    let wram_ext = hex_decode("cartridge.memory.wram_ext", &dto.wram_ext)?;
    Ok(CartridgeMemorySnapshot {
        nametables,
        chr_ram,
        prg_ram,
        wram_ext,
        prg_ram_enabled: dto.prg_ram_enabled,
        mirroring: decode_mirroring(&dto.mirroring)?,
        wram_6000_base: dto.wram_6000_base as usize,
        ppu_window: dto.ppu_window,
        prg_window: dto.prg_window,
    })
}

// ---------------------------------------------------------------------------
// Public snapshot API: save at the boundary, restore into a machine built
// from the ROM.
// ---------------------------------------------------------------------------

/// Serializes `machine` state to the versioned JSON save format. The
/// machine must sit at the save-point boundary (scanline 261, dot 0..=2),
/// which is where [`NesMachine::process_frame`] returns.
pub fn save_state_json<P>(machine: &NesMachine<P>) -> Result<String, SaveError>
where
    P: Plugin<NesMcu>,
{
    let (mapper_state, memory_snapshot) = machine
        .mcu()
        .cartridge()
        .capture_snapshot()
        .map_err(SaveError::Mapper)?;
    let timing = machine.mcu().ppu().timing();
    let (mapper_tag, mapper_payload) = encode_mapper(&mapper_state);
    let dto = SaveDto {
        format_version: SAVE_FORMAT_VERSION,
        rom_sha256: hex_encode(&rom_sha256(machine.rom())),
        frame: machine.frame_no() as u64,
        clock: machine.clock_cycles(),
        cpu: CpuSection {},
        bus: BusSection {},
        dma: DmaSection {},
        ppu: PpuSection {
            scanline: timing.scanline(),
            dot: timing.dot(),
        },
        apu: ApuSection {},
        controller: ControllerSection {},
        cartridge: CartridgeSectionDto {
            mapper: mapper_tag,
            memory: encode_memory(&memory_snapshot),
            state: mapper_payload,
        },
    };
    serde_json::to_string_pretty(&dto).map_err(|error| SaveError::Corrupt(error.to_string()))
}

/// Restores a save produced by [`save_state_json`] into `machine`
/// (built from the same ROM). Every check runs before any mutation, so
/// a failed load leaves the machine byte-identical to before the call.
pub fn load_state_json<P>(machine: &mut NesMachine<P>, text: &str) -> Result<(), SaveError>
where
    P: Plugin<NesMcu>,
{
    let raw: Value =
        serde_json::from_str(text).map_err(|error| SaveError::Corrupt(error.to_string()))?;
    let dto: SaveDto =
        serde_json::from_value(raw).map_err(|error| SaveError::Corrupt(error.to_string()))?;

    if dto.format_version != SAVE_FORMAT_VERSION {
        return Err(SaveError::VersionMismatch {
            found: dto.format_version,
        });
    }

    let stored_hash = hex_decode("rom_sha256", &dto.rom_sha256)?;
    if stored_hash.len() != 32 {
        return Err(SaveError::Corrupt(
            "field rom_sha256 has wrong length, expected 32 bytes".to_string(),
        ));
    }
    let machine_hash = rom_sha256(machine.rom());
    if stored_hash[..] != machine_hash[..] {
        return Err(SaveError::RomMismatch {
            expected: dto.rom_sha256.clone(),
            actual: hex_encode(&machine_hash),
        });
    }

    if !KNOWN_MAPPER_TAGS.contains(&dto.cartridge.mapper.as_str()) {
        return Err(SaveError::UnknownEnum {
            name: "Mapper".to_string(),
            variant: dto.cartridge.mapper.clone(),
        });
    }
    let machine_tag = machine
        .mcu()
        .cartridge()
        .mapper_tag()
        .map_err(SaveError::Mapper)?;
    if dto.cartridge.mapper != machine_tag {
        return Err(SaveError::MapperMismatch {
            saved: dto.cartridge.mapper.clone(),
            actual: machine_tag.to_string(),
        });
    }

    // Decode every payload before touching the machine. The tag check
    // above guarantees the mapper restore's internal check passes, and
    // each restore rejects a foreign variant before mutating.
    let mapper_state = decode_mapper(&dto.cartridge.mapper, dto.cartridge.state)?;
    let memory_snapshot = decode_memory(&dto.cartridge.memory)?;

    if dto.ppu.scanline != SAVE_SCANLINE {
        return Err(SaveError::Corrupt(format!(
            "stored scanline {} is not the save-point scanline {SAVE_SCANLINE}",
            dto.ppu.scanline
        )));
    }
    if dto.ppu.dot > MAX_SAVE_DOT {
        return Err(SaveError::Corrupt(format!(
            "stored dot {} is outside 0..={MAX_SAVE_DOT}",
            dto.ppu.dot
        )));
    }

    machine
        .mcu_mut()
        .cartridge_mut()
        .restore_snapshot(&mapper_state, &memory_snapshot)
        .map_err(SaveError::Mapper)?;
    machine.mcu_mut().ppu_mut().timing_mut().set_save_point(
        dto.ppu.scanline,
        dto.ppu.dot,
        dto.frame as usize,
    );
    machine.set_clock_cycles(dto.clock);
    machine.mcu_mut().ppu_mut().set_cycle(dto.clock);
    Ok(())
}

// ---------------------------------------------------------------------------
// Tests (ticket #192: round-trip stability + every refusal path; no
// continuation-fidelity claim — that is ticket #193).
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::EmptyPlugin;

    /// Minimal valid iNES file: `prg_pages` x 16 KiB PRG, `chr_pages` x
    /// 8 KiB CHR, mapper `mapper_no`. PRG starts with a `JMP $8000`
    /// infinite loop (same body as the `nes_machine` tests).
    fn test_rom(mapper_no: u8, prg_pages: u8, chr_pages: u8, prg_tail: u8) -> INesFile {
        let mut rom = Vec::new();
        rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a]);
        rom.push(prg_pages);
        rom.push(chr_pages);
        rom.push(mapper_no << 4);
        rom.push(0x00);
        rom.extend_from_slice(&[0; 8]);
        let prg_len = usize::from(prg_pages) * 16 * 1024;
        rom.extend_from_slice(&[0x4C, 0x00, 0x80]);
        rom.extend(std::iter::repeat_n(0, prg_len - 3));
        *rom.last_mut().unwrap() = prg_tail;
        rom.extend(std::iter::repeat_n(0, usize::from(chr_pages) * 8 * 1024));
        INesFile::new(rom).unwrap()
    }

    fn nrom_file() -> INesFile {
        test_rom(0, 1, 1, 0x00)
    }

    fn nrom_file_alt() -> INesFile {
        test_rom(0, 1, 1, 0xFF)
    }

    fn mmc1_file() -> INesFile {
        test_rom(1, 1, 1, 0x00)
    }

    fn machine_for(file: &INesFile) -> NesMachine<EmptyPlugin<NesMcu>> {
        NesMachine::new(file, EmptyPlugin::new())
    }

    /// Machine fingerprint: the save JSON itself. A failed load must
    /// leave it unchanged.
    fn fingerprint<P>(machine: &NesMachine<P>) -> String
    where
        P: Plugin<NesMcu>,
    {
        save_state_json(machine).expect("fingerprint save must succeed")
    }

    fn tamper(save: &str, path: &[&str], value: Value) -> String {
        let mut raw: Value = serde_json::from_str(save).unwrap();
        let mut slot = &mut raw;
        for key in &path[..path.len() - 1] {
            slot = &mut slot[key];
        }
        slot[path[path.len() - 1]] = value;
        serde_json::to_string(&raw).unwrap()
    }

    #[test]
    fn save_load_save_is_byte_identical_and_restores_frame_and_clock() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        for _ in 0..2 {
            machine.process_frame();
        }
        assert_eq!(machine.mcu().ppu().timing().scanline(), SAVE_SCANLINE);
        assert!(machine.mcu().ppu().timing().dot() <= MAX_SAVE_DOT);
        let frame_before = machine.frame_no();
        let clock_before = machine.clock_cycles();

        let first = save_state_json(&machine).unwrap();

        let mut loaded = machine_for(&file);
        assert_ne!(loaded.frame_no(), frame_before);
        load_state_json(&mut loaded, &first).unwrap();
        assert_eq!(loaded.frame_no(), frame_before);
        assert_eq!(loaded.clock_cycles(), clock_before);
        assert_eq!(loaded.mcu().ppu().timing().scanline(), SAVE_SCANLINE);
        assert_eq!(loaded.mcu().ppu().cycle(), clock_before);

        let second = save_state_json(&loaded).unwrap();
        assert_eq!(first, second, "re-save must be byte-identical");
    }

    #[test]
    fn format_version_is_a_single_shared_constant() {
        assert_eq!(SAVE_FORMAT_VERSION, 1);
        let file = nrom_file();
        let machine = machine_for(&file);
        let save = save_state_json(&machine).unwrap();
        assert!(
            save.contains(&format!("\"format_version\": {SAVE_FORMAT_VERSION}")),
            "save must stamp the single constant"
        );
    }

    #[test]
    fn rom_mismatch_reports_both_hashes_and_leaves_machine_untouched() {
        let file = nrom_file();
        let other = nrom_file_alt();
        let mut machine = machine_for(&file);
        machine.process_frame();
        let save = save_state_json(&machine).unwrap();

        let mut other_machine = machine_for(&other);
        other_machine.process_frame();
        let before = fingerprint(&other_machine);
        let frame_before = other_machine.frame_no();

        let error = load_state_json(&mut other_machine, &save).unwrap_err();
        let expected = hex_encode(&rom_sha256(&file));
        let actual = hex_encode(&rom_sha256(&other));
        assert_eq!(
            error.to_string(),
            format!(
                "save state is for a different ROM: expected sha256 {expected}, ROM is {actual}"
            )
        );
        assert_eq!(fingerprint(&other_machine), before);
        assert_eq!(other_machine.frame_no(), frame_before);
    }

    #[test]
    fn version_mismatch_is_refused_and_leaves_machine_untouched() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        machine.process_frame();
        let save = save_state_json(&machine).unwrap();
        let tampered = tamper(&save, &["format_version"], Value::from(7));

        let before = fingerprint(&machine);
        let error = load_state_json(&mut machine, &tampered).unwrap_err();
        assert_eq!(
            error.to_string(),
            "save state format version 7 is not supported; this build reads version 1"
        );
        assert_eq!(fingerprint(&machine), before);
    }

    #[test]
    fn mapper_mismatch_is_refused_and_leaves_machine_untouched() {
        let nrom = nrom_file();
        let mmc1 = mmc1_file();
        let mut nrom_machine = machine_for(&nrom);
        nrom_machine.process_frame();
        let save = save_state_json(&nrom_machine).unwrap();

        let mut mmc1_machine = machine_for(&mmc1);
        let before = fingerprint(&mmc1_machine);
        let error = load_state_json(&mut mmc1_machine, &save).unwrap_err();
        assert_eq!(
            error.to_string(),
            "save state mapper NRom does not match ROM mapper MMC1"
        );
        assert_eq!(fingerprint(&mmc1_machine), before);
    }

    #[test]
    fn unknown_mapper_tag_is_refused_as_unknown_enum() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        machine.process_frame();
        let save = save_state_json(&machine).unwrap();
        let tampered = tamper(
            &save,
            &["cartridge", "mapper"],
            Value::String("Bogus".to_string()),
        );

        let before = fingerprint(&machine);
        let error = load_state_json(&mut machine, &tampered).unwrap_err();
        assert_eq!(
            error.to_string(),
            "save state has unknown Mapper variant \"Bogus\""
        );
        assert_eq!(fingerprint(&machine), before);
    }

    #[test]
    fn unknown_mirroring_is_refused_as_unknown_enum() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        machine.process_frame();
        let save = save_state_json(&machine).unwrap();
        let tampered = tamper(
            &save,
            &["cartridge", "memory", "mirroring"],
            Value::String("Diagonal".to_string()),
        );

        let before = fingerprint(&machine);
        let error = load_state_json(&mut machine, &tampered).unwrap_err();
        assert_eq!(
            error.to_string(),
            "save state has unknown Mirroring variant \"Diagonal\""
        );
        assert_eq!(fingerprint(&machine), before);
    }

    #[test]
    fn corrupt_json_reports_the_serde_error_and_leaves_machine_untouched() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        machine.process_frame();
        let save = save_state_json(&machine).unwrap();
        let truncated = save[..save.len() / 2].to_string();

        let before = fingerprint(&machine);
        let error = load_state_json(&mut machine, &truncated).unwrap_err();
        let serde_error =
            serde_json::from_str::<Value>(&truncated).expect_err("truncation must not parse");
        assert_eq!(
            error.to_string(),
            format!("save state is corrupt: {serde_error}")
        );
        assert_eq!(fingerprint(&machine), before);
    }

    #[test]
    fn bad_hex_rom_hash_is_refused() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        machine.process_frame();
        let save = save_state_json(&machine).unwrap();
        let tampered = tamper(&save, &["rom_sha256"], Value::String("zz".repeat(32)));

        let before = fingerprint(&machine);
        let error = load_state_json(&mut machine, &tampered).unwrap_err();
        assert_eq!(
            error.to_string(),
            "save state is corrupt: field rom_sha256 is not valid hex"
        );
        assert_eq!(fingerprint(&machine), before);
    }

    #[test]
    fn bad_hex_memory_bytes_are_refused_with_field_path() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        machine.process_frame();
        let save = save_state_json(&machine).unwrap();
        let mut raw: Value = serde_json::from_str(&save).unwrap();
        raw["cartridge"]["memory"]["nametables"][0] = Value::String("zz".to_string());
        let tampered = serde_json::to_string(&raw).unwrap();

        let before = fingerprint(&machine);
        let error = load_state_json(&mut machine, &tampered).unwrap_err();
        assert_eq!(
            error.to_string(),
            "save state is corrupt: field cartridge.memory.nametables[0] is not valid hex"
        );
        assert_eq!(fingerprint(&machine), before);
    }

    #[test]
    fn stored_scanline_off_the_save_point_is_refused() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        machine.process_frame();
        let save = save_state_json(&machine).unwrap();
        let tampered = tamper(&save, &["ppu", "scanline"], Value::from(240));

        let before = fingerprint(&machine);
        let error = load_state_json(&mut machine, &tampered).unwrap_err();
        assert_eq!(
            error.to_string(),
            "save state is corrupt: stored scanline 240 is not the save-point scanline 261"
        );
        assert_eq!(fingerprint(&machine), before);
    }

    #[test]
    fn stored_dot_outside_zero_to_two_is_refused() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        machine.process_frame();
        let save = save_state_json(&machine).unwrap();
        let tampered = tamper(&save, &["ppu", "dot"], Value::from(5));

        let before = fingerprint(&machine);
        let error = load_state_json(&mut machine, &tampered).unwrap_err();
        assert_eq!(
            error.to_string(),
            "save state is corrupt: stored dot 5 is outside 0..=2"
        );
        assert_eq!(fingerprint(&machine), before);
    }

    #[test]
    fn cartridge_mapper_registers_and_memory_bytes_round_trip() {
        let file = mmc1_file();
        let mut machine = machine_for(&file);
        let custom_mapper = MapperState::MMC1(Mmc1State {
            control: 0x1c,
            chr_bank0: 3,
            chr_bank1: 5,
            prg_bank: 2,
            shift_register: 0b10101,
            last_write_cycle: Some(12345),
        });
        let (_, mut memory_snapshot) = machine
            .mcu()
            .cartridge()
            .capture_snapshot()
            .expect("MMC1 snapshot must be supported");
        memory_snapshot.nametables[0][0] = 0xab;
        memory_snapshot.prg_ram[7][1023] = 0xcd;
        machine
            .mcu_mut()
            .cartridge_mut()
            .restore_snapshot(&custom_mapper, &memory_snapshot)
            .unwrap();

        let save = save_state_json(&machine).unwrap();
        assert!(save.contains("\"last_write_cycle\": 12345"));

        let mut loaded = machine_for(&file);
        load_state_json(&mut loaded, &save).unwrap();
        let (restored_mapper, restored_memory) = loaded
            .mcu()
            .cartridge()
            .capture_snapshot()
            .expect("MMC1 snapshot must be supported");
        assert_eq!(restored_mapper, custom_mapper);
        assert_eq!(restored_memory.nametables[0][0], 0xab);
        assert_eq!(restored_memory.prg_ram[7][1023], 0xcd);

        let resaved = save_state_json(&loaded).unwrap();
        assert_eq!(save, resaved, "re-save must be byte-identical");
    }

    #[test]
    fn mmc1_absent_write_cycle_round_trips_as_null() {
        let file = mmc1_file();
        let machine = machine_for(&file);
        let save = save_state_json(&machine).unwrap();
        assert!(save.contains("\"last_write_cycle\": null"));

        let mut loaded = machine_for(&file);
        load_state_json(&mut loaded, &save).unwrap();
        let resaved = save_state_json(&loaded).unwrap();
        assert_eq!(save, resaved, "re-save must be byte-identical");
    }

    /// Pure DTO round trip for every mapper tag: encode then decode must
    /// return the identical snapshot. This covers the whole cartridge
    /// section for all 12 mapper types without needing 12 ROMs.
    #[test]
    fn every_mapper_tag_encodes_and_decodes_identically() {
        let states = vec![
            MapperState::NRom(NRomState),
            MapperState::MMC1(Mmc1State {
                control: 0x1c,
                chr_bank0: 3,
                chr_bank1: 5,
                prg_bank: 2,
                shift_register: 0b10101,
                last_write_cycle: Some(12345),
            }),
            MapperState::MMC1(Mmc1State {
                control: 0,
                chr_bank0: 0,
                chr_bank1: 0,
                prg_bank: 0,
                shift_register: 0,
                last_write_cycle: None,
            }),
            MapperState::UxRom(UxRomState {
                selected_prg_bank: 300,
            }),
            MapperState::CnRom(CnRomState),
            MapperState::MMC3(Mmc3State {
                bank_select: 0x46,
                chr_bank_registers: [1, 2, 3, 4, 5, 6, 7, 8],
                reg_6: 0xaa,
                reg_7: 0x55,
                prg_ram_enabled: true,
                prg_ram_write_protect: true,
                irq_latch: 9,
                irq_counter: 10,
                irq_reload: true,
                irq_enabled: true,
                irq_pending: true,
                prev_a12: true,
                a12_low_ticks: 11,
                a12_transition_this_scanline: true,
            }),
            MapperState::MMC5(Mmc5State {
                prg_mode: 3,
                prg_banks: [1, 2, 3, 4, 5],
                protect1: 0x51,
                protect2: 0x52,
                audio: Mmc5AudioState {
                    pulse1: Mmc5PulseState {
                        timer_period: 100,
                        timer_counter: 50,
                        divider_period: 7,
                        divider_counter: 3,
                        length_counter: 20,
                        length_halt: true,
                        length_enabled: true,
                        envelope_period: 5,
                        envelope_divider_counter: 2,
                        envelope_counter: 9,
                        envelope_loop: true,
                        envelope_disabled: false,
                        envelope_reset_pending: true,
                        duty: 2,
                        sequencer_position: 4,
                    },
                    pulse2: Mmc5PulseState::default(),
                    pcm: Mmc5PcmState {
                        read_mode: true,
                        irq_enabled: true,
                        irq_trip: true,
                        dac: 0x3f,
                    },
                    frame_acc: 777,
                },
                chr_mode: 1,
                chr_spr: [10, 20, 30, 40, 50, 60, 70, 80],
                chr_bg: [11, 22, 33, 44],
                chr_high: 2,
                last_chr_set_is_bg: true,
                exram_mode: 1,
                nt_select: 2,
                fill_tile: 0xaa,
                fill_attr: 0x55,
                exram: [0x77; 1024],
                rendering: true,
                tile_latch: 0x1234,
                irq_target: 200,
                irq_enabled: true,
                irq_pending: true,
                irq_in_frame: true,
                irq_counter: 150,
                last_tick_scanline: 100,
                dots_in_line: 250,
                render_seen: true,
                split_fetch: 3,
                split_active: true,
                split_mode: 1,
                split_scroll: 33,
                split_bank: 4,
                mult_a: 6,
                mult_b: 7,
                mult_result: 42,
            }),
            MapperState::AxRom(AxRomState {
                selected_prg_bank: 9,
            }),
            MapperState::Action53(Action53State {
                regs: [1, 2, 3, 4],
                selected: 70_000,
                mirroring_bit: 1,
            }),
            MapperState::Nina001(Nina001State {
                selected_prg_bank: 2,
                chr_bank0: 4,
                chr_bank1: 6,
            }),
            MapperState::BxRom(BxRomState {
                selected_prg_bank: 512,
            }),
            MapperState::Vrc24(Vrc24State {
                prg_ram_enabled: true,
                prg_select_0: 3,
                prg_select_1: 5,
                prg_swap_mode: true,
                irq_latch: 0xaa,
                irq_counter: 0x55,
                irq_prescaler: 0x1234,
                irq_mode: true,
                irq_enable: true,
                irq_enable_after_ack: true,
                irq_pending: true,
                microwire_latch: 0xcc,
                chr_nibbles: [9; 16],
            }),
            MapperState::J87(J87State),
        ];
        for state in &states {
            let (tag, payload) = encode_mapper(state);
            assert_eq!(tag, state.tag());
            assert!(
                KNOWN_MAPPER_TAGS.contains(&tag.as_str()),
                "tag {tag} must be a known mapper tag"
            );
            let back = decode_mapper(&tag, payload).unwrap();
            assert_eq!(&back, state, "tag {tag} must round-trip identically");
        }
    }
}
