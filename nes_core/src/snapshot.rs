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
//! via the mapper state API; every other component section exists with
//! its final name but is left defaulted (ticket #193 fills them). The
//! `ppu` section carries the live scanline/dot so load can validate the
//! save-point boundary (scanline 241, dot 2..=4).
//!
//! Load validates everything before mutating anything: format version,
//! ROM hash, mapper tag, enum names, JSON syntax, hex, scanline/dot. A
//! failed load leaves the machine untouched.

use crate::{
    Plugin,
    cpu::Flag,
    cpu::{
        InterruptType, InterruptWindowTag, Register, ValueSource,
        microcode::{
            AOrMemory, BranchTest, CrossPageBehavior, ImmediateOp, IncDecTarget, Microcode,
            OpAfterAddressing, PushTarget, TransferDirection,
        },
    },
    dma_bus::DmaBusState,
    ines::INesFile,
    nes::{
        NesMcu,
        apu::{
            ApuState, DmcDmaRequestState, DmcState, FrameCounterModeTag, FrameCounterState,
            LatchedFrameStepState, MixerState, NoiseState, PendingFrameModeState, PulseState,
            TriangleState,
        },
        controller::ControllerState,
        dmc_dma::{DmcDmaPhase, DmcDmaState},
        mapper::{
            Action53State, AxRomState, BxRomState, CartridgeMemorySnapshot, CnRomState, J87State,
            MapperState, Mirroring, Mmc1State, Mmc3State, Mmc5AudioState, Mmc5PcmState,
            Mmc5PulseState, Mmc5State, NRomState, Nina001State, SnapshotError, UxRomState,
            Vrc24State,
        },
        ppu::{
            Attribute, BackgroundActivationState, PpuState, Sprite, SpriteEvalModeTag,
            SpriteManagerState, SpriteOverflowEvalState, SpriteRowState, TileCacheState,
        },
    },
    nes_machine::NesMachine,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Save-file layout version. Bumped on any layout change or change to a
/// field's meaning (emulation-behavior changes do not bump it). Old
/// versions are refused, never migrated: there is exactly one constant.
///
/// Version 3: the save point is the end of the CPU cycle containing the
/// VBlank set dot (scanline 241, dot 2..=4), matching `process_frame()`.
/// Version 2 (ticket #193): every component section is filled from live
/// state (version 1 left `cpu`/`bus`/`dma`/`apu`/`controller` empty and
/// `ppu` with only the save-point dot).
pub const SAVE_FORMAT_VERSION: u32 = 3;

/// Scanline of the VBlank-entry save point (ADR 0024).
const SAVE_SCANLINE: u16 = 241;

/// Smallest returned dot when the VBlank set dot falls in the last PPU
/// sub-dot of a CPU cycle.
const MIN_SAVE_DOT: u16 = 2;

/// Largest returned dot when the VBlank set dot falls in the first PPU
/// sub-dot of a CPU cycle.
const MAX_SAVE_DOT: u16 = 4;

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
    /// Mapper state capture/restore failure (save path only: every
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
    if !text.len().is_multiple_of(2) || !text.bytes().all(|b| b.is_ascii_hexdigit()) {
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

/// Mirror of [`InterruptWindowTag`]: which interrupt sequence owns the
/// live window, if any. Compared by tag string; unknown names refuse.
fn encode_interrupt_window_tag(tag: InterruptWindowTag) -> &'static str {
    match tag {
        InterruptWindowTag::Reset => "Reset",
        InterruptWindowTag::Nmi => "Nmi",
        InterruptWindowTag::Irq => "Irq",
        InterruptWindowTag::Brk => "Brk",
    }
}

fn decode_interrupt_window_tag(name: &str) -> Result<InterruptWindowTag, SaveError> {
    match name {
        "Reset" => Ok(InterruptWindowTag::Reset),
        "Nmi" => Ok(InterruptWindowTag::Nmi),
        "Irq" => Ok(InterruptWindowTag::Irq),
        "Brk" => Ok(InterruptWindowTag::Brk),
        other => Err(SaveError::UnknownEnum {
            name: "InterruptWindow".to_string(),
            variant: other.to_string(),
        }),
    }
}

fn encode_interrupt_type(t: InterruptType) -> &'static str {
    match t {
        InterruptType::None => "None",
        InterruptType::Nmi => "Nmi",
        InterruptType::Irq => "Irq",
    }
}

fn decode_interrupt_type(name: &str) -> Result<InterruptType, SaveError> {
    match name {
        "None" => Ok(InterruptType::None),
        "Nmi" => Ok(InterruptType::Nmi),
        "Irq" => Ok(InterruptType::Irq),
        other => Err(SaveError::UnknownEnum {
            name: "InterruptType".to_string(),
            variant: other.to_string(),
        }),
    }
}

// ---------------------------------------------------------------------------
// Microcode mirror codec (ticket #193).
//
// The queue is an ordered front-first list of strings like
// `"IndexedXWithOp(And(Mem),FirstClock)"`, `"DummyReadAt(4660)"`, or
// `"FetchAndDecode"`. Payload enums use their variant names; unknown
// names refuse with `UnknownEnum`, wrong arity or bad numbers are
// `Corrupt`.

/// Split `"Name"` or `"Name(p0, p1)"` into its tag and top-level
/// payloads (nested parens stay intact). `None` on unbalanced input.
fn split_mirror(s: &str) -> Option<(&str, Vec<String>)> {
    let (name, rest) = match s.find('(') {
        None => return Some((s, Vec::new())),
        Some(i) => (&s[..i], &s[i + 1..]),
    };
    if !rest.ends_with(')') {
        return None;
    }
    let inner = &rest[..rest.len() - 1];
    let mut parts = Vec::new();
    let mut depth = 0usize;
    let mut start = 0usize;
    for (i, c) in inner.char_indices() {
        match c {
            '(' => depth += 1,
            ')' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
            }
            ',' if depth == 0 => {
                parts.push(inner[start..i].to_string());
                start = i + 1;
            }
            _ => {}
        }
    }
    if depth != 0 {
        return None;
    }
    parts.push(inner[start..].to_string());
    Some((name, parts))
}

fn corrupt_microcode(s: &str) -> SaveError {
    SaveError::Corrupt(format!("microcode `{s}` is not a valid mirror string"))
}

fn unknown_microcode(name: &str, variant: &str) -> SaveError {
    SaveError::UnknownEnum {
        name: name.to_string(),
        variant: variant.to_string(),
    }
}

fn encode_value_source(v: ValueSource) -> &'static str {
    match v {
        ValueSource::Immediate => "Immediate",
        ValueSource::ZeroPage => "ZeroPage",
        ValueSource::Mem => "Mem",
    }
}

fn decode_value_source(s: &str) -> Result<ValueSource, SaveError> {
    match s {
        "Immediate" => Ok(ValueSource::Immediate),
        "ZeroPage" => Ok(ValueSource::ZeroPage),
        "Mem" => Ok(ValueSource::Mem),
        other => Err(unknown_microcode("ValueSource", other)),
    }
}

fn encode_register(r: Register) -> &'static str {
    match r {
        Register::A => "A",
        Register::X => "X",
        Register::Y => "Y",
    }
}

fn decode_register(s: &str) -> Result<Register, SaveError> {
    match s {
        "A" => Ok(Register::A),
        "X" => Ok(Register::X),
        "Y" => Ok(Register::Y),
        other => Err(unknown_microcode("Register", other)),
    }
}

fn encode_immediate_op(op: ImmediateOp) -> &'static str {
    match op {
        ImmediateOp::Adc => "Adc",
        ImmediateOp::And => "And",
        ImmediateOp::Sbc => "Sbc",
        ImmediateOp::Cmp => "Cmp",
        ImmediateOp::Cpx => "Cpx",
        ImmediateOp::Cpy => "Cpy",
        ImmediateOp::Ora => "Ora",
        ImmediateOp::Eor => "Eor",
    }
}

fn decode_immediate_op(s: &str) -> Result<ImmediateOp, SaveError> {
    match s {
        "Adc" => Ok(ImmediateOp::Adc),
        "And" => Ok(ImmediateOp::And),
        "Sbc" => Ok(ImmediateOp::Sbc),
        "Cmp" => Ok(ImmediateOp::Cmp),
        "Cpx" => Ok(ImmediateOp::Cpx),
        "Cpy" => Ok(ImmediateOp::Cpy),
        "Ora" => Ok(ImmediateOp::Ora),
        "Eor" => Ok(ImmediateOp::Eor),
        other => Err(unknown_microcode("ImmediateOp", other)),
    }
}

fn encode_transfer(d: TransferDirection) -> &'static str {
    match d {
        TransferDirection::AtoX => "AtoX",
        TransferDirection::XtoA => "XtoA",
        TransferDirection::AtoY => "AtoY",
        TransferDirection::YtoA => "YtoA",
        TransferDirection::SPtoX => "SPtoX",
        TransferDirection::XtoSP => "XtoSP",
    }
}

fn decode_transfer(s: &str) -> Result<TransferDirection, SaveError> {
    match s {
        "AtoX" => Ok(TransferDirection::AtoX),
        "XtoA" => Ok(TransferDirection::XtoA),
        "AtoY" => Ok(TransferDirection::AtoY),
        "YtoA" => Ok(TransferDirection::YtoA),
        "SPtoX" => Ok(TransferDirection::SPtoX),
        "XtoSP" => Ok(TransferDirection::XtoSP),
        other => Err(unknown_microcode("TransferDirection", other)),
    }
}

fn encode_inc_dec(t: IncDecTarget) -> &'static str {
    match t {
        IncDecTarget::IncrementX => "IncrementX",
        IncDecTarget::IncrementY => "IncrementY",
        IncDecTarget::DecrementX => "DecrementX",
        IncDecTarget::DecrementY => "DecrementY",
        IncDecTarget::IncrementAlu => "IncrementAlu",
        IncDecTarget::DecrementAlu => "DecrementAlu",
    }
}

fn decode_inc_dec(s: &str) -> Result<IncDecTarget, SaveError> {
    match s {
        "IncrementX" => Ok(IncDecTarget::IncrementX),
        "IncrementY" => Ok(IncDecTarget::IncrementY),
        "DecrementX" => Ok(IncDecTarget::DecrementX),
        "DecrementY" => Ok(IncDecTarget::DecrementY),
        "IncrementAlu" => Ok(IncDecTarget::IncrementAlu),
        "DecrementAlu" => Ok(IncDecTarget::DecrementAlu),
        other => Err(unknown_microcode("IncDecTarget", other)),
    }
}

fn encode_a_or_memory(v: AOrMemory) -> &'static str {
    match v {
        AOrMemory::Accumulator => "Accumulator",
        AOrMemory::Memory => "Memory",
    }
}

fn decode_a_or_memory(s: &str) -> Result<AOrMemory, SaveError> {
    match s {
        "Accumulator" => Ok(AOrMemory::Accumulator),
        "Memory" => Ok(AOrMemory::Memory),
        other => Err(unknown_microcode("AOrMemory", other)),
    }
}

fn encode_cross_page(c: CrossPageBehavior) -> &'static str {
    match c {
        CrossPageBehavior::FirstClock => "FirstClock",
        CrossPageBehavior::FirstClockAlways => "FirstClockAlways",
    }
}

fn decode_cross_page(s: &str) -> Result<CrossPageBehavior, SaveError> {
    match s {
        "FirstClock" => Ok(CrossPageBehavior::FirstClock),
        "FirstClockAlways" => Ok(CrossPageBehavior::FirstClockAlways),
        other => Err(unknown_microcode("CrossPageBehavior", other)),
    }
}

fn encode_branch_test(t: BranchTest) -> &'static str {
    match t {
        BranchTest::IfCarryClear => "IfCarryClear",
        BranchTest::IfCarrySet => "IfCarrySet",
        BranchTest::IfZeroSet => "IfZeroSet",
        BranchTest::IfZeroClear => "IfZeroClear",
        BranchTest::IfNegativeSet => "IfNegativeSet",
        BranchTest::IfNegativeClear => "IfNegativeClear",
        BranchTest::IfOverflowSet => "IfOverflowSet",
        BranchTest::IfOverflowClear => "IfOverflowClear",
    }
}

fn decode_branch_test(s: &str) -> Result<BranchTest, SaveError> {
    match s {
        "IfCarryClear" => Ok(BranchTest::IfCarryClear),
        "IfCarrySet" => Ok(BranchTest::IfCarrySet),
        "IfZeroSet" => Ok(BranchTest::IfZeroSet),
        "IfZeroClear" => Ok(BranchTest::IfZeroClear),
        "IfNegativeSet" => Ok(BranchTest::IfNegativeSet),
        "IfNegativeClear" => Ok(BranchTest::IfNegativeClear),
        "IfOverflowSet" => Ok(BranchTest::IfOverflowSet),
        "IfOverflowClear" => Ok(BranchTest::IfOverflowClear),
        other => Err(unknown_microcode("BranchTest", other)),
    }
}

fn encode_push_target(t: PushTarget) -> &'static str {
    match t {
        PushTarget::A => "A",
        PushTarget::Pcl => "Pcl",
        PushTarget::Pch => "Pch",
    }
}

fn decode_push_target(s: &str) -> Result<PushTarget, SaveError> {
    match s {
        "A" => Ok(PushTarget::A),
        "Pcl" => Ok(PushTarget::Pcl),
        "Pch" => Ok(PushTarget::Pch),
        other => Err(unknown_microcode("PushTarget", other)),
    }
}

fn encode_flag(f: Flag) -> &'static str {
    match f {
        Flag::Carry => "Carry",
        Flag::Zero => "Zero",
        Flag::InterruptDisabled => "InterruptDisabled",
        Flag::Decimal => "Decimal",
        Flag::Break => "Break",
        Flag::NotUsed => "NotUsed",
        Flag::Overflow => "Overflow",
        Flag::Negative => "Negative",
    }
}

fn decode_flag(s: &str) -> Result<Flag, SaveError> {
    match s {
        "Carry" => Ok(Flag::Carry),
        "Zero" => Ok(Flag::Zero),
        "InterruptDisabled" => Ok(Flag::InterruptDisabled),
        "Decimal" => Ok(Flag::Decimal),
        "Break" => Ok(Flag::Break),
        "NotUsed" => Ok(Flag::NotUsed),
        "Overflow" => Ok(Flag::Overflow),
        "Negative" => Ok(Flag::Negative),
        other => Err(unknown_microcode("Flag", other)),
    }
}

fn encode_op_after(op: OpAfterAddressing) -> String {
    match op {
        OpAfterAddressing::And(s) => format!("And({})", encode_value_source(s)),
        OpAfterAddressing::LoadIntoA => "LoadIntoA".to_string(),
        OpAfterAddressing::LoadIntoX => "LoadIntoX".to_string(),
        OpAfterAddressing::LoadIntoY => "LoadIntoY".to_string(),
        OpAfterAddressing::LoadIntoAlu => "LoadIntoAlu".to_string(),
        OpAfterAddressing::StoreA => "StoreA".to_string(),
        OpAfterAddressing::Ora => "Ora".to_string(),
        OpAfterAddressing::Eor => "Eor".to_string(),
        OpAfterAddressing::Lax => "Lax".to_string(),
        OpAfterAddressing::Las => "Las".to_string(),
        OpAfterAddressing::Sbc => "Sbc".to_string(),
        OpAfterAddressing::Adc => "Adc".to_string(),
        OpAfterAddressing::Cmp => "Cmp".to_string(),
        OpAfterAddressing::Shx => "Shx".to_string(),
        OpAfterAddressing::Shy => "Shy".to_string(),
        OpAfterAddressing::Sha => "Sha".to_string(),
        OpAfterAddressing::Tas => "Tas".to_string(),
        OpAfterAddressing::Nop => "Nop".to_string(),
    }
}

fn decode_op_after(s: &str) -> Result<OpAfterAddressing, SaveError> {
    let (name, args) = split_mirror(s).ok_or_else(|| corrupt_microcode(s))?;
    if name == "And" {
        if args.len() != 1 {
            return Err(corrupt_microcode(s));
        }
        return Ok(OpAfterAddressing::And(decode_value_source(&args[0])?));
    }
    if !args.is_empty() {
        return Err(corrupt_microcode(s));
    }
    match name {
        "LoadIntoA" => Ok(OpAfterAddressing::LoadIntoA),
        "LoadIntoX" => Ok(OpAfterAddressing::LoadIntoX),
        "LoadIntoY" => Ok(OpAfterAddressing::LoadIntoY),
        "LoadIntoAlu" => Ok(OpAfterAddressing::LoadIntoAlu),
        "StoreA" => Ok(OpAfterAddressing::StoreA),
        "Ora" => Ok(OpAfterAddressing::Ora),
        "Eor" => Ok(OpAfterAddressing::Eor),
        "Lax" => Ok(OpAfterAddressing::Lax),
        "Las" => Ok(OpAfterAddressing::Las),
        "Sbc" => Ok(OpAfterAddressing::Sbc),
        "Adc" => Ok(OpAfterAddressing::Adc),
        "Cmp" => Ok(OpAfterAddressing::Cmp),
        "Shx" => Ok(OpAfterAddressing::Shx),
        "Shy" => Ok(OpAfterAddressing::Shy),
        "Sha" => Ok(OpAfterAddressing::Sha),
        "Tas" => Ok(OpAfterAddressing::Tas),
        "Nop" => Ok(OpAfterAddressing::Nop),
        other => Err(unknown_microcode("OpAfterAddressing", other)),
    }
}

fn one_arg(_name: &str, args: &[String], s: &str) -> Result<String, SaveError> {
    if args.len() != 1 {
        return Err(corrupt_microcode(s));
    }
    Ok(args[0].clone())
}

fn two_args(_name: &str, args: &[String], s: &str) -> Result<(String, String), SaveError> {
    if args.len() != 2 {
        return Err(corrupt_microcode(s));
    }
    Ok((args[0].clone(), args[1].clone()))
}

fn no_args(_name: &str, args: &[String], s: &str) -> Result<(), SaveError> {
    if !args.is_empty() {
        return Err(corrupt_microcode(s));
    }
    Ok(())
}

/// Encode one microcode as its mirror string.
fn encode_microcode(m: &Microcode) -> String {
    match m {
        Microcode::FetchAndDecode => "FetchAndDecode".to_string(),
        Microcode::FetchOnly => "FetchOnly".to_string(),
        Microcode::LoadR(v, r) => {
            format!("LoadR({},{})", encode_value_source(*v), encode_register(*r))
        }
        Microcode::StoreR(v, r) => {
            format!(
                "StoreR({},{})",
                encode_value_source(*v),
                encode_register(*r)
            )
        }
        Microcode::StoreAlu(v) => format!("StoreAlu({})", encode_value_source(*v)),
        Microcode::ZeroPage => "ZeroPage".to_string(),
        Microcode::ZeroPageIndexedX => "ZeroPageIndexedX".to_string(),
        Microcode::ZeroPageIndexedY => "ZeroPageIndexedY".to_string(),
        Microcode::AbsoluteL => "AbsoluteL".to_string(),
        Microcode::AbsoluteH => "AbsoluteH".to_string(),
        Microcode::IndexedXWithOp { op, first_clock } => format!(
            "IndexedXWithOp({},{})",
            encode_op_after(*op),
            encode_cross_page(*first_clock)
        ),
        Microcode::IndexedYWithOp { op, first_clock } => format!(
            "IndexedYWithOp({},{})",
            encode_op_after(*op),
            encode_cross_page(*first_clock)
        ),
        Microcode::Nop => "Nop".to_string(),
        Microcode::DummyReadAt(addr) => format!("DummyReadAt({addr})"),
        Microcode::DummyStackRead => "DummyStackRead".to_string(),
        Microcode::SkipDetectInterrupt => "SkipDetectInterrupt".to_string(),
        Microcode::IndexedL(v) => format!("IndexedL({})", encode_value_source(*v)),
        Microcode::IndexedH(v) => format!("IndexedH({})", encode_value_source(*v)),
        Microcode::SkipImmediate => "SkipImmediate".to_string(),
        Microcode::LoadIntoAlu(v) => {
            format!("LoadIntoAlu({})", encode_value_source(*v))
        }
        Microcode::Shx => "Shx".to_string(),
        Microcode::Shy => "Shy".to_string(),
        Microcode::Sha => "Sha".to_string(),
        Microcode::Tas => "Tas".to_string(),
        Microcode::ImmediateWithOp(op) => {
            format!("ImmediateWithOp({})", encode_immediate_op(*op))
        }
        Microcode::Adc(v) => format!("Adc({})", encode_value_source(*v)),
        Microcode::Sbc(v) => format!("Sbc({})", encode_value_source(*v)),
        Microcode::Cmp(v) => format!("Cmp({})", encode_value_source(*v)),
        Microcode::Cpx(v) => format!("Cpx({})", encode_value_source(*v)),
        Microcode::Cpy(v) => format!("Cpy({})", encode_value_source(*v)),
        Microcode::Ora(v) => format!("Ora({})", encode_value_source(*v)),
        Microcode::Eor(v) => format!("Eor({})", encode_value_source(*v)),
        Microcode::And(v) => format!("And({})", encode_value_source(*v)),
        Microcode::AneImmediate => "AneImmediate".to_string(),
        Microcode::LaxImmediate => "LaxImmediate".to_string(),
        Microcode::Las => "Las".to_string(),
        Microcode::Bit(v) => format!("Bit({})", encode_value_source(*v)),
        Microcode::Asl(a, v) => format!(
            "Asl({},{})",
            encode_a_or_memory(*a),
            encode_value_source(*v)
        ),
        Microcode::Lsr(a, v) => format!(
            "Lsr({},{})",
            encode_a_or_memory(*a),
            encode_value_source(*v)
        ),
        Microcode::Rol(a, v) => format!(
            "Rol({},{})",
            encode_a_or_memory(*a),
            encode_value_source(*v)
        ),
        Microcode::Ror(a, v) => format!(
            "Ror({},{})",
            encode_a_or_memory(*a),
            encode_value_source(*v)
        ),
        Microcode::AlrImmediate => "AlrImmediate".to_string(),
        Microcode::AncImmediate => "AncImmediate".to_string(),
        Microcode::ArrImmediate => "ArrImmediate".to_string(),
        Microcode::AxsImmediate => "AxsImmediate".to_string(),
        Microcode::Lax(v) => format!("Lax({})", encode_value_source(*v)),
        Microcode::Sax(v) => format!("Sax({})", encode_value_source(*v)),
        Microcode::Rla(v) => format!("Rla({})", encode_value_source(*v)),
        Microcode::Dcp(v) => format!("Dcp({})", encode_value_source(*v)),
        Microcode::Isc(v) => format!("Isc({})", encode_value_source(*v)),
        Microcode::Rra(v) => format!("Rra({})", encode_value_source(*v)),
        Microcode::Slo(v) => format!("Slo({})", encode_value_source(*v)),
        Microcode::Sre(v) => format!("Sre({})", encode_value_source(*v)),
        Microcode::IndexedHAndJump => "IndexedHAndJump".to_string(),
        Microcode::SetFlag(f) => format!("SetFlag({})", encode_flag(*f)),
        Microcode::ClearFlag(f) => format!("ClearFlag({})", encode_flag(*f)),
        Microcode::Transfer(d) => format!("Transfer({})", encode_transfer(*d)),
        Microcode::IncDec(t, v) => {
            format!("IncDec({},{})", encode_inc_dec(*t), encode_value_source(*v))
        }
        Microcode::BranchRelative(t) => {
            format!("BranchRelative({})", encode_branch_test(*t))
        }
        Microcode::PushStatus { break_flag } => format!("PushStatus({break_flag})"),
        Microcode::Plp => "Plp".to_string(),
        Microcode::PopPcL => "PopPcL".to_string(),
        Microcode::PopPcH => "PopPcH".to_string(),
        Microcode::IncPc => "IncPc".to_string(),
        Microcode::PushStack(t) => format!("PushStack({})", encode_push_target(*t)),
        Microcode::PopStack => "PopStack".to_string(),
        Microcode::UpdateAFromAlu => "UpdateAFromAlu".to_string(),
        Microcode::Kill => "Kill".to_string(),
        Microcode::LoadResetPcL => "LoadResetPcL".to_string(),
        Microcode::LoadResetPcH => "LoadResetPcH".to_string(),
        Microcode::LoadNmiPcL => "LoadNmiPcL".to_string(),
        Microcode::LoadNmiPcH => "LoadNmiPcH".to_string(),
        Microcode::LoadPcAbsoluteH => "LoadPcAbsoluteH".to_string(),
        Microcode::LoadIrqPcL => "LoadIrqPcL".to_string(),
        Microcode::LoadIrqPcH => "LoadIrqPcH".to_string(),
    }
}

/// Decode one microcode mirror string. Unknown variant names refuse with
/// `UnknownEnum`; wrong arity or bad numbers are `Corrupt`.
fn decode_microcode(s: &str) -> Result<Microcode, SaveError> {
    let (name, args) = split_mirror(s).ok_or_else(|| corrupt_microcode(s))?;
    match name {
        "FetchAndDecode" => {
            no_args(name, &args, s)?;
            Ok(Microcode::FetchAndDecode)
        }
        "FetchOnly" => {
            no_args(name, &args, s)?;
            Ok(Microcode::FetchOnly)
        }
        "LoadR" => {
            let (v, r) = two_args(name, &args, s)?;
            Ok(Microcode::LoadR(
                decode_value_source(&v)?,
                decode_register(&r)?,
            ))
        }
        "StoreR" => {
            let (v, r) = two_args(name, &args, s)?;
            Ok(Microcode::StoreR(
                decode_value_source(&v)?,
                decode_register(&r)?,
            ))
        }
        "StoreAlu" => Ok(Microcode::StoreAlu(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "ZeroPage" => {
            no_args(name, &args, s)?;
            Ok(Microcode::ZeroPage)
        }
        "ZeroPageIndexedX" => {
            no_args(name, &args, s)?;
            Ok(Microcode::ZeroPageIndexedX)
        }
        "ZeroPageIndexedY" => {
            no_args(name, &args, s)?;
            Ok(Microcode::ZeroPageIndexedY)
        }
        "AbsoluteL" => {
            no_args(name, &args, s)?;
            Ok(Microcode::AbsoluteL)
        }
        "AbsoluteH" => {
            no_args(name, &args, s)?;
            Ok(Microcode::AbsoluteH)
        }
        "IndexedXWithOp" => {
            let (op, first) = two_args(name, &args, s)?;
            Ok(Microcode::IndexedXWithOp {
                op: decode_op_after(&op)?,
                first_clock: decode_cross_page(&first)?,
            })
        }
        "IndexedYWithOp" => {
            let (op, first) = two_args(name, &args, s)?;
            Ok(Microcode::IndexedYWithOp {
                op: decode_op_after(&op)?,
                first_clock: decode_cross_page(&first)?,
            })
        }
        "Nop" => {
            no_args(name, &args, s)?;
            Ok(Microcode::Nop)
        }
        "DummyReadAt" => {
            let addr = one_arg(name, &args, s)?
                .parse::<u16>()
                .map_err(|_| corrupt_microcode(s))?;
            Ok(Microcode::DummyReadAt(addr))
        }
        "DummyStackRead" => {
            no_args(name, &args, s)?;
            Ok(Microcode::DummyStackRead)
        }
        "SkipDetectInterrupt" => {
            no_args(name, &args, s)?;
            Ok(Microcode::SkipDetectInterrupt)
        }
        "IndexedL" => Ok(Microcode::IndexedL(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "IndexedH" => Ok(Microcode::IndexedH(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "SkipImmediate" => {
            no_args(name, &args, s)?;
            Ok(Microcode::SkipImmediate)
        }
        "LoadIntoAlu" => Ok(Microcode::LoadIntoAlu(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Shx" => {
            no_args(name, &args, s)?;
            Ok(Microcode::Shx)
        }
        "Shy" => {
            no_args(name, &args, s)?;
            Ok(Microcode::Shy)
        }
        "Sha" => {
            no_args(name, &args, s)?;
            Ok(Microcode::Sha)
        }
        "Tas" => {
            no_args(name, &args, s)?;
            Ok(Microcode::Tas)
        }
        "ImmediateWithOp" => Ok(Microcode::ImmediateWithOp(decode_immediate_op(&one_arg(
            name, &args, s,
        )?)?)),
        "Adc" => Ok(Microcode::Adc(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Sbc" => Ok(Microcode::Sbc(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Cmp" => Ok(Microcode::Cmp(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Cpx" => Ok(Microcode::Cpx(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Cpy" => Ok(Microcode::Cpy(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Ora" => Ok(Microcode::Ora(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Eor" => Ok(Microcode::Eor(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "And" => Ok(Microcode::And(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "AneImmediate" => {
            no_args(name, &args, s)?;
            Ok(Microcode::AneImmediate)
        }
        "LaxImmediate" => {
            no_args(name, &args, s)?;
            Ok(Microcode::LaxImmediate)
        }
        "Las" => {
            no_args(name, &args, s)?;
            Ok(Microcode::Las)
        }
        "Bit" => Ok(Microcode::Bit(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Asl" => {
            let (a, v) = two_args(name, &args, s)?;
            Ok(Microcode::Asl(
                decode_a_or_memory(&a)?,
                decode_value_source(&v)?,
            ))
        }
        "Lsr" => {
            let (a, v) = two_args(name, &args, s)?;
            Ok(Microcode::Lsr(
                decode_a_or_memory(&a)?,
                decode_value_source(&v)?,
            ))
        }
        "Rol" => {
            let (a, v) = two_args(name, &args, s)?;
            Ok(Microcode::Rol(
                decode_a_or_memory(&a)?,
                decode_value_source(&v)?,
            ))
        }
        "Ror" => {
            let (a, v) = two_args(name, &args, s)?;
            Ok(Microcode::Ror(
                decode_a_or_memory(&a)?,
                decode_value_source(&v)?,
            ))
        }
        "AlrImmediate" => {
            no_args(name, &args, s)?;
            Ok(Microcode::AlrImmediate)
        }
        "AncImmediate" => {
            no_args(name, &args, s)?;
            Ok(Microcode::AncImmediate)
        }
        "ArrImmediate" => {
            no_args(name, &args, s)?;
            Ok(Microcode::ArrImmediate)
        }
        "AxsImmediate" => {
            no_args(name, &args, s)?;
            Ok(Microcode::AxsImmediate)
        }
        "Lax" => Ok(Microcode::Lax(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Sax" => Ok(Microcode::Sax(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Rla" => Ok(Microcode::Rla(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Dcp" => Ok(Microcode::Dcp(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Isc" => Ok(Microcode::Isc(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Rra" => Ok(Microcode::Rra(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Slo" => Ok(Microcode::Slo(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "Sre" => Ok(Microcode::Sre(decode_value_source(&one_arg(
            name, &args, s,
        )?)?)),
        "IndexedHAndJump" => {
            no_args(name, &args, s)?;
            Ok(Microcode::IndexedHAndJump)
        }
        "SetFlag" => Ok(Microcode::SetFlag(decode_flag(&one_arg(name, &args, s)?)?)),
        "ClearFlag" => Ok(Microcode::ClearFlag(decode_flag(&one_arg(
            name, &args, s,
        )?)?)),
        "Transfer" => Ok(Microcode::Transfer(decode_transfer(&one_arg(
            name, &args, s,
        )?)?)),
        "IncDec" => {
            let (t, v) = two_args(name, &args, s)?;
            Ok(Microcode::IncDec(
                decode_inc_dec(&t)?,
                decode_value_source(&v)?,
            ))
        }
        "BranchRelative" => Ok(Microcode::BranchRelative(decode_branch_test(&one_arg(
            name, &args, s,
        )?)?)),
        "PushStatus" => {
            let flag = one_arg(name, &args, s)?;
            let break_flag = match flag.as_str() {
                "true" => true,
                "false" => false,
                _ => return Err(corrupt_microcode(s)),
            };
            Ok(Microcode::PushStatus { break_flag })
        }
        "Plp" => {
            no_args(name, &args, s)?;
            Ok(Microcode::Plp)
        }
        "PopPcL" => {
            no_args(name, &args, s)?;
            Ok(Microcode::PopPcL)
        }
        "PopPcH" => {
            no_args(name, &args, s)?;
            Ok(Microcode::PopPcH)
        }
        "IncPc" => {
            no_args(name, &args, s)?;
            Ok(Microcode::IncPc)
        }
        "PushStack" => Ok(Microcode::PushStack(decode_push_target(&one_arg(
            name, &args, s,
        )?)?)),
        "PopStack" => {
            no_args(name, &args, s)?;
            Ok(Microcode::PopStack)
        }
        "UpdateAFromAlu" => {
            no_args(name, &args, s)?;
            Ok(Microcode::UpdateAFromAlu)
        }
        "Kill" => {
            no_args(name, &args, s)?;
            Ok(Microcode::Kill)
        }
        "LoadResetPcL" => {
            no_args(name, &args, s)?;
            Ok(Microcode::LoadResetPcL)
        }
        "LoadResetPcH" => {
            no_args(name, &args, s)?;
            Ok(Microcode::LoadResetPcH)
        }
        "LoadNmiPcL" => {
            no_args(name, &args, s)?;
            Ok(Microcode::LoadNmiPcL)
        }
        "LoadNmiPcH" => {
            no_args(name, &args, s)?;
            Ok(Microcode::LoadNmiPcH)
        }
        "LoadPcAbsoluteH" => {
            no_args(name, &args, s)?;
            Ok(Microcode::LoadPcAbsoluteH)
        }
        "LoadIrqPcL" => {
            no_args(name, &args, s)?;
            Ok(Microcode::LoadIrqPcL)
        }
        "LoadIrqPcH" => {
            no_args(name, &args, s)?;
            Ok(Microcode::LoadIrqPcH)
        }
        other => Err(unknown_microcode("Microcode", other)),
    }
}

// ---------------------------------------------------------------------------
// CPU section (ticket #193): every live field from `CpuState`.

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct InterruptWindowDto {
    window: String,
    started_at: Option<u64>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct NmiDetectorDto {
    last_lagged_sample: u64,
    lagged_sample: u64,
    asserted_since: u64,
    pending_asserted_since: u64,
    consumed_through: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
struct FrozenReasonDto {
    frozen: bool,
    halt: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct CpuSection {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    status: u8,
    ab: u16,
    db: u8,
    alu: u8,
    microcode_queue: Vec<String>,
    active_interrupt_window: Option<InterruptWindowDto>,
    nmi_detector: NmiDetectorDto,
    interrupt_detected: String,
    irq_input: bool,
    request_detect_interrupt: Option<bool>,
    frozen_reason: Option<FrozenReasonDto>,
}

fn encode_cpu(state: &crate::cpu::CpuState) -> CpuSection {
    CpuSection {
        a: state.a,
        x: state.x,
        y: state.y,
        pc: state.pc,
        sp: state.sp,
        status: state.status,
        ab: state.ab,
        db: state.db,
        alu: state.alu,
        microcode_queue: state.microcode_queue.iter().map(encode_microcode).collect(),
        active_interrupt_window: state.active_interrupt_window.map(|(tag, started_at)| {
            InterruptWindowDto {
                window: encode_interrupt_window_tag(tag).to_string(),
                started_at,
            }
        }),
        nmi_detector: NmiDetectorDto {
            last_lagged_sample: state.nmi_last_lagged_sample,
            lagged_sample: state.nmi_lagged_sample,
            asserted_since: state.nmi_asserted_since,
            pending_asserted_since: state.nmi_pending_asserted_since,
            consumed_through: state.nmi_consumed_through,
        },
        interrupt_detected: encode_interrupt_type(state.interrupt_detected).to_string(),
        irq_input: state.irq_input,
        request_detect_interrupt: state.request_detect_interrupt,
        frozen_reason: state
            .frozen_reason
            .map(|(frozen, halt)| FrozenReasonDto { frozen, halt }),
    }
}

fn decode_cpu(dto: &CpuSection) -> Result<crate::cpu::CpuState, SaveError> {
    let mut microcode_queue = Vec::with_capacity(dto.microcode_queue.len());
    for entry in &dto.microcode_queue {
        microcode_queue.push(decode_microcode(entry)?);
    }
    Ok(crate::cpu::CpuState {
        a: dto.a,
        x: dto.x,
        y: dto.y,
        pc: dto.pc,
        sp: dto.sp,
        status: dto.status,
        ab: dto.ab,
        db: dto.db,
        alu: dto.alu,
        microcode_queue,
        active_interrupt_window: dto
            .active_interrupt_window
            .as_ref()
            .map(|w| decode_interrupt_window_tag(&w.window).map(|tag| (tag, w.started_at)))
            .transpose()?,
        nmi_last_lagged_sample: dto.nmi_detector.last_lagged_sample,
        nmi_lagged_sample: dto.nmi_detector.lagged_sample,
        nmi_asserted_since: dto.nmi_detector.asserted_since,
        nmi_pending_asserted_since: dto.nmi_detector.pending_asserted_since,
        nmi_consumed_through: dto.nmi_detector.consumed_through,
        interrupt_detected: decode_interrupt_type(&dto.interrupt_detected)?,
        irq_input: dto.irq_input,
        request_detect_interrupt: dto.request_detect_interrupt,
        frozen_reason: dto.frozen_reason.map(|f| (f.frozen, f.halt)),
    })
}
// ---------------------------------------------------------------------------
// Bus section (ticket #193): reachable RAM as one hex string (0x800
// bytes — the winning side of the #175/#176 conflict: every access masks
// with 0x7FF so bytes 0x800..0xF00 are never read), the OAM DMA request
// latch, the open-bus byte, and the deferred APU length-counter writes.

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct DeferredApuWriteDto {
    apply_at: u64,
    address: u16,
    value: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct BusSection {
    ram: String,
    oam_dma_pending: Option<u8>,
    open_bus: u8,
    deferred_apu_writes: Vec<DeferredApuWriteDto>,
}

fn encode_bus(state: &crate::nes::McuState) -> BusSection {
    BusSection {
        ram: hex_encode(&state.ram),
        oam_dma_pending: state.oam_dma_pending,
        open_bus: state.open_bus,
        deferred_apu_writes: state
            .deferred_apu_writes
            .iter()
            .map(|(apply_at, address, value)| DeferredApuWriteDto {
                apply_at: *apply_at,
                address: *address,
                value: *value,
            })
            .collect(),
    }
}

fn decode_bus(
    dto: &BusSection,
) -> Result<([u8; 0x800], Option<u8>, u8, Vec<(u64, u16, u8)>), SaveError> {
    let raw = hex_decode("bus.ram", &dto.ram)?;
    if raw.len() != 0x800 {
        return Err(SaveError::Corrupt(format!(
            "bus.ram holds {} bytes, expected 2048 (0x800 reachable RAM bytes)",
            raw.len()
        )));
    }
    let mut ram = [0u8; 0x800];
    ram.copy_from_slice(&raw);
    Ok((
        ram,
        dto.oam_dma_pending,
        dto.open_bus,
        dto.deferred_apu_writes
            .iter()
            .map(|w| (w.apply_at, w.address, w.value))
            .collect(),
    ))
}
// ---------------------------------------------------------------------------
// DMA section (ticket #193): the DMC DMA transfer phase plus the active
// OAM transfer, if any.

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct DmcDmaDto {
    phase: String,
    delay: u8,
    first_attempt: bool,
    halt_on_put: bool,
    addr: u16,
    halt_bus_addr: u16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
struct OamActiveDto {
    page: u8,
    needs_startup_align: bool,
    transfer_cycle: u16,
    latch: u8,
    needs_pause_align: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct DmaSection {
    dmc: DmcDmaDto,
    oam: Option<OamActiveDto>,
}

fn encode_dmc_dma_phase(phase: DmcDmaPhase) -> (&'static str, u8, bool, bool) {
    match phase {
        DmcDmaPhase::Inactive => ("Inactive", 0, false, false),
        DmcDmaPhase::DelayForLoad(n) => ("DelayForLoad", n, false, false),
        DmcDmaPhase::TryHalt {
            first_attempt,
            halt_on_put,
        } => ("TryHalt", 0, first_attempt, halt_on_put),
        DmcDmaPhase::AlignOrRead => ("AlignOrRead", 0, false, false),
        DmcDmaPhase::Dummy => ("Dummy", 0, false, false),
        DmcDmaPhase::Read => ("Read", 0, false, false),
        DmcDmaPhase::Unfreeze => ("Unfreeze", 0, false, false),
    }
}

fn decode_dmc_dma_phase(dto: &DmcDmaDto) -> Result<DmcDmaPhase, SaveError> {
    match dto.phase.as_str() {
        "Inactive" => Ok(DmcDmaPhase::Inactive),
        "DelayForLoad" => Ok(DmcDmaPhase::DelayForLoad(dto.delay)),
        "TryHalt" => Ok(DmcDmaPhase::TryHalt {
            first_attempt: dto.first_attempt,
            halt_on_put: dto.halt_on_put,
        }),
        "AlignOrRead" => Ok(DmcDmaPhase::AlignOrRead),
        "Dummy" => Ok(DmcDmaPhase::Dummy),
        "Read" => Ok(DmcDmaPhase::Read),
        "Unfreeze" => Ok(DmcDmaPhase::Unfreeze),
        other => Err(SaveError::UnknownEnum {
            name: "DmcDmaPhase".to_string(),
            variant: other.to_string(),
        }),
    }
}

fn encode_dma(state: &DmaBusState) -> DmaSection {
    let (phase, delay, first_attempt, halt_on_put) = encode_dmc_dma_phase(state.dmc.phase);
    DmaSection {
        dmc: DmcDmaDto {
            phase: phase.to_string(),
            delay,
            first_attempt,
            halt_on_put,
            addr: state.dmc.addr,
            halt_bus_addr: state.dmc.halt_bus_addr,
        },
        oam: state.oam.map(|o| OamActiveDto {
            page: o.page,
            needs_startup_align: o.needs_startup_align,
            transfer_cycle: o.transfer_cycle,
            latch: o.latch,
            needs_pause_align: o.needs_pause_align,
        }),
    }
}

fn decode_dma(dto: &DmaSection) -> Result<DmaBusState, SaveError> {
    Ok(DmaBusState {
        dmc: DmcDmaState {
            phase: decode_dmc_dma_phase(&dto.dmc)?,
            addr: dto.dmc.addr,
            halt_bus_addr: dto.dmc.halt_bus_addr,
        },
        oam: dto.oam.map(|o| crate::dma_bus::OamActive {
            page: o.page,
            needs_startup_align: o.needs_startup_align,
            transfer_cycle: o.transfer_cycle,
            latch: o.latch,
            needs_pause_align: o.needs_pause_align,
        }),
    })
}
// ---------------------------------------------------------------------------
// APU section (ticket #193): all five channels, the frame counter, the
// mixer (DC filter as f32 bits), the latched MMC5 expansion mix, and the
// length-race clobber flags. The audio buffer is excluded (rewritten each
// frame) and the mixer sample rate is a construction constant.

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct PulseDto {
    timer_period: u16,
    timer_counter: u16,
    divider2_period: u8,
    divider2_counter: u8,
    length_counter: u8,
    length_halt: bool,
    length_enabled: bool,
    envelope_period: u8,
    envelope_divider_counter: u8,
    envelope_counter: u8,
    envelope_loop: bool,
    envelope_disabled: bool,
    envelope_reset_pending: bool,
    sweep_divider_period: u8,
    sweep_divider_counter: u8,
    sweep_reload: bool,
    sweep_enabled: bool,
    sweep_negate: bool,
    sweep_is_second: bool,
    sweep_shift: u8,
    duty: u8,
    sequencer_position: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct TriangleDto {
    timer_period: u16,
    timer_counter: u16,
    length_counter: u8,
    length_halt: bool,
    length_enabled: bool,
    linear_counter: u8,
    linear_halt: bool,
    linear_reload_value: u8,
    sequencer_position: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct NoiseDto {
    timer_period: u16,
    timer_counter: u16,
    length_counter: u8,
    length_halt: bool,
    length_enabled: bool,
    envelope_period: u8,
    envelope_divider_counter: u8,
    envelope_counter: u8,
    envelope_loop: bool,
    envelope_disabled: bool,
    envelope_reset_pending: bool,
    shifter_short_mode: bool,
    shifter_val: u16,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct DmcDmaRequestDto {
    kind: String,
    addr: u16,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct DmcDto {
    sample_address: u8,
    sample_length: u8,
    enable_loop: bool,
    irq_enabled: bool,
    current_address: u16,
    bytes_remaining: u16,
    request_interrupt: bool,
    dma_pending: bool,
    dma_request: Option<DmcDmaRequestDto>,
    shift_register: u8,
    remain_bits: u8,
    silence_flag: bool,
    output: u8,
    sample_buffer: Option<u8>,
    timer_period: u16,
    timer_counter: u16,
    dac_writes: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
struct LatchedFrameStepDto {
    irq: bool,
    length_and_sweep: bool,
    envelop_and_linear: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
struct PendingFrameModeDto {
    control_byte: u8,
    cycles_left: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct FrameCounterDto {
    output_latch: Option<LatchedFrameStepDto>,
    frame_interrupt: bool,
    block: u8,
    cpu_cycles: u32,
    frame_interrupt_inhibit: bool,
    pending_mode: Option<PendingFrameModeDto>,
    mode: String,
    saved_mode: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct MixerDto {
    sample_accumulator: u32,
    dc_last_input_bits: u32,
    dc_last_output_bits: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct ExpansionMixDto {
    pulse1: u8,
    pulse2: u8,
    pcm: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct ApuSection {
    pulse1: PulseDto,
    pulse2: PulseDto,
    triangle: TriangleDto,
    noise: NoiseDto,
    dmc: DmcDto,
    frame_counter: FrameCounterDto,
    mixer: MixerDto,
    expansion: ExpansionMixDto,
    length_race_clobber: Vec<bool>,
}

fn encode_apu_pulse(state: &PulseState) -> PulseDto {
    PulseDto {
        timer_period: state.timer_period,
        timer_counter: state.timer_counter,
        divider2_period: state.divider2_period,
        divider2_counter: state.divider2_counter,
        length_counter: state.length_counter,
        length_halt: state.length_halt,
        length_enabled: state.length_enabled,
        envelope_period: state.envelope_period,
        envelope_divider_counter: state.envelope_divider_counter,
        envelope_counter: state.envelope_counter,
        envelope_loop: state.envelope_loop,
        envelope_disabled: state.envelope_disabled,
        envelope_reset_pending: state.envelope_reset_pending,
        sweep_divider_period: state.sweep_divider_period,
        sweep_divider_counter: state.sweep_divider_counter,
        sweep_reload: state.sweep_reload,
        sweep_enabled: state.sweep_enabled,
        sweep_negate: state.sweep_negate,
        sweep_is_second: state.sweep_is_second,
        sweep_shift: state.sweep_shift,
        duty: state.duty,
        sequencer_position: state.sequencer_position as u8,
    }
}

fn check_sequencer_position(name: &str, position: u8, len: usize) -> Result<usize, SaveError> {
    let position = position as usize;
    if position >= len {
        return Err(SaveError::Corrupt(format!(
            "apu.{name}.sequencer_position {position} is outside 0..{len}"
        )));
    }
    Ok(position)
}

fn decode_apu_pulse(dto: &PulseDto) -> Result<PulseState, SaveError> {
    if dto.duty > 3 {
        return Err(SaveError::Corrupt(format!(
            "apu pulse duty {} is outside 0..=3",
            dto.duty
        )));
    }
    Ok(PulseState {
        timer_period: dto.timer_period,
        timer_counter: dto.timer_counter,
        divider2_period: dto.divider2_period,
        divider2_counter: dto.divider2_counter,
        length_counter: dto.length_counter,
        length_halt: dto.length_halt,
        length_enabled: dto.length_enabled,
        envelope_period: dto.envelope_period,
        envelope_divider_counter: dto.envelope_divider_counter,
        envelope_counter: dto.envelope_counter,
        envelope_loop: dto.envelope_loop,
        envelope_disabled: dto.envelope_disabled,
        envelope_reset_pending: dto.envelope_reset_pending,
        sweep_divider_period: dto.sweep_divider_period,
        sweep_divider_counter: dto.sweep_divider_counter,
        sweep_reload: dto.sweep_reload,
        sweep_enabled: dto.sweep_enabled,
        sweep_negate: dto.sweep_negate,
        sweep_is_second: dto.sweep_is_second,
        sweep_shift: dto.sweep_shift,
        duty: dto.duty,
        sequencer_position: check_sequencer_position("pulse", dto.sequencer_position, 8)?,
    })
}

fn encode_triangle(state: &TriangleState) -> TriangleDto {
    TriangleDto {
        timer_period: state.timer_period,
        timer_counter: state.timer_counter,
        length_counter: state.length_counter,
        length_halt: state.length_halt,
        length_enabled: state.length_enabled,
        linear_counter: state.linear_counter,
        linear_halt: state.linear_halt,
        linear_reload_value: state.linear_reload_value,
        sequencer_position: state.sequencer_position as u8,
    }
}

fn decode_triangle(dto: &TriangleDto) -> Result<TriangleState, SaveError> {
    Ok(TriangleState {
        timer_period: dto.timer_period,
        timer_counter: dto.timer_counter,
        length_counter: dto.length_counter,
        length_halt: dto.length_halt,
        length_enabled: dto.length_enabled,
        linear_counter: dto.linear_counter,
        linear_halt: dto.linear_halt,
        linear_reload_value: dto.linear_reload_value,
        sequencer_position: check_sequencer_position("triangle", dto.sequencer_position, 32)?,
    })
}

fn encode_noise(state: &NoiseState) -> NoiseDto {
    NoiseDto {
        timer_period: state.timer_period,
        timer_counter: state.timer_counter,
        length_counter: state.length_counter,
        length_halt: state.length_halt,
        length_enabled: state.length_enabled,
        envelope_period: state.envelope_period,
        envelope_divider_counter: state.envelope_divider_counter,
        envelope_counter: state.envelope_counter,
        envelope_loop: state.envelope_loop,
        envelope_disabled: state.envelope_disabled,
        envelope_reset_pending: state.envelope_reset_pending,
        shifter_short_mode: state.shifter_short_mode,
        shifter_val: state.shifter_val,
    }
}

fn decode_noise(dto: &NoiseDto) -> Result<NoiseState, SaveError> {
    Ok(NoiseState {
        timer_period: dto.timer_period,
        timer_counter: dto.timer_counter,
        length_counter: dto.length_counter,
        length_halt: dto.length_halt,
        length_enabled: dto.length_enabled,
        envelope_period: dto.envelope_period,
        envelope_divider_counter: dto.envelope_divider_counter,
        envelope_counter: dto.envelope_counter,
        envelope_loop: dto.envelope_loop,
        envelope_disabled: dto.envelope_disabled,
        envelope_reset_pending: dto.envelope_reset_pending,
        shifter_short_mode: dto.shifter_short_mode,
        shifter_val: dto.shifter_val,
    })
}

fn encode_dmc(state: &DmcState) -> DmcDto {
    DmcDto {
        sample_address: state.sample_address,
        sample_length: state.sample_length,
        enable_loop: state.enable_loop,
        irq_enabled: state.irq_enabled,
        current_address: state.current_address,
        bytes_remaining: state.bytes_remaining,
        request_interrupt: state.request_interrupt,
        dma_pending: state.dma_pending,
        dma_request: state.dma_request.map(|r| DmcDmaRequestDto {
            kind: if r.is_load {
                "Load".to_string()
            } else {
                "Reload".to_string()
            },
            addr: r.addr,
        }),
        shift_register: state.shift_register,
        remain_bits: state.remain_bits,
        silence_flag: state.silence_flag,
        output: state.output,
        sample_buffer: state.sample_buffer,
        timer_period: state.timer_period,
        timer_counter: state.timer_counter,
        dac_writes: state.dac_writes,
    }
}

fn decode_dmc(dto: &DmcDto) -> Result<DmcState, SaveError> {
    let dma_request = dto
        .dma_request
        .as_ref()
        .map(|r| match r.kind.as_str() {
            "Load" => Ok(DmcDmaRequestState {
                is_load: true,
                addr: r.addr,
            }),
            "Reload" => Ok(DmcDmaRequestState {
                is_load: false,
                addr: r.addr,
            }),
            other => Err(SaveError::UnknownEnum {
                name: "DmcDmaType".to_string(),
                variant: other.to_string(),
            }),
        })
        .transpose()?;
    Ok(DmcState {
        sample_address: dto.sample_address,
        sample_length: dto.sample_length,
        enable_loop: dto.enable_loop,
        irq_enabled: dto.irq_enabled,
        current_address: dto.current_address,
        bytes_remaining: dto.bytes_remaining,
        request_interrupt: dto.request_interrupt,
        dma_pending: dto.dma_pending,
        dma_request,
        shift_register: dto.shift_register,
        remain_bits: dto.remain_bits,
        silence_flag: dto.silence_flag,
        output: dto.output,
        sample_buffer: dto.sample_buffer,
        timer_period: dto.timer_period,
        timer_counter: dto.timer_counter,
        dac_writes: dto.dac_writes,
    })
}

fn encode_frame_counter_mode(mode: FrameCounterModeTag) -> &'static str {
    match mode {
        FrameCounterModeTag::FourStep => "FourStep",
        FrameCounterModeTag::FiveStep => "FiveStep",
    }
}

fn decode_frame_counter_mode(s: &str) -> Result<FrameCounterModeTag, SaveError> {
    match s {
        "FourStep" => Ok(FrameCounterModeTag::FourStep),
        "FiveStep" => Ok(FrameCounterModeTag::FiveStep),
        other => Err(SaveError::UnknownEnum {
            name: "FrameCounterMode".to_string(),
            variant: other.to_string(),
        }),
    }
}

fn encode_apu(state: &ApuState) -> ApuSection {
    ApuSection {
        pulse1: encode_apu_pulse(&state.pulse1),
        pulse2: encode_apu_pulse(&state.pulse2),
        triangle: encode_triangle(&state.triangle),
        noise: encode_noise(&state.noise),
        dmc: encode_dmc(&state.dmc),
        frame_counter: FrameCounterDto {
            output_latch: state
                .frame_counter
                .output_latch
                .map(|step| LatchedFrameStepDto {
                    irq: step.irq,
                    length_and_sweep: step.length_and_sweep,
                    envelop_and_linear: step.envelop_and_linear,
                }),
            frame_interrupt: state.frame_counter.frame_interrupt,
            block: state.frame_counter.block,
            cpu_cycles: state.frame_counter.cpu_cycles,
            frame_interrupt_inhibit: state.frame_counter.frame_interrupt_inhibit,
            pending_mode: state
                .frame_counter
                .pending_mode
                .map(|p| PendingFrameModeDto {
                    control_byte: p.control_byte,
                    cycles_left: p.cycles_left,
                }),
            mode: encode_frame_counter_mode(state.frame_counter.mode).to_string(),
            saved_mode: encode_frame_counter_mode(state.frame_counter.saved_mode).to_string(),
        },
        mixer: MixerDto {
            sample_accumulator: state.mixer.sample_accumulator,
            dc_last_input_bits: state.mixer.dc_last_input_bits,
            dc_last_output_bits: state.mixer.dc_last_output_bits,
        },
        expansion: ExpansionMixDto {
            pulse1: state.expansion.pulse1,
            pulse2: state.expansion.pulse2,
            pcm: state.expansion.pcm,
        },
        length_race_clobber: state.length_race_clobber.to_vec(),
    }
}

fn decode_apu(dto: &ApuSection) -> Result<ApuState, SaveError> {
    if dto.length_race_clobber.len() != 4 {
        return Err(SaveError::Corrupt(format!(
            "apu.length_race_clobber holds {} entries, expected 4",
            dto.length_race_clobber.len()
        )));
    }
    let mut length_race_clobber = [false; 4];
    length_race_clobber.copy_from_slice(&dto.length_race_clobber);
    Ok(ApuState {
        pulse1: decode_apu_pulse(&dto.pulse1)?,
        pulse2: decode_apu_pulse(&dto.pulse2)?,
        triangle: decode_triangle(&dto.triangle)?,
        noise: decode_noise(&dto.noise)?,
        dmc: decode_dmc(&dto.dmc)?,
        frame_counter: FrameCounterState {
            output_latch: dto
                .frame_counter
                .output_latch
                .map(|step| LatchedFrameStepState {
                    irq: step.irq,
                    length_and_sweep: step.length_and_sweep,
                    envelop_and_linear: step.envelop_and_linear,
                }),
            frame_interrupt: dto.frame_counter.frame_interrupt,
            block: dto.frame_counter.block,
            cpu_cycles: dto.frame_counter.cpu_cycles,
            frame_interrupt_inhibit: dto.frame_counter.frame_interrupt_inhibit,
            pending_mode: dto
                .frame_counter
                .pending_mode
                .map(|p| PendingFrameModeState {
                    control_byte: p.control_byte,
                    cycles_left: p.cycles_left,
                }),
            mode: decode_frame_counter_mode(&dto.frame_counter.mode)?,
            saved_mode: decode_frame_counter_mode(&dto.frame_counter.saved_mode)?,
        },
        mixer: MixerState {
            sample_accumulator: dto.mixer.sample_accumulator,
            dc_last_input_bits: dto.mixer.dc_last_input_bits,
            dc_last_output_bits: dto.mixer.dc_last_output_bits,
        },
        expansion: crate::nes::apu::ExpansionMix {
            pulse1: dto.expansion.pulse1,
            pulse2: dto.expansion.pulse2,
            pcm: dto.expansion.pcm,
        },
        length_race_clobber,
    })
}
// ---------------------------------------------------------------------------
// Controller section (ticket #193): shift-register sampling state for
// both pads plus the in-flight Zapper trigger countdown. Live held
// buttons, Zapper aim/connection, and host configuration are excluded
// per the spec (host-side input, not emulation state).

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct PadDto {
    strobe: bool,
    locked_bits: u8,
    bit_position: u8,
    oe: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct ControllerSection {
    a: PadDto,
    b: PadDto,
    zapper_trigger_cycles: u32,
}

fn encode_pad(strobe: bool, locked_bits: u8, bit_position: u8, oe: bool) -> PadDto {
    PadDto {
        strobe,
        locked_bits,
        bit_position,
        oe,
    }
}

fn encode_controller(state: &ControllerState) -> ControllerSection {
    ControllerSection {
        a: encode_pad(
            state.a.strobe,
            state.a.locked_bits,
            state.a.bit_position,
            state.a.oe,
        ),
        b: encode_pad(
            state.b.strobe,
            state.b.locked_bits,
            state.b.bit_position,
            state.b.oe,
        ),
        zapper_trigger_cycles: state.zapper_trigger_cycles,
    }
}

fn decode_controller(dto: &ControllerSection) -> ControllerState {
    crate::nes::controller::ControllerState {
        a: crate::nes::controller::PadState {
            strobe: dto.a.strobe,
            locked_bits: dto.a.locked_bits,
            bit_position: dto.a.bit_position,
            oe: dto.a.oe,
        },
        b: crate::nes::controller::PadState {
            strobe: dto.b.strobe,
            locked_bits: dto.b.locked_bits,
            bit_position: dto.b.bit_position,
            oe: dto.b.oe,
        },
        zapper_trigger_cycles: dto.zapper_trigger_cycles,
    }
}

// ---------------------------------------------------------------------------
// PPU section (ticket #193): timing, raw register bits, latches, memory,
// and evaluation state. The framebuffer, palette-index buffer,
// color-split log, and theme stay excluded per the spec (rebuilt by
// rendering). The VBlank-entry save-point boundary (scanline 241, dot 2..=4)
// is validated on load.

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct SpriteDto {
    y: u8,
    tile_idx: u8,
    attributes: u8,
    x: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct SpriteRowDto {
    x: u8,
    attributes: u8,
    low: u8,
    high: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct SpriteEvalDto {
    oam_index: u8,
    byte_index: u8,
    start_index: u8,
    mode: String,
    mode_count: u8,
    pending_sprite_bytes: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct SpriteManagerDto {
    overflow_pending: bool,
    eval: SpriteEvalDto,
    current_scanline_oam: Vec<SpriteDto>,
    next_scanline_oam: Vec<SpriteDto>,
    current_zero_sprite: Option<SpriteDto>,
    next_zero_sprite: Option<SpriteDto>,
    rows: Vec<SpriteRowDto>,
    rows_scanline: Option<u8>,
    zero_row_idx: Option<u64>,
    oam_copybuffer: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct TileCacheDto {
    bitplane_low: u8,
    bitplane_high: u8,
    palette_idx: u8,
    remaining: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct BackgroundActivationDto {
    screen_x: u8,
    vram_addr: u16,
    fine_x: u8,
    ctrl: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct PpudataLastReadDto {
    cycle: u64,
    value: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct PpuSection {
    scanline: u16,
    dot: u16,
    odd_frame: bool,
    frame_no: u64,
    cycle: u64,
    ren_latched_at_338: bool,
    ctrl: u8,
    mask: u8,
    status: u8,
    vram_addr: u16,
    temp_vram_addr: u16,
    fine_x: u8,
    write_toggle: bool,
    ppudata_buffer: u8,
    bus_latch: u8,
    bus_latch_decay_deadlines: Vec<u64>,
    bus_latch_earliest_decay: u64,
    palette: String,
    oam: String,
    oam_addr: u8,
    sprite: SpriteManagerDto,
    tile_cache: TileCacheDto,
    background_anchor: Option<BackgroundActivationDto>,
    pending_background_activation: Option<BackgroundActivationDto>,
    ppudata_last_read: Option<PpudataLastReadDto>,
}

fn encode_sprite(sprite: &Sprite) -> SpriteDto {
    SpriteDto {
        y: sprite.y,
        tile_idx: sprite.tile_idx,
        attributes: sprite.attributes.into_bytes()[0],
        x: sprite.x,
    }
}

fn decode_sprite(dto: &SpriteDto) -> Sprite {
    Sprite {
        y: dto.y,
        tile_idx: dto.tile_idx,
        attributes: Attribute::from_bytes([dto.attributes]),
        x: dto.x,
    }
}

fn encode_sprite_eval(eval: &SpriteOverflowEvalState) -> SpriteEvalDto {
    let (mode, mode_count) = match eval.mode {
        SpriteEvalModeTag::Idle => ("Idle", 0),
        SpriteEvalModeTag::ScanY => ("ScanY", 0),
        SpriteEvalModeTag::CopySprite(n) => ("CopySprite", n),
        SpriteEvalModeTag::OverflowSearchDelay => ("OverflowSearchDelay", 0),
        SpriteEvalModeTag::OverflowSearch => ("OverflowSearch", 0),
        SpriteEvalModeTag::Done => ("Done", 0),
    };
    SpriteEvalDto {
        oam_index: eval.oam_index,
        byte_index: eval.byte_index,
        start_index: eval.start_index,
        mode: mode.to_string(),
        mode_count,
        pending_sprite_bytes: hex_encode(&eval.pending_sprite_bytes),
    }
}

fn decode_sprite_eval(dto: &SpriteEvalDto) -> Result<SpriteOverflowEvalState, SaveError> {
    let mode = match dto.mode.as_str() {
        "Idle" => SpriteEvalModeTag::Idle,
        "ScanY" => SpriteEvalModeTag::ScanY,
        "CopySprite" => SpriteEvalModeTag::CopySprite(dto.mode_count),
        "OverflowSearchDelay" => SpriteEvalModeTag::OverflowSearchDelay,
        "OverflowSearch" => SpriteEvalModeTag::OverflowSearch,
        "Done" => SpriteEvalModeTag::Done,
        other => {
            return Err(SaveError::UnknownEnum {
                name: "SpriteEvalMode".to_string(),
                variant: other.to_string(),
            });
        }
    };
    let raw = hex_decode(
        "ppu.sprite.eval.pending_sprite_bytes",
        &dto.pending_sprite_bytes,
    )?;
    if raw.len() != 4 {
        return Err(SaveError::Corrupt(format!(
            "ppu.sprite.eval.pending_sprite_bytes holds {} bytes, expected 4",
            raw.len()
        )));
    }
    let mut pending_sprite_bytes = [0u8; 4];
    pending_sprite_bytes.copy_from_slice(&raw);
    Ok(SpriteOverflowEvalState {
        oam_index: dto.oam_index,
        byte_index: dto.byte_index,
        start_index: dto.start_index,
        mode,
        pending_sprite_bytes,
    })
}

fn encode_background_activation(a: &BackgroundActivationState) -> BackgroundActivationDto {
    BackgroundActivationDto {
        screen_x: a.screen_x,
        vram_addr: a.vram_addr,
        fine_x: a.fine_x,
        ctrl: a.ctrl,
    }
}

fn decode_background_activation(dto: &BackgroundActivationDto) -> BackgroundActivationState {
    BackgroundActivationState {
        screen_x: dto.screen_x,
        vram_addr: dto.vram_addr,
        fine_x: dto.fine_x,
        ctrl: dto.ctrl,
    }
}

fn encode_ppu(state: &PpuState) -> PpuSection {
    // Spec #184: neither flag is stored. `process_frame()` consumes
    // `vblank_entered` before returning, and the VBlank set dot clears
    // `suppress_vblank_pending`.
    assert!(
        !state.vblank_entered,
        "save point must observe vblank_entered unset"
    );
    assert!(
        !state.suppress_vblank_pending,
        "save point must observe suppress_vblank_pending unset"
    );
    PpuSection {
        scanline: state.scanline,
        dot: state.dot,
        odd_frame: state.odd_frame,
        frame_no: state.frame_no as u64,
        cycle: state.cycle,
        ren_latched_at_338: state.ren_latched_at_338,
        ctrl: state.registers_ctrl,
        mask: state.registers_mask,
        status: state.registers_status,
        vram_addr: state.vram_addr,
        temp_vram_addr: state.temp_vram_addr,
        fine_x: state.fine_x,
        write_toggle: state.write_toggle,
        ppudata_buffer: state.ppudata_buffer,
        bus_latch: state.bus_latch,
        bus_latch_decay_deadlines: state.bus_latch_decay_deadlines.to_vec(),
        bus_latch_earliest_decay: state.bus_latch_earliest_decay,
        palette: hex_encode(&state.palette_data),
        oam: hex_encode(&state.oam_data),
        oam_addr: state.oam_addr,
        sprite: SpriteManagerDto {
            overflow_pending: state.sprite.overflow_pending,
            eval: encode_sprite_eval(&state.sprite.eval),
            current_scanline_oam: state
                .sprite
                .current_scanline_oam
                .iter()
                .map(encode_sprite)
                .collect(),
            next_scanline_oam: state
                .sprite
                .next_scanline_oam
                .iter()
                .map(encode_sprite)
                .collect(),
            current_zero_sprite: state.sprite.current_zero_sprite.as_ref().map(encode_sprite),
            next_zero_sprite: state.sprite.next_zero_sprite.as_ref().map(encode_sprite),
            rows: state
                .sprite
                .rows
                .iter()
                .map(|r| SpriteRowDto {
                    x: r.x,
                    attributes: r.attributes,
                    low: r.low,
                    high: r.high,
                })
                .collect(),
            rows_scanline: state.sprite.rows_scanline,
            zero_row_idx: state.sprite.zero_row_idx.map(|i| i as u64),
            oam_copybuffer: state.sprite.oam_copybuffer,
        },
        tile_cache: TileCacheDto {
            bitplane_low: state.tile_cache.bitplane_low,
            bitplane_high: state.tile_cache.bitplane_high,
            palette_idx: state.tile_cache.palette_idx,
            remaining: state.tile_cache.remaining,
        },
        background_anchor: state
            .background_anchor
            .as_ref()
            .map(encode_background_activation),
        pending_background_activation: state
            .pending_background_activation
            .as_ref()
            .map(encode_background_activation),
        ppudata_last_read: state
            .ppudata_last_read
            .map(|(cycle, value)| PpudataLastReadDto { cycle, value }),
    }
}

fn decode_ppu(dto: &PpuSection) -> Result<PpuState, SaveError> {
    let palette_raw = hex_decode("ppu.palette", &dto.palette)?;
    if palette_raw.len() != 0x20 {
        return Err(SaveError::Corrupt(format!(
            "ppu.palette holds {} bytes, expected 32",
            palette_raw.len()
        )));
    }
    let mut palette_data = [0u8; 0x20];
    palette_data.copy_from_slice(&palette_raw);
    let oam_raw = hex_decode("ppu.oam", &dto.oam)?;
    if oam_raw.len() != 256 {
        return Err(SaveError::Corrupt(format!(
            "ppu.oam holds {} bytes, expected 256",
            oam_raw.len()
        )));
    }
    let mut oam_data = [0u8; 256];
    oam_data.copy_from_slice(&oam_raw);
    if dto.bus_latch_decay_deadlines.len() != 8 {
        return Err(SaveError::Corrupt(format!(
            "ppu.bus_latch_decay_deadlines holds {} entries, expected 8",
            dto.bus_latch_decay_deadlines.len()
        )));
    }
    let mut bus_latch_decay_deadlines = [0u64; 8];
    bus_latch_decay_deadlines.copy_from_slice(&dto.bus_latch_decay_deadlines);
    let eval = decode_sprite_eval(&dto.sprite.eval)?;
    let mut rows = Vec::with_capacity(dto.sprite.rows.len());
    for r in &dto.sprite.rows {
        rows.push(SpriteRowState {
            x: r.x,
            attributes: r.attributes,
            low: r.low,
            high: r.high,
        });
    }
    Ok(PpuState {
        scanline: dto.scanline,
        dot: dto.dot,
        odd_frame: dto.odd_frame,
        frame_no: dto.frame_no as usize,
        cycle: dto.cycle,
        // Spec #184: neither flag is stored; a restored machine starts
        // with both unset, matching the post-`process_frame()` boundary.
        vblank_entered: false,
        suppress_vblank_pending: false,
        ren_latched_at_338: dto.ren_latched_at_338,
        registers_ctrl: dto.ctrl,
        registers_mask: dto.mask,
        registers_status: dto.status,
        vram_addr: dto.vram_addr,
        temp_vram_addr: dto.temp_vram_addr,
        fine_x: dto.fine_x,
        write_toggle: dto.write_toggle,
        ppudata_buffer: dto.ppudata_buffer,
        bus_latch: dto.bus_latch,
        bus_latch_decay_deadlines,
        bus_latch_earliest_decay: dto.bus_latch_earliest_decay,
        palette_data,
        oam_data,
        oam_addr: dto.oam_addr,
        sprite: SpriteManagerState {
            overflow_pending: dto.sprite.overflow_pending,
            eval,
            current_scanline_oam: dto
                .sprite
                .current_scanline_oam
                .iter()
                .map(decode_sprite)
                .collect(),
            next_scanline_oam: dto
                .sprite
                .next_scanline_oam
                .iter()
                .map(decode_sprite)
                .collect(),
            current_zero_sprite: dto.sprite.current_zero_sprite.as_ref().map(decode_sprite),
            next_zero_sprite: dto.sprite.next_zero_sprite.as_ref().map(decode_sprite),
            rows,
            rows_scanline: dto.sprite.rows_scanline,
            zero_row_idx: dto.sprite.zero_row_idx.map(|i| i as usize),
            oam_copybuffer: dto.sprite.oam_copybuffer,
        },
        tile_cache: TileCacheState {
            bitplane_low: dto.tile_cache.bitplane_low,
            bitplane_high: dto.tile_cache.bitplane_high,
            palette_idx: dto.tile_cache.palette_idx,
            remaining: dto.tile_cache.remaining,
        },
        background_anchor: dto
            .background_anchor
            .as_ref()
            .map(decode_background_activation),
        pending_background_activation: dto
            .pending_background_activation
            .as_ref()
            .map(decode_background_activation),
        ppudata_last_read: dto.ppudata_last_read.as_ref().map(|r| (r.cycle, r.value)),
    })
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

/// Encodes a mapper state: returns the saved tag plus the payload.
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
/// machine must sit at the save-point boundary (scanline 241, dot 2..=4),
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
    let (mapper_tag, mapper_payload) = encode_mapper(&mapper_state);
    let mcu_state = machine.mcu().capture_mcu_state();
    let dto = SaveDto {
        format_version: SAVE_FORMAT_VERSION,
        rom_sha256: hex_encode(&rom_sha256(machine.rom())),
        frame: machine.frame_no() as u64,
        clock: machine.clock_cycles(),
        cpu: encode_cpu(&machine.cpu().capture_cpu_state()),
        bus: encode_bus(&mcu_state),
        dma: encode_dma(&machine.dma_bus().capture_dma_bus_state()),
        ppu: encode_ppu(&mcu_state.ppu),
        apu: encode_apu(&mcu_state.apu),
        controller: encode_controller(&mcu_state.controller),
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
    let cpu_state = decode_cpu(&dto.cpu)?;
    let (ram, oam_dma_pending, open_bus, deferred_apu_writes) = decode_bus(&dto.bus)?;
    let dma_state = decode_dma(&dto.dma)?;
    let ppu_state = decode_ppu(&dto.ppu)?;
    let apu_state = decode_apu(&dto.apu)?;
    let controller_state = decode_controller(&dto.controller);

    if dto.ppu.scanline != SAVE_SCANLINE {
        return Err(SaveError::Corrupt(format!(
            "stored scanline {} is not the save-point scanline {SAVE_SCANLINE}",
            dto.ppu.scanline
        )));
    }
    if !(MIN_SAVE_DOT..=MAX_SAVE_DOT).contains(&dto.ppu.dot) {
        return Err(SaveError::Corrupt(format!(
            "stored dot {} is outside {MIN_SAVE_DOT}..={MAX_SAVE_DOT}",
            dto.ppu.dot
        )));
    }

    machine
        .mcu_mut()
        .cartridge_mut()
        .restore_snapshot(&mapper_state, &memory_snapshot)
        .map_err(SaveError::Mapper)?;
    machine.cpu_mut().restore_cpu_state(&cpu_state);
    machine.dma_bus_mut().restore_dma_bus_state(&dma_state);
    machine.mcu_mut().restore_mcu_state(&crate::nes::McuState {
        ram,
        ppu: ppu_state,
        apu: apu_state,
        controller: controller_state,
        oam_dma_pending,
        open_bus,
        deferred_apu_writes,
    });
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
        assert!((MIN_SAVE_DOT..=MAX_SAVE_DOT).contains(&machine.mcu().ppu().timing().dot()));
        let frame_before = machine.frame_no();
        let clock_before = machine.clock_cycles();

        let first = save_state_json(&machine).unwrap();

        let mut loaded = machine_for(&file);
        assert_ne!(loaded.frame_no(), frame_before);
        load_state_json(&mut loaded, &first).unwrap();
        assert_eq!(loaded.frame_no(), frame_before);
        assert_eq!(loaded.clock_cycles(), clock_before);
        assert_eq!(loaded.mcu().ppu().timing().scanline(), SAVE_SCANLINE);
        assert!((MIN_SAVE_DOT..=MAX_SAVE_DOT).contains(&loaded.mcu().ppu().timing().dot()));
        assert_eq!(loaded.mcu().ppu().cycle(), clock_before);

        let second = save_state_json(&loaded).unwrap();
        assert_eq!(first, second, "re-save must be byte-identical");
    }

    /// NROM-16K exercise ROM: reset code enables NMI + rendering, pokes
    /// scroll/VRAM/OAM, pulses the controller strobe, programs all five
    /// APU channels (DMC loops a zero byte, so DMA traffic never stops),
    /// and spins on `INC $00`. NMI does `INC $02`; IRQ just returns. The
    /// frame IRQ stays enabled, so interrupt detectors, the DMC DMA
    /// engine, and the frame counter all see live traffic every frame.
    fn rich_nrom_file() -> INesFile {
        let mut prg = vec![0u8; 16 * 1024];
        let mut code: Vec<u8> = Vec::new();
        let sta = |addr: u16, body: &mut Vec<u8>| {
            body.push(0x8D);
            body.push((addr & 0xFF) as u8);
            body.push((addr >> 8) as u8);
        };
        let lda_sta = |value: u8, addr: u16, body: &mut Vec<u8>| {
            body.push(0xA9);
            body.push(value);
            sta(addr, body);
        };
        // SEI CLD LDX #$FF TXS.
        code.extend_from_slice(&[0x78, 0xD8, 0xA2, 0xFF, 0x9A]);
        lda_sta(0x88, 0x2000, &mut code);
        lda_sta(0x1E, 0x2001, &mut code);
        // Scroll + VRAM origin, then two nametable writes.
        lda_sta(0x00, 0x2005, &mut code);
        sta(0x2005, &mut code);
        sta(0x2006, &mut code);
        sta(0x2006, &mut code);
        lda_sta(0x55, 0x2007, &mut code);
        sta(0x2007, &mut code);
        // OAM: index $10 = $AA.
        lda_sta(0x10, 0x2003, &mut code);
        lda_sta(0xAA, 0x2004, &mut code);
        // Controller strobe pulse (falls back low).
        lda_sta(0x01, 0x4016, &mut code);
        lda_sta(0x00, 0x4016, &mut code);
        // APU: pulse1, triangle, noise with loaded lengths; DMC loops.
        lda_sta(0xBF, 0x4000, &mut code);
        lda_sta(0x08, 0x4001, &mut code);
        lda_sta(0x50, 0x4002, &mut code);
        lda_sta(0x08, 0x4003, &mut code);
        lda_sta(0x80, 0x4008, &mut code);
        lda_sta(0x40, 0x400A, &mut code);
        lda_sta(0x08, 0x400B, &mut code);
        lda_sta(0xBF, 0x400C, &mut code);
        lda_sta(0x05, 0x400E, &mut code);
        lda_sta(0x08, 0x400F, &mut code);
        lda_sta(0x80, 0x4010, &mut code);
        lda_sta(0x00, 0x4012, &mut code);
        sta(0x4013, &mut code);
        lda_sta(0x1F, 0x4015, &mut code);
        // 4-step frame sequencer, frame IRQ enabled.
        lda_sta(0x00, 0x4017, &mut code);
        // Main loop: INC $00; JMP loop.
        let loop_addr = 0x8000 + code.len() as u16;
        code.push(0xE6);
        code.push(0x00);
        code.push(0x4C);
        code.push((loop_addr & 0xFF) as u8);
        code.push((loop_addr >> 8) as u8);
        // NMI handler: INC $02; RTI. IRQ handler: RTI.
        let nmi_addr = 0x8000 + code.len() as u16;
        code.extend_from_slice(&[0xE6, 0x02, 0x40]);
        let irq_addr = 0x8000 + code.len() as u16;
        code.push(0x40);
        prg[..code.len()].copy_from_slice(&code);
        // Vectors at $FFFA/$FFFC/$FFFE (mirror of PRG tail).
        let put = |prg: &mut Vec<u8>, cpu_addr: u16, value: u16| {
            // 16 KiB NROM mirrors $C000-$FFFF onto $8000-$BFFF.
            let off = ((cpu_addr - 0x8000) as usize) % prg.len();
            prg[off] = (value & 0xFF) as u8;
            prg[off + 1] = (value >> 8) as u8;
        };
        put(&mut prg, 0xFFFA, nmi_addr);
        put(&mut prg, 0xFFFC, 0x8000);
        put(&mut prg, 0xFFFE, irq_addr);
        let mut rom = Vec::new();
        rom.extend_from_slice(&[0x4e, 0x45, 0x53, 0x1a, 0x01, 0x01, 0x00, 0x00]);
        rom.extend_from_slice(&[0; 8]);
        rom.extend_from_slice(&prg);
        rom.extend_from_slice(&[0u8; 8 * 1024]);
        INesFile::new(rom).unwrap()
    }

    /// Run `frames` frames, collecting each frame's pixels and audio.
    fn run_frames<P>(machine: &mut NesMachine<P>, frames: usize) -> (Vec<Vec<u8>>, Vec<Vec<f32>>)
    where
        P: Plugin<NesMcu>,
    {
        let mut pixels = Vec::with_capacity(frames);
        let mut audio = Vec::with_capacity(frames);
        for _ in 0..frames {
            let out = machine.process_frame();
            pixels.push(out.frame.to_vec());
            audio.push(out.audio.to_vec());
        }
        (pixels, audio)
    }

    #[test]
    fn resumed_run_is_bit_exact_with_continuous_run() {
        let file = rich_nrom_file();
        let mut continuous = machine_for(&file);
        let (head_pixels, head_audio) = run_frames(&mut continuous, 5);
        let save = save_state_json(&continuous).unwrap();
        let saved_frame = continuous.frame_no();
        let saved_clock = continuous.clock_cycles();
        let (tail_pixels, tail_audio) = run_frames(&mut continuous, 8);

        let mut resumed = machine_for(&file);
        load_state_json(&mut resumed, &save).unwrap();
        assert_eq!(resumed.frame_no(), saved_frame, "resume at same frame");
        assert_eq!(resumed.clock_cycles(), saved_clock, "resume at same clock");
        let (re_pixels, re_audio) = run_frames(&mut resumed, 8);

        assert_eq!(head_pixels.len(), 5);
        assert_eq!(head_audio.len(), 5);
        assert_eq!(re_pixels, tail_pixels, "framebuffer bit-exact after resume");
        assert_eq!(re_audio, tail_audio, "audio bit-exact after resume");
        assert_eq!(resumed.frame_no(), continuous.frame_no());
        assert_eq!(resumed.clock_cycles(), continuous.clock_cycles());
    }

    #[test]
    fn resume_lands_on_the_same_frame_and_clock() {
        let file = rich_nrom_file();
        let mut machine = machine_for(&file);
        run_frames(&mut machine, 3);
        let save = save_state_json(&machine).unwrap();
        let saved_frame = machine.frame_no();
        let saved_clock = machine.clock_cycles();

        let mut resumed = machine_for(&file);
        assert_ne!(resumed.frame_no(), saved_frame);
        load_state_json(&mut resumed, &save).unwrap();
        assert_eq!(resumed.frame_no(), saved_frame);
        assert_eq!(resumed.clock_cycles(), saved_clock);
        assert_eq!(resumed.mcu().ppu().timing().scanline(), SAVE_SCANLINE);
        assert!((MIN_SAVE_DOT..=MAX_SAVE_DOT).contains(&resumed.mcu().ppu().timing().dot()));
    }

    #[test]
    fn format_version_is_a_single_shared_constant() {
        assert_eq!(SAVE_FORMAT_VERSION, 3);
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
            "save state format version 7 is not supported; this build reads version 3"
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
            "save state is corrupt: stored scanline 240 is not the save-point scanline 241"
        );
        assert_eq!(fingerprint(&machine), before);
    }

    #[test]
    fn stored_dot_outside_the_save_point_range_is_refused() {
        let file = nrom_file();
        let mut machine = machine_for(&file);
        machine.process_frame();
        let save = save_state_json(&machine).unwrap();
        let tampered = tamper(&save, &["ppu", "dot"], Value::from(5));

        let before = fingerprint(&machine);
        let error = load_state_json(&mut machine, &tampered).unwrap_err();
        assert_eq!(
            error.to_string(),
            "save state is corrupt: stored dot 5 is outside 2..=4"
        );
        assert_eq!(fingerprint(&machine), before);
    }

    #[test]
    fn cartridge_mapper_registers_and_memory_bytes_round_trip() {
        let file = mmc1_file();
        let mut machine = machine_for(&file);
        machine.process_frame();
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
        let mut machine = machine_for(&file);
        machine.process_frame();
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
