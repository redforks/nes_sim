# BK2 NES determinism fields and warn policy inputs (research for #96)

Part of #94. Companion to #95 (container + input-log contract).
This branch is a throwaway: research output only, never merged, no emulator code changes.

## Anchor: existing FM2 path

`nes_core/src/movie.rs` — `Fm2File::valid_rom_file(&self, file: &INesFile)` decodes the
header's `romChecksum` (`base64:`-prefixed MD5 of PRG+CHR) and compares it to
`md5(prg_rom ++ chr_rom)`. It returns a bool; the caller warns on mismatch and keeps
playing. Precedent: **ROM-identity mismatch is warn-level, never a hard error.**

## What BizHawk loads/applies on NES movie open (primary sources)

- Container/fields: `Header` (key/value text), `Input Log`, `SyncSettings` (JSON blob,
  auto-loaded and applied), `CoreState` (savestate anchor, if present).
  https://tasvideos.org/Bizhawk/BK2Format
- Platform gate: `MovieSession.QueueNewMovie` throws `MoviePlatformMismatchException`
  when `movie.SystemID != loaded systemId` — playback is **refused**.
  Hash mismatch is NOT a gate: `MainForm.StartNewMovie` only posts the on-screen
  message `"Warning: Movie hash does not match the ROM"` and continues; empty hash
  skips the check entirely.
  (`src/BizHawk.Client.Common/movie/MovieSession.cs`,
  `src/BizHawk.Client.EmuHawk/MainForm.Movie.cs`)
- Record-time header writes (`PopulateWithDefaultHeaderValues` in `MainForm.Movie.cs`):
  `Core` = active core name, `GameName`, `SHA1` (= `Game.Hash`), `BoardName` (from
  `IBoardInfo`, only `if (Emulator.HasBoardInfo())`), `PAL`=`1` (only if region is PAL),
  `FirmwareSHA1`/firmware keys, `IsVS` for VS games. `BoardName` is **recorded but never
  compared on load**; `Core` **auto-switches** the preferred core (`QueueNewMovie`
  overwrites `preferredCores`), never warns.
- `StartsFromSavestate` is a convenience flag; "the existence of a CoreState object is
  what makes a movie savestate-anchored. Even if this property was false, that file will
  still be checked and loaded if it exists." On open, `Bk2Movie.LoadBk2Fields` reads the
  `SyncSettings` lump and the `CoreState`/`MovieSaveRam` lumps, and `RunQueuedMovie`
  applies them (`ProcessSavestate`/`ProcessSram`) before frame 0.
  (`src/BizHawk.Client.Common/movie/bk2/Bk2Movie.IO.cs`)
- NES board detection (`NES.Init`, `src/.../NES/NES.cs`): board class is re-derived from
  the ROM every load — GameDB hash lookup, then BootGodDB SHA1, then UNIF/iNES-header
  heuristics — fed by `SyncSettings.BoardProperties` as initial mapper register values.
  Effective region = ROM database `System` string unless `SyncSettings.RegionOverride`
  says otherwise; unknown/default falls back to NTSC.
- NesHawk determinism keys (`NESSyncSettings`, `src/.../NES/NES.ISettable.cs`):
  `BoardProperties`, `RegionOverride` (Default/NTSC/PAL/Dendy), `Controls`,
  `InitialWRamStatePattern`, `VSDipswitches`. Any difference forces a core reboot
  (`NeedsReboot`). Non-sync `NESSettings` (palette, clipping, sprite limit, APU volume)
  do not affect sync. QuickNES sync settings (`QuickNES.ISettable.cs`) are only
  `Port1`/`Port2` peripherals — no region/board keys — so a QuickNES-recorded movie
  carries no region or board information at all.

## Rule table: warn vs play for our BK2 player (NesHawk-class, NTSC, dual gamepads)

| Field | BizHawk behaviour | Our rule |
|---|---|---|
| `Platform` (SystemID) | Hard refuse on mismatch | **REFUSE** unless `NES` |
| `SHA1` (misleading name: CRC32, MD5, or SHA1 hex, unprefixed) | Warn-only on-screen message, plays on | **WARN + play** on digest mismatch; skip check if absent. Accept 8-hex (CRC32) / 32-hex (MD5) / 40-hex (SHA1) against the same bytes BizHawk hashed. FM2 precedent applies |
| `GameName` | Recorded, never compared | **Ignore** (no warn; free text) |
| `BoardName` | Recorded (`IBoardInfo.BoardName`, the C# board class), never compared; board re-derives from ROM+DB | **WARN (info only) + play** on mismatch; if the ROM digest matches, BoardName is implied consistent via the same detection path (drift only if the game DB changed) |
| `Core` | Auto-switches to the movie's core, no warning | **WARN + play** when `Core != NesHawk`. We cannot switch cores; QuickNES movies additionally lack region/board sync data, so desync risk is real but input is still applicable. BizHawk's analogue is silent switching, ours is a loud warn |
| SyncSettings `RegionOverride` | Applied; overrides ROM region (default→NTSC) | Effective region != NTSC → **REFUSE** (cycle timing differs; desync certain). `Default`+NTSC ROM → play. Cross-check `PAL` header flag vs override; conflict → **WARN** |
| SyncSettings `Controls` | Applied; defines port devices | Anything beyond dual gamepads (zapper, Arkanoid, FourScore, etc.) → **REFUSE** (out of #94 scope) |
| SyncSettings `BoardProperties` | Applied as initial mapper register values | Non-empty and unhonoured by our board → **WARN + play** |
| SyncSettings `InitialWRamStatePattern` | Applied; non-default WRAM init | Non-default → **WARN + play** (we use our own power-on init) |
| SyncSettings `VSDipswitches` / `IsVS` | Applied (VS Unisystem only) | Non-default or VS game → **WARN**; VS hardware out of scope, treat like unhonoured board config |
| Other SyncSettings / `NESSettings` (palette, clipping, sprites) | Cosmetic or absent from sync blob | **Ignore** |
| `StartsFromSavestate` / `CoreState` present | CoreState applied to emulator before frame 0; flag alone is not authoritative, lump presence is | **REFUSE by default** — frame 0 is not power-on state, so power-on play desyncs with certainty. Downgrade to **WARN + play-from-power-on** only behind an explicit `--play-from-power-on` override |
| `StartsFromSaveRam` / `MovieSaveRam` present | SRAM blob applied before frame 0 | Same rule as CoreState: **REFUSE by default**, override → WARN + play |
| `emuVersion` / `OriginalEmuVersion`, `Author`, `rerecordCount`, comments, subtitles | Informational | **Ignore** |

## Decisions

1. Only `Platform` mismatch and non-NTSC effective region and non-gamepad `Controls`
   refuse; everything else degrades to warn because BizHawk itself plays through hash
   drift and re-derives the board from the ROM.
2. Savestate/SRAM-anchored movies refuse by default: unlike a hash mismatch (which
   *might* still sync), a wrong frame-0 state *cannot* sync, so silent warn-and-play
   would be dishonest. Explicit override makes the risk opt-in.
3. `BoardName` and `GameName` never gate; ROM digest is the identity check (FM2 parity).
4. `Core=QuickNES` warns but plays: input log stays meaningful, timing may not be.

## Sources

- https://tasvideos.org/Bizhawk/BK2Format
- BizHawk master: `src/BizHawk.Client.Common/movie/MovieSession.cs` (`QueueNewMovie`),
  `src/BizHawk.Client.EmuHawk/MainForm.Movie.cs` (`StartNewMovie`,
  `PopulateWithDefaultHeaderValues`), `src/BizHawk.Client.Common/movie/bk2/Bk2Movie.IO.cs`
  (`LoadBk2Fields`), `src/BizHawk.Client.Common/movie/{HeaderKeys,BasicMovieInfo}.cs`,
  `src/BizHawk.Emulation.Cores/Consoles/Nintendo/NES/NES.cs` (`Init`, region fallback),
  `src/.../NES/NES.ISettable.cs` (`NESSyncSettings`),
  `src/.../QuickNES/QuickNES.ISettable.cs` (`QuickNESSyncSettings`)
- Local anchor: `nes_core/src/movie.rs` (`Fm2File::valid_rom_file`)
