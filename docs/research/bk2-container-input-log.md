# BK2 container and NES input-log contract (research for #95)

Question from #95: pin the exact BK2 bytes `nes_core` must accept for NES —
zip entry names, Header key set, NES LogKey button order, frame-line grammar,
and savestate-anchor detection. No implementation; this doc is the byte-level
contract a future parser must implement. (Determinism fields / warn policy are
#96's job; fixtures are #97's; parser API shape is #98's.)

## Primary sources

- TASVideos BK2 spec: https://tasvideos.org/Bizhawk/BK2Format
- Zip entry names: `BinaryStateLump` —
  `src/BizHawk.Client.Common/savestates/BinaryStateLump.cs` (TASEmulators/BizHawk)
- Archive read/write: `Bk2Movie.IO.cs` (`LoadBk2Fields`, `AddBk2Lumps`),
  input-log read/write: `Bk2Movie.InputLog.cs` (`ExtractInputLog`),
  log-line builder: `Bk2LogEntryGenerator.cs` (`GenerateLogKey`, `CreateLogEntry`)
- NES button order: `NESControllers.cs` (`ControllerNES._buttonOrdinals`,
  `ControllerDefinitionMerger.GetMerged`), console buttons: `NES.Core.cs`
  (`ResetControllerDefinition` adds `Power` then `Reset`; FDS/VS extras after)
- Mnemonic chars: `Bk2MnemonicLookup.cs` (`Lookup`, `BaseMnemonicLookupTable`,
  NES system overrides)
- Grouping rule: `ControllerDefinition.cs` (`ControlsOrdered`: console buttons
  first, then each player's buttons individually)
- Header key constants: `HeaderKeys.cs`
- Existing FM2 path to ground against: `nes_core/src/movie.rs`,
  `nes_core/src/movie/tests.rs`, `nescli/src/actions/play_movie.rs`
- Independent corroboration (name-not-position mapping, savestate refusal):
  bigolgeek/nes-rust PR #319 ("feat(#103): import BizHawk .bk2 movies")
- Real NES samples live on any TASVideos movie page, e.g. https://tasvideos.org/6012M
  ("Emulator Replay: BizHawk 2.9.1 · Download (.bk2)"). Do NOT hotlink files;
  #97 collects redistributable fixtures.

## 1. Container: a ZIP

`.bk2` is a plain zip (open with any unzip tool). Canonical entry file names
(from `BinaryStateLump`, `Name + '.' + Ext`):

| Entry | Contents | Required for NES? |
|---|---|---|
| `Header.txt` | `Key value` lines (see §2) | yes |
| `Input Log.txt` | `[Input]` … `LogKey:` … `\|…\|` frame lines … `[/Input]` (see §3) | yes |
| `SyncSettings.json` | core settings JSON blob, auto-applied by BizHawk | read (enforce in #96) |
| `Comments.txt` | author comments | no (preserve/ignore) |
| `Subtitles.txt` | `subtitle frame x y length color text` lines | no (preserve/ignore) |
| `Core.bin` | binary savestate — anchor frame | must DETECT (see §4) |
| `CoreText.txt` | text savestate (alternative anchor) | must DETECT (see §4) |
| `Framebuffer.bmp` | savestate thumbnail | ignore |
| `MovieSaveRam.bin` | SaveRAM-anchored start | detect; NES path can refuse for now |

Wild variants a parser must also recognise (other writers / old versions):
`CoreState`, `CoreState.bin` (same meaning as `Core.bin`). Matching should be
case-insensitive on the entry name, like the corroborating import in nes-rust.

## 2. `Header.txt`: `Key value` per line

One `Key<space>value` pair per line; the value is the rest of the line
(`Bk2Header.ToString`: `Append(k).Append(' ').AppendLine(v)`). NES-relevant
key set (constants in `HeaderKeys.cs`, field list matches the TASVideos page):

- `MovieVersion`, `emuVersion` (+ `OriginalEmuVersion`), `Platform` (must be `NES`),
  `GameName`, `Author`, `rerecordCount`
- `SHA1` — misleading name: hex-encoded **CRC32, MD5, or SHA1**, unprefixed
  (also possible: `SHA256`, `MD5`, `CRC32` keys)
- `BoardName` (mapper board, e.g. NROM-class boards), `Core` (NesHawk / QuickNES),
  `PAL` (region flag)
- `StartsFromSavestate` (`StartsFromSaveRam` for SaveRAM-anchored) — but this is
  a **convenience property only** (see §4)
- `SyncSettings` (header may echo; canonical copy is `SyncSettings.json`),
  `CycleCount`, `ClockRate`, `FirmwareSHA1`

## 3. `Input Log.txt`: LogKey + frame lines

File shape (written by `Bk2LogEntryGenerator` / `Bk2Movie.WriteInputLog`,
accepted by `ExtractInputLog`):

```
[Input]
LogKey:#Power|Reset|#P1 Up|P1 Down|P1 Left|P1 Right|P1 Start|P1 Select|P1 B|P1 A|#P2 Up|...|P2 A|
|..|........|........|
...
[/Input]
```

Rules:

- **Frame count = number of lines starting with `|`** (TASVideos spec; the
  `[Input]` / `[/Input]` / `LogKey:` lines are not frames).
- **LogKey line** (`LogKey:` prefix): button names in column order. `#` marks
  the start of a control group; within a group names are `|`-separated.
  BizHawk documents the LogKey as informational only — but a third-party
  parser MUST use it (BizHawk itself could rely on fixed order; we cannot).
- **Group order** (`ControlsOrdered`: console buttons first, then one group
  per player). For a standard 2-pad NES movie: group 0 = console
  (`Power`, `Reset` — added in exactly that order in `NES.Core.cs`),
  group 1 = P1, group 2 = P2. (The obsolete `NES.NESController` flat list
  interleaves `Reset, Power` between P1 and P2 — never assume position.)
- **Per-controller button order**: `Up, Down, Left, Right, Start, Select, B, A`
  (from `ControllerNES._buttonOrdinals`), renamed to `P1 …` / `P2 …` by the
  deck merger. Famicom P2 lacks Start/Select. FourScore P3/P4, Zapper axes,
  FDS/VS coin columns may appear in other files — skip by name.
- **Cell encoding**: each boolean button is ONE character — `.` (off; a space
  is also treated as off by our FM2 parser) versus **any other char = on**.
  BizHawk writes the mnemonic char (`MnemonicsCache`), derived as:
  strip `P<n> ` prefix, then table lookup; single-char names map to themselves.
  NES write-side mnemonics: `Up→U, Down→D, Left→L, Right→R, Start→S,
  Select→s, B→B, A→A, Reset→r, Power→P`. (Parser must accept ANY non-`.`
  char as on — readability chars vary by version.)
- **Analog/axis columns** (e.g. subframe `Reset Cycle` axis): ONE logical
  column rendered as 5-char space-padded number + comma (6 chars). A parser
  splitting a group segment purely per-char will misalign — identify columns
  via the LogKey (axis names) and consume `.{5},` for those slots.
- **Schematic example** (illustrative, not a dump): P1 holding Right+B, all
  else off: `|..|...RSsBA|........|` — i.e. `|` console(Power,Reset off) `|`
  P1(`...R` + `S` off? no: Start off) … precisely: P1 cells
  `U(.)D(.)L(.)R(R)S(.)s(.)B(B)A(.)` = `...R..BA`, so the line is
  `|..|...R..BA|........|`.
- Pipe characters are logical separators only; a line may carry trailing `|`.

## 4. Savestate-anchored detection (CoreState present)

TASVideos rule: **the existence of a CoreState object is what makes a movie
savestate-anchored. Even if `StartsFromSavestate` was false, that file will
still be checked and loaded if it exists.** (Current `LoadBk2Fields` gates the
`GetCoreState` call on `StartsFromSavestate`, so trust the LUMP, not the flag.)

Contract: a movie is savestate-anchored if the archive contains any of
`Core.bin`, `CoreText.txt`, `CoreState`, `CoreState.bin` (case-insensitive),
**regardless of** the `StartsFromSavestate` flag value. The first `|…|` line
continues from that anchor frame — playing it from power-on is wrong. Until a
savestate loader exists, the parser must REFUSE such files with a named error
(say so in the message), never silently play them. Same for `MovieSaveRam.bin`
+ `StartsFromSaveRam` (refuse or handle explicitly — decide in #98, record here).

## 5. Mapping onto the existing FM2 path

- FM2 grammar in `nes_core/src/movie.rs`: `|cmd|port0(8 flags)|port1(8 flags)||`,
  `0`=off, any other char=on, `R….B.` style — the BK2 per-pad cell semantics
  are IDENTICAL (`new_gamepad_flag_parser`: `v != b'.' && v != b' '`), only the
  column order differs (FM2 fixed `RLDUTSBA`-ish; BK2 named via LogKey).
- Map by LogKey name: `P1 <btn>` → port0 `GamepadInput`, `P2 <btn>` → port1
  (button names → existing `controller::Button`: A/B/Select/Start/Up/Down/Left/Right).
- `Reset` column → `Command.soft_reset` bit (play_movie.rs already advances past
  soft-reset frames); `Power` column → hard-reset/fresh-machine semantics
  (FM2 has no equivalent — `play_movie.rs` currently asserts `!hard_reset`;
  decide in #96/#98).
- Frame lines with extra groups/axes (P3/P4, `Reset Cycle`, FDS/VS coins):
  skip unknown columns, never guess.
- `valid_rom_file` (MD5-vs-`romChecksum`) has a BK2 counterpart in the `SHA1`
  header key — enforcement policy belongs to #96.

## 6. Out of scope (do not build)

Writing `.bk2`; loading `Core.bin`/`CoreText.txt` anchors; lag-counter and
ROM-hash verification; FM2↔BK2 round-trip; TAStudio-only lumps (`LagLog`,
`GreenZone`, `Markers`, …).
