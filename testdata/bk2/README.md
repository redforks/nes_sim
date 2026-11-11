# BK2 parser fixtures (synthetic, power-on)

Three tiny NES BK2 movies for parser tests. All ROM identity fields are
**illustrative/synthetic** — parser tests must not assert ROM matching
against them (digest checks are covered by warn-path tests with synthetic
ROMs, not by these files).

Shared LogKey (all files):

```text
LogKey:#Power|Reset|#P1 Up|P1 Down|P1 Left|P1 Right|P1 Start|P1 Select|P1 B|P1 A|#P2 Up|P2 Down|P2 Left|P2 Right|P2 Start|P2 Select|P2 B|P2 A|
```

Every frame line has console/pad segment widths 2/8/8. `.` means off, any
other character means on. Console group order is Power then Reset
(mnemonics `P` / `r`).

## Files

- `bk2-blank-3f.bk2` — Header.txt + Input Log.txt only, 3 all-off frames.
  Covers container layout, `|`-line frame counting, absent optional lumps
  tolerated.
- `bk2-inputs-5f.bk2` — + SyncSettings.json, Comments.txt, Subtitles.txt;
  5 frames: blank, P1 Right+B, P1 Up+A with P2 Left+Start, both-pads-all-on,
  blank. The `SHA1` header key holds a CRC32-length value (`DEADBEEF`) to
  pin any-digest-length acceptance. Covers named LogKey mapping, mnemonic
  on-chars, port1 mapping, optional lumps present.
- `bk2-reset-power-3f.bk2` — 3 frames: blank, Reset (`r`), Power (`P`).
  Covers console-group columns.

## Anchored case (recipe, not a file)

All three are power-on movies (no CoreState lump). To test the
savestate-anchor refusal, synthesize the anchored variant in code: take the
blank fixture bytes and add a `CoreState.bin` lump (any bytes) — the parser
must hit the named anchored refusal regardless of the
`StartsFromSavestate` flag value.
