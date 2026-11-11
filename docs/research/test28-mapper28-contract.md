# Mapper 28 (Action 53) board contract from test28.nes — research for #87

Research-only. No code changed. Part of map #85 (map itself untouched).

Primary sources, in ticket order: the ROM
(`/home/forks/nes/nes-test-roms/other/test28.nes`, cited by file offset),
the local NESdev wiki mirror (`nesdev-wiki/wikipages/`, cited as
`nesdev-wiki/wikipages/<Page>.xhtml` = `https://www.nesdev.org/wiki/<Page>`),
`../tetanes/tetanes-core/src/mapper/` (negative result: no mapper-28 source),
and `/mnt/ext/code/Mesen2/Core/NES/Mappers/Homebrew/Action53.h`.
Harness fit is assessed against `nes_cpu_test/src/plugin/monitor_test_status.rs`,
`nes_cpu_test/src/main.rs` (`Args`), and `justfile` per-ROM target conventions.

## 1. ROM identity (header, file offsets)

`test28.nes`, 524304 bytes = 16-byte header + 524288-byte PRG. Header bytes
(file `0x00`–`0x0F`):
`4E 45 53 1A 20 00 C0 10 00 00 00 00 00 00 00 00`.

- `0x04 = 0x20`: 32 × 16 KiB PRG = 512 KiB (file `0x10`–`0x8000F`).
- `0x05 = 0x00`: 0 × 8 KiB CHR → CHR-RAM, no CHR ROM.
- Mapper = `(0x10) | (0xC0 >> 4)` = `0x1C` = **28 (Action 53)**.
- Plain iNES (`byte7 & 0x0C == 0x00`), so **no NES 2.0 submapper field**;
  bytes 8–15 are zero (no PRG-RAM size, no TV-system byte). Emulators must
  assume default (submapper 0) iNES mapper-28 behavior.
- No battery, no trainer (size proves it), header mirroring bit = horizontal
  arrangement (irrelevant — mapper controls mirroring).

Bank `N` (0–31) = file `0x10 + N*0x4000`, 512 KiB total = the 32 × 16 KiB
maximum the mapper's outer-bank scheme addresses (cf. §5).

## 2. Vectors and entry points

- Banks 0–30, tail at bank offset `0x3FF8` (CPU `$FFF8` in an `$8000`-mapped
  view; e.g. bank 0 file `0x400A`–`0x400F`): bank-ID byte `NN`, then vectors
  **NMI=`$C11E` RST=`$C000` IRQ=`$C120`**, identical in every bank.
  Common entry at `$C000` (bank offset `0x0000`): `SEI CLD LDX#$00 STX $2000
  STX $2001 DEX TXS BIT $2002 …` (standard PPU init, palette/nametable clear).
- Bank 31, tail at file `0x80008`–`0x8000F`: bank-ID `0x1F`, then
  **NMI=`$C200` RST=`$C204` IRQ=`$C203`**. Entry `$C204` (file `0x07C214`):
  `SEI … STX $2000/$2001/$4010 … BIT $2002/$4015 LDA#$40 STA $4017 …`, then
  RAM self-tests, menu, and the outer-bank/select tests (§4).

## 3. Pass/fail output protocol — NOT the `$6000` protocol

**`MonitorTestStatus` (`nes_cpu_test/src/plugin/monitor_test_status.rs:14-27`)
cannot see this ROM.** It keys on `peek(0x6001)==0xDE &&
peek(0x6002)==0xB0 && peek(0x6003)==0x61` plus a status byte at `0x6000`
(`0x80` running / `0x81` reset-after-100 ms / `0x00` pass / `0x01–0x7F`
fail). The ROM contains **zero aligned accesses to `$6000`–`$7FFF`**
(full-binary search; the few raw byte-triples such as file `0x07C0A8` are
misaligned overlaps like `LDA $2007` + `STA $0160,X`). It never writes
`$6000`, never `BRK`s.

The actual protocol is **on-screen text + infinite-loop halt + a zero-page
signature that must survive a CPU reset**:

- After its bank-31 tests pass it stores the signature **`$FC=$1E $FD=$F0
  $FE=$01`** (bank-31 offset `0x0320`, file `0x07C330`–`0x07C33A`), prints
  outer-bank/select status text, enables NMI, then spins on `JMP $C31D`
  (file `0x07C32D`) **waiting for the user to press RESET**.
- The bank-0 `$C000` entry re-checks the signature (file offsets; bank offset
  = file offset − `0x10`):
  - `$FC != $1E` or `$FD != $F0` (`LDA $FC / CMP#$1E / BNE →` file `0xC4`;
    `LDA $FD / CMP#$F0 / BNE →` file `0xC4`): prints **`FAIL!\nDOES NOT
    POWER ON WITH\nLAST BANK IN $C000-$FFFF.\nPLEASE TURN THE POWER OFF.`**
    (string at file `0x3B1`, CPU `$C3A1`). I.e. power-on must map bank 31
    (the last 16 KiB) at `$C000`–`$FFFF`.
  - Else if `$FE != 0` (`LDA $FE / BEQ →` file `0xCB`, fall-through prints):
    prints **`NOT SO FAST!\nPLEASE TURN THE POWER OFF,\nAND NEXT TIME DON'T
    PRESS\nRESET UNTIL ASKED.`** (file `0x35C`, CPU `$C34C`).
  - Else (`$FE == 0`): checks `LDA $FFF8` (`BEQ →` PASS): zero (reset
    preserved the bank) prints **`PASS!\nRESET LEAVES BANK
    UNCHANGED.\nPLEASE TURN THE POWER OFF.`** (file `0x43C`, CPU `$C42C`);
    nonzero prints **`FAIL!\nCPU RESET CHANGES BANK.\nPLEASE TURN THE POWER
    OFF.`** (file `0x403`, CPU `$C3F3`).
  - Print helper at file `0xDE`/`0xFE`: pointer `$02/$03`, `$0A` = newline
    (`ADC#$20`), `$00` terminator = return; each verdict ends with rendering
    on + self-`JMP` halt (e.g. `JMP $C0EB`, file `0xFB`).
  - (Interpretation note: the `$FE != 0` → NOT-SO-FAST arm is byte-exact as
    decoded, but its intended UX trigger — reset pressed at the wrong time
    vs. a mapper that corrupts state — should be confirmed against a
    passing run during implementation. The contract-relevant arms —
    power-on-last-bank and reset-preserves-bank — are unambiguous, and both
    agree with the wiki, §5.)
- Fixed strings also present in every bank 0–30 (same bank offset): `HELLO
  WORLD` (file `0x350`), plus a font blob at file `0x300`–`0x33F`.

## 4. Mapper-28 features the ROM exercises

Aligned-disassembly search over the whole binary:

- **Register select `$5000`** (8 sites): `STA $5000` with `#$00/$80/$81`
  (RAM `$0402/$0455/$045D`, files `0x07C012/65/6D`), `STX $5000` (RAM
  `$040D/$0417`), `STY $5000` with `Y=$80/$81` (RAM `$04C5/$04CC`). It never
  touches `$5001`–`$5003` — selection is via `$5000` + `$8000`–`$FFFF`
  writes, per the wiki (§5).
- **Inner PRG**: `STX/STY/LDA $8000` sequencing; `LDA/EOR $FFF6` complement
  test (RAM `$0410`–`$043F`); `STA $FFF7` twice (RAM `$04C1` block);
  `$FFF9` presence test (`BNE` → error path, RAM `$0442/$04D2`); inner index
  `X=$1F…0` via `STX $FFF8` with readback `LDA $BFF8 → $0100,X` vs `LDA
  $FFF8 → $0120,X` (RAM `$046B`–`$0474`).
- **Mirroring**: loop RAM `$046B`–`$04BE` (file `0x07C07B`–`0xCB`) sets
  `STY $5000` = `$01`, then reads PPU `$2007` after `$2006` =
  `$2000/$2400/$2800/$2C00` double-reads into `$0140/$0160/$0180/$01A0,X` —
  detects nametable mirroring from the outer/inner select.
- **Power-on fixed bank**: last 16 KiB at `$C000`–`$FFFF` (FAIL text, §3).
- **Not tested**: no PRG-RAM (`$6000`) enable test, no IRQ test beyond
  `SEI` + `STA $4010/$4017` disable and `INC $FF / RTI` handlers.

## 5. Board contract (wiki + Mesen2)

From `nesdev-wiki/wikipages/Action_53_mapper.xhtml`
(https://www.nesdev.org/wiki/Action_53_mapper);
`INES_Mapper_028.xhtml` just redirects there. Corroborated by
`/mnt/ext/code/Mesen2/Core/NES/Mappers/Homebrew/Action53.h`
(`WriteRegister`/`UpdateState` quoted below as Mesen behavior).

- **Registers**: `$5000`–`$5FFF` = register select; `$8000`–`$FFFF` =
  register value. **No bus conflicts.** (`$5000` value: bit 7 + bit 0 form
  the select; Mesen: `_selectedReg = ((value & 0x80) >> 6) | (value &
  0x01)`, i.e. `$00→regs[0]`, `$01→regs[1]`, `$80→regs[2]`, `$81→regs[3]`.)
- **`$00` CHR bank** (Mesen `regs[0]`, 8 KiB CHR window, `SelectChrPage(0,
  chrSelect)` with `chrSelect = regs[0] & 0x03`): bits 1–0 set CHR-RAM
  A14–A13; D4 feeds mirroring bit 0 when mirroring mode is 0/1 (AxROM
  simulation); ignored in modes 2/3.
- **`$01` inner bank** (Mesen `regs[1]`, `prgSelect = regs[1] & 0x0F`):
  bits 3–0 = current PRG bank within the outer window; D4 likewise feeds
  mirroring bit 0 in 1-screen modes.
- **`$80` mode** (Mesen `regs[2]`): bits 7–6 = PRG outer-bank size (0 = 32
  KiB … 3 = 256 KiB; address lines A15+/A16+/A17+/A18+ from outer bank),
  bits 5–4 = PRG bank mode, bits 1–0 = mirroring (0 = 1-screen lower, 1 =
  1-screen upper, 2 = vertical / PPU A10, 3 = horizontal / PPU A11). While
  mode is 0/1, bit 0 is writable three ways: `$80` bit 0, `$00` D4, `$01`
  D4. PRG modes: 0/1 = BNROM/AOROM 32 KiB at `$8000`–`$FFFF`; 2 = UNROM#180
  (fixed bottom half of outer bank at `$8000`–`$BFFF`, current at
  `$C000`–`$FFFF`); 3 = UNROM#2 (current at `$8000`–`$BFFF`, fixed top half
  at `$C000`–`$FFFF`). Fixed-bank access forces 32 KiB outer size so all
  outer bits pass through (e.g. mode-3 fixed `$C000` can be 16 KiB bank 7,
  1, 3, or 5). Full 12-combination o/i (outer/inner bit) table is on the
  wiki page; Mesen implements it via the `gameSize/prgSize/slotSelect`
  masks in `UpdateState`.
- **`$81` outer bank** (Mesen `regs[3]`, `outerPrgSelect = regs[3] << 1`):
  bits 7–3 always drive PRG A22–A18; bits 2–0 drive A17–A15 only when the
  outer size needs them (larger sizes ignore low bits). Many implementations
  decode only the low 4–6 bits (2 MiB practical iNES limit).
- **Mirroring detail (Mesen `UpdateState`)**: `mirroring = regs[2] & 0x03`;
  if bit 1 clear, `mirroring = _mirroringBit`, where a `$8000`+ write sets
  `_mirroringBit = (value >> 4) & 1` when select ≤ 1, else `(value & 1)`
  when select == 2. So the 1-screen bank bit also rides on `$8000`+ writes.
- **Power-up**: last 16 KiB mapped at `$C000`–`$FFFF`; **all other state
  unspecified**. **Reset leaves mapper state unchanged.** Non-normative
  hint: `$81=$FF`, `$80=$02` gives oversize-BNROM mode in the last bank.
- **Capabilities**: PRG up to 8 MiB (512 KiB/256 KiB inner windows), 32 KiB
  PRG window(s), no PRG-RAM (optional WRAM enable is a hardware-build
  option only), CHR 32 KiB max / 8 KiB window, H/V/1-screen mirroring,
  **no IRQ, no audio**. Wiki notes a `$4444` write seen in traces is a
  menu-program debugging artifact with no mapper function.
- **tetanes**: `../tetanes/tetanes-core/src/mapper/` (21 files) contains no
  mapper-28/Action53 source, so it cannot corroborate; Mesen2 is the sole
  reference implementation consulted.

## 6. What `nes_core` must satisfy (contract checklist)

1. Decode iNES mapper 28 → Action 53; 512 KiB PRG / CHR-RAM accepted.
2. Power-on: bank 31 (last 16 KiB) at `$C000`–`$FFFF`; reset must not alter
   any of regs `$00/$01/$80/$81`, the select latch, or the mirroring bit.
3. `$5000` select + `$8000`–`$FFFF` value writes per §5, no bus conflicts;
   all three mirroring-bit-0 write paths (`$80` b0, `$00`/`$01` D4 in modes
   0/1, `$8000`+ writes per Mesen's `_mirroringBit` rule).
4. PRG mode/size matrix (12 combos) with fixed-bank 32 KiB forcing;
   CHR-RAM 8 KiB window from `$00` bits 1–0; H/V/1-screen nametable control.
5. No `$6000` PRG-RAM, no IRQ source from this mapper.

## 7. Harness fit + wiring checklist (no code landed)

- `MonitorTestStatus` is a non-starter for test28: wrong protocol (§3).
  The existing `0x81 → ShouldReset` CPU-reset path
  (`monitor_test_status.rs:53-72`) fires only on `$6000=$81`, which this ROM
  never writes.
- `justfile` per-ROM convention is `{{ nes_cpu_test }} --quiet -f
  ../nes-test-roms/<suite>/<rom>.nes` (e.g. `mmc1-a12:`, `bntest-*:` at
  `justfile:443-449`), with variants adding `--start-pc`, `--press
  <btn>@<frame>`, `timeout N`, `--dump-frame N --dump-out` + `cmp` against
  blessed PNGs in `nes_cpu_test/src/png-exps/` (e.g. `mmc5-basics-f30`
  targets at `justfile:711-733`), and `--frames`/`--dump-audio` flows
  (`main.rs:29-50`).
- Gaps blocking a `just test28` target today:
  1. `--press` accepts controller buttons only (`a/b/select/start/up/down/
     left/right`; `main.rs:141-150`) — there is **no reset injection**,
     and test28's protocol requires a physical RESET mid-run (§3).
  2. No text-output assertion: verdicts are PPU-rendered strings, so
     automated pass/fail needs either a new plugin (nametable-text or
     `$FC/$FD/$FE` + `$FFF8` signature check after reset) or the
     `--dump-frame` + blessed-PNG `cmp` pattern already used for MMC5
     (`justfile:717-733`) applied to the PASS screen.
- Suggested wiring (for the implementing ticket, not this one): add a
  `--press-reset@<frame>`-style flag reusing the `ExecuteResult::ShouldReset`
  path, plus a `just test28` recipe that runs bank-31 → reset → bank-0 and
  asserts via `--dump-frame` PNG `cmp` (or a signature-check plugin);
  bless expected PNG(s) under `nes_cpu_test/src/png-exps/`. `nes_core`
  mapper modules live in `nes_core/src/nes/mapper/` (`axrom.rs`, `bxrom.rs`,
  …) — the natural home for the new `mapper28`/`action53` module.
