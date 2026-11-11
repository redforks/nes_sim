# Mapper 28 board contract and test28.nes output protocol

Research for #86 (part of map #85). No code changed. Answers: how
`../nes-test-roms/other/test28.nes` reports pass/fail, and what harness
support it needs to run like the existing `just` targets — plus the board
contract a future `nes_core` mapper 28 implementation must match.

Primary sources only: the ROM bytes themselves (disassembled with a throwaway
6502 script, not committed), `nes_cpu_test/src/` + `justfile`, Mesen2's
`Action53.h`, the wiki mirror. Per-claim locations below.

## 1. ROM identity

- File: `../nes-test-roms/other/test28.nes`, 524304 bytes,
  md5 `cf7fc7f37fe035a91fd75a1121b17068`,
  sha1 `c4a2fb612298548b527c26720932a1c291bdf05f`.
- iNES header `4E 45 53 1A 20 00 C0 10`: PRG `0x20` = 32x16 KiB = 512 KiB;
  CHR `0x00` = no CHR ROM (emulator supplies CHR RAM, 8 KiB by default —
  `nes_core/src/nes/mapper/memory.rs:84-96`); flags6 `0xC0` / flags7 `0x10`
  = mapper `(0x1 << 4) | 0xC` = **28**, iNES (not NES 2.0), no battery, no
  trainer, nametable-arrangement bit 0.
- Vectors (last-bank tail): NMI=`$C200`, RST=`$C204`, IRQ=`$C203`.
- Banner strings (fixed bank 31, file offsets): `INL-ROM Action 53 Mapper /
  Interactive behavior test / Copr. 2012 Damian Yerrick` at bank31+`$14EA`,
  `Mode $xx   Outer bank $xx` template at bank31+`$1548`.
- The wiki mirror corroborates identity only:
  `nesdev-wiki/wikipages/Action_53_mapper.xhtml` links "test28, a
  comprehensive test ROM". Tetanes has no mapper-28 reference
  (`../tetanes/tetanes-core/src/mapper/` lists `m000`–`m079`, no `m028`).

## 2. Output protocol: nametable tall-text, NOT the `$6000` protocol

- **No `$6000`-family writes exist in the ROM.** A scan of all 512 KiB of PRG
  for absolute-addressed reads/writes with high byte `$60` finds zero hits
  (the single `$95`-opcode hit is `STA $13,X`, a zeropage false positive).
  So `MonitorTestStatus` (`nes_cpu_test/src/plugin/monitor_test_status.rs`,
  `$6001-$6003` = `DE B0 61` magic, `$6000` = `80/81/00/result`) can **never**
  fire on this ROM. The default `create_ines_machine` plugin set
  (`Console` + `MonitorTestStatus`, `nes_cpu_test/src/image.rs`) cannot
  report it.
- **Verdict channel is nametable text in tall (double-height) tiles**,
  the same encoding `NametableConsole::TallText` already decodes for bntest
  (`nes_cpu_test/src/plugin/nametable_console.rs:decode_tall_char` — pair
  `(c*2, c*2+1)`). Evidence in fixed-bank code:
  - `$C428-$C45D`: print loop writes each byte `ASL A` (even tile, top row)
    then `ROL A` (odd tile, bottom row; carry is set by the preceding
    `CMP #$20`, which is `1` for every printable byte). Bytes `< $20` end
    the row.
  - `$C401-$C427`: NUL-terminated strings, `$0A` row separators (pointer
    advances past the separator, PPU address advances `$40` = one tall row).
  - All message strings are stored as plain ASCII (e.g. `PASS!` at PRG
    bank-offset `$42C`, file `0x43C` in bank 0).
- **Rendering stays on at the verdict screens.** The `$C308` terminal path
  sets `$2000=$80` (NMI on), scroll 0, `$2001=$1E` (BG+OBJ on) via `$C656`,
  then hangs in `JMP $C31D` (verified self-jump). So the
  rendering-enabled gate in `NametableConsole::end` passes; the
  `sampling_without_rendering` quirk (blargg v5) is NOT needed. The alternate
  `$C320` terminal path writes `$2001=$00` (rendering off) — do not bless
  or sample that path with the rendering gate on.
- **NMI handler is `INC $FF; RTI`** (bytes `E6 FF 40` at `$C200`). The
  `$C370` spin (`LDA $FF; CMP $FF; BEQ`) is a frame-sync wait exited by an
  NMI landing between the two reads — not a dead loop.
- **Verdict vocabulary** (current `contains_passed`/`contains_failed`
  match NONE of these — `nametable_console.rs:evaluate_result` looks for
  `PASSED`/`Passed\n`/`FAILED`/`Failed`/`Error `/`Errors: <nonzero>`):
  - `PASS!` / `FAIL!`: per-sub-test verdicts, replicated at the same
    bank offset in outer banks 0-30 (`PASS!` x31 at offset `$42C`;
    `FAIL!` x62 at offsets `$3A1`,`$403`). A naive
    `with_tall_text_magic_success_word("PASS!")` would stop after the
    first passing sub-test and also matches mixed screens (e.g.
    `PASS!` precedes `RESET LEAVES BANK UNCHANGED` in one message) —
    completion needs a final-screen condition, not a first-occurrence word.
  - `Fail!` + detail: 3 fixed-bank screens (bank31+`$0158` PRG-bank-return,
    +`$1320` bus conflict, +`$1387` mirroring).
- **`DetectDeadLoop` must be excluded.** Its `JMP self && $6000 == 0 → exit 0`
  quirk (`detect_dead_loop.rs:end`) fires exit-0 on ANY self-JMP hang here:
  PRG-RAM powers zero (`Bank::empty` = `[0; BANK_SIZE]`,
  `memory.rs:31-33`; LowerRam zero-init, `lower_ram.rs:11-13`) and the ROM
  never writes `$6000`, so a `FAIL!` hang would exit 0. Conversely the
  reset-wait spins would false-fail with exit 1.

## 3. Interactive procedure: exactly one user Reset press, plus a menu

- One prompt, single occurrence (bank31+`$148B`): `One last thing: / Please
  press the Reset / Button on the Control Deck. / (Yeah, like Genesis
  X-Men.)`. An early-reset guard exists per outer bank (`NOT SO FAST! /
  ...don't press Reset until asked.`, 31 copies at offset `$34C`).
- Post-reset discrimination reads consistent with a RAM signature:
  `$C2EC` checks `$FC==$1E`, `$FD==$F0`, `$FE` nonzero/flag. (Inferred
  purpose: tell power-on from Reset-button reset; exact semantics to be
  confirmed by running.)
- The final stage reads the joypad: strobe/read helper at bank31 `$C5E3`
  (`STA $4016` strobe, `LDA $4016/$4017 AND #$03`), called via the `$C5B7`
  wrapper from one site (bank31+`$0380`); bits drive `INC/DEC $1B/$1C`
  (`$C38E-$C3C3`), i.e. a button-adjustable `Mode/Outer bank` menu. So a
  fully unattended run may additionally need `--press` choreography, exact
  buttons TBD by running.
- No `--start-pc` needed: RST vector `$C204` is valid (standard init:
  `SEI`, PPU/APU init, RAM copy loop `$C23E-$C250`, JSRs into RAM at
  `$0400/$040B` so tests execute while banks switch).

## 4. Board contract (for the mapper-28 implementation handoff, not #86)

From `/mnt/ext/code/Mesen2/Core/NES/Mappers/Homebrew/Action53.h` (92 lines,
whole file is the contract):

- Register file: `_regs[4]` + `_selectedReg` + `_mirroringBit`.
- `$5000-$5FFF` writes select: `_selectedReg = ((v & 0x80) >> 6) | (v & 1)`
  (line 80). `$8000+` writes store `_regs[_selectedReg]` and update state
  (lines 81-90); the test ROM's observed writes (`STA $5000` x3,
  `STA $FFF6` x3, `STA $FFF7` x2 in bank 31) fit this select/data split.
- PRG in 16 KiB pages (`GetPrgPageSize 0x4000`), CHR in 8 KiB pages
  (`0x2000`); reset maps last page to slot 1 (`SelectPrgPage(1, -1)`,
  matching the ROM's `DOES NOT POWER ON WITH LAST BANK IN $C000-$FFFF`
  expectation); outer (`_regs[3] << 1`) + inner (`_regs[1]`) bank
  composition per game-size mode (lines 55-75); mirroring from `_regs[2]`
  bits 0-1 with `_mirroringBit` fallback (lines 43-50).
- `nes_core` fit (`nes_core/src/nes/mapper.rs:108-158`,
  `nes_core/CLAUDE.md`): new `Action53`/`M28` mapper type implementing the
  `Mapper` trait (`mapper.rs:287`), bank-window model via
  `CartridgeMemory`, 8 KiB CHR-RAM (`chr_is_ram`), wired into the
  `match mapper_no` dispatch which today panics with
  `Unsupported cartridge mapper no: 28`. That implementation is the
  follow-up handoff, out of scope here.

## 5. Wiring checklist (no code lands in #86)

Prerequisite: mapper 28 implementation (§4) — today the ROM panics at load,
so nothing below is runnable yet.

1. `nes_cpu_test/src/image.rs` `create_ines_machine`: new `test28.nes`
   file-name branch (same dispatch style as `scanline.nes`/`mmc1_a12.nes`):
   `TallText` NametableConsole variant + `Timeout`; OMIT `DetectDeadLoop`
   (§2 false-0/false-1) and OMIT `MonitorTestStatus` (never fires, §2).
2. Verdict condition: extend the console predicates or add a
   completion-aware variant — `PASS!`/`FAIL!`/`Fail!` match nothing today,
   and a bare magic word on `PASS!` stops at the first sub-test. Needs the
   final-screen definition (post-reset screens), confirmable only by running.
3. Scripted Reset-button support (new harness code): `--press` only holds
   joypad buttons (`main.rs:PressAction`); nothing produces
   `ExecuteResult::ShouldReset` for this ROM. Semantics fit the existing
   `MachineWrapper::reset()` — `NesMcu::reset` resets PPU/APU only
   (`nes_core/src/nes.rs:114-119`), preserving mapper registers and RAM
   like a physical Reset button. Suggested form: `--reset-at-frame <n>` or
   a text-watch plugin mapping the `One last thing` screen to a reset
   after N frames.
4. `justfile`: new recipe (e.g. `mapper28`/`test28`) following the existing
   `build_nes_cpu_test` + outer-`timeout` pattern; group under
   `passed_mapper` once green. Timeout budget TBD by run length (15 s outer
   is the family norm).
5. No `--start-pc`, no CHR bless files, no `$6000` wiring. Possible
   `--press` menu choreography for the final stage (§3) — open until runnable.
