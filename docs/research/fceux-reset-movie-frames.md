# FCEUX movie reset/power entries: which frame, which index step?

Research-only. No source or test code changed. Issue #182 (part of #181:
"movie reset entry skips the next input entry").

Primary source: FCEUX tree at `/home/forks/nes/fceux` (HEAD `1e1168d`). All
`file:line` citations below refer to that tree unless prefixed `nes_sim/`.
Comparison code is in this repo (`nescli/`, `nes_core/`). Everything was
established by reading code; **nothing was executed**. Uncertainty is marked
explicitly.

## 0. Format: where reset and power live in a movie

- `src/movie.h:80-81` — `MOVIECMD_RESET = 1`, `MOVIECMD_POWER = 2`
  (other bits: FDS insert/select 4/8, VS coin 16/32, VS service 64; `:82-86`).
- `src/movie.h:130-132` — `uint8 commands;` with `command_reset()` /
  `command_power()` predicates over those bits.
- `src/movie.cpp:260-265` — `MovieRecord::parse`: `commands =
  uint32DecFromIstream(is);` reads the decimal field between the first and
  second `|` of a frame line; the controller fields follow in the same line.
  So the command is **stored on the same record as that frame's pad inputs**.
- `src/movie.cpp:363-366` — `MovieRecord::dump` writes it back the same way
  (`putdec<uint8,3,false>(os, commands)`).
- FM2 example: `|1|........|........||` is a soft reset record; `|2|` is power;
  `|3|` is both (each is a plain decimal bit field).
- **BK2 is not parsed by FCEUX.** A grep of `src/` for `bk2`, `BizHawk`,
  `Input Log` finds nothing relevant. Any statement about the BK2 `Reset`/`Power`
  columns in FCEUX is therefore an analogy through the same per-record command
  bits, not a verified mapping (see section 7).

## 1. Playback path: one call per emulated frame

1. `src/fceu.cpp:745` — `FCEUI_Emulate(...)`, the per-frame entry point. Paused
   frames return early at `src/fceu.cpp:817-826` **before** input is polled, so
   a paused frame does not consume a movie record.
2. `src/fceu.cpp:837` — `FCEU_UpdateInput();` (once per emulated frame).
3. `src/input.cpp:442-468` — `FCEU_UpdateInput` polls drivers (skipped in
   PLAY mode, `:445-451`) and then calls `FCEUMOV_AddInputState();` at
   `src/input.cpp:468`.
4. `src/movie.cpp:1195` — `void FCEUMOV_AddInputState()`. The PLAY branch is
   `src/movie.cpp:1234`:
   - `:1237-1239` — past the last record: `FinishPlayback()`; nothing is applied.
   - `:1246` — `MovieRecord* mr = &currMovieData.records[currFrameCounter];`
     (the record is selected by the **movie frame counter**, not by a separate
     index).
   - `:1248-1252` — `if(mr->command_power()) PowerNES(); if(mr->command_reset())
     ResetNES();`
   - `:1253-1262` — FDS and VS commands (not relevant to NES reset).
   - `:1264-1265` — `joyports[0].load(mr); joyports[1].load(mr);`
   - `src/movie.cpp:1318` — `currFrameCounter++;` (end of `FCEUMOV_AddInputState`,
     reached once per call).

## 2. Which frame does a reset apply to?

**The frame whose record carries the flag, i.e. the same frame as its pad
inputs.** There is no look-ahead or deferred application.

- `src/movie.cpp:1246-1252` — the reset/power commands are read from the
  record at `currFrameCounter`, before that frame is emulated.
- `src/fceu.cpp:919-931` — `ResetNES()` calls `FCEUMOV_AddCommand(FCEUNPCMD_RESET)`
  (a no-op in PLAY mode, see below), `GameInterface(GI_RESETM2)`,
  `FCEUSND_Reset()`, `FCEUPPU_Reset()`, `X6502_Reset()`, and clears the back
  buffer. It does not call `FCEUMOV_AddInputState` or `FCEU_UpdateInput`, and does
  not change `currFrameCounter`.
- `src/movie.cpp:1326-1330` — `FCEUMOV_AddCommand` returns immediately unless
  `movieMode` is RECORD or TASEDITOR, so in PLAY mode the reset is not re-recorded.
- `src/x6502.cpp:516-519` — `X6502_Reset()` only sets `_IRQlow=FCEU_IQRESET`.
  The reset vector is fetched later, inside the CPU run loop:
  `src/x6502.cpp:573-580` (`_PC=RdMem(0xFFFC); _PC|=RdMem(0xFFFD)<<8;`).
  That loop is driven per scanline from `src/ppu.cpp:1314-1328` (`DoLine`,
  `X6502_Run(256 + 69)` etc.), i.e. during the emulation of the same frame.
  - Ordering consequence: the reset is latched before `FCEUPPU_Loop` runs the
    frame, and the frame's pad state was already loaded at `:1264-1265`.

## 3. Do that frame's port inputs still apply?

**Yes, in the plain PLAY path.** Pad state is loaded after the reset/power
calls, and reset does not clear it.

- `src/movie.cpp:1264-1265` — `joyports[n].load(mr)` runs after
  `PowerNES()`/`ResetNES()` (`:1249-1252`).
- `src/input.h:87` — `load(MovieRecord* mr) { driver->Load(w,mr); }`.
- `src/input.cpp:293-299` — `LoadGP`: `joy[0] = mr->joysticks[0];` (port 1
  pad state lands in the global `joy[]`).
- `src/input.cpp:309-320` — `ReadGP` reads `joy[w]` when the game strobes
  `$4016/$4017` during the frame, so the loaded bits are what the game sees.
- `ResetNES` does not touch `joy[]` (`src/fceu.cpp:919-931`).

**Power caveat (not reachable from plain PLAY):**
- `src/fceu.cpp:1016-1075` — `PowerNES()` calls `InitializeInput();` at
  `src/fceu.cpp:1046`.
- `src/input.cpp:647-650` — `InitializeInput` does `memset(joy,0,sizeof(joy));`.
- In the plain PLAY branch, power is applied **before** the pad load
  (`movie.cpp:1249-1250` then `:1264-1265`), so the frame's inputs survive.
- In the TAS Editor branch (`src/movie.cpp:1198-1231`, compiled only under
  `__WIN_DRIVER__`/`__QT_DRIVER__`, `:1197`), the pad load happens **first**
  (`:1214-1215`) and power second (`:1217-1218`). A Power record there would
  zero `joy[]` after loading, so that frame's pad input would read as 0.
  **Uncertainty:** this is read from code only; not executed. It applies only
  when `movieMode == MOVIEMODE_TASEDITOR`, not to `MOVIEMODE_PLAY`.

## 4. Does the movie index advance once or twice per reset entry?

**Once.** A reset entry is one `MovieRecord` and one call to
`FCEUMOV_AddInputState`, which increments `currFrameCounter` exactly once
(`src/movie.cpp:1318`). Neither `ResetNES` (`src/fceu.cpp:919-931`) nor
`PowerNES` (`src/fceu.cpp:1016-1075`) calls `FCEUMOV_AddInputState`,
`FCEU_UpdateInput` or `FCEUI_Emulate`, and neither writes `currFrameCounter`.
The only increment in the PLAY path is `movie.cpp:1318`. (The `currFrameCounter`
writers are `fceu.cpp:233`, `input.cpp:784`, `movie.cpp:1108`, `:1155`,
`:1318`, and `:1941`; none run inside a power/reset command during playback.)

## 5. How does hard reset (power) affect frame counter and movie index?

- **Playback:** `PowerNES()` does not touch `currFrameCounter` or the record
  index (`src/fceu.cpp:1016-1075`; no writes to `currFrameCounter` in that
  range). Power is an in-band command on the current record, like reset.
- `PowerNES()` does reset emulated hardware state: `src/fceu.cpp:1046`
  `InitializeInput()`, `:1047-1048` `FCEUSND_Power`/`FCEUPPU_Power`, `:1061`
  `X6502_Power()`, `:1066` `LagCounterReset()`. `X6502_Power` clears CPU
  counters and calls `X6502_Reset()` (`src/x6502.cpp:548-555`).
- RAM re-seeding only happens when the movie is inactive
  (`src/fceu.cpp:1022-1024`); in playback, RAM init comes from the movie seed.
- `src/input.cpp:780-786` — `ResetFrameCounter()` zeroes `currFrameCounter`
  only when `movieMode == MOVIEMODE_INACTIVE`. It is called only from the
  **UI** commands `FCEUI_ResetNES` (`src/input.cpp:787-796`) and
  `FCEUI_PowerNES` (`src/input.cpp:799-807`), not from the movie replay path.
- Movie start: `FCEUI_LoadMovie` calls `poweron(true)` (`src/movie.cpp:1076`,
  body at `:905-931`) and sets `currFrameCounter = 0` (`src/movie.cpp:1108`).
  Record 0 is then processed like any other frame.
- Movie restart from the beginning: `src/movie.cpp:1940-1941` calls `poweron(true)`
  then `currFrameCounter = 0`.

## 6. Recording side (for completeness)

- `src/input.cpp:719-728` — `FCEU_QSimpleCommand` runs the command and, in
  RECORD/TASEDITOR mode, calls `FCEUMOV_AddCommand` (`src/input.cpp:727-728`).
- `src/movie.cpp:1326-1346` stores it in `_currCommand`, which is OR-ed into
  the **next** recorded frame (`src/movie.cpp:1290-1291`). So a reset issued
  between frames lands on the frame emulated next, the same frame whose
  inputs are logged.

## 7. Answers

1. **Which frame does reset apply to?** The frame whose record carries the flag
   (`movie.cpp:1246-1252`). Reset is not deferred to the next record.
2. **Do that frame's port inputs still apply?** Yes in PLAY (`movie.cpp:1264-1265`
   after `:1249-1252`; reset does not clear `joy[]`). Power is different only in
   the TAS Editor branch (`movie.cpp:1214-1218`, `input.cpp:650`), which is
   unverified at runtime and not PLAY.
3. **Does the movie index advance once or twice per reset entry?** Once
   (`movie.cpp:1318` is the only increment in the path; reset/power do not call
   `FCEUMOV_AddInputState` or `FCEUI_Emulate`).
4. **How does hard reset affect frame counter and movie index?** Playback power
   does not change `currFrameCounter` (section 5). It is an in-band command that
   resets hardware state (`fceu.cpp:1016-1075`). The counter is reset only by
   UI power/reset when no movie is active (`input.cpp:780-807`) and at movie
   open/restart (`movie.cpp:1108`, `:1940-1941`).
5. **BK2 Reset/Power columns?** Not answered by FCEUX source (no BK2 reader).
   By analogy through the same per-record command bits (`movie.h:80-81`), a BK2
   Reset or Power column would be one record's command, consumed as in section 1.
   **Unverified.**

## 8. Implication for nescli

Our code (`nescli/src/sdl/event_loop.rs`):

- `:242` — `source.current_frame()` snapshots `logs[index]`
  (`:46-56`), so the inputs are taken from the entry at the current index.
- `:248-253` — in the reset branch: `source.advance();` (`:249`) then
  `machine.hard_reset();` (`:250`), or `source.advance();` (`:252`) then
  `machine.soft_reset();` (`:253`). The inputs are applied afterwards at
  `:255-257` (`set_controller_a_from_gamepad` / `_b_` / `render.set_input`).
- `:328` — `source.advance();` unconditionally at the end of the loop body.

FCEUX evidence supports:
- **One movie record = one emulated frame = one index step** (`movie.cpp:1318`).
  A reset/power entry is not a separate step. Our reset branch's `advance()` at
  `:249`/`:252` plus the unconditional `:328` gives **two** steps for one
  entry, so entry i+1 is skipped. This is consistent with issue #182.
- Reset/power do not defer to a later record, and the pad state of the same
  record applies in that frame. Our ordering (reset, then controller set from
  the owned snapshot) matches FCEUX's ordering in the PLAY path.

FCEUX evidence does **not** settle:
- Whether the fix should delete the `:249`/`:252` advances or the `:328`
  advance. The FCEUX code supports exactly one advance per entry; which line
  to keep is a design choice for nescli.
- Whether FCEUX's frame-level reset timing (reset vector fetched inside the
  frame's CPU loop, `x6502.cpp:573-580`) matches nescli's `hard_reset` /
  `soft_reset` timing. Only the ordering of the input/reset calls is compared
  here, not cycle timing.
- The BK2 fixture (`testdata/bk2/bk2-reset-power-3f.bk2`) is not an FCEUX
  input, so FCEUX says nothing about its column semantics.

Side observation (nes_core, not in #182 scope):
- `nes_core/src/nes/ppu.rs:372` — `hard_reset` path resets `self.timing =
  Timing::new();`, and `Timing::new` sets `frame_no: 0` (`ppu.rs:147`). So
  `machine.frame_no()` (`nes_core/src/nes_machine.rs:76-78`) returns to 0 on a
  hard reset. FCEUX's `currFrameCounter` does not reset on playback power.
  `nescli`'s `trim_after` check (`event_loop.rs` `machine.frame_no() >=
  opts.trim_after`) therefore counts differently from FCEUX's movie index after
  a hard reset. Uncertainty: not tested.

## 9. Uncertainty summary

- Code reading only; no FCEUX build or run.
- BK2 semantics are not in FCEUX; the BK2 answer is by analogy only.
- TAS Editor power-ordering quirk (section 3) is read from code and applies to
  `MOVIEMODE_TASEDITOR`, not plain PLAY. Not executed.
- Only the SDL and Qt drivers were spot-checked to call `FCEUI_Emulate` once per
  frame (`src/drivers/sdl/sdl.cpp:305`, `src/drivers/Qt/fceuWrapper.cpp:1365`).
  The Win driver was not checked.
- `X6502_Run` is called from `DoLine` (`src/ppu.cpp:1314-1328`); I did not trace
  the full `DoFun`/frame loop (`src/ppu.cpp:286`) to confirm every scanline
  boundary, so the claim "reset fetch happens inside the same frame" is
  inferred from the call structure, not traced end to end.
