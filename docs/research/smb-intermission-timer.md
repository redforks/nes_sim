# SMB WORLD 1-1 intermission timer (issue #201)

Scope: Super Mario Bros. (World) NROM ROM (`/home/forks/nes/Super Mario Bros. (World).nes`, md5 `f94bb9bb55f325d9af8a0fff80b9376d`, mapper 0, 2×16 KB PRG, 1×8 KB CHR).

## Answer

| Item | Value |
|---|---|
| Intermission timer RAM byte | `$07A0` (`ScreenTimer`) |
| Decrement site (PRG) | `$8113` `DEC $0780,X` with X=`$20` in the NMI timer loop `DecTimersLoop` at `$810E` |
| Decrement gate | only on the interval-timer path (see semantics) |
| Load site | `ResetScreenTimer` at `$88A5` (`LDA #$07` / `STA $07A0`), called from `DisplayIntermediate` (`OutputInter`) and from `ResetSpritesAndScreenTimer` |
| Wait loop | `ResetSpritesAndScreenTimer` at `$889D` (`LDA $07A0` / `BNE NoReset`) |

Routine disassembly, NMI handler (ROM bytes verified against `smbdis.asm`):

```text
8100  A2 14      LDX #$14            ; frame-only timers: X = $14..$00
8102  CE 7F 07   DEC $077F          ; IntervalTimerControl
8105  10 07      BPL $810E           ; not expired -> frame timers only
8107  A9 14      LDA #$14
8109  8D 7F 07   STA $077F          ; reload interval counter (20)
810C  A2 23      LDX #$23            ; interval expired: X = $23..$00 (includes $20 = $07A0)
810E  BD 80 07   LDA $0780,X         ; DecTimersLoop
8111  F0 03      BEQ $8116           ; skip if already 0
8113  DE 80 07   DEC $0780,X         ; decrement Timers[X]; X=$20 -> $07A0 ScreenTimer
8116  CA         DEX
8117  10 F5      BPL $810E
8119  E6 09      INC $09             ; FrameCounter (NoDecTimers)
```

Entry gate (before `$8100`): `LDA $0747` (TimerControl) / `BEQ` to `$8100`; otherwise `DEC $0747` and skip (`$80F6`–`$80FD`). NMI entry is `$8082` (NMI vector at `$FFFA` = `$8082`, `LDA $0778`).

Timer table: `Timers = $0780`; `ScreenTimer = $07A0` is index `$20` (= 32) of that table.

## Semantics (per-frame countdown)

- `$07A0` is NOT decremented every frame. It lives in the second half of the `Timers` table (indices `$15`–`$23`), which the NMI loop reaches only when `IntervalTimerControl` ($077F) underflows.
- `IntervalTimerControl` is decremented every NMI (when `TimerControl`=0 and not paused). It counts 20,19,…,0 (bpl taken) and underflows to -1 on the 21st NMI, then reloads to 20 and the interval timers (including `$07A0`) tick once. So `$07A0` ticks once every **21 frames**.
- Intermission flow (`ScreenRoutineTask` $073C): task 6 `DisplayIntermediate` writes the text and calls `ResetScreenTimer` (`$07A0` = 7, task -> 7). Task 7 `ResetSpritesAndScreenTimer` waits while `$07A0` != 0; when it reads 0 it runs `ResetScreenTimer` (`$07A0` = 7, task -> 8) and the level begins.
- Therefore intermission length = k + 6 x 21 frames, where k = NMIs from the load of `$07A0` until the first interval underflow (1 <= k <= 21, set by the phase of `$077F` at load time). Length 127 = k=1; length 147 = 7 x 21 = k=21.
- fceux (this ROM, movie) gives k=1 -> 127 frames. The 147-frame behaviour in our emulator is consistent with k=21, i.e. our `$077F` phase is one NMI behind fceux. This is an inference from arithmetic; I did not instrument our emulator to confirm the phase.

## Trace (fceux 2.6.6, headless, movie `happylee-supermariobros,warped.fm2`)

Lua loop index `n` (≈ frames since script start; Start pressed around n=43 when `$0770` OperMode becomes 1). Full trace: `/tmp/research-smb/trace.txt` (scratch, not in repo). Rows are the state at the start of frame `n`.

| n | `$0772`/`$073C` (task/screen) | `$07A0` ScreenTimer | `$077F` IntervalCtl | note |
|---|---|---|---|---|
| 50 | 01 / 06 | 00 | 01 | DisplayIntermediate about to run |
| 51 | 01 / 07 | 07 | 00 | `$07A0` loaded with 7; underflow in NMI of frame 51 |
| 52 | 01 / 07 | 06 | 14 | first tick (k=1) |
| 73 | 01 / 07 | 05 | 14 | tick, +21 |
| 115 | 01 / 07 | 03 | 14 | |
| 157 | 01 / 07 | 01 | 14 | |
| 177 | 01 / 07 | 01 | 00 | last frame in task 7 |
| 178 | 01 / 08 | 07 | 14 | `$07A0` read 0 -> reloaded 7, task 8 (level starts) |

Intermission = 178 - 51 = **127 frames** in fceux. Only interval ticks at n = 52, 73, 94, 115, 136, 157, 178 (7 ticks) occur, spaced 21 frames apart.

## Sources

- `/home/forks/nes/nes-test-roms/other/smbdis.asm`
  - Defines: `IntervalTimerControl = $077f` (l.79), `Timers = $0780` (l.80), `ScreenTimer = $07a0` (l.101).
  - NMI timer block: l.783–799 (`lda TimerControl` l.785, `dec IntervalTimerControl` l.790, `sta IntervalTimerControl` l.793, `DecTimersLoop` l.795–799, `NoDecTimers` l.800).
  - Screen task table: `ScreenRoutines` l.1362–1380 (6 = `DisplayIntermediate`, 5 and 7 = `ResetSpritesAndScreenTimer`).
  - `DisplayIntermediate` l.1540–1560 (`jsr ResetScreenTimer` l.1555).
  - `ResetSpritesAndScreenTimer` / `ResetScreenTimer` l.1779–1787.
  - NMI entry `NonMaskableInterrupt:` l.739; vector table `.dw NonMaskableInterrupt` l.16350.
- ROM bytes (World ROM, file offset = 0x10 + addr - 0x8000): pattern checks matched the disassembly at `$8082` (`AD 78 07`), `$80F6` (`AD 47 07`), `$8102` (`CE 7F 07`), `$810E` (`BD 80 07`), `$8113` (`DE 80 07`), `$889D` (`AD A0 07 D0 0B`), `$88A5` (`A9 07 8D A0 07`). The Japan/USA ROM has the same code bytes at these addresses (`/home/forks/nes/Super Mario Bros. (Japan, USA).nes`); the header byte 7 differs.
- fceux 2.6.6 headless run (`/usr/bin/fceux`), Lua script `/tmp/research-smb/trace.lua` (scratch). Trace output `/tmp/research-smb/trace.txt`.
- Not used: `/home/forks/nes/nesdev-wiki` (absent; `nesdevwiki.zip` has no SMB RAM map page).

## Uncertainty

- Our emulator's 147 vs fceux 127 is explained by the 21-frame interval phase (k), but I did not trace our emulator to confirm the one-NMI phase offset or its cause (boot RAM state / first NMI timing).
- The `$077F` phase depends on the boot sequence (cold-boot `InitializeMemory` and NMI count since power-on); fceux's trace shows `$077F`=FF at n=0 and first reset at n=10. Not analysed further.
