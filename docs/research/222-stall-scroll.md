# #222: Mario stall at X=$28 and late scroll onset

Parent: #217 (spec), part of #200. No behavior change in nescli/nes_core.
No new probe added in this ticket; all numbers come from existing traces
and RGB (list in Sources). No `[DEBUG-` tag present
(`grep -rn "DEBUG-" nes_core nescli` prints nothing). Note for #225:
this ticket added no temporary probe, so there is nothing to revert.

## Sources

- RAM traces: `/tmp/nesdump/c213/nes213.txt` (`[DEBUG-213]` in
  `nes_core/src/nes_machine.rs` `process_frame`, sl-240 cut; reverted in
  #213), `/tmp/nesdump/c213/fc213.txt`
  (`/tmp/nesdump/c213/fceux_213.lua`, `memory.readbyte` per frame).
  Columns: `0070 0071 0072 0073 0074 006D 0086 071A 071B 071C 071D 071F`.
- RGB: `/tmp/nesdump/nes_rgb/`, `/tmp/nesdump/fc_rgb/` (baseline.sh
  pipeline: nescli `--trim-after 600` + fceux `fceux_dump.lua` +
  `convert_gd.py`; same ROM/movie/palette as #203). Same-index exact
  count for this window rechecked here; 158/600 headline from
  `/tmp/smoke/base600/per_frame.tsv`.
- Prior results: #213 (bg drift follows walk, 0 px diff held-equal),
  #214 (latch not a cause), #205 (press lost state-timed), #203/#219
  (intermission 147 vs 127, phase not cut).

Inputs throughout: `Super Mario Bros. (World).nes`, movie
`happylee-supermariobros,warped.fm2`, `fceux-default.pal`, 600 frames.

## 1. Per-frame table, frames 214-235

X = `$0086` (hex), sc = `$071C` (hex). `same` = same-index RGB exact
compare. `dx` = best horizontal shift of nescli frame n vs fceux frame n
(`c213/scroll2.py`, y 24-240); `m0` = match at dx 0. `bg` = pixel diffs
in x 96-255, y 40-200 (excludes Mario at X <= `$50` and HUD) between
nescli frame n and the fceux frame nearest n-19 with equal X and scroll.

| n | nes X | nes sc | fc X | fc sc | same | dx / m0 | bg (held-equal fc) |
|---|---|---|---|---|---|---|---|
| 214 | 00 | 00 | 32 | 00 | DIFF | +0 / 0.995 | 0 (fc 195) |
| 215 | 28 | 00 | 34 | 00 | DIFF | +0 / 0.992 | 0 (fc 196) |
| 216 | 28 | 00 | 35 | 00 | DIFF | +0 / 0.994 | 0 (fc 197) |
| 217 | 28 | 00 | 37 | 00 | DIFF | +0 / 0.995 | 0 (fc 198) |
| 218 | 28 | 00 | 38 | 00 | DIFF | +0 / 0.994 | 0 (fc 199) |
| 219 | 28 | 00 | 3a | 00 | DIFF | +0 / 0.996 | 0 (fc 200) |
| 220 | 28 | 00 | 3b | 00 | DIFF | +0 / 0.995 | 0 (fc 201) |
| 221 | 28 | 00 | 3d | 00 | DIFF | +0 / 0.993 | 0 (fc 202) |
| 222 | 28 | 00 | 3e | 00 | DIFF | +0 / 0.993 | 0 (fc 202) |
| 223 | 28 | 00 | 40 | 00 | DIFF | +0 / 0.993 | 0 (fc 202) |
| 224 | 28 | 00 | 42 | 00 | DIFF | +0 / 0.993 | 0 (fc 202) |
| 225 | 28 | 00 | 44 | 00 | DIFF | +0 / 0.993 | 0 (fc 202) |
| 226 | 28 | 00 | 46 | 00 | DIFF | +0 / 0.993 | 0 (fc 202) |
| 227 | 29 | 00 | 49 | 00 | DIFF | +0 / 0.993 | 0 (fc 204) |
| 228 | 29 | 00 | 4b | 00 | DIFF | +0 / 0.993 | 0 (fc 204) |
| 229 | 29 | 00 | 4e | 00 | DIFF | +0 / 0.993 | 0 (fc 204) |
| 230 | 2a | 00 | 50 | 00 | DIFF | +0 / 0.993 | 0 (fc 205) |
| 231 | 2a | 00 | 53 | 02 | DIFF | -2 / 0.908 | 0 (fc 205) |
| 232 | 2b | 00 | 55 | 03 | DIFF | -3 / 0.893 | 0 (fc 207) |
| 233 | 2b | 00 | 58 | 05 | DIFF | -5 / 0.887 | 0 (fc 207) |
| 234 | 2c | 00 | 5a | 06 | DIFF | -6 / 0.898 | 0 (fc 208) |
| 235 | 2c | 00 | 5d | 08 | DIFF | -8 / 0.888 | 0 (fc 208) |

Supporting state (constant through the window, both emulators):
`$0070-$0074` = 01, `$006D` = 00, `$071A` = 00, `$071F` = 00.
nescli `$071B`/`$071D` stay 00/FF through 261; fceux flips 00/FF to
01/01 at 231 with scroll. nescli `$071C` stays 00 through 261
(rechecked to 265: still 00, X reaches `$4B`).

## 2. Order: stall starts first, scroll follows

- Stall start: nescli X sticks at `$28` from 215 (Mario spawns at 214:
  X goes 00 at 214 to 28 at 215). fceux shows the same phenomenon at
  196-202 (X = `$28` for 7 frames, then walks from 203).
- Scroll onset: fceux `$071C` leaves 00 at 231, when X reaches `$53`.
  nescli X is `$2A` at 231 and reaches `$53` only after 261, so its
  scroll never starts inside the window.
- Gap: stall start (215) precedes scroll onset (231) by 16 frames in
  same-index terms. In fceux's own timeline the walk stall ends at 202
  and scroll starts at 231, 29 frames later. Scroll cannot cause a stall
  that starts before it and also occurs (shorter) in the emulator that
  scrolls on time. Direction: slow walk -> late X=$53 -> late scroll.

## 3. Cause

Stall cause: the stall is a start-of-walk stall, same type in both
emulators (fceux 7 frames at `$28`, nescli 12 frames). Input is ruled
out: the movie stream is identical in order (`$06FC` = `$01` both,
#205) and the latch reads each row at normal polls in the same frame it
applies (#214). The extra stillness rides on the game-state phase, not
the pad: Mario spawns 19 frames late in nescli (214 vs ~195, the #203
intermission offset: end 197 vs 178), and the stall starts at
spawn + 1 in both timelines (nes 215, fc 196).

OPEN with missing measurement: why the still phase lasts 12 frames in
nescli vs 7 in fceux (5 extra frozen frames at identical rendered
state). Needed: per-frame dump of the walk-driving bytes over
nes 214-227 / fc 195-208 at the sl-240 cut — player horizontal velocity
and walk/skid/animation timers — plus which code path the NMI runs on
frames 215 vs 216+ (walk-accel vs idle), to see whether the extra frames
come from timer phase (#203) or a missed accel step. `$006D` reads 00
in both and `$0070-$0074` are constant, so the bytes that matter are
outside the #213 probe set.

## 4. Background re-check

Held-equal test repeated for all 22 pairs in the table (this ticket, RGB
above): **0 differing pixels in every pair** whenever X and scroll
match. Full-band diffs in the same pairs (up to ~300 px) come from
Mario's sprite pose and HUD, not background. Shift test: dx = 0 with
m0 >= 0.992 for 214-230; from 231 dx = -(fceux `$071C`) exactly
(-2, -3, -5, -6, -8). Background renders correctly; the drift is scroll
onset driven by Mario X, confirming #213 with no correction.

## Repro

```bash
sed -n '214,235p' /tmp/nesdump/c213/nes213.txt   # nes X/scroll
grep -E '^f=2(1[4-9]|2[0-9]|3[0-5]) ' /tmp/nesdump/c213/fc213.txt
cd /tmp/nesdump && python3 c213/scroll2.py 214 236  # dx/m0 column
grep -rn "DEBUG-" nes_core nescli   # must print nothing
```
