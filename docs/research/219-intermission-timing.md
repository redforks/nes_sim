# #219: intermission timing at both frame cuts (sl 261 vs sl 240)

Parent: #217 (spec), part of #200. No behavior change in nescli/nes_core.
All numbers below reproduced from the existing #203 (sl 261) and #211
(sl 240) timer traces; no new probe added in this ticket, no `[DEBUG-`
tag present (`grep -rn "DEBUG-" nes_core nescli` empty). Note for #225:
probes stay temporary/reverted; this ticket added none.

## Sources

- nescli sl-261 cut: `/tmp/nesdump/nes_timer.txt` (`[DEBUG-203]` in
  `nes_core/src/nes_machine.rs` `process_frame`, `$077F`/`$07A0` per
  frame; reverted via `git checkout` in #203).
- nescli sl-240 cut: `/tmp/nesdump/n211.txt` (`[DEBUG-211]`, same probe
  sampling on the first tick where scanline == 240; reverted in #211).
- fceux 2.6.6 both cuts: `/tmp/nesdump/fc_timer.txt`
  (`memory.readbyte` per frame; same ROM/movie/palette, not re-run).
- Analysis: `/tmp/nes217-219-check219.py` (scratch, asserts every number
  below; all PASS). Prior results: #203 (sl 261), #211 (sl 240),
  #212 (`00`-state end artifact), #206 (zero-NMI frames).

Inputs throughout: `Super Mario Bros. (World).nes`, movie
`happylee-supermariobros,warped.fm2`, `fceux-default.pal`, 600 frames.

## 1. sl 261 cut (nescli frame boundary) — matches #203

| event | nescli | fceux | delta |
|---|---|---|---|
| `$07A0` 00→07 (intermission load) | 50 | 51 | −1 (separate cause, #212/#215) |
| first `$07A0` tick | 70 (+20 after load) | 52 (+1 after load) | +18 |
| following ticks, every 21 frames | 91, 112, 133, 154, 175, 196 (`01→00`) | 73, 94, 115, 136, 157 | +18 each |
| `$07A0` back to 07 (end) | 197 (`00→07`) | 178 (`01→07`) | +19 samples, +20 ticks |
| intermission length | 147 | 127 | +20 |
| `$077F` underflow (`00→14`) | 069→070 | 051→052 | same 21-frame period |

Every tick 18 frames late, from the first tick on. Same prescaler rate,
shifted phase.

## 2. sl 240 cut (fceux frame boundary) — matches #211

| event | nescli | fceux | delta |
|---|---|---|---|
| `$07A0` load | 50 (unchanged) | 51 | −1 |
| first `$07A0` tick | 71 (+21 after load) | 52 (+1) | +19 |
| following ticks, every 21 frames | 92, 113, 134, 155, 176 | 73, 94, 115, 136, 157 | +19 each |
| `$07A0` back to 07 (end) | 197 (`01→07` direct) | 178 | gap unchanged |
| intermission length | 147 | 127 | unchanged |

## 3. Frame-cut artifact labels

- ARTIFACT — `$077F` 0/2 decrement pattern. sl 261: 131 non-1 steps
  (67 zeros, 63 twos, frames 2–600). sl 240: 6 non-1, 5 zeros, 0 twos.
  The 46–51 0/2 pairs are gone; the NMI `DEC` lands in the same sample
  as its NMI. Explains #203 irregulars, explains nothing else.
- ARTIFACT — `00` end sample at 196. sl 261 shows `01` at 175 → `00`
  at 196 → `07` at 197; sl 240 shows `01→07` direct at 197, same shape
  as fceux `01→07` at 178 (#212). The tick train is identical; only the
  sampling of the last tick differs.
- ARTIFACT (shifted, not removed) — lone zero-NMI sample: 45 (sl 261,
  fceux) vs 44 (sl 240); matches the 43-vs-45 zero-NMI frames in #206.
- NOT an artifact — first-tick phase (+20/+21 vs +1) and end gap
  (147 vs 127). Neither moves with the cut. Cause is the `$077F`
  prescaler phase against the load, per #211.

## 4. Conclusion for #217

Intermission gap = prescaler phase + early load, not sample boundary.
The cut moves the apparent first-tick offset by exactly 1 (18→19, the
one-frame sampling shift) and removes the 0/2 and `00`-sample
distractions. The −1 load offset (50 vs 51) is cut-independent and stays
with T1 (#215). Numbers match #203 and #211 exactly; no correction to
either.

## Repro

```bash
python3 /tmp/nes217-219-check219.py   # asserts sections 1-2 + artifact counts
grep -rn "DEBUG-" nes_core nescli     # must print nothing
```

Regenerate traces (temporary probes, revert after; see #203/#211
comments for the exact `[DEBUG-203]`/`[DEBUG-211]` snippets):
same 600-frame SMB movie in nescli with the `process_frame` probe at
sl 261 (default) and at `scanline == 240`, plus the fceux
`memory.readbyte` lua run.
