# #220: intermission load trigger ($07A0) and upstream first-change frames

Parent: #217 (spec), part of #200. No behavior change in nescli/nes_core.
Scratch probes added in this ticket were reverted before commit
(`grep -rn "DEBUG-220" nes_core nescli` prints nothing; see Revert note).

## Sources

- nescli write probe: `Cpu::write_mem` PC/dot-clock log + sl240-cut
  `$0700-$07FF` dump per `process_frame`, 70 frames:
  `/tmp/nesdump/w220/nes220.log` (probes reverted; log retained).
- fceux write probe: `memory.registerwrite` on the 11 candidate
  addresses, 70 frames: `/tmp/nesdump/w220/fc_writes.txt`
  (`fc_writes.lua`, `run_fc.sh`).
- Prior: #215 resolution (`gh issue view 215 --comments`): RAM-sample
  first-change table (fceux sample = framecount+1, nescli sample =
  sl261-cut `process_frame` call #), trigger `STA $07A0` at routine
  `$889D` (opcode `$88A7`, PC reads `$88AA`).
- ROM mismatch warning: w220 fceux ran `Super Mario Bros. (Japan, USA).nes`,
  w220 nescli ran `Super Mario Bros. (World).nes`. Same movie
  `happylee-supermariobros,warped.fm2`. Re-run on one ROM for the final
  word (see Open).

## 1. Probe blind spot (why some nescli writes are missing)

The w220 nescli probe sat in `Cpu::write_mem` (`nes_core/src/cpu.rs`)
only. RMW (INC/DEC: double log old,new) goes through it; STA absolute
(single write) goes through `ValueTargetTrait for Mem::write`
(`cpu.rs:292-298`) and is NOT logged. Every STA below is therefore
cut240-window-bounded, never PC-confirmed, on the nescli side. The
missing measurement is a probe at `Mcu::write` level (or `Mem::write`)
with scanline/dot stamp. fceux Lua `registerwrite` sees all writes.

## 2. First-change frames per upstream flag (write space)

fceux frame = `emu.framecount()` during the write. nescli frame =
emulation frame containing the write (clk bracketed by frame261 markers
and the sl240 cut; all observed RMW writes land pre-sl240, i.e. frame
head). STA rows give the cut240 window instead of a PC.

| flag | transition | fceux (PC) | nescli (PC / window) | delta |
|---|---|---|---|---|
| `$0774` | 00->01 init, INC `$804F` | 5 | head-4 | fc = nes+1 |
| `$0772` | 00->01 init, INC `$9060` | 8 | head-7 | fc = nes+1 |
| `$0742` | ->02, STA `$9C93` | 8 | STA, <=7 (cut240-7 shows 02; 1-6 show 00) | fc = nes+1 |
| `$0774` | counter 01->02, INC `$86E9` | 15 | head-14 | fc = nes+1 |
| `$0772` | 01->02, INC `$8751` | 32 | head-31 | fc = nes+1 |
| `$0722` | 00->01, INC `$90C8` | 33 | head-32 | fc = nes+1 |
| `$0772` | 02->03, INC `$90CB` | 33 | head-32 | fc = nes+1 |
| `$0774` | ->00, STA `$9076` | 33 | STA, tail-31/head-32 (cut240 31=0D, 32=00) | 1-2 |
| `$0757` | ->01, STA `$9066` | 33 | STA, tail-31/head-32 (cut240 31=00, 32=01) | 1-2 |
| `$0739` | 00->03 chain, INC `$C25E/$C261/$C264` | 34 | head-33 | fc = nes+1 |
| `$0750` | ->C2, STA `$C245` | 34 | STA, tail-32/head-33 (cut240 32=25, 33=C2) | 1-2 |
| `$0757` | ->00, STA `$9194` | 34 | STA, tail-32/head-33 (cut240 32=01, 33=00) | 1-2 |
| `$0789` | ->18, STA `$B34E` | 36 | STA, tail-34/head-35 (cut240 34=00, 35=18) | 1-2 |
| `$0789` | 18->17, DEC `$8116` (refresh runs most frames) | 37 | head-35 | fc = nes+2, see 5 |
| `$0750` | ->25, STA `$9C09` | 42 | STA, tail-41/head-42 (cut240 41=C2, 42=25) | 0-1 |
| `$074A` | ->10, STA `$8E91` | 42 | STA, tail-41/head-42 (cut240 41=00, 42=10) | 0-1 |
| `$0757` | 00->01, INC `$82F2` | 42 | head-42 | same frame |
| `$0770` | 00->01, INC `$82F5` | 42 | head-42 | same frame |
| `$0742` | 02->00, STA `$90DE` clear | 43 | STA, tail-42/head-43 (cut240 42=02, 43=00) | 0-1 |
| `$0722` | ->00, STA `$90DE` clear | 43 | STA, tail-42/head-43 (cut240 42=01, 43=00) | 0-1 |
| `$0739` | ->00, STA `$90DE` clear | 43 | STA, tail-42/head-43 (cut240 42=03, 43=00) | 0-1 |
| `$074A` | ->00, STA `$90DE` clear | 43 | STA, tail-42/head-43 (cut240 42=10, 43=00) | 0-1 |
| `$0774` | ->01, STA `$905D` | 44 | STA, tail-43/head-44 (cut240 43=00, 44=01) | 0-1 |
| `$0772` | 00->01, INC `$9060` | 44 | head-44 | same frame |
| `$0742` | ->02, STA `$9C93` | 44 | STA, tail-43/head-44 (cut240 43=00, 44=02) | 0-1 |
| `$0789` | 16->00, STA `$8FF0` | 44 | STA, tail-43/head-44 (cut240 43=16, 44=00) | 0-1 |

## 3. Trigger write for the `$07A0` load

- fceux: `framecount=50`, PC `$88AA`, val `07` (STA at `$88A7` in
  routine `$889D`: `LDA $07A0 / BNE / JSR $8220 / LDA #$07 / STA $07A0`).
  Next framecount 51 shows DEC `$8116` 07->06 (prescaler path). Log:
  `fc_writes.txt` lines 150-154.
- nescli: STA unlogged (blind spot, section 1); `$07A0` is 00 at
  cut240-49 and 07 at cut240-50, so the load lands in tail-49/head-50.
  #215 independently reported nescli `frame=50 pc=$88AA`. Consistent,
  but w220 does NOT PC-confirm it: treat nescli trigger PC as
  corroborated, not re-measured.
- Sub-frame: neither side stamps scanline/dot on the write. fceux cut =
  frameadvance boundary; nescli sl240 cut excludes tail writes while the
  sl261 cut (#215) includes them. Cross-emulator sample comparison
  therefore carries +/-1 convention slop; dot-level write timestamps are
  the measurement that closes the last frame.

## 4. Offset 30-41 vs 42-45, and cut240 vs sl261

- Frames 30-41 cluster: fceux one frame later (write space delta 1 for
  all RMW-observed transitions; STA rows bounded to 1-2 by cut240
  sampling). #215 sample space reported 2 here; the extra 1 is sample-cut
  attribution (section 5), not a second emulation frame.
- Frames 42-45 cluster: same frame (INC `$82F2/$82F5/$9060`) or 0-1
  (STA windows). The offset is not constant: it shrinks from 1 to ~0
  across the intermission entry.
- cut240 vs sl261: w220 sampled RAM at sl240 (new) where #215 sampled at
  sl261. All w220-observed RMW writes land pre-sl240 so both cuts agree
  on them; STA positions within the frame are unknown (no dot stamps), so
  a post-sl240 STA would appear one frame later at the sl240 cut than at
  the sl261 cut. Per #211/#212 this cut effect explains transient
  single-frame states but not the load/end gap.

## 5. Open (for #224/#225)

- #215 sample space says nescli 32 where w220 write space says head-31
  (e.g. `$0722`); both cuts should see a head write same-frame. The +1 in
  #215 nescli samples is unexplained: re-check `process_frame` call
  numbering vs the sl261 sampling point.
- `$0789` first DEC 18->17: fc 37 vs nescli head-35 (delta 2 while the
  STA-first-18 delta is 1-2): the STA->DEC lag differs (fceux next frame,
  nescli same frame). Needs dot stamps to tell whether the 18-refresh
  ordering itself diverges.
- Prescaler `$077F` reload 00->14 lands with the load (cut240-49: 00,
  cut240-50: 14, `$077F` not in either probe set): include `$077F` in the
  `Mcu::write`-level probe re-run, same ROM both sides.

## Revert note

Probes removed: `git checkout -- nes_core/src/cpu.rs
nes_core/src/nes_machine.rs` (only `[DEBUG-220]` blocks). Verify with
`grep -rn "DEBUG-220" nes_core nescli` (empty) and `git status --short`
(docs only). Logs kept at `/tmp/nesdump/w220/`
(`nes220.log`, `fc_writes.txt`, `fc_writes.lua`, `run_fc.sh`,
`run_nes.sh`, `w220_first_change.py` from the prior run).
